#obtained and extensively modified from http://code.google.com/p/pyredminews


import urllib
import urllib2
from xml.dom import minidom, getDOMImplementation
import xml.etree.ElementTree as ET
import logging
from StringIO import StringIO
import re,datetime
from operator import itemgetter, attrgetter
import settings

logger = logging.getLogger(__name__)		

def parse_redmine_date(s):
	'''s is something like "2008-09-03T20:56:35.450686Z"'''
	'''or  "2012-09-25 00:27:39 UTC"'''
	try:
		if len(s) == len('yyyy-mm-dd'):
			year, month, day = s.split("-")
			return datetime.datetime(int(year), int(month), int(day))
		if 'UTC' in s:
			date, time, dummy = s.split(" ")
			year, month, day = date.split("-")
			hour, minute, second = time.split(":")
			return datetime.datetime(int(year), int(month), int(day), int(hour), int(minute), int(second))
		else:
			return datetime.datetime(*map(int, re.split('[^\d]', s)[:-1]))
		
	except:
		raise

class _Version:
	def __init__(self, redmine, eTree=None ):
		self.__redmine = redmine
		if eTree is not None:
			self.parseETree( eTree )
			
	def parseETree(self, eTree ):
		self.root = eTree
		self.name = self.root.find('name').text
		self.id = self.root.find('id').text
		self.status = self.root.find('status').text
		self.created_on = self.root.find("created_on").text


class _Project:
	'''Object returned by Redmine getProject calls
	   redmine is the redmine object.
	   objXML is the xml object containing the object data'''
	
	def __init__(self, redmine, eTree=None ):
		self.__redmine = redmine
		self.root = None
		self.number = None
		self.id = None
		self.name = None
		self.custom = {}
		self.tracker = {}
		
		if eTree:
			try:
				self.parseETree( eTree )
			except:
				if self.__redmine.readonlytest:
					self.THIS_IS_FAKE_DATA = True
					self.number = '6789'
					self.name = 'Fake Project'
					self.id = 'fakeproject'
					return
				else:
					raise
		
	def parseETree(self, eTree ):
		self.root = eTree.getroot()
		
		if self.root.tag == 'projects':
			raise TypeError ('XML does not describe a single project, but a collection of projects.')
		elif not self.root.tag == 'project':
			raise TypeError ('XML does not describe a project as I know it.')
		
		self.number = self.root.find('id').text
		self.id = self.root.find('identifier').text
		self.name = self.root.find('name').text
		
		if self.root.find('custom_fields') is not None:
			for field in self.root.find('custom_fields'):
				self.custom[ field.attrib['name'] ] = field.text
			
		if self.root.find('trackers') is not None:
			for tracker in self.root.find('trackers'):
				self.tracker[ tracker.attrib['name'] ] = tracker.attrib['id']
		
	
	def newIssue(self, **data ):
		'''Create a new issue for this project from the given pairs.
		
		newIssue( subject="The Arguments Department is closed", description="Good morning." )
		
		Possible keys are:
		 subject
		 description
		 status_id
		 priority_id
		 tracker_id - can be looked up from name in Project.trackers[name]
		 assigned_to_id
		
		Unfortunately, there is no easy way to discover the valid values for most of the _id fields'''
			
		if not 'subject' in data:
			TypeError('Subject cannot be blank.  Use subject=str')
		
		data[ 'project_id' ] = self.number
		return self.__redmine.newIssueFromDict( data )
		

class _Issue:
	'''Object returned by Redmine getIssue and newIssue calls'''
	def __init__(self, redmine, eTree=None ):
		self.__redmine = redmine
		
		self.id = None
		self.subject = None
		self.description = None
		self.created_on = None
		self.updated_on = None
		self.custom = {}
		self.relations = {}
		self.assigned_to = None
		self.tracker = None
		self.status = None
		self.journals = []
		self.position = 0
		self.estimated_hours = 0
		self.children = []
		
		if eTree is not None:
			try:
				self.parseETree( eTree )
			except:
				if self.__redmine.readonlytest:
					self.THIS_IS_FAKE_DATA = True
					self.id = '12345'
					self.subject = 'Fake Issue'
					self.project = '6789'
					self.status = '1'
					return
				else:
					raise

	
	def parseETree(self, eTree):
		'''Parse fields from given eTree into this object'''
		self.root = eTree.getroot()
		
		if self.root.tag == 'issues':
			raise TypeError ('XML does not describe a single issue, but a collection of issues.')
		elif not self.root.tag == 'issue':
			raise TypeError ('XML does not describe an issue as I know it.')
		
		self.id 		= self.root.find('id').text
		self.subject 	= self.root.find('subject').text.replace("\n", "")
		self.description = self.root.find('description').text
		self.created_on = parse_redmine_date(self.root.find('created_on').text)
		self.updated_on = parse_redmine_date(self.root.find('updated_on').text)
		self.project 	= self.root.find('project').attrib
		self.tracker 	= self.root.find('tracker').attrib
		self.status 	= self.root.find('status').attrib

		try:
			self.estimated_hours = float(self.root.find('estimated_hours').text)
		except:
			pass

		try:
			self.assigned_to = self.root.find('assigned_to').attrib
		except:
			pass
		
		
		if self.root.find('custom_fields') is not None:
			for field in self.root.find('custom_fields'):
				self.custom[ field.attrib['name'] ] = field.text

		if self.root.find('relations') is not None:
			for field in self.root.find('relations'):
				self.relations[ field.attrib['issue_id'] ] = field.attrib

		if self.root.find('children') is not None:
			for field in self.root.find('children'):
				self.children.append({ 'id': field.attrib['id'],
						       'subject': field.find('subject').text})

		if self.root.find('journals') is not None:
			for field in self.root.find('journals'):
				self.journals.append({ 'created_on': parse_redmine_date(field.find('created_on').text),
						       'notes': field.find('notes').text })
			
	def resolve(self):
		'''Resolve this issue'''
		self.__redmine.resolveIssue( self.id )
		
	def close(self):
		'''Close this issue'''
		self.__redmine.closeIssue( self.id )
		
	def save(self):
		'''Saves this issue - updates or creates new issue as necessary.  Failed updates DO NOT return any errors.'''
		pass


class _TimeEntry:
	def __init__(self, redmine, eTree=None ):
		self.__redmine = redmine
		self.id = None
		self.project_name = None
		self.issue_id = None
		self.username = None
		self.hours = None
		self.comments = None
		self.date = None

		if eTree is not None:
			self.parseETree( eTree )

	def parseETree(self, eTree):
		self.root = eTree.getroot()
		if self.root.tag == 'time_entries':
			raise TypeError ('XML does not describe a single timeentry, but a collection of timeentries.')
		elif not self.root.tag == 'time_entry':
			raise TypeError ('XML does not describe an time_entry as I know it.')
		self.id = self.root.find('id').text
		self.project_name = self.root.find('project').attrib['name'].strip()
		self.issue_id = self.root.find('issue').attrib['id']
		self.username = self.root.find('user').attrib['name']
		self.hours = float(self.root.find('hours').text)
		self.comments = self.root.find('comments').text
		self.spent_on = parse_redmine_date(self.root.find('spent_on').text)

class Redmine:
	'''Class to interoperate with a Redmine installation using the REST web services.
	instance = Redmine(url, [key=strKey], [username=strName, password=strPass] )
	
	url is the base url of the Redmine install ( http://my.server/redmine )
	
	key is the user API key found on the My Account page for the logged in user
		All interactions will take place as if that user were performing them, and only
		data that that user can see will be seen

	If a key is not defined then a username and password can be used
	If neither are defined, then only publicly visible items will be retreived	
	'''
	
	def __init__(self, url, key=None, username=None, password=None, debug=False, readonlytest=False, project_name=None ):
		self.__url = url
		
		self.key = key
		self.debug = debug
		self.readonlytest = readonlytest
		self.projects = {}
		self.projectsID = {}
		self.projectsXML = {}

		self.project_name = project_name
		
		self.issuesID = {}
		self.issuesXML = {}
		
		if readonlytest:
			print 'Redmine instance running in read only test mode.  No data will be written to the server.'
		
		self.__opener = None

		# if not username:
		# 	username = key
		# 	self.__key = None
			
		# if not password:
		# 	password = '12345'  #the same combination on my luggage!  (dummy value)
		
		if( username and password ):
			#realm = 'Redmine API'
			# create a password manager
			password_mgr = urllib2.HTTPPasswordMgrWithDefaultRealm()

			password_mgr.add_password(None, url, username, password )
			handler = urllib2.HTTPBasicAuthHandler( password_mgr )

			# create "opener" (OpenerDirector instance)
			self.__opener = urllib2.build_opener( handler )

			# set the opener when we fetch the URL
			self.__opener.open( url )

			# Install the opener.
			urllib2.install_opener( self.__opener )
			
		else:
			if not key:
				pass
				#raise TypeError('Must pass a key or username and password')
		

	def Issue(self, eTree=None ):
		'''Issue object factory'''
		return _Issue( self, eTree )
		
	def Project(self, eTree=None ):
		'''Issue project factory'''
		return _Project( self, eTree )

	def Version(self, eTree=None):
		return _Version(self, eTree)
	
	def TimeEntry(self, eTree=None):
		return _TimeEntry(self, eTree)

	# extend the request to handle PUT command
	class PUT_Request(urllib2.Request):
		def get_method(self):
			return 'PUT'

	# extend the request to handle DELETE command
	class DELETE_Request(urllib2.Request):
		def get_method(self):
			return 'DELETE'
	
	
	
	def openRaw(self, page, parms=None, XMLstr=None, HTTPrequest=None ):
		'''Opens a page from the server with optional XML.  Returns a response file-like object'''
		if not parms:
			parms={}
		
		# if we're using a key, add it to the parms array
		if self.key:
			parms['key'] = self.key
		
		# encode any data
		urldata = ''
		if parms:
			urldata = '?' + urllib.urlencode( parms )
		
		
		fullUrl = self.__url + '/' + page
		
		logger.debug("connecting to redmine using: %s" % fullUrl)

		# register this url to be used with the opener
		if self.__opener:
			self.__opener.open( fullUrl )
			
		#debug
		if self.debug:
			print fullUrl + urldata
		
		# Set up the request
		if HTTPrequest:
			request = HTTPrequest( fullUrl + urldata )
		else:
			request = urllib2.Request( fullUrl + urldata )

		if self.key:
			request.add_header('X-Redmine-API-Key', self.key)

		# get the data and return XML object
		if XMLstr:
			request.add_header('Content-Type', 'text/xml')
			response = urllib2.urlopen( request, XMLstr )			
		else:
			response = urllib2.urlopen( request )

		return response
	
	def open(self, page, parms=None, objXML=None, HTTPrequest=None ):
		'''Opens a page from the server with optional XML.  Returns an XML ETree object or string if return value isn't XML'''
		response = self.openRaw( page, parms, objXML, HTTPrequest )
		try:
			etree = ET.ElementTree()
			etree.parse( response )
			return etree
		except:
			return response.read()
		
	
	def get(self, page, parms=None ):
		'''Gets an XML object from the server - used to read Redmine items.'''
		return self.open( page, parms )
	
	def post(self, page, objXML, parms=None ):
		'''Posts an XML object to the server - used to make new Redmine items.  Returns an XML object.'''
		if self.readonlytest:
			print 'Redmine read only test: Pretending to create: ' + page
			return objXML
		else:
			return self.open( page, parms, objXML )
	
	def put(self, page, objXML, parms=None ):
		'''Puts an XML object on the server - used to update Redmine items.  Returns nothing useful.'''
		if self.readonlytest:
			print 'Redmine read only test: Pretending to update: ' + page
		else:
			return self.open( page, parms, objXML, HTTPrequest=self.PUT_Request )
	
	def delete(self, page ):
		'''Deletes a given object on the server - used to remove items from Redmine.  Use carefully!'''
		if self.readonlytest:
			print 'Redmine read only test: Pretending to delete: ' + page
		else:
			return self.open( page, HTTPrequest=self.DELETE_Request )
			
	def dict2XML(self, tag, dict ):
		'''Return an XML string encoding the given dict'''

		root = ET.Element( tag )
		for key, val in dict.items():
			if key == 'custom_fields':
				custom_fields_el = ET.SubElement( root, "custom_fields" )
				custom_fields_el.set("type", "array")
				for cf_id, cf_value in val.items():
					cf_el = ET.SubElement( custom_fields_el, "custom_field" )
					cf_el.set('id', str(cf_id))
					cf_value_el = ET.SubElement( cf_el, "value" )
					cf_value_el.text = str(cf_value)
			else:
				ET.SubElement( root, str(key) ).text = str(val)
		
		return ET.tostring( root, encoding='UTF-8' )
		
	def getProjects(self):
		'''returns all projects'''
		return self.get('projects.xml')

	def getProject(self, projectIdent ):
		'''returns a project object for the given project name'''
		return self.Project( self.get('projects/'+projectIdent+'.xml') )
		
	def getVersions(self, projectIdent):
		#versions = self.get('api/versions/'+projectIdent)
		#return sorted([ self.Version(v) for v in versions.findall('version') ], key=attrgetter('created_on'))
		versions = self.get('/projects/%s/versions.xml'%projectIdent)
		versions = [ self.Version(ET.ElementTree(version)) for version in versions.findall('version') ]
		return sorted(versions, key=attrgetter('created_on'))

	def getIssue(self, issueID ):
		'''returns an issue object for the given issue number'''
		return self.Issue( self.get('issues/'+str(issueID)+'.xml', parms={'include':'journals,children'}) )

	def addJournal(self, issueID, notes):
		self.updateIssueFromDict(issueID, notes=notes)
	
	def findIssues(self, get_all_info=False, **kwargs):
		'''kwargs can contain any name-value pairs supported by the redmine api, eg
		>>> findIssue(project_id='some_project_name')
		
		If get_all_info is True, then issues are created
		individually. This is slower and more inefficient, but
		useful if issues.xml isn't returning all the required data.

		Possible values defined in redmine spec at time of writing are:
		  project_id: either project id or project identifier
		  tracker_id: get issues from the tracker with the given id
		  status_id: Possible values: open, closed, * to get open and closed issues, status id
		  assigned_to_id: get issues which are assigned to the given user id
		  cf_x: get issues with the given value for custom field with an ID of x.
		'''

		issues = self.get('issues.xml?%s' % "&".join(["%s=%s" % (k,v) for k,v in kwargs.items()]))

		
		if get_all_info:
			return [ self.getIssue(self.Issue(ET.ElementTree(issue)).id) for issue in issues.findall('issue') ]
		else:
			return [ self.Issue(ET.ElementTree(issue)) for issue in issues.findall('issue') ]
		
	def timesheets(self, projectIdent, **kwargs):
		times = self.get('time_entries.xml')
		return [ self.TimeEntry(ET.ElementTree(timeentry)) for timeentry in times.findall('time_entry') ]

	def new_issue(self, projectIdent, tracker_name=None, **kwargs):
		data = {'project_id':projectIdent}
		data.update(kwargs)

		if tracker_name is not None:
			data['tracker_id'] = settings.TRACKERS[tracker_name]

		return self.newIssueFromDict( data )

	def newIssueFromDict(self, dict ):
		'''creates a new issue using fields from the passed dictionary.  Returns the issue number or None if it failed. '''
		xmlStr = self.dict2XML( 'issue', dict )
		newIssue = self.Issue( self.post( 'issues.xml', xmlStr ) )
		return newIssue
	
	def updateIssueFromDict(self, ID, dict ):
		'''updates an issue with the given ID using fields from the passed dictionary'''
		xmlStr = self.dict2XML( 'issue', dict )
		self.put( 'issues/'+str(ID)+'.xml', xmlStr )

	def deleteIssue(self, ID ):
		'''delete an issue with the given ID.  This can't be undone - use carefully!
		Note that the proper method of finishing an issue is to update it to a closed state.'''
		self.delete( 'issues/'+str(ID)+'.xml' )
		
	def issueStatusNew(self, ID ):
		'''close an issue by setting the status to self.ISSUE_STATUS_ID_CLOSED'''
		self.updateIssueFromDict( ID, {'status_id':settings.ISSUE_STATUS_NEW} )
		
	def issueStatusDevDone(self, ID ):
		'''close an issue by setting the status to self.ISSUE_STATUS_ID_RESOLVED'''
		self.updateIssueFromDict( ID, {'status_id':settings.ISSUE_STATUS_DEV_DONE} )
		
	def issueStatusTested(self, ID ):
		'''close an issue by setting the status to self.ISSUE_STATUS_ID_CLOSED'''
		self.updateIssueFromDict( ID, {'status_id':settings.ISSUE_STATUS_TESTED} )
		
	def issueStatusReopened(self, ID ):
		'''close an issue by setting the status to self.ISSUE_STATUS_ID_RESOLVED'''
		self.updateIssueFromDict( ID, {'status_id':settings.ISSUE_STATUS_REOPENED} )

	def issueStatusCantReproduce(self, ID ):
		'''close an issue by setting the status to self.ISSUE_STATUS_ID_CANT_REPRODUCE'''
		self.updateIssueFromDict( ID, {'status_id':settings.ISSUE_STATUS_CANT_REPRODUCE} )
