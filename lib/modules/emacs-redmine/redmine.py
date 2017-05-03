#!/usr/bin/python


import os
import sys
import tempfile
import argparse
import settings
from redmine_api import *
from genshi.template import NewTextTemplate
import csv

class Org():

    root_folder = os.path.dirname(os.path.realpath(__file__))
    template_folder = os.path.join(root_folder, "templates")

    def __init__(self, redmine, args):
        self.redmine = redmine
        self.args = args

    def sprints(self, project):
        sprints = self.redmine.getVersions(project)
        print self._process_template("sprints.org", project=project, sprints=sprints)

    def everything(self, project):
        sprints = self.redmine.getVersions(project)
        
        for sprint in sprints:
            sprint.issues = self.redmine.findIssues(get_all_info=True, project_id=args.redmine_project, include='journals', sort="position",
                                                    tracker_id=settings.TRACKER_SPRINT, fixed_version_id=sprint.id, status_id='*', limit=900)

            for issue in sprint.issues:
                tasks = []
                for child in issue.children:
                    tasks.append(self.redmine.getIssue(child['id']))
                issue.tasks = tasks

        print self._process_template("everything.org", project=project, sprints=sprints)

    def everything_for_sprint(self, project, sprint_id):
        if sprint_id is None:
            raise Exception("sprint must be specified")
        
        sprints = self.redmine.getVersions(project)
        for sprint in sprints:
            if sprint.id == sprint_id:
                break

        sprint.issues = self.redmine.findIssues(get_all_info=True, project_id=args.redmine_project, include='journals',  sort="position",
                                         tracker_id=settings.TRACKER_SPRINT, fixed_version_id=sprint.id, status_id='*', limit=900)

        for issue in sprint.issues:
            tasks = []
            for child in issue.children:
                tasks.append(self.redmine.getIssue(child['id']))
            issue.tasks = tasks
            if issue.status['name'] == "New":
                issue.status['org_name'] = "TODO"
            else:
                issue.status['org_name'] = "DONE"

        print self._process_template("everything_sprint.org", project=project, sprint=sprint)

    def time_sheet(self, project, sprint):
        if sprint is None:
            raise Exception("sprint must be specified")
        timesheets  = self.redmine.timesheets(project, fixed_version_id=sprint)
        timesheet_content = self._process_template("timesheets.org", timesheets=timesheets, project=project, sprint=sprint)
        filename = "/home/gtp/temp/timesheet.csv"
        f = open(os.path.join(filename), "w")
        f.write(timesheet_content)
        print("\nWritten to %s\n\n" % filename)
        print(timesheet_content)

    def issues(self, sprint, page=1):
        """get_all_info=True is much slower since it makes an
        additional api call for each issue, only use it if you want
        each issue in full."""
        issues_per_page=100
        offset = issues_per_page * (page-1)
        if sprint is None:
            raise Exception("sprint must be specified")
        issues = self.redmine.findIssues(get_all_info=False, project_id=args.redmine_project, include='journals',  sort="position",
                                         tracker_id=settings.TRACKER_SPRINT, fixed_version_id=sprint, status_id='*', limit=issues_per_page, offset=offset)
        print self._process_template("issues.org", sprint=sprint, issues=issues)

    def issue(self, issue_id):
        issue = self.redmine.getIssue(issue_id)
        print self._process_template("issue.org", issue=issue)

    def new_issue(self, project, sprint):

        subject = self._read_argument("Subject")
        description = self._read_argument("Description", multi_line=True)
        tracker_name = "feature"

        if sprint is None:
            raise Exception("sprint must be specified")
        self.redmine.new_issue(project, fixed_version_id=sprint, tracker_name=tracker_name, subject=subject, description=description)
        print("Issue created")

    def edit_issue(self, issue_id):
        if issue_id is None:
            raise Exception("issue must be specified")

        args = {}
        issue = self.redmine.getIssue(issue_id)
        print("")
        print("---------------------------------------")
        print("Current subject is : \n %s" % issue.subject)
        print("---------------------------------------")
        print("")
        self._set_argument_or_ignore(args, 'subject', prompt="Subject (leave blank to ignore)")

        print("")
        print("---------------------------------------")
        print("Current description is :\n %s" % issue.description)
        print("---------------------------------------")
        print("")
        self._set_argument_or_ignore(args, 'description', prompt="Description (leave blank to ignore, type 'plus' (without quotes) on a line by itself to insert the existing description at that point)", 
                                     multi_line=True)
        
        if args['description'].startswith("plus\n"):
            args['description'] = issue.description + args['description'][len("plus\n"):]
        args['description'] = args['description'].replace("\nplus\n", "\n%s\n"%issue.description)
        self.redmine.updateIssueFromDict(issue_id, args)
        print("Issue %s updated" % str(issue_id))

    def delete_issue(self, issue_id):
        if issue_id is None:
            raise Exception("issue must be specified")

        confirm = self._read_argument("Are you sure you want to delete issue " + str(issue_id) + "? (type yes to confirm) : ")
        if str(confirm) != "yes":
            return

        self.redmine.deleteIssue(issue_id)
        print("Issue %s deleted" % str(issue_id))

    def add_issue_journal(self, issue_id):
        if issue_id is None:
            raise Exception("issue must be specified")

        args = {}
        self._set_argument_or_ignore(args, 'notes', prompt="Notes (leave blank to ignore)", multi_line=True)
        
        self.redmine.updateIssueFromDict(issue_id, args)
        print("Issue journal entry created for %s" % str(issue_id))

    def set_issue_status(self, issue_id, status):
        if status is None:
            raise Exception("Status must be specified")
        if status == 'new':
            self.redmine.issueStatusNew(issue_id)
        elif status == 'devdone':
            self.redmine.issueStatusDevDone(issue_id)
        elif status == 'tested':
            self.redmine.issueStatusTested(issue_id)
        elif status == 'reopened':
            self.redmine.issueStatusReopened(issue_id)
        elif status == 'cantreproduce':
            self.redmine.issueStatusCantReproduce(issue_id)
        else:
            raise Exception("Unknown status : " + status)
        print "Issue status changed to " + status

    def _process_template(self, template_filename, **kwargs):
        f = open(os.path.join(self.template_folder, template_filename))
        template_text = f.read()
        template = NewTextTemplate(template_text)
        f.close()
        stream = template.generate(**kwargs)
        return stream.render()

    def _read_multiline_input(self, prompt):
        user_input = [] 
        entry = raw_input(prompt + "\n(enter 'done' on its own line when done) \n\n") 
        while entry != "done": 
            user_input.append(entry) 
            entry = raw_input("") 
        user_input = '\n'.join(user_input) 
        return user_input

    def _read_argument(self, prompt, multi_line=False):
        if not multi_line:
            return raw_input(prompt+": ")
        else:
            return self._read_multiline_input(prompt)
            # t = tempfile.NamedTemporaryFile(delete=False)
            # try:
            #     editor = os.environ['EDITOR']
            # except KeyError:
            #     editor = 'nano'
            #     subprocess.call([editor, t.name])
            # return t.read()

    def _set_argument_or_ignore(self, args_dict, arg_name, *args, **kwargs):
        v = self._read_argument(*args, **kwargs)
        if v is None or len(str(v).strip())==0:
            return
        args_dict[arg_name] = v

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Process some integers.')
    parser.add_argument('--url', dest='redmine_url', type=str, required=True,
                        help='the url to redmine')
    parser.add_argument('--userkey', dest='redmine_login_key', type=str, required=True,
                        help='the user key to redmine, can be found on the right of the "my account" page in redmine.')
    parser.add_argument('--project', dest='redmine_project', type=str, required=True,
                        help='the project name')
    parser.add_argument('--action', dest='action', type=str, required=True,
                        help='''What to do, one of: issues issue new-issue set-issue-status versions (and probably more)''')
    parser.add_argument('--sprint', dest='sprint', type=str, required=False, default=None,
                        help='The sprint id, used for some actions')
    parser.add_argument('--page', dest='page', type=int, required=False, default=1,
                        help='The page number, used when viewing issues in a sprint')
    parser.add_argument('--issue', dest='issue', type=str, required=False, default=None,
                        help='The issue id, used for some actions')
    parser.add_argument('--status', dest='status', type=str, required=False, default=None,
                        help='The status name, used for some actions. One of new, devdone, tested, reopened')
    args = parser.parse_args()

    redmine = Redmine(url=args.redmine_url, key=args.redmine_login_key, debug=True)

    org = Org(redmine, args)

    if args.action == "issues":
        org.issues(args.sprint, args.page)
    elif args.action == "everything":
        org.everything(args.redmine_project)
    elif args.action == "everything-sprint":
        org.everything_for_sprint(args.redmine_project, args.sprint)
    elif args.action == "time-sheet":
        org.time_sheet(args.redmine_project, args.sprint)
    elif args.action == "issue":
        org.issue(args.issue)
    elif args.action == "new-issue":
        org.new_issue(args.redmine_project, args.sprint)
    elif args.action == "edit-issue":
        org.edit_issue(args.issue)
    elif args.action == "delete-issue":
        org.delete_issue(args.issue)
    elif args.action == "add-issue-journal":
        org.add_issue_journal(args.issue)
    elif args.action == "sprints":
        org.sprints(args.redmine_project)
    elif args.action == "set-issue-status":
        org.set_issue_status(args.issue, args.status)
    else:
        raise Exception("Unknown action : " + args.action)
