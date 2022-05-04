# Clockify.me API interface for emacs

Access your [Clockify](https://clockify.me) workflow from within emacs

**This is alpha software, and it's currently in development. However you can use it and use the described features.**

This package provides an interface for the actions:
- List workspace projects
- List past time entries for current user and workspace (last 50 time entries)
- List ongoing time entry
- Stop ongoing time entry
- Restart a past time entry selected by description (from last 50 time entries)
- Add a new time entry (provided the user input description and associated with a project)

**Notes:**
- All added entries are with billable option set
- Start and Stop actions use current time

## References

This project was developped from where [this mdallastella version](https://github.com/mdallastella/clockify.el/tree/f1080e7b7d5a75ab4675388bce2f8df89a2fcca9) ended. At the time the project only listed workspace projects, and the remaining features were added by me. 
