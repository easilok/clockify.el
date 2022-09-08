# Clockify.me API interface for emacs

Access your [Clockify](https://clockify.me) workflow from within emacs

**This is alpha software, and it's currently in development. However you can use it and use the described features.**

This package provides an interface for the actions:
- List workspace projects
- Create new projects in the workspace
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

## Get the Clockify API token

This token is used for identifying the user in the API request, which defines what features it can access.

On your the [clockify application](https://app.clockify.me/) with your login select the top right user image (if image not set, should have the user initials) and go to **Profile settings**. At the end of the page, there should be a section called **API** with a box with your **API key**. You can generate a new one with the button next to it.

## Package Variables

|Name|Type|Description|
|---|---|---|
|clockify-auth-token|Custom|Define the token to access clockify API.|
|clockify--current-user-id|Variable|Saves the current user ID. Can be fetched by clockify--user-info function|
|clockify--active-workspace-id|Variable|Saves the current active workspace ID.Can be fetched by clockify--user-info function|
|clockify-api-url|Constant|Current clockify API base url|
|clockify-default-headers|Constant|Default Headers for any request to clockify API|


## Package Functions

The package as two types of functions: **interactive** which can be called in the normal M-x workflow, and prompt user for the needed input, and **internal** which can be used to script your own workflow and interactions with clockify.

### Interactive Functions

|Name|Action|Details|
|---|---|---|
|clockify--stop-entry|Stop ongoing time entry|Uses the current time as end time|
|clockify--restart-previous-entry|Prompt user with a list of the last distinct time entries to restart|The list is composed of the last 50 time entries, filtered by distinct descriptions.|
|clockify--restart-previous-entry-no-project|Prompt user with a list of the last distinct time entries to restart|Same as _clockify--restart-previous-entry_ but it will not associate a project with the time entry|
|clockify--start-new-entry|Prompt the user for a description for the new time entry, followed by it's project from a list||
|clockify--start-new-entry-without-project|Prompt the user for a description for the new time entry, followed by it's project from a list|Same as _clockify--start-new-entry_ but it will not associate a project with the new time entry|
|clockify--add-new-project|Prompt the user for a name for the new project|The new project will be created only with the name. The association of the client, color and other options need to be done on the clockify page|

### Internal Functions

| Name                    | Action | Arguments                           |
|-------------------------|--------|-------------------------------------|
| clockify--add-entry     |Creates a new time entry with the given description and associated to the given project.| - **description** for the new time entry<br /> - **projectId** Id of the project to associate the new added task. It's optional!|
| clockify--ongoing-entry | Fetch the details of the currently ongoing time entry |                                     |
|clockify--time-entries-desc-list |Fetch the last 50 time entries description||
|clockify--time-entries-details-list|Fetch the last 50 time entries details in a dot pair list with format _(description . projectid)_||
|clockify--time-entries|Fetch the last 50 time entries with all the API fields||
|clockify--add-project|Add a new project with the given name|- **name** for the project|
|clockify--projects|Fetch the current workspace project list with all the API fields||
|clockify--user-info|Fetch the current user info and saves userId and workspaceID for usage in other functions||
|clockify--build-list-with-key-pair|Builds a dot pair list from an API fetch response list with the given key value as the new key, and second given key value as new value|- **fetched-list** API list response<br />-**key1** key for getting the API list value to use as key on the returned dot pair element<br />-**key2** key for getting the API list value to use as value on the returned dot pair element|
