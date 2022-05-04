;;; clockify.el --- Start, stop and create Clockify tasks -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Marco Dalla Stella
;; Copyright (C) 2022  Luís Pereira

;; Author: Marco Dalla Stella <marco@dallastella.name>
;;      Luís Pereira <luis@luiscarlospereira.pt>
;; Maintainer: Luís Pereira <luis@luiscarlospereira.pt>
;; Keywords: calendar, tools, clockify
;; Package-Requires: ((emacs "25") (request "0"))
;; Version: 0.0.2
;; URL: https://github.com/easilok/clockify.el

;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;; This package provides the ability to interact with clockify API, creating, restarting and stopping new time entries.
;; Clockify can be accessed at https://clockify.me/

;;; Code:
(require 'json)
(require 'request)
(require 'cl-lib)

(defvar clockify--current-user-id nil)
(defvar clockify--active-workspace-id nil)

;;; User-Configurable Variables
(defgroup clockify nil
  "Manage Clockify tasks from Emacs."
  :tag "Clockify"
  :group 'calendar)

(defcustom clockify-auth-token nil
  "User authorization token."
  :type 'string)

;;; Constants
(defconst clockify-api-url
  "https://api.clockify.me/api/v1"
  "Default Clockify API entry point.")

(defconst clockify-default-headers
  '(("Content-Type" . "application/json"))
  "Default request headers.")


;;; Support Functions
(defun clockify--current-time-ISO8601 ()
  "Generate the current time in the ISO8601 format YYYY-MM-DDThh:mm:ss.sTZD (eg 1997-07-16T19:20:30.45+01:00)."
  (concat
   ;; (format-time-string "%Y-%m-%dT%T")
   ;; ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
   ;;  (format-time-string "%z"))))
   (format-time-string "%Y-%m-%dT%T" nil t)
   "Z"))

(defun clockify--generate-headers ()
  "Generate request headers."
  (if clockify-auth-token
      (cons `("X-Api-Key" . ,clockify-auth-token)
	    clockify-default-headers)
    clockify-default-headers))

(defun clockify--user-endpoint ()
  "Clockify user endpoint."
  (concat clockify-api-url "/user"))

(defun clockify--workspaces-endpoint ()
  "Clockify workspaces endpoint."
  (concat clockify-api-url "/workspaces"))

(defun clockify--projects-endpoint (workspace-id)
  "Clockify projects endpoint.
WORKSPACE-ID the workspace id."
  (concat clockify-api-url (concat "/workspaces/"
				   workspace-id
				   "/projects")))

(defun clockify--user-entries-endpoint (workspace-id user-id)
  "Clockify time entries endpoint.
WORKSPACE-ID the workspace id.
USER-ID the user id"
  (concat clockify-api-url (concat "/workspaces/"
				   workspace-id
				   "/user/"
           user-id
           "/time-entries")))

(defun clockify--workspace-entries-endpoint (workspace-id)
  "Clockify time entries endpoint.
WORKSPACE-ID the workspace id."
  (concat clockify-api-url (concat "/workspaces/"
				   workspace-id
           "/time-entries")))

(defun clockify--build-last-entries-names (last-entries-list)
  "Build distinct entries names based on clockify last entries list.
LAST-ENTRIES-LIST the last entries list returned by the clockify API request."
  (let (entries-descriptions)
    (mapcar (lambda (entry)
              (setq entries-descriptions
                    (cons (cdr (assoc 'description entry))
                          entries-descriptions))
              ) last-entries-list)
    (delete-dups entries-descriptions)))

(defun clockify--build-last-entries-details (last-entries-list)
  "Build distinct entries details based on clockify last entries list.
The entries details are in the format (DESCRIPTION . PROJECTID).
LAST-ENTRIES-LIST the last entries list returned by the clockify API request."
  (let (last-entries-compact)
    (mapcar (lambda (entry)
              (setq last-entries-compact
                    (cons
                     `(,(cdr (assoc 'description entry)) . ,(cdr (assoc 'projectId  entry)))
                          last-entries-compact))
              ) last-entries-list)
    (delete-dups last-entries-compact)))

;;; Functions

(defun clockify--error-fn (&rest args &key error-thrown &allow-other-keys)
  "Callback to run in case of error request response.
ERROR-THROWN is the request response data."
  (message "Got error: %S" error-thrown))

(defun clockify--query (method endpoint &optional data params)
  "Send queries to Clockify APIs.
METHOD is the HTTP method to user
ENDPOINT id the API to hit
DATA is the alist to be json encoded for the body of the request (optional)
PARAMS is the alist to be url encoded for the request (optional)"
  (message (concat "Sending request to: " endpoint))
  (message (concat "with params: " params))
  (message (concat "and with data: " (json-encode data)))
  (let ((response (request-response-data
		   (request endpoint
			    :type method
          :data (json-encode data)
          :params params
			    :headers (clockify--generate-headers)
			    :sync t
			    :parser 'json-read
			    :error 'clockify--error-fn))))
    response))

(defun clockify--user-info ()
  "Retrieve user information."
  (let ((response (clockify--query
		   "GET"
		   (clockify--user-endpoint))))
    (setq clockify--current-user-id (cdr (assoc 'id response)))
    (setq clockify--active-workspace-id (cdr (assoc 'activeWorkspace response)))))

(defun clockify--projects ()
  "Retrieve current active workspace."
  (let ((response (clockify--query
		   "GET"
		   (clockify--projects-endpoint clockify--active-workspace-id))))
    response))

(defun clockify--time-entries ()
  "Retrieve last time entries (default 50)."
  (let ((response (clockify--query
                   "GET"
                   (clockify--user-entries-endpoint clockify--active-workspace-id clockify--current-user-id ))))
                   response))

(defun clockify--time-entries-desc-list ()
  "Retrieve last time entries descriptions (default 50)."
  (clockify--build-last-entries-names (clockify--time-entries)))

(defun clockify--time-entries-details-list ()
  "Retrieve last time entries details (default 50).
The entries details are in the format (DESCRIPTION . PROJECTID)."
  (clockify--build-last-entries-details (clockify--time-entries)))

(defun clockify--ongoing-entry ()
  "Retrieve ongoing time entry."
  (let ((response (clockify--query
                   "GET"
                   (clockify--user-entries-endpoint clockify--active-workspace-id clockify--current-user-id )
                   nil
                   '(("in-progress" . "true")))))
                   response))

(defun clockify--add-entry (description &optional projectId)
  "Add a new time entry to clockify.
DESCRIPTION is the description text of the new entry.
PROJECTID is the id of the project to associate the new entry with (Optional)"
  (let ((response (clockify--query
                   "POST"
                   (clockify--workspace-entries-endpoint clockify--active-workspace-id)
                   `(("start" . ,(clockify--current-time-ISO8601))
                     ("description" . ,description)
                     ("projectId" . ,projectId)
                     ("billable" . "true"))))
                   response)))

(defun clockify--stop-entry ()
  "Stop ongoing time entry."
  (interactive)
  (let ((response (clockify--query
                   "PATCH"
                   (clockify--user-entries-endpoint clockify--active-workspace-id clockify--current-user-id)
                   `(("end" . ,(clockify--current-time-ISO8601))))))
                   response))


(defun clockify--restart-previous-entry-no-project ()
  "Prompt user with a list of previous time entries to restart one.
The new entry will be added without atributing a project."
  (interactive)
  (clockify--add-entry (completing-read "Restart previous entry without project:" (clockify--time-entries-desc-list))))

(defun clockify--restart-previous-entry ()
  "Prompt user with a list of previous time entries to restart one."
  (interactive)
  (let* ((time-entries (clockify--time-entries-details-list))
        (selection (completing-read "Restart previous entry:" time-entries)))
    (clockify--add-entry selection
                         (cdr (assoc selection time-entries)))))

(defun clockify--start-new-entry ()
  "Prompt user for a description for a new time entry.
A project to associate the new entry will be prompt with a list."
  (interactive)
  (let ((description (read-string "Enter new entry description: ")))
    (clockify--add-entry description)))

(provide 'clockify)
;;; clockify.el ends here
