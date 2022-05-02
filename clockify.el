;;; clockify.el --- Start, stop and create Clockify tasks -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Marco Dalla Stella

;; Author: Marco Dalla Stella <marco@dallastella.name>
;; Keywords: calendar, tools
;; Package-Requires: ((emacs "25") (projectile "0") (request "0"))
;; Version: 0.0.1
;; Homepage: https://github.com/mdallastella/clockify.el

;;; Commentary:

;;; Code:

(require 'json)
(require 'request)
(require 'cl-lib)

(defvar clockify--curent-user-id nil)
(defvar clockify--active-workspace-id nil)

;;; User-Configurable Variables
(defgroup clockify nil
  "Manage Clockify tasks from Emacs"
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

;;; Functions

(defun clockify--error-fn (&rest args &key error-thrown &allow-other-keys)
  "Callback to run in case of error request response.
ERROR-THROWN is the request response data."
  (message "Got error: %S" error-thrown))

(defun clockify--query (method endpoint &optional data)
  "Send queries to Clockify APIs.
METHOD is the HTTP method to user
ENDPOINT id the API to hit
DATA is the optional body of the request"
  (let ((response (request-response-data
		   (request endpoint
			    :type method
			    :headers (clockify--generate-headers)
			    :sync t
			    :parser 'json-read
			    :error 'clockify--error-fn
          ))))
    response))

(defun clockify--user-info ()
  "Retrieve user information."
  (let ((response (clockify--query
		   "GET"
		   (clockify--user-endpoint))))
    (setq clockify--curent-user-id (cdr (assoc 'id response)))
    (setq clockify--active-workspace-id (cdr (assoc 'activeWorkspace response)))))

(defun clockify--projects ()
  "Retrieve current active workspace."
  (let ((response (clockify--query
		   "GET"
		   (clockify--projects-endpoint clockify--active-workspace-id))))
    response))



(provide 'clockify)
;;; clockify.el ends here
