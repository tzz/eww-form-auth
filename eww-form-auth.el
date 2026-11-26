;;; eww-form-auth.el --- Fill eww login forms from auth-source  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Maintainer: Teodor Zlatanov <tzz@lifelogs.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: hypermedia, comm
;; URL: https://github.com/tzz/eww-form-auth

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; Emacs keeps passwords in auth-source (~/.authinfo.gpg, pass, the
;; macOS keychain), and url.el already answers HTTP Basic and Digest
;; challenges from it.  HTML login forms in eww get nothing: a
;; `password' input is a text field you type into.  This library fills
;; that gap.
;;
;;   M-x eww-form-auth-fill   (C-c C-a in eww; C-c C-s saves)
;;
;; The name: auth-source is Emacs's credential store; url.el already
;; serves HTTP Basic/Digest challenges from it.  This library serves
;; HTML *forms* in eww, hence form-auth, not HTTP auth.
;;
;; finds the login form on the page (the form under point, else the
;; first form with a password field), looks the page's host up in
;; auth-source, writes the login into the text field before the
;; password field and the secret into the password field, and with a
;; prefix argument submits.  Several entries for one host are offered
;; with completion.  The entry is the ordinary one:
;;
;;   machine example.com login me@example.com password s3cret
;;
;; `eww-form-auth-save' goes the other way: after you typed a login and a
;; password into a form, it stores them for the host through
;; auth-source's own saving (`auth-source-search' with :create), so the
;; file or backend you configured is used.
;;
;; Limits are eww's: a site whose login runs through JavaScript, or
;; needs a token a script fetches, cannot be logged into from eww, with
;; or without this library.

;;; Code:

(require 'auth-source)
(require 'eww)
(require 'url-parse)
(require 'seq)

(defgroup eww-form-auth nil
  "Fill eww login forms from auth-source."
  :group 'eww)

(defcustom eww-form-auth-login-types '("text" "email" "tel")
  "Input types that can hold the login name of a form."
  :type '(repeat string))

(defcustom eww-form-auth-login-names
  '("user" "username" "login" "email" "account" "uid" "id" "name")
  "Input names (substrings, case-insensitive) that mark a login field.
Used to choose among several text fields; the field just before the
password field wins when none matches."
  :type '(repeat string))

(defcustom eww-form-auth-submit-after-fill nil
  "When non-nil, `eww-form-auth-fill' submits the form after filling it.
A prefix argument to `eww-form-auth-fill' inverts this for one call."
  :type 'boolean)

;;;; Finding the form and its fields

(defun eww-form-auth--inputs-in-buffer ()
  "Every input of the page as (POSITION . PLIST), in buffer order.
PLIST is the `eww-form' text property: :eww-form, :name, :type, :value."
  (let ((pos (point-min)) inputs)
    (while (and pos (< pos (point-max)))
      (let ((input (get-text-property pos 'eww-form)))
        (when (and input (plist-get input :type))
          (push (cons pos input) inputs)))
      (setq pos (next-single-property-change pos 'eww-form)))
    (nreverse inputs)))

(defun eww-form-auth--password-input-p (input)
  "Whether INPUT (an `eww-form' plist) is a password field."
  (equal (plist-get input :type) "password"))

(defun eww-form-auth--login-candidate-p (input)
  "Whether INPUT can hold a login name."
  (member (plist-get input :type) eww-form-auth-login-types))

(defun eww-form-auth--login-name-score (input)
  "How strongly INPUT's name says \"login\".
The index of the first match in `eww-form-auth-login-names', or nil."
  (when-let* ((name (plist-get input :name)))
    (seq-position eww-form-auth-login-names (downcase name)
                  (lambda (pattern n) (string-search pattern n)))))

(defun eww-form-auth-find-form (&optional pos)
  "The login form at POS (default point), or the first one on the page.
Return (LOGIN . PASSWORD), each (POSITION . PLIST), LOGIN possibly nil
when the form has only a password field.  Return nil when the page has
no password field."
  (let* ((all (eww-form-auth--inputs-in-buffer))
         (here (get-text-property (or pos (point)) 'eww-form))
         (form-at-point (and here (plist-get here :eww-form)))
         (password
          (or (and form-at-point
                   (seq-find (lambda (i) (and (eq (plist-get (cdr i) :eww-form) form-at-point)
                                              (eww-form-auth--password-input-p (cdr i))))
                             all))
              (seq-find (lambda (i) (eww-form-auth--password-input-p (cdr i))) all))))
    (when password
      (let* ((form (plist-get (cdr password) :eww-form))
             (texts (seq-filter (lambda (i) (and (eq (plist-get (cdr i) :eww-form) form)
                                                 (eww-form-auth--login-candidate-p (cdr i))))
                                all))
             ;; prefer a text field named like a login; else the last one
             ;; before the password field; else any
             (named (seq-filter (lambda (i) (eww-form-auth--login-name-score (cdr i))) texts))
             (login (or (car (sort (copy-sequence named)
                                   (lambda (a b) (< (eww-form-auth--login-name-score (cdr a))
                                                    (eww-form-auth--login-name-score (cdr b))))))
                        (car (last (seq-filter (lambda (i) (< (car i) (car password))) texts)))
                        (car texts))))
        (cons login password)))))

;;;; Writing into fields

(defun eww-form-auth--set-field (input-entry value)
  "Put VALUE into the field INPUT-ENTRY, (POSITION . PLIST), as if typed.
The buffer text, the field's :value and, for a password field, the
asterisk display are updated the way `eww-process-text-input' does.
The field keeps its width: `eww-update-field' sizes the field to the
string it is given, so VALUE is padded with spaces to the current
width (or grows the field when longer)."
  (pcase-let ((`(,pos . ,input) input-entry))
    (save-excursion
      (goto-char pos)
      (let* ((inhibit-read-only t)
             (width (1+ (- (eww-end-of-field) (eww-beginning-of-field))))
             (padded (if (< (length value) width)
                         (concat value (make-string (- width (length value)) ?\s))
                       value))
             (start (eww-update-field padded)))
        (plist-put input :value value)
        (remove-text-properties start (+ start width) '(display nil))
        (when (eww-form-auth--password-input-p input)
          (put-text-property start (+ start (length value))
                             'display (make-string (length value) ?*)))))))

;;;; auth-source

(defun eww-form-auth-host ()
  "The host of the page in the current eww buffer."
  (let ((url (eww-current-url)))
    (unless url (user-error "Eww-form-auth: no page in this buffer"))
    (url-host (url-generic-parse-url url))))

(defun eww-form-auth-credentials (host &optional user)
  "The (USER . SECRET) auth-source holds for HOST, or nil.
With several entries and no USER, ask which.  USER narrows the search."
  (let* ((entries (auth-source-search :host host :max 10
                                      :require '(:user :secret)
                                      :user user))
         (entry (cond ((null entries) nil)
                      ((null (cdr entries)) (car entries))
                      (t (let ((pick (completing-read
                                      (format "Login for %s: " host)
                                      (mapcar (lambda (e) (plist-get e :user)) entries)
                                      nil t)))
                           (seq-find (lambda (e) (equal (plist-get e :user) pick)) entries))))))
    (when entry
      (let ((secret (plist-get entry :secret)))
        (cons (plist-get entry :user)
              (if (functionp secret) (funcall secret) secret))))))

;;;; Commands

;;;###autoload
(defun eww-form-auth-fill (&optional submit)
  "Fill the login form on this page from auth-source.
The form under point is used, else the first form with a password
field.  The host of the page selects the entry; with several entries,
choose one.  With SUBMIT (a prefix argument, or
`eww-form-auth-submit-after-fill'), submit the form afterwards."
  (interactive "P" eww-mode)
  (let* ((host (eww-form-auth-host))
         (fields (or (eww-form-auth-find-form)
                     (user-error "Eww-form-auth: no password field on this page")))
         (creds (or (eww-form-auth-credentials host)
                    (user-error "Eww-form-auth: no auth-source entry for %s" host))))
    (when (car fields)
      (eww-form-auth--set-field (car fields) (car creds)))
    (eww-form-auth--set-field (cdr fields) (cdr creds))
    (goto-char (car (cdr fields)))
    (if (if submit (not eww-form-auth-submit-after-fill) eww-form-auth-submit-after-fill)
        (eww-submit)
      (message "eww-form-auth: filled %s%s for %s; RET on the password field submits"
               (if (car fields) "login and " "") "password" host))))

;;;###autoload
(defun eww-form-auth-save ()
  "Store the login and password typed into the form on this page.
Saved for the page's host through auth-source, into the backend that
accepts new entries (the first file in `auth-sources', unless another
backend takes it).  Nothing is stored when either field is empty."
  (interactive nil eww-mode)
  (let* ((host (eww-form-auth-host))
         (fields (or (eww-form-auth-find-form)
                     (user-error "Eww-form-auth: no password field on this page")))
         (user (and (car fields) (string-trim (or (plist-get (cdr (car fields)) :value) ""))))
         (secret (string-trim (or (plist-get (cdr (cdr fields)) :value) ""))))
    (when (or (null user) (string-empty-p user)) (user-error "Eww-form-auth: the login field is empty"))
    (when (string-empty-p secret) (user-error "Eww-form-auth: the password field is empty"))
    (let* ((entry (car (auth-source-search :host host :user user :secret secret
                                           :max 1 :create t)))
           (save (and entry (plist-get entry :save-function))))
      (cond ((null entry) (user-error "Eww-form-auth: no auth-source backend accepts new entries"))
            (save (funcall save)
                  (message "eww-form-auth: saved %s for %s" user host))
            (t (message "eww-form-auth: %s for %s already stored" user host))))))

;;;; Keys

(defcustom eww-form-auth-bind-keys t
  "When non-nil, bind the eww-form-auth commands in `eww-mode-map' at load.
Control-c Control-a runs `eww-form-auth-fill'; Control-c Control-s runs
`eww-form-auth-save'."
  :type 'boolean)

(defun eww-form-auth--install-keys ()
  "Bind the eww-form-auth commands in `eww-mode-map' per `eww-form-auth-bind-keys'."
  (when eww-form-auth-bind-keys
    (define-key eww-mode-map (kbd "C-c C-a") #'eww-form-auth-fill)
    (define-key eww-mode-map (kbd "C-c C-s") #'eww-form-auth-save)))

(with-eval-after-load 'eww (eww-form-auth--install-keys))
;; If eww is already loaded, the form above ran immediately.

(provide 'eww-form-auth)
;;; eww-form-auth.el ends here
