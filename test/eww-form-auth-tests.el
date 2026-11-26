;;; eww-form-auth-tests.el --- Tests for eww-form-auth  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Teodor Zlatanov

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

;; Pages are rendered with eww's own renderer from DOM fixtures (the
;; shape `libxml-parse-html-region' returns), so the `eww-form' text
;; properties are the real ones and no libxml2 build is needed.  auth-source reads a
;; temporary netrc file; nothing of the user's is touched.
;;
;;   emacs -Q --batch -L . -L test -l test/eww-form-auth-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'eww-form-auth)

(defconst eww-form-auth-test-login-page
  '(html nil
         (body nil
               (h1 nil "Sign in")
               (form ((method . "post") (action . "/session"))
                     (input ((type . "hidden") (name . "csrf") (value . "tok123")))
                     (label nil "Email " (input ((type . "email") (name . "email") (size . "20"))))
                     (label nil "Password " (input ((type . "password") (name . "pw") (size . "20"))))
                     (input ((type . "submit") (name . "go") (value . "Sign in"))))))
  "A login page with a login field before the password field.
A DOM as `libxml-parse-html-region' would return it, so the tests do
not need an Emacs built with libxml2.")

(defconst eww-form-auth-test-search-then-login-page
  '(html nil
         (body nil
               (form ((method . "get") (action . "/search"))
                     (input ((type . "text") (name . "q"))))
               (form ((method . "post") (action . "/login"))
                     (input ((type . "text") (name . "remember") (value . "")))
                     (input ((type . "text") (name . "username")))
                     (input ((type . "password") (name . "password"))))))
  "Two forms; the login form has a decoy text field before the username.")

(defconst eww-form-auth-test-no-password-page
  '(html nil (body nil (form nil (input ((type . "text") (name . "q"))))))
  "A page with a form but no password field.")

(defun eww-form-auth-test--render (dom url)
  "Render DOM as eww would for URL in the current buffer.
`eww-display-document' refuses to run without libxml2 although it only
walks the DOM it is given; on a build without libxml2 the check is
satisfied with a stand-in that is never called (the fixtures are DOMs)."
  (eww-mode)
  (let ((inhibit-read-only t)
        ;; batch has no window and no fonts: without these shr measures
        ;; every character as 0 px and wraps after each one
        (shr-use-fonts nil)
        (shr-width 80))
    (erase-buffer)
    (setq eww-data (list :url url))
    (cl-letf (((symbol-function 'libxml-parse-html-region)
               (or (and (fboundp 'libxml-parse-html-region)
                        (symbol-function 'libxml-parse-html-region))
                   (lambda (&rest _) (error "Not parsed in tests")))))
      (eww-display-html 'utf-8 url (eww-document-base url dom) (point-min) (current-buffer)))
    (goto-char (point-min))))

(defmacro eww-form-auth-test-with-netrc (lines &rest body)
  "Run BODY with `auth-sources' set to a temporary netrc holding LINES."
  (declare (indent 1))
  `(let* ((file (make-temp-file "eww-form-auth-test" nil ".authinfo"))
          (auth-sources (list file))
          (auth-source-do-cache nil)
          ;; save without the "Save auth info to file?" question
          (auth-source-save-behavior t))
     (unwind-protect
         (progn (with-temp-file file (insert (mapconcat #'identity ,lines "\n") "\n"))
                ,@body)
       (delete-file file))))

(defun eww-form-auth-test--field (name)
  "The (POSITION . PLIST) of the input named NAME."
  (seq-find (lambda (i) (equal (plist-get (cdr i) :name) name))
            (eww-form-auth--inputs-in-buffer)))

(ert-deftest eww-form-auth-test-find-form ()
  "The password field and the login field before it are found."
  (with-temp-buffer
    (eww-form-auth-test--render eww-form-auth-test-login-page "https://example.com/signin")
    (pcase-let ((`(,login . ,password) (eww-form-auth-find-form)))
      (should (equal "email" (plist-get (cdr login) :name)))
      (should (equal "pw" (plist-get (cdr password) :name)))))
  (with-temp-buffer
    (eww-form-auth-test--render eww-form-auth-test-search-then-login-page "https://example.com/")
    ;; the search form has no password field; the login form's named
    ;; username wins over the decoy text field
    (pcase-let ((`(,login . ,password) (eww-form-auth-find-form)))
      (should (equal "username" (plist-get (cdr login) :name)))
      (should (equal "password" (plist-get (cdr password) :name))))))

(ert-deftest eww-form-auth-test-fill ()
  "Fields are filled from the netrc entry for the page's host; the password shows as asterisks."
  (eww-form-auth-test-with-netrc '("machine example.com login me@example.com password s3cret"
                              "machine other.org login nobody password nope")
    (with-temp-buffer
      (eww-form-auth-test--render eww-form-auth-test-login-page "https://example.com/signin")
      (eww-form-auth-fill)
      (should (equal "me@example.com" (plist-get (cdr (eww-form-auth-test--field "email")) :value)))
      (should (equal "s3cret" (plist-get (cdr (eww-form-auth-test--field "pw")) :value)))
      ;; the buffer text carries the values (eww submits from :value, but
      ;; the text is what a later edit starts from)
      (let ((pw (eww-form-auth-test--field "pw")))
        (should (string-prefix-p "s3cret" (buffer-substring-no-properties (car pw) (+ (car pw) 6))))
        (should (equal "******" (get-text-property (car pw) 'display))))
      ;; point is left on the password field, where RET submits
      (should (equal "pw" (plist-get (get-text-property (point) 'eww-form) :name))))))

(ert-deftest eww-form-auth-test-fill-submits-what-eww-submits ()
  "After a fill, `eww-submit' posts the login, the password and the hidden token."
  (eww-form-auth-test-with-netrc '("machine example.com login me@example.com password s3cret")
    (with-temp-buffer
      (eww-form-auth-test--render eww-form-auth-test-login-page "https://example.com/signin")
      (let (posted-url posted-data)
        (cl-letf (((symbol-function 'eww-browse-url)
                   (lambda (url &rest _) (setq posted-url url posted-data url-request-data))))
          (let ((eww-form-auth-submit-after-fill t))
            (eww-form-auth-fill)))
        (should (equal "https://example.com/session" posted-url))
        (should (string-match-p "email=me%40example\\.com" posted-data))
        (should (string-match-p "pw=s3cret" posted-data))
        (should (string-match-p "csrf=tok123" posted-data))))))

(ert-deftest eww-form-auth-test-fill-errors ()
  "No password field, or no entry for the host, is a `user-error', not a change."
  (eww-form-auth-test-with-netrc '("machine other.org login nobody password nope")
    (with-temp-buffer
      (eww-form-auth-test--render eww-form-auth-test-login-page "https://example.com/signin")
      (should-error (eww-form-auth-fill) :type 'user-error)
      (should (equal "" (plist-get (cdr (eww-form-auth-test--field "pw")) :value))))
    (with-temp-buffer
      (eww-form-auth-test--render eww-form-auth-test-no-password-page "https://other.org/")
      (should-error (eww-form-auth-fill) :type 'user-error))))

(ert-deftest eww-form-auth-test-several-entries-ask ()
  "Two entries for one host: the user picks, and the pick is used."
  (eww-form-auth-test-with-netrc '("machine example.com login alice password a1"
                              "machine example.com login bob password b2")
    (with-temp-buffer
      (eww-form-auth-test--render eww-form-auth-test-login-page "https://example.com/signin")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (should (equal '("alice" "bob") (sort (copy-sequence choices) #'string<)))
                   "bob")))
        (eww-form-auth-fill))
      (should (equal "bob" (plist-get (cdr (eww-form-auth-test--field "email")) :value)))
      (should (equal "b2" (plist-get (cdr (eww-form-auth-test--field "pw")) :value))))))

(ert-deftest eww-form-auth-test-save ()
  "Typed credentials are written to the netrc through auth-source."
  (eww-form-auth-test-with-netrc '("machine other.org login nobody password nope")
    (with-temp-buffer
      (eww-form-auth-test--render eww-form-auth-test-login-page "https://example.com/signin")
      ;; type into the fields as a user would
      (eww-form-auth--set-field (eww-form-auth-test--field "email") "carol@example.com")
      (eww-form-auth--set-field (eww-form-auth-test--field "pw") "c3")
      (eww-form-auth-save)
      (let ((found (car (auth-source-search :host "example.com" :user "carol@example.com" :max 1))))
        (should found)
        (let ((secret (plist-get found :secret)))
          (should (equal "c3" (if (functionp secret) (funcall secret) secret)))))
      ;; a second save of the same pair does not duplicate it
      (eww-form-auth-save)
      (should (= 1 (length (auth-source-search :host "example.com" :max 10)))))))

(ert-deftest eww-form-auth-test-save-refuses-empty ()
  "An empty login or password is refused."
  (eww-form-auth-test-with-netrc nil
    (with-temp-buffer
      (eww-form-auth-test--render eww-form-auth-test-login-page "https://example.com/signin")
      (should-error (eww-form-auth-save) :type 'user-error)
      (eww-form-auth--set-field (eww-form-auth-test--field "email") "x")
      (should-error (eww-form-auth-save) :type 'user-error))))

(provide 'eww-form-auth-tests)
;;; eww-form-auth-tests.el ends here
