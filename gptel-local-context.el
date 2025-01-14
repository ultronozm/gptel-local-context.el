;;; gptel-local-context.el --- local context for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Paul Nelson

;; Author: Paul Nelson <ultrono@gmail.com>
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1") (transient "0.7.4") (gptel "0.9.7"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Buffer-local context for `gptel', intended for the discussion at
;; https://github.com/karthink/gptel/issues/475

;;; Code:

(require 'gptel)
(require 'gptel-context)
(require 'gptel-transient)
(require 'project)
(require 'org)

(defvar-local gptel-local-context nil
  "List of additional context sources for `gptel'.
Each element should be a string naming a buffer, file, or function.")

;;; Extracting local context

(defun gptel-local-context--find-project-file (filename)
  "Find FILENAME in current project."
  (when-let* ((project (project-current))
              (file (seq-find
                     (lambda (f)
                       (string= (file-name-nondirectory f) filename))
                     (project-files project))))
    file))

(defun gptel-local-context--try-function (source)
  "Try to extract content from SOURCE as a function."
  (when-let ((func (intern-soft source)))
    (when (functionp func)
      (insert (format "Function %s:\n\n```\n%s\n```\n"
                      source
                      (condition-case err
                          (funcall func)
                        (error (format "Error: %S" err))))))))

(defun gptel-local-context--extract-source-content (source)
  "Get wrapped content from SOURCE.
SOURCE can be a buffer name, file path, or function name.  Return a
string containing the wrapped content."
  (with-temp-buffer
    (cond
     ((get-buffer source)
      (let* ((buf (get-buffer source))
             (ov (with-current-buffer buf
                   (make-overlay (point-min) (point-max)))))
        (unwind-protect
            (gptel-context--insert-buffer-string
             buf (list ov))
          (delete-overlay ov))))
     ((or (file-exists-p source)
          ;; If the file doesn't exist in an absolute sense, try to
          ;; find it in the current project.
          (when-let ((project-file (gptel-local-context--find-project-file source)))
            (setq source project-file)))
      (gptel-context--insert-file-string source))
     ((gptel-local-context--try-function source)))
    (and (> (buffer-size) 0)
         (buffer-string))))

;;; Advice

(defun gptel-local-context--advice (context-string request-buffer)
  "Process local context for gptel requests.
CONTEXT-STRING is the original context string.
REQUEST-BUFFER is the buffer where the request originated.
Returns the modified context string with local context added."
  (with-temp-buffer
    (when context-string
      (insert context-string))
    (when-let ((local-context (buffer-local-value 'gptel-local-context request-buffer)))
      (dolist (source local-context)
        (when-let ((content (with-current-buffer request-buffer
                              (gptel-local-context--extract-source-content source))))
          (unless (bobp)
            (insert "\n\n"))
          (insert content))))
    (buffer-string)))


(define-advice gptel-context--string (:around (orig-fun context-alist) local-context)
  "Add local context from `gptel-local-context' to context string."
  (let ((request-buffer (current-buffer)))
    (gptel-local-context--advice (funcall orig-fun context-alist)
                                 request-buffer)))

;;; Helpers

(defun gptel-local-context--get-visible-buffers (&optional exclude-current)
  "Return a list of buffer names that are currently visible.
If EXCLUDE-CURRENT is non-nil, excludes the current buffer.
Always excludes the *transient* buffer."
  (let* ((visible-windows (if exclude-current
                              (seq-remove
                               (lambda (window)
                                 (eq (window-buffer window) (current-buffer)))
                               (window-list))
                            (window-list)))
         (visible-buffers (delete-dups
                           (mapcar #'window-buffer visible-windows))))
    (seq-remove
     (lambda (name)
       (string= name " *transient*"))
     (mapcar #'buffer-name visible-buffers))))

;;; Modify the local context

(defun gptel-local-context-add-visible-buffers ()
  "Add all visible buffers to `gptel-local-context'.
Excludes current buffer."
  (interactive)
  (let ((buffer-names (gptel-local-context--get-visible-buffers t)))
    (setq-local gptel-local-context
                (delete-dups
                 (append gptel-local-context buffer-names)))
    (message "Added %d visible buffer(s) to gptel context"
             (length buffer-names))))

(defun gptel-local-context-add-buffers ()
  "Add selected buffers to `gptel-local-context'."
  (interactive)
  (let ((selected-buffers
         (completing-read-multiple
          "Select buffers to add to gptel context: "
          (mapcar #'buffer-name (buffer-list)))))
    (setq-local gptel-local-context
                (delete-dups
                 (append gptel-local-context selected-buffers)))
    (message "Added %d buffer(s) to gptel context"
             (length selected-buffers))))

(defcustom gptel-local-context-file-wildcard "*"
  "Default wildcard for filtering files when adding directory files.
For example, \"*.py\" for Python files."
  :type 'string
  :group 'gptel)

(defun gptel-local-context-add-files (files)
  "Add FILES to gptel local context.
Prompts for multiple file selections."
  (interactive
   (list (completing-read-multiple
          "Add files to context: "
          #'completion-file-name-table)))
  (let ((added 0))
    (dolist (file files)
      (when (file-exists-p file)
        (let ((rel-file (file-relative-name file)))
          (cl-incf added)
          (setq-local gptel-local-context
                      (delete-dups
                       (append gptel-local-context
                               (list rel-file)))))))
    (message "Added %d file(s) to context" added)))

(defun gptel-local-context-add-directory-files (dir &optional wildcard recursive)
  "Add files from DIR to `gptel-local-context'.
Optional WILDCARD (e.g., \"*.py\") filters files by pattern.
With prefix arg RECURSIVE, include subdirectories recursively.

When called interactively, prompts for a directory, optional wildcard
pattern, and uses the prefix argument to determine if subdirectories
should be included."
  (interactive
   (list (read-directory-name "Select directory: ")
         (read-string "File wildcard (optional, e.g., *.py): " nil nil "*")
         current-prefix-arg))
  (let* ((files (directory-files-recursively
                 dir
                 (if (or (null wildcard) (string= wildcard "*"))
                     ".*"
                   (wildcard-to-regexp wildcard))
                 recursive))
         (relative-files
          (mapcar (lambda (file)
                    (file-relative-name file default-directory))
                  files)))
    (setq-local gptel-local-context
                (delete-dups
                 (append gptel-local-context relative-files)))
    (message "Added %d files from directory %s to gptel context (wildcard: %s)"
             (length relative-files)
             dir
             wildcard)))

(defun gptel-local-context-add-functions ()
  "Add selected function symbols to `gptel-local-context'."
  (interactive)
  (let* ((function-symbols
          (completing-read-multiple
           "Select functions to add to gptel context: "
           (let (symbols)
             (mapatoms
              (lambda (sym)
                (when (fboundp sym)
                  (push (symbol-name sym) symbols))))
             symbols))))
    (setq-local gptel-local-context
                (delete-dups
                 (append gptel-local-context function-symbols)))
    (message "Added %d function(s) to gptel context"
             (length function-symbols))))

(defun gptel-local-context-remove ()
  "Remove entries from `gptel-local-context'."
  (interactive)
  (if (not gptel-local-context)
      (message "No local context entries to remove")
    (let* ((to-remove (completing-read-multiple
                       "Select entries to remove: "
                       gptel-local-context))
           (removed (length to-remove)))
      (setq-local gptel-local-context
                  (cl-set-difference gptel-local-context to-remove
                                     :test #'string=))
      (message "Removed %d context entries" removed))))

(defun gptel-local-context-clear ()
  "Clear all entries from `gptel-local-context'."
  (interactive)
  (if (not gptel-local-context)
      (message "No local context entries to clear")
    (let ((count (length gptel-local-context)))
      (setq-local gptel-local-context nil)
      (message "Cleared %d entries from gptel context" count))))

;;; Transient interface

(defun gptel-local-context--count ()
  "Return string describing number of local context entries."
  (when-let ((local gptel-local-context))
    (format " (%d)" (length local))))

(transient-define-prefix gptel-local-context ()
  "Add or remove global context sources for gptel.
These are included with every gptel request."
  [:description
   (lambda ()
     (concat "Local Context"
             (gptel-local-context--count)
             (when gptel-local-context
               (concat ":\n"
                       (mapconcat
                        (lambda (src)
                          (concat "  " src))
                        gptel-local-context "\n")))))
   [""
    ("v" "Add visible buffers" gptel-local-context-add-visible-buffers)
    ("b" "Add buffers" gptel-local-context-add-buffers)
    ("f" "Add files" gptel-local-context-add-files)
    ("d" "Add directory files" gptel-local-context-add-directory-files)
    ("p" "Add project files" gptel-local-context-add-project-files)
    ("F" "Add functions" gptel-local-context-add-functions)
    ("r" "Remove entries" gptel-local-context-remove)
    ("C" "Clear all" gptel-local-context-clear)
    ("RET" "Return to menu"
     (lambda () (interactive) (gptel-menu))
     :transient t)]])

(transient-define-prefix gptel-menu ()
  "Change parameters of prompt to send to the LLM."
  [:description gptel-system-prompt--format
                [""
                 :if (lambda () (not (gptel--model-capable-p 'nosystem)))
                 "Instructions"
                 ("s" "Set system message" gptel-system-prompt :transient t)
                 (gptel--infix-add-directive)]
                [:pad-keys t
                           ""
                           "Context"
                           (gptel--infix-context-add-region)
                           (gptel--infix-context-add-buffer)
                           (gptel--infix-context-add-file)
                           (gptel--infix-context-remove-all)
                           (gptel--suffix-context-buffer)]
                [:pad-keys t
                           :if (lambda () (and gptel-use-tools gptel--known-tools))
                           "" (:info
                               (lambda () (concat "Tools" (and gptel-tools
                                                               (format " (%d selected)"
                                                                       (length gptel-tools)))))
                               :format "%d" :face transient-heading)
                           ("t" "Select tools" gptel-tools :transient t)]
                [""
                 (:info (lambda () (concat "Local Context" (gptel-local-context--count)))
                        :format "%d" :face transient-heading)
                 ("l" "Manage local context" gptel-local-context :transient t)]
                ]
  [["Request Parameters"
    :pad-keys t
    (gptel--infix-variable-scope)
    (gptel--infix-provider)
    (gptel--infix-max-tokens)
    (gptel--infix-num-messages-to-send
     :if (lambda () (and gptel-expert-commands
                         (or gptel-mode gptel-track-response))))
    (gptel--infix-temperature :if (lambda () gptel-expert-commands))
    (gptel--infix-use-context)
    (gptel--infix-use-tools)
    (gptel--infix-track-response
     :if (lambda () (and gptel-expert-commands (not gptel-mode))))
    (gptel--infix-track-media
     :if (lambda () (and gptel-mode (gptel--model-capable-p 'media))))]
   [" <Prompt from"
    ("m" "Minibuffer instead" "m")
    ("y" "Kill-ring instead" "y")
    ""
    ("i" "Respond in place" "i")]
   [" >Response to"
    ("e" "Echo area" "e")
    ("g" "gptel session" "g"
     :class transient-option
     :prompt "Existing or new gptel session: "
     :reader
     (lambda (prompt _ _history)
       (read-buffer
        prompt (generate-new-buffer-name
                (concat "*" (gptel-backend-name gptel-backend) "*"))
        nil (lambda (buf-name)
              (if (consp buf-name) (setq buf-name (car buf-name)))
              (let ((buf (get-buffer buf-name)))
                (and (buffer-local-value 'gptel-mode buf)
                     (not (eq (current-buffer) buf))))))))
    ("b" "Any buffer" "b"
     :class transient-option
     :prompt "Output to buffer: "
     :reader
     (lambda (prompt _ _history)
       (read-buffer prompt (buffer-name (other-buffer)) nil)))
    ("k" "Kill-ring" "k")]]
  [["Send"
    (gptel--suffix-send)
    ("M-RET" "Regenerate" gptel--regenerate :if gptel--in-response-p)]
   [:description (lambda () (concat (and gptel--rewrite-overlays "Continue ")
                                    "Rewrite"))
                 :if (lambda () (or (use-region-p)
                                    (and gptel--rewrite-overlays
                                         (gptel--rewrite-sanitize-overlays))))
                 ("r"
                  (lambda () (if (get-char-property (point) 'gptel-rewrite)
                                 "Iterate" "Rewrite"))
                  gptel-rewrite)]
   ["Tweak Response" :if gptel--in-response-p :pad-keys t
    ("SPC" "Mark" gptel--mark-response)
    ("P" "Previous variant" gptel--previous-variant
     :if gptel--at-response-history-p
     :transient t)
    ("N" "Next variant" gptel--previous-variant
     :if gptel--at-response-history-p
     :transient t)
    ("E" "Ediff previous" gptel--ediff
     :if gptel--at-response-history-p)]
   ["Dry Run" :if (lambda () (or gptel-log-level gptel-expert-commands))
    ("I" "Inspect query (Lisp)"
     (lambda ()
       "Inspect the query that will be sent as a lisp object."
       (interactive)
       (gptel--sanitize-model)
       (gptel--inspect-query
        (gptel--suffix-send
         (cons "I" (transient-args transient-current-command))))))
    ("J" "Inspect query (JSON)"
     (lambda ()
       "Inspect the query that will be sent as a JSON object."
       (interactive)
       (gptel--sanitize-model)
       (gptel--inspect-query
        (gptel--suffix-send
         (cons "I" (transient-args transient-current-command)))
        'json)))]]
  (interactive)
  (gptel--sanitize-model)
  (transient-setup 'gptel-menu))

;;; Convenience

;;;###autoload
(defun gptel-with-visible-buffers ()
  "Launch gptel and automatically add all visible buffers as local context."
  (interactive)
  (let ((visible-buffers (gptel-local-context--get-visible-buffers))) ; Don't exclude current
    (with-current-buffer (call-interactively #'gptel)
      (setq-local gptel-local-context
                  (delete-dups
                   (append gptel-local-context visible-buffers)))
      (message "Added %d visible buffer(s) to gptel context"
               (length visible-buffers)))))

(defun gptel-local-context-add-project-files (&optional dir wildcard)
  "Add files from a selected project to `gptel-local-context'.
Prompts for the project to use and excludes the current file.  DIR is
the directory of the selected project.  WILDCARD, if provided, is used
to filter the files (e.g., \"*.py\" for Python files)."
  (interactive
   (list (funcall project-prompter)
         (read-string "File wildcard (optional, e.g., *.py): " nil nil "*")))
  (let* ((project (project-current nil dir))
         (current-file (buffer-file-name))
         (project-vc-include-untracked nil)
         (project-ignore-filenames-regexp
          (concat "\\."
                  "\\(png\\|jpg\\|jpeg\\|gif\\|bmp\\|tiff\\|ico\\|svg"
                  "\\|pdf\\|doc\\|docx\\|xls\\|xlsx\\|ppt\\|pptx"
                  "\\|zip\\|tar\\|gz\\|rar\\|7z"
                  "\\|exe\\|dll\\|so\\|dylib"
                  "\\|pyc\\|pyo\\|pyd"
                  "\\|class\\|jar"
                  "\\|mp3\\|mp4\\|avi\\|mov\\|wav"
                  "\\)$")))
    (if (not project)
        (error "No project found for directory %s" dir)
      (let* ((project-files (project-files project))
             (relative-files
              (mapcar (lambda (file)
                        (file-relative-name file default-directory))
                      project-files))
             (filtered-files
              (seq-remove
               (lambda (file)
                 (or (string-match-p project-ignore-filenames-regexp file)
                     (and current-file
                          (string= file (file-relative-name
                                         current-file default-directory)))))
               relative-files))
             (final-files
              (if (or (null wildcard) (string= wildcard "*"))
                  filtered-files
                (let ((regexp (wildcard-to-regexp wildcard)))
                  (seq-filter (lambda (file)
                                (string-match-p regexp file))
                              filtered-files)))))
        (setq-local gptel-local-context
                    (delete-dups
                     (append gptel-local-context final-files)))
        (message "Added %d files from project %s to gptel context (wildcard: %s)"
                 (length final-files)
                 (project-root project)
                 wildcard)))))

;; requires Emacs 28.1+
;;;###autoload
(defun gptel-project (&optional arg)
  "Start a gptel chat buffer in the current project's root directory.
If a buffer already exists for a gptel chat in the project's root,
switch to it.  Otherwise, create a new gptel chat buffer.
With any prefix ARG, create a new chat buffer even if one already exists."
  (interactive "P")
  (let* ((project (project-current t))
         (default-directory (project-root project))
         (default-project-gptel-name (project-prefixed-buffer-name "gptel"))
         (buffer-name (if (and (not arg)
                               (get-buffer default-project-gptel-name))
                          default-project-gptel-name
                        (generate-new-buffer-name default-project-gptel-name)))
         (buffer (gptel buffer-name)))
    (with-current-buffer buffer
      (gptel-local-context-add-project-files))
    (pop-to-buffer buffer)
    (message "Started new gptel project chat in %s" default-directory)))

;;; Saving and loading

(defun gptel-local-context--save-state-advice (orig-fun &rest args)
  "Save gptel-local-context state.
In `org-mode', save as a property at the root level.
In other modes, save as a file-local variable.
Calls ORIG-FUN with ARGS."
  (if (derived-mode-p 'org-mode)
      (progn
        (apply orig-fun args)
        (when (local-variable-p 'gptel-local-context)
          (org-with-wide-buffer
           (org-entry-delete (point-min) "GPTEL_LOCAL_CONTEXT")
           (dolist (item gptel-local-context)
             (org-entry-add-to-multivalued-property
              (point-min) "GPTEL_LOCAL_CONTEXT" item)))))
    (progn
      (apply orig-fun args)
      (when (local-variable-p 'gptel-local-context)
        (add-file-local-variable 'gptel-local-context gptel-local-context)))))

(defun gptel-local-context--restore-state-advice (orig-fun &rest args)
  "Restore `gptel-local-context' state.
From org properties in `org-mode', from file-local vars otherwise.
Calls ORIG-FUN with ARGS."
  (if (derived-mode-p 'org-mode)
      (progn
        (apply orig-fun args)
        (when-let ((context (org-entry-get-multivalued-property
                             (point-min) "GPTEL_LOCAL_CONTEXT")))
          (setq-local gptel-local-context context)))
    (apply orig-fun args)))

(advice-add 'gptel--save-state :around #'gptel-local-context--save-state-advice)
(advice-add 'gptel--restore-state :around #'gptel-local-context--restore-state-advice)


(provide 'gptel-local-context)
;;; gptel-local-context.el ends here
