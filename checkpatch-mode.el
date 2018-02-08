;;; checkpatch-mode.el --- Support for checkpatch
;;
;; Copyright (C) 2014-16 Alex Bennée

;; Author: Alex Bennée <alex.bennee@linaro.org>
;; Maintainer: Alex Bennée <alex.bennee@linaro.org>
;; Version: 0.1
;; Homepage: http://github.com/stsquad/checkpatch-mode
;; Package-Requires: ((emacs "24.3"))


;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This provides a simple compile-mode derived mode for checkpatch output.
;;
;;; Code:

;; Require prerequisites

(require 'compile) ; for compilation-mode

;; Variables

;; Match checkpatch.pl output
(defvar checkpatch-mode-regex
  '(checkpatch
    "\\(WARNING\\|ERROR\\).*\n#.*FILE: \\([^:]+\\):\\([^:digit:]+\\).*\n.*"
    2 ; file
    3 ; line
    )
  "A regular expressions for `compilation-error-regexp-alist-alist'")

(defvar checkpatch-script-path
  nil
  "Path to the default checkpatch script.")
(make-variable-buffer-local 'checkpatch-script-path)
(put 'checkpatch-script-path 'permanent-local t)

(defvar checkpatch-result
  nil
  "Result of last checkpatch call.")
(make-variable-buffer-local 'checkpatch-result)
(put 'checkpatch-result 'permanent-local t)


;; Helpers

(defun checkpatch-mode-update-error-regexp ()
  "Make sure the error regexp is upto date."
  (add-to-list
   'compilation-error-regexp-alist-alist checkpatch-mode-regex)
  (add-to-list 'compilation-error-regexp-alist 'checkpatch))

(defun checkpatch-mode-done()
  "Bury or destroy the checkpatch buffer"
  (interactive)
  (when (eq major-mode 'checkpatch-mode)
    (if (eq checkpatch-result 0)
        (kill-buffer)
      (bury-buffer))))

;;; Mode magic
(defvar checkpatch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'checkpatch-mode-done)
    (define-key map (kbd "C-c C-c") 'checkpatch-mode-done)
    map)
  "Keymap for major mode `checkpatch-mode'.")

;; Launch functions
(defun checkpatch-run (script file &optional file-is-patch)
  "Run the checkpatch `SCRIPT' against `FILE'."
  (interactive)
  (let ((proc-name "checkpatch")
        (buff-name (format "*checkpatch-%s*" (file-name-base file))))
    (switch-to-buffer buff-name)
    (goto-char (point-min))
    (erase-buffer)
    (setq checkpatch-result
          (if file-is-patch
              (call-process script nil t t file)
            (call-process script nil t t "-f" file)))
    (checkpatch-mode)))

(defun checkpatch-run-against-commit (script commit)
  "Run the checkpatch `SCRIPT' against `COMMIT'."
  (let ((proc-name "checkpatch")
        (buff-name (format "checkpatch-%s" commit)))
    (start-process-shell-command
     proc-name
     buff-name
     (format "git show --pretty=email %s | %s -" commit script))
    (switch-to-buffer buff-name)
    (goto-char (point-min))
    (checkpatch-mode)))

(defun checkpatch-find-script ()
  "Find checkpatch script or return nil if we can't find it."
  (cond
   ;; Is checkpatch-script-path already set and correct?
   ((and checkpatch-script-path
         (file-exists-p checkpatch-script-path))
    checkpatch-script-path)
   ;; Can we find it in relation to current buffer-file-name?
   ((and buffer-file-name
         (locate-dominating-file
          (buffer-file-name) "scripts/checkpatch.pl"))
    (concat (locate-dominating-file
             (buffer-file-name) "scripts/checkpatch.pl")
            "scripts/checkpatch.pl"))
   ;; What about w.r.t default-directory
   ((and default-directory
         (locate-dominating-file
          default-directory "scripts/checkpatch.pl"))
    (concat (locate-dominating-file
             default-directory "scripts/checkpatch.pl")
            "scripts/checkpatch.pl"))
   (t nil)))

(defun checkpatch-find-script-or-prompt ()
  "Find checkpatch script or prompt the user if not found."
  (interactive)
  (let ((script (checkpatch-find-script)))
    (unless script
      (setq script
            (ido-read-file-name
              "Checkpatch Script: " default-directory)))
    (setq checkpatch-script-path script)))

(defun checkpatch-run-against-patch-file (patch-file)
  "Run checkpatch against `PATCH-FILE'."
  (interactive)
  (let ((script (checkpatch-find-script-or-prompt)))
    (checkpatch-run script patch-file t)))

(defun checkpatch-run-against-file (&optional file)
  "Run checkpatch against `FILE'.
If `FILE' is not set assume it is the file of the current buffer."
  (interactive)
  (let ((script (checkpatch-find-script-or-prompt)))
    (if (not file)
        (setq file (buffer-file-name)))
    (checkpatch-run script file)))

;; Run from inside magit
(defun checkpatch-run-from-magit ()
  "Run a checkpatch script against current magit commit."
  (interactive)
  (let ((commit (magit-commit-at-point)))
    (when (and commit
               (checkpatch-find-script-or-prompt))
      (checkpatch-run-against-commit checkpatch-script-path commit))))

(defun checkpatch-magit-hook ()
  "Hook checkpatch commands into magit.

This is intended to be run from various magit-mode hooks and will add
a keybinding to the local magit to call checkpatch if the script is
found."
  (when (checkpatch-find-script)
    (local-set-key (kbd "C") 'checkpatch-run-from-magit)))

(eval-after-load 'magit
  (add-hook 'magit-log-mode-hook 'checkpatch-magit-hook))


;; Define the mode
;;###autoload
(define-derived-mode checkpatch-mode compilation-mode "CHKPTCH"
  "A simple mode for the results of checkpatch scripts .

\{checkpatch-mode-map}"
  :lighter " CHKPTCH"
  (checkpatch-mode-update-error-regexp)
  (message "in derived mode"))

(provide 'checkpatch-mode)
;;; checkpatch-mode.el ends here

