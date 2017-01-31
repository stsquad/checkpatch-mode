;;; checkpatch-mode --- A simple mode for the results of checkpatch scripts
;;
;; Copyright (C) 2014 Alex Bennée

;; Author: Alex Bennée <alex.bennee@linaro.org>
;; Maintainer: Alex Bennée <alex.bennee@linaro.org>
;; Version: 0.1
;; Homepage: http://github.com/stsquad/checkpatch-mode

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
  "A regular expressions for `compilation-error-regexp-alist-alist'"
  '(checkpatch
    "\\(WARNING\\|ERROR\\).*\n#.*FILE: \\([^:]+\\):\\([^:digit:]+\\).*\n.*"
    2 ; file
    3 ; line
    ))

(defvar checkpatch-script-path
  "Path to the default checkpatch script."
  nil)
(make-variable-buffer-local 'checkpatch-script-path)
(put 'checkpatch-script-path 'permanent-local t)

;; Helpers

(defun checkpatch-mode-update-error-regexp ()
  "Make sure the error regexp is upto date."
  (add-to-list
   'compilation-error-regexp-alist-alist checkpatch-mode-regex)
  (add-to-list 'compilation-error-regexp-alist 'checkpatch))

;;; Mode magic
(defvar checkpatch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'checkpatch-mode-done)
    map)
  "Keymap for major mode `checkpatch-mode'.")

;; Launch functions
(defun run-checkpatch (&optional script file)
  "Run the checkpatch script against `COMMIT'."
  (interactive)
  (unless script
    (setq script checkpatch-script-path))
  (unless file
    (setq file (buffer-file-name)))
  (let ((proc-name "checkpatch")
        (buff-name (format "checkpatch-%s" file)))
    (start-process-shell-command
     proc-name
     buff-name
     (format "%s %s" script file))
    (switch-to-buffer buff-name)
    (goto-char (point-min))
    (checkpatch-mode)))

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

