;;; lterm.el --- Terminal emulator with a line-wise user-interface

;; Copyright (C) 2012  Taylan Ulrich B.

;; Author: Taylan Ulrich B. <taylanbayirli@gmail.com>
;; Keywords: processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Specification:

;; 1. Be a real terminal emulator, like `term-mode'.

;; 2. In line-mode, the last line of the window is reserved as the input line
;; and is not part of the virtual terminal screen.

;; 3. In character-mode, behave like `term-char-mode'.

;; Lies:

;; The above specification is a total lie.  However, it is the ideal; maybe some
;; day someone will make it come true.

;; We use `lui-mode', meaning we're limited to line-wise I/O, no ncurses.  We
;; support terminal escapes as long as they don't break out of their line.  Most
;; importantly for me, we support colors, and hooking the input so a certain
;; input-syntax can be crafted.  This reflects my original goal: to make a MUD
;; client.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'lui)

(require 'xterm-color)
;; https://github.com/atomontage/xterm-color
;; Summary: Calls to `xterm-color-filter' communicate with a state machine,
;; whose state is buffer-local, and allows us to interpret a bytestream with
;; terminal escape sequences chunk-by-chunk.  That is, it will e.g. remember
;; color codes that were started and not changed/reset, and continue with them.


;;; Customization

(defcustom lterm-default-program nil
  "The default program lterm should run."
  :group 'lterm :type 'string)

(defcustom lterm-default-prompt "> "
  "The default prompt for lterm."
  :group 'lterm :type 'string)

(defcustom lterm-echo-before-filters nil
  "Whether lterm should echo the user input as it is received."
  :group 'lterm :type 'boolean)

(defcustom lterm-echo-after-filters nil
  "Whether lterm should echo the user input, after it has been
piped through `lterm-input-filters', before it's sent to the
process."
  :group 'lterm :type 'boolean)

(defcustom lterm-convert-crlf t
  "Whether lterm should convert occurrences of CRLF to LF."
  :group 'lterm :type 'boolean)

(defcustom lterm-input-filters nil
  "List of unary functions through which user-input is piped, in
order, before being sent to the process.
If any of these return nil, lterm aborts the input."
  :group 'lterm :type 'hook)

(defcustom lterm-output-filters nil
  "List of unary functions through which the process's output is piped,
in order, before it's inserted into the buffer.
If any of these return nil, lterm aborts inserting the output."
  :group 'lterm :type 'hook)


;;; Main

(defvar lterm-process nil
  "The process of the current lterm buffer.")
(make-variable-buffer-local 'lterm-process)

(define-derived-mode lterm-mode lui-mode "Linewise-Term"
  "Line-wise interaction with an inferior process, understanding
some simple terminal escapes (e.g. colors) and allowing the input
to be filtered through a processor to allow macros etc.."
  :group 'lterm
  (setq lterm-process (get-buffer-process (current-buffer)))
  (set-process-filter lterm-process 'lterm-process-output-handler)
  (setq lui-input-function 'lterm-user-input-handler)
  (lui-set-prompt lterm-default-prompt)
  (set (make-local-variable 'lui-fill-type) nil)
  (goto-char (point-max)))

(defun lterm-start-process (name program &rest program-args)
  "This should be called just before `lterm-mode'.
Similarly, libraries definind derived modes of lterm should use
this in their \"main entry\" function, a la `lterm', before
calling their mode function."
  (let ((process-environment (cons "TERM=xterm-256color" process-environment))
        (inhibit-eol-conversion t)
        (coding-system-for-read 'binary))
    (apply 'start-process name (current-buffer) program program-args)))

(defun lterm (program)
  "Start a line-wise terminal-emulator in a new buffer.
The buffer is in `lterm-mode'."
  (interactive (list (read-from-minibuffer "Run program: "
                                           (or lterm-default-program
                                               (getenv "ESHELL")
                                               (getenv "SHELL")
                                               "/bin/sh"))))
  (switch-to-buffer (generate-new-buffer "*lterm*"))
  (lterm-start-process "lterm" program)
  (lterm-mode))


;;; Input/output handling

(defmacro lterm--filter (filters string)
  (let ((filter (make-symbol "filter")))
    `(dolist (,filter ,filters ,string)
       (unless (setq ,string (funcall ,filter ,string))
         (return nil)))))

(defun lterm-user-input-handler (string)
  "Function to handle the user-input in lterm buffers."
  (when lterm-echo-before-filters
    (lui-insert string))
  (setq string (lterm--filter lterm-input-filters string))
  (when string
    (when lterm-echo-after-filters
      (lui-insert string))
    (process-send-string lterm-process (concat string "\n"))))

(defvar lterm-prompt-regex "^\\$ "
  "Regular expression used to detect a prompt.
This should probably start with ^.  The remainder of the matching
line will be treated as usual process output.")
(make-variable-buffer-local 'lterm-prompt-regex)

(defvar lterm-prompt-replacement "\\&"
  "The replacement-string to be used, after matching a
prompt-line, to create the prompt.  Alternatively, this can be a
function that should return the prompt as a string. It will be
called so that `match-string' can be used.  The prompt is a lui
prompt.")
(make-variable-buffer-local 'lterm-prompt-replacement)

;; Call after (string-match lterm-prompt-regex <string>) .
(defsubst lterm-set-prompt (string)
  (lui-set-prompt
   (cond
    ((stringp lterm-prompt-replacement)
     (let ((match (substring string (match-beginning 0) (match-end 0))))
      (replace-match lterm-prompt-replacement nil nil match)))
    ((functionp lterm-prompt-replacement)
     (funcall lterm-prompt-replacement))
    (t (error "invalid value for `lterm-prompt-replacement'"))))
  (let ((remainder (replace-match "" nil nil string)))
    (when (< 0 (length remainder))
      (lterm-process-output-line-handler remainder))))

(defvar *lterm-output-remainder* "")
(make-variable-buffer-local '*lterm-output-remainder*)
(defun lterm-process-output-handler (process string)
  "Function to handle the output of lterm processes."
  (with-current-buffer (process-buffer process)
    (when lterm-convert-crlf
      (setq string (replace-regexp-in-string "\r\n" "\n" string)))
    (let ((lines (split-string (concat *lterm-output-remainder* string) "\n")))
      (if (null (cdr lines))
          (setq *lterm-output-remainder* (car lines))
        (let ((second-last (last lines 2)))
          (setq *lterm-output-remainder* (cadr second-last))
          (setcdr second-last nil))
        (dolist (line lines)
          (let ((filtered (xterm-color-filter line)))
            (if (string-match lterm-prompt-regex filtered)
                (lterm-set-prompt filtered)
              (lterm-process-output-line-handler filtered))))))
    (let ((remainder
           (with-temp-buffer
             (xterm-color-filter *lterm-output-remainder*)
             (buffer-string))))
      (when (string-match lterm-prompt-regex remainder)
        (xterm-color-filter *lterm-output-remainder*)
        (setq *lterm-output-remainder* "")
        (lterm-set-prompt remainder)))))

(defun lterm-process-output-line-handler (line)
  "Function to handle a single line of process output."
  (let ((output (lterm--filter lterm-output-filters line)))
    (when output
      (lui-insert line))))

(provide 'lterm)
;;; lterm.el ends here
