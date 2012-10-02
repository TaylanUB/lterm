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

(require 'lui)
(require 'xterm-color)

(define-derived-mode lterm-mode lui-mode "Linewise-Term"
  "Line-wise interaction with an inferior process, understanding
some simple terminal escapes (e.g. colors) and allowing the input
to be filtered through a processor to allow macros etc.."
  :group 'lterm
  (setq lui-input-function 'lterm-user-input-handler)
  (lui-set-prompt lterm-default-prompt)
  (set (make-local-variable 'lui-fill-type) nil)
  (goto-char (point-max)))

(defvar lterm-default-prompt "> "
  "The default prompt for lterm.")

(defvar lterm-echo-before-filters nil
  "Whether lterm should echo the user input as it is received.")

(defvar lterm-echo-after-filters nil
  "Whether lterm should echo the user input, after it has been
piped through `lterm-input-filters', before it's sent to the
process.")

(defvar lterm-convert-crlf t
  "Whether lterm should convert occurrences of CRLF to LF.")

(defvar lterm-process nil
  "The process of the current lterm buffer.")
(make-variable-buffer-local 'lterm-process)

(defvar lterm-input-filters nil
  "List of unary functions through which user-input is piped, in
order, before being sent to the process.")

(defvar lterm-output-filters nil
  "List of unary functions through which the process's output is piped,
in order, before it's inserted into the buffer.")

(defun lterm-user-input-handler (line)
  "Function to handle the user-input in lterm buffers."
  (when lterm-echo-before-filters
    (lui-insert line))
  (dolist (filter lterm-input-filters)
    (setq line (funcall filter line)))
  (when lterm-echo-after-filters
    (lui-insert line))
  (process-send-string lterm-process (concat line "\n")))

(defun lterm-process-output-handler (process line)
  "Function to handle the output of lterm processes."
  (when lterm-convert-crlf
    (setq line (replace-regexp-in-string "\r\n" "\n" line)))
  (when (equal (substring line -1) "\n")
    (setq line (substring line 0 -1)))
  (setq line (xterm-color-filter line))
  (with-current-buffer (process-buffer process)
    (lui-insert line)))

(defun lterm (program)
  "Start a line-wise terminal-emulator in a new buffer.
The buffer is in `lterm-mode'."
  (interactive (list (read-from-minibuffer "Run program: "
                                           (or explicit-shell-file-name
                                               (getenv "ESHELL")
                                               (getenv "SHELL")
                                               "/bin/sh"))))
  (switch-to-buffer (generate-new-buffer "*lterm*"))
  (let ((process-environment (cons "TERM=xterm-256color" process-environment))
        (inhibit-eol-conversion t)
        (coding-system-for-read 'binary))
    (setq lterm-process (start-process "lterm" (current-buffer) program)))
  (set-process-filter lterm-process 'lterm-process-output-handler)
  (lterm-mode))

(provide 'lterm)
;;; lterm.el ends here
