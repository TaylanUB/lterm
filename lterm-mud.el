;;; lterm-mud.el --- lterm-derived mode for MUDding

;; Copyright (C) 2012  Taylan Ulrich B.

;; Author: Taylan Ulrich B. <taylanbayirli@gmail.com>
;; Keywords: games, processes

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

;; 

;;; Code:

(require 'lterm)


;;; Customization

(defvar lterm-mud-prompt-regex nil
  "The regular expression to match telnet output lines that
indicate the prompt.")
(make-variable-buffer-local 'lterm-mud-prompt-regex)

(defvar lterm-mud-prompt-replacement nil
  "The replacement-string to be used, after matching a
prompt-line, to create the prompt.  Alternatively, this can be a
function that should return the prompt as a string. It will be
called so that `match-string' can be used.  The prompt is a lui
prompt.")
(make-variable-buffer-local 'lterm-mud-prompt-replacement)


;;; Main

(define-derived-mode lterm-mud-mode lterm-mode "Linewise-MUD"
  "Cheap MUD client that wraps telnet using `lterm-mode'."
  :group 'lterm-mud
  (add-to-list 'lterm-output-filters 'lterm-mud-output-filter))

(defun lterm-mud (host port)
  "Connect to host:port and start `lterm-mud-mode'."
  (interactive "sHost: \nsPort: ")
  (switch-to-buffer (generate-new-buffer "*lterm-mud*"))
  (lterm-start-process "lterm-mud" "telnet" host port)
  (lterm-mud-mode))


;;; Input/output handling

(defun lterm-mud-output-filter (line)
  "The main output filter of `lterm-mud-mode'.
This will be added to `lterm-output-filters' in `lterm-mud-mode'."
  (lterm-mud-check-prompt line)
  line)

(defun lterm-mud-check-prompt (line)
  "See if LINE is a prompt-line, and act upon it if so."
  (when (and lterm-mud-prompt-regex (string-match lterm-mud-prompt-regex line))
    (lui-set-prompt
     (cond
      ((stringp lterm-mud-prompt-replacement)
       (replace-match lterm-mud-prompt-replacement))
      ((functionp lterm-mud-prompt-replacement)
       (funcall lterm-mud-prompt-replacement))
      (t (error "`lterm-mud-prompt-replacement' must be a string or a function."))))))

(provide 'lterm-mud)
;;; lterm-mud.el ends here
