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


;;; Main

(define-derived-mode lterm-mud-mode lterm-mode "Linewise-MUD"
  "Cheap MUD client that wraps telnet using `lterm-mode'."
  :group 'lterm-mud
  (setq lterm-prompt-regex "^%lterm-mud-prompt% \\(.*\\) %lterm-mud-prompt%")
  (setq lterm-prompt-replacement "\\1"))

(defun lterm-mud (host port)
  "Connect to host:port and start `lterm-mud-mode'."
  (interactive "sHost: \nsPort: ")
  (switch-to-buffer (generate-new-buffer "*lterm-mud*"))
  (lterm-start-process "lterm-mud" "telnet" host port)
  ;; Switch telnet(1) to character mode:
  (process-send-string lterm-process "mode character\n")
  (lterm-mud-mode))

(provide 'lterm-mud)
;;; lterm-mud.el ends here
