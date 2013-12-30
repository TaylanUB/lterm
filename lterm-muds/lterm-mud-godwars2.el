(defun godwars2 ()
  (interactive)
  (lterm-mud "godwars2.org" "3000"))

(defun gw2-exec (cmd)
  (lterm-user-input-handler cmd))

(defun gw2-combo (list)
  (catch 'stop
    (mapl
     (lambda (list)
       (let ((cmd-spec (car list))
             (rest (cdr list)))
         (cond
          ((stringp cmd-spec) (gw-exec cmd-spec))
          ((symbolp cmd-spec) (gw-exec (symbol-name cmd-spec)))
          ((consp cmd-spec)
           (let ((cmd (car cmd-spec))
                 (args (cdr cmd-spec)))
             (case cmd
               ('pause
                (lexical-let ((rest rest))
                  (run-with-timer (car args) nil
                                  (lambda () (gw-combo rest))))
                (throw 'stop nil))
               (t (error "No such command: %s" cmd)))))
          (t (error "Commands must be strings or symbols. Given: %s"
                    cmd-spec)))))
     list)))
