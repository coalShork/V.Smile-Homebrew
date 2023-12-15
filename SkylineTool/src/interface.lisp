(in-package :skyline-tool)



;; entry point from shell

(defvar *invocation*
  (list :--help 'about-skyline-tool
        :-h 'about-skyline-tool
        :help 'about-skyline-tool
        :allocate-assets 'allocate-assets
        :atari800-label-file 'atari800-label-file
        :build-banking 'build-banking
        :burn-rom 'click.adventuring.skyline.eprom::burn-rom
        :blob-rip-7800 'blob-rip-7800
        :check-for-absent-assets 'check-for-absent-assets
        :compile-index 'compile-index
        :compile-art 'compile-art
        :compile-enemies 'compile-enemies
        :compile-font 'compile-font-command
        :compile-item-drops 'compile-item-drops
        :compile-map 'compile-map
        :compile-obj 'compile-obj
        :compile-player-frames 'compile-player-frames
        :compile-code 'compile-skylisp
        :compile-shops 'compile-shops
        :collect-strings 'collect-strings
        :compile-music 'compile-music
        :compile-script 'compile-script
        :compile-shops 'compile-shops
        :compile-tileset 'compile-tileset
        :collect-assets 'collect-assets
        :compile-critters 'compile-critters
        :compile-art-7800 'compile-art-7800
        :extract-tileset-palette 'extract-tileset-palette
        :labels-to-mame 'labels-to-mame
        :labels-to-include 'labels-to-include
        :make-classes-for-oops 'make-classes-for-oops
        :prepend-fundamental-mode 'prepend-fundamental-mode
        :push-7800gd 'push-7800gd-bin
        :patch-7800gd 'push-7800gd-bin-no-execute
        :write-asset-bank 'write-asset-bank
        :write-asset-ids 'write-asset-ids
        :write-cart-header 'write-cart-header
        :write-gimp-palettes 'write-gimp-palettes
        :write-projection-tables.s 'write-projection-tables.s
        :write-master-makefile 'write-master-makefile))

(defvar *command-line* nil)

(defun debug-myself-in-emacs ()
  (let ((swank (find-package :swank)))
    (funcall (intern "SETUP-SERVER" swank) 0
             (lambda (port)
               (uiop:run-program (list "/usr/bin/emacsclient" "-e"
                                       (format nil "(progn
 \(load (expand-file-name \"~~/quicklisp/slime-helper.el\"))~
 \(eval-after-load \"slime\" (quote (slime-connect \"::1\" ~d))))" port))))
             (intern "*COMMUNICATION-STYLE*" swank)
             (intern "*DONT-CLOSE*" swank) nil)))

#+mcclim
(defun edit-myself-in-climacs (file)
  (climacs:edit-file file
                     :process-name "Editing Skyline-Tool"))


#+mcclim
(defun start-listener ()
  (clim-listener:run-listener :new-process t
                              :process-name "Skyline Tool"
                              :package :Skyline-Tool))

(defun x11-p ()
  (when-let (display (sb-posix:getenv "DISPLAY"))
    (find #\: display)))

(defun prompt (query)
  (format *query-io* "~&~a" query)
  (force-output *query-io*)
  (prog1
      (string-trim #(#\Space #\Page #\Return #\Linefeed #\Tab)
                   (read-line *query-io*))
    (terpri *query-io*)
    (finish-output *query-io*)))

(defun prompt-function (prompt)
  (lambda (s)
    (let ((*query-io* s))
      (if (and (not (tty-xterm-p)) #+mcclim (x11-p) #-mcclim nil)
          #+mcclim
          (clim-simple-interactor:run-in-simple-interactor
           (lambda () (prompt prompt)))
          #-mcclim nil
          (prompt prompt)))))

(defun dialog (title message &rest args)
  (if (and (not (tty-xterm-p)) (x11-p) #+:mcclim t #-mcclim nil)
      (or #+mcclim (clim-simple-interactor:run-in-simple-interactor
                    (lambda ()
                      (apply #'format *query-io* message args)
                      (finish-output *query-io*)
                      (clim:with-text-size (*query-io* :small)
                        (format *query-io* "~5%~20t(Done. Press Return)"))
                      (finish-output *query-io*)
                      (read-char))
                    :process-name title)
          #'break)
      (progn
        (format t "~&~% ★ ~a ★~%" title)
        (apply #'format t message args)
        (fresh-line)
        (force-output))))

(defun friendly-offer-single-restart (restart)
  (if (y-or-n-p "~%Would you like to run this restart? (Say “N” to quit)
~s: ~a
⇒ "
                restart
                restart)
      (invoke-restart-interactively restart)
      (sb-ext:exit :code 4)))

(defun friendly-offer-restart-list (condition-class restarts)
  (loop
     (format *query-io* "~%To handle this ~:(~a~), choose a restart: ~{~% ~{~2d. [3m~va[0m  ~a~}~}"
             condition-class
             (let ((n 0)
                   (max-name-length (loop for restart in restarts
                                          maximize (length (string (restart-name restart))))))
               (mapcar (lambda (restart)
                         (list (incf n)
                               max-name-length
                               (restart-name restart)
                               restart))
                       restarts)))
     (format *query-io* "~2%Choose a restart by [3mNAME[0m or number above. ⇒ ")
     (finish-output *query-io*)
     (let* ((reply (read-line *query-io*))
            (reply-number (ignore-errors (parse-integer reply :junk-allowed t)))
            (reply-name-matches (remove-if-not (lambda (restart)
                                                 (string-equal (restart-name restart) reply))
                                               restarts))
            (reply-name-partials
              (and reply
                   (not (emptyp reply))
                   (remove-if-not (lambda (restart)
                                    (search (string-upcase reply)
                                            (string-upcase (restart-name restart))))
                                  restarts))))
       (cond
         ((and reply-number (<= 1 reply-number (length restarts)))
          (invoke-restart-interactively (elt restarts (1- reply-number))))
         ((= 1 (length reply-name-matches))
          (format *query-io* "“~a”" (first reply-name-matches))
          (finish-output *query-io*)
          (invoke-restart-interactively (first reply-name-matches)))
         (reply-name-matches
          (format *query-io* "“~a” is the name of ~r restart~:p."
                  reply (length reply-name-partials))
          (finish-output *query-io*)
          (dolist (reply-name-match reply-name-matches)
            (when (y-or-n-p "You want to “~a”?~%  ([3m~a[0m) ⇒ "
                            reply-name-match (restart-name reply-name-match))
              (invoke-restart-interactively reply-name-match)))
          (warn "No restart selected"))
         (reply-name-partials
          (format *query-io* "“~a” matches ~r restart name~:p (partially)"
                  reply (length reply-name-partials))
          (finish-output *query-io*)
          (dolist (reply-name-match reply-name-partials)
            (when (y-or-n-p "~&You want to “~a”?~%  ([3m~a[0m)  ⇒ "
                            reply-name-match (restart-name reply-name-match))
              (invoke-restart-interactively reply-name-match)))
          (warn "No restart selected"))
         (t
          (format *error-output* "~&I'm sorry, I don't see any restart like “~a.”" reply)
          (finish-output *error-output*))))))

(defvar *system-debugger* *debugger-hook*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'cl-user::user-real-name)
    (defun cl-user::user-real-name (&optional (user-id #+sbcl (sb-posix:geteuid) #-sbcl nil))
      #-sbcl (declare (ignore user-id))
      #+sbcl (first (split-sequence #\,
                                    (sb-posix:passwd-gecos (sb-posix:getpwuid user-id))))
      #-sbcl (car (last (remove-if #'emptyp (split-sequence #\/
                                                            (namestring (user-homedir-pathname)))))))))

(defvar *speech-thread* nil)

(defun say-aloud (format &rest args)
  (unless (and *speech-thread* (thread-alive-p *speech-thread*))
    (setf *speech-thread*
          (make-thread (lambda ()
                         (uiop/run-program:run-program
                          (list "speak-ng" (format nil "\"~a\""
                                                   (substitute #\apostrophe #\quotation_mark
                                                               (apply #'format nil format args))))
                          :ignore-error-status t))
                       :name "Speaking aloud"))))

(defun user-real-name (&optional (user-id (sb-posix:geteuid)))
  (first (split-sequence #\,
                         (sb-posix:passwd-gecos (sb-posix:getpwuid user-id)))))

(defun friendly-tty-debugger (condition &optional myself)
  (declare (ignore myself))
  (when (string-equal (or (sb-ext:posix-getenv "RESTARTS") "") "NIL")
    (finish-output)
    (finish-output *trace-output*)
    (format *error-output* "~%~|
[31;1mAn error of type ~:(~a~) was signalled and RESTARTS=NIL,
ending immediately.[0m
~a
"
            (class-name (class-of condition))
            condition)
    (finish-output *error-output*)
    (sb-ext:exit :code 2))
  (let ((restarts (compute-restarts))
        (good-day (let ((hour (nth-value 2 (decode-universal-time (get-universal-time)))))
                    (cond
                      ((< hour 12) "Good morning")
                      ((< hour 18) "Good afternoon")
                      (t "Good evening")))))
    (when-let ((restart (and (string-equal (or (sb-ext:posix-getenv "AUTOCONTINUE") "") "T")
                             (let* ((restart (find "CONTINUE" restarts
                                                   :key #'restart-name :test #'string-equal))
                                    (restart-function (when restart
                                                        (sb-kernel::restart-function restart))))
                               (when (and restart-function
                                          (emptyp (sb-introspect:function-lambda-list
                                                   restart-function)))
                                 restart)))))
      (if (string-equal 'interactive-interrupt (class-name (class-of condition)))
          (progn
            (format *error-output* "
~|~2%An Interactive Interrupt (probably STOP or ^C) was signalled. Exiting.~2%")
            (invoke-restart 'quit-completely))
          (format *error-output* "~%~|
[31;1mAn error of type ~:(~a~) was signalled,
but a CONTINUE restart was available and AUTOCONTINUE=T.[0m
~a ⇒ ~a
"
                  (class-name (class-of condition))
                  condition restart))
      (invoke-restart-interactively restart))
    (let ((c (princ-to-string condition)))
      (say-aloud "~a, ~a. I'm sorry to tell you, that an error of type ~a was signalled. ~a~@[, et cetera~]."
                 good-day (user-real-name) (class-name (class-of condition))
                 (subseq c 0 (position #\Newline c))
                 (position #\Newline c))
      (format *error-output* "~%~|


~a, ~a. I'm sorry to tell you, that
while running ~s~@[ with AUTOCONTINUE=~a~],

[1m*** An error of type ~:(~a~) was signalled ***[0m

[31;2m   ~a[0m

There ~[are no restart options~;is one restart option~:;are ~:*~:d restart options~] available."
              good-day
              (user-real-name)
              *command-line*
              (sb-ext:posix-getenv "AUTOCONTINUE")
              (class-name (class-of condition))
              c
              (length restarts)))
    (finish-output *error-output*)
    (cond
      ((zerop (length restarts))
       (format *error-output* "~%Since you are out of options, I'm ending things.~2%Goodbye.~%")
       (sb-ext:exit :code 4))
      ((= 1 (length restarts))
       (friendly-offer-single-restart (first restarts)))
      (t (friendly-offer-restart-list (class-name (class-of condition)) restarts)))))

(defmacro with-happy-restarts (&body body)
  `(tagbody do-over
      (let ((*system-debugger* *debugger-hook*)
            (*debugger-hook* (or
                              #+mcclim (when  (and (not (tty-xterm-p)) (x11-p)) #'clim-debugger:debugger)
                              #'friendly-tty-debugger
                              *debugger-hook*)))
        (restart-case
            (unwind-protect
                 (progn ,@body)
              (force-output *trace-output*))
          (do-over ()
            :report "Try again, from the top"
            (go do-over))
          (abort ()
            :report "Cancel this command")
          (change-directory (new-directory)
            :report (lambda (s) (format s "Change the working directory from ~a"
                                        *default-pathname-defaults*))
            :interactive-function (prompt-function "Change to directory?")
            (setf *default-pathname-defaults* new-directory)
            (sb-posix:chdir new-directory))
          (invoke-make (target)
            :report "Ask GNU Make to generate a file"
            :interactive-function (prompt-function "What file should be regenerated?")
            (uiop:run-program (list "make" "-r" target)))
          (gimp (file)
            :report "Edit a file in Gimp"
            :interactive-function (prompt-function "Edit which file in Gimp?")
            (uiop:run-program (list "gimp" file)))
          (tiled (file)
            :report "Edit a file in Tiled"
            :interactive-function (prompt-function "Edit which file in Tiled?")
            (uiop:run-program (list "tiled" file)))
          #+mcclim (start-repl ()
                     :report "Open a read-eval-print-loop listener"
                     (if (x11-p)
                         (clim-listener:run-listener :process-name "Skyline Tool REPL"
                                                     :package :skyline-tool))
                     (go do-over))
          #-mcclim (start-repl ()
                     :report "Open a read-eval-print-loop listener"
                     (replic:repl)
                     (go do-over))
          (debug-in-emacs ()
            :report "Debug this in a running GNU Emacs"
            (debug-myself-in-emacs)
            (go do-over))
          #+mcclim (edit-in-climacs (file)
                     :report "Edit in Climacs"
                     :interactive-function (prompt-function "Edit which file in Climacs?")
                     (edit-myself-in-climacs file)
                     (format t "Climacs now open … ~
recompile when you've corrected the error. (C-c C-k) and restart.")
                     (go do-over))
          (recompile-tool ()
            :report "Recompile system Skyline-Tool (and retry)"
            (format t "
I will  reload the ASDF  System Skyline-Tool  and integrate it  into the
running   image.  This   will   NOT  affect   the  application   program
file  (bin/skyline-tool),  though;  in   order  to  make  these  changes
effective for next time, you should run Make (which will run Buildapp).
")
            (compile-file (make-pathname
                           :directory '(:relative "SkylineTool")
                           :name "setup" :type "lisp")
                          :print nil)
            (load (make-pathname
                   :directory '(:relative "SkylineTool" )
                   :name "setup" :type "fasl"))
            (ql:quickload :skyline-tool)
            (go do-over))
          (quit-completely ()
            :report "Quit completely from Skyline-Tool"
            (bye))))))

(defun about-skyline-tool (&rest commands)
  "Display help and version information.

Supply a list of verb(s) to see detailed documentation"
  (format *trace-output* "~&

 Skyline-Tool
 ————————————

Copyright © 2014-2023
 Bruce-Robert Pocock (brpocock@star-hope.org)
 Most Rights Reserved.  See COPYING for details.

Compiler: ~a,~%	version ~a
System software: ~a,~%	version ~a
Machine: ~a, type: ~a,~%	version ~a
~@[Site: ~a~]~@[~%	(~a)~]

Usage: the first  parameter must be a verb;  following parameters depend
on the verb being invoked. You almost certainly want to just look at the
Makefile for an example, but you can try “help” + command-name for the
documentation also.

"
          (lisp-implementation-type) (lisp-implementation-version)
          (software-type) (software-version)
          (machine-instance) (machine-type) (machine-version)
          (short-site-name) (long-site-name))
  (if commands
      (dolist (command commands)
        (if-let (fun (getf *invocation* (make-keyword (string-upcase command))))
          (format *trace-output* "~2% • ~(~a~)~2%~a"
                  command (or (documentation fun 'function)
                              "(no documentation yet)"))
          (format *trace-output* "~% • ~a: unknown ☹" command)))
      (dolist (verb (sort (remove-if-not #'keywordp *invocation*)
                          #'string-lessp))
        (format *trace-output* "~% • ~(~a~): ~a"
                verb (or (first-line (documentation (getf *invocation* verb)
                                                    'function))
                         "(no documentation yet)")))))

(defun bye ()
  (when (and *speech-thread* (thread-alive-p *speech-thread*))
    (destroy-thread *speech-thread*))
  (format t "~&~|~%Good-bye.~2%")
  (sb-ext:exit :timeout 10))

(defun print-useful-help ()
  (dialog "About Skyline-Tool"
          "

This is Skyline Tool. It does a bunch of neat things that help with game
development, particularly on  Commodore and Atari 8-bit  systems. It can
convert maps, graphics, music, scripts, or whatever is useful.

Generally,  you'll  specify   a  command  to  run,  and   it  will  take
some parameters.

To see a list of available commands, try

 bin/skyline-tool help

To see specifics about one command, add its name to the end, e.g.

 bin/skyline-tool help check-for-absent-assets

If you need more help, ask Bruce-Robert.

"))

(defun command (argv)
  (format *trace-output* "~&Skyline tool invoked:
(Skyline-Tool:Command '~s)~@[~%~10t• AUTOCONTINUE=~a~]"
          argv (sb-ext:posix-getenv "AUTOCONTINUE"))
  (finish-output *trace-output*)
  (format t "]2;~a — Skyline-Tool" (or (and (< 1 (length argv)) (second argv))
                                           "?"))
  (finish-output)
  (let ((sb-impl::*default-external-format* :utf-8)
        (*command-line* (and (< 1 (length argv)) (subseq argv 1))))
    (with-happy-restarts
      (unless (< 1 (length argv))
        (restart-case
            (error "Ask for help if you need it, argument required")
          (help () :report "Explain how this tool works"
            (print-useful-help)
            (bye))))
      (destructuring-bind (self verb &rest invocation) argv
        (if-let (fun (getf *invocation* (make-keyword (string-upcase verb))))
          (flet ((runner ()
                   (apply fun (remove-if (curry #'string= self)
                                         invocation))
                   (fresh-line)))
            (if (and (x11-p) (string-equal "t" (sb-posix:getenv "SKYLINE-GUI")))
                #+mcclim
                (clim-simple-interactor:run-in-simple-interactor
                 #'runner
                 :process-name
                 (format nil "Skyline-Tool: running ~:(~a~)~{ ~a~}"
                         (substitute #\Space #\- verb)
                         invocation))
                #-mcclim nil
                (funcall #'runner)))
          (error "Command not recognized: “~a” (try “help”)" verb))
        (fresh-line)))))

(defun c (&rest args)
  (funcall #'command (cons "c" args)))

#+mcclim (assert (fboundp 'clim-debugger:debugger))
