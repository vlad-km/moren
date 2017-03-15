;;; -*- mode:lisp;  coding:utf-8 -*-


;;;
;;; Moren IDE
;;;
;;; Copyleft, 2016-2017, mvk
;;;
;;;


(defparameter div-banner (dom-create "div" (pair '("id" "class") '("banner-id" "tide-banner tide-default-font"))))
(defparameter div-console (dom-create "div" (pair '("id" "class") '("console-id" "tide-console tide-default-font"))))
(defparameter div-select (dom-create "div" (pair '("id" "class") '("select-id" "tide-canvas unvisible"))))
(defparameter div-control (dom-create "div" (pair '("id" "class") '("control-tab-id" "tide-control-pad tide-default-font"))))


;;; wtf?
;;;
(defparameter br-div (dom-create "br"))
(defparameter space-div (dom-create-text-node "|  |"))


;;;
;;; Mount dom resource
;;;
(defun tide-mount ()
    ;; mount all panels's to document.body
    (dom-mount (dom-get-body)
               div-banner
               div-select
               div-console
               div-control) )


;;;
;;; Tide console
;;; init standard-output stream
;;; See the repl-web/repl.lisp web-init function from
;;; jscl master-branch


;;; Output to console
(defun %tide-console-write (str)
    (let* ((record (dom-create-text-node str)))
        (dom-append div-console record "<br>" )
        ))

(defun %tide-console-char (ch)
    (let* ((record (string ch)))
        (cond ((equal ch #\newline)
               (setf record "<br>")))
        (dom-append div-console record  )
        ))


(defun %tide-console-char-1 (ch)
    (dom-append div-console (string ch)))


;;;
;;; Tide welcome message
;;;

#|
(defun tide-welcome ()
    (dom-append div-banner
                "<br>" "<strong  style=\"color:red;text-align:center\"> Welcome! </strong>") )
|#

(export '(banner-msg))
(defun banner-msg (idx txt)
    (let* ((fmt (list "<strong  style=\"color:red;\">~a</strong>"
                      "<font style=\"color:red\">~a</font>"
                      "<strong style=\"color:orange;\">~a</strong>"
                      "<font style=\"color:orange\">~a</font>"
                      "<strong style=\"color:green;\">~a</strong>"
                      "<font style=\"color:green\">~a</font>")))
        (dom-set-inner-html jscl::div-banner (format nil (elt fmt idx) txt))
        (values)))

(defun tide-welcome ()
    (let* ((platform #j:window:navigator:userAgent)
           (cpu #j:window:navigator:hardwareConcurrency)
           (online #j:window:navigator:onLine))
        (dom-set-inner-html div-banner
                            (format nil
                                    (concat "<strong style=\"color:orange\">Welcome JSCL console.</strong><br>"
                                            "<font style=\"color:orange\">Platform: ~a CPU:~a Online:~a</font><br>"
                                            "<font style=\"color:orange\">Type (man \"man\") for read manual")
                                    platform cpu online))))


;;;
;;; jqconsole + web-repl integration
;;;


(defun %write-string (string &optional (escape t))
    (if #j:jqconsole
        (#j:jqconsole:Write string "jqconsole-output" "" escape)
        (#j:console:log string)))


(defun load-history ()
    (let ((raw (#j:localStorage:getItem "jqhist")))
        (unless (js-null-p raw)
            (#j:jqconsole:SetHistory (#j:JSON:parse raw)))))

(defun save-history ()
    (#j:localStorage:setItem "jqhist" (#j:JSON:stringify (#j:jqconsole:GetHistory))))

;;;
;;; Shadow JQ-console history
;;;
;;; console history stored in localStorage
;;;  does not match the entries in the jqconsole  dom structure
;;;
(defparameter tide-jq-history nil)

;;;
;;; get console history from localStorage and place to array
;;;
;;; (explore-console-history)
;;; => History has 90 items
;;;
(export '(take-console-history))
(defun take-console-history ()
    (let ((raw (#j:localStorage:getItem "jqhist"))
          (lh 0))
        (unless (jscl::js-null-p raw)
            ;;(format t "raw ~a hist ~a~%" (length raw) (length tide-jq-history))
            (setf tide-jq-history (#j:JSON:parse raw))
            (setf raw nil)
            (setf lh (length tide-jq-history)))
        (banner-msg 4 (format nil "<br>History has ~a items" lh))
        (values)))

;;;
;;; look console history from to items
;;;
;;;
;;; (look-console-history 5 10)
;;;
(export '(look-console-history))
(defun look-console-history (&optional (start 0) (end 5))
    (let ((lh (length tide-jq-history)))
        (when (> lh 0)
            (loop for i from start to (if (>= end lh) (1- lh) end)
                  do
                     (format t "~a: ~s~%" i (string (aref tide-jq-history i)))))
        (values)))

;;;
;;; take console history item
;;;
;;; if  lazily flipping long history by with by keystroke, just type
;;;    (take-history-item 2 or other number)
;;;
;;;    (take-history 2)<enter>
;;;    <enter>
;;;
(export '(take-history-item))
(defun take-history-item (num)
    (let ((state #j:jqconsole:state))
        (setf #j:jqconsole:state 0)
        (#j:jqconsole:SetPromptText (aref tide-jq-history num))
        (setf #j:jqconsole:state state)
        (values) ))



;;;
;;; Console history reset button
;;;
;;; Large history of working with the console slow down your browser
;;; All historic items will be droped from localStorage
;;;
;;; Droped localStorage
;;;
(defparameter ctrl-btn-reset-hist
  (dom-create-button "RST"
                     (pair '("id" "class" )
                           '("ctrl- rst-button-id" "tide-button "))
                     (list (cons  "onclick" #'(lambda (event)
                                                  (declare (ignorable event))
                                                  (#j:jqconsole:ResetHistory)
                                                  (setf tide-jq-history nil)
                                                  (banner-msg 4 (format nil "<br>Console history reset"))
                                                  (values))))))

;;;
;;; Just type (rst-btn-on)
;;; Button RST will be install into panel
;;;
;;; Press button "RST" - all console history will be deleted
;;; from localStorage Without recovery
;;;
(export '(rst-btn-on))
(defun rst-btn-on ()
    (dom-mount jscl::div-control ctrl-btn-reset-hist)
    (values))

;;;
;;; (rst-btn-off) => button "RST" will be removed from control pane
(export '(rst-btn-off))
(defun rst-btn-off ()
    (dom-remove ctrl-btn-reset-hist)
    (values))


;;;
;;; Integrated rst-btn
;;;
;;; (rst-btn :on) => install button RST
;;; (rst-btn :off) =>remove RST button from pane
;;;
;;; if error in cmd, then none
;;;

(export '(rst-btn))
(defun rst-btn (cmd)
    (case cmd
      (:on (dom-mount   div-control ctrl-btn-reset-hist))
      (:off (dom-remove ctrl-btn-reset-hist))
      (values-list nil)))

;;;
;;; Reset jqconsole shortcuts
;;;
;;; All keyboars shortcuts are deleted from the jqconsole
;;;

(export '(rst-console-shortcuts))
(defun rst-console-shortcuts ()
    (#j:jqconsole:ResetShortcuts))




;;;
;;; jqconsole DOM cleaner
;;;
;;;
(defparameter dom-jq-cleaner-timer nil)
(defparameter dom-jq-cleaner-run nil)

;;; ;;;todo: Deprecate
(export '(*dom-jq-clr-classes))
(defparameter *dom-jq-clr-classes
  (list
   ".jqconsole-output" ".jqconsole-prompt" ".jqconsole-old-prompt" ".jqconsole-error" ".jqconsole-return"))



;;;
;;; call (dom-cleaner (elt *dom-jq-clr-classes N)) for cut jq dom structure
;;; for class with name N
;;; return the number of deleted items with class from dom-jq-clr-classes
;;;
;;;todo: Deprecate
;;;
(export '(dom-cleaner))
(defun dom-cleaner (&optional (gc-class ".jqconsole-output"))
    (let* ((q (#j:jqconsole:$container:find gc-class))
           (lenq (length q))
           (tail (- lenq 100))
           (droped 0))
        ;;(format t "Clr len ~a tail ~a~%" lenq tail)
        (when (> tail 0)
            (dotimes (i tail)
                (dom-remove (aref q i))
                (setf (aref q i) nil))
            (setf droped tail))
        (setf q nil)
        droped))

;;;todo: Deprecate
(export '(dom-cleaner-top))
(defun dom-cleaner-top ()
    (let ((droped #(0 0 0 0 0))
          (idx 0)
          (rc 0))
        (dolist (cls *dom-jq-clr-classes)
            (incf rc (setf (aref droped idx) (dom-cleaner cls)))
            (incf idx))
        (if (> rc 0)
            (banner-msg 2 (format nil "<br>CLR ~a~%" droped)))
        (values)))


;;;
;;; New version of function for DOM console clearing
;;;

(defun clean-dom-console-childs ()
    (let* ((jqhist nil)
           (jqlen nil))
        ;; Yes, and this allows oget
        ;; first - take length
        (setf jqlen (oget #j:jqconsole:$container "0" "childNodes" "0" "childNodes" "length"))
        ;;(format t "Length ~a~%" jqlen)
        ;; if the length abowe limit 120 elements, the deletion begins
        ;; todo: made length as parameter
        (when (>= jqlen 120)
            (setf jqhist (oget #j:jqconsole:$container "0" "childNodes" "0" "childNodes"))
            ;;(format t "History ~a~%" (length jqhist))
            (loop for idx from 1 to 10
                  ;; first element (console header) is saved
                  ;; started with index = 1
                  ;; Remove item from the dom structure
                  ;; and niling it in the local array
                  ;; In the hope that the garbage collector works
                  do (dom-remove (aref jqhist idx))
                     (setf (aref jqhist idx) nil))
            (format t "<font color='green'>CLR 10:~a</font>~%" jqlen)
            ;; zero local array with other dom els
            ;; to delete all refernces
            (loop for idx from 0 below jqlen do (setf (aref jqhist idx) nil))
            (setf jqhist nil))
        (value-list nil)))



#|
(defun dom-jq-cleaner-switch ()
    (cond (dom-jq-cleaner-run
           (#j:window:clearTimeout dom-jq-cleaner-timer)
           (setf dom-jq-cleaner-run nil)
           (banner-msg 4 (format nil "<br>CLR OFF")))
          (t
           (setf dom-jq-cleaner-timer (#j:window:setInterval #'dom-cleaner-top 10000))
           (setf dom-jq-cleaner-run t)
           (banner-msg 4 (format nil "<br>CLR ON"))))
    (values))

|#

;;;
;;; Button switcher on/off
;;;
(defun dom-jq-cleaner-switch ()
    (cond (dom-jq-cleaner-run
           (#j:window:clearTimeout dom-jq-cleaner-timer)
           (setf dom-jq-cleaner-run nil)
           (banner-msg 4 (format nil "<br>CLR OFF")))
          (t
           (setf dom-jq-cleaner-timer (#j:window:setInterval #'clean-dom-console-childs 10000))
           (setf dom-jq-cleaner-run t)
           (banner-msg 4 (format nil "<br>CLR ON"))))
    (values))



(defun dom-jq-cleaner-stop ()
    (#j:window:clearTimeout dom-jq-cleaner-timer))

;;;
;;; Some button CLR
;;;
(defparameter ctrl-btn-jq-dom-clr
  (dom-create-button "CLR"
                     (pair '("id" "class" )
                           '("ctrl-clr-button-id" "tide-button "))
                     (list (cons  "onclick" #'(lambda (event)
                                                  (declare (ignorable event))
                                                  (dom-jq-cleaner-switch)
                                                  (values))))))

;;;
;;; (clr-btn-on)
;;; Press button "CLR".
;;; Message "CLR on" - jq dom cleaner is executed every 10 seconds.
;;; Message "CLR off" - jq dom cleaner will be stopped
;;;
;;; press "CLR" - toggle start/stop dom cleaner
;;;
(export '(clr-btn-on))
(defun clr-btn-on ()
    (dom-mount jscl::div-control ctrl-btn-jq-dom-clr)
    (values))

;;; (clr-btn-off)
;;; Remove CLR button from control panel
;;;
(export '(clr-btn-off))
(defun clr-btn-off ()
    (when dom-jq-cleaner-run
        (#j:window:clearTimeout dom-jq-cleaner-timer)
        (setf dom-jq-cleaner-run nil))
    (dom-remove ctrl-btn-jq-dom-clr)
    (values))


;;;
;;; Integrated clr-btn
;;;
;;; (clr-btn :on) => install button CLR
;;; (clr-btn :off) =>remove CLR button from pane
;;;
;;; if error in cmd, then none
;;;

(export '(clr-btn))
(defun clr-btn (cmd)
    (case cmd
      (:on (dom-mount jscl::div-control ctrl-btn-jq-dom-clr))
      (:off
       (when dom-jq-cleaner-run
           (#j:window:clearTimeout dom-jq-cleaner-timer)
           (setf dom-jq-cleaner-run nil))
       (dom-remove ctrl-btn-jq-dom-clr)))
    (values-list nil))


;;;
;;; Borrowed from original jscl/repl-web/repl.lisp
;;;
;;; Decides wheater the input the user has entered is completed or we
;;; should accept one more line.
(defun indent-level (string)
    (let ((i 0)
          (stringp nil)
          (s (length string))
          (depth 0))

        (while (< i s)
            (cond
              (stringp
               (case (char string i)
                 (#\\
                  (incf i))
                 (#\"
                  (setq stringp nil)
                  (decf depth))))
              (t
               (case (char string i)
                 (#\( (incf depth))
                 (#\) (decf depth))
                 (#\"
                  (incf depth)
                  (setq stringp t)))))
            (incf i))

        (if (and (zerop depth))
            nil
            ;; We should use something based on DEPTH in order to make
            ;; edition nice, but the behaviour is a bit weird with
            ;; jqconsole.
            0)))


;;;
;;; Borrowed from original jscl/repl-web/repl.lisp
;;;

(defun toplevel ()
    (#j:jqconsole:RegisterMatching "(" ")" "parents")
    (let ((prompt (format nil "~a> " (package-name *package*))))
        (#j:jqconsole:Write prompt "jqconsole-prompt"))
    (flet ((process-input (input)
               (%js-try
                (handler-case
                    (let* ((form (read-from-string input))
                           (results (multiple-value-list (eval-interactive form))))
                        (dolist (x results)
                            (#j:jqconsole:Write (format nil "~S~%" x) "jqconsole-return")))
                  (error (err)
                      (#j:jqconsole:Write "ERROR: " "jqconsole-error")
                      (#j:jqconsole:Write (apply #'format nil (!condition-args err)) "jqconsole-error")
                      (#j:jqconsole:Write (string #\newline) "jqconsole-error")))

                (catch (err)
                    (#j:console:log err)
                    (let ((message (or (oget err "message") err)))
                        (#j:jqconsole:Write (format nil "ERROR[!]: ~a~%" message) "jqconsole-error"))))

               (save-history)
               (toplevel)))
        (#j:jqconsole:Prompt t #'process-input #'indent-level)))


;;;
;;; LAZY KLUDGE
;;; TODO: remove
;;;
(defun %%export ()
    (export '(new oget concat def!struct join make-new fset) )
    (fset 'console-log #j:console:log) )


(defun tide-go ()
    (let* ((jq))
        (tide-mount)
        (setf jq (#j:$ "#console-id"))
        (setf #j:jqconsole (funcall ((oget jq "jqconsole" "bind") jq  "" "")))
        (load-history)
        (setq *standard-output*
              (vector 'stream
                      (lambda (ch) (%write-string (string ch)))
                      (lambda (string) (%write-string string))))
        (tide-welcome)
        (%%export)
        ;;(setf dom-jq-cleaner-timer (#j:window:setInterval #'dom-cleaner 10000 ".jqconsole-output"))
        ))

;;;

(defun tide ()
    (tide-go)
    (#j:window:addEventListener "load" (lambda (&rest args) (toplevel))))

(tide)


;;;;; EOF
