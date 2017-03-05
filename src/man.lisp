;;; -*- mode:lisp;  coding:utf-8 -*-

(defparameter *man-db*
  (make-js-object "man" "man/man0.txt" "dom" "man/dom0.txt"))

(defparameter *man-def-man* "man/man0.txt")

(defun man-prefix (topic)
    (concat "man-" topic))


(defun load-man (man host-file-name)
    (xhr-receive  host-file-name
                  (lambda (input)
                      (let* ((txt))
                          (setf txt (split-str (list #\\)
                                               (substitute #\Space (code-char 13)
                                                           (substitute #\Space (code-char 10) input)) ))
                          (res-alloc man txt)
                          (display-man-topic txt)))
                  (lambda (uri status)
                      (banner-msg 0 (format nil "<br> Man: Can't load ~s" uri))) )
    (values))


(defun display-man-topic (lst)
    (dolist (str lst)
        (format t "~a~%" str)))

(defun check-topic (name)
    (let* ((fname (oget *man-db* name)))
        (if fname fname *man-def-man*)))


(export '(man))
(defun man (topic)
    (let* ((res-pref (man-prefix topic))
           (hm (res-refer res-pref)))
        (cond  ((not (null hm)) (display-man-topic hm))
               (t (load-man res-pref (check-topic topic))))
        (values)))
