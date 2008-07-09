
;; cl-n-back : An N-Back implementation
;; Copyright (C) 2008 Zach Smith

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(in-package :cl-n-back)

(defmacro lambda-in-dynamic-environment (specials vars &body body)
  "Define an anonymous function with lambda list VARS and BODY which executes
with the specified dynamic variables in SPECIALS bound as they are when it is
declared (not as they are when it is called).  This kind of defeating the
purpose of dynamic variables, but I find it useful when writing callback
functions (which are invoked in code that I did not write and thus have little
control over the dynamic bindings there).  Note that declarations in the body
are handled correctly.

Ex:
 (let ((*print-pretty* nil))
   (lambda-in-dynamic-environment (*print-pretty*) (some-form some-other-variable)
     (declare (ignore some-other-variable))
     (print some-form) ))

...will print without pretty printing when invoked (no matter that the dynamic
bindings are at that time).

I find it preferable to actually SETFing the dynamic variables.  If anyone has
a better way to do what I am trying to do, I would like to know it (like not
using dynamic variables at all?)."
  (let ((var-names (tb::get-gensyms (length specials))))
    (multiple-value-bind (body-decl body)
        (split-if (/. (x) (not (eql (car x) 'cl:declare))) body)
      `(let ,(group (shuffle var-names specials) 2)
         (lambda ,vars
           ,@body-decl
           (let ,(group (shuffle specials var-names) 2)
             (declare (special ,@specials))
             ,@body ))))))

(defun time-function (fn &rest args)
  "Call function FN with ARGS and measure the time to do so which is
returned as a float approximating the number of seconds. The time is
passed as the first of the values returned, all the other values are
pushed one down the list.

  We use internal-real-time here as it usually has more precision
than universal time (which is measured in seconds)"
  (let ((start (get-internal-real-time))
        (result (multiple-value-list (apply fn args)))
        (end (get-internal-real-time)) )
    (values-list (cons (float (/ (- end start) internal-time-units-per-second) 0.0)
                       result ))))

(defun play-mp3 (snd)
  "Play the mp3 in the file given by SND using mpg321 through a bash
shell.  We are asking trivial-shell not to wait for completion (the
second arguement passed as nil) but we will return the time it takes
to run this just in case there is some overhead, that way we can
adjust for it elsewhere."
  (time-function
   #'shell-command (strcat "mpg321 -q " snd) ))

(defvar *high-mark* .7)
(defvar *low-mark* .2)

(defvar *pictures*)
(defvar *sounds*)
(defvar *cvs*)

(defvar *announce*)
(defvar *score-board*)
(defvar *score*)
(defvar *?-back*)

(defun n-back-control (n block-size n-times)
  (iter (for i below n-times)
        (itemconfigure *score-board* *?-back*
                         :text (format nil "~A-BACK" n) )
        (let ((score (n-back n block-size)))
          (cond ((> score *high-mark*)
                 (incf n) )
                ((and (< score *low-mark*) (> n 1))
                 (decf n) )))))

(defun n-back (n block-size)
  (intro n)
  (let ((pic-ring (alexandria:make-circular-list n))
        (snd-ring (alexandria:make-circular-list n))
        (score 0)
        (errors 0)
        (matches 0) )
    (iter (for i below (+ n block-size))
          (for pics on pic-ring)
          (for snds on snd-ring)
          (itemconfigure *score-board* *score*
                         :text (format nil "SCORE: ~A"
                                       (if (= 0 matches)
                                           0.5 (/ (- score errors) matches) )))
          (reset-keys)
          (for snd next (play-sound))
          (for pic next (display-picture))
          (sleep 2.5)
          (process-events)
          (when (car pic-ring)
            (let (snd-match pic-match)
              (when (eql (car pics) pic)
                (setf pic-match t)
                (incf matches) )
              (when (equal (car snds) snd)
                (setf snd-match t)
                (incf matches) )
              (when (and snd-match (snd-keypressed))
                (incf score) )
              (when (and pic-match (pic-keypressed))
                (incf score) )
              (when (or (and snd-match (not (snd-keypressed)))
                        (and (not snd-match) (snd-keypressed)) )
                (incf errors) )
              (when (or (and pic-match (not (pic-keypressed)))
                        (and (not pic-match) (pic-keypressed)) )
                (incf errors) )
              ;(format t "Iteration ~A~%  Sound Match: ~A~%  Picture Match: ~A~%  Sound Pressed: ~A~%  Picture Pressed: ~A~%  SCORE: ~A ERRORS: ~A MATCHES: ~A~%" i snd-match pic-match (snd-keypressed) (pic-keypressed) score errors matches)
              ))
          (setf (car pics) pic
                (car snds) snd )
          (finally (return (if (= 0 matches) 0.5 (/ (- score errors) matches)))) )))

;;;; A set of closures to handle key presses
(let (snd-key pic-key)
  (defun reset-keys ()
    (setf snd-key nil
          pic-key nil ))
  (defun pic-keypressed ()
    pic-key )
  (defun snd-keypressed ()
    snd-key )
  (defun handle-pic-key (evt)
    (declare (ignore evt))
    (setf pic-key t) )
  (defun handle-snd-key (evt)
    (declare (ignore evt))
    (setf snd-key t) ))

(defun play-sound ()
  (let ((snd (alexandria:random-elt *sounds*)))
    (bordeaux-threads:make-thread
     (/. () (play-mp3 snd))
     :name 'play-sound )
    snd ))

(defun display-picture ()
  (let ((pic (alexandria:random-elt *pictures*)))
    (itemconfigure *cvs* pic :fill :blue)
    (sleep .5)
    (process-events)
    (itemconfigure *cvs* pic :fill "")
    pic ))

(defun intro (n)
  (mapcar (/. (pic)
            (itemconfigure *cvs* pic :state :hidden) )
          *pictures* )
  (itemconfigure *cvs* *announce* :text (format nil "GET READY FOR ~A-BACK!!!" n))
  (dotimes (i 4)
    (itemconfigure *cvs* *announce* :state :normal)
    (sleep .5)
    (itemconfigure *cvs* *announce* :state :hidden)
    (sleep .2) )
  (mapcar (/. (pic)
            (itemconfigure *cvs* pic :state :normal) )
          *pictures* ))

(defun run-n-back (n &key (n-times 10) (block-size 20))
  "Run a dual n-back game."
  (handler-bind
      ;; Handle broken pipes, just quit from the program
      ((stream-error (/. (condition)
                       (declare (ignore condition))
                       (return-from run-n-back) )))
    (with-ltk ()
      (let* ((*score-board* (make-instance 'canvas :height 60 :width 600))
             (*score* (create-text *score-board* 20 20 "SCORE: 0"))
             (*?-back* (create-text *score-board* 500 20 (format nil "~A-BACK" n)))
             (*cvs* (make-instance 'canvas :height 600 :width 600))
             (*announce* (create-text *cvs* 200 290 (format nil "GET READY FOR ~A-BACK!!!" n)))
             (*pictures* (iter outer
                               (for i below 3)
                               (iter (for j below 3)
                                     (in outer
                                         (collect (create-rectangle
                                                   *cvs*
                                                   (* 200 i)
                                                   (* 200 j)
                                                   (* 200 (1+ i))
                                                   (* 200 (1+ j)) ))))))
             (*sounds*
              (ppcre:split "\\s+"
                           (shell-command "echo ~/src/haskell/hback-0.0.2/sounds/*.mp3") ))
             (n-text-field (make-instance 'entry :text (mkstr n)))
             (start-n-back (make-instance
                            'button :text "Play N-Back"
                            :command (lambda-in-dynamic-environment
                                         (*score-board* *score* *?-back* *cvs* *announce*
                                                        *pictures* *sounds* )
                                         ()
                                       (n-back-control n block-size n-times) ))))
        ;; Hide the announcement text
        (itemconfigure *cvs* *announce* :state :hidden)
        ;; Put borders around the blocks
        (mapcar (/. (block)
                  (itemconfigure *cvs* block "outline" "black")
                  (itemconfigure *cvs* block :fill "") )
                *pictures* )
        ;; set up key bindings
        (bind *tk* "<q>" (/. (evt) (declare (ignore evt)) (return-from run-n-back)))
        (bind *tk* "<o>" #'handle-snd-key)
        (bind *tk* "<p>" #'handle-pic-key)
        ;; Packing the widgets
        (pack *score-board*)
        (pack *cvs*)
        (pack n-text-field :side :right)
        (pack start-n-back :side :right) ))))

;; This does not work (hangs while processing events, though it seems it shouldn't).
(defun process-commands-while-waiting (seconds &optional (delay-time 1/20))
  (let ((time 0))
    (tb:while (< time seconds)
      (incf time (time-function #'process-events))
      (sleep delay-time) )))
