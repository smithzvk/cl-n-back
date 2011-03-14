
;; cl-n-back : An N-Back implementation

(in-package :cl-n-back)

(defun time-function (fn &rest args)
  "Call function FN with ARGS and measure the time to do so which is returned
as a float approximating the number of seconds. The time is passed as the first
of the values returned, all the other values are pushed one down the list.

We use internal-real-time here as it usually has more precision than universal
time (which is measured in seconds)"
  (let ((start (get-internal-real-time))
        (result (multiple-value-list (apply fn args)))
        (end (get-internal-real-time)) )
    (values-list (cons (float (/ (- end start) internal-time-units-per-second) 0.0)
                       result ))))

(defmacro timed-progn (&body body)
  "Run code in an environment that measures how long it takes.  Return the time
it takes as the first return value, the rest will be the expected return values
but shifted by one."
  `(let ((start (get-internal-real-time))
         end )
     (let ((result
             (multiple-value-list
                 (unwind-protect
                      (progn ,@body)
                   (setf end (get-internal-real-time)) ))))
       (let ((time (float (/ (- end start) internal-time-units-per-second) 0.0)))
         (when (< time 0)
           (error "Time was negative.  Perhaps the internal time has wrapped around?") )
         (values-list (cons time result)) ))))

(defun play-mp3 (snd)
  "Play the mp3 in the file given by SND using mpg321 through a bash shell.  We
don't want to wait for completion so timing works correctly."
  ;; Background the shell command and send output to /dev/null and the command
  ;; won't wait.
  (timed-progn #>(mpg321 -q ,snd > /dev/null 2>&1 &)) )

(defvar *high-mark* .7)
(defvar *low-mark* .2)

(defvar *pictures*)
(defvar *sounds*)
(defvar *cvs*)

(defvar *announce*)
(defvar *score-board*)
(defvar *score*)
(defvar *?-back*)

(defvar *delay* 2.5)
(defvar *picture-delay* 0.5)

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
  ;; Mark the beginning of a new round by some flashing text
  (intro n)
  ;; {pic,snd}-ring hold the old pictures and sounds
  (let ((pic-ring (alexandria:make-circular-list n))
        (snd-ring (alexandria:make-circular-list n))
        (score nil)
        (correct 0)
        (errors 0)
        (matches 0) )
    (iter (for i below (+ n block-size))
          (for pics on pic-ring)
          (for snds on snd-ring)
          (itemconfigure *score-board* *score*
                         :text (format nil "SCORE: ~A"
                                       (if score
                                           (round (* 1000 score))
                                           'N/A )))
          (reset-keys)
          (for snd next (play-sound))
          (for pic next (display-picture))
          (sleep *delay*)
          (process-events)
          (when (car pic-ring) ; i.e. when we have actually gone
                               ; through at least N iterations
            (let (snd-match pic-match)
              (when (eql (car pics) pic)
                (setf pic-match t)
                (incf matches) )
              (when (equal (car snds) snd)
                (setf snd-match t)
                (incf matches) )
              (when (and snd-match (snd-keypressed))
                (incf correct) )
              (when (and pic-match (pic-keypressed))
                (incf correct) )
              (when (or (and snd-match (not (snd-keypressed)))
                        (and (not snd-match) (snd-keypressed)) )
                (incf errors) )
              (when (or (and pic-match (not (pic-keypressed)))
                        (and (not pic-match) (pic-keypressed)) )
                (incf errors) )
              (when (< 0 (+ correct errors))
                (setf score (/ correct (+ correct errors))) )
              ;(format t "Iteration ~A~%  Sound Match: ~A~%  Picture Match: ~A~%  Sound Pressed: ~A~%  Picture Pressed: ~A~%  SCORE: ~A ERRORS: ~A MATCHES: ~A~%" i snd-match pic-match (snd-keypressed) (pic-keypressed) score errors matches)
              ))
          (setf (car pics) pic
                (car snds) snd )
          (finally (itemconfigure *score-board* *score*
                         :text (format nil "SCORE: ~A"
                                       (if score
                                           (round (* 1000 score))
                                           'N/A )))
                   (return (or score 0.5))) )))

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
     :name "play-sound" )
    snd ))

(defun display-picture ()
  (let ((pic (alexandria:random-elt *pictures*)))
    (itemconfigure *cvs* pic :fill :blue)
    (sleep *picture-delay*)
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
             (*sounds* #>(ls ~/src/lisp/packages/cl-n-back/sounds/*.mp3)/#\Newline)
             (n-text-field (make-instance 'entry :text (mkstr n)))
             (n-times-field (make-instance 'entry :text (mkstr n-times)))
             (block-size-field (make-instance 'entry :text (mkstr block-size)))
             (start-n-back (make-instance
                            'button :text "Play N-Back"
                            :command (lambda-in-dyn-env
                                         (*cvs* *?-back* *score* *score-board* *announce*
                                                *pictures* *sounds* *delay* *picture-delay* )
                                         ()
                                       (unwind-protect
                                            (catch 'stop-n-back
                                              (configure n-text-field :state :disabled)
                                              (configure n-times-field :state :disabled)
                                              (configure block-size-field :state :disabled)
                                              (n-back-control
                                               (read-from-string (text n-text-field))
                                               block-size n-times ))
                                         (configure n-text-field :state :normal)
                                         (configure n-times-field :state :normal)
                                         (configure block-size-field :state :normal) )))))
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
        (pack n-text-field :side :left)
        (pack start-n-back :side :left)
        (pack n-times-field :side :left)
        (pack block-size-field :side :left) ))))

;; This does not work (hangs while processing events, though it seems it shouldn't).
(defun process-commands-while-waiting (seconds &optional (delay-time 1/20))
  (let ((time 0))
    (tb::while (< time seconds)
      (incf time (time-function #'process-events))
      (sleep delay-time) )))
