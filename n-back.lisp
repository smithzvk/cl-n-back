
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
   #'create-shell-process (strcat "mpg321 -q " snd) nil ))

(defun show-block (cvs block)
  (itemconfigure cvs block "fill" "blue") )

(defun hide-block (cvs block)
  (itemconfigure cvs block "fill" "") )



(defvar *high-mark* .7)
(defvar *low-mark* .2)

(let ((block-size 20))
  )

(defun make-queues (n)
  (list #1=(funds:make-queue
            :initial-contents
            (make-list n :initial-element nil) )
        #1# ) )

(let ((block-size 20)
      (pics '(1 2 3))
      (snds '(a b c)) )
  )

(defun n-back (n &key (block-size 20))
  "Run a dual n-back game."
  (with-ltk ()
    (let* ((cvs (make-instance 'canvas :height 600 :width 600))
           (blocks (iter outer
                         (for i below 3)
                         (iter (for j below 3)
                               (in outer
                                   (collect (create-rectangle cvs
                                                              (* 200 i)
                                                              (* 200 j)
                                                              (* 200 (1+ i))
                                                              (* 200 (1+ j)) ))))))
           (sounds
            (split "\\s+"
                   (shell-command "echo ~/src/haskell/hback-0.0.2/sounds/*.mp3") )))
      (mapcar (/. (block)
                (itemconfigure cvs block "outline" "black")
                (hide-block cvs block) )
              blocks )
;      (return-from function)
      (pack cvs)
;;       (labels
;;           ((display-picture (pic)
;;              (show-block cvs pic)
;;              (sleep 2.5)
;;              (hide-block cvs pic) )
;;            (n-back-block (n iters score &rest queues)
;;              (cond ((= 0 iters)
;;                     (n-back-retrain n score) )
;;                    (t (let ((pelt (random-elt pics))
;;                             (selt (random-elt snds))
;;                             (time 0) )
;;                         (play-mp3 selt)
;;                         (display-picture belt)
;;                         (apply #'n-back-block n (1- iters) score queues) ))))
;;            (n-back-retrain (n score)
;;              (condlet (((> score *high-mark*)
;;                         (n (1+ n)) )
;;                        ((< score *low-mark*)
;;                         (n (1- n)) ))
;;                (apply #'n-back-block block-size 0 (make-queues n)) ))))
        ;; Run the N-Back game
                                        ;(apply #'n-back-block n block-size 0 (make-queues n)) )))))
        (iter (for i below (+ n block-size))
              (let ((belt (random-elt blocks))
                    (selt (random-elt sounds)) )
                (show-block cvs belt)
                (play-mp3 selt)
                (sleep 1/2)
                (hide-block cvs belt)
                (sleep 5/2) )))))
