;;;; cv-tests.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(in-package #:cv-tests)

(cffi:load-foreign-library "/opt/local/lib/libopencv_highgui.dylib")
(cffi:load-foreign-library "/opt/local/lib/libopencv_flann.dylib")
(cffi:load-foreign-library "/opt/local/lib/libopencv_core.dylib")

(defmacro with-gui-thread (&body body)
  "Wraps BODY in code which masks float traps.
   This is needed in SBCL on OSX because native code often
   generate :inexact traps and land you in the debugger.
   For non SBCL this wraps body in a progn."
  `(trivial-main-thread:call-in-main-thread
    (lambda () 
      #+sbcl (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow)
	       ,@body)
      #+ccl (unwind-protect (progn
			       (ccl:set-fpu-mode :invalid nil)
			       ,@body)
	       (ccl:set-fpu-mode :invalid t))
      #-(or sbcl ccl)
      ,@body)))

(defun edge-detect-image (file-name)
  (with-gui-thread
    (cv:with-named-window (file-name)
      (let* ((image (cv:load-image file-name)))
        (cv:with-ipl-images ((src (cv:get-size image) cv:+ipl-depth-8u+ 1)
                             (dst (cv:get-size image) cv:+ipl-depth-8u+ 1)
                             (final (cv:get-size image) cv:+ipl-depth-32f+ 1))
          (cv:cvt-color image src cv:+bgr-2-gray+)
          (cv:canny src dst 100.0d0 200.0d0 3)
          (cv:threshold dst dst 1.0 255 cv:+thresh-binary-inv+)
          (cv:dist-transform dst final cv:+dist-l2+ 3 (list 10.2 20.2))
          (cv:show-image file-name final))
        (loop
           (let ((c (cv:wait-key 33)))
             (when (= c 27)
               (return))))))))


;; (defun read-barcode (file-name &optional (threshold 127) )
;;   (let* ((img (png:decode-file file-name))
;;          (width (array-dimension img 1))
;;          (height (array-dimension img 0))
;;          (bars nil)
;;          (number-mapping #( #(3 2 1 1) ;; 0
;;                          #(2 2 2 1) ;; 1
;;                          #(2 1 2 2) ;; 2
;;                          #(1 4 1 1) ;; 3
;;                          #(1 1 3 2) ;; 4
;;                          #(1 2 3 1) ;; 5
;;                          #(1 1 1 4) ;; 6
;;                          #(1 3 1 2) ;; 7
;;                          #(1 2 1 3) ;; 8
;;                          #(3 1 1 2))))
;;     (threshold img)

;;     (loop
;;        with found-bar = nil
;;        with bar-width = 0
;;        for j below width do
;;          (cond ((and (< (aref img (floor (/ height 3)) j 0) threshold))
;;                 (incf bar-width)
;;                 (setf found-bar t))
;;                (found-bar
;;                 (setf found-bar nil)
;;                 (format t "Last bar was of width ~a~%" bar-width)
;;                 (push bar-width bars)
;;                 (setf bar-width 0))))
;;     (format t "Found bars: ~a~%" (reverse bars ))))

(defun read-bar-code (img display &key (threshold 100))
  (let* ((size (cv:get-size img))
         (width (cv:size-width size))
         (height (cv:size-height size))
         (bars nil))
    (format t "Checking image of size: ~ax~a~%" width height)
    (handler-case 
        (loop
           with bar-start = nil
           for j below width do
             (let ((y-coord (floor (/ height 3))))
               (cond ((and (> (cv:get-real-2d img y-coord j) threshold)
                           (null bar-start))
                      (setf bar-start j)
                      (cv:set-real-2d display y-coord j 255))
                     ((and (> (cv:get-real-2d img y-coord j) threshold)
                           bar-start)
                      (push (cons j bar-start) bars)
                      
                      (setf bar-start nil))
                     
                     (bar-start
                      (cv:set-real-2d display y-coord j 255)))))
      (error (err) (format t "Error: ~a~%" err)))
    (format t "Found bars: ~a~%" bars)
    bars))

(defun read-bar-code-from-camera (&key (camera 0) (fps 60))
  (with-gui-thread
    (cv:with-named-window ("bar-code-scanner")
      (cv:with-captured-camera (vid :width 1920 :height 1080
                                    :idx camera)
        (loop
           (let* ((frame (cv:query-frame vid)))
             (cv:with-ipl-images ((src (cv:get-size frame) cv:+ipl-depth-8u+ 1)
                                  (dst (cv:get-size frame) cv:+ipl-depth-8u+ 1)
                                  (final (cv:get-size frame) cv:+ipl-depth-32f+ 1))
               
               (cv:cvt-color frame src cv:+rgb-2-gray+)
               (cv:canny src dst 80.0 100.0 7)
               (cv:adaptive-threshold dst dst 255 cv:+adaptive-thresh-mean-c+ cv:+thresh-binary+ 7 7 )
               (read-bar-code dst dst)
               (cv:show-image "bar-code-scanner" dst))
             (let ((c (cv:wait-key (floor (/ 1000 fps)))))
               (when (= c 27)
                 (return)))))))))
