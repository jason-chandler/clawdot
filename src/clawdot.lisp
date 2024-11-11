(uiop:define-package :clawdot
  (:use :cl))

(uiop:define-package :%aw-godot)

(in-package :clawdot)

(cffi:load-foreign-library "/home/jason/common-lisp/libresect.so")

(defmacro defctype-helper (unknown-type)
  `(cffi:defctype ,unknown-type (claw-utils:claw-pointer :void)))

(defun handle-undefined-foreign-type (condition)
  (let ((unknown-type (cffi::undefined-foreign-type-error-type condition)))
    (warn "Replacing unknown foreign type ~A with (claw-utils:claw-pointer :void)" unknown-type)
    (defctype-helper unknown-type)))

(defmacro clawdot::defitype (name c-type &optional doc)
  ;; Establish the handler during macro expansion
  (handler-bind ((cffi::undefined-foreign-type-error
                  (lambda (condition)
                    (handle-undefined-foreign-type condition)
                    (invoke-restart 'retry))))
    ;; Expand the original macro within the handler
    (macroexpand `(iffi:defitype ,name ,c-type ,doc))))

(defun list-hpp-files (directory)
  (uiop:directory-files
   (uiop:merge-pathnames* "*.hpp" directory)))


(defun collect-godot-cpp-headers ()
  (labels ((remove-prefix (str)
             (let* ((path-root (namestring (asdf:system-relative-pathname :clawdot "./src/godot-cpp/gen/include/")))
                    (full-path (namestring str))
                    (pos (search path-root full-path)))
               (subseq full-path (+ pos (length path-root))))))
  (cons :headers
        (mapcar #'remove-prefix (list-hpp-files (asdf:system-relative-pathname :clawdot "./src/godot-cpp/gen/include/godot_cpp/classes/"))))))

(let ((godot-cpp-headers (collect-godot-cpp-headers)))
  (eval `(claw:defwrapper (:aw-godot
                    (:system :clawdot/wrapper)
                    (:headers "godot_cpp/classes/node3d.hpp")
                    (:includes :godot-includes :godot-gen-includes)
                    (:targets ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
                              ((:and :x86-64 :windows) "x86_64-pc-windows-gnu"))
                    (:persistent :godot-bindings
                     :depends-on (:claw-utils)
                     :asd-path "/home/jason/common-lisp/clawdot/src/godot-bindings.asd"
                     :bindings-path "/home/jason/common-lisp/clawdot/src/bindings/")
                    (:language :c++)
                    (:standard :c++17)
                    (:include-definitions "^godot::.*" "GDExtensionPropertyInfo" ".*char.*" ".*type" ".*size.*" ".*_ptr.*" ".*atomic.*"))
    :in-package :%aw-godot
    :trim-enum-prefix t
    :recognize-bitfields t
    :recognize-strings t
    :with-adapter (:static
                   :path "lib/adapter.cxx")
    :symbolicate-names (:in-pipeline
                        (:by-removing-complex-prefix "^m[A-Z]\\w*" 1)
                        (:by-removing-prefixes "gd"))
    :override-types ((:string claw-utils:claw-string)
                     (:pointer claw-utils:claw-pointer)
                     (%AW-GODOT::CHAR16 :char16_t)
                     (%AW-GODOT::CHAR32 :char32_t)
                     ))))

;; (:char16 :uint16)
                   ;; (:CHAR16 :uint16)
                   ;; (:CHAR32 :uint32)
                   ;; (:char32 :uint32)

;; (cffi:defctype %godot::char16 :uint16)
;; (cffi:defctype %godot::char32 :uint32)
;; (claw:generate-wrapper :aw-godot)

;;(clawdot::generate-and-load-wrapper)




;; (claw:load-wrapper :aw-godot)
(defun generate-and-load-wrapper ()
  (handler-bind ((cffi::foreign-type-error #'handle-undefined-foreign-type))
    (claw:generate-wrapper :aw-godot)
    ;;(replace-iffi-defitype-in-directory #P"./lib")
    (claw:load-wrapper :aw-godot)))



;; replace iffi definition
;;

(defun list-lisp-files (directory)
  (uiop:directory-files
   (uiop:merge-pathnames* "*.lisp" directory)
   :test 'uiop:file-exists-p
   :directories t))

(defun replace-defitype-in-form (form)
  (cond
    ((consp form)
     (let ((head (first form)))
       (if (and (symbolp head)
                (string= (symbol-name head) "DEFITYPE")
                (string= (symbol-package-name head) "IFFI"))
           (cons (intern "DEFITYPE" "CLAWDOT")
                 (mapcar #'replace-defitype-in-form (rest form)))
           (mapcar #'replace-defitype-in-form form))))
    (t form)))

(defun replace-defitype (contents)
  (with-input-from-string (in contents)
    (let ((forms '()))
      (loop for form = (read in nil nil)
            while form
            do (push form forms))
      (let ((modified-forms (mapcar #'replace-defitype-in-form (reverse forms))))
        (with-output-to-string (out)
          (dolist (form modified-forms)
            (prin1 form out)
            (terpri out)))))))

(defun process-lisp-file (file)
  (let ((backup-file (uiop:parse-native-namestring
                      (concatenate 'string (namestring file) ".bak"))))
    (uiop:copy-file file backup-file :overwrite t)
    (with-open-file (in file :direction :input)
      (let* ((contents (make-string (file-length in)))
             (read-count (read-sequence contents in)))
        (declare (ignore read-count))
        (let ((new-contents (replace-defitype contents)))
          (with-open-file (out file :direction :output :if-exists :supersede)
            (write-string new-contents out)))))))

(defun replace-iffi-defitype-in-directory (directory)
  (let ((files (list-lisp-files directory)))
    (dolist (file files)
      (format t "Processing file: ~a~%" file)
      (process-lisp-file file))))

;; Usage





;; (defmacro with-undefined-foreign-type-handler (&body body)
;;   ;; Establish the handler during compile time
;;   (eval-when (:compile-toplevel)
;;     (handler-bind ((cffi::undefined-foreign-type-error
;;                     (lambda (condition)
;;                       (handle-undefined-foreign-type condition)
;;                       (invoke-restart 'retry))))
;;       (dolist (form `,body)
;;         (eval form))))
;;   ;; At load and execution time, just execute the body normally
;;   `(progn ,@body))



;; (handler-bind ((cffi::undefined-foreign-type-error #'(lambda (c)
;;                                                        (use-value (handle-undefined-foreign-type c))
;;                                                        (let ((r (find-restart 'retry)))
;;                                                          (when r (invoke-restart r))))))
;;     (iffi:defitype %aw-godot::%%madeup-t
;;       %aw-godot::|C@SA@LSDIV-T|
;;       "/usr/include/x86_64-linux-gnu/bits/types.h:41:20"))





;; (defmacro defctype-helper (unknown-type)
;;    `(cffi:defctype ,unknown-type (claw-utils:claw-pointer :void)))

;; (macroexpand-1 `(defctype-helper ,(cffi::foreign-type-error/compound-name (error 'cffi::unknown-foreign-type-error))))

;; (defun handle-undefined-foreign-type (condition)
;;   (let ((unknown-type (cffi::foreign-type-error/compound-name condition)))
;;     (warn "Replacing unknown foreign type ~A with (claw-utils:claw-pointer :void)" unknown-type)
;;     (defctype-helper unknown-type)
;;     (let ((r (find-restart 'retry condition)))
;;       (when r (invoke-restart r)))))




;; (defitype-with-handler %aw-godot::%%madeup-t
;;     %aw-godot::|C@SA@LSDIV-T|
;;     "/usr/include/x86_64-linux-gnu/bits/types.h:41:20")
;; (defitype-with-handler )
