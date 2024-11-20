(uiop:define-package :clawdot
  (:use :cl))

(uiop:define-package :%aw-godot)

(in-package :clawdot)

(if (member :darwin *features*)
    (cffi:load-foreign-library "/Users/jason/common-lisp/libresect.dylib")
    (cffi:load-foreign-library "/home/jason/common-lisp/libresect.so"))

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

(princ (collect-godot-cpp-headers))

(defun ignore-uninstantiable ()
  (claw.resect:ignore-some
   (claw.resect:ignore-every
    (claw.resect:ignore-names
     "godot::Ref.*"
     "godot::PropertyInfo.*")
    (claw.resect:ignore-not
     (claw.resect:ignore-names
      "godot::Ref<.*")))))

(claw:defwrapper (:aw-godot
                    (:system :clawdot/wrapper)
                    (:headers "godot_cpp/classes/animatable_body2d.hpp"
                              "godot_cpp/classes/animatable_body3d.hpp"
                              "godot_cpp/classes/animation_player.hpp"
                              "godot_cpp/classes/area2d.hpp"
                              "godot_cpp/classes/area3d.hpp"
                              "godot_cpp/classes/audio_listener2d.hpp"
                              "godot_cpp/classes/audio_listener3d.hpp"
                              "godot_cpp/classes/box_shape3d.hpp"
                              "godot_cpp/classes/camera2d.hpp"
                              "godot_cpp/classes/camera3d.hpp"
                              "godot_cpp/classes/character_body2d.hpp"
                              "godot_cpp/classes/character_body3d.hpp"
                              "godot_cpp/classes/collision_shape2d.hpp"
                              "godot_cpp/classes/collision_shape3d.hpp"
                              "godot_cpp/classes/cylinder_shape3d.hpp"
                              "godot_cpp/classes/input_event_action.hpp"
                              "godot_cpp/classes/input_event_key.hpp"
                              "godot_cpp/classes/input_event.hpp"
                              "godot_cpp/classes/input_event_joypad_button.hpp"
                              "godot_cpp/classes/input_event_joypad_motion.hpp"
                              "godot_cpp/classes/input_event_mouse_button.hpp"
                              "godot_cpp/classes/input_event_mouse_motion.hpp"
                              "godot_cpp/classes/input_event_mouse.hpp"
                              "godot_cpp/classes/kinematic_collision2d.hpp"
                              "godot_cpp/classes/kinematic_collision3d.hpp"
                              "godot_cpp/classes/light2d.hpp"
                              "godot_cpp/classes/light3d.hpp"
                              "godot_cpp/classes/material.hpp"
                              "godot_cpp/classes/mesh_instance2d.hpp"
                              "godot_cpp/classes/mesh_instance3d.hpp"
                              "godot_cpp/classes/node2d.hpp"
                              "godot_cpp/classes/node3d.hpp"
                              "godot_cpp/classes/object.hpp"
                              "godot_cpp/classes/path2d.hpp"
                              "godot_cpp/classes/path3d.hpp"
                              "godot_cpp/classes/ray_cast2d.hpp"
                              "godot_cpp/classes/ray_cast3d.hpp"
                              "godot_cpp/classes/rectangle_shape2d.hpp"
                              "godot_cpp/classes/resource_importer_image_font.hpp"
                              "godot_cpp/classes/resource_importer_image.hpp"
                              "godot_cpp/classes/resource_importer_mp3.hpp"
                              "godot_cpp/classes/resource_importer_obj.hpp"
                              "godot_cpp/classes/resource_importer_ogg_vorbis.hpp"
                              "godot_cpp/classes/resource_importer_scene.hpp"
                              "godot_cpp/classes/rigid_body2d.hpp"
                              "godot_cpp/classes/rigid_body3d.hpp"
                              "godot_cpp/classes/scene_tree.hpp"
                              "godot_cpp/classes/sphere_shape3d.hpp"
                              "godot_cpp/classes/sprite2d.hpp"
                              "godot_cpp/classes/sprite3d.hpp"
                              "godot_cpp/classes/standard_material3d.hpp"
                              "godot_cpp/classes/static_body2d.hpp"
                              "godot_cpp/classes/static_body3d.hpp"
                              "godot_cpp/variant/aabb.hpp"
                              "godot_cpp/variant/array.hpp"
                              "godot_cpp/variant/array_helpers.hpp"
                              "godot_cpp/variant/basis.hpp"
                              "godot_cpp/variant/builtin_types.hpp"
                              "godot_cpp/variant/callable.hpp"
                              "godot_cpp/variant/char_string.hpp"
                              "godot_cpp/variant/char_utils.hpp"
                              "godot_cpp/variant/dictionary.hpp"
                              "godot_cpp/variant/plane.hpp"
                              "godot_cpp/variant/projection.hpp"
                              "godot_cpp/variant/quaternion.hpp"
                              "godot_cpp/variant/rect2.hpp"
                              "godot_cpp/variant/rect2i.hpp"
                              "godot_cpp/variant/string.hpp"
                              "godot_cpp/variant/string_name.hpp"
                              "godot_cpp/variant/transform2d.hpp"
                              "godot_cpp/variant/transform3d.hpp"
                              "godot_cpp/variant/typed_array.hpp"
                              "godot_cpp/variant/utility_functions.hpp"
                              "godot_cpp/variant/variant.hpp"
                              "godot_cpp/variant/vector3.hpp"
                              "godot_cpp/variant/vector3i.hpp"
                              "godot_cpp/variant/vector4.hpp"
                              "godot_cpp/variant/vector4i.hpp"
                              )
                    (:includes :godot-includes :godot-gen-includes)
                    (:targets ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
                              ((:and :x86-64 :windows) "x86_64-pc-windows-gnu")
                              ((:and :aarch64 :darwin) "aarch64-pc-darwin-gnu"))
                    (:persistent :godot-bindings
                     :depends-on (:claw-utils)
                     :asd-path "src/godot-bindings.asd"
                     :bindings-path "src/bindings/")
                    (:language :c++)
                    (:standard :c++17)
                    (:include-definitions "^godot::.*")
                    (:exclude-definitions "^godot::.*::_.*" "godot::EditorPlugins.*" "^godot::internal::.*" "^godot::GetTypeInfo.*" "^godot::ClassDB.*" ".*_MethodBindings" ".*atomic.*" ".*_gde_.*"))
    :in-package :%aw-godot
    :trim-enum-prefix t
    :recognize-bitfields t
    :recognize-strings t
  :ignore-entities (ignore-uninstantiable)
    :with-adapter (:static
                   :path "lib/adapter.cxx")
    :symbolicate-names (:in-pipeline
                        ;; (:by-removing-complex-prefix "^m[A-Z]\\w*" 1)
                        (:by-removing-prefixes "gd"))
    :override-types ((:string claw-utils:claw-string)
                     (:pointer claw-utils:claw-pointer)
                     (:char16 :uint16)
                     (:char32 :uint32)
                     ))


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
