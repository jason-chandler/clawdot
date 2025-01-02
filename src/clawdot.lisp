(uiop:define-package :clawdot
  (:use :cl))

(uiop:define-package :%aw-godot)

(cffi::defctype %aw-godot::char16 :uint16)
(cffi::defctype %aw-godot::char32 :uint32)

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
  (lambda (e)
    (funcall (claw.resect:ignore-some
               (claw.resect:ignore-every
                 (claw.resect:ignore-names
                   "godot::Ref.*"
                   "godot::List.*"
                   "godot::List::Element.*"
                   "godot::PropertyInfo.*"
                   "godot::.*<char.*>.*")
                 (claw.resect:ignore-not
                  (claw.resect:ignore-names
                    "godot::Ref<.*"
                    "godot::List<.*"
                    "godot::List::Element<.*"
                    "godot::List::Iterator<.*")))) e)
    (funcall (claw.resect:ignore-functions
	      (:in-class "godot::BitField<godot::KeyModifierMask>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::KeyModifierMask&>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::KeyModifierMask>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::KeyModifierMask&>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::Mesh::ArrayFormat>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::Mesh::ArrayFormat&>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::Mesh::ArrayFormat>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::Mesh::ArrayFormat&>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::MethodFlags>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::MethodFlags>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::MethodFlags&>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::MethodFlags&>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::MouseButtonMask>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::MouseButtonMask>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::MouseButtonMask&>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::MouseButtonMask&>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::Node::ProcessThreadMessages>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::Node::ProcessThreadMessages>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::Node::ProcessThreadMessages&>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::Node::ProcessThreadMessages&>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::PropertyUsageFlags>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::PropertyUsageFlags>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::PropertyUsageFlags&>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::PropertyUsageFlags&>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::TextServer::JustificationFlag&>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::TextServer::JustificationFlag>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::TextServer::JustificationFlag&>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::TextServer::JustificationFlag>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::TextServer::FontStyle>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::TextServer::FontStyle&>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::TextServer::FontStyle>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::TextServer::FontStyle&>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::TextServer::GraphemeFlag>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::TextServer::GraphemeFlag&>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::TextServer::GraphemeFlag>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::TextServer::LineBreakFlag>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::TextServer::LineBreakFlag>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::TextServer::LineBreakFlag&>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::TextServer::LineBreakFlag&>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::TextServer::TextOverrunFlag>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::TextServer::TextOverrunFlag>"
			 ("operator long"))
	      (:in-class "godot::BitField<godot::TextServer::TextOverrunFlag&>"
			 ("operator long"))
	      (:in-class "godot::BitField<const godot::TextServer::TextOverrunFlag&>"
			 ("operator long"))
               (:in-class "godot::GDExtensionBinding"
                          ("init"))
               (:in-class "godot::GDExtensionBinding::InitObject"
                          (:ctor)
                          ("InitObject" :any :any :any)
                          (:dtor))
               (:in-class "godot::CallableCustom"
                          ("call" :any :any :any :any))
               (:in-class "godot::CallableCustomBase"
                          ("call" :any :any))
               (:in-class "godot::List::ConstIterator"
                          (:ctor)
                          (:dtor))
               (:in-class "godot::List::Element"
                          (:ctor)
                          ("erase")
                          (:dtor))
               (:in-class "godot::List::Iterator"
                          (:ctor)
                          (:dtor))
               (:in-class "godot::List<godot::PropertyInfo>"
                          ("ConstIterator" :any)
                          ("begin")
                          ("end")
                          ("erase" :any)
			  ("operator ConstIterator" :any)
			  ("Iterator" :any)
			  ("const operator ConstIterator" :any)
			  ("operator ConstIterator const" :any)
                          ("sort_custom")
                          ("sort_custom_inplace")
                          ("sort"))
               (:in-class "godot::List<godot::PropertyInfo>::ConstIterator"
                          (:ctor :any :any)
                          (:ctor :any)
                          (:ctor)
                          ("ConstIterator" :any)
			  ("operator ConstIterator" :any)
			  ("const operator ConstIterator" :any)
			  ("operator ConstIterator const" :any)
                          ("begin")
                          ("end")
                          (:dtor))
	       (:in-class "godot::List<godot::PropertyInfo>::Iterator"
                          (:ctor :any :any)
                          (:ctor :any)
                          (:ctor)
                          ("ConstIterator" :any)
			  ("operator ConstIterator" :any)
			  ("const operator ConstIterator" :any)
			  ("operator ConstIterator const" :any)
                          ("begin")
                          ("end")
                          (:dtor))
               (:in-class "godot::MethodBind"
                          ("bind_call" :any :any :any :any :any :any)
                          ("bind_ptrcall" :any :any :any :any)
                          ("call" :any :any :any "int*")
                          ("get_arguments_metadata_list")
                          ("get_argument_metadata" :any)
                          ("get_argument_type")
                          ("ptrcall" :any :any :any))
	       (:in-class "godot::ObjectId"
			  (:ctor)
			  ("operator long")
			  (:dtor))
               (:in-class "godot::PtrToArg<const char&>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
               (:in-class "godot::PtrToArg<const char*>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
               (:in-class "godot::PtrToArg<char>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
               (:in-class "godot::PtrToArg<char16*>"
                          (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
                          (:dtor))
               (:in-class "godot::PtrToArg<const char16*>"
                          (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
                          (:dtor))
               (:in-class "godot::PtrToArg<char32*>"
                          (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
                          (:dtor))
               (:in-class "godot::PtrToArg<const char32*>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
               (:in-class "godot::PtrToArg<const double&>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
               (:in-class "godot::PtrToArg<const double*>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
               (:in-class "godot::PtrToArg<double>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
               (:in-class "godot::PtrToArg<const int&>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
               (:in-class "godot::PtrToArg<const int*>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
               (:in-class "godot::PtrToArg<int>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
	       (:in-class "godot::PtrToArg<long>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
	       (:in-class "godot::PtrToArg<long*>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
	       (:in-class "godot::PtrToArg<const long*>"
                          (:ctor)
			  ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
                          (:dtor))
	       (:in-class "godot::PtrToArg<const long&>"
                          (:ctor)
			  ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
                          (:dtor))
	       (:in-class "godot::PtrToArg<short>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
	       (:in-class "godot::PtrToArg<short*>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
	       (:in-class "godot::PtrToArg<const short*>"
                          (:ctor)
			  ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
                          (:dtor))
	       (:in-class "godot::PtrToArg<const short&>"
                          (:ctor)
			  ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
                          (:dtor))
	       (:in-class "godot::PtrToArg<unsigned char>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
	       (:in-class "godot::PtrToArg<unsigned char*>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
	       (:in-class "godot::PtrToArg<const unsigned char*>"
                          (:ctor)
			  ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
                          (:dtor))
	       (:in-class "godot::PtrToArg<const unsigned char&>"
                          (:ctor)
			  ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
                          (:dtor))
	       (:in-class "godot::PtrToArg<unsigned int>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
	       (:in-class "godot::PtrToArg<unsigned int*>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
	       (:in-class "godot::PtrToArg<const unsigned int*>"
                          (:ctor)
			  ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
                          (:dtor))
	       (:in-class "godot::PtrToArg<const unsigned int&>"
                          (:ctor)
			  ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
                          (:dtor))
	       (:in-class "godot::PtrToArg<unsigned long>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
	       (:in-class "godot::PtrToArg<unsigned long*>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
	       (:in-class "godot::PtrToArg<const unsigned long*>"
                          (:ctor)
			  ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
                          (:dtor))
	       (:in-class "godot::PtrToArg<const unsigned long&>"
                          (:ctor)
			  ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
                          (:dtor))
	       (:in-class "godot::PtrToArg<unsigned short>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
	       (:in-class "godot::PtrToArg<unsigned short*>"
			  (:ctor)
                          ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
			  (:dtor))
	       (:in-class "godot::PtrToArg<const unsigned short*>"
                          (:ctor)
			  ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
                          (:dtor))
	       (:in-class "godot::PtrToArg<const unsigned short&>"
                          (:ctor)
			  ("convert")
                          ("convert" :any)
                          ("encode_arg" :any)
                          ("encode" :any :any)
                          (:dtor))
	       (:in-class "godot::PtrToArg<godot::PhysicsServer3D::SpaceParameter>"
			  (:ctor)
			  (:dtor))
               (:in-class "godot::PackedFloat64Array::ConstIterator"
                          (:ctor)
                          (:dtor))
               (:in-class "godot::PackedInt64Array::ConstIterator"
                          (:ctor)
                          (:dtor))
               (:in-class "godot::PackedVector2Array::ConstIterator"
                          (:ctor)
                          (:dtor))
               (:in-class "godot::PackedVector3Array::ConstIterator"
                          (:ctor)
                          (:dtor))
               (:in-class "godot::PropertyInfo"
                          (:ctor :any :any :any :any :any :any)
                          (:ctor :any)
                          ("sort")
                          ("operator<" :any)
                          ("operator==" :any)
                          (:dtor))
               (:in-class "const godot::PropertyInfo"
                          ("sort")
                          ("operator<" :any)
                          ("operator==" :any))
               (:in-class "godot::Ref"
                          (:ctor)
                          (:ctor :any)
                          ("instantiate")
                          ("is_null")
                          ("is_valid")
                          ("operator=" :any)
                          ("operator Variant")
                          ("unref")
                          ("~Ref<T>")
                          (:dtor))
               (:in-class "godot::RID"
                          (:ctor :any)
                          (:dtor))
               (:in-class "godot::String"
                          (:ctor)
                          ("operator+" :any)
                          ("operator==" :any)
                          ("operator")
                          (:dtor))
               (:in-class "godot::StringName"
                          (:ctor)
                          ("operator+" :any)
                          ("operator==" :any)
                          (:dtor))
               (:in-class "godot::TypedArray<char>"
			  (:ctor)
                          (:ctor :any)
                          ("operator=" :any)
			  (:dtor))
               (:in-class "godot::TypedArray<char*>"
			  (:ctor)
                          (:ctor :any)
                          ("operator=" :any)
			  (:dtor))
	       (:in-class "godot::TypedArray<long>"
			  (:ctor)
                          (:ctor :any)
			  ("operator=" :any)
                          (:dtor))
	       (:in-class "godot::TypedArray<long*>"
			  (:ctor)
                          (:ctor :any)
			  ("operator=" :any)
                          (:dtor))
	       (:in-class "godot::TypedArray<unsigned long>"
			  (:ctor)
                          (:ctor :any)
			  ("operator=" :any)
                          (:dtor))
	       (:in-class "godot::TypedArray<unsigned long*>"
			  (:ctor)
                          (:ctor :any)
			  ("operator=" :any)
                          (:dtor))
	       (:in-class "godot::TypedArray<godot::CompositorEffect>"
			  (:ctor)
			  (:ctor :any)
			  ("operator=" :any)
			  (:dtor))
               (:in-class "godot::Variant"
                          (:ctor :any)
                          ("callp_static" :any :any :any :any :any :any)
                          ("callp" :any :any :any :any :any)
                          ("operator AABB")
			  ("operator long")
                          ("operator RID")
                          ("operator Variant")
                          ("blend" :any :any :any :any)
                          ("interpolate" :any :any :any :any)
                          (:dtor))
	       (:in-class "godot::Vector<unsigned char>"
			  (:ctor :any :any)
                          (:ctor :any)
                          (:ctor)
                          ("ConstIterator" :any)
			  ("get" :any)
                          ("begin")
                          ("end")
                          (:dtor))
	       (:in-class "godot::Vector<unsigned char>::ConstIterator"
			  (:ctor :any :any)
                          (:ctor :any)
                          (:ctor)
                          ("ConstIterator" :any)
                          ("begin")
                          ("end")
                          (:dtor))
	       (:in-class "godot::Vector<godot::StringName>::ConstIterator"
			  (:ctor :any :any)
                          (:ctor :any)
                          (:ctor)
                          ("ConstIterator" :any)
                          ("begin")
                          ("end")
                          (:dtor))
	       (:in-class "godot::Vector<godot::StringName>"
			  (:ctor :any :any)
                          (:ctor :any)
                          (:ctor)
                          ("ConstIterator" :any)
                          ("begin")
                          ("end")
                          (:dtor))
	       (:in-class "godot::Vector::ConstIterator"
                          (:ctor :any :any)
                          (:ctor :any)
                          (:ctor)
                          ("ConstIterator" :any)
                          ("begin")
                          ("end")
                          (:dtor))
	       (:in-class "godot::Vector::Iterator"
			  (:ctor :any :any)
                          (:ctor :any)
                          (:ctor)
                          ("ConstIterator" :any)
                          ("begin")
                          ("end")
                          (:dtor))) e)))

;; (defun instantiate-some (decl)
;;   (when (and (string= "godot" (claw.resect:declaration-namespace decl))
;;              (string= "Ref" (claw.resect:declaration-name decl)))
;;     '(("World2D") ("World3D"))))

(claw:defwrapper (:aw-godot
                    (:system :clawdot/wrapper)
                    (:headers "godot_cpp/classes/animatable_body2d.hpp"
                              "godot_cpp/classes/animatable_body3d.hpp"
                              "godot_cpp/classes/animation.hpp"
                              "godot_cpp/classes/animation_library.hpp"
                              "godot_cpp/classes/animation_player.hpp"
                              "godot_cpp/classes/area2d.hpp"
                              "godot_cpp/classes/area3d.hpp"
                              "godot_cpp/classes/array_mesh.hpp"
                              "godot_cpp/classes/audio_listener2d.hpp"
                              "godot_cpp/classes/audio_listener3d.hpp"
			      "godot_cpp/classes/audio_sample.hpp"
			      "godot_cpp/classes/audio_sample_playback.hpp"
                              "godot_cpp/classes/audio_stream_ogg_vorbis.hpp"
                              "godot_cpp/classes/audio_stream_playback.hpp"
                              "godot_cpp/classes/box_shape3d.hpp"
                              "godot_cpp/classes/callback_tweener.hpp"
                              "godot_cpp/classes/camera_attributes.hpp"
                              "godot_cpp/classes/camera2d.hpp"
                              "godot_cpp/classes/camera3d.hpp"
                              "godot_cpp/classes/character_body2d.hpp"
                              "godot_cpp/classes/character_body3d.hpp"
                              "godot_cpp/classes/collision_shape2d.hpp"
                              "godot_cpp/classes/collision_shape3d.hpp"
			      "godot_cpp/classes/compositor.hpp"
                              "godot_cpp/classes/concave_polygon_shape2d.hpp"
                              "godot_cpp/classes/concave_polygon_shape3d.hpp"
                              "godot_cpp/classes/convex_polygon_shape2d.hpp"
                              "godot_cpp/classes/convex_polygon_shape3d.hpp"
                              "godot_cpp/classes/curve2d.hpp"
                              "godot_cpp/classes/curve3d.hpp"
                              "godot_cpp/classes/cylinder_shape3d.hpp"
                              "godot_cpp/classes/environment.hpp"
                              "godot_cpp/classes/font.hpp"
                              "godot_cpp/classes/image.hpp"
                              "godot_cpp/classes/input_event_action.hpp"
                              "godot_cpp/classes/input_event_key.hpp"
                              "godot_cpp/classes/input_event.hpp"
                              "godot_cpp/classes/input_event_joypad_button.hpp"
                              "godot_cpp/classes/input_event_joypad_motion.hpp"
                              "godot_cpp/classes/input_event_mouse_button.hpp"
                              "godot_cpp/classes/input_event_mouse_motion.hpp"
                              "godot_cpp/classes/input_event_mouse.hpp"
                              "godot_cpp/classes/interval_tweener.hpp"
                              "godot_cpp/classes/kinematic_collision2d.hpp"
                              "godot_cpp/classes/kinematic_collision3d.hpp"
                              "godot_cpp/classes/light2d.hpp"
                              "godot_cpp/classes/light3d.hpp"
                              "godot_cpp/classes/material.hpp"
                              "godot_cpp/classes/mesh.hpp"
                              "godot_cpp/classes/mesh_instance2d.hpp"
                              "godot_cpp/classes/mesh_instance3d.hpp"
                              "godot_cpp/classes/method_tweener.hpp"
                              "godot_cpp/classes/multi_mesh.hpp"
                              "godot_cpp/classes/multiplayer_api.hpp"
                              "godot_cpp/classes/multiplayer_peer.hpp"
                              "godot_cpp/classes/node2d.hpp"
                              "godot_cpp/classes/node3d.hpp"
                              "godot_cpp/classes/node3d_gizmo.hpp"
                              "godot_cpp/classes/ogg_packet_sequence.hpp"
                              "godot_cpp/classes/object.hpp"
                              "godot_cpp/classes/packed_scene.hpp"
                              "godot_cpp/classes/path2d.hpp"
                              "godot_cpp/classes/path3d.hpp"
                              "godot_cpp/classes/physics_material.hpp"
                              "godot_cpp/classes/physics_test_motion_parameters3d.hpp"
                              "godot_cpp/classes/property_tweener.hpp"
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
                              "godot_cpp/classes/scene_state.hpp"
                              "godot_cpp/classes/scene_tree.hpp"
                              "godot_cpp/classes/scene_tree_timer.hpp"
                              "godot_cpp/classes/skin.hpp"
			      "godot_cpp/classes/skin_reference.hpp"
                              "godot_cpp/classes/sky.hpp"
                              "godot_cpp/classes/sphere_shape3d.hpp"
                              "godot_cpp/classes/sprite2d.hpp"
                              "godot_cpp/classes/sprite3d.hpp"
                              "godot_cpp/classes/standard_material3d.hpp"
                              "godot_cpp/classes/static_body2d.hpp"
                              "godot_cpp/classes/style_box.hpp"
                              "godot_cpp/classes/triangle_mesh.hpp"
                              "godot_cpp/classes/tween.hpp"
                              "godot_cpp/classes/world2d.hpp"
                              "godot_cpp/classes/world3d.hpp"
                              "godot_cpp/variant/aabb.hpp"
                              "godot_cpp/variant/array.hpp"
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
                    ;;(:instantiate #'instantiate-some)
                    (:targets ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
                              ((:and :x86-64 :windows) "x86_64-pc-windows-gnu")
                              ((:and :aarch64 :darwin) "aarch64-pc-darwin-gnu"))
                    (:persistent :godot-bindings
                     :depends-on (:claw-utils)
                     :asd-path "godot-bindings.asd"
                     :bindings-path "bindings/")
                    (:language :c++)
                    (:standard :c++17)
                    (:include-definitions "^godot::.*")
                    (:exclude-definitions "^godot::.*::_.*" "^godot::.*Array.*begin.*" "^godot::.*Array.*end.*" ".*wchar.*" ".*Wchar.*" "^std::.*" "godot::EditorPlugins.*" "^godot::internal::.*" "^godot::GetTypeInfo.*" "^godot::ClassDB.*" ".*_MethodBindings" ".*atomic.*" ".*_gde_.*"))
    :in-package :%aw-godot
    :trim-enum-prefix t
    :recognize-bitfields t
    :recognize-strings t
    :ignore-entities (ignore-uninstantiable)
    :with-adapter (:static
                   :path "lib/adapter.cxx")
    :symbolicate-names (:in-pipeline
                        ;; (:by-removing-complex-prefix "^m[A-Z]\\w*" 1)
                        (:by-removing-prefixes "gd" "godot"))
    :override-types ((:string claw-utils:claw-string)
                     (:pointer claw-utils:claw-pointer)
                     ("char16" :uint16)
                     ("char32" :uint32)))


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
