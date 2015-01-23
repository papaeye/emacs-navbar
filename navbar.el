;;; navbar.el --- Navigation bar for Emacs           -*- lexical-binding: t; -*-

;; Copyright (C) 2015  papaeye

;; Author: papaeye <papaeye@gmail.com>
;; Keywords: convenience
;; Version: 0.1.0
;; Homepage: https://github.com/papaeye/emacs-navbar
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; navbar.el is a navigation bar for Emacs.
;; Just like the navbar component of Bootstrap <http://getbootstrap.com/>,
;; navbar.el can contain various components called navbar items.
;;
;; navbar.el requires Emacs 24.3 or later.

;;; Code:

(defconst navbar-version "0.1.0")

(eval-when-compile (require 'cl-lib))

;;; Customization

(defgroup navbar nil
  "Navigation bar for Emacs"
  :group 'environment)

(defcustom navbar-item-list nil
  "List of navbar items.
It is necessary to run `navbar-initialize' to reflect the change of
`navbar-item-list'."
  ;; TODO: :type
  :group 'navbar)

(defcustom navbar-item-separator
  (propertize " " 'display '(space :width 0.2))
  "String to separate navbar items."
  :type 'string
  :group 'navbar)

(defcustom navbar-serialize-function #'navbar-serialize
  "Function to convert `navbar-item-alist' to a string."
  :type 'function
  :group 'navbar)

(defcustom navbar-display-function #'navbar-display
  "Function to display serialized `navbar-item-alist' in a buffer."
  :type 'function
  :group 'navbar)

;;; Utilities

(defconst navbar-font-lock-keywords
  '(("(\\(navbar-define-item\\)\\>\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))))

(defun navbar--flatten (l)
  (cond
   ((null l) nil)
   ((atom l) (list l))
   (t (append (navbar--flatten (car l)) (navbar--flatten (cdr l))))))

;;; Features

(defvar navbar-item-alist nil)

(defmacro navbar-define-item (item doc &rest args)
  "Define a navbar item ITEM.
A navbar item is a plain property list.
This macro defines a variable ITEM whose value is the property list.
If `:get' property described below is supplied in ARGS, this macro also
defines a function `ITEM-update' which updates the navbar buffer cleverly.

DOC is a doc string for variable ITEM.

:enable
	VALUE should be a symbol of a variable.
	If the symbol value is `nil', the navbar item is not displayed.
:get	VALUE should be a function which returns the current value of
	the navbar item.
	It should return symbol `unchanged' if the value is not updated.
:initialize
	VALUE should be a function which is run by `navbar-initialize'
	if ENABLE is non-`nil' at that time.
:deinitialize
	VALUE should be a function which is run by `navbar-deinitialize'.
:hooks	VALUE should be a list of cons:
	CAR is a symbol of a hook and CDR is a function.
	Each function is added to the corresponding hook by
	`navbar-initialize' and removed from it by `navbar-deinitialize'.
:mode	VALUE should be a symbol of a mode.
:mode-on
	VALUE should be a function added to MODE-on-hook.
	It is removed from the hook by `navbar-deinitialize'.
	This is also run by `navbar-initialize'
	if the mode is enabled at that time.
:mode-off
	VALUE should be a function added to MODE-off-hook.
	It is removed from the hook by `navbar-deinitialize'.
	This is also run by `navbar-deinitialize'."
  (declare (indent defun) (doc-string 2))
  (let ((key `(quote ,item))
	(item-update (intern (concat (symbol-name item) "-update")))
	(enable t)
	getter
	get
	initialize
	deinitialize
	hooks
	;; for mode
	mode
	mode-on-hook
	mode-on-func
	mode-off-hook
	mode-off-func
	mode-hooks
	;; for extra keywords
	extra-keywords
	keyword)

    (while (keywordp (setq keyword (car args)))
      (setq args (cdr args))
      (pcase keyword
	(`:enable (setq enable `(quote ,(pop args))))
	(`:get (setq getter (pop args)))
	(`:initialize (setq initialize (list :initialize (pop args))))
	(`:deinitialize (setq deinitialize (list :deinitialize (pop args))))
	(`:hooks (setq hooks (list :hooks (pop args))))
	(`:mode (setq mode (pop args)))
	(`:mode-on (setq mode-on-func (pop args)))
	(`:mode-off (setq mode-off-func (pop args)))
	(_ (push keyword extra-keywords)
	   (push (pop args) extra-keywords))))

    (when getter
      (setq get (list :get getter)))

    (when mode
      (setq enable `(quote ,mode))
      (setq mode-on-hook (intern (concat (symbol-name mode) "-on-hook")))
      (setq mode-off-hook (intern (concat (symbol-name mode) "-off-hook")))
      (when mode-on-func
	(setq initialize (list :initialize mode-on-func))
	(push (list 'cons `(quote ,mode-on-hook) mode-on-func) mode-hooks))
      (when mode-off-func
	(setq deinitialize (list :deinitialize mode-off-func))
	(push (list 'cons `(quote ,mode-off-hook) mode-off-func) mode-hooks))
      (when mode-hooks
	(setq hooks (list :hooks `(list ,@(nreverse mode-hooks))))))

    `(progn
       ,(when getter
	  `(defun ,item-update ()
	     (when (navbar-item-update ,key)
	       (navbar-update nil ,key))))
       (defvar ,item (list :key ,key :enable ,enable
			   ,@get
			   ,@initialize
			   ,@deinitialize
			   ,@hooks
			   ,@(nreverse extra-keywords))
	 ,doc))))

(defun navbar-item-value-get (key)
  "Return KEY's `:value' property value."
  (plist-get (cdr (assq key navbar-item-alist))
	     :value))

(defun navbar-item-enabled-p (key)
  "Return non-`nil' if KEY's item is enabled."
  (navbar--item-enabled-p
   (cdr (assq key navbar-item-alist))))

(defun navbar--item-enabled-p (item)
  (or (not (plist-member item :enable))
      (symbol-value (plist-get item :enable))))

(defun navbar-item-update (key &optional force)
  "Update KEY's value by running the value of :get property if available.
If optional FORCE argument is non-nil, it is passed to :get function.

Return non-nil if the item has :get property and the return value of
the :get function is neither symbol `unchanged' nor existing value."
  (let* ((item (cdr (assq key navbar-item-alist)))
	 (getter (plist-get item :get))
	 old-value new-value)
    (when getter
      (setq old-value (plist-get item :value))
      (setq new-value (and (navbar--item-enabled-p item)
			   (if force
			       (funcall getter force)
			     (funcall getter))))
      (unless (or (eq new-value 'unchanged)
		  (equal new-value old-value))
	(plist-put item :value new-value)))))

(defun navbar-serialize ()
  "Convert `navbar-item-alist' to a string."
  (mapconcat 'identity
	     (navbar--flatten
	      (cl-loop for item in (mapcar 'cdr navbar-item-alist)
		       when (navbar--item-enabled-p item)
		       collect (plist-get item :value)))
	     navbar-item-separator))

(defun navbar-display (buffer)
  "Display serialized `navbar-item-alist' in a BUFFER."
  (with-current-buffer buffer
    (let (deactivate-mark)
      (erase-buffer)
      (insert (funcall navbar-serialize-function)))))

(defun navbar-update (&optional frame _key)
  "Update navbar of FRAME."
  (funcall navbar-display-function (navbar-buffer frame)))

(defun navbar--funcall-with-no-display (function &rest arguments)
  (let ((navbar-display-function #'ignore))
    (apply #'funcall function arguments)))

(defun navbar-initialize ()
  "Initialize `navbar-item-alist' and add functions to hooks,
Also, this runs :initialize functions without updating the navbar buffer."
  (navbar-deinitialize)
  (setq navbar-item-alist nil)
  (dolist (item (nreverse navbar-item-list))
    (when (symbolp item)
      (unless (boundp item)
	(require item nil t))
      (setq item (symbol-value item)))
    (when (stringp item)
      (setq item (list :key t :value item)))
    (let ((key (plist-get item :key))
	  (value (copy-tree item))
	  (func (plist-get item :initialize))
	  (hooks (plist-get item :hooks)))
      (push (cons key value) navbar-item-alist)
      (dolist (hook hooks)
	(add-hook (car hook) (cdr hook)))
      (when (and func (navbar--item-enabled-p value))
	(navbar--funcall-with-no-display func)))))

(defun navbar-deinitialize ()
  "Remove functions from hooks and clean up `navbar-item-alist'.
Also, this runs :deinitialize functions without updating the navbar buffer."
  (dolist (item (mapcar 'cdr navbar-item-alist))
    (let ((hooks (plist-get item :hooks))
	  (func (plist-get item :deinitialize)))
      (dolist (hook hooks)
	(remove-hook (car hook) (cdr hook)))
      (when func
	(navbar--funcall-with-no-display func))))
  (setq navbar-item-alist nil))

;;; GUI

(defun navbar-property-at (point prop window)
  (with-selected-window window
    (get-text-property point prop)))

(defvar navbar-base-map
  (let ((map (make-sparse-keymap)))
    (dolist (mouse '("mouse-1" "mouse-2" "mouse-3"))
      (dolist (kind '("" "drag-" "down-"))
	(dolist (repeat '("" "double-" "triple-"))
	  (dolist (modifier '("" "A-" "C-" "H-" "M-" "S-" "s-"))
	    (define-key map
	      (kbd (concat modifier "<" repeat kind mouse ">"))
	      'ignore)))))
    map)
  "Keymap ignoring all mouse events.")

(defun navbar-buffer-name (&optional frame)
  (let ((string (prin1-to-string (or frame (selected-frame)))))
    ;; Match the frame address in core
    (string-match " \\(0x[^ ]+\\)>\\'" string)
    (concat " *navbar " (match-string 1 string) "*")))

(defun navbar-buffer (&optional frame)
  (get-buffer (navbar-buffer-name frame)))

(defun navbar-buffer-create (&optional frame)
  (let* ((name (navbar-buffer-name frame))
	 (buffer (get-buffer name)))
    (unless buffer
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
	(setq mode-line-format nil)
	(setq cursor-type nil)
	(setq truncate-lines t)
	(setq window-size-fixed 'height)
	(use-local-map navbar-base-map)))
    buffer))

(defun navbar-make-window (&optional frame)
  (unless frame
    (setq frame (selected-frame)))
  (with-selected-frame frame
    (let* ((buffer (navbar-buffer-create frame))
	   (window (display-buffer-in-side-window
		    buffer '((side . top) (window-height . 1)))))
      (set-window-fringes window 0)
      (set-window-parameter window 'delete-window 'ignore)
      (set-window-parameter window 'no-other-window t)
      (set-window-parameter window 'navbar-window t)
      window)))

(defun navbar-window (&optional frame)
  (get-buffer-window (navbar-buffer-name frame) frame))

(defun navbar-kill-buffer-and-window (&optional frame)
  (unless frame
    (setq frame (selected-frame)))
  (let ((window (navbar-window frame))
	(buffer (navbar-buffer frame)))
    (when window
      (delete-side-window window))
    (when buffer
      (kill-buffer buffer))))

;;; Advices

(defun navbar-advices-setup ()
  (defadvice next-window (around navbar-ignore activate)
    (let ((first-window (or (ad-get-arg 0) (selected-window))))
      ad-do-it
      (while (and (not (eq ad-return-value first-window))
		  (window-parameter ad-return-value 'navbar-window))
	(ad-set-arg 0 ad-return-value)
	ad-do-it))
    ad-return-value)

  (defadvice window-list (around navbar-ignore activate)
    ad-do-it
    (setq ad-return-value
	  (cl-loop for window in ad-return-value
		   unless (window-parameter window 'navbar-window)
		   collect window))
    ad-return-value))

(defun navbar-advices-teardown ()
  (dolist (func '(next-window window-list))
    ;; Ignore already removed advices
    (ignore-errors (ad-remove-advice func 'around 'navbar-ignore))
    (ad-update func)))

;;; Minor mode

(defun navbar-setup ()
  (navbar-advices-setup)
  (add-hook 'after-make-frame-functions #'navbar-update)
  (add-hook 'after-make-frame-functions #'navbar-make-window)
  (mapc #'navbar-make-window (frame-list))
  (navbar-initialize)
  (mapc #'navbar-update (frame-list))
  (font-lock-add-keywords 'emacs-lisp-mode navbar-font-lock-keywords))

(defun navbar-teardown ()
  (navbar-deinitialize)
  (navbar-advices-teardown)
  (remove-hook 'after-make-frame-functions #'navbar-update)
  (remove-hook 'after-make-frame-functions #'navbar-make-window)
  (mapc 'navbar-kill-buffer-and-window (frame-list))
  (font-lock-remove-keywords 'emacs-lisp-mode navbar-font-lock-keywords))

;;;###autoload
(define-minor-mode navbar-mode nil
  :group 'navbar
  :global t
  (if navbar-mode
      (navbar-setup)
    (navbar-teardown)))

(provide 'navbar)
;;; navbar.el ends here
