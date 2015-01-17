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
;; navbar.el can contain several components called navbar items.
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
  '(("(\\(navbar-define\\(?:\\sw\\|\\s_\\)*-item\\)\\>\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))))

(defun navbar--flatten (l)
  (cond
   ((null l) nil)
   ((atom l) (list l))
   (t (append (navbar--flatten (car l)) (navbar--flatten (cdr l))))))

;;; Features

(defvar navbar-item-alist nil)

(defmacro navbar-define-item (item enable doc &rest body)
  "Define a navbar item ITEM.
ENABLE is a symbol of a variable.
If the symbol value is `nil', the navbar item is not displayed.
DOC is a doc string for variable ITEM.

:get	VALUE should be a function which has two responsibilities:
	It should store ITEM's value to `:cache' property.
	It should return non-`nil' if the value is updated.
	Otherwise it should return `nil'.
	`navbar-item-cache-put' is a helper function for this.
:initialize
	VALUE should be a function which is run by `navbar-initialize'
	if ENABLE is non-`nil' at that time.
:hooks	VALUE should be a list of cons:
	CAR is a symbol is a hook and CDR is a function.
	These are registered and unregistered by
	`navbar-initialize' and `navbar-deinitialize' respectively."
  (declare (indent 0) (doc-string 3))
  (let ((key `(quote ,item))
	(cache-put (intern (concat (symbol-name item) "-cache-put")))
	(item-update (intern (concat (symbol-name item) "-update")))
	(getter (plist-get body :get)))
    `(progn
       (defun ,cache-put (value)
	 (navbar-item-cache-put ,key value))
       ,(when getter
	  `(defun ,item-update ()
	     (when (if (symbol-value ,enable)
		       (funcall ,getter)
		     (navbar-item-cache-put ,key nil))
	       (navbar-update nil ,key))))
       (defvar ,item (list :key ,key :enable ,enable ,@body)
	 ,doc))))

(defmacro navbar-define-string-item (item string doc &rest body)
  "Define a navbar item ITEM for string STRING.
DOC is a doc string for variable ITEM.

:enable
	Same as ENABLE in `navbar-define-item'."
  (declare (indent 0) (doc-string 3))
  (let ((enable t)
	extra-keywords
	keyword)
    (while (keywordp (setq keyword (car body)))
      (setq body (cdr body))
      (pcase keyword
	(`:enable (setq enable (pop body)))
	(_ (push keyword extra-keywords)
	   (push (pop body) extra-keywords))))
    `(navbar-define-item
       ,item ,enable ,doc :cache ,string ,@(nreverse extra-keywords))))

(defmacro navbar-define-mode-item (item feature getter doc &rest body)
  "Define a navbar item ITEM for feature FEATURE.
GETTER is same as `:get' in `navbar-define-item'.
DOC is a doc string for variable ITEM.

:mode-on
	VALUE should be a function added to FEATURE-mode-on-hook.
	This is also run by `navbar-initialize'
	if the mode is enabled at that time.
:mode-off
	VALUE should be a function added to FEATURE-mode-off-hook.

These functions added to hooks are removed by `navbar-deinitialize'."
  (declare (indent 0) (doc-string 4))
  (let* ((mode-name (concat (symbol-name feature) "-mode"))
	 (mode (intern mode-name))
	 (hook-on (intern (concat mode-name "-on-hook")))
	 (hook-off (intern (concat mode-name "-off-hook")))
	 func-on
	 func-off
	 hooks
	 extra-keywords
	 keyword)
    (while (keywordp (setq keyword (car body)))
      (setq body (cdr body))
      (pcase keyword
	(`:mode-on (setq func-on (pop body))
		   (push (list 'cons `(quote ,hook-on) func-on) hooks))
	(`:mode-off (setq func-off (pop body))
		    (push (list 'cons `(quote ,hook-off) func-off) hooks))
	(_ (push keyword extra-keywords)
	   (push (pop body) extra-keywords))))
    `(navbar-define-item
       ,item (quote ,mode) ,doc
       :get ,getter :initialize ,func-on
       :hooks (list ,@(nreverse hooks))
       ,@(nreverse extra-keywords))))

(defun navbar-item-cache-put (key new-value)
  "Put KEY's `:cache' value to NEW-VALUE.
Return non-`nil', if NEW-VALUE is not same as existing value."
  (let* ((item (cdr (assq key navbar-item-alist)))
	 (old-value (plist-get item :cache)))
    (unless (equal new-value old-value)
      (plist-put item :cache new-value))))

(defun navbar-item-cache-get (key)
  "Return KEY's `:cache' value."
  (plist-get (cdr (assq key navbar-item-alist))
	     :cache))

(defun navbar--item-enabled-p (item)
  ;; TODO: Make this function consistent with above functions
  (or (not (plist-member item :enable))
      (symbol-value (plist-get item :enable))))

(defun navbar-serialize ()
  "Convert `navbar-item-alist' to a string."
  (mapconcat 'identity
	     (navbar--flatten
	      (cl-loop for item in (mapcar 'cdr navbar-item-alist)
		       when (navbar--item-enabled-p item)
		       collect (plist-get item :cache)))
	     navbar-item-separator))

(defun navbar-display (buffer)
  "Display serialized `navbar-item-alist' in a BUFFER."
  (with-current-buffer buffer
    (let (deactivate-mark)
      (erase-buffer)
      (insert (navbar-serialize)))))

(defun navbar-update (frame &optional key)
  "Update navbar of FRAME.
If KEY is `nil', all items are updated by their `:get' functions."
  (unless key
    (dolist (item (mapcar 'cdr navbar-item-alist))
      (when (plist-get item :get)
	(funcall (plist-get item :get)))))
  (funcall navbar-display-function (navbar-buffer frame)))

(defun navbar-initialize ()
  "Initialize `navbar-item-alist' and add functions to hooks."
  (navbar-deinitialize)
  (setq navbar-item-alist nil)
  (dolist (item (nreverse navbar-item-list))
    (when (symbolp item)
      (unless (boundp item)
	(require item nil t))
      (setq item (symbol-value item)))
    (when (stringp item)
      (setq item (list :key t :cache item)))
    (let ((key (plist-get item :key))
	  (value (copy-tree item))
	  (func-init (plist-get item :initialize))
	  (hooks (plist-get item :hooks)))
      (push (cons key value) navbar-item-alist)
      (dolist (hook hooks)
	(add-hook (car hook) (cdr hook)))
      (when (and func-init (navbar--item-enabled-p value))
	(funcall func-init)))))

(defun navbar-deinitialize ()
  "Remove functions from hooks and clean up `navbar-item-alist'."
  (dolist (item (mapcar 'cdr navbar-item-alist))
    (let ((hooks (plist-get item :hooks)))
      (dolist (hook hooks)
	(remove-hook (car hook) (cdr hook)))))
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
  ;; TODO: Make `navbar-initialize' aware of frames.
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
