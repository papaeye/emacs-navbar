;;; navbar.el --- Navigation bar for Emacs           -*- lexical-binding: t; -*-

;; Copyright (C) 2015  papaeye

;; Author: papaeye <papaeye@gmail.com>
;; Keywords: convenience
;; Version: 0.1.0
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

;;

;;; Code:

(defconst navbar-version "0.1.0")

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

(defcustom navbar-display-function #'navbar-display
  "Function to convert `navbar-item-alist' to a string and display it
in a buffer."
  :type 'function
  :group 'navbar)

;;; Features

(defvar navbar-item-alist nil)

(defun navbar-item-cache-put (key new-value)
  "Put KEY's `:cache' value to NEW-VALUE.
Return non-`nil', if NEW-VALUE is not same as existing value."
  (let* ((item (cdr (assq key navbar-item-alist)))
	 (old-value (plist-get item :cache)))
    (unless (equal new-value old-value)
      (plist-put item :cache new-value))))

(defun navbar-display (buffer)
  "Convert `navbar-item-alist' to a string and display it in BUFFER."
  (with-current-buffer buffer
    (let (deactivate-mark)
      (erase-buffer)
      (insert (mapconcat (lambda (pair)
			   (when (symbol-value (car pair))
			     (let ((value (plist-get (cdr pair) :cache)))
			       (if (listp value)
				   (apply 'concat value)
				 value))))
			 navbar-item-alist
			 nil)))))

(defun navbar-update (frame &optional key)
  "Update navbar of FRAME by updating KEY's item.
If KEY is `nil', all items are updated by their `:get' functions."
  (unless key
    (dolist (item (mapcar 'cdr navbar-item-alist))
      (when (plist-get item :get)
	(funcall (plist-get item :get)))))
  (funcall navbar-display-function (navbar-buffer frame)))

(defmacro navbar--mode-on-hook (key)
  `(intern (concat (symbol-name ,key) "-on-hook")))

(defmacro navbar--mode-off-hook (key)
  `(intern (concat (symbol-name ,key) "-off-hook")))

(defun navbar-initialize ()
  "Initialize `navbar-item-alist' and mode's on/off hooks."
  (navbar-deinitialize)
  (setq navbar-item-alist
	(mapcar (lambda (item)
		  (when (symbolp item)
		    (setq item (symbol-value item)))
		  (let ((key (plist-get item :key))
			(value (copy-sequence item))
			(func-on (plist-get item :on))
			(func-off (plist-get item :off)))
		    (when func-on
		      (add-hook (navbar--mode-on-hook key) func-on)
		      (when (symbol-value key)
			(funcall func-on)))
		    (when func-off
		      (add-hook (navbar--mode-off-hook key) func-off))
		    (cons key value)))
		navbar-item-list)))

(defun navbar-deinitialize ()
  "Clean up `navbar-item-alist' and mode's on/off hooks."
  (dolist (item (mapcar 'cdr navbar-item-alist))
    (let ((key (plist-get item :key))
	  (func-on (plist-get item :on))
	  (func-off (plist-get item :off)))
      (when func-on
	(remove-hook (navbar--mode-on-hook key) func-on))
      (when func-off
	(remove-hook (navbar--mode-off-hook key) func-off))))
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
    (ad-remove-advice func 'around 'navbar-ignore)
    (ad-update func)))

;;; Minor mode

(defun navbar-setup ()
  (navbar-advices-setup)
  (add-hook 'after-make-frame-functions #'navbar-update)
  (add-hook 'after-make-frame-functions #'navbar-make-window)
  (dolist (frame (frame-list))
    (navbar-make-window frame)
    (navbar-update frame)))

(defun navbar-teardown ()
  (navbar-advices-teardown)
  (remove-hook 'after-make-frame-functions #'navbar-update)
  (remove-hook 'after-make-frame-functions #'navbar-make-window)
  (mapc 'navbar-kill-buffer-and-window (frame-list)))

;;;###autoload
(define-minor-mode navbar-mode nil
  :group 'navbar
  :global t
  (if navbar-mode
      (progn
	(navbar-initialize)
	(navbar-setup))
    (navbar-deinitialize)
    (navbar-teardown)))

(provide 'navbar)
;;; navbar.el ends here
