;;; navbar-test.el --- Tests for navbar.el           -*- lexical-binding: t; -*-

;; Copyright (C) 2015  papaeye

;; Author: papaeye <papaeye@gmail.com>
;; Keywords: convenience

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

(require 'ert)
(require 'navbar)

(defmacro navbar-test-save-buffer-list (&rest body)
  (declare (indent 0) (debug t))
  `(let ((old-buffer-list (buffer-list)))
     (unwind-protect
	 (progn ,@body)
       (dolist (buffer (buffer-list))
	 (unless (memq buffer old-buffer-list)
	   (kill-buffer buffer))))))

(defmacro navbar-test-with-mode (&rest body)
  (declare (indent 0) (debug t))
  `(let ((old-mode (or navbar-mode -1)))
     (unwind-protect
	 (progn
	   (navbar-mode 1)
	   ,@body)
       (navbar-mode old-mode))))

(defmacro navbar-test-save-item-list (&rest body)
  (declare (indent 0) (debug t))
  `(let ((old-item-list (copy-tree navbar-item-list))
	 (old-item-alist (copy-tree navbar-item-alist)))
     (unwind-protect
	 (progn ,@body)
       (setq navbar-item-list old-item-list)
       (setq navbar-item-alist old-item-alist))))

(defvar navbar-test-mode-on-hook)
(defvar navbar-test-mode-off-hook)

(define-minor-mode navbar-test-mode nil
  :group 'navbar
  :global t)

(defvar navbar-test--t t)
(defvar navbar-test--nil nil)

(defun navbar-test--func1 ()
  (put 'navbar-test--func1 'called t))
(defun navbar-test--func2 ())

(defvar navbar-test--item '(:key t :cache "foo"))
(defvar navbar-test--mode-item (list :key 'navbar-test-mode
				     :on 'navbar-test--func1
				     :off 'navbar-test--func2))

;;; Features

;;;; navbar item API

(ert-deftest navbar-define-item:test ()
  (unwind-protect
      (progn
	(navbar-define-item navbarx-foo foo
	  nil :cache "foo")
	(should (boundp 'navbarx-foo))
	(should (equal navbarx-foo (list :key 'foo :cache "foo"))))
    (makunbound 'navbarx-foo)))

(ert-deftest navbar-define-mode-item:test ()
  (unwind-protect
      (progn
	(navbar-define-mode-item navbarx-foo foo
	  nil :mode-on 'func1 :mode-off 'func2 :get 'func3)
	(should (boundp 'navbarx-foo))
	(should (equal navbarx-foo
		       (list :key 'foo-mode
			     :on 'func1
			     :off 'func2
			     :get 'func3))))
    (makunbound 'navbarx-foo)))

;;;; `navbar-item-cache-put'

(ert-deftest navbar-item-cache-put:new-value ()
  (navbar-test-save-item-list
    (setq navbar-item-alist '((foo :cache "foo")))
    (should (navbar-item-cache-put 'foo "bar"))))

(ert-deftest navbar-item-cache-put:unchanged ()
  (navbar-test-save-item-list
    (setq navbar-item-alist '((foo :cache "foo")))
    (should-not (navbar-item-cache-put 'foo "foo"))))

(ert-deftest navbar-item-cache-put:nil ()
  (navbar-test-save-item-list
    (setq navbar-item-alist '((foo :cache "foo")))
    (should (navbar-item-cache-put 'foo nil))))

;;;; `navbar-serialize'

(ert-deftest navbar-serialize:string-cache ()
  (navbar-test-save-item-list
    (setq navbar-item-list '((:key t :cache "foo")))
    (navbar-initialize)
    (should (string= (navbar-serialize) "foo"))))

(ert-deftest navbar-serialize:list-cache ()
  (navbar-test-save-item-list
    (setq navbar-item-list '((:key t :cache ("foo" "bar"))))
    (navbar-initialize)
    (should (string= (navbar-serialize) "foo bar"))))

(ert-deftest navbar-serialize:ignore-nil-key ()
  (navbar-test-save-item-list
    (setq navbar-item-list '((:key navbar-test--t :cache "foo")
			     (:key navbar-test--nil :cache "bar")))
    (navbar-initialize)
    (should (string= (navbar-serialize) "foo"))))

(ert-deftest navbar-serialize:nest ()
  (navbar-test-save-item-list
    (setq navbar-item-list '((:key t :cache (("foo" "bar")))
			     (:key t :cache "baz")))
    (navbar-initialize)
    (should (string= (navbar-serialize) "foo bar baz"))))

;;;; `navbar-update'

(ert-deftest navbar-update:nil-key ()
  (navbar-test-save-item-list
    (navbar-test-save-buffer-list
      (setq navbar-item-list
	    '((:key foo :cache "foo")
	      (:key bar :cache "bar" :get (lambda ()
					    (navbar-item-cache-put
					     'bar "baz")))))
      (setq navbar-display-function 'ignore)
      (navbar-initialize)
      (save-window-excursion
	(navbar-make-window)
	(navbar-update (selected-frame))
	(should (string= (navbar-item-cache-get 'foo)
			 "foo"))
	(should (string= (navbar-item-cache-get 'bar)
			 "baz"))))))

;;;; `navbar-initialize'

(ert-deftest navbar-initialize:simple ()
  (navbar-test-save-item-list
    (setq navbar-item-list (list navbar-test--item))
    (navbar-initialize)
    ;; Initialize `navbar-item-alist'
    (should (equal navbar-item-alist `((t ,@navbar-test--item))))))

(ert-deftest navbar-initialize:symbol-item ()
  (navbar-test-save-item-list
    (setq navbar-item-list '(navbar-test--item))
    (navbar-initialize)
    (should (equal navbar-item-alist `((t ,@navbar-test--item))))))

(ert-deftest navbar-initialize:autoload ()
  (navbar-test-save-item-list
    (setq navbar-item-list '(navbarx-version))
    (navbar-initialize)
    (should (boundp 'navbarx-version))))

(ert-deftest navbar-initialize:order ()
  (navbar-test-save-item-list
    (let ((item1 '(:key t :cache "foo"))
	  (item2 '(:key bar :cache "bar")))
      (setq navbar-item-list (list item1 item2))
      (navbar-initialize)
      (should (eq (car (nth 0 navbar-item-alist)) 't))
      (should (eq (car (nth 1 navbar-item-alist)) 'bar)))))

(ert-deftest navbar-initialize:mode ()
  (navbar-test-save-item-list
    (setq navbar-item-list (list navbar-test--mode-item))
    (unwind-protect
	(progn
	  (navbar-initialize)
	  ;; Initialize `navbar-item-alist'
	  (should (equal navbar-item-alist
			 `((navbar-test-mode ,@navbar-test--mode-item))))
	  ;; Setup mode's on/off hooks
	  (should (memq (plist-get navbar-test--mode-item :on)
			navbar-test-mode-on-hook))
	  (should (memq (plist-get navbar-test--mode-item :off)
			navbar-test-mode-off-hook))
	  ;; Don't call `:on' function
	  (should-not (get (plist-get navbar-test--mode-item :on) 'called)))
      (setq navbar-test-mode-on-hook nil)
      (setq navbar-test-mode-off-hook nil)
      (put (plist-get navbar-test--mode-item :on) 'called nil))))

(ert-deftest navbar-initialize:call-on-func ()
  (navbar-test-save-item-list
    (setq navbar-item-list
	  (list (list :key 'navbar-test-mode
		      :on (lambda () (navbar-item-cache-put
				      'navbar-test-mode "foo")))))
    (unwind-protect
	(progn
	  (navbar-test-mode 1)
	  (navbar-initialize)
	  (should (string= (navbar-item-cache-get 'navbar-test-mode)
			   "foo")))
      (navbar-test-mode -1)
      (setq navbar-test-mode-on-hook nil)
      (setq navbar-test-mode-off-hook nil))))

(ert-deftest navbar-initialize:deinitialize ()
  (navbar-test-save-item-list
    (setq navbar-item-list (list navbar-test--mode-item))
    (navbar-initialize)
    (unwind-protect
	(progn
	  (setq navbar-item-list (list navbar-test--item))
	  (navbar-initialize)
	  (should (equal navbar-item-alist `((t ,@navbar-test--item))))
	  (should-not (memq (plist-get navbar-test--mode-item :on)
			    navbar-test-mode-on-hook))
	  (should-not (memq (plist-get navbar-test--mode-item :off)
			    navbar-test-mode-off-hook)))
      (setq navbar-test-mode-on-hook nil)
      (setq navbar-test-mode-off-hook nil))))

(ert-deftest navbar-deinitialize:test ()
  (navbar-test-save-item-list
    (setq navbar-item-list (list navbar-test--mode-item))
    (unwind-protect
	(progn
	  (navbar-initialize)
	  (navbar-deinitialize)
	  (should-not navbar-item-alist)
	  (should-not (memq (plist-get navbar-test--mode-item :on)
			    navbar-test-mode-on-hook))
	  (should-not (memq (plist-get navbar-test--mode-item :off)
			    navbar-test-mode-off-hook)))
      (setq navbar-test-mode-on-hook nil)
      (setq navbar-test-mode-off-hook nil))))

;;; GUI

(unless noninteractive
  (ert-deftest navbar-buffer-name:unique ()
    (let ((old-frame (selected-frame))
	  (new-frame (make-frame)))
      (unwind-protect
	  (should-not (string= (navbar-buffer-name old-frame)
			       (navbar-buffer-name new-frame)))
	(delete-frame new-frame)))))

(ert-deftest navbar-buffer-create:create ()
  (navbar-test-save-buffer-list
    (let ((buffer (navbar-buffer-create)))
      (should (bufferp buffer))
      (with-current-buffer buffer
	(should (string-prefix-p " *navbar " (buffer-name)))
	(should-not mode-line-format)
	(should-not cursor-type)
	(should truncate-lines)
	(should (eq (car (current-active-maps)) navbar-base-map))))))

(ert-deftest navbar-buffer-create:existing-buffer ()
  (navbar-test-save-buffer-list
    (should (eq (navbar-buffer-create) (navbar-buffer-create)))))

(ert-deftest navbar-make-window:create ()
  (navbar-test-save-buffer-list
    (save-window-excursion
      (let ((window (navbar-make-window)))
	(should (windowp window))
	(should (= (window-total-height window) 1))
	(should (= (car (window-fringes window)) 0))
	(should (= (window-parameter window 'window-slot) 0))
	(should (eq (window-parameter window 'window-side) 'top))
	(should (eq (window-parameter window 'delete-window) 'ignore))
	(should (eq (window-parameter window 'no-other-window) t))
	(should (eq (window-parameter window 'navbar-window) t))))))

(ert-deftest navbar-make-window:existing-window ()
  (navbar-test-save-buffer-list
    (save-window-excursion
      (should (eq (navbar-make-window) (navbar-make-window))))))

(ert-deftest navbar-kill-buffer-and-window:test ()
  (navbar-test-save-buffer-list
    (save-window-excursion
      (let ((window (navbar-make-window)))
	(navbar-kill-buffer-and-window)
	(should-not (navbar-buffer))
	(should-not (navbar-window))
	(should-not (window-valid-p window))))))

(ert-deftest navbar-mode:hooks ()
  (navbar-test-with-mode
    (should (memq 'navbar-update after-make-frame-functions))
    (should (memq 'navbar-make-window after-make-frame-functions)))
  (should-not (memq 'navbar-update after-make-frame-functions))
  (should-not (memq 'navbar-make-window after-make-frame-functions)))

(unless noninteractive
  (ert-deftest navbar-mode:multiple-frames ()
    (let ((frame1 (selected-frame))
	  (frame2 (make-frame)))
      (unwind-protect
	  (let (window1 window2)
	    (navbar-test-with-mode
	      (should (setq window1 (navbar-window frame1)))
	      (should (setq window2 (navbar-window frame2))))
	    (should-not (window-valid-p window1))
	    (should-not (window-valid-p window2)))
	(delete-frame frame2)))))

(unless noninteractive
  (ert-deftest navbar-mode:make-frame ()
    (navbar-test-with-mode
      (let ((new-frame (make-frame)))
	(unwind-protect
	    (should (window-live-p (navbar-window new-frame)))
	  (delete-frame new-frame))))))

;;; Mode

(ert-deftest navbar-mode:advices ()
  (navbar-test-with-mode
    (should (and (ad-find-advice 'next-window 'around 'navbar-ignore)))
    (should (and (ad-find-advice 'window-list 'around 'navbar-ignore))))
  (should-not (and (ad-find-advice 'next-window 'around 'navbar-ignore)))
  (should-not (and (ad-find-advice 'window-list 'around 'navbar-ignore))))

(ert-deftest navbar-mode:initialize-after-setup ()
  (navbar-test-save-item-list
    ;; Call `:on' function in `navbar-initialize'.
    (navbar-test-mode 1)
    (setq navbar-item-list
	  (list (list :key 'navbar-test-mode
		      :on (lambda () (navbar-update nil 'navbar-test-mode)))))
    (navbar-test-with-mode)))

(provide 'navbar-test)
;;; navbar-test.el ends here
