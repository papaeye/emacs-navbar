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

(require 'advice)
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

(defmacro navbar-test-with-temp-item-definition (item &rest body)
  (declare (indent 1) (debug t))
  (let ((cache-put (intern (concat (symbol-name item) "-cache-put")))
	(item-update (intern (concat (symbol-name item) "-update"))))
    `(unwind-protect
	 (progn
	   (setq navbar-display-function (lambda (_buffer) "displayed"))
	   ,@body)
       (setq navbar-display-function #'navbar-display)
       (makunbound (quote ,item))
       (fmakunbound (quote ,cache-put))
       (fmakunbound (quote ,item-update)))))

(defvar navbar-test-mode-on-hook)
(defvar navbar-test-mode-off-hook)

(define-minor-mode navbar-test-mode nil
  :group 'navbar
  :global t)

(defun navbar-test--mode-on-func ()
  (put 'navbar-test--mode-on-func 'called t))
(defun navbar-test--mode-off-func ())

(defvar navbar-test--mode-hooks
  (list (cons 'navbar-test-mode-on-hook 'navbar-test--mode-on-func)
	(cons 'navbar-test-mode-off-hook 'navbar-test--mode-off-func)))

(navbar-define-string-item
  navbar-test--item "foo"
  "Navbar string item for testing.")

(navbar-define-mode-item
  navbar-test--mode-item navbar-test #'ignore
  "Navbar mode item for testing."
  :mode-on 'navbar-test--mode-on-func
  :mode-off 'navbar-test--mode-off-func)

;;; Features

;;;; navbar item API

(ert-deftest navbar-define-item/test ()
  (navbar-test-with-temp-item-definition navbarx-foo
    (navbar-define-item
      navbarx-foo 'navbar-version nil
      :cache "foo")
    (should (equal navbarx-foo (list :key 'navbar-version :cache "foo")))
    (should (fboundp 'navbarx-foo-cache-put))
    (should-not (fboundp 'navbarx-foo-update))))

(ert-deftest navbar-define-item/should-define-update-if-get-available ()
  (navbar-test-with-temp-item-definition navbarx-foo
    (navbar-define-item
      navbarx-foo 'navbar-version nil
      :get 'ignore)
    (should (equal navbarx-foo (list :key 'navbar-version :get 'ignore)))
    (should (fboundp 'navbarx-foo-cache-put))
    (should (fboundp 'navbarx-foo-update))))

(ert-deftest navbar-define-item/item-update/t--new-value--displayed ()
  (navbar-test-with-temp-item-definition navbarx-foo
    (navbar-test-save-item-list
      (navbar-define-item
	navbarx-foo t nil
	:get (lambda () "new-value"))
      (should (string= (navbarx-foo-update) "displayed")))))

(ert-deftest navbar-define-item/item-update/t--nil--nil ()
  (navbar-test-with-temp-item-definition navbarx-foo
    (navbar-test-save-item-list
      (navbar-define-item
	navbarx-foo t nil
	:get (lambda () nil))
      (should-not (navbarx-foo-update)))))

(ert-deftest navbar-define-item/item-update/nil--changed--displayed ()
  (navbar-test-with-temp-item-definition navbarx-foo
    (navbar-test-save-item-list
      (navbar-define-item
	navbarx-foo nil nil
	:get 'ignore)
      (setq navbar-item-list '(navbarx-foo))
      (navbar-initialize)
      ;; Make next (navbar-item-cache-put nil nil) non-`nil'.
      (navbar-item-cache-put nil t)
      (should (string= (navbarx-foo-update) "displayed")))))

(ert-deftest navbar-define-item/item-update/nil--unchanged--nil ()
  (navbar-test-with-temp-item-definition navbarx-foo
    (navbar-test-save-item-list
      (navbar-define-item
	navbarx-foo nil nil
	:get 'ignore)
      (should-not (navbarx-foo-update)))))

(ert-deftest navbar-define-string-item/test ()
  (navbar-test-with-temp-item-definition navbarx-foo
    (navbar-define-string-item
      navbarx-foo "foo" nil
      :key 'navbar-version)
    (should (equal navbarx-foo (list :key 'navbar-version :cache "foo")))))

(ert-deftest navbar-define-string-item/default-key ()
  (navbar-test-with-temp-item-definition navbarx-foo
    (navbar-define-string-item
      navbarx-foo "foo" nil)
    (should (equal navbarx-foo (list :key t :cache "foo")))))

(ert-deftest navbar-define-mode-item/test ()
  (navbar-test-with-temp-item-definition navbarx-foo
    (navbar-define-mode-item
      navbarx-foo navbar-test 'ignore
      nil
      :mode-on 'navbar-test--mode-on-func
      :mode-off 'navbar-test--mode-off-func)
    (should (equal navbarx-foo
		   (list :key 'navbar-test-mode
			 :get 'ignore
			 :initialize 'navbar-test--mode-on-func
			 :hooks navbar-test--mode-hooks)))))

;;;; `navbar-item-cache-put'

(ert-deftest navbar-item-cache-put/new-value ()
  (navbar-test-save-item-list
    (setq navbar-item-alist '((foo :cache "foo")))
    (should (navbar-item-cache-put 'foo "bar"))))

(ert-deftest navbar-item-cache-put/unchanged ()
  (navbar-test-save-item-list
    (setq navbar-item-alist '((foo :cache "foo")))
    (should-not (navbar-item-cache-put 'foo "foo"))))

(ert-deftest navbar-item-cache-put/nil ()
  (navbar-test-save-item-list
    (setq navbar-item-alist '((foo :cache "foo")))
    (should (navbar-item-cache-put 'foo nil))))

;;;; `navbar-serialize'

(ert-deftest navbar-serialize/string-cache ()
  (navbar-test-save-item-list
    (setq navbar-item-list '((:key t :cache "foo")))
    (navbar-initialize)
    (should (string= (navbar-serialize) "foo"))))

(ert-deftest navbar-serialize/list-cache ()
  (navbar-test-save-item-list
    (setq navbar-item-list '((:key t :cache ("foo" "bar"))))
    (navbar-initialize)
    (should (string= (navbar-serialize) "foo bar"))))

(ert-deftest navbar-serialize/ignore-nil-key ()
  (navbar-test-save-item-list
    (setq navbar-item-list '((:key t :cache "foo")
			     (:key nil :cache "bar")))
    (navbar-initialize)
    (should (string= (navbar-serialize) "foo"))))

(ert-deftest navbar-serialize/nest ()
  (navbar-test-save-item-list
    (setq navbar-item-list '((:key t :cache (("foo" "bar")))
			     (:key t :cache "baz")))
    (navbar-initialize)
    (should (string= (navbar-serialize) "foo bar baz"))))

;;;; `navbar-update'

(ert-deftest navbar-update/should-run-get-functions-if-key-is-nil ()
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

(ert-deftest navbar-initialize/raw-list ()
  (navbar-test-save-item-list
    (setq navbar-item-list (list navbar-test--item))
    (navbar-initialize)
    ;; Initialize `navbar-item-alist'
    (should (equal navbar-item-alist `((t ,@navbar-test--item))))))

(ert-deftest navbar-initialize/symbol-item ()
  (navbar-test-save-item-list
    (setq navbar-item-list '(navbar-test--item))
    (navbar-initialize)
    (should (equal navbar-item-alist `((t ,@navbar-test--item))))))

(ert-deftest navbar-initialize/string-item ()
  (navbar-test-save-item-list
    (setq navbar-item-list '("Hello, world!"))
    (navbar-initialize)
    (should (equal navbar-item-alist `((t :key t :cache "Hello, world!"))))))

(ert-deftest navbar-initialize/autoload ()
  (navbar-test-save-item-list
    (setq navbar-item-list '(navbarx-version))
    (navbar-initialize)
    (should (boundp 'navbarx-version))))

(ert-deftest navbar-initialize/order ()
  (navbar-test-save-item-list
    (let ((item1 '(:key t :cache "foo"))
	  (item2 '(:key bar :cache "bar")))
      (setq navbar-item-list (list item1 item2))
      (navbar-initialize)
      (should (eq (car (nth 0 navbar-item-alist)) 't))
      (should (eq (car (nth 1 navbar-item-alist)) 'bar)))))

(ert-deftest navbar-initialize/hooks ()
  (navbar-test-save-item-list
    (setq navbar-item-list `((:key t :hooks ,navbar-test--mode-hooks)))
    (unwind-protect
	(progn
	  (navbar-initialize)
	  (should (memq 'navbar-test--mode-on-func
			navbar-test-mode-on-hook))
	  (should (memq 'navbar-test--mode-off-func
			navbar-test-mode-off-hook)))
      (setq navbar-test-mode-on-hook nil)
      (setq navbar-test-mode-off-hook nil))))

(ert-deftest navbar-initialize/call-on-func-if-key-is-non-nil ()
  (navbar-test-save-item-list
    (setq navbar-item-list `((:key t :initialize navbar-test--mode-on-func)))
    (unwind-protect
	(progn
	  (navbar-initialize)
	  (should (get 'navbar-test--mode-on-func 'called)))
      (put 'navbar-test--mode-on-func 'called nil))))

(ert-deftest navbar-initialize/dont-call-on-func-if-key-is-nil ()
  (navbar-test-save-item-list
    (setq navbar-item-list `((:key nil :initialize navbar-test--mode-on-func)))
    (unwind-protect
	(progn
	  (navbar-initialize)
	  (should-not (get 'navbar-test--mode-on-func 'called)))
      (put 'navbar-test--mode-on-func 'called nil))))

(ert-deftest navbar-initialize/deinitialize ()
  (navbar-test-save-item-list
    (setq navbar-item-list `((:key t :hooks ,navbar-test--mode-hooks)))
    (navbar-initialize)
    (unwind-protect
	(progn
	  (setq navbar-item-list (list navbar-test--item))
	  (navbar-initialize)
	  (should (equal navbar-item-alist `((t ,@navbar-test--item))))
	  (should-not (memq 'navbar-test-mode-on-func
			    navbar-test-mode-on-hook))
	  (should-not (memq 'navbar-test-mode-off-func
			    navbar-test-mode-off-hook)))
      (setq navbar-test-mode-on-hook nil)
      (setq navbar-test-mode-off-hook nil))))

(ert-deftest navbar-deinitialize/test ()
  (navbar-test-save-item-list
    (setq navbar-item-list `((:key t :hooks ,navbar-test--mode-hooks)))
    (unwind-protect
	(progn
	  (navbar-initialize)
	  (navbar-deinitialize)
	  (should-not navbar-item-alist)
	  (should-not (memq 'navbar-test-mode-on-func
			    navbar-test-mode-on-hook))
	  (should-not (memq 'navbar-test-mode-off-func
			    navbar-test-mode-off-hook)))
      (setq navbar-test-mode-on-hook nil)
      (setq navbar-test-mode-off-hook nil))))

;;; GUI

(unless noninteractive
  (ert-deftest navbar-buffer-name/unique ()
    (let ((old-frame (selected-frame))
	  (new-frame (make-frame)))
      (unwind-protect
	  (should-not (string= (navbar-buffer-name old-frame)
			       (navbar-buffer-name new-frame)))
	(delete-frame new-frame)))))

(ert-deftest navbar-buffer-create/create ()
  (navbar-test-save-buffer-list
    (let ((buffer (navbar-buffer-create)))
      (should (bufferp buffer))
      (with-current-buffer buffer
	(should (string-prefix-p " *navbar " (buffer-name)))
	(should-not mode-line-format)
	(should-not cursor-type)
	(should truncate-lines)
	(should (eq window-size-fixed 'height))
	(should (eq (car (current-active-maps)) navbar-base-map))))))

(ert-deftest navbar-buffer-create/existing-buffer ()
  (navbar-test-save-buffer-list
    (should (eq (navbar-buffer-create) (navbar-buffer-create)))))

(ert-deftest navbar-make-window/create ()
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

(ert-deftest navbar-make-window/existing-window ()
  (navbar-test-save-buffer-list
    (save-window-excursion
      (should (eq (navbar-make-window) (navbar-make-window))))))

(ert-deftest navbar-kill-buffer-and-window/test ()
  (navbar-test-save-buffer-list
    (save-window-excursion
      (let ((window (navbar-make-window)))
	(navbar-kill-buffer-and-window)
	(should-not (navbar-buffer))
	(should-not (navbar-window))
	(should-not (window-valid-p window))))))

(unless noninteractive
  (ert-deftest navbar-test/fullheight-should-not-change-window-height ()
    (let* ((frame (make-frame))
	   (window (navbar-make-window frame)))
      (unwind-protect
	  (progn
	    (set-frame-parameter frame 'fullscreen 'fullheight)
	    (redisplay)
	    (should (= (window-total-height window) 1)))
	(delete-frame frame)))))

;;; Advices

(ert-deftest navbar-advice-next-window ()
  (save-window-excursion
    (delete-other-windows)
    (navbar-make-window)
    (unwind-protect
	(progn
	  (navbar-advices-setup)
	  (should (eq (selected-window) (next-window))))
      (navbar-advices-teardown)
      (should-not (eq (selected-window) (next-window))))))

(ert-deftest navbar-advice-window-list ()
  (save-window-excursion
    (delete-other-windows)
    (navbar-make-window)
    (unwind-protect
	(progn
	  (navbar-advices-setup)
	  (should (equal (list (selected-window)) (window-list))))
      (navbar-advices-teardown)
      (should-not (equal (list (selected-window)) (window-list))))))

;;; Mode

(ert-deftest navbar-mode/hooks ()
  (navbar-test-with-mode
    (should (memq 'navbar-update after-make-frame-functions))
    (should (memq 'navbar-make-window after-make-frame-functions)))
  (should-not (memq 'navbar-update after-make-frame-functions))
  (should-not (memq 'navbar-make-window after-make-frame-functions)))

(unless noninteractive
  (ert-deftest navbar-mode/multiple-frames ()
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
  (ert-deftest navbar-mode/make-frame ()
    (navbar-test-with-mode
      (let ((new-frame (make-frame)))
	(unwind-protect
	    (should (window-live-p (navbar-window new-frame)))
	  (delete-frame new-frame))))))

(ert-deftest navbar-mode/advices ()
  (navbar-test-with-mode
    (should (and (ad-find-advice 'next-window 'around 'navbar-ignore)))
    (should (and (ad-find-advice 'window-list 'around 'navbar-ignore))))
  (should-not (and (ad-find-advice 'next-window 'around 'navbar-ignore)))
  (should-not (and (ad-find-advice 'window-list 'around 'navbar-ignore))))

(ert-deftest navbar-mode/should-initialize-after-setup ()
  (navbar-test-save-item-list
    (setq navbar-item-list
	  (list (list :key t :cache "foo"
		      :initialize (lambda () (navbar-update nil t)))))
    (navbar-test-with-mode
      ;; Call `:initialize' function by `navbar-initialize'.
      ;; It is necessary to run `navbar-make-window' before that.
      )))

(ert-deftest navbar-mode/should-update-after-initialize ()
  (navbar-test-save-item-list
    (setq navbar-item-list '((:key t :cache "foo")))
    (navbar-test-with-mode
      ;; It is necessary to `navbar-update' after `navbar-initialize'
      ;; to display static contents.
      (with-current-buffer (navbar-buffer)
	(should (string= (buffer-string) "foo"))))))

;;; navbarx

(require 'navbarx-version)
(ert-deftest navbarx-version/test ()
  (should (= (length navbarx-version) 4))
  (should (eq (plist-get navbarx-version :key) t))
  (should (string= (plist-get navbarx-version :cache) "\u00bb\u00bb")))

(require 'navbarx-time)
(ert-deftest navbarx-time/test ()
  (should (= (length navbarx-time) 8))
  (should (eq (plist-get navbarx-time :key) 'display-time-mode))
  (should (eq (plist-get navbarx-time :get) 'navbarx-time-get))
  (should (eq (plist-get navbarx-time :initialize) 'navbarx-time-on))
  (should (equal (plist-get navbarx-time :hooks)
		 '((display-time-mode-on-hook . navbarx-time-on)
		   (display-time-mode-off-hook . navbarx-time-off)))))

(defvar navbarx-elscreen)
(when (require 'elscreen nil t)
  (require 'navbarx-elscreen)
  (ert-deftest navbarx-elscreen/test ()
    (should (= (length navbarx-elscreen) 8))
    (should (eq (plist-get navbarx-elscreen :key) 'elscreen-mode))
    (should (eq (plist-get navbarx-elscreen :get) 'navbarx-elscreen-get))
    (should (eq (plist-get navbarx-elscreen :initialize) 'navbarx-elscreen-on))
    (should (equal (plist-get navbarx-elscreen :hooks)
		   '((elscreen-mode-on-hook . navbarx-elscreen-on)
		     (elscreen-mode-off-hook . navbarx-elscreen-off))))))

(provide 'navbar-test)
;;; navbar-test.el ends here
