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

(defmacro navbar-test-with-item-list (item-list &rest body)
  (declare (indent 1) (debug t))
  `(navbar-test-save-item-list
     (setq navbar-item-list ,item-list)
     ,@body))

(defmacro navbar-test-with-temp-item (item &rest body)
  (declare (indent 1) (debug t))
  `(let* ((temp-item ,item)
	  (temp-item-name (symbol-name temp-item))
	  (temp-item-value-put (intern (concat temp-item-name "-value-put")))
	  (temp-item-update (intern (concat temp-item-name "-update")))
	  (navbar-display-function (lambda (_buffer) "displayed")))
     (unwind-protect
	 (progn ,@body)
       (makunbound temp-item)
       (fmakunbound temp-item-value-put)
       (fmakunbound temp-item-update))))

(defmacro navbar-test-save-test-mode (&rest body)
  (declare (indent 0) (debug t))
  `(let ((old-mode (or navbar-test-mode -1)))
     (unwind-protect
	 (progn ,@body)
       (navbar-test-mode old-mode)
       (setq navbar-test-mode-on-hook nil)
       (setq navbar-test-mode-off-hook nil)
       (put 'navbar-test--mode-on-func 'called nil)
       (put 'navbar-test--mode-off-func 'called nil))))

(defvar navbar-test-mode-on-hook)
(defvar navbar-test-mode-off-hook)
(define-minor-mode navbar-test-mode nil
  :group 'navbar
  :global t)

(defun navbar-test--mode-on-func ()
  (put 'navbar-test--mode-on-func 'called t))
(defun navbar-test--mode-off-func ()
  (put 'navbar-test--mode-off-func 'called t))

(defvar navbar-test--mode-hooks
  (list (cons 'navbar-test-mode-on-hook 'navbar-test--mode-on-func)
	(cons 'navbar-test-mode-off-hook 'navbar-test--mode-off-func)))

(navbar-define-item navbar-test--item
  "Navbar string item for testing."
  :value "foo")

(navbar-define-item navbar-test--mode-item
  "Navbar mode item for testing."
  :get #'ignore
  :mode navbar-test-mode
  :mode-on 'navbar-test--mode-on-func
  :mode-off 'navbar-test--mode-off-func)

;;; Features

;;;; navbar item API

(ert-deftest navbar-define-item/test ()
  (navbar-test-with-temp-item (navbar-define-item navbarx-foo nil
				:enable navbar-version)
    (should (equal navbarx-foo '(:key navbarx-foo :enable navbar-version)))
    (should (fboundp 'navbarx-foo-value-put))
    (should-not (fboundp 'navbarx-foo-update))))

(ert-deftest navbar-define-item/should-define-update-if-:get-available ()
  (navbar-test-with-temp-item (navbar-define-item navbarx-foo nil
				:get 'ignore)
    (should (equal navbarx-foo '(:key navbarx-foo :enable t :get ignore)))
    (should (fboundp 'navbarx-foo-value-put))
    (should (fboundp 'navbarx-foo-update))))

(ert-deftest navbar-define-item/item-update/t--new-value--displayed ()
  (navbar-test-with-temp-item (navbar-define-item navbarx-foo nil
				:get (lambda () "new-value"))
    (navbar-test-save-item-list
      (setq navbar-item-alist `((navbarx-foo ,@navbarx-foo)))
      (should (string= (navbarx-foo-update) "displayed")))))

(ert-deftest navbar-define-item/item-update/t--nil--nil ()
  (navbar-test-with-temp-item (navbar-define-item navbarx-foo nil
				:get (lambda () nil))
    (navbar-test-save-item-list
      (setq navbar-item-alist `((navbarx-foo ,@navbarx-foo)))
      (should-not (navbarx-foo-update)))))

(ert-deftest navbar-define-item/item-update/nil--changed--displayed ()
  (navbar-test-with-temp-item (navbar-define-item navbarx-foo nil
				:enable nil
				:get 'ignore)
    (navbar-test-save-item-list
      (setq navbar-item-alist `((navbarx-foo ,@navbarx-foo)))
      ;; Make next (navbar-item-value-put 'navbarx-foo nil) non-`nil'.
      (navbar-item-value-put 'navbarx-foo t)
      (should (string= (navbarx-foo-update) "displayed")))))

(ert-deftest navbar-define-item/item-update/nil--unchanged--nil ()
  (navbar-test-with-temp-item (navbar-define-item navbarx-foo nil
				:enable nil
				:get 'ignore)
    (navbar-test-save-item-list
      (setq navbar-item-alist `((navbarx-foo ,@navbarx-foo)))
      (should-not (navbarx-foo-update)))))

(ert-deftest navbar-define-item/string-item ()
  (navbar-test-with-temp-item (navbar-define-item navbarx-foo nil
				:enable navbar-version
				:value "foo")
    (should (equal navbarx-foo
		   '(:key navbarx-foo :enable navbar-version :value "foo")))))

(ert-deftest navbar-define-item/mode-item ()
  (navbar-test-with-temp-item (navbar-define-item navbarx-foo nil
				:get #'ignore
				:mode navbar-test-mode
				:mode-on #'navbar-test--mode-on-func
				:mode-off #'navbar-test--mode-off-func)
    (should (equal navbarx-foo
		   (list :key 'navbarx-foo
			 :enable 'navbar-test-mode
			 :get 'ignore
			 :initialize 'navbar-test--mode-on-func
			 :deinitialize 'navbar-test--mode-off-func
			 :hooks navbar-test--mode-hooks)))))

(ert-deftest navbar-define-item/mode-item-without-mode-property ()
  (navbar-test-with-temp-item (navbar-define-item navbarx-foo nil
				:get #'ignore
				:enable navbar-test-mode
				:initialize #'navbar-test--mode-on-func
				:deinitialize #'navbar-test--mode-off-func
				:hooks navbar-test--mode-hooks)
    (should (equal navbarx-foo
		   (list :key 'navbarx-foo
			 :enable 'navbar-test-mode
			 :get 'ignore
			 :initialize 'navbar-test--mode-on-func
			 :deinitialize 'navbar-test--mode-off-func
			 :hooks navbar-test--mode-hooks)))))

;;;; `navbar-item-value-put'

(ert-deftest navbar-item-value-put/new-value ()
  (navbar-test-save-item-list
    (setq navbar-item-alist '((foo :value "foo")))
    (should (navbar-item-value-put 'foo "bar"))))

(ert-deftest navbar-item-value-put/unchanged ()
  (navbar-test-save-item-list
    (setq navbar-item-alist '((foo :value "foo")))
    (should-not (navbar-item-value-put 'foo "foo"))))

(ert-deftest navbar-item-value-put/nil ()
  (navbar-test-save-item-list
    (setq navbar-item-alist '((foo :value "foo")))
    (should (navbar-item-value-put 'foo nil))))

;;;; `navbar-serialize'

(ert-deftest navbar-serialize/string-value ()
  (navbar-test-with-item-list '((:key t :value "foo"))
    (navbar-initialize)
    (should (string= (navbar-serialize) "foo"))))

(ert-deftest navbar-serialize/list-value ()
  (navbar-test-with-item-list '((:key t :value ("foo" "bar")))
    (navbar-initialize)
    (should (string= (navbar-serialize) "foo bar"))))

(ert-deftest navbar-serialize/ignore-disabled-item ()
  (navbar-test-with-item-list '((:key t :value "foo")
				(:key t :enable nil :value "bar"))
    (navbar-initialize)
    (should (string= (navbar-serialize) "foo"))))

(ert-deftest navbar-serialize/nest ()
  (navbar-test-with-item-list '((:key t :value (("foo" "bar")))
				(:key t :value "baz"))
    (navbar-initialize)
    (should (string= (navbar-serialize) "foo bar baz"))))

;;;; `navbar-initialize'

(ert-deftest navbar-initialize/raw-list ()
  (navbar-test-with-item-list '((:key t :value "foo"))
    (navbar-initialize)
    ;; Initialize `navbar-item-alist'
    (should (equal navbar-item-alist '((t :key t :value "foo"))))))

(ert-deftest navbar-initialize/symbol-item ()
  (navbar-test-with-item-list '(navbar-test--item)
    (navbar-initialize)
    (should (equal navbar-item-alist
		   `((navbar-test--item ,@navbar-test--item))))))

(ert-deftest navbar-initialize/string-item ()
  (navbar-test-with-item-list '("Hello, world!")
    (navbar-initialize)
    (should (equal navbar-item-alist `((t :key t :value "Hello, world!"))))))

(ert-deftest navbar-initialize/autoload ()
  (navbar-test-with-item-list '(navbarx-version)
    (navbar-initialize)
    (should (boundp 'navbarx-version))))

(ert-deftest navbar-initialize/order ()
  (navbar-test-with-item-list '((:key t :value "foo")
				(:key bar :value "bar"))
    (navbar-initialize)
    (should (eq (car (nth 0 navbar-item-alist)) 't))
    (should (eq (car (nth 1 navbar-item-alist)) 'bar))))

(ert-deftest navbar-initialize/hooks ()
  (navbar-test-with-item-list `((:key t :hooks ,navbar-test--mode-hooks))
    (navbar-test-save-test-mode
      (navbar-initialize)
      (should (memq 'navbar-test--mode-on-func
		    navbar-test-mode-on-hook))
      (should (memq 'navbar-test--mode-off-func
		    navbar-test-mode-off-hook)))))

(ert-deftest navbar-initialize/call-:initialize-if-enabled ()
  (navbar-test-with-item-list
      `((:key t :initialize navbar-test--mode-on-func))
    (navbar-test-save-test-mode
      (navbar-initialize)
      (should (get 'navbar-test--mode-on-func 'called)))))

(ert-deftest navbar-initialize/dont-call-:initialize-if-disabled ()
  (navbar-test-with-item-list
      `((:key t :enable nil :initialize navbar-test--mode-on-func))
    (navbar-test-save-test-mode
      (navbar-initialize)
      (should-not (get 'navbar-test--mode-on-func 'called)))))

(ert-deftest navbar-initialize/deinitialize ()
  (navbar-test-with-item-list `((:key t :hooks ,navbar-test--mode-hooks))
    (navbar-test-save-test-mode
      (navbar-initialize)
      (setq navbar-item-list (list navbar-test--item))
      (navbar-initialize)
      (should (equal navbar-item-alist
		     `((navbar-test--item ,@navbar-test--item))))
      (should-not (memq 'navbar-test-mode-on-func
			navbar-test-mode-on-hook))
      (should-not (memq 'navbar-test-mode-off-func
			navbar-test-mode-off-hook)))))

(ert-deftest navbar-deinitialize/hooks ()
  (navbar-test-with-item-list `((:key t :hooks ,navbar-test--mode-hooks))
    (navbar-test-save-test-mode
      (navbar-initialize)
      (navbar-deinitialize)
      (should-not navbar-item-alist)
      (should-not (memq 'navbar-test-mode-on-func
			navbar-test-mode-on-hook))
      (should-not (memq 'navbar-test-mode-off-func
			navbar-test-mode-off-hook)))))

(ert-deftest navbar-deinitialize/call-:deinitialize ()
  (navbar-test-with-item-list
      '((:key t :deinitialize navbar-test--mode-off-func))
    (navbar-test-save-test-mode
      (navbar-initialize)
      (navbar-deinitialize)
      (should (get 'navbar-test--mode-off-func 'called)))))

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
  (navbar-test-with-item-list
      (list (list :key t :value "foo"
		  :initialize (lambda () (navbar-update nil t))))
    (navbar-test-with-mode
      ;; Call `:initialize' function by `navbar-initialize'.
      ;; It is necessary to run `navbar-make-window' before that.
      )))

(ert-deftest navbar-mode/should-update-after-initialize ()
  (navbar-test-with-item-list '((:key t :value "foo"))
    (navbar-test-with-mode
      ;; It is necessary to `navbar-update' after `navbar-initialize'
      ;; to display static contents.
      (with-current-buffer (navbar-buffer)
	(should (string= (buffer-string) "foo"))))))

(ert-deftest navbar-mode/font-lock-keywords ()
  (navbar-test-with-mode
    (should
     (member navbar-font-lock-keywords
	     (cadr (assq 'emacs-lisp-mode font-lock-keywords-alist)))))
  (should-not
   (member navbar-font-lock-keywords
	   (cadr (assq 'emacs-lisp-mode font-lock-keywords-alist)))))

;;; navbarx

(require 'navbarx-version)
(ert-deftest navbarx-version/test ()
  (should (= (length navbarx-version) 6))
  (should (eq (plist-get navbarx-version :key) 'navbarx-version))
  (should (eq (plist-get navbarx-version :enable) t))
  (should (string= (plist-get navbarx-version :value) "\u00bb\u00bb")))

(require 'navbarx-time)
(ert-deftest navbarx-time/test ()
  (navbar-test-with-item-list '(navbarx-time)
    (navbar-test-with-mode
      (unwind-protect
	  (progn
	    (display-time-mode)
	    (should-not (memq 'display-time-string global-mode-string))
	    (should (memq #'navbarx-time-update display-time-hook))
	    (should (navbar-item-value-get 'navbarx-time))
	    (should (navbar-item-enabled-p 'navbarx-time)))
	(display-time-mode -1)
	(should-not (navbar-item-value-get 'navbarx-time))
	(should-not (navbar-item-enabled-p 'navbarx-time))
	(should-not (memq #'navarx-time-update display-time-hook))))))

(defvar navbarx-elscreen)
(defvar elscreen-screen-update-hook)
(when (require 'elscreen nil t)
  (require 'navbarx-elscreen)
  (ert-deftest navbarx-elscreen/test ()
    (navbar-test-with-item-list '(navbarx-elscreen)
      (navbar-test-with-mode
	(unwind-protect
	    (progn
	      (elscreen-mode)
	      (should-not (memq 'elscreen-tab-update
				elscreen-screen-update-hook))
	      (should (memq 'navbarx-elscreen-update
			    elscreen-screen-update-hook))
	      (should (navbar-item-value-get 'navbarx-elscreen))
	      (should (navbar-item-enabled-p 'navbarx-elscreen)))
	  (elscreen-mode -1)
	  (should (memq 'elscreen-tab-update elscreen-screen-update-hook))
	  (should-not (memq 'navbarx-elscreen-update
			    elscreen-screen-update-hook))
	  (should-not (navbar-item-value-get 'navbarx-elscreen))
	  (should-not (navbar-item-enabled-p 'navbarx-elscreen)))))))

(provide 'navbar-test)
;;; navbar-test.el ends here
