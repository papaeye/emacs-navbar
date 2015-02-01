;;; navbarx-elscreen.el --- ElScreen support for navbar.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  papaeye

;; Author: papaeye <papaeye@gmail.com>
;; Keywords: convenience
;; Version: 0.1.0
;; Homepage: https://github.com/papaeye/emacs-navbar
;; Package-Requires: ((navbar "0.1.0") (elscreen "1.4.6"))

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

(require 'elscreen)
(require 'navbar)

(defun navbarx-elscreen-screen-command (command)
  (lambda (event)
    (interactive "e")
    (let* ((position (event-start event))
	   (point (posn-point position))
	   (window (posn-window position))
	   (screen (navbar-property-at point 'navbarx-elscreen-screen window)))
      (funcall command screen))))

(defun navbarx-elscreen-kill-screen-help (window _object pos)
  (let ((screen (navbar-property-at pos 'navbarx-elscreen-screen window)))
    (format
     "mouse-1: kill screen %d, M-mouse-1: kill screen %d and buffers on it"
     screen screen)))

(defvar navbarx-elscreen-kill-screen-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") (navbarx-elscreen-screen-command
				       'elscreen-kill))
    (define-key map (kbd "M-<mouse-1>") (navbarx-elscreen-screen-command
					 'elscreen-kill-screen-and-buffers))
    map))

(defvar navbarx-elscreen-tab-body-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") (navbarx-elscreen-screen-command
				       'elscreen-goto))
    map))

(defvar navbarx-elscreen-kill-screen
  (concat (propertize "[\u00d7]"
		      'keymap navbarx-elscreen-kill-screen-map
		      'help-echo 'navbarx-elscreen-kill-screen-help)
	  ;; Reset the keymap for the right item
	  (propertize " " 'display '(space :width 0))))

(defun navbarx-elscreen-get (&optional force)
  (if (and (not (window-minibuffer-p))
	   ;; The order is significant
	   (or (elscreen-screen-modified-p 'navbarx-elscreen-get)
	       force))
      (navbarx-elscreen--get)
    'unchanged))

(defun navbarx-elscreen--get ()
  (let ((screen-list (sort (elscreen-get-screen-list) '<))
	(screen-to-name-alist (elscreen-get-screen-to-name-alist))
	(current-screen (elscreen-get-current-screen)))
    (mapcar
     (lambda (screen)
       (let ((screen-name (cdr (assq screen screen-to-name-alist)))
	     (tab-face (if (= screen current-screen)
			   'elscreen-tab-current-screen-face
			 'elscreen-tab-other-screen-face))
	     tab-string)

	 (setq tab-string
	       (concat
		(when (memq elscreen-tab-display-kill-screen '(left t))
		  navbarx-elscreen-kill-screen)
		navbar-item-padding
		(propertize
		 (concat
		  (when (< screen 10)
		    (number-to-string screen))
		  (elscreen-status-label screen)
		  navbar-item-padding
		  screen-name)
		 'help-echo screen-name
		 'keymap navbarx-elscreen-tab-body-map)
		navbar-item-padding
		(when (eq elscreen-tab-display-kill-screen 'right)
		  navbarx-elscreen-kill-screen)))

	 (propertize tab-string
		     'face tab-face
		     'pointer 'hand
		     'navbarx-elscreen-screen screen)))
     screen-list)))

(defun navbarx-elscreen-on ()
  (if elscreen-frame-confs
      (navbarx-elscreen--on)
    (defadvice elscreen-start (after navbarx-elscreen-start activate)
      (navbarx-elscreen--on))))

(defun navbarx-elscreen--on ()
  (remove-hook 'elscreen-screen-update-hook 'elscreen-tab-update)
  (add-hook 'elscreen-screen-update-hook #'navbarx-elscreen-update)
  ;; When `elscreen-start' is run after `navbar-mode' is enabled,
  ;; hook functions of `elscreen-screen-update-hook' have already run
  ;; by `elscreen-make-frame-confs' before adding `navbarx-elscreen-update'
  ;; to `elscreen-screen-update-hook' by `navbarx-elscreen-on',
  ;; thus it is necessary to run `navbarx-elscreen-update' here.
  ;;
  ;; Moreover, successive call of `navbarx-elscreen-get' returns `unchanged'
  ;; because of `elscreen-screen-modified-p', thus it is necessary to
  ;; get ElScreen tabs forcibly.
  (navbarx-elscreen-update 'force))

(defun navbarx-elscreen-off ()
  (ignore-errors
    (ad-remove-advice 'elscreen-start 'after 'navbarx-elscreen-start)
    (ad-update 'elscreen-start))

  (add-hook 'elscreen-screen-update-hook 'elscreen-tab-update)
  (remove-hook 'elscreen-screen-update-hook #'navbarx-elscreen-update))

;;;###autoload (autoload 'navbarx-elscreen "navbarx-elscreen")
(navbar-define-item navbarx-elscreen
  "Navbar item for ElScreen support."
  :enable t
  :get #'navbarx-elscreen-get
  :initialize #'navbarx-elscreen-on
  :deinitialize #'navbarx-elscreen-off)

(provide 'navbarx-elscreen)
;;; navbarx-elscreen.el ends here
