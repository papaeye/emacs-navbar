;;; navbarx-elscreen.el --- ElScreen support for navbar.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  papaeye

;; Author: papaeye <papaeye@gmail.com>
;; Keywords: convenience
;; Version: 0.1.0
;; Homepage: https://github.com/papaeye/emacs-navbar
;; Package-Requires: ((navbar "0.1.0") (elscreen "20141230"))

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

(defun navbarx-elscreen--screen-command (command)
  (lambda (event)
    (interactive "e")
    (let* ((position (event-start event))
	   (point (posn-point position))
	   (window (posn-window position))
	   (screen (navbar-property-at point 'navbarx-elscreen-screen window)))
      (funcall command screen))))

(defvar navbarx-elscreen--kill-screen-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") (navbarx-elscreen--screen-command
				       'elscreen-kill))
    (define-key map (kbd "M-<mouse-1>") (navbarx-elscreen--screen-command
					 'elscreen-kill-screen-and-buffers))
    map))

(defvar navbarx-elscreen--tab-body-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") (navbarx-elscreen--screen-command
				       'elscreen-goto))
    map))

(defun navbarx-elscreen--kill-screen-help (window _object pos)
  (let ((screen (navbar-property-at pos 'navbarx-elscreen-screen window)))
    (format "mouse-1: kill screen %d, M-mouse-1: kill screen %d and buffers on it" screen screen)))

(defun navbarx-elscreen-get ()
  (if (and (not (window-minibuffer-p))
	   (elscreen-screen-modified-p 'navbarx-elscreen-get))
      (navbarx-elscreen-get1)
    'unchanged))

(defun navbarx-elscreen-get1 ()
  (let ((screen-list (sort (elscreen-get-screen-list) '<))
	(screen-to-name-alist (elscreen-get-screen-to-name-alist))
	(current-screen (elscreen-get-current-screen))
	(half-space (propertize
		     " "
		     'keymap navbar-base-map
		     'display '(space :width 0.5)))
	(kill-screen (propertize
		      "[\u00d7]"
		      'keymap navbarx-elscreen--kill-screen-map
		      'help-echo 'navbarx-elscreen--kill-screen-help)))
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
		  kill-screen)
		half-space
		(propertize
		 (concat
		  (when (< screen 10)
		    (number-to-string screen))
		  (elscreen-status-label screen)
		  half-space
		  screen-name)
		 'help-echo screen-name
		 'keymap navbarx-elscreen--tab-body-map
		 'navbar-truncate t)
		(when (eq elscreen-tab-display-kill-screen 'right)
		  (concat half-space kill-screen))))
	 (concat
	  (propertize tab-string
		      'face tab-face
		      'pointer 'hand
		      'navbarx-elscreen-screen screen)
	  ;; Reset the keymap for the right item
	  (propertize " " 'display '(space :width 0)))))
     screen-list)))

(defun navbarx-elscreen-on ()
  (remove-hook 'elscreen-screen-update-hook 'elscreen-tab-update)
  (add-hook 'elscreen-screen-update-hook #'navbarx-elscreen-update)
  ;; This is necessary to initialize the navbar buffer
  ;; when `elscreen-mode' is enabled after `navbar-mode' is enabled,
  ;; since `elscreen-make-frame-confs' is run before `navbarx-elscreen-on'.
  (navbarx-elscreen-update))

(defun navbarx-elscreen-off ()
  (navbarx-elscreen-update)
  (add-hook 'elscreen-screen-update-hook 'elscreen-tab-update)
  (remove-hook 'elscreen-screen-update-hook #'navbarx-elscreen-update))

(navbar-define-item navbarx-elscreen
  "Navbar item for ElScreen support."
  :get #'navbarx-elscreen-get
  :mode elscreen-mode
  :mode-on #'navbarx-elscreen-on
  :mode-off #'navbarx-elscreen-off)

(provide 'navbarx-elscreen)
;;; navbarx-elscreen.el ends here
