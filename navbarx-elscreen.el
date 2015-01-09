;;; navbarx-elscreen.el --- ElScreen support for navbar.el  -*- lexical-binding: t; -*-

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

(defvar navbarx-elscreen-key 'elscreen-mode)

(defun navbarx-elscreen-get ()
  (when (and (not (window-minibuffer-p))
	     (elscreen-screen-modified-p 'navbarx-elscreen-get))
    (navbar-item-cache-put navbarx-elscreen-key
			   (navbarx-elscreen-get1))))

(defun navbarx-elscreen-get1 ()
  (let ((screen-list (sort (elscreen-get-screen-list) '<))
	(screen-to-name-alist (elscreen-get-screen-to-name-alist))
	(current-screen (elscreen-get-current-screen))
	(half-space (propertize
		     " "
		     'keymap navbar-base-map
		     'display '(space :width 0.5)))
	;; When `elscreen-tab-display-kill-screen' is `right',
	;; two consecutive half spaces precede the screen number.
	;; It is necessary to insert two different half space objects,
	;; since two consecutive `half-space' display one half space.
	(tab-separator (propertize
			" "
			'keymap navbar-base-map
			'display '(space :width 0.2)))
	(kill-screen (propertize
		      "[\u00d7]"
		      'keymap navbarx-elscreen--kill-screen-map
		      'help-echo 'navbarx-elscreen-kill-screen-help)))
    (apply 'append
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
		(list
		 (propertize tab-string
			     'face tab-face
			     'pointer 'hand
			     'navbarx-elscreen-screen screen)
		 tab-separator)))
	    screen-list))))

(defun navbarx-elscreen-update ()
  (when (if (symbol-value navbarx-elscreen-key)
	    (navbarx-elscreen-get)
	  (navbar-item-cache-put navbarx-elscreen-key nil))
    (navbar-update nil navbarx-elscreen-key)))

(defun navbarx-elscreen-on ()
  (add-hook 'elscreen-screen-update-hook #'navbarx-elscreen-update))

(defun navbarx-elscreen-off ()
  (navbarx-elscreen-update)
  (remove-hook 'elscreen-screen-update-hook #'navbarx-elscreen-update))

;;;###autoload
(defvar navbarx-elscreen
  (list :key navbarx-elscreen-key
	:get #'navbarx-elscreen-get
	:on #'navbarx-elscreen-on
	:off #'navbarx-elscreen-off))

(provide 'navbarx-elscreen)
;;; navbarx-elscreen.el ends here
