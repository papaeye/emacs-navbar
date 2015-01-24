;;; navbarx-mew.el --- Mew support for navbar.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2015  papaeye

;; Author: papaeye <papaeye@gmail.com>
;; Keywords: convenience
;; Version: 0.1.0
;; Homepage: https://github.com/papaeye/emacs-navbar
;; Package-Requires: ((navbar "0.1.0") (mew "6.6"))

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

(require 'mew)
(require 'navbar)

(defface navbarx-mew
  '((t :inherit navbar-item))
  "Face of `navbarx-mew'."
  :group 'navbar)

(defvar navbarx-mew-biff-function-original nil)

(defun navbarx-mew-biff-function (n)
  (funcall navbarx-mew-biff-function-original n)
  (navbarx-mew-update))

(defun navbarx-mew-get ()
  (and mew-biff-string
       (propertize (concat navbar-item-padding mew-biff-string)
		   'face 'navbarx-mew)))

(defun navbarx-mew-delete-mode-string ()
  (setq global-mode-string
	(assq-delete-all 'mew-biff-string global-mode-string)))

(defun navbarx-mew-on ()
  (setq navbarx-mew-biff-function-original mew-biff-function)
  (setq mew-biff-function #'navbarx-mew-biff-function)
  (add-hook 'mew-status-update-hook #'navbarx-mew-delete-mode-string)
  (add-hook 'mew-pop-sentinel-hook #'navbarx-mew-update)
  (add-hook 'mew-imap-sentinel-hook #'navbarx-mew-update))

(defun navbarx-mew-off ()
  (setq mew-biff-function navbarx-mew-biff-function-original)
  (remove-hook 'mew-status-update-hook #'navbarx-mew-delete-mode-string)
  (remove-hook 'mew-pop-sentinel-hook #'navbarx-mew-update)
  (remove-hook 'mew-imap-sentinel-hook #'navbarx-mew-update))

;;;###autoload (autoload 'navbarx-mew "navbarx-mew")
(navbar-define-item navbarx-mew
  "Navbar item for Mew support."
  :enable mew-init-p
  :get #'navbarx-mew-get
  :initialize #'navbarx-mew-on
  :deinitialize #'navbarx-mew-off
  :hooks (list (cons 'mew-init-hook #'navbarx-mew-on)
	       (cons 'mew-quit-hook #'navbarx-mew-off)))

(provide 'navbarx-mew)
;;; navbarx-mew.el ends here
