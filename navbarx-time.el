;;; navbarx-time.el --- display-time-mode support for navbar.el  -*- lexical-binding: t; -*-

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

(require 'time)
(require 'navbar)

;;;###autoload
(navbar-define-mode-item navbarx-time display-time
  nil
  :mode-on #'navbarx-time-on
  :mode-off #'navbarx-time-off
  :get #'navbarx-time-get)

(defvar navbarx-time-key (plist-get navbarx-time :key))

(defun navbarx-time-get ()
  (navbar-item-cache-put navbarx-time-key display-time-string))

(defun navbarx-time-update ()
  (when (if (symbol-value navbarx-time-key)
	    (navbarx-time-get)
	  (navbar-item-cache-put navbarx-time-key nil))
    (navbar-update nil navbarx-time-key)))

(defun navbarx-time-on ()
  (setq global-mode-string
	(delq 'display-time-string global-mode-string))
  (add-hook 'display-time-hook #'navbarx-time-update)
  (display-time-update))

(defun navbarx-time-off ()
  (navbarx-time-update)
  (remove-hook 'display-time-hook #'navbarx-time-update))

(provide 'navbarx-time)
;;; navbarx-time.el ends here
