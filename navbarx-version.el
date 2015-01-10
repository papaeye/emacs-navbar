;;; navbarx-version.el --- navbar-version support for navbar.el  -*- lexical-binding: t; -*-

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

(require 'navbar)

(defface navbarx-version
  '((t :foreground "#dc322f" :inverse-video t))
  "Face of `navbarx-version'."
  :group 'navbar)

;;;###autoload
(defvar navbarx-version
  (list :key 'navbar-version
	:cache (propertize
		"\u00bb\u00bb"
		'face 'navbarx-version
		'help-echo (concat "navbar " navbar-version))))

(provide 'navbarx-version)
;;; navbarx-version.el ends here
