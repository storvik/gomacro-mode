;;; gomacro-mode-tests.el --- Tests for gomacro mode

;; Copyright © 2020

;; Author: Petter S. Storvik

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'gomacro-mode)
(message "Emacs version: %s" emacs-version)

(defvar gomacro-sanitize-string-struct "type element struct {
	number int    // Comment here
	str    string // Another comment here
}")

(defvar gomacro-sanitize-string-interface "type Printer interface {
	Print()

	String()
}")

(ert-deftest gomacro-sanitize-string()
  (should (equal (gomacro--sanitize-string gomacro-sanitize-string-struct)
                 "type element struct {number int;str string;}"))
  (should (equal (gomacro--sanitize-string gomacro-sanitize-string-interface)
                 "type Printer interface {Print();String();}")))

(provide 'gomacro-mode-test)
;;; gomacro-mode-test.el ends here
