;;; org-menu.el --- Hide verbose org syntax behind interactive menus.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  D. Williams, Rasmus Pank

;; Author: D. Williams <d.williams@posteo.net>
;; Maintainer: D. Williams <d.williams@posteo.net>
;; Keywords: faces, outlines
;; Version: 0
;; Homepage: https://github.com/integral-dw/org-menu-mode
;; Package-Requires: ((org "9.1.9") (emacs "26.2"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Anatomy of a menu-ized header line:
;; SRC blocks:
;; (menu icon) (language name) (menu button)

;;; Code:

(require 'org)
(require 'org-element)

(defgroup org-menu nil
  "Hide Org syntax behind interactive menus."
  :group 'org-appearance)

(defcustom org-menu-src-default ?🖉
  "Default menu icon for Org source code blocks."
  :group 'org-menu
  :type 'character)

(defcustom org-menu-header-symbol ?☰
  "Character used for the menu button."
  :group 'org-menu
  :type 'character)

(defcustom org-menu-src-alist
  '(("emacs-lisp" . ?λ)
    ("lisp" . ?λ)
    ("clojure". ?λ))
  "Alist of language-specific menu icons for Org source code blocks.

Each key should be the name of the language as a string, each
value should be a character to be used for display.  If no entry
exists for a given language, ‘org-menu-src-default’ is used
instead."
  :group 'org-menu
  :type `(alist :key-type (string :format "Language name: %v")
                :value-type (character :value ,org-menu-src-default
                                       :format "Menu icon: %v\n")))

;;; Accessor Functions

(defun org-menu--get-src-icon ()
  "Return the desired menu icon for the current Org source code block.

This function returns the desired icon from ‘org-menu-src-alist’,
if specified.  If the selected language has no user-defined icon,
‘org-menu-src-default’ is used instead."
  (let ((lang (match-string 1)))
    (or (cdr (assoc-string lang org-menu-src-alist t))
        org-menu-src-default)))


;;; Predicates

(defun org-menu-in-src-block-p ()
  "Return t if point is in an Org source code block."
  (save-match-data
    (when (memq
           (org-element-type (org-element-context))
           '(inline-src-block src-block))
      t)))


;;; Fontification

(defun org-menu--prettify-src-begin ()
  "Prettify the delimiter of an Org source code block.
"
  (let ()

    ))

;;; Font Lock

(defvar org-menu--src-regexp
  (concat "^\\(?:[ \t]*\\)" ;; indentation
          "\\(?2:#\\+begin_src\\)"  ;; delimiter
          "\\(?4:[ \t]+)" ;; garbage
          "\\(?1:\\S-+\\)" ;; language name
          "\\(?3:\\s-.*\\)$") ;; rest
  "Regular expression used to identify Org source code blocks.")

(defvar-local org-menu--font-lock-keywords nil)
(defun org-menu--update-font-lock-keywords ()
  "Set ‘org-menu--font-lock-keywords’ to reflect current settings.
You should not call this function to avoid confusing this mode's
cleanup routines."
  (setq org-menu--font-lock-keywords
        `(;; SRC blocks are special, and deserve extra fanciness.
          (,org-menu--src-regexp
           ;; First, conditionally compose #+BEGIN_SRC
           (2 (org-menu--prettify-src-begin)))
          ;; TODO: more later.
          )))


(provide 'org-menu)
;;; org-menu.el ends here
