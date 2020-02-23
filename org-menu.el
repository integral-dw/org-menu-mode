;;; org-menu.el --- Hide verbose org syntax behind interactive menus  -*- lexical-binding: t; -*-

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
(require 'subr-x)

(defgroup org-menu nil
  "Hide Org syntax behind interactive menus."
  :group 'org-appearance)

;;;; Custom Variables

(defcustom org-menu-src-default ?🖉
  "Default menu icon for Org source code blocks."
  :group 'org-menu
  :type 'character)

(defcustom org-menu-symbol ?☰
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


;;;; Text Properties and Manipulation
;; Here we define the lower level routines operating directly on text
;; properties.

(defvar org-menu--extra-props
  '(org-menu-region org-menu-include-eol)
  "List of text properties for Org Menu mode’s font lock internals.")

;;; Local State Variables
(defvar-local org-menu--active-region nil
  "Holds the delimiters of the region composed by Org Menu at point.

If point does not reside in any composed region, it’s value is
nil.  Otherwise, it’s value is a list of the form:

  (START END . META-PLIST)

where START and END delimit the affected region.

META-PLIST is a property list holding additional information
about the current region.

This variable is necessary because the \"true\" position of point
is not accessible from within font-lock.")

;;; Getters and Setters for the Active Region
(defun org-menu--active-region-start ()
  "Return the beginning of the active region."
  (car org-menu--active-region))

(defun org-menu--active-region-end ()
  "Return the end of the active region."
  (cadr org-menu--active-region))

(defun org-menu--set-active-region ()
  "If point is in an Org Menu composed region, make it active.
If no region could be found, return nil.  Otherwise, return a
list of the form (START END), containing the delimiting points of
the region found."
  ;; NOTE: This package is not made for single-char matches.
  ;; Consequently, there is no need to look at eol's face props.
  (let* ((pos (if (and (eolp) (not (bobp)))
                  (1- (point)) (point)))
         (new-region (get-text-property pos 'org-menu-region))
         (include-eol (get-text-property pos 'org-menu-include-eol)))
    (when new-region
      (setq org-menu--active-region
            (append new-region
                    `(include-eol ,include-eol)))
    new-region)))

(defun org-menu--get-prop (property)
  "Return the value of PROPERY stored in the active region.

PROPERTY should be an optional argument name of the function
‘org-menu--mark-composed’, as a symbol."
  (plist-get (cddr org-menu--active-region) property))

;;; Active Region Predicates
(defun org-menu--active-region-p (start end)
  "Return t if the region between START and END is active."
  (and org-menu--active-region
       (= start (org-menu--active-region-start))
       (= end (org-menu--active-region-end))))

(defun org-menu--outside-active-region-p ()
  "Return t if point is not in the active region.
If there is no active region, return nil."
  (let ((start (org-menu--active-region-start))
        (end (org-menu--active-region-end))
        (pos (point)))
    ;; Here we only cover the case moving from the active region to
    ;; EOL.  Entering an active region via EOL is governed by
    ;; ‘org-menu--set-active-region’.

    ;; This does not work yet.  The region information must be updated
    ;; when writing on the same line.
    (when (and (eolp) (org-menu--get-prop 'include-eol))
      (setq pos (1- pos)))
    (and org-menu--active-region
         (not (<= start pos end)))))

;;; Manipulating the Region

(defun org-menu--mark-composed (start end &optional include-eol)
  "Mark region as composed by Org Menu mode.

START and END are positions (integers or markers) specifying the
region.  If the optional argument INCLUDE-EOL is non-nil, and END
extends to the end of the line, the region will be decomposed
even when point is immediately after the line, much like when
setting ‘prettify-symbols-unprettify-at-point’ to ‘right-edge’."
  (add-text-properties start end
                       `(org-menu-region ,(list start end)))
  (when include-eol
    (add-text-properties start end '(org-menu-include-eol t))))

(defun org-menu--unmark (start end)
  "Remove markers set by Org Menu mode in region.

START and END are positions (integers or markers)
specifying the region."
  (remove-text-properties start end
                          org-menu--extra-props))

(defun org-menu--update-region ()
  "Update the active region.
If point left the currently active region, update internal
variables and notify font-lock."
  (let ((start (org-menu--active-region-start))
        (end (org-menu--active-region-end)))
    (when (org-menu--outside-active-region-p)
      ;; We left the region, it's no longer active.
      (setq org-menu--active-region nil)
      ;; Let font-lock recompose the region.
      (font-lock-flush start end)))
  ;; TODO: Check what happens when you don't update but stay in the
  ;; region.
  (when-let ((new-region (org-menu--set-active-region)))
    (with-silent-modifications
      (apply #'decompose-region new-region))))

(defun org-menu--remove-props ()
  "Remove all references to Org Menu related text properties in buffer."
  (dolist (prop org-menu--extra-props)
    (setq font-lock-extra-managed-props
          (delq prop font-lock-extra-managed-props)))
  (org-menu--unmark (point-min) (point-max)))


;;;; Menu Icons and Fontification
;; Here we define the higher level routines dealing with the actual
;; composition of the menus.

(defun org-menu--get-src-icon ()
  "Return the desired menu icon for the current Org source code block.

This function returns the desired icon from ‘org-menu-src-alist’,
if specified.  If the selected language has no user-defined icon,
‘org-menu-src-default’ is used instead."
  (let ((lang (match-string 2)))
    (or (cdr (assoc-string lang org-menu-src-alist t))
        org-menu-src-default)))


;;; Fontification

(defun org-menu--prettify-src-begin ()
  "Prettify the delimiter of an Org source code block.

The menu is unprettified automatically when the user is working
on that line."
  (let ((start (match-beginning 0))
        (delim-beg (match-beginning 1))
        (delim-end (match-end 1))
        (lang-beg (match-beginning 2))
        (rest-beg (match-beginning 3))
        (end (match-end 0)))
    (cond
     ;; A match at point?  Throw all composition out the window.
     ((org-menu--active-region-p delim-beg end)
      (decompose-region start end)
      (org-menu--unmark start end))
     (t
      (compose-region delim-beg delim-end (org-menu--get-src-icon))
      (compose-region delim-end lang-beg ?\s)
      (when (< rest-beg end)
        (compose-region rest-beg end org-menu-symbol))
      ;; Bolt text props onto region.
      (org-menu--mark-composed delim-beg end))))
  nil)


;;; Predicates

(defun org-menu-in-src-block-p ()
  "Return t if point is in an Org source code block."
  (save-match-data
    (when (memq
           (org-element-type (org-element-context))
           '(inline-src-block src-block))
      t)))


;;; Font Lock

;; TODO: Make matcher a case-insensitive function instead of a regex.
(defvar org-menu--src-regexp
  (concat "^\\(?:[ \t]*\\)" ;; indentation
          "\\(?1:#\\+\\(begin_src\\|BEGIN_SRC\\)\\)"  ;; delimiter
          "\\(?:[ \t]+\\)" ;; garbage
          "\\(?2:\\S-+\\)" ;; language name
          "[ \t]?\\(?3:.*\\)$") ;; rest
  "Regular expression used to identify Org source code blocks.")

(defvar-local org-menu--font-lock-keywords nil)
(defun org-menu--update-font-lock-keywords ()
  "Set ‘org-menu--font-lock-keywords’ to reflect current settings.
You should not call this function to avoid confusing this mode's
cleanup routines."
  (setq org-menu--font-lock-keywords
        `(;; SRC blocks are special, and deserve extra fanciness.
          (,org-menu--src-regexp
           (0 (org-menu--prettify-src-begin)))
          ;; TODO: more later.
          )))

(defun org-menu--fontify-buffer ()
  "Fontify the buffer."
  (when font-lock-mode
    (save-restriction
      (widen)
      (font-lock-ensure)
      (font-lock-flush))))

;;; Mode commands
;;;###autoload
(define-minor-mode org-menu-mode
  "Toggle Org Menu mode.
With a prefix argument ARG, enable Org Menu mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  nil " >_" nil
  :group 'org-menu
  :require 'org
  (cond
   ;; Bail if Org is not enabled.
   ((and org-menu-mode
         (not (derived-mode-p 'org-mode)))
    (message "Org mode is not enabled in this buffer.")
    (org-menu-mode 0))
   ;; Set up Org Menu mode
   (org-menu-mode
    ;; Prepare font-lock
    (font-lock-remove-keywords nil org-menu--font-lock-keywords)
    (org-menu--update-font-lock-keywords)
    (font-lock-add-keywords nil org-menu--font-lock-keywords 'append)
    (setq-local font-lock-extra-managed-props
                (append font-lock-extra-managed-props org-menu--extra-props))
    ;; Trigger interactive updates
    (add-hook 'post-command-hook
              #'org-menu--update-region nil t)
    (org-menu--fontify-buffer))
   ;; Clean up remaining variables.
   (t
    (font-lock-remove-keywords nil org-menu--font-lock-keywords)
    (org-menu--remove-props)
    (remove-hook 'post-command-hook #'org-menu--update-region t)
    (setq org-menu--font-lock-keywords
          (default-value 'org-menu--font-lock-keywords))
    (org-menu--fontify-buffer))))


(provide 'org-menu)
;;; org-menu.el ends here
