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
(require 'subr-x)

(defgroup org-menu nil
  "Hide Org syntax behind interactive menus."
  :group 'org-appearance)

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

;;; Accessor Functions

(defun org-menu--get-src-icon ()
  "Return the desired menu icon for the current Org source code block.

This function returns the desired icon from ‘org-menu-src-alist’,
if specified.  If the selected language has no user-defined icon,
‘org-menu-src-default’ is used instead."
  (let ((lang (match-string 1)))
    (or (cdr (assoc-string lang org-menu-src-alist t))
        org-menu-src-default)))


;;; Text Properties and Manipulation

(defvar-local org-menu--active-region nil
  "Holds the delimiters of the region composed by Org Menu at point.

If point does not reside in any composed region, it’s value is
nil.  Otherwise, it’s value is a cons cell of the form:

  (START . END)

where START and END delimit the affected region.

This variable is necessary because the \"true\" position of point
is not accessible from within font-lock.

This variable is used internally by
‘org-menu--compose-region-p’.")

(defvar org-menu--extra-props
  '(org-menu-region)
  "List of text properties for Org Menu mode’s font lock internals.")

(defun org-menu--mark-composed (start end)
  "Mark region as composed by Org Menu mode.

START and END are positions (integers or markers) specifying the
region."
  (add-text-properties start end
                       `(org-menu-region ,(cons start end))))

(defun org-menu--unmark (start end)
  "Remove marks set by Org Menu mode in region.

START and END are positions (integers or markers)
specifying the region."
  (remove-text-properties start end
                          org-menu--extra-props))

(defun org-menu--update-region ()
  "Update the active region.
If point left the currently active region, "
  (let ((start (car org-menu--active-region))
        (end (cdr org-menu--active-region)))
    (when (and org-menu--active-region
               (not (<= start (point) end)))
      ;; We left the region, it's no longer active.
      (setq org-menu--active-region nil)
      ;; Let font-lock recompose the region.
      (font-lock-flush start end)))
  ;; TODO: Check what happens when you don't update when you don't
  ;; leave the region.
  (when-let* ((new-region (get-text-property (point) 'org-menu-region)))
    (setq org-menu--active-region new-region)
    (decompose-region (car new-region) (cdr new-region))))

(defun org-menu--remove-props ()
  "Remove all references to Org Menu related text properties in buffer."
  (dolist (prop org-menu--extra-props)
    (setq font-lock-extra-managed-props
          (delq prop font-lock-extra-managed-props)))
  (org-menu--unmark (point-min) (point-max)))


;;; Predicates

(defun org-menu-in-src-block-p ()
  "Return t if point is in an Org source code block."
  (save-match-data
    (when (memq
           (org-element-type (org-element-context))
           '(inline-src-block src-block))
      t)))

(defun org-menu--compose-region-p (start end)
  "Return t if region between START and END should be composed."
  (not (equal (cons start end)
              org-menu--active-region)))


;;; Fontification

(defun org-menu--prettify-src-begin ()
  "Prettify the delimiter of an Org source code block.

The menu is unprettified automatically when the user is working
on that line."
  (let ((start (match-beginning 0))
        (delim-beg (match-beginning 1))
        (lang-beg (match-beginning 2))
        (lang-end (match-end 2))
        (end (match-end 0)))
    (cond
     ((org-menu--compose-region-p delim-beg end)
      (compose-region src-beg src-end (org-menu--get-src-icon))
      (compose-region src-end lang-beg ?\s)
      (when (/= lang-end end)
        (compose-region lang-end end org-menu-symbol))
      ;; Bolt text props onto region.
      (org-menu--mark-composed start end)
     ;; A match at point?  Throw all composition out the window.
     (t
      (decompose-region start end)
      (org-menu--unmark start end)))))
  nil)


;;; Font Lock

;; TODO: Make matcher a case-insensitive function instead of a regex.
(defvar org-menu--src-regexp
  (concat "^\\(?:[ \t]*\\)" ;; indentation
          "\\(?1:#\\+\\(begin_src\\|BEGIN_SRC\\)\\)"  ;; delimiter
          "\\(?:[ \t]+\\)" ;; garbage
          "\\(?2:\\S-+\\)" ;; language name
          "\\(?3:.*\\)$") ;; rest
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
