;;; org-menu.el --- Hide verbose org syntax behind interactive menus  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  D. Williams, Rasmus Pank Roulund

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
;(require 'org-menu-core)

(defgroup org-menu nil
  "Hide Org syntax behind interactive menus."
  :group 'org-appearance)

;;;; Custom Variables

(defcustom org-menu-src-default ?🖉
  "Default menu icon for Org source code blocks."
  :group 'org-menu
  :type 'character)

(defcustom org-menu-char ?☰
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

(defcustom org-menu-src-end-char ?□ ;;■
  "Character used for an Org source code END delimiter."
  :group 'org-menu
  :type '(character
          :format "Display ‘#+END_SRC’ as: %v\n"))

(defcustom org-menu-begin-default ?✍
  "Default icon for generic #+BEGIN_ Org blocks delimiters.
This character is used if ‘org-menu-get-begin-character’ finds no
appropriate entry in ‘org-menu-delimiter-alist’."
  :group 'org-menu
  :type 'character)

(defcustom org-menu-end-default org-menu-src-end-char
  "Default icon for generic #+END_ Org blocks delimiters.
This character is used if ‘org-menu-get-end-character’ finds no
appropriate entry in ‘org-menu-delimiter-alist’."
  :group 'org-menu
  :type 'character)


(defcustom org-menu-delimiter-alist
  `(,(list 'quote ?🙶 ?🙷) ;; stop highlighting this
    ;; 🪶 will be a feather/quill in a future update (supposedly)
    (verse ?¶ ?⁂))
  "Alist associating block types with their delimiter characters.

Elements should be of the form:
  (KEY . (BEGIN-DELIM END-DELIM))

Each KEY should be a symbol specifying the name of the block.
Each value should be a list of two characters, BEGIN-DELIM being
used to display the #+BEGIN_ delimter, END-DELIM being used to
display the #+END_ delimiter."
  :group 'org-menu
  :type
  `(alist
    :key-type (symbol :format "Block type: %v\n")
    :value-type
    (list :format "Delimiter list:\n%v"
          (character :format "‘#+BEGIN_’ delimiter: %v\n"
                     :value ,org-menu-begin-default)
          (character :format "‘#+END_’ delimiter: %v\n"
                     :value ,org-menu-end-default))))

;;; Other Variables

(defvar org-menu-additional-keywords nil
  "Additional font-lock keywords to be managed by Org Menu mode.")


;;;; Text Properties and Manipulation
;; Here we define the lower level routines operating directly on text
;; properties.

(defvar org-menu--extra-props
  '(org-menu-region org-menu-right-edge)
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
  (let* ((pos (point))
         (new-region (get-text-property pos 'org-menu-region))
         (right-edge nil))
    ;; If there's no region at point, check the character before.
    (unless (or new-region (bobp))
      (setq pos (1- pos))
      (when (setq right-edge ;; Is it a right-edge region?
                  (get-text-property pos 'org-menu-right-edge))
        (setq new-region (get-text-property pos 'org-menu-region)
              right-edge t)))
    (when new-region
      (setq org-menu--active-region
            `(,@new-region right-edge ,right-edge)))
    new-region))

(defun org-menu--get-prop (property)
  "Return the value of PROPERY stored in the active region.

PROPERTY should be an optional argument name of the function
‘org-menu--mark’, as a symbol."
  (plist-get (cddr org-menu--active-region) property))

;;; Active Region Predicates
(defun org-menu--active-region-p (start end)
  "Return t if the region START...END is active.
If the region has overlap with the active region, treat the whole
region as active.  If there is no active region, return nil."
  (and org-menu--active-region
       ;; [a,b] and [c,d] overlap if and only if
       ;; a <= d and b >= c
       (<= start (org-menu--active-region-end))
       (>= end (org-menu--active-region-start))))

(defun org-menu--left-active-region-p ()
  "Return t if point left the active region.
If there is no active region, return nil."
  (let ((start (org-menu--active-region-start))
        (end (org-menu--active-region-end))
        (pos (point)))
    (and org-menu--active-region
         (not (<= start pos end)))))

;;; Manipulating the Region

(defun org-menu--mark (start end &optional right-edge)
  "Mark region as composed by Org Menu mode.

START and END are positions (integers or markers) specifying the
region.

If the optional argument RIGHT-EDGE is non-nil, the region will
be decomposed even when point is immediately after the match,
much like when setting ‘prettify-symbols-unprettify-at-point’ to
‘right-edge’.  The only exception to this behavior occurs when
the right edge belongs to another marked region."
  (add-text-properties start end
                       `(org-menu-region ,(list start end)))
  (when right-edge
    (add-text-properties start end '(org-menu-right-edge t))))

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
    (when org-menu--active-region
      (org-menu--fontify-buffer start end))
    (when (org-menu--left-active-region-p)
      ;; We left the region, it's no longer active.
      (setq org-menu--active-region nil)
      ;; Let font-lock recompose the region immediately.
      (org-menu--fontify-buffer start end)))

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

(defun org-menu-get-begin-character (block-type)
  "Return the specified +#BEGIN_BLOCK-TYPE replacement char."
  (or (elt (assq block-type org-menu-delimiter-alist) 1)
      org-menu-begin-default))

(defun org-menu-get-end-character (block-type)
  "Return the specified +#END_BLOCK-TYPE replacement char."
  (or (elt (assq block-type org-menu-delimiter-alist) 2)
      org-menu-end-default))

;;; Fontification

(defun org-menu--prettify-src-begin ()
  "Prettify the BEGIN delimiter of an Org source code block.

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
      (decompose-region start end))
     (t
      (compose-region delim-beg delim-end (org-menu--get-src-icon))
      (compose-region delim-end lang-beg ?\s)
      (when (< rest-beg end)
        (compose-region rest-beg end org-menu-char))))
    ;; Bolt text props onto region.
    (org-menu--mark delim-beg end t))
  nil)

;; TODO: handle dynamic blocks



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
  "Regular expression used to identify Org source code blocks.
This regex only matches the BEGIN_SRC delimiter.")

(defvar org-menu--src-end-regexp
  (concat "^\\(?:[ \t]*\\)"
          "\\(?1:#\\+\\(end_src\\|END_SRC\\)\\)")
  "Regular expression used to identify Org source code blocks.
This regex only matches the END_SRC delimiter.")


(defvar-local org-menu--font-lock-keywords nil)
(defun org-menu--update-font-lock-keywords ()
  "Set ‘org-menu--font-lock-keywords’ to reflect current settings.
You should not call this function to avoid confusing this mode's
cleanup routines."
  (setq org-menu--font-lock-keywords
        `(;; SRC blocks are special, and deserve extra fanciness.
          (,org-menu--src-regexp
           (0 (org-menu--prettify-src-begin)))
          (,org-menu--src-end-regexp
           (0 (org-menu-prettify-simple-delim org-menu-src-end-char)))
          ,@(org-menu-simple-delim-keywords 'quote)
          ;; TODO: more later.
          )))

(defun org-menu--fontify-buffer (&optional start end)
  "Fontify the buffer in region START...END.
If the region is not specified, it defaults to the entire
accessible region of the buffer."
  (when font-lock-mode
    (save-restriction
      (widen)
      (font-lock-flush start end)
      (font-lock-ensure start end))))

;;; Mode commands
;;;###autoload
(define-minor-mode org-menu-mode
  "Toggle Org Menu mode.
With a prefix argument ARG, enable Org Menu mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  nil " Menu" nil
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
