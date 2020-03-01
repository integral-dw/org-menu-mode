;;; org-menu-simple.el --- An API for prettifying "simple" Org blocks  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  D. Williams

;; Author: D. Williams <d.williams@posteo.net>
;; Keywords: faces, extensions, outlines
;; Version: 0
;; Homepage: https://github.com/integral-dw/org-menu-mode
;; Package-Requires: ((org "9.1.9") (emacs "26.2"))

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

;; This file defines a set of font-lock wrappers allowing you to
;; quickly add Org Menu support for simple Org blocks not specified in
;; the main package.  A "simple Org block" is one that is neither a
;; source code ("SRC"), an export nor a dynamic block, each of which
;; requires additional hacking.

;; To add, for example, support for EXAMPLE blocks (behaving like the
;; other blocks defined by Org Menu), all you have to do is:

;; (setq org-menu-additional-keywords
;;       `(,@(org-menu-simple-delim-keywords 'example)
;;         ;; more keywords here
;;         ...))

;; To control which characters are used to visualize #+BEGIN_EXAMPLE
;; and #+END_EXAMPLE, customize ‘org-menu-delimiter-alist’.

;; To mess with individual bits of the font-locking process, see the
;; functions below.  In contrast to most of the package, these are
;; considered "public", and serve as conveniences or templates for
;; further hacking.

;;; Code:

(declare-function org-menu-active-region-p "org-menu" (start end))
(declare-function org-menu-mark "org-menu" (start end &optional right-edge))
(defvar org-menu-char)

;;; Complete Keywords
(defun org-menu-simple-delim-keywords (type)
  "Return a list of ‘font-lock-keywords’ entries for Org TYPE blocks.

TYPE, a lowercase symbol, should be the name of the type of Org
block to fontify within Org Menu’s framework.  To add, for
example, support for EXAMPLE blocks (behaving like the other
blocks defined by Org Menu) the following code to set
‘org-menu-additional-keywords’ is sufficient:

  (setq org-menu-additional-keywords
        `(,@(org-menu-simple-delim-keywords 'example)))

See ‘org-menu-delimiter-alist’ for how to control the characters
used to display #+BEGIN_ and #+END_ delimiters."
  `((,(org-menu-simple-begin-matcher (symbol-name type))
     (0 (org-menu-prettify-simple-delim (quote ,type))))
    (,(org-menu-simple-begin-matcher (symbol-name type))
     (0 (org-menu-prettify-simple-delim (quote ,type))))))


;;; Keyword Matchers
(defun org-menu-simple-begin-matcher (block-name)
  "Return a function matching the #+BEGIN_ delimiter of a simple Org block.
The delimiter matched by the returned function is #+BEGIN_BLOCK-NAME.

The returned function takes a single argument LIMIT, and serves
as a ‘font-lock-keywords’ MATCHER argument, which see.

The matcher provides two SUBEXPressions, 1 being the delimiter
itself (without indentation) and 2 being the rest of the line."
  (let ((matcher
         (concat "^\\(?:[ \t]*\\)" ;; indentation
                 "\\(?1:#\\+begin_" block-name "\\)" ;; delimiter
                 "\\(?2:[ \t].*\\)?$"))) ;; parameters or data
    (lambda (limit)
      (let ((case-fold-search t))
        (re-search-forward matcher limit t)))))

(defun org-menu-simple-end-matcher (block-name)
  "Return a function matching the #+END_ delimiter of a simple Org block.
The delimiter matched by the returned function is #+END_BLOCK-NAME.

The returned function takes a single argument LIMIT, and serves
as a ‘font-lock-keywords’ MATCHER argument, which see.

The matcher provides the SUBEXPression 1, which is the delimiter
itself (without indentation)."
  (let ((case-fold-search t)
        (matcher
         (concat "^\\(?:[ \t]*\\)" ;; indentation
                 "\\(?1:#\\+END_" block-name "\\)" ;; delimiter
                 "\\(?:[ \t].*\\)$"))) ;; garbage
    (lambda (limit)
      (let ((case-fold-search t))
        (re-search-forward matcher limit 'move-point)))))

;;; FACENAME Functions / Prettifiers
(defun org-menu-simple-prettify-delim (delim-char &optional subexp nomenu)
  "Prettify a simple Org block delimiter.

Compose the delimiter as DELIM-CHAR.

The composed region is determined by the SUBEXP (default 1), see
‘match-beginning’ and ‘match-end’.  The delimiter is unprettified
automatically when the user is working on that line.

If the optional argument NOMENU is non-nil, the remainder of the
line is not prettified.  Otherwise, the rest of the line is
hidden behind a menu icon defined by ‘org-menu-char’.

The menu is unprettified automatically when the user is working
on that line."
  (let* ((subexp (or subexp 1))
         (start (match-beginning 0))
         (delim-beg (match-beginning subexp))
         (delim-end (match-end subexp))
         (rest-beg (1+ delim-end))
         (end (match-end 0)))
    (cond
     ((org-menu-active-region-p delim-beg end)
      (decompose-region start end))
     (t
      (compose-region delim-beg delim-end delim-char)
      (unless nomenu
        (when (< delim-end end)
          (compose-region delim-end rest-beg ?\s))
        (when (< rest-beg end)
          (compose-region rest-beg end org-menu-char)))))
    (org-menu-mark delim-beg end t))
  nil)

(provide 'org-menu-simple)
;;; org-menu-simple.el ends here
