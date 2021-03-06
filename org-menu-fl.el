;;; org-menu-fl.el --- Essential wrappers for interactive font locking for Org Menu  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  D. Williams

;; Author: D. Williams <d.williams@posteo.net>
;; Keywords: faces, outlines, extensions
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

;; This file contains all functions and variables that are "aware" of
;; text property manipulation.  In essence, this is a bastardized
;; version of ‘prettify-symbols-mode’, with a few mayor technical
;; differences making it impossible to use it.  See below for a
;; description of the thought process behind these routines.

;; The core motivation behind both Prettify Symbols and this package
;; component is the same: Having font-locked regions of text respond
;; to the position of point in real time.  The issue with that is two
;; fold:
;;
;; 1) Font-lock's value of point is not the one experienced by the
;;    user.  The way font-lock works based on regexp searches moves
;;    the point across the buffer.
;;
;; 2) Even if it knew the "true" value of point at the time of
;;    fontification, there is no reason for it to update an unchanged
;;    part of the buffer just because point changes position.
;;
;; Enter ‘post-command-hook’.  Unlike font-lock, this hook is executed
;; after each command, meaning it updates when point updates.
;; However, it is naturally not aware of the regions manipulated by
;; font-lock.
;;
;; Text properties are a straightforward way to establish
;; communication between the two.  First, font-lock adds metadata to
;; the text that needs an interactive response, which is extracted by
;; the hook from point.  The metadata read defines an "active region",
;; which will be forced to be updated by font-lock after each command
;; until point no longer resides in it.  Here is a graphic summarizing
;; the core scheme:
;;
;;       +----------------------calls--------------------------+
;;       V                                                     |
;; +-----------+         +-----------------+            +--------------+
;; | font-lock |--sets-->| text properties |--read by-->| post-command |
;; +-----------+         +-----------------+            +--------------+
;;       Λ                +---------------+                    |
;;       |                | active region |                    |
;;       +---influences---|     info      |<-------sets--------+
;;                        +---------------+

;;; Public API:
;; While most of the code here is not generally intended for
;; tinkering, there are a few "reliable" functions intended for use.
;; I will most likely incrementally change "private" functions to
;; "public" ones as I find uses cases for them outside of other
;; function definitions within this file.
;;
;; The names below are defined in the main file instead of here to
;; make package-lint happy.
;;
;; ‘org-menu-active-region-p’: Is <region> active?
;; ‘org-menu-mark’: Apply text properties to region.
;; ‘org-menu-unmark’: Remove all Org Menu text props.

;;; Code:

(require 'subr-x)

(declare-function org-menu--fontify-buffer
                  "org-menu" (&optional start end))

(defvar org-menu--extra-props)

;;;; Active Region
;;; Local State Variables
(defvar-local org-menu-fl--active-region nil
  "Holds the delimiters of the region composed by Org Menu at point.

If point does not reside in any composed region, it’s value is
nil.  Otherwise, it’s value is a list of the form:

  (START END . META-PLIST)

where START and END delimit the affected region.

META-PLIST is a property list holding additional information
about the current region.

This variable is necessary because the \"true\" position of point
is not accessible from within font-lock.")

(defvar-local org-menu-fl--point nil
  "Holds the last recorded value of point.

This variable is used if the specific cursor position is
relevant to a function called from within font-lock.")

;;; Getters and Setters for the Active Region
(defun org-menu-fl--active-region-start ()
  "Return the beginning of the active region."
  (car org-menu-fl--active-region))

(defun org-menu-fl--active-region-end ()
  "Return the end of the active region."
  (cadr org-menu-fl--active-region))

(defun org-menu-fl--recompute-region (pos)
  "Force font-lock to update the active region metadata near point.
The argument POS is the exact buffer position to extract text
properties from.  Return the new value of the text property
‘org-menu-region’ found at POS.

This function is used internally to account for stale region
information caused by significant changes to the buffer’s
contents since the last time font-lock updated the region’s
metadata."
  (org-menu--fontify-buffer (line-beginning-position)
                            (line-end-position))
  (get-text-property pos 'org-menu-region))

(defun org-menu-fl--set-active-region (&optional no-font-lock)
  "If point is in an Org Menu composed region, make it active.
If no region could be found, return nil.  Otherwise, return a
list of the form (START END), containing the delimiting points of
the region found.

Since the region information found at point may be outdated (for
example because of insertions since the last invocation of
font-lock), force font-lock to update the region’s metadata.

If the optional argument NO-FONT-LOCK is non-nil, assume the
region information at point to be unconditionally up to date.
This is useful to avoid recursive invocations of font-lock."
  (let* ((pos org-menu-fl--point)
         (new-region (get-text-property pos 'org-menu-region))
         (right-edge (get-text-property pos 'org-menu-right-edge)))
    ;; If there's no region at point, check the character before.
    (unless (or new-region (bobp))
      (setq pos (1- pos))
      (when (setq right-edge ;; Is it a right-edge region?
                  (get-text-property pos 'org-menu-right-edge))
        (setq new-region (get-text-property pos 'org-menu-region)
              right-edge t)))
    (cond
     (new-region
      (unless no-font-lock
        (setq new-region (org-menu-fl--recompute-region pos)))
      (setq org-menu-fl--active-region
            `(,@new-region right-edge ,right-edge)))
     (t
      (setq org-menu-fl--active-region nil)))
    new-region))

(defun org-menu-fl--get-prop (property)
  "Return the value of PROPERY stored in the active region.

PROPERTY should be an optional argument name of the function
‘org-menu-mark’, as a symbol."
  (plist-get (cddr org-menu-fl--active-region) property))


(defun org-menu-fl--update-point ()
  "Update the value of point accessed by font-lock internals."
  (setq org-menu-fl--point (point)))

;;ps also has the issue with prettyfying at point while typing.
(defun org-menu-fl--update-region ()
  "Update the active region.
If point left the currently active region, update internal
variables and notify font-lock."
  (org-menu-fl--update-point)
  (let ((start (org-menu-fl--active-region-start))
        (end (org-menu-fl--active-region-end)))
    (when org-menu-fl--active-region
      (unless (org-menu-fl--active-region-p start end)
        ;; We have left the region.
        ;; Let font-lock recompose the region immediately.
        (org-menu--fontify-buffer start end))))
  (org-menu-fl--set-active-region))

;;; Active Region Predicates
(defun org-menu-fl--active-region-p (start end)
  "Return t if the region START...END is active.
A region is considered active if the value of point (outside of
font-lock) resides within START...END.

Please do not use this function in your own code; use
‘org-menu-active-region-p’ instead."
  (<= start org-menu-fl--point end))



;;;; General Region Operations
(defun org-menu-fl--mark (start end &optional right-edge)
  "Mark region as composed by Org Menu mode.

START and END are positions (integers or markers) specifying the
region.

If the optional argument RIGHT-EDGE is non-nil, the region will
be decomposed even when point is immediately after the match,
much like when setting ‘prettify-symbols-unprettify-at-point’ to
‘right-edge’.  The only exception to this behavior occurs when
the right edge belongs to another marked region.

Please do not use this function in your own code; use
‘org-menu-mark’ instead."
  (add-text-properties
   start end
   `(org-menu-region ,(list start end)
                     org-menu-right-edge ,(and right-edge t))))

(defun org-menu-fl--unmark (start end)
  "Remove markers set by Org Menu mode in region.

START and END are positions (integers or markers)
specifying the region.

Please do not use this function in your own code; use
‘org-menu-unmark’ instead."
  (remove-text-properties start end
                          org-menu--extra-props))

(defun org-menu-fl--remove-props ()
  "Remove all references to Org Menu related text properties in buffer."
  (dolist (prop org-menu--extra-props)
    (setq font-lock-extra-managed-props
          (delq prop font-lock-extra-managed-props)))
  (org-menu-fl--unmark (point-min) (point-max)))

(provide 'org-menu-fl)
;;; org-menu-fl.el ends here
