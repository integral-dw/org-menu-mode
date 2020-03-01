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
;;

;;; Code:

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


(provide 'org-menu-fl)
;;; org-menu-fl.el ends here
