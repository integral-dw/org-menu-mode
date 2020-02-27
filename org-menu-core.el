;;; org-menu-core.el --- Essential wrappers for interactive font locking for Org Menu  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  D. Williams

;; Author: D. Williams <d.williams@posteo.net>
;; Keywords: faces, outlines, extensions

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

;; ATTENTION: I am not sure whether to make this package really a
;; multi-file one in the long run, but for now I will separate the
;; bits for the sake of gaining an overview.

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



(provide 'org-menu-core)
;;; org-menu-core.el ends here
