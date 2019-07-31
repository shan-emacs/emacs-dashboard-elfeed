;;; dashboard-elfeed.el --- Display a filtered search from Elfeed on Dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Khinshan Khan, all rights reserved

;; Author: Khinshan Khan <khinshan.khan@gmail.com>
;; URL: https://github.com/kkhan01/emacs-dashboard-elfeed
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3") (dashboard "1.7.0-SNAPSHOT") (elfeed "3.1.0"))

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

;; Display a filtered search from Elfeed in Dashboard widget.

;; See README.org for installation and usage.

;;; Code:

(require 'elfeed)
(require 'dashboard)

(defvar de/key ""
  "Specify the key for shortcut on dashboard.")

(defvar de/dashboard-search-filter "@6-months-ago +unread"
  "Specify the search.
Default value is \"@6-months-ago +unread\", it filters
from elfeed from 6 months ago and unread. Refer to README.org")

(defvar de/dashboard-results nil
  "Holder for transference from display to click.")

(defvar de/key-1 ""
  "Refer to de/key.
For the sake of having multiple filters.")

(defvar de/dashboard-search-filter-1 "@6-months-ago +unread"
  "Refer to de/dashboard-search-filter.
For the sake of having multiple filters.")

(defvar de/dashboard-results-1 nil
  "Refer to de/dashboard-results.
For the sake of having multiple filters.")

(defvar de/key-2 ""
  "Refer to de/key.
For the sake of having multiple filters.")

(defvar de/dashboard-search-filter-2 "@6-months-ago +unread"
  "Refer to de/dashboard-search-filter.
For the sake of having multiple filters.")

(defvar de/dashboard-results-2 nil
  "Refer to de/dashboard-results.
For the sake of having multiple filters.")

(defvar de/key-3 ""
  "Refer to de/key.
For the sake of having multiple filters.")

(defvar de/dashboard-search-filter-3 "@6-months-ago +unread"
  "Refer to de/dashboard-search-filter.
For the sake of having multiple filters.")

(defvar de/dashboard-results-3 nil
  "Refer to de/dashboard-results.
For the sake of having multiple filters.")

(defvar de/key-4 ""
  "Refer to de/key.
For the sake of having multiple filters.")

(defvar de/dashboard-search-filter-4 "@6-months-ago +unread"
  "Refer to de/dashboard-search-filter.
For the sake of having multiple filters.")

(defvar de/dashboard-results-4 nil
  "Refer to de/dashboard-results.
For the sake of having multiple filters.")

(defun de/pretty-entry (entry)
  "Return a string with ENTRY's important information in a nice format."
  (mapconcat 'identity
             `(,(cadr (elfeed-meta--plist (elfeed-entry-feed entry)))
               ,(elfeed-entry-title entry)
               ,(format-time-string "%F" (elfeed-entry-date entry))
               ,(mapconcat 'symbol-name (elfeed-entry-tags entry) ", "))
             " | "))

(defmacro de/elfeed-list (list-size search-filter res)
  "Return a list of size LIST-SIZE of the feeds from elfeed.
Will ensure the database is updated.
Filter is determined by SEARCH-FILTER and RES (which user shouldn't interact
 with)."
  `(progn
     (with-temp-buffer
       (elfeed-search-mode)
       (elfeed-db-load)
       (elfeed-update)
       (setq de/search (concat ,search-filter " #" (number-to-string (+ 5 ,list-size))))
       (elfeed-search-set-filter de/search)
       (setq de/entries elfeed-search-entries)
       (kill-buffer "*elfeed-search*"))
     (setq ,res (mapcar* 'cons (mapcar 'de/pretty-entry de/entries) de/entries))
     (mapcar 'de/pretty-entry de/entries)))

(defun de/elfeed-list-interact (arg res)
  "Display a single entry, ARG, from the list.
Filter is determined by RES (which user shouldn't interact with)."
  (let ((buffname (get-buffer-create "*elfeed-entry*")))
    (with-current-buffer buffname
      (elfeed-show-mode)
      (setq de/entry (assoc-default arg res))
      (elfeed-search-show-entry de/entry))
    (switch-to-buffer buffname)))

(defun dashboard-elfeed (list-size)
  "Add the elfeed functionality to dashboard.
Makes the list as long as LIST-SIZE."
  (dashboard-insert-section
   ;; widget title
   (concat "Elfeed: [" de/dashboard-search-filter "]")
   ;; list generated for dashboard
   (de/elfeed-list list-size de/dashboard-search-filter de/dashboard-results)
   list-size
   de/key
   ;; decide what to do when user clicks on item
   `(lambda (&rest ignore)
      (de/elfeed-list-interact ',el de/dashboard-results))
   ;; displays list in dashboard
   (format "%s" el)))

(defun dashboard-elfeed-1 (list-size)
  "Refer to dashboard-elfeed.
For the sake of having multiple filters.
ARGS: LIST-SIZE."
  (dashboard-insert-section
   (concat "Elfeed: [" de/dashboard-search-filter-1 "]")
   ;; list generated for dashboard
   (de/elfeed-list list-size de/dashboard-search-filter-1 de/dashboard-results-1)
   list-size
   de/key-1
   ;; decide what to do when user clicks on item
   `(lambda (&rest ignore)
      (de/elfeed-list-interact ',el de/dashboard-results-1))
   ;; displays list in dashboard
   (format "%s" el)))

(defun dashboard-elfeed-2 (list-size)
  "Refer to dashboard-elfeed.
For the sake of having multiple filters.
ARGS: LIST-SIZE."
  (dashboard-insert-section
   (concat "Elfeed: [" de/dashboard-search-filter-2 "]")
   ;; list generated for dashboard
   (de/elfeed-list list-size de/dashboard-search-filter-2 de/dashboard-results-2)
   list-size
   de/key-2
   ;; decide what to do when user clicks on item
   `(lambda (&rest ignore)
      (de/elfeed-list-interact ',el de/dashboard-results-2))
   ;; displays list in dashboard
   (format "%s" el)))

(defun dashboard-elfeed-3 (list-size)
  "Refer to dashboard-elfeed.
For the sake of having multiple filters.
ARGS: LIST-SIZE."
  (dashboard-insert-section
   (concat "Elfeed: [" de/dashboard-search-filter-3 "]")
   ;; list generated for dashboard
   (de/elfeed-list list-size de/dashboard-search-filter-3 de/dashboard-results-3)
   list-size
   de/key-3
   ;; decide what to do when user clicks on item
   `(lambda (&rest ignore)
      (de/elfeed-list-interact ',el de/dashboard-results-3))
   ;; displays list in dashboard
   (format "%s" el)))

(defun dashboard-elfeed-4 (list-size)
  "Refer to dashboard-elfeed.
For the sake of having multiple filters.
ARGS: LIST-SIZE."
  (dashboard-insert-section
   (concat "Elfeed: [" de/dashboard-search-filter-4 "]")
   ;; list generated for dashboard
   (de/elfeed-list list-size de/dashboard-search-filter-4 de/dashboard-results-4)
   list-size
   de/key-4
   ;; decide what to do when user clicks on item
   `(lambda (&rest ignore)
      (de/elfeed-list-interact ',el de/dashboard-results-4))
   ;; displays list in dashboard
   (format "%s" el)))

(provide 'dashboard-elfeed)
;;; dashboard-elfeed.el ends here
