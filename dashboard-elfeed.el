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
      (setq de/entry (nth (cl-position arg (mapcar 'car res) :test 'equal) (mapcar 'cdr res)))
      (elfeed-search-show-entry de/entry))
    (switch-to-buffer buffname)))

(defun dashboard-elfeed-template (list-size key search-filter res)
  "Add the template dashboard widgets.
ARGS: KEY LIST-SIZE SEARCH-FILTER RES."
  (dashboard-insert-section
   (concat "Elfeed: [" de/dashboard-search-filter "]")
   ;; list generated for dashboard
   (de/elfeed-list list-size search-filter res)
   list-size
   key
   ;; decide what to do when user clicks on item
   `(lambda (&rest ignore)
      (de/elfeed-list-interact ',el res))
   ;; displays list in dashboard
   (format "%s" el)))

(defun dashboard-elfeed (list-size)
  "Add the elfeed functionality to dashboard.
Makes the list as long as LIST-SIZE."
  (dashboard-elfeed-template list-size
                             de/key
                             de/dashboard-search-filter
                             de/dashboard-results))



(provide 'dashboard-elfeed)
;;; dashboard-elfeed.el ends here
