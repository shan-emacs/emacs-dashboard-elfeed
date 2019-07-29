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



;;;###autoload
(defun de/elfeed-search-filters (&optional search-filter-arg)
  "Wrapper for better searching.
Will prompt user for filter terms, or else use given SEARCH-FILTER-ARG.
Can be use in a hook too"
  (interactive)
  (unless search-filter-arg
    (setq search-filter-arg
          ;; ask for user input if need be
          (split-string (read-string "Enter your filter terms:") split-string-default-separators)))
  this is a slightly hacky solution to determine if time is provided
  (setq timewords '("-day-ago" "-days-ago" "-month-ago" "-months-ago" "-year-ago" "-years-ago"))
  (setq search-filter-arg (string-join
                           ;; normalize the search terms for elfeed filter function
                           (mapcar (apply-partially (lambda (arg)
                                                      (cond ((or (string-prefix-p "+" arg)
                                                                 (string-prefix-p "-" arg)
                                                                 (string-prefix-p "@" arg)
                                                                 (string-prefix-p "#" arg)) arg)
                                                            ;; determine if term is a time term
                                                            ((member 't
                                                                     (mapcar (apply-partially (lambda (timeword)
                                                                                                (string-suffix-p timeword arg))) timewords))
                                                             (concat "@" arg))
                                                            ;; heuristic that everything not explicitly excluded is included
                                                            (t (concat "+" arg)))
                                                      ))
                                   search-filter-arg)
                           " "))
  (elfeed-search-set-filter search-filter-arg))

(defun de/elfeed-list (list-size search-filter)
  "Return a list of size LIST-SIZE of the feeds from elfeed.
Will ensure the database is updated.
Filter is determined by SEARCH-FILTER (which user shouldn't interact with).
The elfeed buffers are purposefully not closed."
  (switch-to-buffer "*elfeed-search*")
  (elfeed-search-mode)
  (elfeed-db-load)
  (elfeed-update)
  (elfeed-search-set-filter search-filter)
  (setq de/dashboard-results elfeed-search-entries)
  (print de/dashboard-results)
  (switch-to-buffer "*dashboard*")
  de/dashboard-results)

(defun de/elfeed-list-interact (arg res)
  "Act on a single argument, ARG, from the list.
Filter is determined by RES (which user shouldn't interact with)."
  (switch-to-buffer "*elfeed-search*")
  (elfeed-show-mode)
  (elfeed-show-entry arg)
  (kill-buffer "*elfeed-search*"))

(defun dashboard-elfeed-template (key list-size search-filter res)
  "Template for a dashboard section.
ARGS: KEY LIST-SIZE SEARCH-FILTER RES."
  (dashboard-insert-section
   (concat "Elfeed: [" search-filter "]")
   ;; list generated for dashboard
   (de/elfeed-list list-size search-filter)
   list-size
   key
   `(lambda (&rest ignore)
      ;; decide what to do when user clicks on item
      (de/elfeed-list-interact (intern ,el))
      (dashboard-refresh-buffer) res)
   ;; displays list in dashboard
   (format "%s" el)))

(defun dashboard-elfeed (list-size)
  "Add the elfeed functionality to dashboard.
Makes the list as long as LIST-SIZE."
  (dashboard-elfeed-template de/key
                             list-size
                             de/dashboard-search-filter
                             de/dashboard-results))

(defun dashboard-elfeed-1 (list-size)
  "Refer to dashboard-elfeed.
For the sake of having multiple filters.
ARGS: LIST-SIZE."
  (dashboard-elfeed-template de/key-1
                             list-size
                             de/dashboard-search-filter-1
                             de/dashboard-results-1))

(defun dashboard-elfeed-2 (list-size)
  "Refer to dashboard-elfeed.
For the sake of having multiple filters.
ARGS: LIST-SIZE."
  (dashboard-elfeed-template de/key-2
                             list-size
                             de/dashboard-search-filter-2
                             de/dashboard-results-2))

(defun dashboard-elfeed-3 (list-size)
  "Refer to dashboard-elfeed.
For the sake of having multiple filters.
ARGS: LIST-SIZE."
  (dashboard-elfeed-template de/key-3
                             list-size
                             de/dashboard-search-filter-3
                             de/dashboard-results-3))

(defun dashboard-elfeed-4 (list-size)
  "Refer to dashboard-elfeed.
For the sake of having multiple filters.
ARGS: LIST-SIZE."
  (dashboard-elfeed-template de/key-4
                             list-size
                             de/dashboard-search-filter-4
                             de/dashboard-results-4))

(provide 'dashboard-elfeed)
;;; dashboard-elfeed.el ends here
