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

(defvar de/dashboard-search-filter "@6-months-ago +unread"
  "Specify the search.
Default value is \"@6-months-ago +unread\", it filters
from elfeed from 6 months ago and unread. Refer to README.org")

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
  ;; this is a slightly hacky solution to determine if time is provided
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

(defun de/elfeed-list (list-size)
  "Return a list of size LIST-SIZE of the feeds from elfeed.
Will ensure the database is updated.
The elfeed buffers are purposefully not closed."
  (switch-to-buffer "*elfeed-search*")
  (elfeed-search-mode)
  (switch-to-buffer "*dashboard*")
  `("post1" "post2" "post3" "post4" "post5"))

(defun de/elfeed-list-interact (arg)
  "Act on a single argument, ARG, from the list."
  (switch-to-buffer "*elfeed-search*")
  (elfeed-search-mode)
  (print elfeed-search-entries))

(defun dashboard-elfeed (list-size)
  "Add the elfeed functionality to dashboard.
Makes the list as long as LIST-SIZE."
  (dashboard-insert-section
   "Elfeed:"
   ;; list generated for dashboard
   (de/elfeed-list list-size)
   list-size
   "b"
   `(lambda (&rest ignore)
      ;; decide what to do when user clicks on item
      (de/elfeed-list-interact (intern ,el))
      (dashboard-refresh-buffer))
   ;; displays list in dashboard
   (format "%s" el)))

(provide 'dashboard-elfeed)
;;; dashboard-elfeed.el ends here
