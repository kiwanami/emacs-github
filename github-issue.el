;;; github-issue.el --- github issue viewer

;; Copyright (C) 2014  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Keywords: github
;; Package-Requires: ((request-deferred "0.2.0") (ctable "0.1.2"))

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

;; set your application token with the `repo' scope to `gh:auth-token', and
;; M-x open-buffer-issues

;;; Code:

(require 'request-deferred)
(require 'ctable)

(defvar gh:auth-token "TOKEN-CODE"
  "your application token which you got from https://github.com/settings/applications")

;;;###autoload
(defun gh:open-buffer-issues ()
  (interactive)
  (deferred:$
    (gh:retrieve-issues-d)
    (deferred:nextc it
      (lambda (data) 
        (switch-to-buffer
         (gh:make-buffer-issues data))))))

(defun gh:retrieve-issues-d ()
  (deferred:$
    (request-deferred "https://api.github.com/issues" 
                      :params '(("filter" . "all"))
                      :headers `(("Authorization" . ,(concat "token " gh:auth-token)))
                      :parser 'json-read)
    (deferred:nextc it
      (lambda (d) (request-response-data d)))))

(defun gh:get-issue-columns (alst columns)
  (loop for c in columns
        collect
        (or
         (if (listp c)
             (loop with val = alst
                   for a in c do
                   (setq val
                         (cond
                          ((functionp a)
                           (funcall a val))
                          (t
                           (cdr (assq a val)))))
                   (unless val (return nil))
                   finally return val)
           (cdr (assq c alst))) "")))

(defun gh:on-click-issue ()
  (let* ((cp (ctbl:cp-get-component))
         (row (ctbl:cp-get-selected-data-row cp)))
    (when row
      (let* ((data (nth 7 row))
             (url (cdr (assq 'html_url data))))
        (browse-url url)))))

(defun gh:make-buffer-issues (json-data)
  (let* ((param 
          (copy-ctbl:param ctbl:default-rendering-param))
         (column-model
          (list
           (make-ctbl:cmodel
            :title "Repo" :align 'left :min-width 5 :max-width 20 )
           (make-ctbl:cmodel
            :title "State" :align 'left :min-width 5 :max-width 20 )
           (make-ctbl:cmodel
            :title "#No" :align 'right)
           (make-ctbl:cmodel
            :title "Comments" :align 'right)
           (make-ctbl:cmodel
            :title "PR" :align 'left :min-width 4 :max-width 4)
           (make-ctbl:cmodel
            :title "Title" :align 'left :min-width 10 :max-width 40 )
           (make-ctbl:cmodel
            :title "Updated" :align 'left )))
         (rows (loop for i across json-data
                     collect
                     (append
                      (gh:get-issue-columns
                       i '((repository name) state number comments
                           (pull_request url (lambda (x) (if x "X" "")))
                           title updated_at))
                      (list i))))
         (model
          (make-ctbl:model :column-model column-model :data rows :sort-state '(-7)))
         cp)
    (setf (ctbl:param-fixed-header param) t) ; set header parameters
    (setq cp (ctbl:create-table-component-buffer
              :width 'full :model model :param param))
    (ctbl:cp-add-click-hook cp 'gh:on-click-issue)
    (ctbl:cp-get-buffer cp)))

;; (eval-current-buffer)
;; (setq gh:auth-token "....")
;; (gh:open-buffer-issues)

(provide 'github-issue)
;;; github-issue.el ends here
