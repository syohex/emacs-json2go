;;; json2go.el --- json to go struct

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/
;; Version: 0.01
;; Package-Requires: ((cl-lib "0.5"))

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

;;; Code:

(require 'cl-lib)
(require 'json)

(defun json2go--validate-array-type (array)
  (let ((type (type-of (aref array 0))))
    (when (memq type '(integer float string))
      (cl-loop for elm across array
               always (eq type (type-of elm))))))

(defsubst json2go--array-type (array)
  (type-of (aref array 0)))

(defun json2go--tag (name)
  (format "`json:\"%s\"`" name))

(defun json2go--indent (level)
  (dotimes (_i level)
    (insert "\t")))

(defun json2go--convert (json level)
  (cl-loop for (keystr . val) in json
           for keystr = (symbol-name keystr)
           do
           (progn
             (json2go--indent level)
             (cl-typecase val
               (integer (insert (format "%s int" keystr)))
               (float (insert (format "%s float" keystr)))
               (string (insert (format "%s string" keystr)))
               (vector
                (unless (json2go--validate-array-type val)
                  (error "Every vector element should be same type"))
                (insert (format "%s []%s"
                                keystr (json2go--array-type val) (capitalize keystr))))
               (cons
                (insert (format "%s struct {\n" keystr))
                (json2go--convert val (1+ level))
                (json2go--indent level)
                (insert "}")))
             (insert " " (json2go--tag keystr) "\n"))))

;;;###autoload
(defun json2go (beg end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let* ((jsonstr (buffer-substring-no-properties beg end))
         (json (ignore-errors
                 (json-read-from-string jsonstr))))
    (unless json
      (error "Invalid JSON!!"))
    (unless (listp json)
      (error "JSON must be Object!!"))
    (with-current-buffer (get-buffer-create "json2go")
      (insert "type YourStruct struct {\n")
      (json2go--convert json 1)
      (insert "}\n")
      (pop-to-buffer (current-buffer)))))

(provide 'json2go)

;;; json2go.el ends here
