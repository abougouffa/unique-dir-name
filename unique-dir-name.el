;;; unique-dir-name.el --- Keep unique names based on directories -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Abdelhak Bougouffa
;;
;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Maintainer: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Created: July 07, 2024
;; Modified: July 07, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/Symbol’s function definition is void: doom-call-process/unique-dir-name
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


;;; Unique name from directory

(defvar unique-dir-name-map-default (make-hash-table :test 'equal))
(defvar unique-dir-name-format "%s[%s]")

;;; Helpers

(defun unique-dir-name--get-dir-elements (dir)
  (butlast (reverse (file-name-split (directory-file-name (expand-file-name dir))))))

(defun unique-dir-name--unique-dir-elements (dir1 dir2)
  "Return unique elements of DIR1 and DIR2."
  (let* ((els1 (unique-dir-name--get-dir-elements dir1))
         (els2 (unique-dir-name--get-dir-elements dir2)))
    (while-let ((el1 (car els1))
                (el2 (car els2))
                (_ (string= el1 el2)))
      (pop els1) (pop els2))
    (cons els1 els2)))

;;; API

;;;###autoload
(cl-defun unique-dir-name-create-or-update (dir &key ((:map map-sym) 'unique-dir-name-map-default) ((:rename-fn rename-func) nil))
  "See `unique-dir-name-register'."
  (let* ((dir (expand-file-name dir))
         (name (file-name-nondirectory (directory-file-name (expand-file-name dir))))
         (unique-map (eval map-sym))
         (unique-name
          (cl-loop for other-path in (hash-table-keys unique-map)
                   with len-min = most-positive-fixnum
                   with len-max = most-negative-fixnum
                   with max-path = nil
                   collect
                   (let ((curr-element (gethash other-path unique-map)))
                     (when (and (not (string= dir other-path)) ; not the same dir
                                (string= name (alist-get 'dir-name curr-element))) ; have the same name
                       (cl-destructuring-bind (els1 . els2) (unique-dir-name--unique-dir-elements dir other-path)
                         (let ((len (length els1)))
                           (setq len-min (min len-min len))
                           (when (> len len-max)
                             (setq len-max len
                                   max-path els1))))))
                   finally return
                   (let ((s (string-join
                             (reverse (butlast max-path
                                               (- (length max-path)
                                                  (1+ (- len-max len-min)))))
                             "/")))
                     (if (string-empty-p s)
                         name
                       (format unique-dir-name-format name s))))))
    (let* ((old (gethash dir unique-map))
           (old-name (assoc 'unique-name old)))
      (when (and (functionp rename-func) (not (equal (cdr old-name) unique-name)))
        (ignore-errors (funcall rename-func (cdr old-name) unique-name)))
      (if old
          (setcdr old-name unique-name)
        (puthash dir `((dir-name . ,name) (unique-name . ,unique-name)) unique-map)))))

;;;###autoload
(cl-defun unique-dir-name-register (dir &key ((:map map-sym) 'unique-dir-name-map-default) ((:rename-fn rename-func) nil))
  "Make a unique name derived from DIR.
The :MAP is a symbol for the hash-table used to register the names, all
names will be renamed accordingly when needed.
The :RENAME-FN is a function of signature (OLD NEW), called before renaming
the hash-table elements."
  (append
   `((path . ,dir))
   (if-let* ((dir (expand-file-name dir))
             (name (file-name-nondirectory (directory-file-name (expand-file-name dir))))
             (unique-map (eval map-sym))
             (element (gethash dir unique-map)))
       element
     (puthash dir `((dir-name . ,name) (unique-name . ,name)) unique-map)
     (dolist (path (hash-table-keys unique-map)) ; Update all the names
       (unique-dir-name-create-or-update path :map map-sym :rename-fn rename-func))
     (gethash dir unique-map))))


(provide 'unique-dir-name)
;;; unique-dir-name.el ends here
