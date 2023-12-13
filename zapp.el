;;; zapp.el --- Zebugger Adapter Protocol Plugin -*- lexical-binding: t; -*-

;; Copyright (C) 2023  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: 

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

;;; Code:

(require 'cl-lib)
(require 'eieio)
(with-no-warnings (require 'yow))

(defvar zapp--mode-line-key [mode-line mouse-1])

(defun zapp--info-tabbar (w)
  "Compute tab string for window W, caching wisely."
  (or
   (window-parameter w 'zapp--info-tabbar)
   (setf (window-parameter w 'zapp--info-tabbar)
         (mapconcat
          (lambda (b)
            (apply
             #'propertize
             (buffer-local-value 'zapp--buffer-nick b)
             (if (eq b (window-buffer w))
                 `(face tab-bar-tab-group-current)
               `(face mode-line-inactive
                      help-echo ,(format "Go to '%s'" (buffer-name b))
                      mouse-face mode-line-highlight
                      keymap
                      ,(let ((m (make-sparse-keymap)))
                         (define-key
                          m zapp--mode-line-key
                          (lambda (&rest _event)
                            (interactive)
                            (set-window-buffer w b)
                            (setf (window-parameter w 'zapp--info-tabbar) nil)
                            (set-window-dedicated-p w 'dedicated)))
                         m)))))
          (window-parameter w 'zapp--siblings) " "))))

(defun zapp--info-mlf ()
  (let ((w (selected-window)))
    (if (window-dedicated-p w) (zapp--info-tabbar w)
      (default-value 'mode-line-format))))

(define-derived-mode zapp--info-mode special-mode ""
  :interactive nil
  (setq-local buffer-read-only t
              cursor-in-non-selected-windows nil
              mode-line-format '(:eval (zapp--info-mlf))
              header-line-format nil)
  (buffer-disable-undo))

(define-derived-mode zapp--scope-vars-mode zapp--info-mode "Scope"
  :interactive nil)

(defun zapp--info-dba ()
  "Compute a a suitable value of `display-buffer-alist'"
  (let ((fns '(display-buffer-in-side-window)))
    `(((derived-mode . zapp--scope-vars-mode)
       .
       (,fns . ((side . left) (slot . -1))))
      ("Threads" .     (,fns . ((side . left) (slot . -1))))
      ("Watch" .       (,fns . ((side . left) (slot . -1))))
      ("Breakpoints" . (,fns . ((side . left) (slot . 1))))
      ("Exceptions" .  (,fns . ((side . left) (slot . 1))))
      ("REPL" .        (,fns . ((side . bottom) (slot . 0))))
      ("Shell" .       (,fns . ((side . bottom) (slot . 1)))))))

(defclass zapp--dap-server ()
  ((name  :initarg :name)
   (status :initform "created")
   (timer :initform nil)
   (buffers :initform nil)))

(cl-defmethod initialize-instance :before ((server zapp--dap-server) &optional args)
  (cl-destructuring-bind (&key ((:name n))) args
    (unless n (error "name is required!"))
    (with-slots (name buffers) server
      (setq name n)
      (cl-loop for spec in '("Threads" "Watch" "Breakpoints"
                             "Exceptions" "REPL" "Shell"
                             ("Global" . zapp--scope-vars-mode)
                             ("Local" . zapp--scope-vars-mode))
               for (nick . mode) = (ensure-list spec)
               do (push (with-current-buffer
                           (get-buffer-create
                            (format "*ZAPP %s for %s*" (downcase nick)
                                    (slot-value server 'name)))
                         (let ((inhibit-read-only t))
                           (erase-buffer)
                           (funcall (or mode 'zapp--info-mode))
                           (setq-local zapp--buffer-nick nick))
                         (current-buffer))
                        buffers)))))

(defvar zapp--buffer-nick "???")

(defvar zapp--current-server nil
  "Server instance for current debug session")

(defvar zapp--current-server nil)

(defun zapp--current-server () zapp--current-server)

(defun zapp--current-server-or-lose ()
  (or (zapp--current-server)
      (error "No current debugging session.")))

(defun zapp-setup-windows (server)
  (interactive (list (zapp--current-server-or-lose)))
  (cl-loop with display-buffer-alist = (zapp--info-dba)
           for b in (slot-value server 'buffers)
           for w = (display-buffer b)
           do
           (cl-pushnew b (window-parameter w 'zapp--siblings))
           (setf (window-parameter w 'zapp--info-tabbar) nil)
           (set-window-dedicated-p w 'dedicated)))

(defun zapp--mlf ()
  (with-slots (name status) zapp--current-server
    (format "%s: %s" name (propertize status 'face 'success))))

(defvar zapp--mlf `(:eval (zapp--mlf)))
(put 'zapp--mlf 'risky-local-variable t)

(add-to-list 'mode-line-misc-info
             `(zapp--current-server (" [" zapp--mlf  "] ")))

(cl-defmacro zapp--1seclater ((&optional repeat) &body body)
  "Silly util." (declare (indent 1))
  `(run-at-time 1 ,repeat (lambda () ,@body (force-mode-line-update))))

(defun zapp (name)
  (interactive "sName this server: ")
  (if (zapp--current-server) (user-error "Quit current session first!"))
  (let ((s (make-instance 'zapp--dap-server :name name)))
    (with-slots (status timer buffers) s
      (setf zapp--current-server s status "connecting")
      (zapp--1seclater ()
        (setf status  "initializing")
        (zapp-setup-windows s)
        (zapp--1seclater ()
          (setf status  "connected"
                timer
                (zapp--1seclater (4)
                  (dolist (b buffers)
                    (with-current-buffer b
                      (save-excursion
                        (let ((inhibit-read-only t)) (yow t))))))))))))

(defun zapp-shutdown ()
  (interactive)
  (unless zapp--current-server
    (user-error "Nothing to shutdown"))
  (zapp--1seclater ()
    (dolist (b (slot-value zapp--current-server 'buffers))
      (when-let ((w (get-buffer-window b)))
        (delete-window w)))
    (cancel-timer (slot-value zapp--current-server 'timer))
    (setq zapp--current-server nil)))
