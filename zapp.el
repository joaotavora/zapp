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
(require 'jsonrpc)


;;;; Utils of questionable utility
(cl-defmacro zapp--1seclater ((&optional repeat) &body body)
  "Silly util." (declare (indent 1))
  `(run-at-time 1 ,repeat (lambda () ,@body (force-mode-line-update))))

(defun zapp--error (format &rest args)
  (error "[zapp] %s" (apply #'format format args)))

(defun zapp--user-error (format &rest args)
  (user-error "[zapp] %s" (apply #'format format args)))

(defun zapp--message (format &rest args)
  (message "[zapp] %s" (apply #'format format args)))

(defun zapp--warn (format &rest args)
  (apply #'zapp--message (concat "(warning) " format) args)
  (let ((warning-minimum-level :error))
    (display-warning 'zapp (apply #'format format args) :warning)))


;;;; Data model
(defclass zapp--dap-server (jsonrpc-process-connection)
  ((status :initform "created" :initarg :status)
   (yow-timer :initform nil)
   (last-id :initform 0)
   (n-sent-notifs :initform 0)
   (buffers :initform nil)
   (capabilities :initform nil)
   (nickname :initform "?" :initarg :nickname)
   (autostart-inferior-process :initform nil)))

(cl-defmethod initialize-instance :after ((server zapp--dap-server) &optional _)
  (with-slots (name buffers nickname) server
    (cl-loop for spec in '("Threads" "Watch" "Breakpoints"
                           "Exceptions" "REPL" "Shell"
                           ("Global" . zapp--scope-vars-mode)
                           ("Local" . zapp--scope-vars-mode))
             for (nick . mode) = (ensure-list spec)
             do (push (with-current-buffer
                          (get-buffer-create
                           (format "*ZAPP %s for `%s'*" (downcase nick)
                                   nickname))
                        (let ((inhibit-read-only t))
                          (erase-buffer)
                          (funcall (or mode 'zapp--info-mode))
                          (setq-local zapp--buffer-nick nick))
                        (current-buffer))
                      buffers))))

(defvar zapp--buffer-nick "???")

(defvar zapp--current-server nil
  "Server instance for current debug session")

(defun zapp--current-server () zapp--current-server)

(defun zapp--current-server-or-lose ()
  (or (zapp--current-server)
      (error "No current debugging session.")))


;;; Stupid DAP base protocol
(cl-defmethod jsonrpc-convert-to-endpoint ((conn zapp--dap-server)
                                           message subtype)
  "Convert JSONRPC MESSAGE to DAP's JSONRPCesque format."
  (cl-destructuring-bind (&key method id error params
                               (result nil result-supplied-p))
      message
    (with-slots (last-id n-sent-notifs) conn
      (cond ((eq subtype 'notification)
             (cl-incf n-sent-notifs)
             `(:type "event"
                     :seq ,(+ last-id n-sent-notifs)
                     :event ,method
                     :body ,params))
            ((eq subtype 'request)
             `(:type "request"
                     :seq ,(+ (setq last-id id) n-sent-notifs)
                     :command ,method
                     ,@(when params `(:arguments ,params))))
            (t
             (cond (error
                    `(:type "response"
                            :seq ,(+ (setq last-id id) n-sent-notifs)
                            :request_seq ,last-id
                            :success :json-false
                            :message ,(plist-get error :message)
                            :body ,(plist-get error :data)))
                   (result-supplied-p
                    `(:type "response"
                            :seq ,(+ (setq last-id id) n-sent-notifs)
                            :request_seq ,last-id
                            :command ,method
                            :success t
                            ,@(and result `(:body ,result))))))))))

(cl-defmethod jsonrpc-convert-from-endpoint ((_conn zapp--dap-server) dap-message)
  "Convert JSONRPCesque DAP-MESSAGE to JSONRPC plist."
  (cl-destructuring-bind (&key type request_seq seq command arguments
                               event body success message &allow-other-keys)
      dap-message
    (cond ((string= type "event")
           `(:jsonrpc "2.0" :method ,event :params ,body))
          ((eq success :json-false)
           `(:jsonrpc "2.0" :id ,request_seq
                      :error ,(list :code 32600
                                    :message (or (plist-get body :error) message))))
          ((eq success t)
           `(:jsonrpc "2.0" :id ,request_seq :result ,body))
          (command
           `(:jsonrpc "2.0" :id ,seq :method ,command :params ,arguments)))))


;;;; Zapp info buffers
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

(defun zapp-setup-windows (server)
  (interactive (list (zapp--current-server-or-lose)))
  (cl-loop with display-buffer-alist = (zapp--info-dba)
           for b in (slot-value server 'buffers)
           for w = (display-buffer b)
           do
           (cl-pushnew b (window-parameter w 'zapp--siblings))
           (setf (window-parameter w 'zapp--info-tabbar) nil)
           (set-window-dedicated-p w 'dedicated)))



;;;; Mode-line horseplay
(defun zapp--mlf ()
  (with-slots (name status) zapp--current-server
    (format "%s: %s" name (propertize status 'face 'success))))

(defvar zapp--mlf `(:eval (zapp--mlf)))
(put 'zapp--mlf 'risky-local-variable t)

(add-to-list 'mode-line-misc-info
             `(zapp--current-server (" [" zapp--mlf  "] ")))


(defun zapp--on-shutdown (server)
  "Called when SERVER dead."
  (with-slots (buffers yow-timer) server
    (dolist (b buffers)
      (when-let ((w (get-buffer-window b)))
        (delete-window w)))
    (when yow-timer (cancel-timer yow-timer))
    (setq zapp--current-server nil)))

(defun zapp--cmd (contact) contact) ; maybe future tramp things

(defun zapp--contact-initargs (_class contact) ; make generic
  "Initargs for CLASS, subclass of `zapp--dap-server'."
  (let (nickname name)
    (cond ((and (stringp (car contact))
                (cl-find-if (lambda (x)
                              (or (eq x :autoport)
                                  (eq (car-safe x) :autoport)))
                            contact))
           (setq nickname (format "%s-autoport"
                                  (file-name-base (car contact))))
           (setq name (format "ZAPP (%s)" nickname))
           `(:process
             ,(jsonrpc-autoport-bootstrap nickname contact
                                          :connect-args '(:noquery t))
             :nickname ,nickname
             :name ,name))
          ((stringp (car contact))
           (setq nickname (file-name-base (car contact)))
           (setq name (format "ZAPP (%s)" nickname))
           `(:process
             ,(lambda (&optional _server)
                (make-process
                 :name name
                 :command (zapp--cmd contact)
                 :connection-type 'pipe
                 :coding 'utf-8-emacs-unix
                 :noquery t
                 :stderr (get-buffer-create (format "*%s stderr*" name))
                 :file-handler t))
             :name ,name
             :nickname ,nickname))
          (t (zapp--error "Unsupported contact %s" contact)))))

(defun zapp--kickoff-args (_class debuggee _contact) ; make generic
  "Plist for initial `:launch' or `:attach' request."
  (let (pid program)
    (cond ((and (= 1 (length debuggee))
                (> (setq pid (string-to-number (car debuggee))) 0))
           `(:zapp-method :attach :pid ,pid))
          ((and (stringp (car debuggee))
                (setq program (expand-file-name (car debuggee))))
           `(:zapp-method :launch :cwd ,(file-name-directory program)
                          :program ,program
                          :args ,(apply #'vector (cdr debuggee)))))))

(defun zapp--connect (contact kickoff-method kickoff-args class)
  (cl-flet ((spread (fn) (lambda (s m p) (apply fn s m p))))
    (let* ((s (apply
               #'make-instance
               class
               :notification-dispatcher (spread #'zapp-handle-notification)
               :request-dispatcher #'ignore
               :on-shutdown #'zapp--on-shutdown
               (zapp--contact-initargs class contact))))
      (with-slots (status yow-timer buffers capabilities nickname) s
        (setf zapp--current-server s)
        (setf status "contacting")
        (setf capabilities (jsonrpc-request s :initialize `(:adapterID ,nickname)))
        (jsonrpc-request s kickoff-method kickoff-args)
        (setf status "yay?")
        (zapp--1seclater ()
          (zapp-setup-windows s)
          (zapp--1seclater ()
            (setf status  "yay2?"
                  yow-timer
                  (zapp--1seclater (4)
                    (dolist (b buffers)
                      (with-current-buffer b
                        (save-excursion
                          (let ((inhibit-read-only t)) (yow t)))))))))))))


;;;; Command-parsing heroics
(defvar zapp-command-history nil)

(defun zapp--parse-command ()
  (let* (separator
         (unquoting-read
          (lambda ()
            (skip-chars-forward " \t\n")
            (let ((beg (point)))
              (cond ((eq (char-after) ?\")
                     (list (read (current-buffer)) beg (point)))
                    ((char-after)
                     (list (buffer-substring-no-properties
                            beg (+ beg (skip-chars-forward "^ \t\n")))
                           beg (point) t))))))
         (contact
          (cl-loop until separator
                   for spec = (funcall unquoting-read)
                   while spec
                   for (tok b e simple-p) = spec
                   if (string-equal tok "--")
                   do (setq separator (cons b e))
                   else if (and simple-p (string-match ":autoport" tok))
                   collect
                   (let ((tok tok)) ; This bug is my longest relationship ever
                     (cons :autoport
                           (lambda (p)
                             (string-replace ":autoport"
                                             (number-to-string p)
                                             tok))))
                   else collect tok))
         (debuggee
          (cl-loop with first-kwarg = nil
                   until first-kwarg
                   for spec = (funcall unquoting-read)
                   while spec
                   for (tok b e simple-p) = spec
                   if (and simple-p (> (length tok) 1) (string-prefix-p ":" tok))
                   do (setq first-kwarg (cons b e))
                   (goto-char b)
                   else collect tok))
         (extra (cl-loop do (skip-chars-forward " \t\n")
                         while (not (eobp)) collect (read (current-buffer))))
         (class (zapp--guess-class contact extra))
         (kickoff-plist (zapp--kickoff-args class debuggee contact))
         (merged (cl-loop for (k v) on extra by #'cddr
                          do (setq kickoff-plist (plist-put kickoff-plist k v))
                          finally return kickoff-plist))
         (method (plist-get merged :zapp-method)))
    (cl-remf merged :zapp-method)
    (list separator contact method merged (zapp--guess-class contact extra))))

(defvar zapp--minibuffer-acf-overlay (make-overlay (point) (point)))

(defun zapp--minibuffer-acf (_beg _end _prelen)
  (save-excursion
    (goto-char (minibuffer-prompt-end))
    (pcase-let ((`((,bsep . ,esep) ,_contact ,method ,args ,class _)
                 (ignore-errors (zapp--parse-command))))
      (when bsep
        (add-text-properties bsep esep
                             '(display " →  " face minibuffer-prompt separator t)))
      (overlay-put
       zapp--minibuffer-acf-overlay
       'after-string
       (propertize (format " \ndebugger: %s\nmethod: %s\narguments: %S"
                           class method args)
                   'face 'minibuffer-prompt
                   'cursor 0))
      (move-overlay zapp--minibuffer-acf-overlay
                    (1- (point-max)) (point-max) (current-buffer)))))

(defun zapp--guess-class (_contact _extra) 'zapp--dap-server)

(defun zapp--interactive ()
  (let ((str
         (minibuffer-with-setup-hook
             (lambda ()
               (setq-local text-property-default-nonsticky
                           (append '((separator . t) (face . t))
                                   text-property-default-nonsticky))
               (add-hook 'after-change-functions #'zapp--minibuffer-acf nil t))
           (read-shell-command "[zapp] Command? " nil 'zapp-command-history))))
    (with-temp-buffer (save-excursion (insert str)) (cdr (zapp--parse-command)))))


;;;; Interactive fun
(defun zapp (contact method kickoff-args class)
  (interactive
   (let ((current-server (zapp--current-server)))
     (unless (or (null current-server) (y-or-n-p "\
[zapp] Shut down current connection before attempting new one?"))
       (zapp--user-error "Connection attempt aborted by user."))
     (prog1 (zapp--interactive)
       (when current-server (ignore-errors (zapp-shutdown current-server))))))
  (zapp--connect contact method kickoff-args class))

(defun zapp-shutdown (server &optional timeout preserve-buffers)
  (interactive (list (zapp--current-server-or-lose) nil current-prefix-arg))
  (zapp--message "Asking %s politely to terminate" (jsonrpc-name server))
  (unwind-protect
      (jsonrpc-request server :disconnect nil :timeout (or timeout 1.5))
    ;; Now ask jsonrpc.el to shut down the server.
    (jsonrpc-shutdown server (not preserve-buffers))
    (unless preserve-buffers
      (kill-buffer (jsonrpc-events-buffer server))
      (mapc #'kill-buffer (slot-value server 'buffers)))))

(defun zapp--nuke () (interactive) (setq zapp--current-server nil))


;;;; Handlers

(cl-defmethod zapp-handle-notification ((s zapp--dap-server) m
                                        &key &allow-other-keys)
  (jsonrpc--debug s "Unknown notification method: %s" m))

(cl-defmethod zapp-handle-notification ((s zapp--dap-server) (_m (eql initialized))
                                        &key &allow-other-keys)
  ;; lalala some stuff missing here
  (jsonrpc-request s :configurationDone nil))
