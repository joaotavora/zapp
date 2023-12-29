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


;;;; User tweakable stuff
(defgroup zapp nil "Zapp Branningan's DAP plugin" :prefix "zapp-"
  :group 'applications)

(defcustom zapp-show-command-hints t "Show `M-x zapp' hints" :type 'boolean)

(defcustom zapp--server-programs
  `(((python-mode python-ts-mode)
     :class zapp-debugpy
     :program "python -m debugpy.adapter"
     :kickoff-args (:type "executable" :justMyCode nil :showReturnValue t))
    ((c++-mode c++-ts-mode c-mode c-ts-mode)
     :class zapp-codelldb-cc
     :program "codelldb --port :autoport"
     :hint "codelldb"
     :kickoff-args (:type "lldb"))
    ((rust-mode rust-ts-mode)
     :class zapp-codelldb-rust
     :program "codelldb --port :autoport --settings {\"sourceLanguages\":[\"rust\"]}"
     :hint "codelldb.*sourceLanguages.*rust"
     :kickoff-args (:type "lldb")))
  "Hmmm"
  :type '(alist :key-type (repeat :tag "Major modes" symbol)
                :value-type
                (plist :key-type (symbol :tag "Keyword")
                       :options (((const :tag "Program suggestion" :program)
                                  (radio string (repeat :tag "List of strings" string)))
                                 ((const :tag "Class" :class) symbol)
                                 ((const :tag "Kickoff args" :kickoff-args)
                                  (choice
                                   (plist :key-type (symbol :tag "key")
                                          :value-type (sexp :tag "sexp"))
                                   (sexp)))
                                 ((const :tag "Nickname" :nickname) string)
                                 ((const :tag "Class-divination hint" :hint)
                                  regexp)))))

(defcustom zapp-preserved-windows '(:repl) "Hmmm"
  :type '(repeat :tag "Keyword" symbol))


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

(cl-defmacro zapp--when-live-buffer (buf &rest body)
  "Check BUF live, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf)) (if (buffer-live-p ,b) (with-current-buffer ,b ,@body)))))


;;;; Data model
(defclass zapp-generic-server (jsonrpc-process-connection)
  ((status :initform "created" :initarg :status)
   (yow-timer :initform nil)
   (last-id :initform 0)
   (n-sent-notifs :initform 0)
   (buffers :initform nil)
   (capabilities :initform nil)
   (nickname :initform "?" :initarg :nickname)
   (autostart-inferior-process :initform nil)
   ;; mostly for reconnection purposes
   (contact :initarg :contact)
   (kickoff-method :initarg :kickoff-method)
   (kickoff-args :initarg :kickoff-args)
   (class :initarg :class)))

(cl-defun zapp--buffer (name server &key mode wipe)
  "Get Zapp buffer for NAME, SERVER."
  (let ((name
         (cond ((keywordp name) (capitalize (substring (symbol-name name) 1)))
               ((symbolp name) (capitalize (symbol-name name)))
               (t name))))
    (with-current-buffer
        (get-buffer-create (format "*ZAPP %s for `%s'*" (downcase name)
                                   (slot-value server 'nickname)))
      (when (or mode wipe)
        (let ((inhibit-read-only t))
          (when wipe
            (erase-buffer))
          (when mode
            (funcall (or mode 'zapp--info-mode))))
        (setq-local zapp--buffer-nick name))
      (current-buffer))))

(cl-defmethod initialize-instance :after ((server zapp-generic-server) &optional _)
  (with-slots (name buffers nickname) server
    (cl-loop for spec in '("Threads" "Watch" "Breakpoints"
                           "Exceptions"
                           ("REPL" zapp--repl-mode t)
                           "Shell"
                           ("Global" zapp--scope-vars-mode)
                           ("Local" zapp--scope-vars-mode))
             for (nick mode preserve) = (ensure-list spec)
             do (push (zapp--buffer nick server
                                    :wipe (not preserve)
                                    :mode (or mode 'zapp--info-mode))
                      buffers))))

(defvar zapp--buffer-nick "???")

(defvar zapp--current-server nil
  "Server instance for current debug session")

(defun zapp--current-server () zapp--current-server)

(defun zapp--current-server-or-lose ()
  (or (zapp--current-server)
      (error "No current debugging session.")))


;;; Stupid DAP base protocol
(cl-defmethod jsonrpc-convert-to-endpoint ((conn zapp-generic-server)
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

(cl-defmethod jsonrpc-convert-from-endpoint ((_conn zapp-generic-server) dap-message)
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
           for existing = (get-buffer-window-list b)
           for w = (display-buffer b)
           do
           (cl-pushnew b (window-parameter w 'zapp--siblings))
           (setf (window-parameter w 'zapp--info-tabbar) nil)
           (set-window-dedicated-p w 'dedicated)
           (dolist (e (delq w existing)) (delete-window e))))

(defun zapp-teardown-windows (server)
  (interactive (list (zapp--current-server-or-lose)))
  (dolist (b (slot-value server 'buffers))
    (when-let ((w (and
                   (cl-loop for kw in zapp-preserved-windows
                            never (eq b (zapp--buffer kw server)))
                   (get-buffer-window b))))
      (delete-window w))))


;;;; Mode-line horseplay
(defun zapp--mlf ()
  (with-slots (name status) zapp--current-server
    (format "%s: %s" name (propertize status 'face 'success))))

(defvar zapp--mlf `(:eval (zapp--mlf)))
(put 'zapp--mlf 'risky-local-variable t)

(add-to-list 'mode-line-misc-info `(zapp--current-server (" [" zapp--mlf "] ")))


(defvar zapp-pre-connect-hook '())
(defvar zapp-connect-hook '(zapp-setup-windows))
(defvar zapp-disconnect-hook '(zapp-teardown-windows))

(defun zapp--on-shutdown (server)
  "Called when SERVER dead."
  (with-slots (buffers yow-timer) server
    (run-hook-wrapped 'zapp-disconnect-hook
                      (lambda (fn)
                        (condition-case-unless-debug _err
                            (funcall fn server)
                          (error))))
    (setq zapp--current-server nil)))

(defun zapp--cmd (contact) contact) ; maybe future tramp things

(defun zapp--contact-initargs (class contact) ; make generic?
  "Initargs for CLASS, subclass of `zapp-generic-server'."
  (let (nickname name)
    (cond ((and (stringp (car contact))
                (cl-find-if (lambda (x)
                              (or (eq x :autoport)
                                  (eq (car-safe x) :autoport)))
                            contact))
           (setq nickname (zapp--nickname class contact :autoport t))
           (setq name (format "ZAPP (%s)" nickname))
           `(:process
             ,(jsonrpc-autoport-bootstrap nickname contact
                                          :connect-args '(:noquery t))
             :nickname ,nickname
             :name ,name))
          ((stringp (car contact))
           (setq nickname (zapp--nickname class contact :autoport nil))
           (setq name (format "ZAPP (%s)" nickname))
           (unless (executable-find (car contact))
             (zapp--user-error "Can't find `%s'" (car contact)))
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

(cl-defun zapp--nickname (class contact &key autoport)
  (let ((s (or
            (cl-loop
             for (_ . plist) in zapp--server-programs
             for c = (plist-get plist :class)
             thereis (and (eq class c) (zapp--nickname-from-plist plist)))
            (file-name-base (car contact)))))
    (if autoport (format "%s-autoport" s) s)))

(defun zapp--kickoff-args (class debuggee _contact)
  "Plist for initial `:launch' or `:attach' request."
  (cl-loop with pid = nil with program = nil
           with base =
           (cond ((and (= 1 (length debuggee))
                       (> (setq pid (string-to-number (car debuggee))) 0))
                  `(:zapp-method :attach :pid ,pid))
                 ((and (stringp (car debuggee))
                       (setq program (expand-file-name (car debuggee))))
                  `(:zapp-method :launch :cwd ,(file-name-directory program)
                                 :program ,program
                                 :args ,(apply #'vector (cdr debuggee)))))
           for (_ . plist) in zapp--server-programs
           for c = (plist-get plist :class)
           when (eq class c)
           do (cl-loop for (k v) on (plist-get plist :kickoff-args) by #'cddr
                       do (setq base (plist-put base k v)))
           finally return base))

(defun zapp--connect (contact kickoff-method kickoff-args class)
  (cl-flet ((spread (fn) (lambda (s m p) (apply fn s m p))))
    (let* ((s (apply
               #'make-instance
               (if (find-class class) class
                 ;; let's not use (eval `(defclass ...)) mischief for now
                 'zapp-generic-server)
               :notification-dispatcher (spread #'zapp-handle-notification)
               :request-dispatcher #'ignore
               :on-shutdown #'zapp--on-shutdown
               :contact contact
               :class class
               :kickoff-method kickoff-method
               :kickoff-args kickoff-args
               (zapp--contact-initargs class contact))))
      (with-slots (status capabilities nickname) s
        (setf zapp--current-server s)
        (setf status "contacting")
        (run-hook-with-args 'zapp-pre-connect-hook s)
        (setf capabilities (jsonrpc-request s :initialize `(:adapterID ,nickname)))
        (jsonrpc-request s kickoff-method kickoff-args)
        (setf status "yay?")
        (run-hook-with-args 'zapp-connect-hook s)
        (zapp--message "Connected to '%s'!" nickname)))))


;;;; Command-parsing heroics
(defvar zapp-command-history nil)

(defun zapp--nickname-from-plist (plist)
  (or (plist-get plist :nickname)
      (when-let ((c (plist-get plist :class)) (n (symbol-name c)))
        (and (string-match "^zapp-" n) (substring n (match-end 0))))))

(defun zapp--guess-class (contact _extra)
  (cl-loop for (_ . plist)
           in (cl-sort (copy-sequence zapp--server-programs) #'>
                       :key (lambda (e)
                              (length (plist-get (cddr e) :hint))))
           for c = (plist-get plist :class)
           for hintre = (or (plist-get plist :hint)
                            (zapp--nickname-from-plist plist))
           thereis (and hintre
                        (string-match hintre (string-join
                                              (cl-remove-if-not #'stringp contact)
                                              " "))
                        c)
           finally return 'zapp-generic-server))

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
                   if (string-equal tok "->")
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
         (class (or (plist-get extra :zapp-class)
                    (zapp--guess-class contact extra)))
         (kickoff-plist (copy-sequence (zapp--kickoff-args class debuggee contact)))
         (merged (cl-loop for (k v) on extra by #'cddr
                          do (setq kickoff-plist (plist-put kickoff-plist k v))
                          finally return kickoff-plist))
         (method (plist-get merged :zapp-method)))
    (cl-remf merged :zapp-method)
    (cl-remf merged :zapp-class)
    (list separator contact method merged class)))

(defvar zapp--minibuffer-acf-overlay (make-overlay (point) (point)))

(defvar zapp--minibuffer-help-blurb
  (concat
   "\n\nSeparate debugger and debuggee with \"->\", extra keyword plist at end."
   "\n(toggle hints: C-c ?, guess: C-c C-c, navigate history: M-p, M-n or C-r)"))

(defvar zapp--origin-mode nil)

(defun zapp--minibuffer-acf (&rest _ignored)
  (save-excursion
    (goto-char (minibuffer-prompt-end))
    (pcase-let ((ov zapp--minibuffer-acf-overlay)
                (`((,bsep . ,esep) ,_contact ,method ,args ,class)
                 (ignore-errors (zapp--parse-command))))
      (set-text-properties (point) (point-max) nil)
      (when bsep
        (add-text-properties bsep esep
                             '(display "→ " face minibuffer-prompt separator t)))
      (if zapp-show-command-hints
          (let* ((fill-column (min (window-width)
                                   (* 2 fill-column)))
                 (pp-args (pp-to-string args)))
            (when (string-search "\n " pp-args)
              (setq pp-args (replace-regexp-in-string "\n[ ]+" "\n      " pp-args)))
            (with-temp-buffer
              (insert (propertize " \n" 'cursor 0) )
              (cl-loop
               for (k v)
               on `(:zapp-class ,class :zapp-method ,method ,@args) by #'cddr
               do (insert "\n"
                          (propertize (format "%s: "
                                              (substring (symbol-name k) 1))
                                      'face 'shadow)
                          (format "%S" v)))
              (insert (propertize zapp--minibuffer-help-blurb 'face 'shadow))
              (overlay-put ov 'after-string (buffer-string)))
            (move-overlay ov (1- (point-max)) (point-max) (current-buffer)))
        (delete-overlay ov)))))

(defun zapp--minibuffer-guess ()
  (interactive)
  (cl-loop with p = (minibuffer-prompt-end)
           with empty-p = (= p (point-max))
           for (m . plist) in zapp--server-programs
           for modes = (ensure-list m)
           when (provided-mode-derived-p zapp--origin-mode modes)
           do
           (save-excursion
             (goto-char p)
             (when (search-forward-regexp "\\s-->\\s-" nil t)
               (delete-region p (point)))
             (insert
              (string-join
               (ensure-list (plist-get plist :program))
               " ")
              " -> "))
           (when empty-p
             (goto-char (point-max))
             (save-excursion (insert "my-program -f 42")))
           and return nil
           finally do (zapp--user-error "Can't guess for `%s'!"
                                        zapp--origin-mode))
  (zapp--minibuffer-acf))

(defun zapp--minibuffer-toggle-hints ()
  (interactive)
  (setq zapp-show-command-hints (not zapp-show-command-hints))
  (zapp--minibuffer-acf))

(defun zapp--interactive ()
  (let* ((minibuffer-local-shell-command-map
          (let ((m (make-sparse-keymap)))
            (set-keymap-parent m minibuffer-local-shell-command-map)
            (define-key m (kbd "C-c ?") #'zapp--minibuffer-toggle-hints)
            (define-key m (kbd "C-c C-c") #'zapp--minibuffer-guess)
            m))
         (origin-mode major-mode)
         (str
          (minibuffer-with-setup-hook
              (lambda ()
                (setq-local text-property-default-nonsticky
                            (append '((separator . t) (face . t))
                                    text-property-default-nonsticky)
                            resize-mini-windows t
                            zapp--origin-mode origin-mode)
                (add-hook 'after-change-functions #'zapp--minibuffer-acf nil t)
                (zapp--minibuffer-acf))
            (read-shell-command "[zapp] Command: " nil 'zapp-command-history))))
    (with-temp-buffer (save-excursion (insert str)) (cdr (zapp--parse-command)))))


;;;; Interactive fun
(defun zapp (contact kickoff-method kickoff-args class)
  (interactive
   (let ((current-server (zapp--current-server)))
     (unless (or (null current-server) (y-or-n-p "\
[zapp] Shut down current connection before attempting new one?"))
       (zapp--user-error "Connection attempt aborted by user."))
     (prog1 (zapp--interactive)
       (when current-server (ignore-errors (zapp-shutdown current-server))))))
  (zapp--connect contact kickoff-method kickoff-args class))

(defun zapp-shutdown (server &optional timeout preserve-buffers)
  (interactive (list (zapp--current-server-or-lose) nil current-prefix-arg))
  (zapp--message "Asking %s politely to terminate" (jsonrpc-name server))
  (unwind-protect
      (jsonrpc-request server :disconnect nil :timeout (or timeout 1.5))
    ;; Now ask jsonrpc.el to shut down the server.
    (jsonrpc-shutdown server (not preserve-buffers))
    (unless preserve-buffers
      (kill-buffer (jsonrpc-events-buffer server))
      (dolist (b (slot-value server 'buffers))
        (when (cl-loop for kw in zapp-preserved-windows
                       never (eq b (zapp--buffer kw server)))
          (kill-buffer b))))))

(defun zapp-reconnect (server)
  (interactive (list (zapp--current-server-or-lose)))
  (when (jsonrpc-running-p server)
    (ignore-errors (zapp-shutdown server nil 'preserve-buffers)))
  (with-slots (contact kickoff-method kickoff-args class) server
    (zapp--connect contact kickoff-method kickoff-args class)))

(defun zapp--nuke () (interactive) (setq zapp--current-server nil))


;;;; Some handlers
(cl-defmethod zapp-handle-notification ((s zapp-generic-server) m
                                        &key &allow-other-keys)
  (jsonrpc--debug s "Unknown notification method: %s" m))

(cl-defmethod zapp-handle-notification ((s zapp-generic-server)
                                        (_m (eql initialized))
                                        &key &allow-other-keys)
  ;; lalala some stuff missing here
  (jsonrpc-request s :configurationDone nil))


;;;; Zapp REPL, mostly lifted from SLY
(require 'comint)

(add-hook 'zapp-pre-connect-hook 'zrepl//setup 50)
(add-hook 'zapp-disconnect-hook 'zrepl//teardown -50)

(defvar zrepl//omark nil "Output mark.")
(defvar zrepl//wmark nil "Waiting for prompt mark.")

(defun zrepl//proc ()
  "Dummy comint process for comint.el goodies."
  (get-buffer-process (current-buffer)))
(defun zrepl//pmark ()
  "Process process mark."
  (process-mark (zrepl//proc)))
(defun zrepl//spmark ()
  "Safe process mark."
  (if (zrepl//proc) (zrepl//pmark) (point-max)))

(defun zrepl//ocatch-up ()
  "Catch-up omark with pmark"
  (set-marker zrepl//omark (zrepl//pmark)))

(defun zrepl//syntax-propertize (beg end)
  (remove-text-properties beg end '(syntax-table nil))
  (let ((end (min end (zrepl//spmark)))
        (beg beg))
    (when (> end beg)
      (unless (nth 8 (syntax-ppss beg))
        (add-text-properties beg (1+ beg)
                             `(syntax-table ,(string-to-syntax "!"))))
      (add-text-properties (1- end) end
                           `(syntax-table ,(string-to-syntax "!"))))))

(defun zrepl//forward-sexp (n)
  (when (or (cl-plusp n) (/= (point) (zrepl//spmark)))
    (let ((forward-sexp-function nil)) (forward-sexp n))))

(defun zrepl//input-sender (_proc string)
  (let ((buf (current-buffer)))
    (zrepl//ocatch-up)
    (zapp--1seclater ()
      (z//when-live-buffer buf
        (zrepl//output
         (format "Yes, definitely thinking hard about '%s'" string))
        (zapp--1seclater ()
          (z//when-live-buffer buf
            (zrepl//insert-prompt (z//current-server-or-lose))))))))

(define-derived-mode zapp--repl-mode comint-mode "zrepl"
  (setq-local
   comint-use-prompt-regexp             nil
   comint-inhibit-carriage-motion       t
   comint-input-sender                  #'zrepl//input-sender
   comint-output-filter-functions       nil
   comint-input-filter-functions        nil
   comint-history-isearch               'dwim
   comint-input-ignoredups              t
   comint-prompt-read-only              t
   comint-process-echoes                nil
   comint-completion-addsuffix          ""
   comint-scroll-show-maximum-output    nil
   comint-scroll-to-bottom-on-input     nil
   comint-scroll-to-bottom-on-output    nil

   zrepl//omark                         (point-marker)
   zrepl//wmark                         (point-marker)

   mode-line-process                    nil
   parse-sexp-ignore-comments           t
   syntax-propertize-function           #'zrepl//syntax-propertize
   forward-sexp-function                #'zrepl//forward-sexp
   inhibit-field-text-motion            nil

   mode-line-format '(:eval (zapp--info-mlf))

   buffer-file-coding-system            'utf-8-unix)
  (set-marker-insertion-type zrepl//omark nil)
  (set-marker-insertion-type zrepl//wmark t))

(defun zrepl//prompt (server) (concat (slot-value server 'nickname) "> "))

(defun zrepl//ensure-newline () (unless (zerop (current-column)) (insert "\n")))

(defun zrepl//recenter ()
  (dolist (w (get-buffer-window-list))
    (with-selected-window w
      (recenter (min (count-screen-lines (window-start) (point) t)
                     (- (window-height) 3))))))

(cl-defun zrepl//call-committing-text (mark props fn)
  (let ((start-point (point)))
    (unwind-protect
        (let ((start-pos (marker-position mark))
              (inhibit-read-only t))
          (set-marker zrepl//wmark (point))
          (goto-char start-pos)
          (funcall fn)
          (set-marker mark (point))
          (add-text-properties
           start-pos (point)
           (append `(read-only t front-sticky (read-only))
                   props)))
      (goto-char zrepl//wmark)
      (unless (= (point) start-point)
        (zrepl//recenter)))))

(cl-defmacro zrepl//committing-text (mark-form (&key props)  &rest body)
  (declare (debug (sexp sexp &rest form)) (indent 2))
  `(zrepl//call-committing-text ,mark-form ,props (lambda () ,@body)))

(defun zrepl//insert-prompt (server)
  (zrepl//committing-text (zrepl//pmark)
      (:props '(font-lock-face
                comint-highlight-prompt
                ;; brainlessly cargo-culted from SLY
                inhibit-line-move-field-capture t
                field output
                rear-nonsticky (field
                                inhibit-line-move-field-capture
                                read-only
                                font-lock-face
                                insert-in-front-hooks)))
    (trace-values "Prompt insertion at" (zrepl//pmark))
    (zrepl//ensure-newline)
    (insert (slot-value server 'nickname) "> ")))

(cl-defun zrepl//setup (&optional (server (zapp--current-server-or-lose)))
  (with-current-buffer (zapp--buffer :repl server)
    (when-let ((p (zrepl//proc)))
      (delete-process p))
    (start-process (format "ZAPP (%s) dummy"
                           (slot-value server 'nickname))
                   (current-buffer)
                   nil)
    (set-process-query-on-exit-flag (zrepl//proc) nil)
    (set-process-sentinel (zrepl//proc) #'ignore)
    (add-hook 'kill-buffer-hook #'zrepl//kbh nil t)

    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (add-text-properties (point-min) (point) '(font-lock-face font-lock-comment-face))
      (set-marker (process-mark (get-buffer-process (current-buffer))) (point)))
    (zrepl//committing-text (zrepl//pmark)
        (:props '(font-lock-face comint-highlight-prompt))
      (insert "\n\n--- " (yow) " ---\n\n"))
    (zrepl//ocatch-up)
    (unless (get-buffer-window)
      (display-buffer (current-buffer)))
    (zrepl//insert-prompt server)))

(cl-defun zrepl//teardown (server &key (reason "disconnected"))
  (with-current-buffer (zapp--buffer :repl server)
    (remove-hook 'kill-buffer-hook 'zrepl//kbh t)
    (when-let ((p (zrepl//proc)))
      (zrepl//committing-text (zrepl//pmark)
          (:props '(font-lock-face comint-highlight-prompt))
        (zrepl//ensure-newline)
        (insert (format "--- %s" (or reason "REPL teardown")))
        (zrepl//ensure-newline))
      (zrepl//ocatch-up)
      (delete-process p))))

(defun zrepl//kbh () (zrepl//teardown (zapp--current-server-or-lose)))

(defface zrepl/output-face
  '((((class color) (background dark)) (:foreground "VioletRed1"))
    (((class color) (background light)) (:foreground "steel blue"))
    (t (:bold t :italic t)))
  "Zapp repl output face.")

(cl-defun zrepl//output (string &key category)
    (cond ((zrepl//proc)
         (zrepl//committing-text zrepl//omark
                   (:props `(font-lock-face
                             ,(pcase category
                                ("stdout" 'zrepl/output-face)
                                ("stderr" 'warning)
                                ((or "console" "output") 'zrepl/output-face))))
                 ;; no `comint-output-filter'
                 (insert string))
         (when (< (zrepl//pmark) zrepl//omark)
           (zrepl//committing-text (zrepl//pmark) ()
             (goto-char zrepl//omark)
             (zrepl//ensure-newline))))
        (t
         (zapp--warn "Ignoring some output for a non-ready REPL"))))

(cl-defmethod zapp-handle-notification ((s zapp-generic-server)
                                        (_m (eql output))
                                        &key output category &allow-other-keys)
  (unless (equal category "telemetry")
    (with-current-buffer (zapp--buffer :repl s)
      (zrepl//output output :category category))))


;;;; Completion


;;;; pinhead
(add-hook 'zapp-connect-hook 'zapp--pinhead 99)
(add-hook 'zapp-disconnect-hook 'zapp--pinhead-stop -99)

(defun zapp--pinhead (s)
  (with-slots (status yow-timer buffers) s
    (zapp--1seclater ()
      (setf status  "yay2?"
            yow-timer
            (zapp--1seclater (4)
              (dolist (b (cl-remove (zapp--buffer :repl s) buffers))
                (zapp--when-live-buffer b
                  (save-excursion
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert (yow)))))))))))

(defun zapp--pinhead-stop (s)
  (with-slots (yow-timer) s
    (when yow-timer (cancel-timer yow-timer))))


;; Local Variables:
;; read-symbol-shorthands: (("z//" . "zapp--") ("z/" . "zapp-") ("zrepl//" . "zapp--repl-") ("zrepl/" . "zapp-repl-"))
;; End:
