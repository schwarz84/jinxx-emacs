;;; early-init.el --- Carga temprana ultra-optimizada para dcmacs  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Entorno básico
(defvar dcmacs-dir (file-name-directory (or load-file-name buffer-file-name)))

;; Desactivar cualquier código global de site-lisp (acelera + evita sorpresas)
(setq site-run-file nil)

;; Helper para setear/actualizar alist sin duplicar
(defun dcmacs--set-frame-opt (key val)
  (let ((cell (assq key default-frame-alist)))
    (if cell (setcdr cell val)
      (push (cons key val) default-frame-alist))))

;; Interfaz mínima (sin duplicar)
(dolist (pair '((menu-bar-lines         . 0)
                (tool-bar-lines         . 0)
                (vertical-scroll-bars   . nil)
                (horizontal-scroll-bars . nil)
                (internal-border-width  . 12)
                (fullscreen             . maximized)))
  (dcmacs--set-frame-opt (car pair) (cdr pair)))

(setq use-dialog-box nil)
(tooltip-mode -1)
(setq ring-bell-function #'ignore)

;; Rendimiento durante arranque
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Congela file-handlers y conserva copia para restaurar luego (en init.el)
(defvar dcmacs--file-name-handler-alist (copy-alist file-name-handler-alist))
(setq file-name-handler-alist nil
      inhibit-compacting-font-caches t)

;; Compilación nativa (Emacs 28/29+)
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (when (boundp 'native-comp-async-report-warnings-errors)
    (setq native-comp-async-report-warnings-errors 'silent))
  (when (boundp 'native-comp-deferred-compilation) ; Emacs 28
    (setq native-comp-deferred-compilation t))
  (when (boundp 'native-comp-jit-compilation)      ; Emacs 29+
    (setq native-comp-jit-compilation t)))

;; Paquetes y carga (package.el deshabilitado; straight se bootstrapea en core.org)
(setq package-enable-at-startup nil
      package-quickstart       nil
      load-prefer-newer        nil
      auto-window-vscroll      nil)

;; Reducciones de trabajo del redisplay y buffers de proceso grandes (mejor para LSP)
(setq read-process-output-max (* 4 1024 1024)
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      idle-update-delay 0.75)

;; Comportamiento de arranque
(setq inhibit-startup-message           t
      inhibit-startup-echo-area-message t
      inhibit-startup-screen            t
      inhibit-default-init              t
      initial-scratch-message           nil
      frame-inhibit-implied-resize      t
      frame-resize-pixelwise            t
      message-log-max                   1000)

;; Seguridad y warnings (no esconder demasiado)
(setq enable-local-variables :safe
      warning-minimum-level :warning
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;;; early-init.el ends here

