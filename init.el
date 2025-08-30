;;; init.el --- Punto de entrada principal para jinxx_emacs -*- lexical-binding: t; -*-

(require 'subr-x)

;; Restauración post-arranque
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1)
            (when (boundp 'jinxx--file-name-handler-alist)
              (setq file-name-handler-alist jinxx--file-name-handler-alist
                    inhibit-compacting-font-caches nil))
            (message "Jinxx Emacs cargado en %.2fs"
                     (float-time (time-subtract after-init-time before-init-time)))))


;; custom.el separado
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; PATH desde la shell de login
(let* ((sh (or (getenv "SHELL") "/bin/bash"))
       (cmd (format "%s -lc 'printf %%s \"$PATH\"'" sh))
       (path (string-trim (with-temp-buffer
                            (call-process shell-file-name nil t nil shell-command-switch cmd)
                            (buffer-string)))))
  (when (and path (not (string-empty-p path)))
    (setenv "PATH" path)
    (setq exec-path (split-string path path-separator))))

;; Lista y carga modular
(defconst jinxx-modules
  '("core" "packages" "ui" "functions" "keybindings" "orgmode" "data")
  "Módulos que componen la configuración.")

(defun jinxx--module-path (base ext)
  (expand-file-name (format "config/%s.%s" base ext) user-emacs-directory))

(defun jinxx--file-newer-p (a b)
  "Si esta mas actualizado lo recarga"
  (let ((fa (and (file-exists-p a) (file-attributes a)))
        (fb (and (file-exists-p b) (file-attributes b))))
    (cond
     ((and fa fb)
      (time-less-p (file-attribute-modification-time fb)
                   (file-attribute-modification-time fa)))
     (fa t)
     (t nil))))

(defun jinxx--maybe-tangle-and-load (mod)
  "Carga el .el si está al día; si falta o está viejo, tanglea el .org y carga."
  (let* ((org (jinxx--module-path mod "org"))
         (el  (jinxx--module-path mod "el")))
    (cond
     ((and (file-exists-p el)
           (not (jinxx--file-newer-p org el)))
      (load el nil 'nomessage))
     ((file-exists-p org)
      (require 'ob-tangle) ;; usa Org incluido
      (ignore-errors (org-babel-tangle-file org el))
      (when (file-exists-p el)
        (load el nil 'nomessage)))
     (t
      (message "Módulo %s no encontrado; se omite" mod)))))

(mapc #'jinxx--maybe-tangle-and-load jinxx-modules)
(message "Configuración modular cargada (el-first; tangle on-demand)")

;; Ajustes generales
(setq ad-redefinition-action 'accept
      safe-local-variable-values '((mode . org) (lexical-binding . t)))

(add-hook 'after-init-hook
          (lambda ()
            (garbage-collect)
            (when (boundp 'native-comp-async-report-warnings-errors)
              (setq native-comp-async-report-warnings-errors 'warn))))

;; Servidor para emacsclient
(require 'server)
(unless (server-running-p) (server-start))

;; Git autenticacion
(setq auth-sources '("~/.authinfo.gpg"))

(provide 'init)
;;; init.el ends here

