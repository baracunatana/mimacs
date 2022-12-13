(add-hook 'emacs-startup-hook
          (lambda ()
            (message "mimacs se cargó en %s."
                     (emacs-init-time))))

(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil))

(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                                    'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(add-to-list 'load-path (expand-file-name "modulos/" user-emacs-directory))

(defvar mimacs-archivo-configuracion-usuario "~/.config/mimacs/config.el" "Archivo de configuración del usuario .")

(when (file-exists-p crafted-config-file)
  (load mimacs-archivo-configuracion-usuario nil 'nomessage))
