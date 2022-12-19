(use-package vertico
  :config
  ;; Activa la extension =vertico-directory=
  (add-to-list 'load-path
	       (expand-file-name "straight/build/vertico/extensions"
				 straight-base-dir))
  (require 'vertico-directory)
  (vertico-mode))

(use-package savehist
  :after vertico
  :config
  (savehist-mode))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode))

(use-package all-the-icons-completion
  :after marginalia
  :config
  (all-the-icons-completion-mode))

(use-package orderless
  :after vertico
  :config
  (setq completion-styles '(orderless basic)))

(use-package consult
  :after vertico)

;; Este paquete se encarga de integrar =embark=y =consult= . Debe ser cargado antes de cargar =embark=
(use-package embark-consult
  :after consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark
  :config
  ;; La siguiente configuración hace que =embark= muestre un menu de
  ;; ayuda con cualquier prefijo de acorde. Es como un =which-key= bajo
  ;; demanda que se activa con =C-h=. Por ejemplo, estando en =org-mode=,
  ;; si se hace =C-c= no pasa nada (teniendo =which-key= desactivado),
  ;; pero si se agrega =C-h= se muestra un menú completo de las funciones
  ;; disponibles luego de ese prefijo. También funciona con acordes de
  ;; =evil=
  (setq prefix-help-command #'embark-prefix-help-command))

(provide 'mimacs-seleccion)
