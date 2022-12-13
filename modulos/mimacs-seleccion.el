(straight-use-package 'vertico)
(require 'vertico)

(with-eval-after-load 'evil           ; Acordes en modo selección de vertico
  (define-key vertico-map (kbd "C-j") 'vertico-next)
  (define-key vertico-map (kbd "C-k") 'vertico-previous)
  (define-key vertico-map (kbd "M-h") 'vertico-directory-up))
(vertico-mode)

;;(setq prefix-help-command #'embark-prefix-help-command)

(provide 'mimacs-seleccion)
