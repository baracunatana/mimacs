(use-package evil
  :custom
  ;; Inicia en modo NORMAL por defecto en todos los modos
  (evil-default-state 'normal)
  ;; Para evitar conflictos con TAB en org-mode
  (evil-want-C-i-jump nil)
  :config
  ;; Iniciar compilation mode en estado normal
  (add-to-list 'evil-normal-state-modes 'compilation-mode) 
  ;; Empatar undo-tree con evil
  (with-eval-after-load 'undo-tree                         
    (customize-set-variable 'evil-undo-system 'undo-tree)
    (customize-set-variable 'evil-undo-system 'undo-redo))
  ;; activa =evil-mode= de forma global
  (evil-mode))

(provide 'mimacs-evil)
