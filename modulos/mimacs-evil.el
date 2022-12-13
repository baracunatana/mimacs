(straight-use-package 'evil)
(require 'evil)

(customize-set-variable evil-default-state 'normal)      ; iniciar en modo normal
(customize-set-variable evil-want-C-i-jump nil)          ; Para evitar conflictos con TAB en org-mode
(add-to-list 'evil-normal-state-modes 'compilation-mode) ; Iniciar compilation mode en estado normal
(with-eval-after-load 'undo-tree                         ; Empatar undo-tree con evil
  (customize-set-variable 'evil-undo-system 'undo-tree)
  (customize-set-variable 'evil-undo-system 'undo-redo))

(evil-mode)

(provide 'mimacs-evil)
