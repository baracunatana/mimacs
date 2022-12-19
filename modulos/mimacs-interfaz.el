(when (display-graphic-p)
  (set-fringe-mode 10)      ; Dar algo de espacio entre ventanas
  (scroll-bar-mode -1))     ; Desabilitar el scroll bar
(tool-bar-mode -1)          ; Desabilitar la barra de herramientas
(tooltip-mode -1)           ; Desabilitar tool tips
(menu-bar-mode -1)          ; Desabilitar barra de menú

(global-visual-line-mode)   ; Word wrapping por defecto en todos los modos
(global-auto-revert-mode t) ; Activar global auto-revert

(use-package modus-themes
  :config
  (modus-themes-load-themes)
  (modus-themes-load-vivendi))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package all-the-icons)

(use-package doom-modeline
  :after
  all-the-icons
  :hook
  (after-init . doom-modeline-mode))

(provide 'mimacs-interfaz)
