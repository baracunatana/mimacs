(scroll-bar-mode -1)        ; Desabilitar el scroll bar
(tool-bar-mode -1)          ; Desabilitar la barra de herramientas
(tooltip-mode -1)           ; Desabilitar tool tips
(set-fringe-mode 10)        ; Dar algo de espacio entre ventanas
(menu-bar-mode -1)          ; Desabilitar barra de menú

(global-visual-line-mode)   ; Word wrapping por defecto en todos los modos
(global-auto-revert-mode t) ; Activar global auto-revert

(straight-use-package 'modus-themes)
(modus-themes-load-themes)
(modus-themes-load-vivendi)

(straight-use-package 'undo-tree)
(global-undo-tree-mode)

(straight-use-package 'all-the-icons)

(straight-use-package 'doom-modeline)
(add-hook 'after-init-hook 'doom-modeline-init)

(provide 'mimacs-interfaz)
