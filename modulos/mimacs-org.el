(use-package org
  ;;:mode 
  ;;("\\.org\\'" . org-mode)
  ;; También se cargara cuando se ejecuten las siguientes funciones
  :commands 
  (org-capture org-agenda)
  :custom
  (org-startup-folded t)           ; Colapsar contenido al abrir un archivo
  (org-startup-align-all-table t)  ; Empezar con las tablas colapsadas
  (org-startup-indented t)         ; Activar org-indent-mode por defecto 
  (org-tags-column 0)              ; Quitar espacio entre título y etiquetas
  (org-list-allow-alphabetical t)  ; Permitir listas con letras
  (org-table-header-line-p t)      ; Congelar primera fila de tablas largas
  (org-confirm-babel-evaluate nil) ; No pedir confirmación para ejecutar código desde babel
  
  :hook
  
  ;;Desactivar electric-indent-mode en org
  (org-mode . (lambda () (electric-indent-local-mode -1)))
  :config
  
  ;;Seguir enlaces en la misma ventana
  (setf (alist-get 'file org-link-frame-setup) #'find-file))

(provide 'mimacs-org)
