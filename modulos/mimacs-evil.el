(straight-use-package 'evil)
(require 'evil)

(customize-set-variable evil-default-state 'normal)      ; iniciar en modo normal
(setq evil-want-C-i-jump nil)                            ; Para evitar conflictos con TAB en org-mode
(add-to-list 'evil-normal-state-modes 'compilation-mode) ; Iniciar compilation mode en estado normal
(with-eval-after-load 'undo-tree                         ; Empatar undo-tree con evil
  (customize-set-variable 'evil-undo-system 'undo-tree)
  (customize-set-variable 'evil-undo-system 'undo-redo))

(evil-mode)

(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader nil (kbd "M-SPC"))
(evil-set-leader 'normal (kbd "SPC m") t)
(evil-set-leader nil (kbd "M-SPC m") t)

(evil-define-key 'normal 'global (kbd "<leader>ag") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>aa") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>ag") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>aE") 'mimacs-aliminar-archivo-y-buffer)

(evil-define-key 'nil 'global (kbd "<leader>ve" 'evil-window-delete)
(evil-define-key 'nil 'global (kbd "<leader>vd" 'evil-window-split)
;;(evil-define-key 'nil 'global (kbd "<leader>v<" 'evil-window-decrease-width)
;;(evil-define-key 'nil 'global (kbd "<leader>v>" 'evil-window-increase-width)
(evil-define-key 'nil 'global (kbd "<leader>vj" 'evil-window-down)
(evil-define-key 'nil 'global (kbd "<leader>vq" 'evil-quit-all)
(evil-define-key 'nil 'global (kbd "<leader>vk" 'evil-window-up)
(evil-define-key 'nil 'global (kbd "<leader>vh" 'evil-window-left)
(evil-define-key 'nil 'global (kbd "<leader>vl" 'evil-window-right)
(evil-define-key 'nil 'global (kbd "<leader>vo" 'delete-other-windows)
;;(evil-define-key 'nil 'global (kbd "<leader>vTAB"'(evil-window-next)
(evil-define-key 'nil 'global (kbd "<leader>vv" 'evil-window-vsplit)

(defun mimacs-aliminar-archivo-y-buffer ()
  "Eliminar el archivo actual del disco duro y cierra su buffer"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "¿De verdad quiere eliminar " filename " ?"))
            (progn
              (delete-file filename)
              (message "%s eliminado." filename)
              (kill-buffer)))
      (message "Este buffer no representaba un archivo"))))

(provide 'mimacs-evil)
