(use-package general
  :after evil
  :config
  (general-create-definer mimacs-lider
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  ;; definición de tecla lider local (relativo al major mode) para modo normal.
  (general-create-definer mimacs-lider-local
    :states '(normal visual insert emacs)
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m"))

(mimacs-lider
  :infix "a"
  "" '(:ignore t)
  "a" 'find-file
  "g" 'save-buffer
  "E" 'mimacs-eliminar-archivo-y-buffer
  "G" 'write-file)

(defun mimacs-eliminar-archivo-y-buffer ()
  "Eliminar el archivo actual del disco duro y cierra su buffer"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
	(if (y-or-n-p (concat "¿De verdad quiere eliminar " filename "?"))
	    (progn
	      (delete-file filename)
	      (message "%s eliminado." filename)
	      (kill-buffer)))
      (message "Este buffer no representaba un archivo"))))

(mimacs-lider
  :infix "v"
  "" '(:ignore t)
  "e" 'evil-window-delete
  "d" 'evil-window-split
  "<" 'evil-window-decrease-width
  ">" 'evil-window-increase-width
  "j" 'evil-window-down
  "q" 'evil-quit-all
  "k" 'evil-window-up
  "h" 'evil-window-left
  "l" 'evil-window-right
  "o" 'delete-other-windows
  "TAB" 'evil-window-next
  "v" 'evil-window-vsplit)

(mimacs-lider
  :infix "f"
  "" '(:ignore t)
  "TAB" 'other-frame
  "e" 'delete-frame
  "c" 'make-frame)

(mimacs-lider
  :infix "b"
  "" '(:ignore t)
  "e" 'kill-this-buffer
  "k" 'previous-buffer
  "-" 'text-scale-adjust
  "+" 'text-scale-adjust
  "r" 'revert-buffer
  "v" 'visual-line-mode
  "u" 'evil-switch-to-windows-last-buffer
  "j" 'next-buffer)

(with-eval-after-load 'consult
  (mimacs-lider
    :infix "b"
    "" '(:ignore t)
    "b" 'consult-buffer))

(general-define-key
  :states '(normal)
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

(general-define-key
  :states '(normal)
  :infix "g"
  "h" 'evil-beginning-of-line
  "G" 'end-of-buffer
  "j" 'evil-next-line
  "k" 'evil-previous-line
  "l" 'evil-end-of-line)

(with-eval-after-load 'consult
  (general-define-key
   :states '(normal)
   "gs"   'consult-line))

(mimacs-lider
  :states '(normal insert)
  "SPC" 'set-mark-command)

(general-define-key
  :states '(normal insert emacs)
  "C-M-i" 'completion-at-point)

(with-eval-after-load 'embark
  (general-define-key
   :states '(normal insert emacs visual)
   "M-o"  'embark-act
   "M-S-o" 'embark-dwim
   "C-h B" 'embark-bindings))

(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-j") 'vertico-next)
  (define-key vertico-map (kbd "C-k") 'vertico-previous)
  (define-key vertico-map (kbd "M-H") 'vertico-directory-up))

(with-eval-after-load 'consult
  (mimacs-lider
    :infix "k"
    "c" '(consult-kmacro :which-key "cargar macro")))

(mimacs-lider
  :infix "k"
  "" '(:ignore t)
  "g" 'kmacro-start-macro
  "d" 'kmacro-end-macro
  "TAB" 'kmacro-insert-counter
  "e" 'kmacro-set-counter
  "s" 'kmacro-add-counter
  "k" 'kmacro-call-macro)

(provide 'mimacs-acordes)
