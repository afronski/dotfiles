;;; customizations --- My settings for Emacs Prelude.
;;; Commentary:
;;; - Disabling arrow keys and other easy keys.
;;; - Line numbers on gutter.
;;; - Adding key mapping for Solarized themes switching.
;;; Code:

; Setting the color theme.
(setq prelude-theme 'solarized-light)

; Enable line numbers.
(global-linum-mode 1)

; Adding new lines on C-n.
(setq next-line-add-newlines t)

; Disable "easy" bindings.
(setq guru-warn-only nil)

(defun light-solarized-theme ()
  "Enable light solarized theme."
  (interactive)
  (load-theme 'solarized-light t))

(defun dark-solarized-theme ()
  "Enable dark solarized theme."
  (interactive)
  (load-theme 'solarized-dark t))

(global-set-key (kbd "C-x C-M-l") 'light-solarized-theme)
(global-set-key (kbd "C-x C-M-d") 'dark-solarized-theme)

(provide 'customizations)
;;; customizations.el ends here
