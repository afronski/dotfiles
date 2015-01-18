(require 'package)

(add-to-list
 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

; Adding .emacs.d to the load path.
(add-to-list 'load-path "~/.emacs.d/lisp")

; Weird init of init.ec file.
(load-file "~/.emacs.d/init.ec")

; Enable line numbers.
(global-linum-mode 1)

; Enable and configure IDO mode.
(ido-mode 1)
(setq
 ido-enable-flex-matching t
 ido-everywhere t)

; Disable Welcome Screens and Messages.
(setq
 inhibit-startup-message t
 inhibit-startup-echo-area-message t)

; Enable indentation globally.
(define-key
  global-map
    (kbd "RET") 'newline-and-indent)

; Adding new lines on C-n.
(setq next-line-add-newlines t)

; Enable I-Menu with GoTo symbol.
(global-set-key
 (kbd "M-i") 'ido-goto-symbol)

(require 'etags)
(defun ido-find-tag ()
  "Find a tag using ido."
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
        (unless (integerp x)
          (push (prin1-to-string x t) tag-names)))
      tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(global-set-key [remap find-tag] 'ido-find-tag)
(global-set-key (kbd "C-.") 'ido-find-file-in-tag-files)

; Enable smartscan.
(smartscan-mode 1)

; Load Solarized theme - light by default.
(defun initialize-solarized-theme (frame)
  (load-theme 'solarized t)
  (set-frame-parameter frame 'background-mode 'light)
  (enable-theme 'solarized))

(defun light-solarized-theme ()
  (interactive)
  (set-frame-parameter nil 'background-mode 'light)
  (enable-theme 'solarized))

(defun dark-solarized-theme ()
  (interactive)
  (set-frame-parameter nil 'background-mode 'dark)
  (enable-theme 'solarized))

(set-frame-parameter nil 'background-mode 'light)

(if (daemonp)
    (add-hook 'after-make-frame-functions 'initialize-solarized-theme)
    (load-theme 'solarized t))

(global-set-key (kbd "C-x C-M-l") 'light-solarized-theme)
(global-set-key (kbd "C-x C-M-d") 'dark-solarized-theme)

; Enable no-easy-keys.
(require 'no-easy-keys)
(no-easy-keys 1)
