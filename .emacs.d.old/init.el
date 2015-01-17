;; Hide toolbar and menu.
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Erlang mode.
(setq load-path (cons  "/usr/lib/erlang/lib/tools-2.6.15/emacs" load-path))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(require 'erlang-start)

;; Hide this ugly splash screen.
(setq inhibit-splash-screen t)

;; Use spaces only.
(setq-default indent-tabs-mode nil)

;; Adding package repositories.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Set up the keyboard so the <delete> key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)

;; CTRL-C, CTRL-X, CTRL-V - CuaMode
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)    ;; Don't tabify after rectangle commands
(transient-mark-mode 1)                  ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t)      ;; Standard Windows behaviour

;; Emacs will not automatically add new lines.
(setq next-line-add-newlines nil)

;; Emacs will highlight active line.
(global-hl-line-mode 1)

;; [Home] & [End] key should take you to beginning and end of lines.
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

;; Displays the time in the status bar.
(display-time)

;; Emacs will not automatically add new lines.
(setq next-line-add-newlines nil)

;; Syntax highlighting by default.
(global-font-lock-mode 1)

;; Changes all yes/no questions to y/n type.
(fset 'yes-or-no-p 'y-or-n-p)

;; Scroll down with the cursor,move down the buffer one
;; line at a time, instead of in larger amounts.
(setq scroll-step 1)
(scroll-bar-mode -1)

;; do not make backup files
(setq make-backup-files nil)

;; Enable ido-mode for switching buffers.
(ido-mode t)

;; Enable transient mark mode.
(transient-mark-mode 1)

;; Loading paths (also for installed packages).
(add-to-list 'load-path "/home/afronski/.emacs.d/lisp")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")

(let ((default-directory "~/.emacs.d/elpa/"))
      (normal-top-level-add-subdirs-to-load-path))

;; Setting up the color theme.
(load-theme 'bubbleberry t)

;; Enable line numbering.
(require 'linum)
(global-linum-mode t)

;; Show trailing whitespace.
(require 'whitespace)
(setq show-trailing-whitespace t)

;; JavaScript configuration file.
;;(load "js-config")

;; Configuration for org-mode.
;; Enable it first, then add all files from specific
;; directories agenda files.
(require 'org)
(setq org-agenda-files (list "~/Repositories/playground-notes"))

;; Set proper key shortcuts for org-mode.
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; Recording timestamps for DONE status of task.
(setq org-log-done t)

;; Additional keywords for org-mode.
;;   TASK is a closed, isolated issue which should be done in near future.
;;   PROJECT is a near to the dictionary definition (task which takes time and effort).
;;   DREAM should be done before next year.
;;   FUTURE is an isolated task which should be done in far future (next month, year, undetermined).
;;
;;   DONE is completed (date is not important, however it should be logged as well).
;;   SUSPENDED is state where task is suspended for unspecified time from described reason.
;;   POSTPONED task is a task moved into doing it nearest future from some reason, with declared date.
(setq org-todo-keywords
  '((sequence "TASK" "PROJECT" "DREAM" "FUTURE" "|" "DONE" "SUSPENDED" "POSTPONED")))

;; Make org-mode work with files ending in *.org.
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Custom ser variables and faces.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "936e5cac238333f251a8d76a2ed96c8191b1e755782c99ea1d7b8c215e66d11e" "c73c384550e8204077d230edf564b57d77bb0f80803781ca7ba3832aac9a84d8" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Haskell Mode
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; ESC quits.
(defun minibuffer-keyboard-quit ()
    "Abort recursive edit. In Delete Selection mode, if the mark is active, just deactivate it;
     then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Powerline.
(require 'powerline)
;;(powerline-evil-vim-color-theme)
(display-time-mode t)