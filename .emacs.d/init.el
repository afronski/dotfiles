;; Hide toolbar and menu.
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Hide this ugly splash screen.
(setq inhibit-splash-screen t)

;; Adding package repositories.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Set up the keyboard so the <delete> key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)

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

;; do not make backup files
(setq make-backup-files nil)

;; Enable ido-mode for switching buffers.
(ido-mode t)

;; Enable transient mark mode.
(transient-mark-mode 1)

;; Loading paths (also for installed packages).
(add-to-list 'load-path "~/.emacs.d")

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
(setq org-agenda-files (list "~/Repositories/notes"))

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
