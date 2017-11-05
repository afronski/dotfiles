(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq package-enable-at-startup nil)
(package-initialize)

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(transient-mark-mode 1)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'default-frame-alist '(font . "Monospace-12"))
(set-face-attribute 'default t :font "Monospace-12")

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it's not. Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
       nil
       (package-install package)
	 ))
   packages))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(package-initialize)

(ensure-package-installed
 'nlinum
 'solarized-theme
 'evil
 'evil-leader
 'evil-org
 'helm
 'org
 'org-bullets
 'org-journal)

(load-theme 'solarized-dark t)

(require 'org)
(require 'org-bullets)
(require 'org-journal)

(setq backup-directory-alist `(("." . "~/.emacs.d/backup")))

(setq org-directory "~/.org")
(setq org-default-notes-file "~/.org/notes/notes.org")
(setq org-agenda-files (list "~/.org/notes/backlog.org"))
(setq org-log-done t)
(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELLED")))

(setq org-journal-dir "~/.org/journal/")
(setq org-archive-location (concat "~/.org/archive/archive.org::* " (format-time-string "%Y-%m-%d (week %V)")))

(setq org-journal-file-format "%Y-%m-%d.org")
(setq org-journal-date-format "%Y-%m-%d (week %V)")

(define-key global-map "\C-cc" 'org-capture)

(defun read-lines (filename)
  "Return a list of lines of a file at `filename`."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))

(defun list-ledger-meta-data ()
  (mapcar (lambda (line) (string-remove-prefix "account " line))
	  (cl-remove-if (lambda (line) (string= "" line))
			(read-lines "~/.org/accounting/meta.journal"))))

(defun ledger-meta-data ()
  (string-join (list-ledger-meta-data) "|"))

(defun capture-ledger-file-name ()
  "Function that should leave point at the end of a ledger file that we asked for."
  (let ((name (read-string "Ledger file name: ")))
    (find-file (expand-file-name (format "%s.journal" name) "~/.org/accounting"))
    (end-of-buffer)))

(setq org-capture-templates
      '(
	("t" "New task" entry (file+headline "~/.org/notes/backlog.org" "Not Reviewed Tasks")
	 "** TODO %?\n   %i\n   %a")

	("n" "New note" entry (file+headline "~/.org/notes/notes.org" "Not Reviewed Notes")
	 "** [%<%Y-%m-%d>] %^{Note title}%? :%^G:\n   %^{Content}\n   %i\n   %a")

	("l" "New ledger entry" plain (function capture-ledger-file-name)
	 "%<%Y-%m-%d> %^{Ledger entry title}%?\n  %^{What|Expenses:Buying:Food:Groceries|%(ledger-meta-data)}  %^{Amount} %^{Currency|PLN|EUR|USD|NOK}\n  %^{From which account|Assets:Account|%(ledger-meta-data)}"
	 :empty-lines 1)

	("a" "New account for ledger" plain (file "~/.org/accounting/meta.journal")
	 "account %^{Account name}"
	 :empty-lines 1)
	))

(helm-mode 1)

(defun adaptive-size-nlinum-mode-hook ()
  (when nlinum-mode
    (setq-local nlinum-format
                (concat "%" (number-to-string
                             (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
                        "d"))))

(setq org-bullets-bullet-list '("■" "◆" "▲" "▶"))

(defun customizations-for-org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0))
  (flyspell-mode t))

(add-hook 'nlinum-mode-hook #'adaptive-size-nlinum-mode-hook)
(add-hook 'prog-mode-hook 'nlinum-mode)

(add-hook 'org-mode-hook 'customizations-for-org-mode-hook)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode t)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'evil)
(evil-mode t)
