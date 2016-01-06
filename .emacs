;; -*- lexical-binding: t -*-

;;;; -----
;;;; Libraries

(require 'cl-lib)

;;;; -----
;;;; Scripts location

;; (Homebrew installation)
;; Add the following to your init file to have packages installed by
;; Homebrew added to your load-path:
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))
;(let ((default-directory "C:\Program Files\emacs-24.5-bin-i686-mingw32\share\emacs\site-lisp"))
;    (normal-top-level-add-subdirs-to-load-path))

;;;; -----
;;;; Package config

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize))

;;; prelude-packages.el (auto-install packages)

(defvar prelude-packages
    '(auto-complete 
        alchemist ; Elixir
        auto-complete-pcmp
        clojure-mode
        coffee-mode
        elixir-mix
        elixir-mode
        evil
        evil-nerd-commenter
        evil-org
        evil-tutor
        gist
        goto-chg
        groovy-mode
        haml-mode
        haskell-mode
        inf-ruby
        key-chord
        linum-relative
        log4e
        magit
        markdown-mode
        ob-elixir
        org-ac
        popup
        powerline
        powerline-evil
        python
        sass-mode
        scss-mode
        solarized-theme
        undo-tree
        web-mode
        yaml-mode
        yaxception)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (cl-every 'package-installed-p prelude-packages))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'prelude-packages)
;;; prelude-packages.el ends here

;;;; -----
;;;; Misc

;; Tabs
; Tab character distance in spaces
(setq-default tab-width 2)
(setq-default evil-shift-width 2)
; Spaces instead of tabs
(setq-default indent-tabs-mode nil)
; Make CC-mode have equal tab distance as tab-width
(defvaralias 'c-basic-offset 'tab-width)
; Add alias for other modes to make tab distances consistent

;; Visual line mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
; (global-visual-line-mode 1) ; 1 for on, 0 for off.

;; Window size and position
(when window-system (set-frame-size (selected-frame) 120 60))
(when window-system (set-frame-position (selected-frame) 550 75))
;(when window-system (set-frame-size (selected-frame) 120 50))
;(when window-system (set-frame-position (selected-frame) 500 50))

;; Don't open new frame (window)
(setq ns-pop-up-frames nil)

;;;; -----
;;;; Evil-mode package

;; Enable
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;; Escape to normal mode by "jk" (using key-chord lib)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)

;; Relative line numbers
(require 'linum-relative)
(global-linum-mode t)

;;;; -----
;;;; Org mode package

;; The following lines are always needed.  Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; org-agenda-files set from list of files
;; taken from:
;; From http://www.emacswiki.org/emacs/ElispCookbook#toc58
(defun directory-dirs (dir)
  "Find all directories in DIR."
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((dir (directory-file-name dir))
        (dirs '())
        (files (directory-files dir nil nil t)))
    (dolist (file files)
      (unless (member file '("." ".."))
        (let ((file (concat dir "/" file)))
          (when (file-directory-p file)
            (setq dirs (append (cons file
                                     (directory-dirs file))
                               dirs))))))
    dirs))


(setq my-org-agenda-root "~/temp")
(setq my-org-agenda-files-list "~/.emacs.d/org-agenda-list.el")

(defun my-update-org-agenda-files ()
  "Create or update the `my-org-agenda-files-list' file.
  This file contains elisp code to set `org-agenda-files' to a
  recursive list of all children under `my-org-agenda-root'. "
  (interactive)
  (message "agenda file list updated"
    (with-temp-buffer
      (insert
      ";; Warning: this file has been automatically generated\n"
      ";; by `my-update-org-agenda-files'\n")
      (let ((dir-list (directory-dirs my-org-agenda-root))
            (print-level nil)
            (print-length nil))
        (cl-prettyprint `(setq org-agenda-files (quote ,dir-list))))
      (write-file my-org-agenda-files-list))))

;; Store clock across Emacs sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;;;; -----
;;;; Powerline package

(require 'powerline)
(powerline-default-theme)


;;;; -----
;;;; custom.el

(setq custom-file "~/.emacs-custom.el")
(load custom-file)
