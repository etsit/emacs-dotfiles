;; -*- lexical-binding: t -*-

;;;; -----
;;;; Libraries

(require 'cl-lib)


;;;; -----
;;;; Encoding

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)


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
        unbound
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

;; Escape to normal mode by "kj" (using key-chord lib)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-mode 1)

;; Relative line numbers
;;(require 'linum-relative)
;;(global-linum-mode t)

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

;; Let marking as DONE insert CLOSED timestamp
(setq org-log-done 'time)

;; ///// Automatically set variable org-agenda-files
;; Taken from http://www.emacswiki.org/emacs/ElispCookbook#toc58

;; Collect all .org from my Org directory and subdirs
;; The following code works well in emacs 24.3+
;; "It does not require intermediate files creation
(setq org-agenda-file-regexp "\\`[^.].*\\.org\\'") ; default value
(defun load-org-agenda-files-recursively (dir) "Find all directories in DIR."
    (unless (file-directory-p dir) (error "Not a directory `%s'" dir))
    (unless (equal (directory-files dir nil org-agenda-file-regexp t) nil)
      (add-to-list 'org-agenda-files dir)
    )
    (dolist (file (directory-files dir nil nil t))
        (unless (member file '("." ".."))
            (let ((file (concat dir file "/")))
                (when (file-directory-p file)
                    (load-org-agenda-files-recursively file)
                )
            )
        )
    )
)
(load-org-agenda-files-recursively "~/Dropbox/documents/todo/" ) ; NOTE! trailing slash required
;; To be able to refile to any file found add this:
(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 1)))
;; /////


;; Store clock across Emacs sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Tags
(setq org-tag-alist
      '((:startgroup . nil)
        ("buy" . ?B)
        ("healthcare" . ?H)
        ("meta" . ?M)
        (:endgroup . nil)
        (:startgroup . nil)
        ("@home" . ?O)
        ("@skovde" . ?S)
        ("@work" . ?W)
        (:endgroup . nil)
        ("me" . ?e)
        ("mamma" . ?m)
        ("pappa" . ?p)
        ("jonas" . ?j)
        ("josefine" . ?f)
        ("noel" . ?n)
        ("malva" . ?v)
        ("lennox" . ?l)
        ("mystudies" . ?y)
        ("healthcare" . ?h)))

;;;; -----
;;;; Powerline package

(require 'powerline)
(powerline-default-theme)


;;;; -----
;;;; custom.el

(setq custom-file "~/.emacs-custom.el")
(load custom-file)


;;;; -----
;;;; Own function lib

(defun setpos (index fn lst)
  "Applies fn to element at index in lst.
  Returns a modified copy of lst."
  (let ((m (- (length lst) index)))
    (append (butlast lst m)
            (list (funcall fn (nth index lst)))
            (last lst (1- m)))))

(defun nsetpos (index fn lst)
  "Applies fn to element at index in lst.
  lst is modified.
  Returns value of fn application."
  (setcar
   (nthcdr index lst)
   (funcall fn (nth index lst))))

(defun inc-date-year (offset lst)
  "Offsets year field in date list."
  (nsetpos 5 (apply-partially '+ offset) lst))

(defun inc-date-month (offset lst)
  "Offsets month field in date list."
  (nsetpos 4 (apply-partially '+ offset) lst))

(defun inc-date-day (offset lst)
  "Offsets day field in date list."
  (nsetpos 3 (apply-partially '+ offset) lst))

(defun insert-date-from-list (date-lst)
  (org-insert-time-stamp (apply 'encode-time date-lst)))

;; Insertion of "my studies" timestamps:
;; Increasing time stamps from current date
;; according to recommendations regarding repetition
(defun org-my-custom-timestamp ()
  (interactive)
  (let ((date-lst (decode-time (current-time))))
    (inc-date-day 1 date-lst)
    (insert-date-from-list date-lst)
    (inc-date-day 7 date-lst)
    (insert-date-from-list date-lst)
    (inc-date-month 1 date-lst)
    (insert-date-from-list date-lst)
    (inc-date-month 3 date-lst)
    (insert-date-from-list date-lst)
    (inc-date-month 6 date-lst)
    (insert-date-from-list date-lst)
    (inc-date-year 1 date-lst)
    (insert-date-from-list date-lst)
    (inc-date-year 2 date-lst)
    (insert-date-from-list date-lst)))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\C-ct" 'org-my-custom-timestamp)))

;;;; -----
;;;; Ido mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

