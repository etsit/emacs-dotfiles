;; -*- lexical-binding: t -*-


;;;; -----
;;;; Libraries

(require 'cl)
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
        elixir-mode
        elm-mode
        evil
        evil-nerd-commenter
        evil-org
        evil-tutor
        exec-path-from-shell ; Env vars from shell, e.g. PATH (i.e. access to shell cmds)
        gist
        goto-chg
        groovy-mode
        helm-ag
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
        parse-time
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
(when window-system (set-frame-size (selected-frame) 200 60))
(when window-system (set-frame-position (selected-frame) 250 75))

;; Don't open new frame (window)
(setq ns-pop-up-frames nil)

;;;; -----
;;;; Evil-mode package

;; Enable
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;; Escape to normal mode by "kj" (using key-chord lib)
;(setq key-chord-two-keys-delay 0.15)
;(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
;(key-chord-mode 1)

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
(load-org-agenda-files-recursively "~/Dropbox/org-emacs/" ) ; NOTE! trailing slash required
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
        (:endgroup . nil)
        (:startgroup . nil)
        ("@home" . ?O)
        ("@skovde" . ?S)
        ("@work" . ?K)
        (:endgroup . nil)
        ("christian" . ?c)
        ("claes" . ?l)
        ("olin" . ?o)
        ("magnuse" . ?g)
        ("iman" . ?i)
        ("victor" . ?v)
        ("me" . ?e)
        ("mamma" . ?m)
        ("pappa" . ?p)
        ("jonas" . ?j)
        ("josefine" . ?f)
        ("noel" . ?n)
        ("malva" . ?a)
        ("lennox" . ?l)
        ("wf" . ?W)
        ))


;;;; -----
;;;; Powerline package

(require 'powerline)
(powerline-default-theme)


;;;; -----
;;;; Color theme

(load-theme 'solarized-light t)


;;;; -----
;;;; custom.el

(setq custom-file "~/.emacs-custom.el")
(load custom-file)


;;;; -----
;;;; curry-compose.el (partial)
;;;; https://gist.github.com/eschulte/6167923

(defsubst compose (function &rest more-functions)
  (cl-reduce (lambda (f g)
               (lexical-let ((f f) (g g))
                            (lambda (&rest arguments)
                              (funcall f (apply g arguments)))))
             more-functions
             :initial-value function))


;;;; -----
;;;; Own function lib

(defun setpos (index fn lst)
  "Applies fn to element at index in lst.
  Returns a modified copy of lst."
  (let ((m (- (length lst) index)))
    (append (copy-list (butlast lst m))
            (list (funcall fn (nth index lst)))
            (copy-list (last lst (1- m))))))

(defun inc-date-year (offset lst)
  "Offsets year field in date list."
  (setpos 5 (apply-partially #'+ offset) lst))

(defun inc-date-month (offset lst)
  "Offsets month field in date list."
  (setpos 4 (apply-partially #'+ offset) lst))

(defun inc-date-day (offset lst)
  "Offsets day field in date list."
  (setpos 3 (apply-partially #'+ offset) lst))

(defun insert-date-from-list (date-lst)
  "Inserts a date in org-mode format at cursor position."
  (progn
    (message "%s" date-lst)
    (org-insert-time-stamp (apply #'encode-time date-lst))))

(defun merge-lists-prefer-first (l1 l2)
  "Returns a new list with the same length as l1 and l2
  where an element at position i is selected from the
  corresponding position at l1 if it holds a non-nil value,
  otherwise from l2 if non-nil, otherwise nil"
  (cl-mapcar
   (lambda (x y) (cond (x) (y)))
   l1 l2))

(defun parse-date-defaulting-to-current-time (date-string)
  "Accepts a date string parameter parsed by parse-time-string,
  positions with nil values are replaced by 0."
  (merge-lists-prefer-first
   (parse-time-string date-string)
   (make-list 9 0)))

(defun study-repetition-date-offset-funs ()
  "Returns a list of functions that each accept a single argument:
  a date list (of the format returned by encode-time). Each function
  offsets the date according to best study repetition practices,
  in order, respectivelly:
  - x1: 1 day after argument
  - x2: 7 days after x1
  - x3: 1 month after x2
  - x4: 3 months after x3
  - x5: 6 months after x4
  - x6: 1 year after x5
  - x7: 2 years after x6.
  Note that the functions will modify the date list in a manner that
  may increase positions over their proper values, e.g. a month value
  exceeding 11. When a date is encoded by e.g. encode-time, this will
  be adjusted for."
  (nreverse (cl-maplist (lambda (fs) (apply #'compose fs))
              (list (apply-partially #'inc-date-year 2)
                    (apply-partially #'inc-date-year 1)
                    (apply-partially #'inc-date-month 6)
                    (apply-partially #'inc-date-month 3)
                    (apply-partially #'inc-date-month 1)
                    (apply-partially #'inc-date-day 7)
                    (apply-partially #'inc-date-day 1)))))

(cl-defun study-repetition-dates (&optional (first-learning-date (decode-time (current-time))))
  "Returns study repetition dates in the date list format
  as returned by decode-time. For descriptions of which dates,
  see documentation string of study-repetition-date-offset-funs.
  If no value is passed for first-learning-date, current-time is used."
  (cl-mapcar
   (lambda (f) (funcall f first-learning-date))
   (study-repetition-date-offset-funs)))

(defun org-insert-study-repetition-dates-from-now ()
  "Insertion of \"my studies\" timestamps:
  Increasing time stamps from current date
  according to recommendations regarding repetition"
  (interactive)
  (cl-loop for d in (study-repetition-dates)
           do (insert-date-from-list d)))

(defun org-insert-study-repetition-dates-from-custom-date (date-text)
  "Insertion of \"my studies\" timestamps:
  Increasing time stamps from a provided date text string
  according to recommendations regarding repetition"
  (interactive "sEnter date: ")
  (let ((date-lst (parse-date-defaulting-to-current-time date-text)))
    (cl-loop for d in (study-repetition-dates date-lst)
             do (insert-date-from-list d))))


;;;; -----
;;;; org-mode

;; My studies date insertions
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\C-cy" 'org-insert-study-repetition-dates-from-now)))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\C-cu" 'org-insert-study-repetition-dates-from-custom-date)))

;; Capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)


;;;; -----
;;;; Ido mode

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)


;;;; -----
;;;; helm-ag

;; Interactive search
(global-set-key (kbd "C-c h") 'helm-do-ag) 


;;;; -----
;;;; ielm

; Auto complete
(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)


;;;; -----
;;;; exec-path-from-shell

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;;; -----
;;;; Startup

;; Default directory
(setq command-line-default-directory "~/Dropbox/org-emacs")

;;; Set org mode on start
;(pop-to-buffer (get-buffer-create (generate-new-buffer-name "*scratch-org*")))
;(insert "Scratch buffer with org-mode.\n\n")
;(org-mode)

