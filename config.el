(use-package org-ref
  :ensure t)

(use-package yasnippet
  :ensure t
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (custom-set-variables
    '( yas-verbosity 3)
    '(yas-global-mode 1)
    )
  )

(use-package company
  :ensure t
  :commands company-mode
  :init
  ;(add-hook 'prog-mode-hook 'company-mode) 
  ;(add-hook 'LaTeX-mode-hook 'company-mode)
  ;(add-hook 'org-mode-hook 'company-mode)
  :config
 (custom-set-variables
 '(company-idle-delay .1)
 '(company-show-numbers t)
 '(company-echo-delay 0)
 '(company-minimum-prefix-length 2)
 '(company-selection-wrap-around t)
 '(company-transformers '(company-sort-by-occurrence company-sort-by-backend-importance ))
)
  (delete 'company-capf company-backends)
  :bind ("M-<Space>" . company-complete-common)
  )

(use-package company-quickhelp
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode)
  )

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (smartparens-global-mode)
  :bind 
  ("M-<backspace>". sp-backward-kill-sexp)
  ("M-<delete>". sp-forward-kill-sexp)
  :config
  (require 'smartparens-config)
  (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
  ;; markdown
  (defun sp--markdown-skip-asterisk (ms mb me)
    (save-excursion
      (goto-char mb)
      (save-match-data (looking-at "^\\* "))))
  (sp-with-modes 'markdown-<mode
    (sp-local-pair "*" "*"
     :unless '(sp-point-after-word-p sp-point-at-bol-p)
     :skip-match 'sp--markdown-skip-asterisk)
    (sp-local-pair "**" "**")
    (sp-local-pair "_" "_" :unless '(sp-point-after-word-p)))
  ;; haskell
  (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-mode)
  ;;; org-mode
  (defun sp--org-skip-asterisk (ms mb me)
    (or (and (= (line-beginning-position) mb)
	     (eq 32 (char-after (1+ mb))))
	(and (= (1+ (line-beginning-position)) me)
	     (eq 32 (char-after me)))))
  (defun sp--org-inside-LaTeX (id action context)
    (org-inside-LaTeX-fragment-p))
  (sp-with-modes 'org-mode
    (sp-local-pair "*" "*"
     :unless '(sp-point-after-word-p sp--org-inside-LaTeX sp-point-at-bol-p)
     :skip-match 'sp--org-skip-asterisk)
    (sp-local-pair "/" "/" :unless '(sp-point-after-word-p sp--org-inside-LaTeX))
    (sp-local-pair "~" "~" :unless '(sp-point-after-word-p sp--org-inside-LaTeX))
    (sp-local-pair "=" "=" :unless '(sp-point-after-word-p sp--org-inside-LaTeX))
    (sp-local-pair "\\[" "\\]")))

(use-package multiple-cursors
  :ensure t
  :bind(
	("C-n". mc/mark-next-like-this)	      
	("C-s-p" . mc/mark-previous-like-this)	  
	("C-f". mc/mark-all-like-this)	
	("C-<mouse-1>" . mc/add-cursor-on-click)
	)
  )

(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))
    )
  )

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)

  )

(ido-mode)

(use-package ido-completing-read+
  :ensure t
  :config (ido-ubiquitous-mode)
  )

(use-package flx-ido
   :ensure t
   :config
   (flx-ido-mode 1)
   (setq ido-enable-flex-matching 1)
   (setq ido-use-faces nil)
)

(use-package latex-pretty-symbols
  :ensure t
  :init
  (progn 
    (add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)
    (add-hook 'LaTex-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (setq reftex-plug-into-AUCTeX t)
    (setq TeX-auto-save t)
    )
  )

(global-prettify-symbols-mode 1)
(add-hook
 'python-mode-hook
 (lambda ()
   (mapc (lambda (pair) (push pair prettify-symbols-alist))
         '(;; Syntax
           ("def" .      #x2131)
           ("not" .      #x2757)
           ("in" .       #x2208)
           ("not in" .   #x2209)
           ("return" .   #x27fc)
           ("yield" .    #x27fb)
           ("for" .      #x2200)
           ;; Base Types
           ("int" .      #x2124)
           ("float" .    #x211d)
           ("str" .      #x1d54a)
           ("True" .     #x1d54b)
           ("False" .    #x1d53d)
           ;; Mypy
	   ("*"	.	 #x00d7)
           ("Dict" .     #x1d507)
           ("List" .     #x2112)
           ("Tuple" .    #x2a02)
           ("Set" .      #x2126)
	   ("sum" . 	 #x2211)
           ("Iterable" . #x1d50a)
           ("Any" .      #x2754)
	   ("lambda" .	 #x03bb)
           ("Union" .    #x22c3)
	   )
	 )
   )
 )

;; FiraCode support 
(when (window-system)
  (set-frame-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

(use-package rainbow-delimiters
  :ensure t 
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'LaTex-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook 'rainbow-delimiters-mode)
  )

(use-package flycheck
  :ensure t
  :defer t
  :commands (flycheck-mode)
  :init
  (progn
    (eval-after-load 'flycheck '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))
    (add-hook 'prog-mode-hook 'flycheck-mode)
    (add-hook 'text-mode-hook 'flycheck-mode)
    )
  )

(use-package flyspell
  :ensure t
  :defer t
  :init
  (progn
    (setq-default ispell-program-name "aspell")
    (setq-default ispell-extra-args '("--sug-mode=fast"))
    (setq-default ispell-dictionary "english")
    (add-hook 'markdown-mode-hook '(lambda () (flyspell-mode 1)))
    (add-hook 'text-mode-hook '(lambda () (flyspell-mode 1))))
  :config
  )

(use-package expand-region
  :ensure t
  :bind ("C-v" . er/expand-region)
  :bind ("C-S-v" . /er/contract-region)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; notifier 
;; requires 'sudo gem install terminal-notifier'
;; stolen from erc-notifier

(defvar terminal-notifier-command
  (executable-find "terminal-notifier")
  "The path to terminal-notifier."
  )
((lambda () terminal-notifier-command))
;; Some of my own modifications
(defvar window
  (if (boundp 'aquamacs-version) "org.gnu.Aquamacs" "org.gnu.Emacs") "The window to activate on clicking")
(defvar message-title
  (if (boundp 'aquamacs-version) "Aquamacs" "Emacs") "the title of notifications")
;;Check if we're running Emacs or Aquamacs.
(defvar icon
  (if (boundp 'aquamacs-version)
      "/Applications/Aquamacs.app/Contents/Resources/Aquamacs.icns" "https://www.gnu.org/software/emacs/images/emacs.png")
  )


(defun terminal-notifier-notify (title message)
  "Show a message with `terminal-notifier-command`."
  (start-process "terminal-notifier"
		 "*terminal-notifier*"
		 terminal-notifier-command
		 "-title" title
		 "-message" message
		 "-activate" window
		 "-sound" "default"
		 "-appIcon" icon
		 )
  )



(defun timed-notification(time message)
  (interactive
   "sNotify when (e.g: 2 minutes, 60 seconds, 3 days): \nsMessage: ")
  (run-at-time time nil
	       (lambda (msg) (terminal-notifier-notify message-title msg)) message)
  )

(use-package alert
  :defer t
  :config
  (alert-add-rule :mode     'org-mode
		  :category "random-todo"
		  :style 'notifier
		  :continue t)
  (alert-add-rule :mode 'org-mode
		  :category "org-alert"
		  :style 'notifier
		  :continue t)
		  )

(defvar quick-todo-file "~/Dropbox/org-notes/todo.org" "docstring")
(defun quick-todo ()
  "Quickly jot down a todo"
  (interactive)				

  (progn 
    (find-file quick-todo-file)
    (org-insert-todo-heading nil)
  )
  )

(use-package org-alert
  :ensure t
  :config
  (setq ))

(use-package cdlatex
  :ensure t
)

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  )

(global-set-key (kbd "C-M-;") 'comment-region-or-line )
(global-set-key (kbd "C-M-:") 'uncomment-region-or-line )

(custom-set-variables '(emulate-mac-british-keyboard-mode t))

(define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-kill-sexp)
(define-key smartparens-mode-map (kbd "M-<delete>") 'sp-forward-kill-sexp)
(message "this got executed")

(global-set-key (kbd "C-x t") 'quick-todo)
