;; these functions are useful. activate them.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;; (setq org-src-fontify-natively t)
  (custom-set-variables
   '(org-fontify-natively t)
   )

(custom-set-variables '(global-linum-mode t))

(setq split-height-threshold nil)
(setq split-width-threshold 0)

(defun todo()
(interactive)
(find-file "/Users/app/Dropbox/org-notes/todo.org")
)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(use-package diminish
  :ensure t
  )
(use-package bind-key
  :ensure t
  )

(use-package use-package-ensure-system-package
  :ensure t
  )

(use-package org
  :ensure org-plus-contrib)

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
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'LaTeX-mode-hook 'company-mode)
  (add-hook 'org-mode-hook 'company-mode)
  :config
 (custom-set-variables
 '(company-idle-delay .5)
 '(company-show-numbers t)
 '(company-echo-delay 1)
 '(company-minimum-prefix-length 2)
 '(company-selection-wrap-around t)
 '(company-transformers '(company-sort-by-occurrence company-sort-by-backend-importance ))
 )
  :bind ("M-<Space>" . company-complete-common)
  )

(use-package company-quickhelp
  :ensure t
  :defer t
  :hook global-company-mode
  )

(define-key global-map (kbd "C-.") 'company-files)

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init (smartparens-global-mode)
  :hook eval-expression-minibuffer-setup
  :bind 
  ("M-<backspace>". sp-backward-kill-sexp)
  ("M-<delete>". sp-forward-kill-sexp)
  ("M-[" . sp-backward-slurp-sexp)
  ("M-]" . sp-forward-slurp-sexp)
  ("M-S-[" . sp-backward-barf-sexp)
  ("M-S-]" . sp-forward-barf-sexp)
  :config
  (require 'smartparens-config)
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

(use-package dired-ranger
  :ensure t
  :config
  (setq dired-ranger-copy-ring-size 1)
    (define-key dired-mode-map (kbd "C-w")
        (lambda ()
            (interactive)
            (dired-ranger-copy t)
            (define-key dired-mode-map (kbd "C-y") 'dired-ranger-move)))
    (define-key dired-mode-map (kbd "M-w")
        (lambda ()
            (interactive)
            (dired-ranger-copy nil)
            (define-key dired-mode-map (kbd "C-y") 'dired-ranger-paste)))
)

(use-package multiple-cursors
  :ensure t
  :bind(
	("C-n". mc/mark-next-like-this)	      
	("C-s-p" . mc/mark-previous-like-this)	  
	("C-f". mc/mark-all-like-this)	
	("M-<mouse-1>" . mc/add-cursor-on-click)
	)
  )

(use-package clang-format
  :ensure t
  )

(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))
    )
  )

(use-package tramp
:ensure t
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
  ;;:hook (org-mode . lambda () (org-bullets-mode 1))
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))) 
  )

(use-package rainbow-delimiters
  :ensure t 
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'LaTex-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook #'rainbow-delimiters-mode)
  )

(use-package flycheck
  :ensure t
  :config (progn (add-hook 'after-init-hook #'global-flycheck-mode)
		 (add-hook 'python-mode-hook (lambda ()
					       (flycheck-select-checker 'python-pylint)))
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

(use-package cdlatex
  :ensure t
)

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  )

(use-package ox-reveal
  :ensure t)
(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
;; (setq org-reveal-mathjax t)

(global-set-key (kbd "C-M-;") 'comment-region-or-line )
(global-set-key (kbd "C-M-:") 'uncomment-region-or-line )
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-s") 'save-buffer)

(define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-kill-sexp)
(define-key smartparens-mode-map (kbd "M-<delete>") 'sp-forward-kill-sexp)

(global-set-key (kbd "C-x t") 'quick-todo)
