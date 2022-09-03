(setq inhibit-startup-message t)  ; Disable startup page
(setq gc-cons-threshold 100000000) ; Set when GC is called (100MB)

;; Disable warning pop ups from native compilation
(setq comp-async-report-warnings-errors nil)

(add-to-list 'load-path "~/.config/emacs/packages/")

;;(setq use-package-verbose t)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
	 (package-refresh-contents))

(unless (package-installed-p 'use-package)
	 (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(scroll-bar-mode -1)   ; Disable scrollbar
(tool-bar-mode -1)     ; Disable toolbar
(tooltip-mode -1)      ; Disable tooltips
(set-fringe-mode 10)   ; Give some breathing room
(menu-bar-mode -1)     ; Disable the menu bar

(defun my/set-font-faces ()
  (set-face-attribute 'default nil :font "Iosevka" :height 115))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (my/set-font-faces))))
  (my/set-font-faces))

;(use-package ligature
;:load-path "path-to-ligature-repo"
;:config
;;; Enable the "www" ligature in every possible major mode
;(ligature-set-ligatures 't '("www"))
;;; Enable traditional ligature support in eww-mode, if the
;;; `variable-pitch' face supports it
;(ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
;;; Enable all Cascadia Code ligatures in programming modes
;(ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
;                                     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
;                                     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
;                                     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
;                                     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
;                                     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
;                                     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
;                                     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
;                                     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
;                                     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
;                                     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
;                                     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
;                                     "\\\\" "://"))
;;; Enables ligature checks globally in all buffers. You can also do it
;;; per mode with `ligature-mode'.
;(global-ligature-mode t))

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package tao-theme)
(use-package base16-theme)
(use-package doom-themes
:init (load-theme 'doom-dracula t)
:config
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t)) ; if nil, italics is universally disable

(use-package smart-mode-line)

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                (setq doom-modeline-icon t)))
    (setq doom-modeline-icon t)))

(defun my/mode-visual-fill ()
  (setq visual-fill-column-width 140
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
(use-package visual-fill-column
  :hook
  (org-mode . my/mode-visual-fill)
  (eww-mode . my/mode-visual-fill))

(setq user-full-name "Rafael Moraes"
      user-mail-address "rafael1.618@outlook.com")

; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-j"))

(setq org-confirm-babel-evaluate nil)

(defun my/org-font-setup ()
;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
	 (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Set faces for heading levels
(dolist (face '((org-level-1 . 1.5)
		(org-level-2 . 1.3)
		(org-level-3 . 1.15)
		(org-level-4 . 1.1)
		(org-level-5 . 1.1)
		(org-level-6 . 1.1)
		(org-level-7 . 1.1)
		(org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Liberation Serif" :weight 'regular :height (cdr face)))
;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun my/org-mode-setup ()
(org-indent-mode)
(variable-pitch-mode 1)
(visual-line-mode 1)
(setq org-file-apps '(("\\.pdf\\'" . emacs)) ) )

(use-package org
  :hook (org-mode . my/org-mode-setup)
  :config
  (setq org-latex-pdf-process (list
			 "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))
  (add-to-list 'org-latex-packages-alist
	 '("AUTO" "babel" t ("pdflatex")))
  (setq org-ellipsis " ⯆"
	org-hide-emphasis-markers t)
  (my/org-font-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))



(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(defun my/org-babel-tangle-config ()
    (when (string-equal (buffer-file-name)
			(expand-file-name "~/.config/emacs/emacs.org"))
(let ((org-confirm-babel-evaluate nil))
	(org-babel-tangle))))
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-config)))

;; Enable code evaluation on Org-mode
   (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . nil)
(python . t)))

;; Syntax highlight in org-mode latex exported
(setq org-latex-listings 'minted
org-latex-packages-alist '(("" "minted"))
org-latex-pdf-process
'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Disable line breaks
(dolist (mode '(prog-mode-hook
                ))
  (add-hook mode (lambda () (auto-fill-mode 0))))
;; Disable line wraping
(set-default 'truncate-lines t)

;; Enable auto-fill-mode in text files
(setq text-mode-hook 'turn-on-auto-fill)

;; Spaces as tabs
(setq indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-ident 2)

;; Configure Spelling
(cond
 ;; try hunspell at first
  ;; if hunspell does NOT exist, use aspell
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-local-dictionary "pt_BR")
  (setq ispell-local-dictionary-alist
        ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
        ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))

 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

;(use-package cdlatex)

(use-package counsel
:bind (("M-x" . counsel-M-x)
 ("C-x b" . counsel-ibuffer)
 ("C-x C-f" . counsel-find-file)
 :map minibuffer-local-map
 ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy)
  (ivy-mode 1)  ; Activate ivy
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")

(use-package company)

(use-package lsp-mode
    :commands (lsp lsp-deferred)
    :hook (c++-mode-hook . lsp-deferred)
    :init
    (setq lsp-keymap-prefix "C-c l")
    :config
    (lsp-enable-which-key-integration t)
    (setq lsp-enable-on-type-formatting nil)
    :custom
    )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-show-with-mouse t))

;; For Ivy
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package ccls
  :after lsp-mode
  :ensure t
  :config
  (setq ccls-executable "ccls")
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

(setq-default c-basic-offset 2)

;(use-package modern-cpp-font-lock
;  :hook
;  (c++-mode . modern-cpp-font-lock)
;  :ensure t)

(use-package glsl-mode
:ensure t)

(use-package projectile
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))
(setq projectile-project-run-cmd "make run")

(use-package lsp-haskell)
(use-package haskell-mode
  :hook (haskell-mode . lsp))

(use-package rustic)
(setq-default rustic-ident-offset 2)
(setq-default rust-ident-offset 2)

(use-package elm-mode)

;(require 'ox-reveal)
;(use-package htmlize)

(use-package nix-mode
  :mode "\\.nix\\'")

;(use-package elpy
;  :ensure t
;  :init
;  (elpy-enable))

(use-package lua-mode)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump t)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-s") 'evil-write)
  (define-key evil-normal-state-map (kbd "C-f") 'counsel-find-file)
  (define-key evil-normal-state-map (kbd "C-q") 'kill-current-buffer)
  (define-key evil-normal-state-map (kbd "g l") 'next-buffer)
  (define-key evil-normal-state-map (kbd "g h") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "g b") 'counsel-ibuffer)
  (define-key evil-normal-state-map (kbd "g r") 'ff-find-other-file)
  (define-key evil-insert-state-map (kbd "C-j") 'newline))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(fset 'yes-or-no-p 'y-or-n-p)

(defun config-load () (find-file (expand-file-name "~/.config/emacs/emacs.org")))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(use-package eshell-syntax-highlighting
  :after esh-mode
  :demand t ;; Install if not already installed.
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))



(defalias 'ff 'find-file)
(defalias 'ffo 'find-file-other-window)

(setenv "PATH"
        (concat
         "/usr/bin" ":"
         "/home/rafael049/.local/bin" ":"
         "/home/rafael049/.cargo/bin" ":"
         (getenv "PATH")))

(use-package which-key
:defer 0
:init (which-key-mode)
:diminish which-key-mode
:config
(setq which-key-idle-delay 0.3))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-find-file))

(use-package dired-single)

(define-skeleton skeleton/org-latex-abnt2
"Org latex abnt2 skeleton" 
""
"#+OPTIONS: toc:nil\n"
"#+LANGUAGE: pt-br\n"
"#+LATEX_CLASS: article\n"
"#+LATEX_HEADER: \\usepackage{hyperref}\n"
"#+LATEX_HEADER: \\hypersetup{hidelinks}\n"
"#+LATEX_HEADER: \\usepackage[alf]{abntex2cite}\n"
"#+LATEX_HEADER: \\usepackage{times}\n"
"#+LATEX_HEADER: \\usepackage{indentfirst}\n"
"#+LATEX_HEADER: \\usepackage[lmargin=3cm, rmargin=2cm, tmargin=3cm, bmargin=2cm]{geometry}\n"
"\n"
"#+BEGIN_EXPORT latex\n"
"\\title{"_"}\n"
"\\author{Rafael Batista de Moraes}\n"
"\\maketitle\n"
"#+END_EXPORT\n"
_
"\n\n\n\\bibliography{ref.bib}")

(define-skeleton skeleton/bib-tex
  "Bib skeleton"
  ""
  "\@BOOK{"_",\n"
  "AUTHOR=\"""\",\n"
  "TITLE=\"\",\n"
  "PUBLISHER=\"\",\n"
  "YEAR=\"\",\n"
  "}\n"
)
(put 'upcase-region 'disabled nil)
