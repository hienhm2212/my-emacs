;;; init.el --- Little Fox Emacs -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;; Customfile
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;;; Default
;; Theme
(use-package ef-themes
  :ensure t
  :config
  (ef-themes-select 'ef-dark)
  (setq ef-themes-to-toggle '(ef-dark ef-summer)))

;; Disable ui
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; File Backups
(setq create-lockfiles nil ; Having.# files around ain't helpful
      auto-save-default t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Zooming In/Out
(use-package emacs
  :bind
  ("C-+" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("<C-wheel-up>" . text-scale-increase)
  ("<C-wheel-down>" . text-scale-decrease))

;;; UI
;; Full size frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Font
(set-face-attribute 'default nil
                    :font "FiraCode Nerd Font Mono" ;; Set your favorite type of font or download JetBrains Mono
                    :height 110
                    :weight 'medium)
;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.

;;(add-to-list 'default-frame-alist '(font . "JetBrains Mono")) ;; Set your favorite font
(setq-default line-spacing 0.12)

;; Icons
(use-package nerd-icons
  :ensure t
  :if (display-graphic-p)
  :demand t)

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-hook 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :after dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . (lambda () (nerd-icons-ibuffer-mode t))))

;; Dashboard
(use-package dashboard
  :ensure t
  :after nerd-icons
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t
        dashboard-startup-banner (concat user-emacs-directory "assets/emacs_banner.png")
        dashboard-icon-type 'nerd-icons
        dashboard-display-icons-p t
        dashboard-items '((recents   . 5)
                        (projects  . 5)
                        (bookmarks . 5)
                        (agenda    . 5))
        dashboard-projects-backend 'project-el
        dashboard-footer-messages '("The one true editor, Emacs!"
                                    "Free as free speech, free as free Beer"
                                    "Happy coding!"
                                    "I use Emacs, which might be thought of as a thermonuclear word processor. --Neal Stephenson"
                                    "Welcome to the church of Emacs"
                                    "In the beginning was the lambda, and the lambda was with Emacs, and Emacs was the lambda."
                                    "While any text editor can save your files, only Emacs can save your soul")
   )
)

(defgroup lf-ui '()
  "UI configuration for Little Fox Emacs."
  :tag "Little Fox UI"
  :group 'littlefox)

(defcustom lf-ui-line-numbers-enabled-modes
  '(conf-mode prog-mode)
  "Modes which should display line numbers."
  :type 'list
  :group 'lf-ui)

(defcustom lf-ui-line-numbers-disabled-modes
  '(org-mode)
  "Modes which should not display line numbers.

Modes derived from the modes defined in
`lf-ui-line-numbers-enabled-modes', but should node display line numbers."
  :type 'list
  :group 'lf-ui)

;; Note: There is one more customization variable
;; `lf-ui-display-line-numbers', which is defined below because it
;; depends on further code.

;; Help Buffers

;; Make `describe-*' screens mode helpful
(use-package helpful
  :ensure t)

(when (require 'helpful nil :noerror)
  (keymap-set helpful-mode-map "<remap> <revert-buffer>" #'helpful-update)
  (keymap-global-set "<remap> <describe-command>"        #'helpful-command)
  (keymap-global-set "<remap> <describe-function>"       #'helpful-callable)
  (keymap-global-set "<remap> <describe-key>"            #'helpful-key)
  (keymap-global-set "<remap> <describe-symbol>"         #'helpful-symbol)
  (keymap-global-set "<remap> <describe-variable>"       #'helpful-variable)
  (keymap-global-set "C-h F"                             #'helpful-function))

;; Bind extra `describe-*' commands
(keymap-global-set "C-h K" #'describe-keymap)

;; Line Numbers
(defun lf-ui--enable-line-numbers-mode ()
  "Turn on line numbers mode.

Used as hook for modes which should display line numbers."
  (display-line-numbers-mode 1))

(defun lf-ui--disable-line-numbers-mode ()
  "Turn off line numbers mode.

Used as hook for modes which should not display line numbers."
  (display-line-numbers-mode -1))

(defun lf-ui--update-line-numbers-display ()
  "Update configuration for line numbers display."
  (if lf-ui-display-line-numbers
      (progn
        (dolist (mode lf-ui-line-numbers-enabled-modes)
          (add-hook (intern (format "%s-hook" mode))
                    #'lf-ui--enable-line-numbers-mode))
        (dolist (mode lf-ui-line-numbers-disabled-modes)
          (add-hook (intern (format "%s-hook" mode))
                    #'lf-ui--disable-line-numbers-mode))
        (setq-default
         display-line-numbers-grow-only t
         display-line-numbers-type t
         display-line-numbers-width 2))
    (progn
      (dolist (mode lf-ui-line-numbers-enabled-modes)
        (remove-hook (intern (format "%s-hook" mode))
                     #'lf-ui--enable-line-numbers-mode))
      (dolist (mode lf-ui-line-numbers-disabled-modes)
        (remove-hook (intern (format "%s-hook" mode))
                     #'lf-ui--disable-line-numbers-mode)))))

(defcustom lf-ui-display-line-numbers nil
  "Whether line numbers should be enabled."
  :type 'boolean
  :group 'lf-ui
  :set (lambda (sym val)
         (set-default sym val)
         (lf-ui--update-line-numbers-display)))

(customize-set-variable 'lf-ui-display-line-numbers t)
;; Elisp Demos
(use-package elisp-demos
  :ensure t)
;; also add some examples
(when (require 'elisp-demos nil :noerror)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; add visual pulse when changing focus, like beacon but built-in
;; from from https://karthinks.com/software/batteries-included-with-emacs/
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command
                   scroll-down-command
                   recenter-top-bottom
                   other-window))
  (advice-add command :after #'pulse-line))

;; Breadcrumbs
(use-package breadcrumb
  :ensure t)
(when (require 'breadcrumb nil :noerror)
  (breadcrumb-mode))


;; Buffer
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; The value `t' means to guess the default target directory.
(customize-set-variable 'dired-dwim-target t)

;; automatically update dired buffers on revisiting their directory
(customize-set-variable 'dired-auto-revert-buffer t)

;; pop up dedicated buffers in a different window.
(customize-set-variable 'switch-to-buffer-in-dedicated-window 'pop)

(customize-set-variable 'switch-to-buffer-obey-display-actions t)

;; all the icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
:config
  (setq major-mode-hydra-title-generator
        '(lambda (mode)
           (let ((title (thread-last mode
                                     (symbol-name)
                                     (string-replace "-" " ")
                                     (string-replace " mode" "")
                                     (s-titleize))))
             (s-concat ; (s-repeat 5 " ")
              (all-the-icons-icon-for-mode mode :v-adjust 0.05)
              " " title " Commands")))))  

;; Mode Line
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-minor-modes nil
        doom-modeline-buffer-encoding nil
        doom-modeline-major-mode-color-icons t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-percent-position nil)
  (doom-modeline-mode 1))

;; Diminish
(use-package diminish
  :ensure t)

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; multiple cursors
(use-package multiple-cursors
  :ensure t)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;; COMPLETION
;; turn of fido mode when use vertico
(fido-vertical-mode -1)

;; No matter which completion mode is used:
(customize-set-variable 'tab-always-indent 'complete)
(customize-set-variable 'completion-cycle-threshold 3)
(customize-set-variable 'completion-category-overrides
                        '((file (styles . (partial-completion)))))
(customize-set-variable 'completions-detailed t)

;; use completion system instead of popup window
(customize-set-variable 'xref-show-definitions-function
                        #'xref-show-definitions-completing-read)

;;; EDITTING
;; Typed text replaces the selection if selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Do not save duplicates in kill-ring
(customize-set-variable 'kill-do-not-save-duplicates t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; define a key to define the word at point.
(keymap-set global-map "M-#" #'dictionary-lookup-definition)

;; Show dictionary definition on the left
(add-to-list 'display-buffer-alist
             '("^\\*Dictionary\\*"
               (display-buffer-in-side-window)
               (side . left)
               (window-width . 70)))

;; turn on spell checking, if avaiable.
(with-eval-after-load 'ispell
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

;;; Navigation
(when (and (require 'hydra nil :noerror)
           (require 'dumb-jump nil :noerror))
  (defhydra dumb-jump-hydra (:color blue :columns 3)
            "Dumb Jump"
            ("j" dumb-jump-go "Go")
            ("o" dumb-jump-other-window "Other window")
            ("e" dumb-jump-go-prefer-external "Go external")
            ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
            ("i" dumb-jump-go-prompt "Prompt")
            ("l" dumb-jump-quick-look "Quick Look")
            ("b" dumb-jump-back "Back"))
  (keymap-set dumb-jump-mode-map "C-M-y" #'dumb-jump-hydra/body))

;; use xref
(with-eval-after-load 'dumb-jump
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;; Persistence between sessions
;; Turn on recentf mode
(add-hook 'after-init-hook #'recentf-mode)
;; Enable savehist-mode for command history
(savehist-mode 1)

;; save the bookmarks file every time a bookmark is made or deleted
;; rather than waiting for Emacs to be killed.  Useful especially when
;; Emacs is a long running process.
(customize-set-variable 'bookmark-save-flag 1)

;; Window management

;; Turning on `winner-mode' provides an "undo" function for resetting ;; your window layout.  We bind this to` C-c w u' for winner-undo and
;; `C-c w r' for winner-redo (see below).
(winner-mode 1)

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; open man pages in their own window, and switch to that window to
;; facilitate reading and closing the man page.
(customize-set-variable 'Man-notify-method 'aggressive)

;; keep the Ediff control panel in the same frame
(customize-set-variable 'ediff-window-setup-function
                        'ediff-setup-windows-plain)

;; Window configuration for special windows.
(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)))

(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 10)))

;;; COMPLETION
;; cape
(use-package cape
  :ensure t)

(when (require 'cape nil :noerror)
  ;; Setup Cape for better completion-at-point support and more
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)

  ;; Silence the pcomplete capf, noeerors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; add behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

  ;; No auto-completion or completion-on-quit in eshell
  (defun my/completion-corfu-eshell()
    "Special settings for when using corfu with eshell."
    (setq-local corfu-quit-at-boundary t
                corfu-quit-no-match t
                corfu-auto nil)
    (corfu-mode))
  (add-hook 'eshell-mode-hook #'my/completion-corfu-eshell))

;; consult
(use-package consult
  :ensure t)
(when (locate-library "consult")
  ;; Set some consult bindings
  (keymap-global-set "C-s" 'consult-line)
  (keymap-set minibuffer-local-map "C-r" 'consult-history)
  (setq completion-in-region-function #'consult-completion-in-region))

;; corfu
(use-package corfu
  :ensure t)
(use-package corfu-terminal
  :ensure t)

(when (require 'corfu nil :noerror)
  (unless (display-graphic-p)
    (when (require 'corfu-terminal nil :noerror)
      (corfu-terminal-mode +1)))

  ;; Setup corfu for popup like completion
  (customize-set-variable 'corfu-cycle t)        ; Allows cycling through candidates
  (customize-set-variable 'corfu-auto t)         ; Enable auto completion
  (customize-set-variable 'corfu-auto-prefix 2)  ; Complete with less prefix keys

  (global-corfu-mode 1)
  (when (require 'corfu-popupinfo nil :noerror)

    ```
    (corfu-popupinfo-mode 1)
    (eldoc-add-command #'corfu-insert)
    (keymap-set corfu-map "M-p" #'corfu-popupinfo-scroll-down)
    (keymap-set corfu-map "M-n" #'corfu-popupinfo-scroll-up)
    (keymap-set corfu-map "M-d" #'corfu-popupinfo-toggle)))

```

;; embark
(use-package embark
  :ensure t)
(use-package embark-consult
  :ensure t)

(when (require 'embark nil :noerror)
  (keymap-global-set "<remap> <describe-bindings>" #'embark-bindings)
  (keymap-global-set "C-." 'embark-act)
  ;; Use embark to show bindings in a key prefix with `C-h`
  (setq prefix-help-command #'embark-prefix-help-command)
  (when (require 'embark-consult nil :noerror)
    (with-eval-after-load 'embark-consult
      (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))))

;; marginalia
(use-package marginalia
  :ensure t)
(when (require 'marginalia nil :noerror)
  ;; Configure Marginalia
  (customize-set-variable 'marginalia-annotators
                          '(marginalia-annotators-heavy
                            marginalia-annotators-light
                            nil))
  (marginalia-mode 1))

;; orderless
(use-package orderless
  :ensure t)
(when (require 'orderless nil :noerror)
  ;; Set up Orderless for better fuzzy matching
  (customize-set-variable 'completion-styles '(orderless basic))
  (customize-set-variable 'completion-category-overrides
                          '((file (styles . (partial-completion))))))
;; vertico
(use-package vertico
  :ensure t)
(when (require 'vertico nil :noerror)
  (require 'vertico-directory)
  (customize-set-variable 'vertical-cycle t)

  ;; Start Vertico
  (vertico-mode 1))
;;; LISP
;; Global defaults
(require 'eldoc)

;; aggressive-indent-mode for all lisp modes
(use-package aggressive-indent
  :ensure t
  :hook 
  ((lisp-mode . aggressive-indent-mode)
   (clojure-mode . aggressive-indent-mode)
   (scheme-mode . aggressive-indent-mode)))

;; emacs lisp
(use-package package-lint
  :ensure t)
(use-package package-lint-flymake
  :ensure t)
(when (locate-library "package-lint-flymake")
  (add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup))

;; sly - common lisp
(use-package sly
  :ensure t)
(use-package sly-asdf
  :ensure t
  :after sly)
(use-package sly-quicklisp
  :ensure t
  :after sly)
(use-package sly-repl-ansi-color
  :ensure t
  :after sly)

(with-eval-after-load 'sly
  (require 'sly-quicklisp "sly-quicklisp" :no-error)
  (require 'sly-repl-ansi-color "sly-repl-ansi-color" :no-error)
  (require 'sly-asdf "sly-asdf" :no-error))
(when (locate-library "sly")
  (add-hook 'lisp-mode-hook #'sly-editing-mode))


;;; IDE
;; dumb jump
(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-prefer-searcher 'rg
        xref-history-storage #'xref-window-local-history
        xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; Never using the etags backend.
  (remove-hook 'xref-backend-functions #'etags--xref-backend))

;; Eglot
(defun lf-ide--add-eglot-hooks (mode-list)
  "Add `eglot-ensure' to modes in MODE-LIST.

The mode must be loaded, i.e. found with `fboundp'. A mode which
is not loaded will not have a hook added, in which case add it
manually with something like this:
`(add-hook 'some-mode-hook #'eglot-ensure)'"
  (dolist (mode-def mode-list)
    (let ((mode (if (listp mode-def) (car mode-def) mode-def)))
      (cond
       ((listp mode) (lf-ide-add-eglot-hooks mode))
       (t
        (when (and (fboundp mode)
                   (not (eq 'clojure-mode mode)) ; prefer cider
                   (not (eq 'lisp-mode mode)) ; prefer sly/slime
                   (not (eq 'scheme-mode mode)) ; prefer geiser
                   )
          (let ((hook-name (format "%s-hook" (symbol-name mode))))
            (message "adding eglot to %s" hook-name)
            (add-hook (intern hook-name) #'eglot-ensure))))))))

(defun lf-ide--lsp-bin-exists-p (mode-def)
  "Return non-nil if LSP binary of MODE-DEF is found via `executable-find'."
  (let ((lsp-program (cdr mode-def)))
    ;; `lsp-program' is either a list of strings or a function object
    ;; calling `eglot-alternatives'.
    (if (functionp lsp-program)
        (condition-case nil
            (car (funcall lsp-program))
          ;; When an error occurs it's because Eglot checked for a
          ;; binary and didn't find one among alternatives.
          (error nil))
      (executable-find (car lsp-program)))))

(defun lf-ide-eglot-auto-ensure-all ()
  "Add `eglot-ensure' to major modes that offer LSP support.

Major modes are only selected if the major mode's associated LSP
binary is detected on the system."
  (when (require 'eglot nil :noerror)
    (lf-ide--add-eglot-hooks (seq-filter
                              #'lf-ide--lsp-bin-exists-p
                              eglot-server-program))))

;; Shot down server when last managed buffer is killed
(customize-set-variable 'eglot-autoshutdown t)

;; tree-sitter
(defun lf-ide--configure-tree-sitter (opt-in-only)
  "Configure tree-sitter for Emacs 29 or later.

OPT-IN-ONLY is a list of symbols of language grammars to 
auto-install instead of all grammars."
  ;; only attemp to use tree-sitter when Emacs was built with it.
  (when (member "TREE-SITTER" (split-string system-configuration-features))
    (when (require 'treesit-auto nil :noerror)
      ;; add all items of opt-in-only to the `treesit-auto-langs'.
      (when opt-in-only
        ;; (mapc (lambda (e) (add-to-list 'treesit-auto-langs e)) opt-in-only)
        (if (listp opt-in-only)
            (customize-set-variable 'treesit-auto-langs opt-in-only)
          (customize-set-variable 'treesit-auto-langs (list opt-in-only)))
        )
      ;; prefer tree-sitter modes
      (global-treesit-auto-mode)
      ;; install all the tree-sitter grammars
      (treesit-auto-install-all)
      ;; configure `auto-mode-alist' for tree-sitter modes relying on
      ;; `fndamental-mode'
      (treesit-auto-add-to-auto-mode-alist))
    (when (locate-library "combobulate")
      ;; perhaps too gross of an application, but the *-ts-modes
      ;; eventually derive from this mode.
      (add-hook 'prog-mode-hook #'combobulate-mode))))

(defun lf-ide-configure-tree-sitter (&optional opt-in-only)
  "Configure tree-sitter.

Requires a C compiler (gcc, cc, c99) installed on the system.
Nodate that OPT-IN-ONLY only affects seupts with Emacs 29 or later.

For Emacs 29 or later:
Requires Emacs to be built using \"--with-tree-sitter\".
All language grammars are auto-installed unless they are a member
of OPT-IN-ONLY, in which case *only* those grammars are
installed"
  (lf-ide--configure-tree-sitter opt-in-only))

;; Combobulate
(use-package treesit
  :config
  (use-package combobulate
    :preface
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (setq combobulate-key-prefix "C-c o")

    ;; Optional, but recommended.
    ;;
    ;; You can manually enable Combobulate with `M-x
    ;; combobulate-mode'.
    :hook
    ((python-ts-mode . combobulate-mode)
     (js-ts-mode . combobulate-mode)
     (go-mode . go-ts-mode)
     (html-ts-mode . combobulate-mode)
     (css-ts-mode . combobulate-mode)
     (yaml-ts-mode . combobulate-mode)
     (typescript-ts-mode . combobulate-mode)
     (json-ts-mode . combobulate-mode)
     (tsx-ts-mode . combobulate-mode)
     (typescript-ts-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    :load-path ("~/.emacs.d/assets/combobulate")))


;; Version control
(use-package magit
  :ensure t
  
  )

;; Editorconfig
(use-package editorconfig
  :defer t)

;; turn on editorconfig if it is avaiable.
(when (require 'editorconfig nil :noerror)
  (add-hook 'prog-mode-hook #'editorconfig-mode))

;; Ibuffer project
(use-package ibuffer-project
  :ensure t)

;; enhance ibuffer with ibuffer-project if it is avaiable.
(when (require 'ibuffer-project nil :noerror)
  (defun lf-ide-enhance-ibuffer-with-ibuffer-project ()
    "Set up integration for `ibuffer' with `ibuffer-project'."
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative)))
  (add-hook 'ibuffer-hook #'lf-ide-enhance-ibuffer-with-ibuffer-project))

;;; IDE ends here

;;; Programming
;; Turn on global fly check mode
(global-flycheck-mode)
;; Smart Parenthesis
(use-package smartparens
  :ensure t
  :custom 
  (smartparens-global-strict-mode t)
  :config
  (sp-with-modes sp-lisp-modes
                 ;; disable ', as it's the quote character:
                 (sp-local-pair "'" nil :actions nil))
  (sp-with-modes (-difference sp-lisp-modes sp-clojure-modes)
                 ;; use the pseudo-quote inside strings where it serve as hyperlink.
                 (sp-local-pair "`" "'"
                 :when '(sp-in-string-p
                         sp-in-comment-p)
                 :skip-match (lambda (ms _mb _me)
                               (cond
                                ((equal ms "'") (not (sp-point-in-string-or-comment)))
                                (t (not (sp-point-in-string-or-comment)))))))
  :hook
  (prog-mode . smartparens-strict-mode))

;; Configuration Files
(use-package conf-mode
  :mode (("\\.conf\\'" . conf-space-mdoe)
         ("\\.repo\\'" . conf-unix-mdoe)
         ("\\.setup.*\\'" . conf-space-mdoe)))

;; Ruby
(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :mode "\\.rake\\'"
  :bind (:map ruby-ts-mode-map
              ("C-c r b" . 'treesit-beginning-of-defun)
              ("C-c r e" . 'treesit-end-of-defun))
  :hook (ruby-ts-mode . eglot-ensure)
  :custom
  (ruby-indent-level 2)
  (ruby-indent-tabs-mode nil))

(use-package inf-ruby
  :ensure t)

(use-package ruby-electric
  :ensure t
  :hook (ruby-ts-mode . ruby-electric-mode))

;; Markdown
(use-package markdown-mode
  :ensure t
  :magic "\\.md\\'")

;; Typescript 
(use-package tide
  :ensure t)
;; Emmet

;;; Programming ends here




;;; Org Mode
;; Provides basic configuration for Org Mode.
;; Return or left-click with mouse follows link
(customize-set-variable 'org-return-follows-link t)
(customize-set-variable 'org-mouse-1-follows-link t)

;; Display links as the description provided
(customize-set-variable 'org-link-descriptive t)

;; Visually indent org-mode files to a given header level
;;(add-hook 'org-mode-hook #'org-indent-mode) ; remove when use org modern mode 

;; Hide markup markers
(customize-set-variable 'org-hide-emphasis-markers t)

(use-package org-appear
  :ensure t
  :after org)
(when (locate-library "org-appear")
  (add-hook 'org-mode-hook 'org-appear-mode))


;; Disable auto-pairing of "<" in org-mode with electric-pair-mode
(defun lf-org-enhance-electric-pair-inhibit-predicate ()
  "Disable auo-pairing of \"<\" in `org-mode' when using `electric-pair-mode'."
  (when (and electric-pair-mode (eql major-mode #'org-mode))
    (setq-local electric-pair-inhibit-predicate
                `(lambda (c)
                   (if (char-equal c ?<)
                       t
                     (,electric-pair-inhibit-predicate c))))))

;; Electric pair mode
;; Add hook to both electric-pair-mode-hook and org-mode-hook
(add-hook 'electric-pair-mode-hook #'lf-org-enhance-electric-pair-inhibit-predicate)
(add-hook 'org-mode-hook #'lf-org-enhance-electric-pair-inhibit-predicate)

;; Org Modern
(use-package org-modern
  :ensure t
  :after org
  :hook 
  ((org-mode . org-modern-mode)
   (org-adenda-finalize . org-modern-agenda)))

;; Tasks
(setq org-todo-keywords '((sequence "TODO(t)" "DOING(g)" "|" "DONE(d)")
                          (sequence "BLOCKED(b)" "|" "CANCELED(c)")))


;; Babel Blocks
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window)

(org-babel-do-load-languages 'org-babel-do-load-languages
                             '((shell . t)
                               (js . t)
                               (emacs-lisp . t)
                               (python . t)
                               (ruby . t)
                               (dot . t)
                               (css . t)
                               (http . t)
                               (sql . t)
                               (restclient . t)
                               (plantuml . t)))

;; SQL 
(use-package ob-sql-mode
  :ensure t
  :after org)

;; REST Web Services
(use-package ob-http
  :ensure t
  :after org)

;; Restclient
(use-package ob-restclient
  :ensure t
  :after org)

;; PlantUML
(use-package plantuml-mode
  :ensure t
  :config
  (setq org-plantuml-jar-path "/usr/local/share/plantuml/plantuml.jar"))

;; Mermaid 
(use-package mermaid-mode
  :ensure t
  :config
  (setq mermaid-mmdc-location "/usr/local/bin/mmdc"))
(use-package ob-mermaid
  :ensure t
  :config
  (setq ob-mermaid-cli-path mermaid-mmdc-location))

;; Image handling
(defun lf/org-image-resize (frame)
  (when (derived-mode-p 'org-mode)
    (if (< (window-total-width) 80)
        (setq org-image-actual-width (window-pixel-width))
      (setq org-image-actual-width (* 80 (window-font-width))))
  (org-redisplay-inline-images)))
(add-hook 'window-size-change-functions 'lf/org-image-resize)

(defun lf/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))
(add-hook 'org-babel-after-execute-hook 'lf/display-inline-images 'append)

;; Do not show warning 
(setq warning-minimum-level :error)

;; Denote
;; Second brain/zettlekasten by Protesilaos Stavrou, 
;; similar features as Org-Roam, but keeps everything in a
;; single directory, does not use a database preferring filenaming
;; conventions and grep instead.
(use-package denote
  :ensure t)

;;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)

;;; init.el ends here
