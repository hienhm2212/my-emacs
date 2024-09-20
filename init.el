;; Kickstart.emacs is *not* a distribution.
;; It's a template for your own configuration.

;; It is *recommeded* to configure it from the *config.org* file.
;; The goal is that you read every line, top-to-bottom, understand
;; what your configuration is doing, and modify it to suit your needs.

;; You can delete this when you're done. It's your config now. :)

;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun start/org-babel-tangle-config ()
  "Automatically tangle our Emacs.org config file when we save it. Credit to Emacs From Scratch for this one!"
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'start/org-babel-tangle-config)))

(require 'use-package-ensure) ;; Load use-package-always-ensure
(setq use-package-always-ensure t) ;; Always ensures that a package is installed
(setq package-archives '(("melpa" . "https://melpa.org/packages/") ;; Sets default package repositories
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/"))) ;; For Eat Terminal

(use-package general
:init 
(general-auto-unbind-keys)
    :config
    ;; (general-evil-setup)
    ;; Set up 'C-c' as the leader key
    (general-create-definer start/leader-keys
      :prefix "C-c")           ;; Set leader key

    (start/leader-keys
      "." '(find-file :wk "Find file")
      "TAB" '(comment-line :wk "Comment lines")
      "p" '(projectile-command-map :wk "Projectile command map"))

    (start/leader-keys
      "f" '(:ignore t :wk "Find")
      "f c" '((lambda () (interactive) (find-file "~/.emacs.d/config.org")) :wk "Edit emacs config")
      "f r" '(consult-recent-file :wk "Recent files")
      "f f" '(consult-fd :wk "Fd search for files")
      "f g" '(consult-ripgrep :wk "Ripgrep search in files")
      "f l" '(consult-line :wk "Find line")
      "f i" '(consult-imenu :wk "Imenu buffer locations"))

    (start/leader-keys
      "b" '(:ignore t :wk "Buffer Bookmarks")
      "b b" '(consult-buffer :wk "Switch buffer")
      "b k" '(kill-this-buffer :wk "Kill this buffer")
      "b i" '(ibuffer :wk "Ibuffer")
      "b n" '(next-buffer :wk "Next buffer")
      "b p" '(previous-buffer :wk "Previous buffer")
      "b r" '(revert-buffer :wk "Reload buffer")
      "b j" '(consult-bookmark :wk "Bookmark jump"))

    (start/leader-keys
      "c" '(:ignore t :wk "ChatGPT")
      "c c" '(gptel :wk "Start")
      "c s" '(gptel-send :wk "Send")
      "c m" '(gptel-menu :wk "Menu"))

    (start/leader-keys
      "d" '(:ignore t :wk "Dired")
      "d v" '(dired :wk "Open dired")
      "d j" '(dired-jump :wk "Dired jump to current"))

    (start/leader-keys
      "e" '(:ignore t :wk "Eglot Evaluate")
      "e e" '(eglot-reconnect :wk "Eglot Reconnect")
      "e f" '(eglot-format :wk "Eglot Format")
      "e l" '(consult-flymake :wk "Consult Flymake")
      "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
      "e r" '(eval-region :wk "Evaluate elisp in region"))

    (start/leader-keys
      "g" '(:ignore t :wk "Git")
      "g g" '(magit-status :wk "Magit status"))

    (start/leader-keys
      "h" '(:ignore t :wk "Help") ;; To get more help use C-h commands (describe variable, function, etc.)
      "h q" '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon")
      "h r" '((lambda () (interactive)
                (load-file "~/.config/emacs/init.el"))
              :wk "Reload Emacs config"))

    (start/leader-keys
      "s" '(:ignore t :wk "Show")
      "s e" '(eat :wk "Eat terminal"))

    (start/leader-keys
      "t" '(:ignore t :wk "Toggle")
      "t t" '(visual-line-mode :wk "Toggle truncated lines (wrap)")
      "t l" '(display-line-numbers-mode :wk "Toggle line numbers")))

(use-package emacs
  :custom
  (menu-bar-mode nil)         ;; Disable the menu bar
  (scroll-bar-mode nil)       ;; Disable the scroll bar
  (tool-bar-mode nil)         ;; Disable the tool bar
  (inhibit-startup-screen t)  ;; Disable welcome screen

  (delete-selection-mode t)   ;; Select text and delete it by typing.
  (electric-indent-mode nil)  ;; Turn off the weird indenting that Emacs does by default.
  (electric-pair-mode t)      ;; Turns on automatic parens pairing

  (blink-cursor-mode nil)     ;; Don't blink cursor
  (global-auto-revert-mode t) ;; Automatically reload file and show changes if the file has changed

  ;;(dired-kill-when-opening-new-dired-buffer t) ;; Dired don't create new buffer
  (recentf-mode t) ;; Enable recent file mode

  ;;(global-visual-line-mode t)           ;; Enable truncated lines
  ;; (display-line-numbers-type 'relative) ;; Relative line numbers
  (global-display-line-numbers-mode t)  ;; Display line numbers

  (mouse-wheel-progressive-speed nil) ;; Disable progressive speed when scrolling
  (scroll-conservatively 10) ;; Smooth scrolling
  ;;(scroll-margin 8)

  (tab-width 4)

  (make-backup-files nil) ;; Stop creating ~ backup files
  (auto-save-default nil) ;; Stop creating # auto save files
  (visible-bell t) ;; Turn off audible bell
  :hook
  (prog-mode . (lambda () (hs-minor-mode t))) ;; Enable folding hide/show globally
  :config
  ;; Move customization variables to a separate file and load it, avoid filling up init.el with unnecessary variables
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  :bind (
         ([escape] . keyboard-escape-quit) ;; Makes Escape quit prompts (Minibuffer Escape)
         )
  ;; Fix general.el leader key not working instantly in messages buffer with evil mode
  ;; :ghook ('after-init-hook
  ;;         (lambda (&rest _)
  ;;           (when-let ((messages-buffer (get-buffer "*Messages*")))
  ;;             (with-current-buffer messages-buffer
  ;;               (evil-normalize-keymaps))))
  ;;         nil nil t)
  )

(fset 'yes-or-no-p 'y-or-n-p) ; accept y/n instead of yes/no in prompts

(use-package amx
  :ensure t
  :config
  (amx-mode 1))

(setq user-full-name "Hien Huynh-Minh"
      user-mail-address "blackcat22121996@gmail.com")

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t)) ;; We need to add t to trust this package

(add-to-list 'default-frame-alist '(alpha-background . 95)) ;; For all new frames henceforth

(set-face-attribute 'default nil
                    ;; :font "JetBrains Mono" ;; Set your favorite type of font or download JetBrains Mono
                    :height 120
                    :weight 'medium)
;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.

;;(add-to-list 'default-frame-alist '(font . "JetBrains Mono")) ;; Set your favorite font
(setq-default line-spacing 0.12)

(use-package emacs
  :bind
  ("C-+" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("<C-wheel-up>" . text-scale-increase)
  ("<C-wheel-down>" . text-scale-decrease))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)     ;; Sets modeline height
  (doom-modeline-bar-width 5)   ;; Sets right bar width
  (doom-modeline-persp-name t)  ;; Adds perspective name to modeline
  (doom-modeline-persp-icon t)) ;; Adds folder icon next to persp name

(use-package nerd-icons
  :ensure t
  :demand t)

(use-package dashboard
  :ensure t
  :after nerd-icons
  :config
  (dashboard-setup-startup-hook)
  :init
  (setq 
   dashboard-startup-banner (concat user-emacs-directory "assets/emacs_banner.png")
   dashboard-banner-logo-title nil ; The text below the logo
   dashboard-set-heading-icons t
   dashboard-set-file-icons t
   dashboard-icon-type 'nerd-icons
   dashboard-display-icons-p t
   dashboard-items '((projects . 5)
                     (recents . 5)
                     (agenda . 5)
                     (bookmarks . 5))
   dashboard-startupify-list '(dashboard-insert-banner
                               dashboard-insert-newline
                               dashboard-insert-banner-title
                               dashboard-insert-newline
                               dashboard-insert-navigator
                               dashboard-insert-newline
                               dashboard-insert-init-info
                               dashboard-insert-items
                               dashboard-insert-newline
                               ;; dashboard-insert-footer
                               )
   dashboard-footer-messages '("The one true editor, Emacs!"
                               "Free as free speech, free as free Beer"
                               "Happy coding!"
                               "I use Emacs, which might be thought of as a thermonuclear word processor. --Neal Stephenson"
                               "Welcome to the church of Emacs"
                               "In the beginning was the lambda, and the lambda was with Emacs, and Emacs was the lambda."
                               "While any text editor can save your files, only Emacs can save your soul")
   )
  )

(use-package projectile
  :init
  (projectile-mode)
  :custom
  (projectile-run-use-comint-mode t) ;; Interactive run dialog when running projects inside emacs (like giving input)
  (projectile-switch-project-action #'projectile-dired) ;; Open dired when switching to a project
  (projectile-generic-command "fd . -0 --type f --color=never")     
  (projectile-project-search-path '("~/projects/"))) ;; . 1 means only search the first subdirectory level for projects
  ;; Use Bookmarks for smaller, not standard projects

(use-package eglot
  :ensure nil ;; Don't install eglot because it's now built-in
  :init
  (setq eglot-stay-out-of '(flymake))
  :hook (prog-mode . eglot-ensure)
  :custom
  ;; Good default
  (eglot-events-buffer-size 0) ;; No event buffers (Lsp server logs)
  (eglot-autoshutdown t);; Shutdown unused servers.
  (eglot-report-progress nil) ;; Disable lsp server logs (Don't show lsp messages at the bottom, java)
  ;; Manual lsp servers
  ;; :config
  ;; (add-to-list 'eglot-server-programs
  ;;             `((ruby-mode ruby-ts-mode) "ruby-lsp")) ;; Adds our lua lsp server to eglot's server list
  )

(use-package yasnippet-snippets
  :hook (prog-mode . yas-minor-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :bind (:map ruby-ts-mode-map
              ("C-c r b" . 'treesit-beginning-of-defun)
              ("C-c r e" . 'treesit-end-of-defun))
  :custom
  (ruby-indent-level 2)
  (ruby-indent-tabs-mode nil))

(use-package eldoc
  :init
  (global-eldoc-mode))

(use-package flymake
  :hook (prog-mode . flymake-mode))

(use-package company)

(use-package markdown-mode
  :ensure t
  :magic "\\.md\\'")

(use-package eat
  :hook ('eshell-load-hook #'eat-eshell-mode))



;; (use-package copilot
;;   :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
;;   :ensure t)
;; you can utilize :map :hook and :config to customize copilot

(defun me/read-openai-key ()
  (with-temp-buffer
    (insert-file-contents "./key.txt")
    (string-trim (buffer-string))))

(use-package gptel
  :config
  (setq gptel-playback t)
  (setq gptel-api-key #'me/read-openai-key)
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model "gpt-4o-mini"))

(defun ad/ai-from-anywhere ()
(interactive)
(let* ((screen-width (display-pixel-width))
       (screen-height (display-pixel-height))
       (frame-width (/ screen-width 3))
       (frame-height screen-height)
       (frame-left (- screen-width frame-width))
       (frame-top 0)
       (chat-frame (make-frame `((window-system . ns)  ;;change this if you are not on macOS. For example you can use "x" instead of "ns" for x systems. Refer to make-frame documentation for more details
                            (top . ,frame-top)
                            (left . ,frame-left)
                            (width . (text-pixels . ,frame-width))
                            (heigth . (text-pixels . ,frame-height))
                            (minibuffer . t)
                            ))))
  (select-frame chat-frame)
  )
  (add-hook 'gptel-post-response-hook (lambda () (goto-char (point-max))))
  (gptel "My:AI Chat" gptel-api-key nil)
  (switch-to-buffer "My:AI Chat")
  (delete-other-windows)
)

;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; (require 'start-multiFileExample)

;; (start/hello)

(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package magit
  :commands magit-status)

(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode))

(use-package hl-todo
:defer t
:config (global-hl-todo-mode 1))

(use-package magit-todos
  :after (magit)
  :config (magit-todos-mode 1))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum length of prefix for auto completion.
  (corfu-popupinfo-mode t)       ;; Enable popup information
  (corfu-popupinfo-delay 0.5)    ;; Lower popupinfo delay to 0.5 seconds from 2 seconds
  (corfu-separator ?\s)          ;; Orderless field separator, Use M-SPC to enter separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (completion-ignore-case t)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  (corfu-preview-current nil) ;; Don't insert completion without confirmation
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :after corfu
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; The functions that are added later will be the first in the list

  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers
  (add-to-list 'completion-at-point-functions #'cape-dict) ;; Dictionary completion
  (add-to-list 'completion-at-point-functions #'cape-file) ;; Path completion
  (add-to-list 'completion-at-point-functions #'cape-elisp-block) ;; Complete elisp in Org or Markdown mode
  (add-to-list 'completion-at-point-functions #'cape-keyword) ;; Keyword/Snipet completion

  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev) ;; Complete abbreviation
  ;;(add-to-list 'completion-at-point-functions #'cape-history) ;; Complete from Eshell, Comint or minibuffer history
  ;;(add-to-list 'completion-at-point-functions #'cape-line) ;; Complete entire line from current buffer
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol) ;; Complete Elisp symbol
  ;;(add-to-list 'completion-at-point-functions #'cape-tex) ;; Complete Unicode char from TeX command, e.g. \hbar
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml) ;; Complete Unicode char from SGML entity, e.g., &alpha
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345) ;; Complete Unicode char using RFC 1345 mnemonics
  )

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init
  (vertico-mode))

(savehist-mode) ;; Enables save history mode

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  ('marginalia-mode-hook . 'nerd-icons-completion-marginalia-setup))

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;; consult-theme :preview-key '(:debounce 0.2 any)
  ;; consult-ripgrep consult-git-grep consult-grep
  ;; consult-bookmark consult-recent-file consult-xref
  ;; consult--source-bookmark consult--source-file-register
  ;; consult--source-recent-file consult--source-project-recent-file
  ;; :preview-key "M-."
  ;; :preview-key '(:debounce 0.4 any))

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
           ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
           ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
           ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
           ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
           ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  :bind (
         ;; ("C-c M-x" . consult-mode-command)
         ;; C-x bindings (ctrl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g o" . consult-outline) ;; Alternativa: consult-org-heading
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g f" . consult-flymake)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s i" . consult-info)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history))
  )

(use-package embark-consult
:ensure t
:hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package diminish)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init
  (which-key-mode 1)
  :diminish
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha) ;; Same as default, except single characters are sorted alphabetically
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1) ;; Number of spaces to add to the left of each column
  (which-key-min-display-lines 6)  ;; Increase the minimum lines to display, because the default is only 1
  (which-key-idle-delay 0.8)       ;; Set the time delay (in seconds) for the which-key popup to appear
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit nil)) ;; Fixes which-key window slipping out in Emacs Daemon

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ("C-x o" . ace-window))

(use-package drag-stuff
  :ensure t
  :bind
  ( :map global-map
    ("M-<up>" . drag-stuff-up)
    ("M-p" . drag-stuff-up)
    ("M-<down>" . drag-stuff-down)
    ("M-n" . drag-stuff-down)
    ))

(use-package wc-mode
  :ensure t
  :defer t)

(use-package rotate
:ensure t
:bind
(:map global-map
("C-x C-l" . 'rotate-layout)) 
)

(use-package pdf-tools
:ensure t
:defer t
:config (pdf-tools-install))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package org
  :ensure nil
  :hook
  (org-mode . org-indent-mode)
  )

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

(use-package org-tempo
  :ensure nil
  :after org)

(setq org-babel-results-keyword "results")
(setq org-src-fontify-natively t)

(use-package ob-go :after org)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))

(use-package ob-rust :after org)

(use-package ox-gfm
  :defer t
  :ensure t
  :config (ox-gfm :type git :host github :repo "larstvei/ox-gfm"))

(use-package org-fragtog
  :ensure t
  :after org
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-startup-with-latex-preview t)
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto))
  )

(defun my/org-image-resize (frame)
  (when (derived-mode-p 'org-mode)
    (if (< (window-total-width) 80)
        (setq org-image-actual-width (window-pixel-width))
      (setq org-image-actual-width (* 80 (window-font-width))))
  (org-redisplay-inline-images)))
(add-hook 'window-size-change-functions 'my/org-image-resize)

(defun my/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))
(add-hook 'org-babel-after-execute-hook 'my/display-inline-images 'append)

(require 'ox-publish)
