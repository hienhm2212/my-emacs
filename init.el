;; -*- lexical-binding: t; outline-regexp: ";;;"; -*-
;;; System information
(defvar my-laptop-p (equal (system-name) "hienhm-lap"))

;;; Personal information
(setq user-full-name "Hien Huynh-Minh"
      user-mail-address "blackcat22121996@gmail.com")

;;; startup
(setq gc-cons-tkkhreshold (* 100 1000 1000))
(add-hook 'emacs-startup-hook
	  #'(lambda ()
	      (message "Startup in %s sec with %d garbage collections"
		       (emacs-init-time "%.2f")
		       gcs-done)))
(require 'server)
(unless (server-running-p)
  (server-start))

;;; for lsp performance
(setq read-process-output-max (* 1024 1024))

;;; No bell, no startup screen
(setq ring-bell-function 'ignore
      inhibit-startup-screen t
      )

;; Package manager
(package-initialize)

;; Ensure packages
(setq use-package-always-ensure t)

;; Support installing from git
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

;;; package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;; use-package
(setq user-package-verbose nil)

;; don't add custom settings here
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file t)

;;; undo tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-audo-save-history nil))

;;; org-mode
(setq my-todo-file "~/Projects/orgs/todo.org"
      my-schd-file "~/Projects/orgs/schedule.org")

(setq org-list-allow-alphabetical t
      org-export-with-toc nil
      org-image-actual-width nil)

;; only show hours in clock tables, hopefully it doesn't disrupt anything else
(setq org-duration-format '((special . h:mm)))

(setq org-reverse-note-order t)

(setq org-hide-emphasis-markers t)

(add-hook 'org-mode-hook
	  (defun my/org-mode-hook ()
	    (local-set-key (kbd "C-c i")
			   #'consult-org-heading)))

(setq org-refile-targets (list '(nil :level . 1)
			       '(nil :level . 2)))


;; citation
(require 'org)
(plist-put org-format-latex-options :scale 1.5)
(set-face-attribute 'org-table nil :foreground "midnight blue")
(set-face-attribute 'org-code nil
		    :family "Monospace"
		    :foreground "black"
		    :weight 'semi-light)
(customize-set-variable 'org-export-backends
			(add-to-list 'org-export-backends 'beamer))

(defun hhm/org-insert-screenshot ()
  "Copy a screenshot from my quick screenshot location (/tmp/screenshot.png) to the current directory and insert an org-mode link to it at point"
  (interactive)
  (let ((name (concat default-directory (make-temp-name "img") ".png")))
    (copy-file "/tmp/screenshot.png" name)
    (insert (format "[[%s]]" name))))

(defun open-pdf (path)
  "open pdf: links in org mode using zathura"
  (save-window-excursion
    (let* ((split (split-string path ":"))
	   (cmd
	    (cl-ecase (length split)
	      (1
	       (concat "zathura " path))
	      (2
	       (concat "zathura -P "
		       (cadr split)
		       " "
		       (car split))))))
      (async-shell-command cmd))))

(org-add-link-type "pdf" #'open-pdf)

;; use modified file completion for inserting pdf links
(setf (alist-get "pdf" org-link-parameters nil nil #'equal)
      (plist-put (cdr (assoc "pdf" org-link-parameters))
		 :complete
		 #'(lambda ()
		     (let ((default-directory "~/Library/"))
		       (replace-regexp-in-string "^file:"
						 "pdf:~/Library/"
						 (org-complete-file))))))

(org-add-link-type
 "ssh"
 (defun open-ssh (path)
   "open ssh links in org mode"
   (save-window-excursion
     (let ((split (split-string-path ":")))
       (call-process-shell-command
	(concat "st bash -c \"exec ssh -t "
		(car split)
		" 'cd "
		(cadr split)
		" ; bash --login'\"")
	nil 0)))))

(org-add-link-type
 "proj"
 (defun open-proj-link (path)
   (projectile-switch-project-by-name path)))

(defun proj-store-link ()
  (let ((projects (projectile-relevant-known-projects)))
    (when projects
      (projectil-completing-read
       "Store link to: " projects))))

(setf (alist-get "proj" org-link-parameters nil nil #'equal)
      (plist-put (cdr (assoc "proj" org-link-parameters))
		 :complete #'proj-store-link))

(setq org-make-link-description-function
      (defun my/org-make-link-description-function (link desc)
	"make the default description when inserting org links more intelligent"
	(cond
	 ((region-active-p)
	  (buffer-substring (mark) (point)))
	 ((string-prefix-p "pdf:" link)
	  (replace-regexp-in-string "pdf:~/Library/\\(.*\\).pdf$" "\\1" link)))))

(setq org-agenda-files (list my-schd-file)
      org-agenda-start-on-weekday 0
      org-agenda-restore-windows-after-quit t)

(defun org-instert-equation ()
      (interactive)
      (insert "\\begin{equation}
\\end{equation}
"))

;;; org-tempo
;; for old-style <s source blocks
(require 'org-tempo)
(mapcar #'(lambda (x)
	    (add-to-list 'org-structure-template-alist x))
	'(("el" . "src emacs-lisp")
	  ("gp" . "src gnuplot")
	  ("li" . "src lisp")
	  ("awk" . "src awk")
	  ("txt" . "src text")
	  ("ru" . "src rust")
	  ("rb" . "src ruby")
	  ("sql" . "src sql")
	  ("shell" . "src shell")))

(setq org-src-window-setup 'other-window)

(use-package ob-go
  :ensure t
  :defer t)

(use-package org-alert
  :ensure t
  :config
  (setq alert-default-style 'libnotify
	org-alert-interval 60
	alert-persist-idle-time 0
	alert-fade-time 600
	org-alert-match-string
	"SCHEDULED<\"<yesterday>\"+SCHEDULED<\"<tomorrow>\"")
  (org-alert-enable))

;; babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (perl . t)
   (lisp . t)
   (go . t)
   (dot . t)
   (awk . t)
   (ruby . t)
   (shell . t)
   (gnuplot . t)))

;; conf
(setq org-link-frame-setup '((vm . vm-visit-folder-other-frame)
			     (vm-imap . vm-visit-imap-folder-other-frame)
			     (gnus . org-gnus-no-new-news)
			     (file . find-file-other-window)
			     (wl . wl-other-frame)))


(defun hhm/org-in-an-hour ()
  "insert a time one hour from now"
  (let* ((hour (format-time-string "%H"))
	 (fmt (format "%%Y-%%m-%%d %%a %%0d:%%M" (+ 1 (string-to-number hour)))))
    (org-insert-time-stamp
     (org-read-date t t (format-time-string fmt))
     t )))

(setq org-cycle-separator-lines 1)
(setq org-src-window-setup 'current-window)
(setq org-capture-templates
      '(("s" "Schedule")
	("ss" "Schedule" entry (file my-schd-file)
	 "* TODO %^{task}\n SCHEDULED: %^T")
	("sm" "Schedule+Mail" entry (file my-schd-file)
	 "* TODO %^{task}\n SCHEDULED: %^T\n %a")
	("q" "Quick" entry (file my-schd-file)
	 "* TODO %^{task}\n SCHEDULED: %(hhm/org-in-an-hour)")
	("i" "Idea" entry (file+headline my-todo-file "Inbox")
	 "* IDEA %^{task}\n")
	("r" "Read" entry (file+headline my-todo-file "Inbox")
	 "* READ %^{task}\n")
	("l" "Link" entry (file+headline my-todo-file "Inbox")
	 "* LINK %^{task}\n")
	("m" "Email" entry (file my-todo-file)
	 "* TODO %a\n\n %i" :prepend t)
	("t" "Todo" entry (file+headline my-todo-file "Inbox")
	 "* TODO %^{task}\n")
	("c" "Code" entry (file my-todo-file)
	 "* TODO %a\n %i" :prepend t)))

(setq org-archive-location "~/projects/Notes/archive.org::")
(setq org-hierarchical-todo-statistics nil)
(setq org-todo-keywords '((sequence "PROJECT")
			  (type "IDEA" "READ" "LINK")
			  (sequence "TODO" "WAIT" "|" "DONE")))

;; show available colors:
;; (list-colors-display)
(setq org-todo-keyword-faces '(("WAIT" . "orange")
			       ("ASK" . "orange red")
			       ("RUNN" . "dark turquoise")
			       ("RUNN" . "cyan")
			       ("NOPE" . "lawn green")
			       ("IDEA" . "dodger blue")
			       ("READ" . "green")
			       ("WATCH" . "slate blue")
			       ("LINK" . "orchid3")
			       ("ISSUES" . "deep pink")
			       ("PROJECT" . "magenta"))
      org-tag-faces '(("BAD" ."red")
		      ("OKAY" . "orange")
		      ("DEV" . "red")
		      ("problem" . "red")
		      ("ALLY" . "orange")
		      ("GRIFFIN" . "gold2")))

(setq bookmark-fontify nil)

;; set done text color to the same as todo
(custom-set-faces
 '(org-headline-done
   ((t (:foreground "sienna")))))

;; stripe-buffer
(custom-set-faces
 '(stripe-highlight
   ((t (:background "#EDEDFF")))))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; org sort list
(defun my-org-sort-list-in-custom-order (order)
	"Sort the current Org lists so that items are in the specified order.
        ORDER is a list of regexps."
	(org-sort-list
	 nil ?f
	 (lambda ()
		 (let ((case-fold-search t)
					 (item
						(when (looking-at "[ \t]*[-+*0-9.)]+\\([ \t]+\\[[- X]\\]\\)?[ \t]+")
							(org-sort-remove-invisible (buffer-substring (match-end 0) (point-at-eol))))))
			 (or (cl-position item order :test (lambda (a b) (string-match b a))) (l+ (length order)))))
	 '<))
;;; End org

(use-package ledger-mode
  :ensure t
  :defer t)

(use-package company-ledger
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-ledger))
  (add-hook 'ledger-mode-book 'company-mode 1))

;;; completion / vertico
(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
	read-buffer-completion-ignore-code t
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  :ensure t
  :config
  (keymap-set minibuffer-mode-map "M-." #'embark-collect)
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the current target followed by an ellipsis if there are further targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
	  (which-key--hide-popup-ignore-command)
	(which-key--show-keymap
	 (if (eq (plist-get (car targets) :type) 'embark-become)
	     "Become"
	   (format "Act on %s '%s'%s"
		   (plist-get (car targets) :type)
		   (embark--truncate-target (plist-get (car targets) :target))
		   (if (cdr targets) "..." "")))
	 (if prefix
	     (pcase (lookup-key keymap prefix 'accept-default)
	       ((and (pred keymapp) km) km)
	       (_ (key-binding prefix 'accept-default)))
	   keymap)
	 nil nil t (lambda (binding)
		     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
	'(embark-which-key-indicator
	  embark-highlight-indicator
	  emark-isearch-highlight-indicator)))

(use-package embark-consult
  :ensure t)

(use-package which-key
  :ensure t)

;;; perl
(use-package perl-doc
  :ensure t)

;;; toml
(use-package toml-mode
  :ensure t)

;;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :hook (treemacs-mode . treemacs-add-and-display-current-project-exclusively)
  :config
  (setq treemacs-recenter-after-project-ump 'always
	treemacs-no-delete-other-windows t
	treemacs-tag-follow-delay 0.1
	treemacs-recenter-after-file-follow 'always)
  ;; this is unusable with org-roam in treemacs
  (treemacs-project-follow-mode 1)
  (treemacs-follow-mode 1))

;; we're close, but follow-mode stops working after visiting one of
;; the excluded dirs. must be setting a var somewhere when I do this
(defvar *my-treemacs-exclude-dirs*
  (list "/home/hienhm/Research/Roam/"))

(defun around-treemacs-follow (oldfun)
  (let* ((current-buffer (current-buffer))
	 (current-file
	  (or (buffer-file-name current-buffer)
	      (when (eq major-mode 'dired-mode)
		(treemacs-canonical-path (dired-current-directory)))))
	 (dir (when current-file
		(file-name-directory current-file))))
    (unless (and dir (member dir *my-treemacs-exclude-dirs*))
      (funcall oldfun))))
(advice-add #'treemacs--follow :around #'around-treemacs-follow)

;;; Go
(use-package go-mode
  :ensure t
  :hook ((go-mode . hs-minor-mode)
	 (go-mode . company-mode)
	 (go-mode . (lambda ()
		      (hi-lock-face-phrase-buffer "TODO"))))
  :bind (:map go-mode-map
	      ("<f6>" . gofmt)
	      ("C-c 6" . gofmt)
	      ("C-c 4" . golint)
	      ("<f4>" . golint))
  :config
  (add-to-list 'exec-path "~/go/bin")
  (setq gofmt-command "goimports"))

(use-package go-dlv
  :ensure t)

;;; Common Lisp
(use-package slime
  :defer t
  :commands (slime)
  :ensure t
  :config
  (add-hook 'slime-mode-mook
	    (lambda ()
	      (local-set-key (kbd "C-c C-k") 'slime-eval-buffer)))
  ;; followed instructions here for installing hyperspec with quicklisp
  ;; https://www.hexstreamsoft.com/articles/getting-started-with-the-clhs
  (let ((clhs-local "/home/hienhm/quicklisp/clhs-use-local"))
    (when (or (file-exists-p (concat clhs-local ".el"))
	      (file-exists-p (concat clhs-local ".elc")))
      (load clhs-local)
      (advice-add 'hyperspec-lookup
		  :around
		  (lambda (orig-fun &rest args)
		    (let ((brownse-url-browser-function 'eww-browse-url))
		      (apply orig-func args))))))
  (setq inferior-lisp-program "sbcl"))

(use-package rainbow-delimiters
  :ensure t
  :hook ((lisp-mode . rainbow-delimiters-mode))
  :config
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "dark green"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "blue"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "violet"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "coral"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "siennal"))))))

;;; company
(use-package company
  :ensure t
  :hook ((emacs-lisp-mode . (lambda ()
			      (setq-local company-backends '(company-elisp))))
	 (emacs-lisp-mode . company-mode))
  :config
  ;; don't select completions with M-#, save that for winum
  (company-keymap--unbind-quick-access company-active-map)
  (company-tng-configure-default)
  (setq company-idle-delay 0.1
	company-minium-prefix-length 1)
  (custom-set-faces
   '(company-preview
     ((t (:foreground "darkgray" :underline t))))
   '(company-preview-common
     ((t (:inherit company-preview))))
   '(company-tooltip
     ((t (:background "lightgray" :foreground "black"))))
   '(company-tooltip-selection
     ((t (:background "steelblue" :foreground "white"))))
   '(company-tooltip-common
     ((((type x)) (:inherit company-tooltip :weight bold))
      (t (:inherit company-tooltip))))
   '(company-tooltip-common-selection
     ((((type x)) (:inherit compay-tooltip-selection :weight bold))
      (t (:inherit company-tooltip-selection))))))

(use-package slime-company
  :ensure t
  :after (company slime)
  :config
  (slime-setup '(slime-company slime-fancy slime-asdf))
  (add-hook 'lisp-mode-hook
	    (lambda ()
	      (set (make-local-variable 'company-backends) '(company-slime))
	      (company-mode))))

;;; winum
(use-package winum
  :ensure t
  :config
  (setq winum-scope 'frame-local)
  (global-set-key (kbd "M-0") 'treemacs-select-window)
  (global-set-key (kbd "M-1") 'winum-select-window-1)
  (global-set-key (kbd "M-2") 'winum-select-window-1)
  (global-set-key (kbd "M-3") 'winum-select-window-1)
  (global-set-key (kbd "M-4") 'winum-select-window-1)
  (global-set-key (kbd "M-5") 'winum-select-window-1)
  (global-set-key (kbd "M-6") 'winum-select-window-1)
  (global-set-key (kbd "M-7") 'winum-select-window-1)
  (global-set-key (kbd "M-8") 'winum-select-window-1)
  (global-set-key (kbd "M-9") 'winum-select-window-1)
  (winum-mode))

;;; tramp
(require 'tramp)
(add-to-list 'tramp-remote-path "/usr/local/go/bin")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(setq exec-path (append exec-path '("/home/hienhm/.cargo/bin")))

;;; yasnippet
(use-package yasnippet-snippets
	:ensure t
	:config
	(yas-global-mode 1)
	(setq yas-snippet-dirs '("/home/hienhm/.emacs.d/snippets")))

;;; magit
(use-package magit
  :ensure t
  :config
  (setq magit-log-section-commit-count 25
	magit-copy-revision-abbreviated t))

;; smerge hydra for quicker conflict merging
(use-package smerge-mode
  :commands smerge-mode
  :config
  (use-package hydra)
  (defhydra sm/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (sm/smerge-hydra/body)))))

(use-package paredit
  :defer t
  :ensure t)

(use-package sed-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))


;;; projectile
(defun hhm/projectile-ignore-project (truename)
  (string-match "\\.cargo" truename))

(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (setq projectile-switch-project-action 'project-find-file
	projectile-per-project-compilation-buffer nil)
  (global-set-key (kbd "C-x f") #'project-find-file)
  :custom
  (projectile-completion-system 'auto)
  (projectile-ignore-project-function #'hhm/projectile-ignore-project))

(use-package flycheck-projectile
  :ensure t)
		  
;;; perspective
(use-package perspective
  :ensure t
  :bind
  (("C-x b" . persp-switch-to-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-c C-j")))

;;; elm
(use-package elm-mode
  :defer t
  :ensure t)

(use-package graphviz-dot-mode
  :ensure t)

;;; org-roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/Research/Roam/"))
	  ;; capture templates
	(org-roam-capture-templates
	 '(("d" "default" plain
			"%?"
			:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
			:unnarrowed t)
		 ("p" "project" plain
			"* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
		 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
		 :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ;; Dailies
	 ("C-c n j" . org-roam-dailies-capture-today))
  :config

  ;; getting a bug when I try to open files that's fixed by this
  (require 'ucs-normalize)
  (org-roam-db-autosync-mode)
  ;; helped with performance issue on save
  ;; https://github.com/org-roam/issues/1752
  (advice-add 'org-roam-db-update-file :around
	      (defun +org-roam-db-update-file (fn &rest args)
		(emacsql-with-transaction (org-roam-db)
					  (apply fn args)))))

(defun hhm/update-roam-ids ()
  "update roam ids when not found see https://github.com/org-roam/org-roam/issues/1688"
  (interactive)
  (org-id-update-id-locations
   (directory-files-recursively org-roam-directory ".org$\\|.org.gpg$")))

(use-package org-roam-ui
  :ensure t
  :defer t
  )
(use-package org-drill
  :ensure t
  :defer t
  :config
  (setq org-drill-add-random-noise-to-intervals-p nil))

;;; gnuplot
(use-package gnuplot
  :defer t
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode)))

(use-package rainbow-mode
  :ensure t)

;;; lua
(use-package lua-mode
  :ensure t)

(use-package cmake-mode
  :ensure t)

;;; dired
(setq dired-listing-switches
      "-al --group-directories-first")

;;; json
(add-hook 'js-json-mode-hook
	  (defun my/json-mode-hook ()
	    (local-set-key (kbd "<f6>")
			   (lambda ()
			     (interactive)
			     (json-pretty-print-buffer)
			     (save-buffer)))))

;;; diff
(require 'diff-mode)
(set-face-attribute 'diff-added nil :background "#99ff99")
(set-face-attribute 'diff-refine-removed nil :background "red")
(set-face-attribute 'diff-removed nil :background "#ff9999")

;;; eshell
(add-hook 'eshell-mode-hook
	  (defun my/eshell-mode-hook ()
	    (set-face-attribute 'eshell-prompt nil :foreground "red")))

;;; electric-pair
(setq-default electric-pair-inhibit-predicate
	      'electric-pair-conservative-inhibit)

;;; themes
(use-package ef-themes
  :ensure t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; load theme
(load-theme 'ef-duo-dark)

(set-face-attribute 'default nil
		    :font "Deja Vu Sans Mono"
		    :height 100
		    :weight 'regular)
(set-face-attribute 'italic nil :slant 'italic :underline nil)

;; require for my notoolkit build i quess
(set-border-color "#00e673")

;;; configuration
(setq-default fill-column 80)
(setq tab-width 2)
(add-hook 'prog-mode-hook
	  (defun my/prog-mode-hook ()
	    (electric-pair-mode -1)
	    (display-line-numbers-mode)
	    (display-fill-column-indicator-mode)))

(setq ring-bell-function 'ignore)
(global-auto-revert-mode 1)

;(setq backup-directory-alist
;	  `(("." , "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
	  `((".*" "~/.emacs.d/auto-save-list/" t)))
(winner-mode t)

;;; emacs-lisp
(define-key emacs-lisp-mode-map
			(kbd "C-c C-c")
			#'eval-defun)


;;; Desktop
(desktop-save-mode 1)
(setq desktop-save 'ask)
(dolist (file
		 '("todo.org" "work.org" "init.el"))
  (add-to-list 'desktop-clear-preserve-buffers file))

;; scroll acceleration; 1: on; nil: off
(setq scroll-step 1
	  scroll-conservatively 10000
	  mouse-wheel-progressive-speed 1
	  mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-visual-line-mode 1)
(setq explicit-shell-file-name "/bin/bash")
;; highlight todos
(mapcar #'(lambda (mode-hook)
			(add-hook mode-hook
					  (defun my/hl-phrase-hook ()
						(hi-lock-face-phrase-buffer "TODO" 'hi-yellow)
						(hi-lock-face-phrase-buffer "// BUG" 'hi-red-b)
						(hi-lock-face-phrase-buffer "NOTE" 'hi-salmon))))
		'(prog-mode-hook LaTeX-mode-hook))

;; don't open a buffer if visible in another frame
(setq-default display-buffer-reuse-frames t)

;; Dired
(use-package dired
  :ensure nil
  :init
  (setq dired-create-destination-dirs 'ask))

; Trying out consult
(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  )

;; Using LSP
(use-package eglot
  :hook
  ((python-ts-mode js-ts-mode typescript-ts-mode tsx-ts-mode ruby-ts-mode) . eglot-ensure))


;; Map to new treesitter modes
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((js-mode . js-ts-mode)
	(js-json-mode . json-ts-mode)
	(typescript-mode . typescript-ts-mode)
	(python-mode . python-ts-mode)))

  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75))))

;; Auto mapping from file extention
(add-to-list 'auto-mode-alist '("\\.[tj]sx?\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))

;; Auto Completion
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(use-package corfu
  :custom  
  (corfu-auto t) 
  (corfu-auto-delay 1)
  :init
  (global-corfu-mode))

;; Need corfu-terminal to get auto-complete working in terminal
(use-package corfu-terminal
  :custom
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;;; Meow Edit Mode
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(require 'meow)
(meow-setup)
(meow-global-mode 1)


;;; YAML
(use-package yaml-mode
	:ensure t
	:mode "\\.yml\\'")

;;; Ruby
(use-package rinari
	:ensure t)

(use-package robe
	:hook
	((ruby-mode-hook . robe-mode)
	 (ruby-mode-hook . ac-robe-setup)
	 (ruby-mode-hook . auto-complete-mode)))

;;; Docker
(use-package dockerfile-mode
	:ensure t
	:mode ("Dockerfile\\'" . dockerfile-mode))


;;; Screenshot
(defun screenshot-svg ()
	"Save a screenshot of the current frame as a SVG image.
Saves to a temp file and puts the filename in the kill ring."
	(interactive)
	(let* ((filename
					(expand-file-name
					 (format-time-string "%Y-%m-%d-%H-%M-%S.svg")
					 my-recordings-dir))
				 (data (x-export-frames nil 'svg)))
		(with-temp-file filename
			(insert data))
		(kill-new filename)
		(message filename)))
(keymap-global-set "C-c s" #'screenshot-svg)

;;; PDF Tools
(use-package pdf-tools
	:ensure t
	:magic ("%PDF" . pdf-view-mode)
	:config
	(pdf-loader-install))
