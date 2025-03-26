;;; init.el --- Emacs-Solo (no external packages) Configuration  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;;; -------------------- GENERAL EMACS CONFIG
;;; EMACS
(use-package emacs
  :ensure nil
  :bind
  (("M-o" . other-window)
   ("M-j" . duplicate-dwim)
   ("M-g r" . recentf)
   ("M-s g" . grep)
   ("M-s f" . find-name-dired)
   ("C-x C-b" . ibuffer)
   ("RET" . newline-and-indent)
   ("C-z" . nil)
   ("C-x C-z" . nil)
   ("C-x C-k RET" . nil))
  :custom
  (ad-redefinition-action 'accept)
  (column-number-mode nil)
  (line-number-mode nil)
  (completion-ignore-case t)
  (completions-detailed t)
  (delete-by-moving-to-trash t)
  (display-line-numbers-width 3)
  (display-line-numbers-widen t)
  (delete-selection-mode 1)
  (enable-recursive minibuffers t)
  (find-ls-option '("-exec ls -ldh {} +" . "-ldh"))  ; find-dired results with human readable sizes
  (frame-resize-pixelwise t)
  (global-auto-revert-non-file-buffers t)
  (help-window-select t)
  (history-length 300)
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (ispell-dictionary "en_US")
  (kill-do-not-save-duplicates t)
  (create-lockfiles nil)   ; No backup files
  (make-backup-files nil)  ; No backup files
  (backup-inhibited t)     ; No backup files
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (ring-bell-function 'ignore)
  (read-answer-short t)
  (recentf-max-saved-items 300) ; default is 20
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))
  (remote-file-name-inhibit-delete-by-moving-to-trash t)
  (remote-file-name-inhibit-auto-save t)
  (resize-mini-windows 'grow-only)
  (ring-bell-function #'ignore)
  (savehist-save-minibuffer-history t)    ; t is default
  (savehist-additional-variables
   '(kill-ring                            ; clipboard
     register-alist                       ; macros
     mark-ring global-mark-ring           ; marks
     search-ring regexp-search-ring))     ; searches
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place-limit 600)
  (set-mark-command-repeat-pop t) ; So we can use C-u C-SPC C-SPC C-SPC... instead of C-u C-SPC C-u C-SPC...
  (split-width-threshold 170)     ; So vertical splits are preferred
  (split-height-threshold nil)
  (shr-use-colors nil)
  (switch-to-buffer-obey-display-actions t)
  (tab-always-indent 'complete)
  (tab-width 4)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-hints t)
  (treesit-font-lock-level 4)
  (truncate-lines t)
  (undo-limit (* 13 160000))
  (undo-strong-limit (* 13 240000))
  (undo-outer-limit (* 13 24000000))
  (use-dialog-box nil)
  (use-file-dialog nil)
  (use-short-answers t)
  (visible-bell nil)
  (window-combination-resize t)
  (window-resize-pixelwise nil)
  (xref-search-program 'ripgrep)
  (grep-command "rg -nS --no-heading ")
  (grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))
  :config
  ;; Makes everything accept utf-8 as default, so buffers with tsx and so
  ;; won't ask for encoding (because undecided-unix) every single keystroke
  (modify-coding-system-alist 'file "" 'utf-8)

  (set-face-attribute 'default nil :family "FiraCode Nerd Font Mono" :height 105)

  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls")
    (setq mac-command-modifier 'meta)
    (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 140))

  ;; Save manual customizations to other file than init.el
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)


  ;; Set line-number-mode with relative numbering
  (setq display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)


  ;; Add option "d" to whenever using C-x s or C-x C-c, allowing a quick preview
  ;; of the diff of what you're asked to save.
  (add-to-list 'save-some-buffers-action-alist
               (list "d"
                     (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                     "show diff between the buffer and its file"))

  ;; On Terminal: changes the vertical separator to a full vertical line
  ;;              and truncation symbol to a right arrow
  (set-display-table-slot standard-display-table 'vertical-border ?\u2502)
  (set-display-table-slot standard-display-table 'truncation ?\u2192)

  ;; Ibuffer filters
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("org" (or
                   (mode . org-mode)
                   (name . "^\\*Org Src")
                   (name . "^\\*Org Agenda\\*$")))
           ("tramp" (name . "^\\*tramp.*"))
           ("emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")
                     (name . "^\\*Warnings\\*$")
                     (name . "^\\*Shell Command Output\\*$")
                     (name . "^\\*Async-native-compile-log\\*$")
                     (name . "^\\*straight-")))
           ("ediff" (or
                     (name . "^\\*ediff.*")
                     (name . "^\\*Ediff.*")))
           ("dired" (mode . dired-mode))
           ("terminal" (or
                        (mode . term-mode)
                        (mode . shell-mode)
                        (mode . eshell-mode)))
           ("help" (or
                    (name . "^\\*Help\\*$")
                    (name . "^\\*info\\*$")
                    (name . "^\\*helpful"))))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))
  (setq ibuffer-show-empty-filter-groups nil) ; don't show empty groups


  ;; So eshell git commands open an instance of THIS config of Emacs
  (setenv "GIT_EDITOR" (format "emacs --init-dir=%s " (shell-quote-argument user-emacs-directory)))
  ;; So rebase from eshell opens with a bit of syntax highlight
  (add-to-list 'auto-mode-alist '("/git-rebase-todo\\'" . conf-mode))


  ;; Runs 'private.el' after Emacs inits
  (add-hook 'after-init-hook
            (lambda ()
              (let ((private-file (expand-file-name "private.el" user-emacs-directory)))
                (when (file-exists-p private-file)
                  (load private-file)))))

  :init
  (set-window-margins (selected-window) 2 0)

  (toggle-frame-maximized)
  (select-frame-set-input-focus (selected-frame))
  (global-auto-revert-mode 1)
  (indent-tabs-mode -1)
  (recentf-mode 1)
  (repeat-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (winner-mode)
  (xterm-mouse-mode 1)
  (file-name-shadow-mode 1) ; allows us to type a new path without having to delete the current one

  (with-current-buffer (get-buffer-create "*scratch*")
    (insert (format ";;
;; ███████╗███╗   ███╗ █████╗  ██████╗███████╗    ███████╗ ██████╗ ██╗      ██████╗
;; ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝    ██╔════╝██╔═══██╗██║     ██╔═══██╗
;; █████╗  ██╔████╔██║███████║██║     ███████╗    ███████╗██║   ██║██║     ██║   ██║
;; ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║    ╚════██║██║   ██║██║     ██║   ██║
;; ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║    ███████║╚██████╔╝███████╗╚██████╔╝
;; ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝    ╚══════╝ ╚═════╝ ╚══════╝ ╚═════╝
;;
;;   Loading time : %s
;;   Packages     : %s
;;
"
                    (emacs-init-time)
                    (number-to-string (length package-activated-list)))))

  (message (emacs-init-time)))


;;; AUTH-SOURCE
(use-package auth-source
  :ensure nil
  :defer t
  :config
  (setq auth-sources
        (list (expand-file-name ".authinfo.gpg" user-emacs-directory)))
  (setq user-full-name "Hien Huynh-Minh"
        user-mail-address "blackcat22121996@gmail.com")

  ;; Use `pass` as an auth-source
  (when (file-exists-p "~/.password-store")
    (auth-source-pass-enable)))

;;; CONF
(use-package conf-mode
  :ensure nil
  :mode ("\\.env\\..*\\'" "\\.env\\'")
  :init
  (add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode)))

;;; COMPILATION
(use-package compile
  :ensure nil
  :hook
  (;; Not ideal, but I do not want this poluting the modeline
   (compilation-start . (lambda () (setq compilation-in-progress nil))))
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (ansi-color-for-compilation-mode t)
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

;;; WINDOW
(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(
     ;; ("\\*.*e?shell\\*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . 0.25)
     ;;  (side . bottom)
     ;;  (slot . -1))
     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\|Bookmark List\\|Occur\\|eldoc\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))
     ("\\*\\([Hh]elp\\)\\*"
      (display-buffer-in-side-window)
      (window-width . 75)
      (side . right)
      (slot . 0))
     ("\\*\\(Ibuffer\\)\\*"
      (display-buffer-in-side-window)
      (window-width . 100)
      (side . right)
      (slot . 1))
     ("\\*\\(Flymake diagnostics\\|xref\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
     ("\\*\\(grep\\|find\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 2))
     )))

;;; ICOMPLETE


;;; DIRED
(use-package dired
  :ensure nil
  :bind
  (("M-i" . emacs-solo/window-dired-vc-root-left))
  :custom
  (dired-dwim-target t)
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open")
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open")
     (".*" "xdg-open" "open")))
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-al --group-directories-first")
  :init
  (defun emacs-solo/dired-rsync-copy (dest)
  "Copy marked files in Dired to DEST using rsync asynchronously, with real-time processing of output."
  (interactive
   (list (expand-file-name (read-file-name "rsync to: "
                                           (dired-dwim-target-directory)))))
  (let* ((files (dired-get-marked-files nil current-prefix-arg))
         (command (append '("rsync" "-hPur") (mapcar #'shell-quote-argument files) (list (shell-quote-argument dest))))
         (buffer (get-buffer-create "*rsync*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Running rsync...\n"))

    (defun rsync-process-filter (proc string)
      (with-current-buffer (process-buffer proc)
        (goto-char (point-max))
        (insert string)
        (goto-char (point-max))
        (while (re-search-backward "\r" nil t)
          (replace-match "\n" nil nil))))

    (make-process
     :name "dired-rsync"
     :buffer buffer
     :command command
     :filter 'rsync-process-filter
     :sentinel
     (lambda (_proc event)
       (when (string-match-p "finished" event)
         (with-current-buffer buffer
           (goto-char (point-max))
           (insert "\n* rsync done *\n"))
         (dired-revert)))
     :stderr buffer)

    (display-buffer buffer)
    (message "rsync started...")))


  (defun emacs-solo/window-dired-vc-root-left (&optional directory-path)
    "Creates *Dired-Side* like an IDE side explorer"
    (interactive)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)

    (let ((dir (if directory-path
                   (dired-noselect directory-path)
         (if (eq (vc-root-dir) nil)
                     (dired-noselect default-directory)
                   (dired-noselect (vc-root-dir))))))

      (display-buffer-in-side-window
       dir `((side . left)
         (slot . 0)
         (window-width . 30)
         (window-parameters . ((no-other-window . t)
                   (no-delete-other-windows . t)
                   (mode-line-format . (" "
                            "%b"))))))
      (with-current-buffer dir
    (let ((window (get-buffer-window dir)))
          (when window
            (select-window window)
        (rename-buffer "*Dired-Side*")
        )))))

  (defun emacs-solo/window-dired-open-directory ()
    "Open the current directory in *Dired-Side* side window."
    (interactive)
    (emacs-solo/window-dired-vc-root-left (dired-get-file-for-visit)))

  (eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "C-<return>") 'emacs-solo/window-dired-open-directory))))


;;; WDIRED
(use-package wdired
  :ensure nil
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))


;;; ESHELL
(use-package eshell
  :ensure nil
  :defer t
  :config
  (defun emacs-solo/eshell-pick-history ()
    "Show Eshell history in a completing-read picker and insert the selected command."
    (interactive)
    (let* ((history-file (expand-file-name "eshell/history" user-emacs-directory))
           (history-entries (when (file-exists-p history-file)
                              (with-temp-buffer
                                (insert-file-contents history-file)
                                (split-string (buffer-string) "\n" t))))
           (selection (completing-read "Eshell History: " history-entries)))
      (when selection
        (insert selection))))


  (defun eshell/cat-with-syntax-highlighting (filename)
    "Like cat(1) but with syntax highlighting.
  Stole from aweshell"
    (let ((existing-buffer (get-file-buffer filename))
          (buffer (find-file-noselect filename)))
      (eshell-print
       (with-current-buffer buffer
         (if (fboundp 'font-lock-ensure)
             (font-lock-ensure)
           (with-no-warnings
             (font-lock-fontify-buffer)))
         (let ((contents (buffer-string)))
           (remove-text-properties 0 (length contents) '(read-only nil) contents)
           contents)))
      (unless existing-buffer
        (kill-buffer buffer))
      nil))
  (advice-add 'eshell/cat :override #'eshell/cat-with-syntax-highlighting)


  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c l") #'emacs-solo/eshell-pick-history)
              (local-set-key (kbd "C-l")
                             (lambda ()
                               (interactive)
                               (eshell/clear 1)
                               (eshell-send-input)))))

  (require 'vc)
  (require 'vc-git)
  (setopt eshell-prompt-function
        (lambda ()
          (concat
           "┌─("
           (if (> eshell-last-command-status 0)
               "❌"
             "🐂")
           " " (number-to-string eshell-last-command-status)
           ")──("
           "🧘 " (or (file-remote-p default-directory 'user) (user-login-name))
           ")──("
           "💻 " (or (file-remote-p default-directory 'host) (system-name))
           ")──("
           "🕝 " (format-time-string "%H:%M:%S" (current-time))
           ")──("
           "📁 "
           (concat (if (>= (length (eshell/pwd)) 40)
                       (concat "..." (car (last (butlast (split-string (eshell/pwd) "/") 0))))
                     (abbreviate-file-name (eshell/pwd))))
           ")\n"

           (when (and (fboundp 'vc-git-root) (vc-git-root default-directory))
             (concat
              "├─(🌿 " (car (vc-git-branches))
              (let* ((branch (car (vc-git-branches)))
                     (behind (string-to-number
                              (shell-command-to-string
                               (concat "git rev-list --count HEAD..origin/" branch)))))
                (if (> behind 0)
                    (concat "  ⬇️ " (number-to-string behind))))

              (let ((modified (length (split-string
                                       (shell-command-to-string
                                        "git ls-files --modified") "\n" t)))
                    (untracked (length (split-string
                                        (shell-command-to-string
                                         "git ls-files --others --exclude-standard") "\n" t))))
                (concat
                 (if (> modified 0)
                     (concat "  ✏️ " (number-to-string modified)))
                 (if (> untracked 0)
                     (concat "  📄 " ))))
              ")\n"))
           "└─➜ ")))

  (setq eshell-prompt-regexp "└─➜ ")

  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))

  (setq eshell-visual-commands
        '("vi" "screen" "top"  "htop" "btm" "less" "more" "lynx" "ncftp" "pine" "tin" "trn"
          "elm" "irssi" "nmtui-connect" "nethack" "vim" "alsamixer" "nvim" "w3m"
          "ncmpcpp" "newsbeuter" "nethack" "mutt")))

;;; VC
(use-package vc
  :ensure nil
  :defer t
  :config
  (setopt
   vc-git-diff-switches '("--patch-with-stat" "--histogram")  ;; add stats to `git diff'
   vc-git-log-switches '("--stat")                            ;; add stats to `git log'
   vc-git-log-edit-summary-target-len 50
   vc-git-log-edit-summary-max-len 70
   vc-git-print-log-follow t
   vc-git-revision-complete-only-branches nil
   vc-annotate-display-mode 'scale
   add-log-keep-changes-together t
   vc-make-backup-files nil)                                  ;; Do not backup version controlled files

  (with-eval-after-load 'vc-annotate
    (setopt vc-annotate-color-map
          '((20 . "#c3e88d")
            (40 . "#89DDFF")
            (60 . "#82aaff")
            (80 . "#676E95")
            (100 . "#c792ea")
            (120 . "#f78c6c")
            (140 . "#79a8ff")
            (160 . "#f5e0dc")
            (180 . "#a6e3a1")
            (200 . "#94e2d5")
            (220 . "#89dceb")
            (240 . "#74c7ec")
            (260 . "#82aaff")
            (280 . "#b4befe")
            (300 . "#b5b0ff")
            (320 . "#8c9eff")
            (340 . "#6a81ff")
            (360 . "#5c6bd7"))))

  ;; This one is for editing commit messages
  (require 'log-edit)
  (setopt log-edit-confirm 'changed
          log-edit-keep-buffer nil
          log-edit-require-final-newline t
          log-edit-setup-add-author nil)

  ;; Removes the bottom window with modified files list
  (remove-hook 'log-edit-hook #'log-edit-show-files)

  (with-eval-after-load 'vc-dir
    ;; In vc-git and vc-dir for git buffers, make (C-x v) a run git add, u run git
    ;; reset, and r run git reset and checkout from head.
    (defun emacs-solo/vc-git-command (verb fn)
      "Execute a Git command with VERB as action description and FN as operation on files."
      (let* ((fileset (vc-deduce-fileset t)) ;; Deduce fileset
             (backend (car fileset))
             (files (nth 1 fileset)))
        (if (eq backend 'Git)
            (progn
              (funcall fn files)
              (message "%s %d file(s)." verb (length files)))
          (message "Not in a VC Git buffer."))))

    (defun emacs-solo/vc-git-add (&optional revision vc-fileset comment)
      (interactive "P")
      (emacs-solo/vc-git-command "Staged" 'vc-git-register))

    (defun emacs-solo/vc-git-reset (&optional revision vc-fileset comment)
      (interactive "P")
      (emacs-solo/vc-git-command "Unstaged"
                                 (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))))


    ;; Bind S and U in vc-dir-mode-map
    (define-key vc-dir-mode-map (kbd "S") #'emacs-solo/vc-git-add)
    (define-key vc-dir-mode-map (kbd "U") #'emacs-solo/vc-git-reset)

    ;; Bind S and U in vc-prefix-map for general VC usage
    (define-key vc-prefix-map (kbd "S") #'emacs-solo/vc-git-add)
    (define-key vc-prefix-map (kbd "U") #'emacs-solo/vc-git-reset)

    ;; Bind g to hide up to date files after refreshing in vc-dir
    (define-key vc-dir-mode-map (kbd "g")
                (lambda () (interactive) (vc-dir-refresh) (vc-dir-hide-up-to-date)))


    (defun emacs-solo/vc-git-visualize-status ()
      "Show the Git status of files in the `vc-log` buffer."
      (interactive)
      (let* ((fileset (vc-deduce-fileset t))
             (backend (car fileset))
             (files (nth 1 fileset)))
        (if (eq backend 'Git)
            (let ((output-buffer "*Git Status*"))
              (with-current-buffer (get-buffer-create output-buffer)
                (read-only-mode -1)
                (erase-buffer)
                ;; Capture the raw output including colors using 'git status --color=auto'
                (call-process "git" nil output-buffer nil "status" "-v")
                (pop-to-buffer output-buffer)))
          (message "Not in a VC Git buffer."))))

    (define-key vc-dir-mode-map (kbd "V") #'emacs-solo/vc-git-visualize-status)
    (define-key vc-prefix-map (kbd "V") #'emacs-solo/vc-git-visualize-status))

  (defun emacs-solo/vc-git-reflog ()
    "Show git reflog in a new buffer with ANSI colors and custom keybindings."
    (interactive)
    (let* ((root (vc-root-dir)) ;; Capture VC root before creating buffer
           (buffer (get-buffer-create "*vc-git-reflog*")))
      (with-current-buffer buffer
        (setq-local vc-git-reflog-root root) ;; Store VC root as a buffer-local variable
        (let ((inhibit-read-only t))
          (erase-buffer)
          (vc-git-command buffer nil nil
                          "reflog"
                          "--color=always"
                          "--pretty=format:%C(yellow)%h%Creset %C(auto)%d%Creset %Cgreen%gd%Creset %s %Cblue(%cr)%Creset")
          (goto-char (point-min))
          (ansi-color-apply-on-region (point-min) (point-max)))

        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "/") #'isearch-forward)
          (define-key map (kbd "p") #'previous-line)
          (define-key map (kbd "n") #'next-line)
          (define-key map (kbd "q") #'kill-buffer-and-window)

          (use-local-map map))

        (setq buffer-read-only t)
        (setq mode-name "Git-Reflog")
        (setq major-mode 'special-mode))
      (pop-to-buffer buffer)))
  (global-set-key (kbd "C-x v R") 'emacs-solo/vc-git-reflog)


  (defun emacs-solo/vc-pull-merge-current-branch ()
  "Pull the latest change from origin for the current branch and display output in a buffer."
  (interactive)
  (let* ((branch (vc-git--symbolic-ref "HEAD"))
         (buffer (get-buffer-create "*Git Pull Output*"))
         (command (format "git pull origin %s" branch)))
    (if branch
        (progn
          (with-current-buffer buffer
            (erase-buffer)
            (insert (format "$ %s\n\n" command))
            (call-process-shell-command command nil buffer t))
          (display-buffer buffer))
      (message "Could not determine current branch."))))


  (defun emacs-solo/vc-browse-remote (&optional current-line)
  "Open the repository's remote URL in the browser.
If CURRENT-LINE is non-nil, point to the current branch, file, and line.
Otherwise, open the repository's main page."
  (interactive "P")
  (let* ((remote-url (string-trim (vc-git--run-command-string nil "config" "--get" "remote.origin.url")))
         (branch (string-trim (vc-git--run-command-string nil "rev-parse" "--abbrev-ref" "HEAD")))
         (file (string-trim (file-relative-name (buffer-file-name) (vc-root-dir))))
         (line (line-number-at-pos)))
    (message "Opening remote on browser: %s" remote-url)
    (if (and remote-url (string-match "\\(?:git@\\|https://\\)\\([^:/]+\\)[:/]\\(.+?\\)\\(?:\\.git\\)?$" remote-url))
        (let ((host (match-string 1 remote-url))
              (path (match-string 2 remote-url)))
          ;; Convert SSH URLs to HTTPS (e.g., git@github.com:user/repo.git -> https://github.com/user/repo)
          (when (string-prefix-p "git@" host)
            (setq host (replace-regexp-in-string "^git@" "" host)))
          ;; Construct the appropriate URL based on CURRENT-LINE
          (browse-url
           (if current-line
               (format "https://%s/%s/blob/%s/%s#L%d" host path branch file line)
             (format "https://%s/%s" host path))))
      (message "Could not determine repository URL"))))
  (global-set-key (kbd "C-x v B") 'emacs-solo/vc-browse-remote)
  (global-set-key (kbd "C-x v o")
                  '(lambda () (interactive) (emacs-solo/vc-browse-remote 1)))


  (defun emacs-solo/vc-diff-on-current-hunk ()
    "Show the diff for the current file and jump to the hunk containing the current line."
    (interactive)
    (let ((current-line (line-number-at-pos)))
      (message "Current line in file: %d" current-line)
      (vc-diff) ; Generate the diff buffer
      (with-current-buffer "*vc-diff*"
        (goto-char (point-min))
        (let ((found-hunk nil))
          (while (and (not found-hunk)
                      (re-search-forward "^@@ -\\([0-9]+\\), *[0-9]+ \\+\\([0-9]+\\), *\\([0-9]+\\) @@" nil t))
            (let* ((start-line (string-to-number (match-string 2)))
                   (line-count (string-to-number (match-string 3)))
                   (end-line (+ start-line line-count)))
              (message "Found hunk: %d to %d" start-line end-line)
              (when (and (>= current-line start-line)
                         (<= current-line end-line))
                (message "Current line %d is within hunk range %d to %d" current-line start-line end-line)
                (setq found-hunk t)
                (goto-char (match-beginning 0))))) ; Jump to the beginning of the hunk
          (unless found-hunk
            (message "Current line %d is not within any hunk range." current-line)
            (goto-char (point-min)))))))
  (global-set-key (kbd "C-x v =") 'emacs-solo/vc-diff-on-current-hunk))

;;; SMERGE
(use-package smerge-mode
  :ensure nil
  :bind (:map smerge-mode-map
              ("C-c ^ u" . smerge-keep-upper)
              ("C-c ^ l" . smerge-keep-lower)
              ("C-c ^ n" . smerge-next)
              ("C-c ^ p" . smerge-previous)))

;;; DIFF
(use-package diff-mode
  :ensure nil
  :defer t
  :config
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  (setq diff-font-lock-syntax 'hunk-also)
  (setq diff-font-lock-prettify nil))

;;; EDIFF
(use-package ediff
  :ensure nil
  :commands (ediff-buffers ediff-files ediff-buffers3 ediff-files3)
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t))

;;; ELDOC
(use-package eldoc
  :ensure nil
  :init
  (global-eldoc-mode))

;;; FLYMAKE
(use-package flymake
  :ensure nil
  :defer t
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-8" . flymake-goto-next-error)
              ("M-7" . flymake-goto-prev-error)
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! t" . toggle-flymake-diagnostics-at-eol))
  :custom
  (flymake-show-diagnostics-at-end-of-line nil)
  ;; (flymake-show-diagnostics-at-end-of-line 'short)
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string
   `((error "!" compilation-error)      ;; Alternatives: », E, W, i, !, ?)
     (warning "?" compilation-warning)
     (note "i" compilation-info)))
  :config
  ;; Define the toggle function
  (defun toggle-flymake-diagnostics-at-eol ()
    "Toggle the display of Flymake diagnostics at the end of the line
and restart Flymake to apply the changes."
    (interactive)
    (setq flymake-show-diagnostics-at-end-of-line
          (not flymake-show-diagnostics-at-end-of-line))
    (flymake-mode -1) ;; Disable Flymake
    (flymake-mode 1)  ;; Re-enable Flymake
    (message "Flymake diagnostics at end of line: %s"
             (if flymake-show-diagnostics-at-end-of-line
                 "Enabled" "Disabled"))))


;;; WHITESPACE
(use-package whitespace
  :ensure nil
  :defer t
  :hook (before-save . whitespace-cleanup)
  ;; if we wanna remove this hook at any time, eval:
  ;; (remove-hook 'before-save-hook #'whitespace-cleanup)
  )

;;; GNUS
(use-package gnus
  :ensure nil
  :defer t
  :custom
  (gnus-init-file (concat user-emacs-directory ".gnus.el"))
  (gnus-startup-file (concat user-emacs-directory ".newsrc"))
  (gnus-init-file (concat user-emacs-directory ".newsrc.eld"))
  (gnus-activate-level 3)
  (gnus-message-archive-group nil)
  (gnus-check-new-newsgroups nil)
  (gnus-check-bogus-newsgroups nil)
  (gnus-show-threads nil)
  (gnus-use-cross-reference nil)
  (gnus-nov-is-evil nil)
  (gnus-group-line-format "%1M%5y  : %(%-50,50G%)\12")
  (gnus-logo-colors '("#2fdbde" "#c0c0c0"))
  (gnus-permanently-visible-groups ".*")
  (gnus-summary-insert-entire-threads t)
  (gnus-thread-sort-functions
   '(gnus-thread-sort-by-most-recent-number
     gnus-thread-sort-by-subject
     (not gnus-thread-sort-by-total-score)
     gnus-thread-sort-by-most-recent-date))
  (gnus-summary-line-format "%U%R%z: %[%d%] %4{ %-34,34n%} %3{  %}%(%1{%B%}%s%)\12")
  (gnus-user-date-format-alist '((t . "%d-%m-%Y %H:%M")))
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
  (gnus-sum--tree-indent " ")
  (gnus-sum-thread-tree-indent " ")
  (gnus-sum-thread-tree-false-root "○ ")
  (gnus-sum-thread-tree-single-indent "◎ ")
  (gnus-sum-thread-tree-leaf-with-other "├► ")
  (gnus-sum-thread-tree-root "● ")
  (gnus-sum-thread-tree-single-leaf "╰► ")
  (gnus-sum-thread-tree-vertical "│)")
  (gnus-select-method '(nnnil nil))
  (gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  (gnus-secondary-select-methods
   '((nntp "news.gwene.org"))))

;;; MAN
(use-package man
  :ensure nil
  :commands (man)
  :config
  (setq Man-notify-method 'pushy)) ; does not obey `display-buffer-alist'


;;; MINIBUFFER
(use-package minibuffer
  :ensure nil
  :custom
  (completion-styles '(partial-completion flex initials))
  (completion-ignore-case t)
  (completion-show-help t)
  ;; (completion-auto-select t) ;; NOTE: only turn this on if not using icomplete, can also be 'second-tab
  (completions-max-height 20)
  (completions-format 'one-column)
  (enable-recursive-minibuffers t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  :config
  ;; Keep the cursor out of the read-only portions of the.minibuffer
  (setq minibuffer-prompt-properties
        '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Keep minibuffer lines unwrapped, long lines like on M-y will be truncated
  (add-hook 'minibuffer-setup-hook
          (lambda () (setq truncate-lines t)))

  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

;;; ELEC_PAIR
(use-package elec-pair
  :ensure nil
  :defer
  :hook (after-init . electric-pair-mode))

;;; PAREN
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-style 'mixed)
  (show-paren-context-when-offscreen t)) ;; show matches within window splits

;;; PROCED
(use-package proced
  :ensure nil
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 1)
  (proced-descent t)
  (proced-filter 'user) ;; We can change interactively with `s'
  :config
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))



;;; TIME
(use-package time
  :ensure nil
  ;; :hook (after-init . display-time-mode) ;; If we'd like to see it on the modeline
  :custom
  (world-clock-time-format "%A %d %B %r %Z")
  (display-time-day-and-date t)
  (display-time-default-load-average nil)
  (display-time-mail-string "")
  (zoneinfo-style-world-list                ; use `M-x worldclock RET' to see it
   '(("America/Los_Angeles" "Los Angeles")
     ("America/Vancouver" "Vancouver")
     ("Canada/Pacific" "Canada/Pacific")
     ("America/Chicago" "Chicago")
     ("America/Toronto" "Toronto")
     ("America/New_York" "New York")
     ("Canada/Atlantic" "Canada/Atlantic")
     ("Brazil/East" "Brasília")
     ("America/Sao_Paulo" "São Paulo")
     ("UTC" "UTC")
     ("Europe/Lisbon" "Lisbon")
     ("Europe/Brussels" "Brussels")
     ("Europe/Athens" "Athens")
     ("Asia/Riyadh" "Riyadh")
     ("Asia/Tehran" "Tehran")
     ("Asia/Tbilisi" "Tbilisi")
     ("Asia/Yekaterinburg" "Yekaterinburg")
     ("Asia/Kolkata" "Kolkata")
     ("Asia/Singapore" "Singapore")
     ("Asia/Shanghai" "Shanghai")
     ("Asia/Seoul" "Seoul")
     ("Asia/Tokyo" "Tokyo")
     ("Asia/Vladivostok" "Vladivostok")
     ("Australia/Brisbane" "Brisbane")
     ("Australia/Sydney" "Sydney")
     ("Pacific/Auckland" "Auckland"))))


;;; UNIQUIFY
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;;; WHICH-KEY
(use-package which-key
  :defer t
  :ensure nil
  :hook
  (after-init . which-key-mode)
  :config
  (setq which-key-separator "  ")
  (setq which-key-prefix-prefix "... ")
  (setq which-key-max-display-columns 3)
  (setq which-key-idle-delay 1.5)
  (setq which-key-idle-secondary-delay 0.25)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-description-length 40))

;;; WEBJUMP
(use-package webjump
  :defer t
  :ensure nil
  :bind ("C-x /" . webjump)
  :custom
  (webjump-sites
   '(("Google" . [simple-query "www.google.com" "www.google.com/search?q=" ""])
     ("Translate" . [simple-query "translate.google.com" "https://translate.google.com/?sl=auto&tl=vi&text=" "&op=translate"])
     ("YouTube" . [simple-query "www.youtube.com/feed/subscriptions" "www.youtube.com/rnesults?search_query=" ""])
     ("ChatGPT" . [simple-query "https://chatgpt.com" "https://chatgpt.com/?q=" ""]))))

 ;;; THEMES


;;; -------------------- NON TREESITTER AREA
;;; SASS-MODE
(use-package scss-mode
  :mode "\\.sass\\'"
  :defer t)



;;; ------------------- EMACS-SOLO CUSTOMS
;;; EMACS-SOLO-HOOKS
;;
(use-package emacs-solo-hooks
  :ensure nil
  :no-require t
  :defer t
  :init

  (defun emacs-solo/prefer-tabs ()
    "Disables indent-tabs-mode, and prefer spaces over tabs."
    (interactive)
    (indent-tabs-mode -1))

  (add-hook 'prog-mode-hook #'emacs-solo/prefer-tabs))


;;; EMACS-SOLO-MOVEMENTS
;;
;;  Functions to better move around text and Emacs
;;
(use-package emacs-solo-movements
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/rename-buffer-and-move-to-new-window ()
    "Promotes a side buffer to a new window."
    (interactive)
    (let ((temp-name (make-temp-name "temp-buffer-")))
      (rename-buffer temp-name t)
      (delete-window)
      (split-window-right)
      (switch-to-buffer temp-name)))

  (global-set-key (kbd "C-x x x") 'emacs-solo/rename-buffer-and-move-to-new-window)


  (defun emacs-solo-movements/scroll-down-centralize ()
    (interactive)
    (scroll-up-command)
    (recenter))

  (defun emacs-solo-movements/scroll-up-centralize ()
    (interactive)
    (scroll-down-command)
    (unless (= (window-start) (point-min))
      (recenter))
    (when (= (window-start) (point-min))
      (let ((midpoint (/ (window-height) 2)))
        (goto-char (window-start))
        (forward-line midpoint)
        (recenter midpoint))))

  (global-set-key (kbd "C-v") #'emacs-solo-movements/scroll-down-centralize)
  (global-set-key (kbd "M-v") #'emacs-solo-movements/scroll-up-centralize)


  (defun emacs-solo-movements/format-current-file ()
    "Format the current file using biome if biome.json is present; otherwise, use prettier.
Also first tries the local node_modules/.bin and later the global bin."
    (interactive)
    (let* ((file (buffer-file-name))
           (project-root (locate-dominating-file file "node_modules"))
           (biome-config (and project-root (file-exists-p (expand-file-name "biome.json" project-root))))
           (local-biome (and project-root (expand-file-name "node_modules/.bin/biome" project-root)))
           (global-biome (executable-find "biome"))
           (local-prettier (and project-root (expand-file-name "node_modules/.bin/prettier" project-root)))
           (global-prettier (executable-find "prettier"))
           (formatter nil)
           (source nil)
           (command nil)
           (start-time (float-time))) ;; Capture the start time
      (cond
       ;; Use Biome if biome.json exists
       ((and biome-config local-biome (file-executable-p local-biome))
        (setq formatter local-biome)
        (setq source "biome (local)")
        (setq command (format "%s format --write %s" formatter (shell-quote-argument file))))
       ((and biome-config global-biome)
        (setq formatter global-biome)
        (setq source "biome (global)")
        (setq command (format "%s format --write %s" formatter (shell-quote-argument file))))

       ;; Fall back to Prettier if no biome.json
       ((and local-prettier (file-executable-p local-prettier))
        (setq formatter local-prettier)
        (setq source "prettier (local)")
        (setq command (format "%s --write %s" formatter (shell-quote-argument file))))
       ((and global-prettier)
        (setq formatter global-prettier)
        (setq source "prettier (global)")
        (setq command (format "%s --write %s" formatter (shell-quote-argument file)))))
      (if command
          (progn
            (save-buffer)
            (shell-command command)
            (revert-buffer t t t)
            (let ((elapsed-time (* 1000 (- (float-time) start-time)))) ;; Calculate elapsed time in ms
              (message "Formatted with %s - %.2f ms" source elapsed-time)))
        (message "No formatter found (biome or prettier)"))))

  (global-set-key (kbd "C-c p") #'emacs-solo-movements/format-current-file)


  (defun emacs-solo/transpose-split ()
    "Transpose a horizontal split into a vertical split, or vice versa."
    (interactive)
    (if (> (length (window-list)) 2)
        (user-error "More than two windows present")
      (let* ((this-win (selected-window))
             (other-win (next-window))
             (this-buf (window-buffer this-win))
             (other-buf (window-buffer other-win))
             (this-edges (window-edges this-win))
             (other-edges (window-edges other-win))
             (this-left (car this-edges))
             (other-left (car other-edges))
             (split-horizontally (not (= this-left other-left))))
        (delete-other-windows)
        (if split-horizontally
            (split-window-vertically)
          (split-window-horizontally))
        (set-window-buffer (selected-window) this-buf)
        (set-window-buffer (next-window) other-buf)
        (select-window this-win))))

  (global-set-key (kbd "C-x 4 t") #'emacs-solo/transpose-split))


;;; EMACS-SOLO-TRANSPARENCY
;;
;;  Custom functions to set/unset transparency
;;
(use-package emacs-solo-transparency
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/clear-terminal-background-color (&optional frame)
    (interactive)
    (or frame (setq frame (selected-frame)))
    "unsets the background color in terminal mode"
    (unless (display-graphic-p frame)
      ;; Set the terminal to a transparent version of the background color
      (send-string-to-terminal
       (format "\033]11;[90]%s\033\\"
               (face-attribute 'default :background)))
      (set-face-background 'default "unspecified-bg" frame)))

  (defun emacs-solo/transparency-set ()
    "Set frame transparency (Graphical Mode)."
    (interactive)
    (unless (display-graphic-p)
        (add-hook 'after-make-frame-functions 'emacs-solo/clear-terminal-background-color)
        (add-hook 'window-setup-hook 'emacs-solo/clear-terminal-background-color)
        (add-hook 'ef-themes-post-load-hook 'emacs-solo/clear-terminal-background-color))

    (when (eq system-type 'darwin)
      (set-frame-parameter (selected-frame) 'alpha '(90 90)))

    (dolist (frame (frame-list))
      (set-frame-parameter frame 'alpha-background 85)))


  (defun emacs-solo/transparency-unset ()
    "Unset frame transparency (Graphical Mode)."
    (interactive)
    (when (eq system-type 'darwin)
      (set-frame-parameter (selected-frame) 'alpha '(100 100)))
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'alpha-background 100)))

  (add-hook 'after-init-hook #'emacs-solo/transparency-set))


;;; EMACS-SOLO-MODE-LINE
;;
;;  Customizations to the mode-line
;;
(use-package emacs-solo-mode-line
  :ensure nil
  :no-require t
  :defer t
  :init
  ;; Shorten big branches names
  (defun emacs-solo/shorten-vc-mode (vc)
    "Shorten VC string to at most 20 characters.
 Replacing `Git-' with a branch symbol."
    (let* ((vc (replace-regexp-in-string "^ Git[:-]" "  " vc))) ;; Options:   ᚠ ⎇
      (if (> (length vc) 20)
          (concat (substring vc 0 20) "…")
        vc)))

  ;; Formats Modeline
  (setq-default mode-line-format
                '("%e" "  "
                  ;; (:propertize " " display (raise +0.1)) ;; Top padding
                  ;; (:propertize " " display (raise -0.1)) ;; Bottom padding
                  (:propertize "λ  " face font-lock-keyword-face)

                  (:propertize
                   ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))

                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "   "
                  mode-line-position
                  mode-line-format-right-align
                  "  "
                  (project-mode-line project-mode-line-format)
                  "  "
                  (vc-mode (:eval (emacs-solo/shorten-vc-mode vc-mode)))
                  "  "
                  mode-line-modes
                  mode-line-misc-info
                  "  ")
                project-mode-line t
                mode-line-buffer-identification '(" %b")
                mode-line-position-column-line-format '(" %l:%c"))

  ;; Provides the Diminish functionality
  (defvar emacs-solo-hidden-minor-modes
    '(abbrev-mode
      eldoc-mode
      flyspell-mode
      smooth-scroll-mode
      outline-minor-mode
      which-key-mode))

  (defun emacs-solo/purge-minor-modes ()
    (interactive)
    (dolist (x emacs-solo-hidden-minor-modes nil)
      (let ((trg (cdr (assoc x minor-mode-alist))))
        (when trg
          (setcar trg "")))))

  (add-hook 'after-change-major-mode-hook 'emacs-solo/purge-minor-modes))


;;; EMACS-SOLO-EXEC-PATH-FROM-SHELL
;;
;;  Loads users default shell PATH settings into Emacs. Usefull
;;  when calling Emacs directly from GUI systems.
;;
(use-package emacs-solo-exec-path-from-shell
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment the same as user Shell."
    (interactive)
    (let ((path-from-shell
           (replace-regexp-in-string
            "[ \t\n]*$" "" (shell-command-to-string
                            "$SHELL --login -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))
      (message ">>> emacs-solo: PATH loaded")))

  (defun emacs-solo/fix-asdf-path ()
  "Ensure asdf shims and active Node.js version's bin directory are first in PATH."
  (interactive)
  (let* ((asdf-shims (expand-file-name "~/.asdf/shims"))
         (node-bin (string-trim (shell-command-to-string "asdf where nodejs 2>/dev/null")))
         (new-paths (list asdf-shims)))

    ;; If Node.js is installed, add its bin path
    (when (file-directory-p node-bin)
      (push (concat node-bin "/bin") new-paths))

    ;; Remove old asdf-related paths from PATH and exec-path
    (setq exec-path (seq-remove (lambda (p) (string-match-p "/\\.asdf/" p)) exec-path))
    (setenv "PATH" (string-join (seq-remove (lambda (p) (string-match-p "/\\.asdf/" p))
                                            (split-string (getenv "PATH") ":"))
                                ":"))

    ;; Add the new paths to exec-path and PATH
    (dolist (p (reverse new-paths))
      (unless (member p exec-path) (push p exec-path))
      (unless (member p (split-string (getenv "PATH") ":"))
        (setenv "PATH" (concat p ":" (getenv "PATH")))))))

  (add-hook 'find-file-hook #'emacs-solo/fix-asdf-path)
  (add-hook 'eshell-mode-hook #'emacs-solo/fix-asdf-path)
  (add-hook 'eshell-pre-command-hook #'emacs-solo/fix-asdf-path)
  (add-hook 'eshell-directory-change-hook #'emacs-solo/fix-asdf-path)

  (add-hook 'after-init-hook #'emacs-solo/set-exec-path-from-shell-PATH)
  (add-hook 'after-init-hook #'emacs-solo/fix-asdf-path))


;;; EMACS-SOLO-RAINBOW-DELIMITERS
;;
;;  Colorizes matching delimiters
;;
;;  FIXME: Make it play nice with treesitter modes
;;
(use-package emacs-solo-rainbow-delimiters
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/rainbow-delimiters ()
    "Apply simple rainbow coloring to parentheses, brackets, and braces in the current buffer.
Opening and closing delimiters will have matching colors."
    (interactive)
    (let ((colors '(font-lock-keyword-face
                    font-lock-type-face
                    font-lock-function-name-face
                    font-lock-variable-name-face
                    font-lock-constant-face
                    font-lock-builtin-face
                    font-lock-string-face
                    )))
      (font-lock-add-keywords
       nil
       `((,(rx (or "(" ")" "[" "]" "{" "}"))
          (0 (let* ((char (char-after (match-beginning 0)))
                    (depth (save-excursion
                             ;; Move to the correct position based on opening/closing delimiter
                             (if (member char '(?\) ?\] ?\}))
                                 (progn
                                   (backward-char) ;; Move to the opening delimiter
                                   (car (syntax-ppss)))
                               (car (syntax-ppss)))))
                    (face (nth (mod depth ,(length colors)) ',colors)))
               (list 'face face)))))))
    (font-lock-flush)
    (font-lock-ensure))

  (add-hook 'prog-mode-hook #'emacs-solo/rainbow-delimiters))


;;; EMACS-SOLO-PROJECT-SELECT
;;
;;  Interactively finds a project in a Projects folder and sets it
;;  to current `project.el' project.
;;
(use-package emacs-solo-project-select
  :ensure nil
  :no-require t
  :init
  (defvar emacs-solo-default-projects-folder "~/projects"
    "Default folder to search for projects.")

  (defvar emacs-solo-default-projects-input "**"
    "Default input to use when finding a project.")

  (defun emacs-solo/find-projects-and-switch (&optional directory)
    "Find and switch to a project directory from ~/Projects."
    (interactive)
    (let* ((d (or directory emacs-solo-default-projects-folder))
           ;; (find-command (concat "fd --type d --max-depth 4 . " d))           ; with fd
           (find-command (concat "find " d " -mindepth 1 -maxdepth 4 -type d"))  ; with find
           (project-list (split-string (shell-command-to-string find-command) "\n" t))
           (initial-input emacs-solo-default-projects-input))
      (let ((selected-project
             (completing-read
              "Search project folder: "
              project-list
              nil nil
              initial-input)))
        (when (and selected-project (file-directory-p selected-project))
          (project-switch-project selected-project)))))

  (defun emacs-solo/minibuffer-move-cursor ()
    "Move cursor between `*` characters when minibuffer is populated with `**`."
    (when (string-prefix-p emacs-solo-default-projects-input (minibuffer-contents))
      (goto-char (+ (minibuffer-prompt-end) 1))))

  (add-hook 'minibuffer-setup-hook #'emacs-solo/minibuffer-move-cursor)

  :bind (:map project-prefix-map
         ("P" . emacs-solo/find-projects-and-switch)))

;;; EMACS-SOLO-HIGHLIGHT-KEYWORDS-MODE
;;
;;  Highlights a list of words like TODO, FIXME...
;;  Code borrowed from `alternateved'
;;
(use-package emacs-solo-highlight-keywords-mode
  :ensure nil
  :no-require t
  :defer t
  :init
  (defcustom +highlight-keywords-faces
    '(("TODO" . error)
      ("FIXME" . error)
      ("HACK" . warning)
      ("NOTE" . warning)
      ("HERE" . compilation-info))
    "Alist of keywords to highlight and their face."
    :group '+highlight-keywords
    :type '(alist :key-type (string :tag "Keyword")
                  :value-type (symbol :tag "Face"))
    :set (lambda (sym val)
           (dolist (face (mapcar #'cdr val))
             (unless (facep face)
               (error "Invalid face: %s" face)))
           (set-default sym val)))

  (defvar +highlight-keywords--keywords
    (when +highlight-keywords-faces
      (let ((keywords (mapcar #'car +highlight-keywords-faces)))
        `((,(regexp-opt keywords 'words)
           (0 (when (nth 8 (syntax-ppss))
                (cdr (assoc (match-string 0) +highlight-keywords-faces)))
              prepend)))))
    "Keywords and corresponding faces for `emacs-solo/highlight-keywords-mode'.")

  (defun emacs-solo/highlight-keywords-mode-on ()
    (font-lock-add-keywords nil +highlight-keywords--keywords t)
    (font-lock-flush))

  (defun emacs-solo/highlight-keywords-mode-off ()
    (font-lock-remove-keywords nil +highlight-keywords--keywords)
    (font-lock-flush))

  (define-minor-mode emacs-solo/highlight-keywords-mode
    "Highlight TODO and similar keywords in comments and strings."
    :lighter " +HL"
    :group '+highlight-keywords
    (if emacs-solo/highlight-keywords-mode
        (emacs-solo/highlight-keywords-mode-on)
      (emacs-solo/highlight-keywords-mode-off)))

  :hook
  (prog-mode . (lambda () (run-at-time "1 sec" nil #'emacs-solo/highlight-keywords-mode-on))))


;;; EMACS-SOLO-GUTTER
;;
;;  A **HIGHLY** `experimental' and slow and buggy git gutter like.
;;
(use-package emacs-solo-gutter
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/goto-next-hunk ()
    "Jump cursor to the closest next hunk."
    (interactive)
    (let* ((current-line (line-number-at-pos))
           (line-numbers (mapcar #'car git-gutter-diff-info))
           (sorted-line-numbers (sort line-numbers '<))
           (next-line-number
            (if (not (member current-line sorted-line-numbers))
                ;; If the current line is not in the list, find the next closest line number
                (cl-find-if (lambda (line) (> line current-line)) sorted-line-numbers)
              ;; If the current line is in the list, find the next line number that is not consecutive
              (let ((last-line nil))
                (cl-loop for line in sorted-line-numbers
                         when (and (> line current-line)
                                   (or (not last-line)
                                       (/= line (1+ last-line))))
                         return line
                         do (setq last-line line))))))

      (when next-line-number
        (goto-line next-line-number))))

  (defun emacs-solo/goto-previous-hunk ()
    "Jump cursor to the closest previous hunk."
    (interactive)
    (let* ((current-line (line-number-at-pos))
           (line-numbers (mapcar #'car git-gutter-diff-info))
           (sorted-line-numbers (sort line-numbers '<))
           (previous-line-number
            (if (not (member current-line sorted-line-numbers))
                ;; If the current line is not in the list, find the previous closest line number
                (cl-find-if (lambda (line) (< line current-line)) (reverse sorted-line-numbers))
              ;; If the current line is in the list, find the previous line number that has no direct predecessor
              (let ((previous-line nil))
                (dolist (line sorted-line-numbers)
                  (when (and (< line current-line)
                             (not (member (1- line) line-numbers)))
                    (setq previous-line line)))
                previous-line))))

      (when previous-line-number
        (goto-line previous-line-number))))


  (defun emacs-solo/git-gutter-process-git-diff ()
    "Process git diff for adds/mods/removals.
Marks lines as added, deleted, or changed."
    (interactive)
    (setq-local result '())
    (let* ((file-path (buffer-file-name))
           (grep-command "rg -Po")                         ; for rgrep
           ;; (grep-command (if (eq system-type 'darwin)   ; for grep / ggrep
           ;;                   "ggrep -Po"
           ;;                 "grep -Po"))
           (output (shell-command-to-string
                    (format
                     "git diff --unified=0 %s | %s '^@@ -[0-9]+(,[0-9]+)? \\+\\K[0-9]+(,[0-9]+)?(?= @@)'"
                     file-path
                     grep-command))))
      (setq-local lines (split-string output "\n"))
      (dolist (line lines)
        (if (string-match "\\(^[0-9]+\\),\\([0-9]+\\)\\(?:,0\\)?$" line)
            (let ((num (string-to-number (match-string 1 line)))
                  (count (string-to-number (match-string 2 line))))
              (if (= count 0)
                  (add-to-list 'result (cons (+ 1 num) "deleted"))
                (dotimes (i count)
                  (add-to-list 'result (cons (+ num i) "changed")))))
          (if (string-match "\\(^[0-9]+\\)$" line)
              (add-to-list 'result (cons (string-to-number line) "added"))))
        (setq-local git-gutter-diff-info result))
      result))


  (defun emacs-solo/git-gutter-add-mark (&rest args)
    "Add symbols to the left margin based on Git diff statuses.
   - '+' for added lines (lightgreen)
   - '~' for changed lines (yellowish)
   - '-' for deleted lines (tomato)."
    (interactive)
    (set-window-margins (selected-window) 2 0) ;; change to 1,2,3 if you want more columns
    (remove-overlays (point-min) (point-max) 'emacs-solo--git-gutter-overlay t)
    (let ((lines-status (or (emacs-solo/git-gutter-process-git-diff) '())))
      (save-excursion
        (dolist (line-status lines-status)
          (let ((line-num (car line-status))
                (status (cdr line-status)))
            (when (and line-num status)
              (goto-char (point-min))
              (forward-line (1- line-num))
              (let ((overlay (make-overlay (point-at-bol) (point-at-bol))))
                (overlay-put overlay 'emacs-solo--git-gutter-overlay t)
                (overlay-put overlay 'before-string
                             (propertize " "
                                         'display
                                         `((margin left-margin)
                                           ,(propertize
                                             (cond                              ;; Alternatives:
                                              ((string= status "added")   "│")  ;; +  │ ▏┃
                                              ((string= status "changed") "│")  ;; ~
                                              ((string= status "deleted") "│")) ;; _
                                             'face
                                             `(:foreground
                                               ,(cond
                                                 ((string= status "added") "lightgreen")
                                                 ((string= status "changed") "gold")
                                                 ((string= status "deleted") "tomato"))))))))))))))

  (defun emacs-solo/timed-git-gutter-on()
    (run-at-time 0.1 nil #'emacs-solo/git-gutter-add-mark))

  (defun emacs-solo/git-gutter-off ()
    "Remove all `emacs-solo--git-gutter-overlay' marks and other overlays."
    (interactive)
    (set-window-margins (selected-window) 2 0)
    (remove-overlays (point-min) (point-max) 'emacs-solo--git-gutter-overlay t)
    (remove-hook 'find-file-hook #'emacs-solo-git-gutter-on)
    (remove-hook 'after-save-hook #'emacs-solo/git-gutter-add-mark))

  (defun emacs-solo/git-gutter-on ()
    (interactive)
    (emacs-solo/git-gutter-add-mark)
    (add-hook 'find-file-hook #'emacs-solo/timed-git-gutter-on)
    (add-hook 'after-save-hook #'emacs-solo/git-gutter-add-mark))

  (global-set-key (kbd "M-9") 'emacs-solo/goto-previous-hunk)
  (global-set-key (kbd "M-0") 'emacs-solo/goto-next-hunk)
  (global-set-key (kbd "C-c g p") 'emacs-solo/goto-previous-hunk)
  (global-set-key (kbd "C-c g r") 'emacs-solo/git-gutter-off)
  (global-set-key (kbd "C-c g g") 'emacs-solo/git-gutter-on)
  (global-set-key (kbd "C-c g n") 'emacs-solo/goto-next-hunk)

  (add-hook 'after-init-hook #'emacs-solo/git-gutter-on))


;;; EMACS-SOLO-ACE-WINDOW
;;
;;  Based on: https://www.reddit.com/r/emacs/comments/1h0zjvq/comment/m0uy3bo/?context=3
;;
;;  TODO: implement ace-swap like feature
(use-package emacs-solo-ace-window
  :ensure nil
  :no-require t
  :defer t
  :init
  (defvar emacs-solo-ace-window/quick-window-overlays nil
    "List of overlays used to temporarily display window labels.")

  (defun emacs-solo-ace-window/quick-window-jump ()
    "Jump to a window by typing its assigned character label.
Windows are labeled starting from the top-left window and proceeding top to bottom, then left to right."
    (interactive)
    (let* ((window-list (emacs-solo-ace-window/get-windows))
           (window-keys (seq-take '("a" "s" "d" "f" "j" "k" "l" "o")
                                  (length window-list)))
           (window-map (cl-pairlis window-keys window-list)))
      (emacs-solo-ace-window/add-window-key-overlays window-map)
      (let ((key (read-key (format "Select window [%s]: " (string-join window-keys ", ")))))
        (emacs-solo-ace-window/remove-window-key-overlays)
        (if-let ((selected-window (cdr (assoc (char-to-string key) window-map))))
            (select-window selected-window)
          (message "No window assigned to key: %c" key)))))

  (defun emacs-solo-ace-window/get-windows ()
    "Return a list of windows in the current frame, ordered from top to bottom, left to right."
    (sort (window-list nil 'no-mini)
          (lambda (w1 w2)
            (let ((edges1 (window-edges w1))
                  (edges2 (window-edges w2)))
              (or (< (car edges1) (car edges2)) ; Compare top edges
                  (and (= (car edges1) (car edges2)) ; If equal, compare left edges
                       (< (cadr edges1) (cadr edges2))))))))

  (defun emacs-solo-ace-window/add-window-key-overlays (window-map)
    "Add temporary overlays to windows with their assigned key labels from WINDOW-MAP."
    (setq emacs-solo-ace-window/quick-window-overlays nil)
    (dolist (entry window-map)
      (let* ((key (car entry))
             (window (cdr entry))
             (start (window-start window))
             (overlay (make-overlay start start (window-buffer window))))
        (overlay-put overlay 'after-string
                     (propertize (format " [%s] " key)
                                 'face '(:foreground "#c3e88d"
                                         :background "#232635"
                                         :weight bold
                                         :height default)))
        (overlay-put overlay 'window window)
        (push overlay emacs-solo-ace-window/quick-window-overlays))))

  (defun emacs-solo-ace-window/remove-window-key-overlays ()
    "Remove all temporary overlays used to display key labels in windows."
    (mapc 'delete-overlay emacs-solo-ace-window/quick-window-overlays)
    (setq emacs-solo-ace-window/quick-window-overlays nil))

  (global-set-key (kbd "M-o") #'emacs-solo-ace-window/quick-window-jump))


;; ---------- EMACS-SOLO-OLIVETTI
(use-package emacs-solo-olivetti
  :ensure nil
  :no-require t
  :defer t
  :init
  (defvar emacs-solo-center-document-desired-width 90
    "The desired width of a document centered in the window.")

  (defun emacs-solo/center-document--adjust-margins ()
    ;; Reset margins first before recalculating
    (set-window-parameter nil 'min-margins nil)
    (set-window-margins nil nil)

    ;; Adjust margins if the mode is on
    (when emacs-solo/center-document-mode
      (let ((margin-width (max 0
                               (truncate
                                (/ (- (window-width)
                                      emacs-solo-center-document-desired-width)
                                   2.0)))))
        (when (> margin-width 0)
          (set-window-parameter nil 'min-margins '(0 . 0))
          (set-window-margins nil margin-width margin-width)))))

  (define-minor-mode emacs-solo/center-document-mode
    "Toggle centered text layout in the current buffer."
    :lighter " Centered"
    :group 'editing
    (if emacs-solo/center-document-mode
        (add-hook 'window-configuration-change-hook #'emacs-solo/center-document--adjust-margins 'append 'local)
      (remove-hook 'window-configuration-change-hook #'emacs-solo/center-document--adjust-margins 'local))
    (emacs-solo/center-document--adjust-margins))


  (add-hook 'org-mode-hook #'emacs-solo/center-document-mode)
  (add-hook 'gnus-group-mode-hook #'emacs-solo/center-document-mode)
  (add-hook 'gnus-summary-mode-hook #'emacs-solo/center-document-mode)
  (add-hook 'gnus-article-mode-hook #'emacs-solo/center-document-mode)

  ;; (add-hook 'newsticker-treeview-list-mode-hook 'emacs-solo/timed-center-visual-fill-on)
  ;; (add-hook 'newsticker-treeview-item-mode-hook 'emacs-solo/timed-center-visual-fill-on)
  )

;; ---------- EMACS-SOLO-SUDO-EDIT
;;
;; Inspired by: https://codeberg.org/daviwil/dotfiles/src/branch/master/Emacs.org#headline-28
(use-package emacs-solo-sudo-edit
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/sudo-edit (&optional arg)
    "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
    (interactive "P")
    (if (or arg (not buffer-file-name))
        (find-file (concat "/sudo:root@localhost:"
                           (completing-read "Find file(as root): ")))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))))


;; ---------- EMACS-SOLO-REPLACE-AS-DIFF
;;
(use-package emacs-solo/replace-regexp-as-diff
  :ensure nil
  :no-require t
  :defer t
  :init

  ;; NOTE: improvements wrappers over `multi-file-replace-regexp-as-diff', so
  ;;       we can:
  ;;       1.) Use it with glob pattern matching in files, including inside
  ;;           subfolders (`emacs-solo/multi-file-replace-regexp-as-diff-with-glob')
  ;;       2.) Use it with marked files and or directories in dired
  ;;           (`emacs-solo/dired-do-replace-regexp-as-diff')


  ;; `M-x emacs-solo/multi-file-replace-regexp-as-diff-with-glob RET'
  ;;
  ;; A wrapper for `multi-file-replace-regexp-as-diff' that extends its functionality
  ;; to support glob patterns for file matching. It recursively searches all files
  ;; in the specified directory (including subdirectories) that match the given glob
  ;; pattern (e.g., `*.js`), and displays the replacements as diffs in the
  ;; `*replace-diff*` buffer. This allows for easy review and application of changes
  ;; across multiple files.
  (defun emacs-solo/multi-file-replace-regexp-as-diff-with-glob (dir regexp to-string &optional delimited glob-pattern)
    "Wrapper for `multi-file-replace-regexp-as-diff` that accepts a directory and a glob pattern.
DIR is the directory to search recursively.
REGEXP is the regular expression to replace.
TO-STRING is the replacement string.
DELIMITED is an optional argument passed to `multi-file-replace-regexp-as-diff`.
GLOB-PATTERN is the glob pattern to match files (e.g., \"*.el\")."
    (interactive
     (let ((dir (file-truename (read-directory-name "Directory: ")))
           (common (query-replace-read-args
                    (concat "Replace"
                            (if current-prefix-arg " word" "")
                            " regexp as diff in files")
                    t t))
           (glob-pattern (read-string "Glob pattern (e.g., *.el): " "*")))
       (list dir (nth 0 common) (nth 1 common) (nth 2 common) glob-pattern)))

    (let* ((glob-regexp (wildcard-to-regexp glob-pattern))
           ;; file-expand-wildcards instead of directory-files-recursively, would
           ;; not allow us to traverse directories
           (files (directory-files-recursively dir glob-regexp)))

      (if files
          (multi-file-replace-regexp-as-diff files regexp to-string delimited)
        (message "No files found for glob-pattern: %s" glob-pattern))))


  ;; `M-x dired RET' mark files and/or directories then
  ;; `M-x emacs-solo/multi-file-replace-regexp-as-diff-with-glob RET'
  ;;
  ;; A version of `dired-do-replace-regexp-as-diff' that adds support for selected
  ;; directories in Dired. When directories are marked, it recursively includes all
  ;; files within them (and their subdirectories) in the replacement operation.
  ;; The replacements are displayed as diffs in the `*replace-diff*` buffer, allowing
  ;; for review and application of changes across multiple files and directories.
  (defun emacs-solo/expand-directories (items)
    "Expand ITEMS to include all files within directories (recursively).
Directories themselves are excluded from the final list."
    (cl-loop for item in items
             if (file-directory-p item)
             append (let ((files (directory-files-recursively item ".*" t)))
                      (cl-remove-if #'file-directory-p files))
             else if (file-regular-p item) ; Ensure only regular files are included
             collect item))
  (defun emacs-solo/dired-do-replace-regexp-as-diff (from to &optional delimited)
    "Do `replace-regexp' of FROM with TO as diff, on all marked files and directories.
If a marked item is a directory, all files within it (recursively) are included.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
The replacements are displayed in the buffer *replace-diff* that
you can later apply as a patch after reviewing the changes."
    (interactive
     (let ((common
            (query-replace-read-args
             "Replace regexp as diff in marked files and directories" t t t)))
       (list (nth 0 common) (nth 1 common) (nth 2 common))))
    (dired-post-do-command)
    (let* ((marked-items (dired-get-marked-files)) ; Include directories in the list
           (files (emacs-solo/expand-directories marked-items)))
      (if files
          (progn
            (multi-file-replace-regexp-as-diff files from to delimited))
        (message "No files found in marked items.")))))


;; ---------- EMACS-SOLO-WEATER
;;
(use-package emacs-solo-weather
  :ensure nil
  :no-require t
  :defer t
  :init
  (setq emacs-solo-weather-city "Ho Chi Minh City")

  (defun emacs-solo/weather-buffer ()
    "Open a new Emacs buffer and asynchronously fetch wttr.in weather data."
    (interactive)
    (let* ((city (shell-quote-argument emacs-solo-weather-city))
           (buffer (get-buffer-create "*Weather*"))
           (url1 (format "curl -s 'wttr.in/%s'" city))
           (url2 (format "curl -s 'v2d.wttr.in/%s'" city)))
      (with-current-buffer buffer
        (read-only-mode -1)
        (erase-buffer)
        (insert "Fetching weather data...\n")
        (read-only-mode 1))
      (switch-to-buffer buffer)
      ;; Fetch both asynchronously
      (emacs-solo--fetch-weather url1 buffer)
      (emacs-solo--fetch-weather url2 buffer t)))

  (defun emacs-solo--fetch-weather (cmd buffer &optional second)
    "Run CMD asynchronously and insert results into BUFFER.
If SECOND is non-nil, separate the results with a newline."
    (make-process
     :name "weather-fetch"
     :buffer (generate-new-buffer " *weather-temp*")
     :command (list "sh" "-c" cmd)
     :sentinel
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (let ((output (with-current-buffer (process-buffer proc)
                         (buffer-string))))
           (kill-buffer (process-buffer proc))
           (setq output (replace-regexp-in-string "^Follow.*\n" ""
                                                  (replace-regexp-in-string "[\x0f]" "" output)))
           (with-current-buffer buffer
             (read-only-mode -1)
             (when second (insert "\n\n"))
             (insert output)
             (ansi-color-apply-on-region (point-min) (point-max))
             (goto-char (point-min))
             (read-only-mode 1))))))))


;;; LITTLE FOX CUSTOMIZE
;; ZOOMING IN/OUT
(use-package emacs
  :bind
  ("C-+" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("<C-wheel-up>" . text-scale-increase)
  ("<C-wheel-down>" . text-scale-decrease)
  :custom
  ;; Emacs 30 and newer: Disaple ispell completion function.
  ;; trye `cape-dict'
  (text-mode-ispell-word-completion nil))

;; LOAD PACKAGES
(require 'package)

(when (version< emacs-version "28")
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(customize-set-variable 'package-archive-priorities
                        '(("gnu" . 99)
                          ("nongnu" . 80)
                          ("stable" . 70)
                          ("melpa" . 0)))

(require 'time-date)

(defvar my-package-perform-stale-archive-check t
  "Check if any package archives are stale.

Set this value in your `early-init.el' file.")

(defvar my-package-update-days 1
  "Number of days before an archive will be considered stale.

Set this value in your `early-init.el' file")

(defun my-package-archive-stale-p (archive)
  "Return t if ARCHIVE is stale.

ARCHIVE is stale if the on-disk cache is older than
`crafted-package-update-days' old.  If
`crafted-package-perform-stale-archive-check' is nil, the check
is skipped"
  (let* ((today (decode-time nil nil t))
         (archive-name (expand-file-name
                        (format "archives/%s/archive-contents" archive)
                        package-user-dir))
         (last-update-time (decode-time (file-attribute-modification-time
                                         (file-attributes archive-name))))
         (delta (make-decoded-time :day my-package-update-days)))
    (when my-package-perform-stale-archive-check
      (time-less-p (encode-time (decoded-time-add last-update-time delta))
                   (encode-time today)))))

(defun my-package-archives-stable-p ()
  "Return t if any package archives' cache is out of date."
  (interactive)
  (cl-some #'my-package-archive-stale-p (mapcar #'car package-archives)))

(when package-enable-at-startup
  (package-initialize)
  (require 'seq)
  (cond ((seq-empty-p package-archive-contents)
         (progn
           (message "little-fox-config: package archives empty, initalizing")
           (package-refresh-contents)))
        ((my-package-archives-stable-p)
         (progn
           (message "little-fox-config: pacakge archives stable, refreshing")
           (package-refresh-contents t))))
  (message "little-fox-package-config: package system initialized!"))


;; ICONS
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

;; BREADCRUMBS
(use-package breadcrumb
  :ensure t
  :defer t)
(when (require 'breadcrumb nil :noerror)
  (breadcrumb-mode))

;; DASHBOARD
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


;; DUMB-JUMP
(use-package dumb-jump
  :ensure t
  :defer t
  :init
  (setq dumb-jump-selector 'consult-completing-read)
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)
         ("M-g b" . dumb-jump-back))
  :config
  (setq dumb-jump-prefer-searcher 'rg
        xref-history-storage #'xref-window-local-history
        xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; Never using the etags backend.
  (remove-hook 'xref-backend-functions #'etags--xref-backend))

;; EGLOT
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

;; TREE-SITTER
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

;; COMBOBULATE
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

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.js\\'" . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)))


;; CAPE
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

;; CONSULT
(use-package consult
  :ensure t
  :bind
  (("M-g i" . consult-imenu)
   ("M-g I" . consult-project-imenu)
   ;; searching for files
   ("M-s f" . consult-find)
   ("M-s r" . consult-ripgrep))
  )
(when (locate-library "consult")
  ;; Set some consult bindings
  (keymap-global-set "C-s" 'consult-line)
  (keymap-set minibuffer-local-map "C-r" 'consult-history)
  (setq completion-in-region-function #'consult-completion-in-region))

;; CORFU
(use-package corfu
  :ensure t
  :custom
  ;; size
  (corfu-min-width 80)
  ;; always use the same width
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle t)

  ;; show documentation in echo area?
  (corfu-echo-documentation t)

  ;; Preselect first candidate?
  (corfu-preselect-first nil)

  ;; Preview current candidate?
  (corfu-preview-current 'insert)

  ;; quit if no match
  (corfu-quit-no-match t)
  :init
  (global-corfu-mode))

(use-package corfu-terminal
  :defer t
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
    (corfu-popupinfo-mode 1)
    (eldoc-add-command #'corfu-insert)
    (keymap-set corfu-map "M-p" #'corfu-popupinfo-scroll-down)
    (keymap-set corfu-map "M-n" #'corfu-popupinfo-scroll-up)
    (keymap-set corfu-map "M-d" #'corfu-popupinfo-toggle)))

;; CORFU-CANDIDATE-OVERLAY
(use-package corfu-candidate-overlay
  :ensure t
  :defer t
  :after corfu
  )

;; EMBARK
(use-package embark
  :defer t
  :ensure t)

(use-package embark-consult
  :ensure t
  :after (embark consult))

(when (require 'embark nil :noerror)
  (keymap-global-set "<remap> <describe-bindings>" #'embark-bindings)
  (keymap-global-set "C-." 'embark-act)
  ;; Use embark to show bindings in a key prefix with `C-h`
  (setq prefix-help-command #'embark-prefix-help-command)
  (when (require 'embark-consult nil :noerror)
    (with-eval-after-load 'embark-consult
      (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))))

;; MARGINALIA
(use-package marginalia
  :defer t
  :ensure t)
(when (require 'marginalia nil :noerror)
  ;; Configure Marginalia
  (customize-set-variable 'marginalia-annotators
                          '(marginalia-annotators-heavy
                            marginalia-annotators-light
                            nil))
  (marginalia-mode 1))

;; ORDERLESS
(use-package orderless
  :defer t
  :ensure t)
(when (require 'orderless nil :noerror)
  ;; Set up Orderless for better fuzzy matching
  (customize-set-variable 'completion-styles '(orderless basic))
  (customize-set-variable 'completion-category-overrides
                          '((file (styles . (partial-completion))))))
;; VERTICO
(use-package vertico
  :defer t
  :ensure t)

(when (require 'vertico nil :noerror)
  (require 'vertico-directory)
  (customize-set-variable 'vertical-cycle t)

  ;; Start Vertico
  (vertico-mode 1))

;; MAGIT
(use-package magit
  :defer t
  :ensure t)

;;; -------------------- TREESITTER AREA
;;; RUBY-TS-MODE
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
  :defer t
  :ensure t)

(use-package ruby-electric
  :ensure t
  :defer t
  :hook ((ruby-ts-mode . ruby-electric-mode)))

(use-package robe
  :ensure t
  :defer t
  :hook ((ruby-ts-mode . robe-mode))
  :bind (:map ruby-ts-mode-map (("C-c d" . robe-doc)))
  :diminish robe-mode)

(use-package yard-mode
  :ensure t
  :defer t
  :hook ((ruby-ts-mode . yard-mode))
  :diminish yard-mode)

;; GO-TS-MODE
(use-package go-ts-mode
  :defer t
  :bind (:map go-ts-mode-map (("C-c d" . godoc-at-point))))


;;; JS-TS-MODE
(use-package js-ts-mode
  :ensure js ;; I care about js-base-mode but it is locked behind the feature "js"
  :mode "\\.jsx?\\'"
  :defer 't
  :custom
  (js-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
  (add-to-list 'treesit-language-source-alist '(jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc" "master" "src")))

;;; TYPESCRIPT-TS-MODE
(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :defer 't
  :custom
  (typescript-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
  (unbind-key "M-." typescript-ts-base-mode-map))

;;; TYPESCRIPT-TS-MODE
(use-package tsx-ts-mode
  :mode "\\.tsx\\'"
  :defer 't
  :custom
  (typescript-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
  (unbind-key "M-." typescript-ts-base-mode-map))

(defun lf-configure-tide()
  (tide-setup)
  (tide-hl-identifier-mode +1))

(use-package tide
  :ensure t
  :defer t
  :hook ((typescript-ts-mode . lf-configure-tide)))

;;; RUST-TS-MODE
(use-package rust-ts-mode
  :ensure rust-ts-mode
  :mode "\\.rs\\'"
  :defer 't
  :custom
  (rust-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(rust "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")))

;;; TOML-TS-MODE
(use-package toml-ts-mode
  :ensure toml-ts-mode
  :mode "\\.toml\\'"
  :defer 't
  :config
  (add-to-list 'treesit-language-source-alist '(toml "https://github.com/ikatyang/tree-sitter-toml" "master" "src")))

;;; MARKDOWN-TS-MODE
(use-package markdown-ts-mode
  :ensure nil
  :mode "\\.md\\'"
  :defer 't
  :config
  (add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode))
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))

;;; YAML-TS-MODE
(use-package yaml-ts-mode
  :ensure yaml-ts-mode
  :mode "\\.yml\\'"
  :defer 't
  :config
  (setq yaml-indent-offset 2))

;;; DOCKERFILE-TS-MODE
(use-package dockerfile-ts-mode
  :ensure dockerfile-ts-mode
  :mode "\\Dockerfile.*\\'"
  :defer 't
  :config
  (add-to-list 'treesit-language-source-alist '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")))

;;; PRETTIER
(use-package apheleia
  :defer t
  :ensure t
  :diminish ""
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (apheleia-global-mode +1))

;;; GPTEL
;; Gptel
(defun lf/read-openai-key()
  (with-temp-buffer
    (insert-file-contents "~/.config/emacs/openai-key-hienhm-mims.txt")
    (string-trim (buffer-string))))

(use-package gptel
  :ensure t
  :config
  (setq gptel-stream t)
  (setq gptel-api-key #'lf/read-openai-key)
  (setq gptel-model "gpt-4o")
  (setq gptel-default-mode 'org-mode)
  :hook (gptel-post-stream . gptel-auto-scroll))


;;; ORG
(use-package org
  :ensure nil
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq
   ;; Start collapsed for speed
   org-startup-folded t

   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  ;; Ellipsis styling
  (setq org-ellipsis " ▼ ")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil))

;; Org Modern
(use-package org-modern
  :ensure t
  :defer t
  :after org
  :hook
  ((org-mode . org-modern-mode)
   (org-adenda-finalize . org-modern-agenda))
  :config
  (setq org-modern-star ["◉ " "○ " "● " "○ " "● " "○ " "● "]
        org-modern-label-border nil))

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
                               (restclient . t)))

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
  :defer t
  :after org)

;; PlantUML
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(use-package plantuml-mode
  :ensure t
  :defer t
  :config
  (setq org-plantuml-jar-path "/usr/local/share/plantuml/plantuml.jar"))
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

;; Mermaid
(use-package mermaid-mode
  :ensure t
  :defer t
  :config
  (setq mermaid-mmdc-location "/usr/local/bin/mmdc"))

(use-package ob-mermaid
  :defer t
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
  :ensure t
  :defer t
  :config
  (setq denote-directory (expand-file-name "~/Documents/denotes/")
        ;; check
        denote-infer-keywords t
        denote-sort-keywords t))

;; THEMES
(use-package ef-themes
  :ensure t
  :config
  (ef-themes-select 'ef-autumn)
  (setq ef-themes-to-toggle '(ef-autumn ef-summer)
        ef-themes-to-rotate '(ef-dark ef-summer ef-autumn)))


(provide 'init)
;;; init.el ends here
