;;; early-init.el --- Little Fox Emacs -*- lexical-binding: t; -*-
;;; Package
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

;; Early Init ends here
