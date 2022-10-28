;; Enable debugging during initialisation (disable at end).
(setq debug-on-error t)
(setq debug-on-quit t)

;; Keep track of loading time.
(defconst emacs-start-time (current-time))

;; Define location of configuration directory.
(defconst config-directory (concat user-emacs-directory "config/"))

;; Define column and tab widths.
(defconst default-column-width   80)
(defconst default-code-tab-width 4 )

;; Initalize all ELPA packages.
(require 'package)

;; Work around for fixing access to repositories.
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; MELPA (or Milkypostman’s ELPA or Milkypostman’s Experimental Lisp
;; Package Repository if you’re not into the whole brevity thing) is
;; a package.el repository for development versions of Emacs
;; packages (hot from the repo).
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/"))
)

;; Load Emacs Lisp packages, and activate them. Ensure packages are
;; installed automatically if not already present on your system.
(package-initialize)
(setq package-enable-at-startup nil)
(setq use-package-always-ensure t)

(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
     (message "Loaded packages in %.3fs" elapsed)
)

;; Boot-strap and load use-package if it does not exist.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish)
  (package-install 'bind-key)
)
(setq use-package-verbose t)
(require 'use-package)
(require 'diminish)
(require 'bind-key)
(setq use-package-always-ensure t)

(defun load-org-config (file)
  "Load org-babel configuration files."
  (setq load-start-time (current-time))
  (message "Loading: %s" (expand-file-name file config-directory))
  (org-babel-load-file (expand-file-name file config-directory))
  (let ((elapsed (float-time (time-subtract (current-time) load-start-time))))
       (message "Loaded: %s (%.3fs)" file elapsed)
  )
)

;; Load the configuration.
(require 'org)
(org-babel-load-file (concat user-emacs-directory "readme.org"))

;; Message how long it took to load everything (minus packages).
(let ((elapsed (float-time (time-subtract (current-time)
                                           emacs-start-time))
               )
      )
  (message "Loading settings...done (%.3fs)" elapsed)
)

;; Turn off debugging after initialisation.
(setq debug-on-error nil)
(setq debug-on-quit nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ein unfill csv-mode multiple-cursors markdown-mode flycheck-yamllint yaml-mode elpy origami helm-projectile projectile diff-hl magit auctex-latexmk auctex ace-jump-helm-line helm-descbinds helm-swoop helm buffer-move ace-jump-buffer ace-window transpose-frame window-numbering mwim hydra undo-tree key-chord fill-column-indicator smooth-scrolling use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
