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

;; MELPA (or Milkypostman’s ELPA or Milkypostman’s Experimental Lisp
;; Package Repository if you’re not into the whole brevity thing) is
;; a package.el repository for development versions of Emacs
;; packages (hot from the repo).
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
)

(package-initialize)
(setq package-enable-at-startup nil)

(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
     (message "Loaded packages in %.3fs" elapsed)
)

;; Boot-strap and load use-package if it does not exist.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(require 'use-package)

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
					  emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))

;; Turn off debugging after initialisation.
(setq debug-on-error nil)
(setq debug-on-quit nil)
