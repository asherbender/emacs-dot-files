;; Keep track of loading time.
(defconst emacs-start-time (current-time))

;; Define location of configuration directory.
(defconst config-directory (concat user-emacs-directory "config/"))

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

;; Load the config.
(require 'org)
(org-babel-load-file (concat config-directory "init.org"))

;; Message how long it took to load everything (minus packages).
(let ((elapsed (float-time (time-subtract (current-time)
					  emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))

