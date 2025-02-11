;; init.el --- Initialization file. -*- lexical-binding: t; -*-

(when (< emacs-major-version 29)
  (error "Configuration relies on Emacs 29. Detected version %s" emacs-major-version)
)

;; Set location for the asynchronous native compilation *.eln files. See:
;;
;;     https://www.gnu.org/software/emacs/manual/html_node/elisp/Native_002dCompilation-Functions.html
;;
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
   (expand-file-name (file-name-as-directory "cache/eln") user-emacs-directory)
  )
)

(defun message-center (str width)
  (let*
      (
       (str-length (length str))
       (remaining-width (- width str-length 2))
       (left-padding  (make-string (/ remaining-width 2) ?-))
       (right-padding (make-string (- remaining-width (/ remaining-width 2)) ?-))
       (padded (concat left-padding " " str " " right-padding))
      )
    (message padded)
  )
)

;; Enable debugging during initialisation (disable at end).
(setq debug-on-error t)
(setq debug-on-quit t)

;; Keep track of loading time.
(defconst emacs-start-time (current-time))

;;------------------------------------------------------------------------------
;;                              Initialise packages
;;------------------------------------------------------------------------------
(message-center "Initialising packages" 80)

(setq package-user-dir
      (locate-user-emacs-file
       (expand-file-name (concat (file-name-as-directory "cache/elpa") emacs-version)
                         user-emacs-directory)
      )
)

;; Package archives.
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("gnu"   . "https://elpa.gnu.org/packages/"))
)

;; Set archive priorities.
(setq package-archive-priorities
      '(("gnu"      . 100)
        ("nongnu"   . 50)
        ("melpa"    . 10))
)

;; Initialize the package system
(package-initialize)

;; Bootstrap `use-package` if it's not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Always ensure packages are installed if missing.
(setq use-package-always-ensure t)

;; Load `use-package` for configuring packages.
(require 'use-package)

;; Use `diminish` and `bind-key` for cleaner mode lines and keybinding
;; management.
(use-package diminish :ensure t)
(use-package bind-key :ensure t)

(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
     (message "Packages initialised in %.3fs" elapsed)
)

;;------------------------------------------------------------------------------
;;                              Load configurations
;;------------------------------------------------------------------------------
(message-center "Loading configurations" 80)

;; Define location of configuration directory.
(defconst config-directory (concat user-emacs-directory "config/"))

;; Define loading function used in `readme.org`.
(defun load-org-config (file)
  "Load org-babel configuration files."
  (setq load-start-time (current-time))

  ;; Set to 't' to output org-babel-load-file messages to the *Messages*
  ;; buffer. Set 'nil' to suppress output.
  (defvar load-configuration-verbose t)
  (if load-configuration-verbose
    (org-babel-load-file (expand-file-name file config-directory))
    (let ((message-log-max nil))
      (org-babel-load-file (expand-file-name file config-directory))
    )
  )

  (let ((elapsed (float-time (time-subtract (current-time) load-start-time))))
       (message "Loaded: %s (%.3fs)" file elapsed)
  )
)

(defun reload-configuration ()
  "Reload the Emacs configuration file."
  (interactive)
  (load-file (expand-file-name user-init-file))
  (message "Configuration reloaded successfully!")
)

;; Load the configuration.
(require 'org)
(org-babel-load-file (concat user-emacs-directory "README.org"))

;;------------------------------------------------------------------------------
;;                             Exit initialisation
;;------------------------------------------------------------------------------
(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message (make-string 80 ?-))
  (message "Initialisation completed in %.3f seconds." elapsed)
  (message "Garbage collections performed: %d." gcs-done)
)

;; Turn off debugging after initialisation.
(setq debug-on-error nil)
(setq debug-on-quit nil)
