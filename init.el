;; Select your preferred version of Emacs from:
;;
;;     http://mirrors.kernel.org/gnu/emacs/
;;
;; Download preferred version:
;;
;;     wget http://mirrors.kernel.org/gnu/emacs/emacs-<VERSION>.tar.gz \
;;          -O ~/src/emacs-<VERSION>.gz
;;
;; Unpack archive:
;;
;;     cd ~/src/
;;     tar -zxvf emacs-<VERSION>.tar.xz
;;
;; Build Emacs:
;;
;;     sudo apt-get install build-essential
;;     sudo apt-get build-dep emacs
;;     cd ~/src/emacs-<VERSION>
;;     mkdir build && cd build
;;     ../configure
;;     sudo make -j6

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

;; Flag start of initialisation in *messages*.
(message-center "Initialising Emacs" 80)

;; Enable debugging during initialisation (disable at end).
(setq debug-on-error t)
(setq debug-on-quit t)

;; Keep track of loading time.
(defconst emacs-start-time (current-time))

;; Define location of configuration directory.
(defconst config-directory (concat user-emacs-directory "config/"))

;; Initalize all ELPA packages.
(require 'package)

;; The Emacs Lisp Package Archive (ELPA) is included in Emacs, starting with
;; version 24. Starting with Emacs 28.1, the NonGNU ELPA repository is also
;; enabled by default. For Emacs >=28.1, the following are no longer required:
;;
;;     (add-to-list 'package-archives '("gnu"    . "https://elpa.gnu.org/packages/"))
;;     (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
;;
(if (< emacs-major-version 28)
    (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
)
(if (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu"    . "https://elpa.gnu.org/packages/"))
)

;; MELPA (or Milkypostman's ELPA or Milkypostman's Experimental Lisp Package
;; Repository if you're not into the whole brevity thing) is a package.el
;; repository for development versions of Emacs packages (hot from the repo).
;;
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(setq use-package-verbose t)

;;------------------------
;; defcustom package-user-dir (locate-user-emacs-file "elpa")

;; Boot-strap and load use-package if it does not exist.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish)
  (package-install 'bind-key)
  (eval-when-compile
      (require 'use-package)
      (require 'diminish)
      (require 'bind-key)
  )
)

;; Load Emacs Lisp packages, and activate them. Ensure packages are
;; installed automatically if not already present on your system.
(package-initialize)
(setq use-package-always-ensure t)

(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
     (message "Packages initialised in %.3fs" elapsed)
)


;; Define loading function used in `readme.org`.
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
  (message "Initialisation completed in %.3f seconds." elapsed)
  (message "Garbage collections performed: %d." gcs-done)
)

(message-center "Initialisation complete" 80)

;; Turn off debugging after initialisation.
(setq debug-on-error nil)
(setq debug-on-quit nil)
