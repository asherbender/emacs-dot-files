;;; early-init.el -*- lexical-binding: t; -*-

(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
   (expand-file-name "auto/eln-cache" user-emacs-directory)
  )
)

;;; Garbage collection
;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;;; UI configuration
;; Remove some unneeded UI elements (the user can turn back on anything they wish)
(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; general settings
(setq initial-scratch-message nil)

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

(setq package-user-dir
      (locate-user-emacs-file
       (concat (file-name-as-directory "elpa") emacs-version)
       )
)

;; Make the initial buffer load faster by setting its mode to fundamental-mode
(customize-set-variable 'initial-major-mode 'fundamental-mode)

(setq package-enable-at-startup nil)
;;(setq use-package-verbose t)
