;; early-init.el --- Early init file -*- lexical-binding: t; -*-


;; Disable UI elements early to reduce load time
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)


;; Turn off cursors in non-selected windows to reduce rendering. See:
;;
;;     https://www.gnu.org/software/emacs/manual/html_node/emacs/Cursor-Display.html
;;
(setq-default cursor-in-non-selected-windows nil)


;; Disable implied frame resizing for simpler rendering. See:
;;
;;     https://www.gnu.org/software/emacs/manual/html_node/elisp/Implied-Frame-Resizing.html
;;
(setq frame-inhibit-implied-resize t)


;; Reduce the amount of garbage collection during startup. See:
;;
;;     https://www.gnu.org/software/emacs/manual/html_node/elisp/Garbage-Collection.html
;;
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Reinstate garbage collection after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1000 1000)) ; 2MB
            (setq gc-cons-percentage 0.1)))


;; Disable *scratch* buffer message to reduce rendering. See:
;;
;;     https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Interaction.html
;;
(setq initial-scratch-message nil)


;; Make the initial buffer load faster by setting its mode to fundamental-mode
(customize-set-variable 'initial-major-mode 'fundamental-mode)


;; Disable package.el in favor of use-package or straight.el

;; Prevent Emacs from automatically making packages available at startup. See:
;;
;;     https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html
;;
(setq package-enable-at-startup nil)
(setq package-quickstart nil)
