;;; evil-tex.el --- Useful features for editing TeX in evil-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Yoav Marco, Itai Y. Efrat
;;
;; Authors: Yoav Marco <http://github/yoavm448>, Itai Y. Efrat <http://github/itai33>
;; Maintainers: Yoav Marco <yoavm448@gmail.com>, Itai Y. Efrat <itai3397@gmail.com>
;; Created: February 01, 2020
;; Modified: February 01, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/itai33/evil-tex
;; Package-Requires: ((evil "1.0") (auctex "11.88"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Useful features for editing TeX in evil-mode
;;
;;; Code:


;; To start, let us try to define a text object for an enviornment,
;; although there might be a better way to do this than regex


;;; Code:

(require 'evil)
(require 'latex)


;; stolen code from https://github.com/hpdeifel/evil-latex-textobjects
(defun evil-tex-txtobj-env-beginning ()
  "Return (start . end) of the \\begin{foo} to the left of point."
  (let (beg)
    (save-excursion
      (LaTeX-find-matching-begin)       ; we are at backslash
      (setq beg (point))
      (skip-chars-forward "^{")         ; goto opening brace
      (forward-sexp)                    ; goto closing brace
      ;; Count the newline after \begin{foo} to the environment header
      ;; Without this, delete-inner-env would unexpectedly move the end
      ;; to the same line as the beginning
      ;; (when (looking-at "[[:blank:]]*$")
      ;;   (message "Newline")
      ;;   (forward-line 1))
      (cons beg (point)))))

(defun evil-tex-env-end ()
  "Return (start . end) of the \\end{foo} to the right of point."
  (let (end)
    (save-excursion
      (LaTeX-find-matching-end)         ; we are at closing brace
      (setq end (point))
      (backward-sexp)                   ; goto opening brace
      (search-backward "\\")            ; goto backslash
      (cons (point) end))))


(evil-define-text-object evil-tex-an-env (count &optional beg end type)
  "Select a LaTeX environment"
  :extend-selection nil
  (let ((beg (evil-tex-env-beginning))
        (end (evil-tex-env-end)))
    (list (car beg) (cdr end))))

(evil-define-text-object evil-tex-inner-env (count &optional beg end type)
  "Select a LaTeX environment"
  :extend-selection nil
  (let ((beg (evil-tex-env-beginning))
        (end (evil-tex-env-end)))
    (list (cdr beg) (car end))))

(defvar evil-tex-outer-map (make-sparse-keymap))
(defvar evil-tex-inner-map (make-sparse-keymap))

(set-keymap-parent evil-tex-outer-map evil-outer-text-objects-map)
(set-keymap-parent evil-tex-inner-map evil-inner-text-objects-map)

(define-key evil-tex-outer-map "e" 'evil-tex-an-env)
(define-key evil-tex-inner-map "e" 'evil-tex-inner-env)

(provide 'evil-tex)

;;; evil-tex ends here
