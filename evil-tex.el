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

(defun evil-tex-brace-movement ()
  "Brace movement similar to TAB in cdlatex.

Example: (| symbolizes point)
\bar{h|} => \bar{h}|
\frac{a|}{} => \frac{a}{|}
\frac{a|}{b} => \frac{a}{b|}
\frac{a}{b|} => \frac{a}{b}|"
  (interactive)
  ;; go to the closing } of the current scope
  (search-backward "{" (line-beginning-position))
  (forward-sexp)
  ;; encountered a {? go to just before its terminating }
  (when (looking-at "{")
    (forward-sexp)
    (backward-char)))


;; stolen code from https://github.com/hpdeifel/evil-tex
(evil-define-text-object evil-tex-inner-dollar (count &optional beg end type)
  "Select inner dollar"
  :extend-selection nil
  (evil-select-quote ?$ beg end type count nil))

(evil-define-text-object evil-tex-a-dollar (count &optional beg end type)
  "Select a dollar"
  :extend-selection t
  (evil-select-quote ?$ beg end type count t))

(evil-define-text-object evil-tex-inner-math (count &optional beg end type)
  "Select innter \\[ \\] or \\( \\)."
  :extend-selection nil
  ;;                  \   [     \   (   \   )   \   ]
  (evil-select-paren "\\\\\\[\\|\\\\(" "\\\\)\\|\\\\\\]" beg end type count nil))

(evil-define-text-object evil-tex-a-math (count &optional beg end type)
  "Select a \\[ \\] or \\( \\)."
  :extend-selection nil
  ;;                  \   [     \   (   \   )   \   ]
  (evil-select-paren "\\\\\\[\\|\\\\(" "\\\\)\\|\\\\\\]" beg end type count t))

(defun evil-tex-macro-beginning ()
  "Return (start . end) of the macro-beginning to the left of point.

If no enclosing macro is found, return nil.
For example for \macro{foo|bar} it returns the start and end of \"\macro{\""
  (let ((beg (TeX-find-macro-start)))
    (when beg
      (save-excursion
        (goto-char beg)
        (forward-char)                 ;; backslash
        (skip-chars-forward "A-Za-z@*");; macro-name
        (when (looking-at "{\\|\\[")
          (forward-char))               ;; opening brace
        (cons beg (point))))))

(defun evil-tex-macro-end ()
  "Return (start . end) of the end of the enclosing macro.

If no such macro can be found, return nil"
  (let ((end (TeX-find-macro-end)))
    (when end
      (save-excursion
        (goto-char end)
        (when (looking-back "}\\|\\]" (- (point) 2))
          (backward-char))              ;; closing brace
        (cons (point) end)))))

;; TODO Support visual selection
;; TODO Support count

(evil-define-text-object evil-tex-a-macro (count &optional beg end type)
  "Select a TeX macro"
  :extend-selection nil
  (let ((beg (evil-tex-macro-beginning))
        (end (evil-tex-macro-end)))
    (if (and beg end)
        (list (car beg) (cdr end))
      (error "No enclosing macro found"))))

(evil-define-text-object evil-tex-inner-macro (count &optional beg end type)
  "Select inner TeX macro, i.e the argument to the macro."
  :extend-selection nil
  (let ((beg (evil-tex-macro-beginning))
        (end (evil-tex-macro-end)))
    (cond
     ((or (null beg) (null end))
      (error "No enclosing macro found"))
     ((= (cdr beg) (car end))          ;; macro has no content
      (list (1+ (car beg))             ;; return macro boundaries excluding \
            (cdr beg)))
     (t (list (cdr beg) (car end))))))

(defun evil-tex-env-beginning ()
  "Return (start . end) of the \\begin{foo} to the left of point."
  (let (beg)
    (save-excursion
      (LaTeX-find-matching-begin)      ;; we are at backslash
      (setq beg (point))
      (skip-chars-forward "^{")        ;; goto opening brace
      (forward-sexp)                   ;; goto closing brace
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
      (LaTeX-find-matching-end)        ;; we are at closing brace
      (setq end (point))
      (backward-sexp)                  ;; goto opening brace
      (search-backward "\\")           ;; goto backslash
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
