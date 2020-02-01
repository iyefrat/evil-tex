;;; ~/projects/evil-tex/evil-tex.el -*- lexical-binding: t; -*-

;; To start, let us try to define a text object for an enviornment,
;; although there might be a better way to do this than regex


;;; Code:

(require 'evil)
(require 'latex)

;; stolen mystery code
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

(defun evil-latex-txtobj-env-end ()
  "Return (start . end) of the \\end{foo} to the right of point."
  (let (end)
    (save-excursion
      (LaTeX-find-matching-end)         ; we are at closing brace
      (setq end (point))
      (backward-sexp)                   ; goto opening brace
      (search-backward "\\")            ; goto backslash
      (cons (point) end))))


(evil-define-text-object evil-latex-txtobj-an-env (count &optional beg end type)
  "Select a LaTeX environment"
  :extend-selection nil
  (let ((beg (evil-latex-txtobj-env-beginning))
        (end (evil-latex-txtobj-env-end)))
    (list (car beg) (cdr end))))

(evil-define-text-object evil-latex-txtobj-inner-env (count &optional beg end type)
  "Select a LaTeX environment"
  :extend-selection nil
  (let ((beg (evil-latex-txtobj-env-beginning))
        (end (evil-latex-txtobj-env-end)))
    (list (cdr beg) (car end))))

(defvar evil-latex-txtobj-outer-map (make-sparse-keymap))
(defvar evil-latex-txtobj-inner-map (make-sparse-keymap))

(set-keymap-parent evil-latex-txtobj-outer-map evil-outer-text-objects-map)
(set-keymap-parent evil-latex-txtobj-inner-map evil-inner-text-objects-map)

(define-key evil-latex-txtobj-outer-map "e" 'evil-latex-txtobj-an-env)
(define-key evil-latex-txtobj-inner-map "e" 'evil-latex-txtobj-inner-env)

(provide 'evil-tex)
;;; evil-scientist.el ends here
