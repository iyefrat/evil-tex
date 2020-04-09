;;; evil-tex-util.el -- Functions to be used by evil-texi -*- lexical-binding: t; -*-
;;; Commentary:
;; This file is part of evil-tex, which provides license
;; information.
;;; Code:

(require 'latex)
(require 'evil-common)


(defun evil-tex-max-key (seq fn)
  "Return the element of SEQ for which FN gives the biggest result.

Comparison is done with `>'.
(evil-tex-max-key '(1 2 -4) (lambda (x) (* x x))) => -4"
  (let* ((res (car seq))
         (res-val (funcall fn res)))
    (dolist (cur (cdr seq))
      (let ((cur-val (funcall fn cur)))
        (when (> cur-val res-val)
          (setq res-val cur-val
                res cur))))
    res))

(defun evil-tex--select-math (&rest args)
  "Return (beg . end) of best math match.

ARGS passed to evil-select-(paren|quote)."
  (evil-tex-max-key
   (list
    (ignore-errors (apply #'evil-select-paren
                          (regexp-quote "\\(") (regexp-quote "\\)") args))
    (ignore-errors (apply #'evil-select-paren
                          (regexp-quote "\\[") (regexp-quote "\\]") args))
    (ignore-errors (apply #'evil-select-quote ?$ args)))
   (lambda (arg) (if (and (consp arg) ; selection succeeded
                          ;; Selection is close enough to point.
                          ;; evil-select-quote can select things further down in
                          ;; the buffer.
                          (<= (- (car arg) 2) (point))
                          (>= (+ (cadr arg) 3) (point)))
                     (car arg)
                   most-negative-fixnum))))


;; Stuff from evil-latex-textobjects

(defun evil-tex-macro-beginning-begend ()
  "Return (start . end) of the macro-beginning to the left of point.

If no enclosing macro is found, return nil.
For example for \\macro{foo|bar} it returns the start and end of \"\\macro{\""
  (let ((beg (TeX-find-macro-start)))
    (when beg
      (save-excursion
        (goto-char beg)
        (forward-char)                  ; backslash
        (skip-chars-forward "A-Za-z@*") ; macro-name
        (when (looking-at "{\\|\\[")
          (forward-char))               ; opening brace
        (cons beg (point))))))

(defun evil-tex-macro-end-begend ()
  "Return (start . end) of the end of the enclosing macro.

If no such macro can be found, return nil"
  (let ((end (TeX-find-macro-end)))
    (when end
      (save-excursion
        (goto-char end)
        (when (looking-back "}\\|\\]" (- (point) 2))
          (backward-char))              ; closing brace
        (cons (point) end)))))

;; TODO Support visual selection
;; TODO Support count

;; Now ours

(defun evil-tex--re-search-backwards-unless-already (str)
  "Search backward for STR unless point is already on it."
  (unless (looking-at str)
    (re-search-backward str)))

(defun evil-tex--get-macro-braces-begend ()
  "Return (beg . end) of the macro braces currently active.
\foo{bar}
    ^   ^"
  (let (beg end)
    (save-excursion
      (evil-tex--re-search-backwards-unless-already "\\\\")
      (search-forward "{")
      (setq beg (point))
      (forward-sexp)
      (setq end (point)))
    (cons beg end)))

(defun evil-tex-env-beginning-begend ()
  "Return (start . end) of the \\begin{foo} of current env.

\\begin{equation}
^               ^"
  (let (beg)
    (save-excursion
      (LaTeX-find-matching-begin)      ; we are at backslash
      (setq beg (point))
      (skip-chars-forward "^{")        ; goto opening brace
      (forward-sexp)                   ; goto closing brace
      ;; Count the newline after \begin{foo} to the environment header
      ;; Without this, delete-inner-env would unexpectedly move the end
      ;; to the same line as the beginning
      ;; (when (looking-at "[[:blank:]]*$")
      ;;   (message "Newline")
      ;;   (forward-line 1))
      (cons beg (point)))))

(defun evil-tex-env-end-begend ()
  "Return (start . end) of the \\end{foo} of current env.

\\end{equation}
^             ^"
  (let (end)
    (save-excursion
      (LaTeX-find-matching-end)        ; we are at closing brace
      (setq end (point))
      (backward-sexp)                  ; goto opening brace
      (search-backward "\\")           ; goto backslash
      (cons (point) end))))


(provide 'evil-tex-util)
;;; evil-tex-util.el ends here
