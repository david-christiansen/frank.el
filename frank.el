;;; frank.el --- A major mode for editing Frank files  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  David Christiansen

;; Author: David Christiansen <david@davidchristiansen.dk>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for editing Frank files.

;;; Code:

(require 'compile)
(require 'font-lock)

(defgroup frank '() "The Frank language" :group 'languages)

(defcustom frank-executable "frank" "The command to run for frank."
  :group 'frank
  :type 'file)

(defface frank-keyword-face
  '((t (:inherit 'font-lock-keyword-face)))
  "How to highlight Frank's keywords"
  :group 'frank)

(defface frank-operator-face
  '((t (:inherit 'font-lock-builtin-face)))
  "How to highlight Frank operators"
  :group 'frank)

(defface frank-braces-face
  '((t (:inherit 'font-lock-function-name-face)))
  "How to highlight Frank suspended computations"
  :group 'frank)

(defface frank-effect-bracket-face
  '((t (:inherit 'font-lock-keyword-face)))
  "How to highlight Frank effect brackets"
  :group 'frank)

(defface frank-command-bracket-face
  '((t (:inherit 'font-lock-variable-name-face)))
  "How to highlight Frank command brackets"
  :group  'frank)

(defface frank-type-arrow-face
  '((t (:inherit 'font-lock-type-face)))
  "How to highlight Frank command brackets"
  :group  'frank)

(defface frank-definition-face
  '((t (:inherit 'font-lock-function-name-face)))
  "How to highlight Frank names in their definitions"
  :group 'frank)

(defface frank-interface-definition-face
  '((t (:inherit 'frank-definition-face)))
  "How to highlight Frank interface names in their definitions"
  :group 'frank)

(defconst frank-keywords
  '("data" "interface" "bind" "sig"))

(defvar frank-font-lock-keywords nil
  "Highlighting instructions for Frank.")

(defun frank--current-line-empty-p ()
  "Non-nil if the current line is empty."
  (string-match-p "^\\s-*$" (thing-at-point 'line))  )

(defun frank--current-line-indentation ()
  "Nil if the current line is empty, or the number of spaces at the beginning of the line."
  (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (if (string-match-p "^\\s-*$" line)
        nil
      (string-match (rx bol
                        (group-n 1 (zero-or-more ?\ ))
                        (not (any ?\ )))
                    line)
      (length (match-string-no-properties 1 line)))))

(defun frank--next-non-indent ()
  "Find position before the next line that outdents.

The current line must be non-empty."
  (save-excursion
    (beginning-of-line)
    (let ((i (frank--current-line-indentation)))
      (when (not i) (error "Current line empty"))
      (forward-line)
      (while (let ((j (frank--current-line-indentation)))
               (and (not (eobp))
                    (or (not j)
                        (> j i))))
        (forward-line))
      (backward-char))
    (point)))

(defvar frank--pos-stack nil
  "Internal position stack for font-lock.")

(defvar font-lock-beg)
(defvar font-lock-end)
(defun frank-font-lock-extend-region ()
  "Extend the font-lock region to cover an indented block."
  (let ((my-beg font-lock-beg)
        (my-end font-lock-end))
    (save-excursion
      (goto-char my-beg)
      (when (not (bolp))
        (beginning-of-line))
      (while (and (not (bobp)) (looking-at-p " "))
        (forward-line -1))
      (setq my-beg (point))
      (goto-char my-end)
      (setq my-end (frank--next-non-indent))
      (if (not (and (= my-beg font-lock-beg)
                    (= my-end font-lock-end)))
          (progn (setq font-lock-beg my-beg)
                 (setq font-lock-end my-end)
                 t)
        nil))))

(defun frank--update-kw ()
  "Update kws."
  (setq frank-font-lock-keywords
        `(
          ;; Interfaces get special highlighting
          (,(rx bol (0+ ?\ )
                (group-n 1 "interface")
                (1+ ?\ )
                (group-n 2 (1+ wordchar)) ;; the name
                (0+ ?\ )
                (group-n 3 "=")
                )
           (1 'frank-keyword-face)
           (2 'frank-interface-definition-face)
           (3 'frank-operator-face))

          ;; Type declarations get special highlighting.
          (,(rx
             bol (0+ ?\ )
             (group-n 1 (1+ wordchar)) ;; The name
             (0+ ?\ )
             (group-n 2 ?\:) ;; the colon
             )
           (1 'frank-definition-face)
           (2 'frank-operator-face)
           (,(rx (group-n 1 (or "{" "}")))
            (progn (push (point) frank--pos-stack)
                   (frank--next-non-indent))
            (goto-char (pop frank--pos-stack))
            (1 'frank-braces-face))
           (,(rx (group-n 1 (or "[" "]")))
            (progn (push (point) frank--pos-stack)
                   (frank--next-non-indent))
            (goto-char (pop frank--pos-stack))
            (1 'frank-effect-bracket-face))
           (,(rx (not (any ?\-)) (group-n 1 "->"))
            (progn (push (point) frank--pos-stack)
                   (frank--next-non-indent))
            (goto-char (pop frank--pos-stack))
            (1 'frank-type-arrow-face))
           (,(rx (not (any ?\-)) (group-n 1 (or "<" ">")))
            (progn (push (point) frank--pos-stack)
                   (frank--next-non-indent))
            (goto-char (pop frank--pos-stack))
            (1 'frank-command-bracket-face)))

          ;; Operator syntax
          (,(regexp-opt '("->" "<" ">" ":" "|" ";") 'symbols) 0 'frank-operator-face)

          ;; Forcing
          ("!" 0 'frank-operator-face)

          ;; Ordinary keywords
          (,(regexp-opt frank-keywords  'words) 0 'frank-keyword-face)
          )))


(defun frank-syntax-propertize-function (start end)
  ;; Apply character syntax
  (save-excursion
    (let ((state 0))
      (goto-char start)
      (while (re-search-forward (rx (group-n 1 "'")
                                    (or (seq "\\" anything)
                                        (not (any ?\')))
                                    (group-n 2"'"))
                                end
                                t)
        (put-text-property (match-beginning 1) (match-end 1)
                           'syntax-table (string-to-syntax "\""))
        (put-text-property (match-beginning 2) (match-end 2)
                           'syntax-table (string-to-syntax "\""))))))

(defconst frank-syntax-table
  (let ((st (make-syntax-table)))

    ;; Matching parens
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)

    ;; Matching {}, but with nested comments
    (modify-syntax-entry ?\{ "(} 1bn" st)
    (modify-syntax-entry ?\} "){ 4bn" st)
    (modify-syntax-entry ?\- "_ 123" st)
    (modify-syntax-entry ?\n ">" st)

    ;; ' and _ can be names
    (modify-syntax-entry ?' "w" st)
    (modify-syntax-entry ?_ "w" st)


    ;; Operator chars (TODO: is this the right set of chars?)
    (mapc #'(lambda (ch) (modify-syntax-entry ch "_" st))
          "!#$%&*+./<=>@^|~:")

    ;; Whitespace is whitespace
    (modify-syntax-entry ?\  " " st)
    (modify-syntax-entry ?\t " " st)

    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "/" st)

    st))

(defun frank--compilation-buffer-name-function (_mode)
  "Compute a buffer name for the Frank compilation buffer."
  "*frank*")

(defun frank-load-buffer ()
  "Load the current file into Frank in an Emacs compilation buffer."
  (interactive)
  (let* ((filename (buffer-file-name))
         (dir (file-name-directory filename))
         (file (file-name-nondirectory filename))
         (command (concat frank-executable " " file))

         ;; Emacs compile config stuff - these are special vars
         (compilation-buffer-name-function
          'frank--compilation-buffer-name-function)
         (default-directory dir))
    (compile command)))

;;;###autoload
(define-derived-mode frank-mode prog-mode "Frank"
  "Major mode for the Frank language.
     \\{frank-mode-map}
Invokes `frank-mode-hook'."
  :syntax-table frank-syntax-table
  (frank--update-kw)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'comment-start) "--")

  (add-hook 'font-lock-extend-region-functions 'frank-font-lock-extend-region)
  (setq font-lock-multiline 'undecided)
  (setq font-lock-defaults '((frank-font-lock-keywords) nil nil))
  (set (make-local-variable 'syntax-propertize-function)
       #'frank-syntax-propertize-function))

(define-key frank-mode-map (kbd "C-c C-c") 'frank-load-buffer)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fk\\'" . frank-mode))

(provide 'frank)
;;; frank.el ends here
