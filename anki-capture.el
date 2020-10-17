;;; anki-capture.el --- Quickly capture notes directly into Anki. -*- lexical-binding: t; -*-
;;
;; © 2020 Nick Econopouly, Cheong Yiufung
;;
;; URL: http://git.wrycode.com/wrycode/anki-capture/log.html
;;
;;; Commentary:
;; Perhaps you want to use Emacs and Org mode for quickly adding notes to
;; Anki, but you do not want to deal with storing and organizing the
;; notes in text format. =anki-capture= is an =org-capture=-like
;; interface for adding notes directly to Anki from anywhere in Emacs.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'anki-editor)

;; These are global vars that you can set yourself in your init
(defcustom anki-capture-file nil
  "Optional file to save anki-capture notes in.")
(defcustom anki-capture-deck nil
  "Current Anki deck for anki-capture.")
(defcustom anki-capture-note-type nil
  "Current Anki note type for anki-capture.")
(defcustom anki-capture-tags nil
  "Current tags for anki-capture.")

;; Thank you Cheong Yiufung for these functions!
(defun anki-editor-cloze-region-auto-incr (&optional arg)
  "Cloze region without hint and increase card number."
  (interactive)
  (anki-editor-cloze-region my-anki-editor-cloze-number "")
  (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
  (forward-sexp))
(defun anki-editor-cloze-region-dont-incr (&optional arg)
  "Cloze region without hint using the previous card number."
  (interactive)
  (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
  (forward-sexp))
(defun anki-editor-reset-cloze-number (&optional arg)
  "Reset cloze number to ARG or 1"
  (interactive)
  (setq my-anki-editor-cloze-number (or arg 1)))

(defun anki-capture-insert-note-skeleton ()
  "Insert an anki-capture note subtree."
  (insert "* Captured note from Emacs")
  (org-set-tags anki-capture-tags)
  (move-end-of-line nil)
  (newline)
  (org-set-property anki-editor-prop-deck anki-capture-deck)
  (org-set-property anki-editor-prop-note-type anki-capture-note-type)

  (let ((fields (anki-editor--anki-connect-invoke-result
		 "modelFieldNames" `((modelName . ,anki-capture-note-type)))))

    ;; leave point at the first field to start entering content immediately
    (newline)
    (insert "** ")
    (insert (car fields))
    (newline)

    ;; rest of the fields
    (save-excursion
      (dolist (field (cdr fields))
	(newline)
	(insert "** ")
	(insert field)))))

(defvar anki-capture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'anki-capture-finish)
    (define-key map "\C-c\C-k" #'anki-capture-cancel)
    map)
  "Keymap for `anki-capture-mode', a minor mode.
Use this map to set additional keybindings anki-capture buffers.")

(defvar anki-capture-mode-hook nil
  "Hook for the `anki-capture-mode' minor mode.")

(define-minor-mode anki-capture-mode
  "Minor mode for special key bindings in an anki-capture buffer.

Turning on this mode runs the normal hook `anki-capture-mode-hook'."
  nil " Cap" anki-capture-mode-map
  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<anki-capture-mode-map>Anki Capture buffer.  Finish: \
`\\[anki-capture-finish]' Cancel: `\\[anki-capture-cancel]'")))

(defun anki-capture-finish ()
  (interactive)
  (anki-editor-mode)			       ;; this is necessary to properly push inline images
  (call-interactively 'anki-editor-push-notes) ;; pushes current note
  (kill-buffer "*anki-capture*"))

(defun anki-capture-cancel ()
  (interactive)
  ;; only deletes the current note because we are in an indirect buffer
  (delete-region (point-min) (point-max))
  (kill-buffer "*anki-capture*"))

;;;###autoload
(defun anki-capture (prefix)
  "This is a unified command to handle adding Anki cards from
anywhere in Emacs. 'anki-capture-finish' will close the temporary
buffer and sync the note to Anki. 'anki-capture-file' must be set
if you want to persistently save a copy of your captured notes,
otherwise you can view notes from this session in a buffer called
'*anki-capture-storage-buffer*'.

The first time you run this command (per Emacs session), you will
be asked to choose three \"note settings\": the deck, note type,
and tags for the card you are adding. Subsequent calls will just
assume you want all of the same settings. To change settings use
prefix arguments as follows:

Use the single prefix argument (C-u) if you want to change some
of the note settings and keep some of them. Your previous setting
will automatically be filled in, so you can type enter to keep
it, or delete it (C-a C-k) to choose a new option.

Use the double prefix argument (C-u C-u) if you want to start
fresh and choose new note settings."

  (interactive "P")

  (let ((storage-buffer (if anki-capture-file (find-file-noselect anki-capture-file)
			  (get-buffer-create "*anki-capture-storage-buffer*"))))

    ;; Prompt the user for note options
    (if (or prefix (not (and anki-capture-deck anki-capture-note-type anki-capture-tags)))
	;; initial input for completing read
	(let* ((double-prefix? (equal prefix '(16)))
	       (default-type (unless double-prefix? anki-capture-note-type))
	       (default-tags (unless double-prefix? anki-capture-tags))
	       (default-deck (unless double-prefix? anki-capture-deck)))
	  ;; this sets the global note setting variables!
	  (setq anki-capture-note-type
		(completing-read "Choose a note type: " (sort (anki-editor-note-types) #'string-lessp)
				 nil t default-type)
		anki-capture-deck
		(completing-read "Choose a deck: " (sort (anki-editor-deck-names) #'string-lessp)
				 nil nil default-deck)
		anki-capture-tags
		(completing-read-multiple "Choose tags (comma-separated, press TAB to complete): "
					  (anki-editor-all-tags)
					  nil nil (mapconcat 'print default-tags ",")))))

    ;; if the user hasn't set anki-capture-file we have to make sure storage-buffer is in org-mode
    (with-current-buffer storage-buffer (org-mode))

    ;; org-capture-like buffer
    (switch-to-buffer (make-indirect-buffer storage-buffer "*anki-capture*" t))
    (anki-capture-mode)

    ;; get ready to insert a note
    (goto-char (point-max))
    (newline)
    (narrow-to-region (point) (point-max))
    (anki-capture-insert-note-skeleton)
    (org-show-all)
    (anki-editor-reset-cloze-number)))

(provide 'anki-capture)

;;; anki-capture.el ends here
