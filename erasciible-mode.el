;;; erasciible-mode.el --- Minor mode to help with an R Knitr and ASCIIDoc workflow

;;; Commentary:
;; 

;;; Code:

;;; Interactive Functions
(defun erasciible-new-analysis (analysis-name)
  "Create a new analysis ANALYSIS-NAME in the current directory.
This will create buffers for both the Rasciidoc and R files necessary,
and open the buffers in the background"
  (interactive "Manalysis name: ")
  (let* ((analysis-safe-name (replace-regexp-in-string "\s+" "_" analysis-name))
		 (rbuf (find-file-noselect (concat "./" analysis-safe-name ".R")))
		 (rasciibuf (find-file-noselect (concat "./" analysis-safe-name ".Rasciidoc"))))
	(message (concat "R and Rasciidoc buffers created for " analysis-safe-name))))

(defun erasciible-insert-knitr-block (block-name)
  "Insert a knitr block BLOCK-NAME in to an R or Rasciidoc file."
  (interactive "Mblock name: ")
  (let* ((block-info (erasciible-knitr-block-type-info))
		 (begrx (nth 0 block-info))
		 (endrx (nth 1 block-info))
		 (begstr (nth 2 block-info))
		 (endstr (nth 3 block-info))
		 (mod-block-name
		  (if (boundp 'erasciible-knitr-block-prefix)
			  (concat erasciible-knitr-block-prefix "-" block-name)
			block-name)))
	(progn
	  (while (or (string-match begrx (thing-at-point 'line))
				 (string-match endrx (thing-at-point 'line)))
		(let ((next-line-add-newlines t))
		  (next-line)))
	  (if (/= 0 (current-column))
		  (progn
			(move-end-of-line 1)
			(newline-and-indent)))
	  (insert (concat begstr " " mod-block-name "\n" endstr "\n"))
	  (previous-line 2)
	  (move-end-of-line 1))))

(defun erasciible-insert-current-block-into-asciidoc ()
  "Insert a reference to the current block in to the asciidoc file at the point in that buffer."
  (interactive)
  (cond ((string-match "\\.Rasciidoc$" (buffer-name))
		 (let ((asciibuf (buffer-name))
			   (rbuf (erasciible-get-paired-knitr-buffer)))
		   (save-excursion
			 (set-buffer rbuf)
			 (let ((block (erasciible-get-current-knitr-block-name)))
			   (set-buffer asciibuf)
			   (erasciible-insert-knitr-block block)))))
		((string-match "\\.R$" (buffer-name))
		 (let ((asciibuf (erasciible-get-paired-knitr-buffer))
			   (block (erasciible-get-current-knitr-block-name)))
		   (save-excursion
			 (set-buffer asciibuf)
			 (erasciible-insert-knitr-block block))))))

(defun erasciible-copy-current-knitr-block-name ()
  "Copy the current knitr block name to the kill ring."
  (interactive)
  (let ((block (erasciible-get-current-knitr-block-name)))
	(kill-new block)))

(defun erasciible-kill-block ()
  "Kill the current block."
  (interactive)
  (let ((pos (erasciible-get-block-pos)))
	(kill-region (car pos) (cdr pos))))

(defun erasciible-goto-knitr-block ()
  "Go to a specified knitr block."
  (interactive)
  (let* ((blocks (erasciible-get-knitr-blocks))
		 (block-names (mapcar 'car blocks))
		 (selblock (ido-completing-read "Select a block: " block-names)))
	(goto-char (car (delq nil
						  (mapcar (lambda (x)
									(if (string= selblock (car x))
										(cadr x)
									  nil))
								  blocks))))))

(defun erasciible-goto-paired-knitr-block ()
  "Go to the current knitr block in the paired file."
  (interactive)
  (let* ((block (erasciible-get-current-knitr-block-name))
		 (paired-buf (erasciible-get-paired-knitr-buffer))
		 (paired-win (get-buffer-window paired-buf t))
		 (paired-frame (window-frame paired-win)))
	(raise-frame paired-frame)
	(if paired-win
		(select-window paired-win)
	  (switch-to-buffer paired-buf))
	(let* ((blocks (erasciible-get-knitr-blocks))
		   (block-names (mapcar 'car blocks)))
	  (goto-char (car (delq nil
							(mapcar (lambda (x)
									  (if (string= block (car x))
										  (cadr x)
										nil))
									blocks)))))))

(defun erasciible-append-missing-knitr-blocks ()
  "Append knitr blocks for all missing blocks to the current buffer.
These will at least get them in to the document to be computed on
and moved around if they are missing."
  (interactive)
  (goto-char (point-max))
  (insert (concat "\n" comment-start "MISSING KNITR BLOCKS\n"))
  (save-excursion
	(mapc 'erasciible-insert-knitr-block (erasciible-get-missing-knitr-blocks))))

(defun erasciible-move-block-to-archive ()
  "Take the whole current knitr block and move it to an archived file.
Only works for the R script portion"
  (interactive)						; TODO: Take block name as arg and offer completions
  (if (not (string-match "\\.R$" (buffer-name)))
	  (message "Can't move a block from anything but an R script")
	(progn
	  (erasciible-kill-block)
	  (let* ((archive-buf-name (concat "archive/archived_" (buffer-name)))
			 (archive-buf (find-file archive-buf-name)))
		(save-excursion
		  (goto-char (point-max))
		  (insert "\n")
		  (yank))))))

;;; Noninteractive Utility Functions

(defun erasciible-get-current-knitr-block-name ()
  "Get the current knitr block name."
  (save-excursion
	(save-match-data
	  (if (re-search-backward (erasciible-get-knitr-block-rx))
		  (match-string 1)))))

(defun erasciible-get-knitr-block-rx ()
  "Get a regex for a knitr block name.
Depends on the type of file the current buffer is."
  (cond ((string-match "\\.Rasciidoc$" (buffer-name))
		 "//begin.rcode\s-+\\([[:alnum:]-_]+\\)")
		((string-match "\\.R$" (buffer-name))
		 "^#[[:space:]#]+@knitr[[:space:]]+\\([[:graph:]]+\\)[[:space:]]*$")))

(defun erasciible-knitr-block-type-info ()
  "Return a list of information denoting the blocks for the current buffer.
The first item in the list is a regex to match the beginning of a
  block specification.  The second item is a regex to match the
  end of a block specification.  The third item is a string with
  the actual text to insert to begin a block prior to the block's
  name.  The fourth item is the text to put at the end of a block
  specification.  Both the second and fourth items, relating to
  the end of the block specification, may be nil."
  (save-match-data
	(cond ((string-match "R$" (buffer-file-name))
		   (list "@knitr"
				 "^#+"
				 "#### @knitr"
				 ""))
		  ((string-match "[rR]asciidoc$" (buffer-file-name))
		   (list "//begin.rcode"
				 "//end.rcode"
				 "//begin.rcode"
				 "//end.rcode")))))

(defun erasciible-get-knitr-blocks ()
  "Get a list of knitr blocks in the .R or Rasciidoc file.
Returned as a list of two element lists.  The first element is the
  name of the block, the second is the position in the file"
  (save-excursion
	(save-match-data
	  (beginning-of-buffer)
	  (let ((blocks ()))
		(while (re-search-forward (erasciible-get-knitr-block-rx) nil t)
		  (let ((matched (match-string 1)))
			(set-text-properties 0 (string-width matched) nil matched)
			(push (list matched (point)) blocks)))
		(nreverse blocks)))))

(defun erasciible-get-missing-knitr-blocks ()
  "Return a list of block names in the paired knitr file that aren't present in the current buffer."
  (let* ((paired-buf (erasciible-get-paired-knitr-buffer))
		 (these-blocks (mapcar 'car (erasciible-get-knitr-blocks)))
		 (paired-blocks
		  (save-excursion
			(set-buffer paired-buf)
			(mapcar 'car (erasciible-get-knitr-blocks)))))
	(nreverse (cl-set-difference paired-blocks these-blocks :test 'string=))))

(defun erasciible-get-block-pos ()
  "Select the whole of a block including preceding comments.
TODO: Take named block defaulting to the current one."
  (save-excursion
	(let ((rx (erasciible-get-knitr-block-rx))
		  (start (point)))
	  (message rx)
	  (if (not (re-search-backward rx 0 t))
		  (error "Couldn't match a block")
		(let ((beg (match-beginning 0)))
		  (goto-char start)
		  (let ((end (if (not (re-search-forward rx (buffer-size) t))
						 (buffer-size)
					   (match-beginning 0))))
			(cons beg end)))))))

(defun erasciible-get-paired-knitr-buffer ()
  "Gets the matching knitr buffer for the current one.
If it's an R buffer it'll be an Rasciidoc and vice-versa."
  (save-match-data
	(if (string-match "\\([[:print:]]+\\)\\.R$" (buffer-name))
		(get-buffer (concat (buffer-name) "asciidoc"))
	  (if (string-match "\\([[:print:]]+\\)\\.Rasciidoc$" (buffer-name))
		  (get-buffer (concat (match-string 1 (buffer-name)) ".R"))))))

;;; Completion Functions

(defun erasciible-get-knitr-block-args
	()
  "Arguments to knitr for block evaluation."
  (list "eval" "echo" "results" "collapse" "warning" "error"
		"message" "split" "include" "strip.white"
		"tidy" "prompt" "comment" "highlight"
		"size" "background"
		"cache" "cache.path" "cache.vars" "cache.lazy"
		"cache.comments" "cache.rebuild" "dependson"
		"autodep" "fig.path" "fig.keep" "fig.show"
		"dev" "dev.args" "fig.ext" "dpi" "fig.width" "fig.height"
		"fig.asp" "out.width" "out.height" "out.extra"
		"fig.retina" "resize.width" "resize.height"
		"fig.align" "fig.env" "fig.cap" "fig.scap"
		"fig.lp" "fig.pos" "fig.subcap" "fig.process"
		"fig.showtext" "external" "sanitize"
		"interval" "aniopts" "ffmpeg.bitrate"
		"ffmpeg.format" "code" "ref.label"
		"child" "engine" "opts.label" "purl"
		"R.options")
  )

(defun erasciible--grab-symbol ()
  "If point is at the end of a symbol, return it.
Otherwise, if point is not inside a symbol, return an empty
string.  This version is modified from company's to allow the
period characters in symbols that are commonly used in R and
knitr"
  (if (or (looking-at "\\_>")
		  (looking-back "\\."))
      (buffer-substring (point) (save-excursion (skip-syntax-backward "w_.")
                                                (point)))
    (unless (and (char-after) (memq (char-syntax (char-after)) '(?w ?_ ?.)))
      "")))

(defun erasciible-company-rasciidoc-backend (command &optional arg &rest ignored)
  "Company mode completion backend for Rasciidoc files.
Completes knitr block names and arguments.  For documentation on
COMMAND, ARG, and IGNORED see function `company-mode'."
  (interactive (list 'interactive))
  (cl-case command
	(interactive (company-begin-backend 'erasciible-company-rasciidoc-backend))
	(prefix (and (eq major-mode 'adoc-mode)
				 (let ((curline (company-grab-line "^//.*")))
				   (if curline
					   (cond ((string= curline "//")
							  curline)
							 ;; Undefined block name
							 ((string-match "//begin.rcode\s+$" curline)
							  "")
							 ;; Partial block name
							 ((string-match "//begin.rcode\s[[:graph:]]+" curline)
							  (erasciible--grab-symbol))
							 )))))
	(candidates
	 (message (concat "Arg is " arg))
	 (cond ((string= arg "//")
			(list "//begin.rcode" "//end.rcode"))

		   ;; TODO: Add support for block arguments
		   ((string= arg "")
			(let ((nwords (length (split-string (company-grab-line "^//.*")))))
 			  (cond ((eq nwords 1)
				     (erasciible-get-missing-knitr-blocks))
					((> nwords 1)
					 (erasciible-get-knitr-block-args))
					((< nwords 1)
					 (list "//begin.rcode" "//end.rcode")))))

		   ;; Partially named blocks or arguments
		   ((string-match "[[:graph:]]+" arg)
			(let ((nwords (length (split-string (company-grab-line "^//.*")))))
			  (cond ((eq nwords 1)
					 (cl-remove-if-not
					  (lambda (x) (string-prefix-p arg x))
					  (erasciible-get-missing-knitr-blocks)))
					((> nwords 1)
					 (cl-remove-if-not
					  (lambda (x) (string-prefix-p arg x))
					  (erasciible-get-knitr-block-args)))
					((< nwords 1)
					 (list)))))
		   ))))

;;; Faces

(defface erasciible-block-keyword-face
  '((default . (:inherit font-lock-warning-face :weight extra-bold
						 :height (lambda (x) (round (* x 1.5))))))
  "Font Lock face used to highlight block keywords (like @knitr) in erasciible ess buffers."
  :group 'erasciible)

(defface erasciible-block-id-face
  '((default . (:inherit font-lock-variable-name-face :weight extra-bold
						 :height (lambda (x) (round (* x 1.5))))))
  "Font Lock face used to highlight block id names in ess erasciible buffers."
  :group 'erasciible)

(defvar erasciible-mode-map
  (let ((map (make-sparse-keymap "eRASCIIble")))
	 (define-key map (kbd "C-c g") 'erasciible-goto-knitr-block)
	 (define-key map (kbd "C-c C-n") 'erasciible-new-analysis)
	 (define-key map (kbd "C-x p") 'erasciible-goto-paired-knitr-block)
	 (define-key map (kbd "C-c u") 'erasciible-copy-current-knitr-block-name)
	 (define-key map (kbd "C-c C-x i") 'erasciible-insert-current-block-into-asciidoc)
	 (define-key map (kbd "C-c b") 'erasciible-insert-knitr-block)
	 map)
  "Keymap used by `erasciible-mode'.")

;;; Define the Mode

;;;###autoload
(define-minor-mode erasciible-mode
  "Work with paired Rasciidoc and R documents for knitr-driven
reproducible document generation.

\\{erasciible-mode-map}"
  :lighter " eRASCIIble"
  :keymap erasciible-mode-map
  (font-lock-add-keywords
   nil
   '(("#.*[[:space:]]+\\(@knitr\\)"
	  1
	  'erasciible-block-keyword-face
	  prepend)
	 ("#.*[[:space:]]+\\(@knitr\\)[[:space:]]+\\([[:graph:]]+\\)"
	  2
	  'erasciible-block-id-face
	  prepend)))
  (if (fboundp 'font-lock-flush)
	  (font-lock-flush)
	(when font-lock-mode
	  (with-no-warnings
		(font-lock-fontify-buffer))))
  (add-to-list 'company-backends 'erasciible-company-rasciidoc-backend))

;;; Additional Configuration

;;;###autoload
(add-hook 'ess-mode-hook 'erasciible-mode)
(add-hook 'adoc-mode-hook 'erasciible-mode)

(provide 'erasciible-mode)

;;; erasciible-mode.el ends here
