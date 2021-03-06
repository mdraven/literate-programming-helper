
;; literate-programming-helper - support literate programming for emacs
;; Copyright (C) 2012 Iljasov Ramil
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



(defcustom literate-buffer-prefix "literate-"
  "Prefix string for literate-programming-helper"
  :type '(string))
(defcustom literate-project-filename "lp-project"
  "Project filename for literate-programming-helper"
  :type '(string))

(defvar literate-lp-syntax nil)
(defvar literate-chunks-cache (make-hash-table :test #'equal))
(defvar literate-files-hash (make-hash-table :test #'equal))
(defvar literate-syntax-functions '(("nuweb" . (literate-nuweb-parser
                                                literate-nuweb-get-target
                                                literate-nuweb-generate-target))
                                    ("noweb" . (literate-noweb-parser
                                                literate-noweb-get-target
                                                literate-noweb-generate-target))))
(defvar literate-overlays nil)
(defvar literate-lp-directory nil)
(defvar literate-lp-filename nil)
(defvar literate-src-dir nil)
(defvar literate-indicators (list))
(defvar literate-ind-current nil)

(require 'cl)


(defmacro literate-case-string (expr &rest clauses)
  "(literate-case-string \"one\"
  (\"one\" 'one)
  (\"two\" 'two)
  (\"three\" 'three))"
  (declare (indent defun))
  (let ((var (gensym)))
    `(let ((,var ,expr))
       (cond
        ,@(mapcar (lambda (x)
                    (list (list 'string= var (car x))
                          (cadr x)))
                  clauses)))))

(defun literate-agressive-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "^\n+\\|^\\s-+\\|\\s-+$\\|\n.*$" str)
    (setq str (replace-match "" t t str)))
  str)

(defun literate-empty-line-p (&optional end)
  (let ((cur (point))
        ind)
    (back-to-indentation)
    (setq ind (point))
    (goto-char cur)
    (if end
        (>= ind (min (point-at-eol) end))
      (eql ind (point-at-eol)))))

(defun literate-num-between (a b c)
  (and (<= a b)
       (<= b c)))

(defun literate-eq-buffer-and-file (filename)
  (when (file-exists-p filename)
    (let ((buf-text (buffer-substring-no-properties (point-min) (point-max))))
      (with-temp-buffer
        (insert-file-contents-literally filename)
        (string= buf-text (buffer-substring-no-properties (point-min) (point-max)))))))


(defun literate-nuweb-parser (beg-pos)
  (or (literate-closed-code-chunk-p (literate-nuweb-code-chunk-parser beg-pos))
      (literate-nuweb-include-chunk-parser beg-pos)
      (literate-nuweb-text-chunk-parser beg-pos)))

(defstruct literate-code-chunk
  subtype name body-beg body-end targets tags next-chunk)

(defun literate-check-hash-of-file (filename)
  (let ((modif-time (nth 6 (file-attributes filename))))
    (if (equal (gethash (expand-file-name filename) literate-files-hash)
               modif-time)
        nil
      (puthash (expand-file-name filename) modif-time literate-files-hash))))

(defun literate-nuweb-code-chunk-parser (beg-pos)
  (if (< (+ 2 beg-pos)
         (point-max))
      (let (subtype name body-beg body-end tags next-chunk)
        (let ((flag (buffer-substring-no-properties beg-pos (+ beg-pos 2))))
          (setq subtype
                (literate-case-string flag
                  ("@o" 'file-chunk)
                  ("@d" 'chunk))))
        (let (open tag close)
          (save-excursion
            (goto-char beg-pos)
            (let ((quote 0))
              (while (and
                      (re-search-forward "@[{}|]\\|\"" nil t)
                      (let ((match (match-string 0))
                            (line-num (point-at-eol)))
                        (if (string= match "\"")
                            (if (/= quote line-num)
                                (setq quote line-num)
                              (setq quote 0)))
                        (if (/= quote line-num)
                            (literate-case-string match
                              ("@{" (or open (setq open (point))))
                              ("@|" (or tag (setq tag (point))))
                              ("@}" (setq close (point)))))
                        (not close))))))
          (or (and open tag close (< open tag) (< tag close)
                   (setq body-end (- tag 2)
                         next-chunk close
                         tags (split-string
                               (buffer-substring-no-properties tag (- close 2)))))
              (and open close (< open close)
                   (setq body-end (- close 2)
                         next-chunk close)))
          (if open
              (setq body-beg open
                    name (literate-agressive-chomp
                          (buffer-substring-no-properties (+ beg-pos 2)
                                                          (- open 2))))))
        (labels ((get-targets (body-beg body-end)
                              (and body-beg
                                   body-end
                                   (let ((targets (list))
                                         (pos body-beg))
                                     (while
                                         (let ((target (literate-get-target pos)))
                                           (setq pos (car target))
                                           (when (and pos
                                                      (< pos body-end))
                                             (incf pos)
                                             (add-to-list 'targets (cadr target)))))
                                     targets))))
          (make-literate-code-chunk :subtype subtype :name name
                                    :body-beg body-beg :body-end body-end
                                    :targets (get-targets body-beg body-end)
                                    :tags tags
                                    :next-chunk next-chunk)))))

(defun literate-closed-code-chunk-p (chunk)
  (and (literate-code-chunk-p chunk)
       (literate-code-chunk-subtype chunk)
       (literate-code-chunk-name chunk)
       (literate-code-chunk-body-beg chunk)
       (literate-code-chunk-body-end chunk)
       chunk))

(defstruct literate-text-chunk
  body-beg body-end)

(defun literate-nuweb-text-chunk-parser (beg-pos)
  (let (body-end)
    (setq body-end (or (let ((a (save-excursion
                                  (goto-char (+ beg-pos 1))
                                  (re-search-forward "^@[odi]" nil t))))
                         (and a (- a 2)))
                       (point-max)))
    (make-literate-text-chunk :body-beg beg-pos :body-end body-end)))

(defstruct literate-include-chunk
  path body-beg next-chunk)

(defun literate-nuweb-include-chunk-parser (beg-pos)
  (if (< (+ 2 beg-pos)
         (point-max))
      (if (string= (buffer-substring-no-properties beg-pos (+ beg-pos 2))
                   "@i")
          (let (name next-chunk)
            (save-excursion
              (goto-char beg-pos)
              (if (re-search-forward "@i\\(.+?\\)$" nil t)
                  (setq next-chunk (1+ (match-end 1))
                        name (literate-agressive-chomp
                              (match-string-no-properties 1)))))
            (make-literate-include-chunk :path (expand-file-name
                                                (concat
                                                 (file-name-as-directory literate-lp-directory)
                                                 (file-name-directory literate-lp-filename)
                                                 name))
                                         :body-beg beg-pos :next-chunk next-chunk)))))

(defun literate-next-chunk-begin (chunk)
  (cond
   ((literate-code-chunk-p chunk) (let ((subtype (literate-code-chunk-subtype chunk)))
                                    (when (or (eq subtype 'chunk)
                                              (eq subtype 'file-chunk))
                                      (literate-code-chunk-next-chunk chunk))))
   ((literate-text-chunk-p chunk) (literate-text-chunk-body-end chunk))
   ((literate-include-chunk-p chunk) (literate-include-chunk-next-chunk chunk))))

(defun literate-nuweb-get-target (pos)
  (let (target-pos target-name)
	(save-excursion
	  (goto-char pos)
      (let ((quote 0))
        (while (and (re-search-forward "@[<>]\\|\"" nil t)
                    (let ((match (match-string 0))
                          (line-num (point-at-eol)))
                      (if (string= match "\"")
                          (if (/= quote line-num)
                              (setq quote line-num)
                            (setq quote 0)))
                      (if (/= quote line-num)
                          (literate-case-string match
                            ("@<" (or target-pos (setq target-pos (- (point) 2))))
                            ("@>" (and target-pos
                                       (setq target-name (buffer-substring-no-properties (+ target-pos 2)
                                                                                         (- (point) 2)))))))
                      (not target-name))))))
	(list target-pos target-name)))

(defun literate-nuweb-generate-target (name)
  (concat "@<" name "@>"))

(defun literate-get-cur-syntax-functions ()
  (if (not literate-lp-syntax)
      (progn (message "Unknown syntax. You must create or open project")
             nil)
    (let ((functions (assoc literate-lp-syntax literate-syntax-functions)))
      (if functions
          (cdr functions)
        (message (concat "Incorrect syntax: " literate-lp-syntax))
        nil))))

(defun literate-parser (beg-pos)
  (let ((functions (literate-get-cur-syntax-functions)))
    (when functions
      (funcall (car functions) beg-pos))))

(defun literate-get-target (pos)
  (let ((functions (literate-get-cur-syntax-functions)))
    (when functions
      (funcall (cadr functions) pos))))

(defun literate-generate-target (name)
  (let ((functions (literate-get-cur-syntax-functions)))
    (when functions
      (funcall (caddr functions) name))))

(defun literate-parse-file (filename)
  (let ((chunks-cache (make-hash-table :test #'equal)))
    (labels ((helper (filename)
                     (if (literate-check-hash-of-file filename)
                         (with-temp-buffer
                           (insert-file-contents-literally filename)
                           (let ((next-chunk-pos 1) chunk)
                             (while (progn
                                      (setq chunk (literate-parser next-chunk-pos)
                                            next-chunk-pos (literate-next-chunk-begin chunk))
                                      (let* ((ex-filename (expand-file-name filename))
                                             (list (or (gethash ex-filename chunks-cache)
                                                       (list))))
                                        (setq list (append list (list chunk)))
                                        (puthash ex-filename list chunks-cache))
                                      (when (literate-include-chunk-p chunk)
                                        (helper (literate-include-chunk-path chunk)))
                                      (< next-chunk-pos (point-max))))))
                       (let* ((ex-filename (expand-file-name filename))
                              (chunks (gethash ex-filename
                                               literate-chunks-cache)))
                         (puthash ex-filename chunks chunks-cache)
                         (dolist (chunk chunks)
                           (when (literate-include-chunk-p chunk)
                             (helper (literate-include-chunk-path chunk))))))))
      (helper filename)
      (setq literate-chunks-cache chunks-cache)))
  (let ((chunks-by-name (make-hash-table :test #'equal))
        (chunks-dependences (make-hash-table :test #'equal))
        (chunks-files (list)))
    (labels ((conc-to-hash (chunkname body-beg body-end targets filename)
                           (let ((list (or (gethash chunkname chunks-by-name)
                                           (list))))
                             (push (list body-beg body-end filename) list)
                             (puthash chunkname list chunks-by-name))
                           (dolist (i targets)
                             (let ((list (or (gethash i chunks-dependences)
                                             (list))))
                               (add-to-list 'list chunkname)
                               (puthash i list chunks-dependences))))
             (helper (filename)
                     (let ((chunks (gethash (expand-file-name filename)
                                            literate-chunks-cache)))
                       (dolist (chunk chunks)
                         (cond
                          ((literate-code-chunk-p chunk)
                           (let ((subtype (literate-code-chunk-subtype chunk)))
                             (when (or (eq subtype 'chunk)
                                       (eq subtype 'file-chunk))
                               (conc-to-hash (literate-code-chunk-name chunk)
                                             (literate-code-chunk-body-beg chunk)
                                             (literate-code-chunk-body-end chunk)
                                             (literate-code-chunk-targets chunk)
                                             filename)
                               (when (eq subtype 'file-chunk)
                                 (add-to-list 'chunks-files
                                              (literate-code-chunk-name chunk))))))
                          ((literate-text-chunk-p chunk) ())
                          ((literate-include-chunk-p chunk)
                           (helper (literate-include-chunk-path chunk))))))))
      (helper filename))
    (list chunks-by-name chunks-dependences chunks-files)))

(defun literate-noweb-parser (beg-pos)
  (or (literate-noweb-code-chunk-parser beg-pos)
      (literate-noweb-text-chunk-parser beg-pos)))

(defun literate-noweb-code-chunk-parser (beg-pos)
  (if (< (+ 5 beg-pos)
         (point-max))
      (let (subtype name body-beg body-end next-chunk)
        (save-excursion
          (goto-char beg-pos)
          (when (and (re-search-forward "^<<\\(.+?\\)>>=.*$" nil t)
                     (= (match-beginning 0) beg-pos))
            (setq body-beg (1+ (match-end 0))
                  name (match-string 1))
            (setq subtype (if (string-match "^.+\\..+$" name) 'file-chunk
                            'chunk))
            (if (re-search-forward "^@.*$" nil t)
                (setq next-chunk (1+ (match-end 0))
                      body-end (- next-chunk 2))
              (setq next-chunk (point-max)
                    body-end next-chunk))
            (make-literate-code-chunk :subtype subtype :name name
                                      :body-beg body-beg :body-end body-end
                                      :tags nil :next-chunk next-chunk))))))

(defun literate-noweb-text-chunk-parser (beg-pos)
  (let (body-end)
    (setq body-end (or (save-excursion
                         (goto-char (+ beg-pos 1))
                         (when (re-search-forward "^<<.+?>>=\\|^@" nil t)
                           (match-beginning 0)))
                       (point-max)))
    (make-literate-text-chunk :body-beg beg-pos :body-end body-end)))

(defun literate-noweb-get-target (pos)
  (let (target-pos target-name)
	(save-excursion
	  (goto-char pos)
	  (when (re-search-forward "<<\\(.+?\\)>>" nil t)
		(setq target-pos (match-beginning 0))
		(setq target-name (match-string 1))))
	(list target-pos target-name)))

(defun literate-noweb-generate-target (name)
  (concat "<<" name ">>"))


(defun literate-expand-file (filename chunks &optional remove-unfound-chunks)
  (prog1
      (with-current-buffer (generate-new-buffer (concat literate-buffer-prefix filename))
        (literate-insert-parts-of-chunks chunks filename)
        (goto-char (point-min)) ;; (beginning-of-buffer)
        (literate-expand-targets chunks remove-unfound-chunks))
    (dolist (i literate-overlays)
      (overlay-put i 'modification-hooks (list #'literate-overlay-modification)))))

(defun literate-expand-targets (chunks &optional remove-unfound-chunks)
  (let (unfound-chunks)
    (let (target target-beg-line target-pos target-name)
      (while (setq target (literate-get-target (point))
                   target-pos (car target)
                   target-name (cadr target))

        (if (or remove-unfound-chunks
                (gethash target-name chunks))
            (delete-region target-pos (+ target-pos
                                         (length (literate-generate-target target-name)))))

        (goto-char target-pos)
        (setq target-beg-line (point-at-bol))

        (if (gethash target-name chunks)
            (progn
              (let ((spaces-first-line (literate-spaces-before-first-string chunks target-name))
                    tabs tabs-str)
                (setq tabs (- (- target-pos target-beg-line)
                              spaces-first-line))
                (if (< tabs 0)
                    (setq tabs 0))
                (setq tabs-str (make-string tabs ?\s))

                (goto-char target-pos)
                (literate-insert-parts-of-chunks chunks target-name)

                (let ((end-of-chunks-block (point)))
                  (while (> (point-at-bol) target-beg-line)
                    (move-beginning-of-line nil)
                    (unless (literate-empty-line-p end-of-chunks-block)
                      (insert tabs-str))
                    (forward-line -1)))

                (goto-char target-pos)
                (delete-char spaces-first-line)))
          (push target-name unfound-chunks))
        (forward-char)))
    unfound-chunks))

(defun literate-insert-parts-of-chunks (hash chunkname)
  (let ((point (point))
        (list (reverse (gethash chunkname hash))))
    (dolist (i list)
      (let ((file (caddr i))
            (beg (1- (car i)))
            (end (1- (cadr i))))
        (insert-file-contents-literally file nil beg end)
        (let ((overlay (make-overlay point (+ point (- end beg)))))
          (push overlay literate-overlays)
          (overlay-put overlay 'literate-chunk (list i chunkname)))
        (setq point (+ point (- end beg)))
        (goto-char point)))
    list))

(defun literate-overlay-modification (overlay flag beg end &rest args)
  (when flag
    (overlay-put overlay 'literate-modify t)))

(defun literate-spaces-before-first-string (hash chunkname)
  (let ((list (gethash chunkname hash)))
    (when list
      (let ((beg (1- (car (car (last list)))))
            (end (1- (cadr (car (last list)))))
            (file (caddr (car (last list)))))
        (with-temp-buffer
          (insert-file-contents-literally file nil beg end)
          (back-to-indentation)
          (1- (point)))))))


(defun literate-get-filenames-list-from-overlays ()
  (let (list)
    (dolist (i literate-overlays)
      (let ((chunk (car (overlay-get i 'literate-chunk))))
        (when (and chunk
                   (overlay-get i 'literate-modify))
          (push (caddr chunk) list))))
    (delete-dups list)))

(defun literate-list-subtract (a b)
  (remove-if (lambda (x) (member x b)) a))

(defun literate-buffer-to-LP ()
  (let ((literate-overlays literate-overlays)
        (files (literate-get-filenames-list-from-overlays)))
    ;; Remove modification-hooks
    (dolist (i literate-overlays)
      (overlay-put i 'modification-hooks nil))
    ;; Create buffers
    (dolist (i files)
      (with-current-buffer (generate-new-buffer (concat literate-buffer-prefix i))
        (insert-file-contents-literally i)))
    ;; Create markers
    (dolist (i literate-overlays)
      (when (overlay-get i 'literate-modify)
        (let* ((chunk (car (overlay-get i 'literate-chunk)))
               (bufname (concat literate-buffer-prefix (caddr chunk)))
               (beg-marker (make-marker))
               (end-marker (make-marker)))
          (with-current-buffer bufname
            ;; (set-marker-insertion-type beg-marker t)
            (set-marker-insertion-type end-marker t)
            (set-marker beg-marker (car chunk))
            (set-marker end-marker (cadr chunk))
            (overlay-put i 'literate-marker (list beg-marker end-marker))))))
    ;; Update chunks in LP
    (while literate-overlays
      (let* ((overlay (car literate-overlays))
             (chunkname (cadr (overlay-get overlay 'literate-chunk))))

        (with-current-buffer (overlay-buffer overlay)
          (let (overlays-and-pos overlays beg-overlays end-overlays rem-spaces)

            (setq overlays-and-pos (literate-get-overlays-near-pos-with-chunkname
                                    (overlay-start overlay)
                                    literate-overlays
                                    chunkname))
            (makunbound 'overlay)

            (setq overlays (car overlays-and-pos)
                  beg-overlays (cadr overlays-and-pos)
                  end-overlays (caddr overlays-and-pos)
                  rem-spaces (literate-get-spaces-before-overlay beg-overlays end-overlays))

            (setq literate-overlays (literate-list-subtract literate-overlays
                                                            overlays))

            (dolist (i overlays)
              (when (overlay-get i 'literate-modify)
                  (let ((markers (overlay-get i 'literate-marker)))
                    (let* ((beg-body (overlay-start i))
                           (end-body (overlay-end i))
                           (body (buffer-substring-no-properties beg-body end-body))
                           (something-written-before-body))
                      (goto-char beg-body)
                      (back-to-indentation)
                      (if (< (point) beg-body)
                          (setq something-written-before-body t))

                      (with-current-buffer (marker-buffer (car markers))
                        (delete-region (marker-position (car markers))
                                       (marker-position (cadr markers)))
                        (save-excursion
                          (goto-char (marker-position (car markers)))
                          (insert body)
                          (goto-char (marker-position (car markers)))

                          (when (= beg-body beg-overlays)
                            (let ((delete-char (car rem-spaces)))
                              (if (> delete-char 0)
                                  (delete-char delete-char)
                                (insert (make-string (abs delete-char) ?\s))))
                            (forward-line))
                          (when (and (/= beg-body beg-overlays)
                                     (< (point) (marker-position (cadr markers))))
                            (unless (and something-written-before-body
                                         (literate-empty-line-p))
                              (delete-char (cadr rem-spaces)))
                            (forward-line))
                          (while (< (point) (marker-position (cadr markers)))
                            (unless (literate-empty-line-p)
                              (delete-char (cadr rem-spaces)))
                            (forward-line))))))))
            (delete-region beg-overlays end-overlays)
            (goto-char beg-overlays)
            (insert (concat (make-string (caddr rem-spaces) ?\s)
                            (literate-generate-target chunkname)))))))
    ;; Save & kill buffers
    (save-current-buffer
      (dolist (i files)
        (set-buffer (concat literate-buffer-prefix i))
        (write-region (point-min) (point-max) i)
        (kill-buffer)))))

(defun literate-get-overlays-near-pos-with-chunkname (pos overlays-list chunkname)
  (let (overlays beg-overlays end-overlays)
    (let ((overlays-list overlays-list))
      (while overlays-list
        (let* ((overlay (car overlays-list))
               (beg (overlay-start overlay))
               (end (overlay-end overlay))
               (name (cadr (overlay-get overlay 'literate-chunk))))
          (when (and (literate-num-between beg pos end)
                     (string= chunkname name))
            (when (or (null beg-overlays)
                      (< beg beg-overlays))
              (setq beg-overlays beg))
            (when (or (null end-overlays)
                      (> end end-overlays))
              (setq end-overlays end))
            (push overlay overlays))
          (setq overlays-list (cdr overlays-list)))))

    (when (and beg-overlays
               (> pos beg-overlays))
      (let ((ret (literate-get-overlays-near-pos-with-chunkname
                  beg-overlays
                  (literate-list-subtract overlays-list
                                          overlays)
                  chunkname)))
        (setq overlays (append overlays (car ret)))
        (when (and (cadr ret)
                   (> beg-overlays (cadr ret)))
          (setq beg-overlays (cadr ret)))))

    (when (and end-overlays
               (< pos end-overlays))
      (let ((ret (literate-get-overlays-near-pos-with-chunkname
                  end-overlays
                  (literate-list-subtract overlays-list
                                          overlays)
                  chunkname)))
        (setq overlays (append overlays (car ret)))
        (when (and (caddr ret)
                   (< end-overlays (caddr ret)))
          (setq end-overlays (caddr ret)))))
    (list overlays beg-overlays end-overlays)))

(defun literate-get-spaces-before-overlay (beg-pos end-pos)
  (let (rem-first rem-other shift-target)
    (save-excursion
      (let (beg-code target-num-spaces other-num-spaces first-num-spaces)
        (goto-char beg-pos)
        (setq target-num-spaces (- beg-pos (point-at-bol)))

        (while (equal (char-after) ?\s)
          (forward-char))
        (setq beg-code (point))

        (when (<= (point) end-pos)
          (setq first-num-spaces (- beg-code (point-at-bol)))
          (setq other-num-spaces first-num-spaces))
        (forward-line)

        (while (< (point) end-pos)
          (let ((beg-line (point)))
            (back-to-indentation)
            (when (< (point) (min (point-at-eol)
                                  end-pos))
              (let ((diff (- (point) beg-line)))
                (when (< diff other-num-spaces)
                  (setq other-num-spaces diff)))))
          (forward-line))

        (if (and first-num-spaces other-num-spaces)
            (progn
              (setq shift-target (- first-num-spaces target-num-spaces)
                    rem-other (min first-num-spaces other-num-spaces)
                    rem-first (if (<= first-num-spaces other-num-spaces)
                                  shift-target
                                (- other-num-spaces first-num-spaces))))
          (setq shift-target 0
                rem-other 0
                rem-first 0))))
    (list rem-first rem-other shift-target)))


(defun literate-save-lp-config (&optional create-p)
  (interactive)
  (if (null literate-lp-directory)
      (progn (message "The project file has not been saved. You must first create or open project")
             nil)
    (let ((proj-file (concat (file-name-as-directory literate-lp-directory)
                             literate-project-filename)))
      (if (and (not (file-exists-p proj-file))
               (not create-p))
          (message (concat "The project file " proj-file " doesn't exist"))
        (with-temp-buffer
          (when literate-lp-syntax
            (insert "Syntax: " literate-lp-syntax)
            (newline))
          (when literate-lp-filename
            (insert "LPFile: " literate-lp-filename)
            (newline))
          (when literate-src-dir
            (insert "SrcDir: " literate-src-dir)
            (newline))
          (write-file proj-file)
          t)))))

(defun literate-set-lp-file (filename)
  (interactive "FLP file: ")
  (if (null literate-lp-directory)
      (progn (message "You must first create or open project")
             nil)
    (setq literate-lp-filename (file-relative-name filename literate-lp-directory))
    (literate-save-lp-config)
    (unless (file-exists-p filename)
      (find-file filename))
    t))

(defun literate-set-src-dir (dir-path)
  (interactive
   (list
    (read-directory-name "Source directory: " nil nil nil
                         (or literate-src-dir "src"))))
  (if (null literate-lp-directory)
      (progn (message "You must first create or open project")
             nil)
    (if (file-exists-p dir-path)
        (unless (file-directory-p dir-path)
          (message (concat dir-path " isn't directory")))
      (make-directory dir-path))
    (when (file-directory-p dir-path)
      (setq literate-src-dir (file-relative-name dir-path literate-lp-directory))
      (literate-save-lp-config))))

;;;###autoload
(defun literate-create-lp-project (dir-path syntax lp-file src-dir)
  (interactive
   (list
    (read-directory-name "LP project directory: ")
    (let ((syntax-types (mapcar #'car literate-syntax-functions)))
      (completing-read "LP syntax type: " syntax-types nil t (car syntax-types)))
    (read-file-name "LP file: ")
    (read-directory-name "Source directory: " nil nil nil "src")))
  (let ((proj-file (concat (file-name-as-directory dir-path)
                           literate-project-filename)))
    (if (file-exists-p proj-file)
        (message (concat "The project file " proj-file " already exists"))
      (setq literate-lp-directory dir-path
            literate-lp-syntax syntax
            literate-lp-filename nil
            literate-src-dir nil)
      (and (literate-save-lp-config t)
           (literate-set-lp-file lp-file)
           (literate-set-src-dir src-dir)))))

(defun literate-filter-correct-syntax (syntax)
  (let ((syntax-types (mapcar #'car literate-syntax-functions)))
    (when (member syntax syntax-types)
      syntax)))

;;;###autoload
(defun literate-open-lp-project (dir-path)
  (interactive "DLP project directory: ")
  (let ((proj-file (concat (file-name-as-directory dir-path)
                           literate-project-filename)))
    (if (not (file-exists-p proj-file))
        (message (concat "The project file " proj-file " doesn't exist"))
      (setq literate-lp-directory dir-path
            literate-lp-syntax nil
            literate-lp-filename nil
            literate-src-dir nil)
       (with-temp-buffer
        (insert-file-contents proj-file)
        (goto-char (point-min))
        (while (re-search-forward "^\\([[:alpha:]]+\\):[[:blank:]]*\\(.+?\\)[[:blank:]]*$" nil t)
          (let ((var (match-string 1))
                (val (match-string 2)))
            (literate-case-string var
              ("Syntax"  (setq literate-lp-syntax
                               (literate-filter-correct-syntax val)))
              ("LPFile" (setq literate-lp-filename val))
              ("SrcDir" (setq literate-src-dir val)))))))))


(defun literate-get-target-files (chunks-dependences chunks-files chunk-name)
  (let ((files (list))
        (visited-chunks (list)))
    (labels ((helper (chunk-name)
                     (if (member chunk-name chunks-files)
                         (add-to-list 'files chunk-name)
                       (dolist (i (gethash chunk-name chunks-dependences))
                         (unless (member i visited-chunks)
                           (add-to-list 'visited-chunks i)
                           (helper i))))))
      (helper chunk-name))
    files))

(defun literate-go-to-body-position (pos)
  (interactive "d")
  (let ((cur-point (position-bytes pos))
        (filename-buffer (buffer-file-name)))
    (when filename-buffer
      (dolist (i literate-overlays)
        (let* ((chunk (car (overlay-get i 'literate-chunk)))
               (beg (car chunk))
               (end (cadr chunk))
               (filename-chunk (caddr chunk)))
          (when (and (literate-num-between beg cur-point end)
                     (string= filename-buffer (expand-file-name filename-chunk)))
            (switch-to-buffer (overlay-buffer i))
            (goto-char (overlay-start i))
            (return)))))))

(defun literate-get-chunk-name (chunks pos)
  (let (chunk-name)
    (catch 'break
      (let ((cur-point (position-bytes pos))
            (filename-buffer (buffer-file-name)))
        (maphash (lambda (key val)
                   (dolist (i val)
                     (let ((body-beg (car i))
                           (body-end (cadr i))
                           (filename (caddr i)))
                       (when (and (literate-num-between body-beg cur-point body-end)
                                  (string= filename-buffer (expand-file-name filename)))
                         (setq chunk-name key)
                         (throw 'break t)))))
                 chunks)))
    chunk-name))

(defun literate-generate-and-go (pos &optional arg)
  (interactive "d\np")
  (setq literate-overlays (list))
  (let ((parse (literate-parse-file (concat (file-name-as-directory literate-lp-directory)
                                            literate-lp-filename))))
    (let ((chunks (car parse))
          (dependences (cadr parse))
          (files (caddr parse)))
      (let ((files (literate-get-target-files dependences files
                                              (literate-get-chunk-name chunks pos)))
            file)
        (setq file (if (eql arg 4)
                       (completing-read "File: " files nil t (car files))
                     (car files)))
        (when file
          (literate-expand-file file chunks)
          (when (and literate-lp-directory
                     literate-src-dir)
            (with-current-buffer (concat literate-buffer-prefix file)
              (write-file (concat (file-name-as-directory literate-lp-directory)
                                  (file-name-as-directory literate-src-dir)
                                  file))))))))
  (literate-go-to-body-position pos)
  (literate-code-mode t))

(defun literate-go-back ()
  (interactive)
  (when (and literate-overlays
             (overlay-buffer (car (last literate-overlays))))
    (let ((buffer (overlay-buffer (car literate-overlays))))
      (with-current-buffer buffer
        (when (buffer-file-name)
          (save-buffer)))
      (literate-buffer-to-LP)
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (kill-buffer))
      (setq literate-overlays nil))))

(defun literate-generate-all-files (&optional arg)
  (interactive "p")
  (setq literate-overlays (list))
  (let ((parse (literate-parse-file (concat (file-name-as-directory literate-lp-directory)
                                            literate-lp-filename))))
    (let ((chunks (car parse))
          (files (caddr parse)))
      (when (and literate-lp-directory
                 literate-src-dir)
        (dolist (file files)
          (literate-expand-file file chunks (eql arg 4))
          (with-current-buffer (concat literate-buffer-prefix file)
            (let ((filename (concat (file-name-as-directory literate-lp-directory)
                                    (file-name-as-directory literate-src-dir)
                                    file)))
              (unless (literate-eq-buffer-and-file filename)
                (write-file filename)))
            (kill-buffer)))))))


(define-minor-mode literate-code-mode
  "Minor mode for generated code from a LP-text"
  nil
  nil
  '(("\C-cc" . literate-go-back))
  (if literate-code-mode
      (progn
        (literate-set-window-margin t)
        (add-hook 'post-command-hook 'literate-code-ind-current-overlay nil t)
        (add-hook 'after-change-functions 'literate-code-ind-after-change nil t))
    (remove-hook 'post-command-hook 'literate-code-ind-current-overlay t)
    (remove-hook 'after-change-functions 'literate-code-ind-after-change t)
    ;; Remove chunk's indicators
    (literate-set-window-margin nil)
    (literate-remove-indicators)
    (setq literate-ind-current nil)))

(defun literate-remove-indicators ()
  (mapc #'delete-overlay literate-indicators)
  (setq literate-indicators (list)))

(defun literate-fill-indicator (overlay)
  (when (and overlay
             (not (equal overlay literate-ind-current)))
    (setq literate-ind-current overlay)
    (let ((beg (overlay-start overlay))
          (end (overlay-end overlay)))
      (when (< beg end)
        (literate-remove-indicators)
        (save-excursion
          (goto-char beg)
          (catch 'break
            (while (< (point) end)
              (let ((ind (make-overlay (point) (point))))
                (push ind literate-indicators)
                (overlay-put ind 'before-string
                             (propertize " " 'display `((margin left-margin) "-"))))
              (if (/= (forward-line) 0)
                  (throw 'break t)))))))))

(defun literate-set-window-margin (activate)
  (let* ((margin (window-margins))
         (left 0)
         (right (cdr margin)))
    (when (car margin)
      (setq left (car margin)))
    (if activate
        (incf left)
      (decf left))
    (set-window-margins nil left right)))

(defun literate-get-overlay-for-indication (pos)
  (dolist (i literate-overlays)
    (when (literate-num-between (overlay-start i)
                                pos
                                (overlay-end i))
      (return i))))

(defun literate-code-ind-current-overlay ()
  (literate-fill-indicator
   (literate-get-overlay-for-indication (point))))

(defun literate-code-ind-after-change (&optional beg end len)
  (setq literate-ind-current nil)
  (literate-code-ind-current-overlay))



(provide 'literate-programming-helper)
