
;; literate-mode - support literate programming for emacs
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
  "Prefix string for literate-mode"
  :type '(string))

(defcustom literate-project-filename "lp-project"
  "Project filename for literate-mode"
  :type '(string))


(defvar literate-syntax-types '("nuweb" "noweb"))
(defvar literate-lp-directory nil)
(defvar literate-lp-syntax nil)
(defvar literate-lp-filename nil)
(defvar literate-src-dir nil)



(require 'cl)



(defmacro literate-case-string (expr &rest clauses)
  "(literate-case-string \"one\"
                      (\"one\" 'one)
                      (\"two\" 'two)
                      (\"three\" 'three))"
  (declare (indent defun))
  `(let ((var123 ,expr))
     (cond
      ,@(mapcar (lambda (x)
                  (list (list 'string= 'var123 (car x))
                        (cadr x)))
                clauses))))

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


(defun literate-nuweb-parser (beg-pos)
  (or (literate-code-chunk-p (literate-nuweb-code-chunk-parser beg-pos))
      (literate-nuweb-include-chunk-parser beg-pos)
      (literate-nuweb-text-chunk-parser beg-pos)))

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
        (list subtype name body-beg body-end tags next-chunk))))

(defun literate-code-chunk-p (chunk)
  (and (car chunk) (cadr chunk) (caddr chunk) (cadddr chunk) chunk))

(defun literate-nuweb-text-chunk-parser (beg-pos)
  (let (body-end)
    (setq body-end (or (let ((a (save-excursion
                                  (goto-char (+ beg-pos 1))
                                  (re-search-forward "^@[odi]" nil t))))
                         (and a (- a 2)))
                       (point-max)))
    (list 'text beg-pos body-end)))

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
            (list 'include name beg-pos next-chunk)))))

(defun literate-next-chunk-begin (chunk)
  (case (car chunk)
    ('chunk (cadddr (cddr chunk)))
    ('file-chunk (cadddr (cddr chunk)))
    ('include (cadddr chunk))
    ('text (caddr chunk))))

(defun literate-parse-file (filename)
  (let ((chunks-by-name (make-hash-table :test #'equal))
        (chunks-dependences (make-hash-table :test #'equal))
        (chunks-files (list)))
    (labels ((get-targets (body-beg body-end)
                          (let ((targets (list))
                                (pos body-beg))
                            (while
                                (let ((target (literate-nuweb-get-target pos)))
                                  (setq pos (car target))
                                  (when (and pos
                                             (< pos body-end))
                                    (incf pos)
                                    (add-to-list 'targets (cadr target)))))
                            targets))
             (conc-to-hash (chunkname body-beg body-end filename)
                           (let ((list (or (gethash chunkname chunks-by-name)
                                           (list))))
                             (push (list body-beg body-end filename) list)
                             (puthash chunkname list chunks-by-name))
                           (let ((targets (get-targets body-beg body-end)))
                             (dolist (i targets)
                               (let ((list (or (gethash i chunks-dependences)
                                               (list))))
                                 (add-to-list 'list chunkname)
                                 (puthash i list chunks-dependences)))))
             (helper (filename)
                     (with-temp-buffer
                       (insert-file-contents-literally filename)
                       (let ((next-chunk-pos 1) chunk)
                         (while (progn
                                  (setq chunk (literate-nuweb-parser next-chunk-pos)
                                        next-chunk-pos (literate-next-chunk-begin chunk))
                                  (case (car chunk)
                                    ('chunk (conc-to-hash (cadr chunk)
                                                          (caddr chunk)
                                                          (cadddr chunk)
                                                          filename))
                                    ('file-chunk (conc-to-hash (cadr chunk)
                                                               (caddr chunk)
                                                               (cadddr chunk)
                                                               filename)
                                                 (add-to-list 'chunks-files (cadr chunk)))
                                    ('include (helper (cadr chunk)))
                                    ('text ()))
                                  (< next-chunk-pos (point-max))))))))
      (helper filename))
    (list chunks-by-name chunks-dependences chunks-files)))

(defun literate-nuweb-get-target (pos)
  (let (target-pos target-name)
	(save-excursion
	  (goto-char pos)
	  (when (re-search-forward "@<\\(.+?\\)@>" nil t)
		(setq target-pos (match-beginning 0))
		(setq target-name (match-string 1))))
	(list target-pos target-name)))


(defun literate-expand-file (filename chunks)
  (with-current-buffer (generate-new-buffer (concat literate-buffer-prefix filename))
    (literate-insert-parts-of-chunks chunks filename)
    (beginning-of-buffer)
    (literate-expand-targets chunks)))

(defun literate-expand-targets (chunks &optional remove-unfound-chunks)
  (let (unfound-chunks)
    (let (target target-beg-line target-pos target-name)
      (while (setq target (literate-nuweb-get-target (point))
                   target-pos (car target)
                   target-name (cadr target))

        (if (or remove-unfound-chunks
                (gethash target-name chunks))
            (replace-match ""))

        (goto-char target-pos)
        (setq target-beg-line (point-at-bol))

        (if (gethash target-name chunks)
            (progn
              (let ((spaces-first-line (spaces-before-first-string chunks target-name))
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
          (push overlay *overlays*)
          (overlay-put overlay 'literate-chunk (list i chunkname)))
        (setq point (+ point (- end beg)))
        (goto-char point)))
    list))

(defun spaces-before-first-string (hash chunkname)
  (let ((list (gethash chunkname hash)))
    (when list
      (let ((beg (1- (car (car (last list)))))
            (end (1- (cadr (car (last list)))))
            (file (caddr (car (last list)))))
        (with-temp-buffer
          (insert-file-contents-literally file nil beg end)
          (back-to-indentation)
          (1- (point)))))))


(defun get-filenames-list-from-*overlays* ()
  (let (list)
    (dolist (i *overlays*)
      (let ((chunk (car (overlay-get i 'literate-chunk))))
        (if chunk
            (push (caddr chunk) list))))
    (delete-dups list)))

(defun list-subtract (a b)
  (remove-if (lambda (x) (member x b)) a))

(defun buffer-to-LP ()
  (let ((*overlays* *overlays*)
        (files (get-filenames-list-from-*overlays*)))
    ;; Create buffers
    (dolist (i files)
      (with-current-buffer (generate-new-buffer (concat literate-buffer-prefix i))
        (insert-file-contents-literally i)))
    ;; Create markers
    (dolist (i *overlays*)
      (let* ((chunk (car (overlay-get i 'literate-chunk)))
             (bufname (concat literate-buffer-prefix (caddr chunk)))
             (beg-marker (make-marker))
             (end-marker (make-marker)))
        (with-current-buffer bufname
          ;; (set-marker-insertion-type beg-marker t)
          (set-marker-insertion-type end-marker t)
          (set-marker beg-marker (car chunk))
          (set-marker end-marker (cadr chunk))
          (overlay-put i 'literate-marker (list beg-marker end-marker)))))
    ;; Update chunks in LP
    (while *overlays*
      (let* ((overlay (car *overlays*))
             (chunkname (cadr (overlay-get overlay 'literate-chunk))))

        (with-current-buffer (overlay-buffer overlay)
          (let (overlays-and-pos overlays beg-overlays end-overlays rem-spaces)

            (setq overlays-and-pos (get-overlays-near-pos-with-chunkname (overlay-start overlay)
                                                                         *overlays*
                                                                         chunkname))
            (makunbound 'overlay)

            (setq overlays (car overlays-and-pos)
                  beg-overlays (cadr overlays-and-pos)
                  end-overlays (caddr overlays-and-pos)
                  rem-spaces (get-spaces-before-overlay beg-overlays end-overlays))

            (setq *overlays* (list-subtract *overlays* overlays))

            (dolist (i overlays)
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
                        (forward-line 1))
                      (when (and (/= beg-body beg-overlays)
                                 (< (point) (marker-position (cadr markers))))
                        (unless (and something-written-before-body
                                     (literate-empty-line-p))
                          (delete-char (cadr rem-spaces)))
                        (forward-line 1))
                      (while (< (point) (marker-position (cadr markers)))
                        (unless (literate-empty-line-p)
                          (delete-char (cadr rem-spaces)))
                        (forward-line 1)))))))
            (delete-region beg-overlays end-overlays)
            (goto-char beg-overlays)
            (insert (concat (make-string (caddr rem-spaces) ?\s)
                            "@<" chunkname "@>"))))))
    ;; Save & kill buffers
    (save-current-buffer
      (dolist (i files)
        (set-buffer (concat literate-buffer-prefix i))
        (write-region (point-min) (point-max) i)
        (kill-buffer)))))

(defun get-overlays-near-pos-with-chunkname (pos overlays-list chunkname)
  (let (overlays beg-overlays end-overlays)
    (let ((overlays-list overlays-list))
      (while overlays-list
        (let* ((overlay (car overlays-list))
               (beg (overlay-start overlay))
               (end (overlay-end overlay))
               (name (cadr (overlay-get overlay 'literate-chunk))))
          (when (and (>= pos beg)
                     (<= pos end)
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
      (let ((ret (get-overlays-near-pos-with-chunkname beg-overlays
                                                       (list-subtract overlays-list
                                                                      overlays)
                                                       chunkname)))
        (setq overlays (append overlays (car ret)))
        (when (and (cadr ret)
                   (> beg-overlays (cadr ret)))
          (setq beg-overlays (cadr ret)))))

    (when (and end-overlays
               (< pos end-overlays))
      (let ((ret (get-overlays-near-pos-with-chunkname end-overlays
                                                       (list-subtract overlays-list
                                                                      overlays)
                                                       chunkname)))
        (setq overlays (append overlays (car ret)))
        (when (and (caddr ret)
                   (< end-overlays (caddr ret)))
          (setq end-overlays (caddr ret)))))
    (list overlays beg-overlays end-overlays)))

(defun get-spaces-before-overlay (beg-pos end-pos)
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
        (forward-line 1)

        (while (< (point) end-pos)
          (let ((beg-line (point)))
            (back-to-indentation)
            (when (< (point) (min (point-at-eol)
                                  end-pos))
              (let ((diff (- (point) beg-line)))
                (when (< diff other-num-spaces)
                  (setq other-num-spaces diff)))))
          (forward-line 1))

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
    (let ((proj-file (concat literate-lp-directory literate-project-filename)))
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

(defun literate-create-lp-project (dir-path syntax lp-file src-dir)
  (interactive
   (list
    (read-directory-name "LP project directory: ")
    (completing-read "LP syntax type: " literate-syntax-types nil t (car literate-syntax-types))
    (read-file-name "LP file: ")
    (read-directory-name "Source directory: " nil nil nil "src")))
  (let ((proj-file (concat dir-path literate-project-filename)))
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
  (when (member syntax literate-syntax-types)
    syntax))

(defun literate-open-lp-project (dir-path)
  (interactive "DLP project directory: ")
  (let ((proj-file (concat dir-path literate-project-filename)))
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


(defun get-target-files (chunks-dependences chunks-files chunk-name)
  (let ((files (list))
        (visited-chunks (list)))
    (labels ((helper (chunk-name)
                     (if (member chunk-name chunks-files)
                         (add-to-list 'files chunk-name)
                       (dolist (i (gethash chunk-name chunks-dependences))
                         (unless (member i visited-chunks)
                           (helper i)
                           (add-to-list 'visited-chunks i))))))
      (helper chunk-name))
    files))

(defun go-to-body-position (pos)
  (interactive "d")
  (let ((cur-point (position-bytes pos))
        (filename-buffer (buffer-file-name)))
    (when filename-buffer
      (dolist (i *overlays*)
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
                     (when (and (literate-num-between (car i) cur-point (cadr i))
                                (string= filename-buffer (expand-file-name (caddr i))))
                       (setq chunk-name key)
                       (throw 'break t))))
                 chunks)))
    chunk-name))

(defun literate-generate-and-go (pos)
  (interactive "d")
  (setq *overlays*
        (let ((parse (literate-parse-file (concat literate-lp-directory "/"
                                                  literate-lp-filename)))
              (*overlays* (list)))
          (let ((chunks (car parse))
                (dependences (cadr parse))
                (files (caddr parse)))
            (let ((file (car (get-target-files dependences files
                                               (literate-get-chunk-name chunks (point))))))
              (when file
                (literate-expand-file file chunks)
                (when (and literate-lp-directory
                           literate-src-dir)
                  (with-current-buffer (concat literate-buffer-prefix file)
                    (write-file (concat literate-lp-directory "/"
                                        literate-src-dir "/"
                                        file)))))))
          *overlays*))
  (go-to-body-position (point)))

(defun literate-go-back ()
  (interactive)
  (when (and *overlays*
             (overlay-buffer (car (last *overlays*))))
    (let ((buffer (overlay-buffer (car *overlays*))))
      (with-current-buffer buffer
        (when (buffer-file-name)
          (save-buffer)))
      (buffer-to-LP)
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (kill-buffer))
      (setq *overlays* nil))))



(provide 'literate-mode)

