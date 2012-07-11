
Интерактивное управление проектом
=============================================

Функция которая возвращает список имен файлов(тег @o в nuweb), которые
содержат чанк chunk-name:
@d Interactive @{
(defun literate-get-target-files (chunks-dependences chunks-files chunk-name)
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
@}
chunks-dependences -- дерево вложености целей, а chunks-files -- листья этого дерева;
  и то, и другое берётся из функции parse-file.
Эту функцию можно использовать для определения файла, который будет генерироваться
  функцией expand-file.

Функция возвращает t, если 'b' между 'a' и 'c':
@d Helpers @{
(defun literate-num-between (a b c)
  (and (<= a b)
       (<= b c)))
@}
наверное она тоже есть в elisp, но вот как называется?

Функция для перехода из LP-текста в сгенерированный исходный код:
@d Interactive @{
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
@}
находит чанк, тело которого находится в позиции pos в LP-файле, и переходит на него.
pos -- это тело чанка без заголовка.
Функция использует переменную literate-overlays.

Функция возвращающая имя чанка на теле которого находится в позиции pos:
@d Interactive @{
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
@}
chunks -- первая хеш-таблица возвращаемая parse-file.
FIXME: избавится от chunk-name, если maphash возвращает nil, то key можно вернуть с
  помощью throw

Функция которая генерирует исходный код и переходит к чанку в этом исходном коде:
@d Interactive @{
(defun literate-generate-and-go (pos &optional arg)
  (interactive "d\np")
  (setq literate-overlays (list))
  (let ((parse (literate-parse-file (concat literate-lp-directory "/"
                                            literate-lp-filename))))
    (let ((chunks (car parse))
          (dependences (cadr parse))
          (files (caddr parse)))
      (let ((files (literate-get-target-files dependences files
                                              (literate-get-chunk-name chunks (point))))
            file)
        (setq file (if (eql arg 4)
                       (completing-read "File: " files nil t (car files))
                     (car files)))
        (when file
          (literate-expand-file file chunks)
          (when (and literate-lp-directory
                     literate-src-dir)
            (with-current-buffer (concat literate-buffer-prefix file)
              (write-file (concat literate-lp-directory "/"
                                  literate-src-dir "/"
                                  file))))))))
  (literate-go-to-body-position (point))
  (literate-code-mode t))
@}
после создания буфера с кодом прикрепляет к нему файл.
У функции есть аргумент arg, если он равен 4(C-u), то у нас запрашивается имя файла,
  который будет генерироваться.
FIXME: при каждом вызове парсит файлы, нужно сделать кеширование.
FIXME: "/" -- платформозависимо, неужели нет функции для генерации путей к файлам?
FIXME: можно вызвать из окна с кодом, тогда literate-overlays станет nil и мы не
  сможем вернуться(можно исправить горячими клавишами)

Функция возвращения из сгенерированного кода:
@d Interactive @{
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
@}
если есть прикреплённый файл, то сохраняет в него. В конце закрывает буфер.
revert не делает, но принципе можно будет сделать, если понадобится.
Использует тот факт, что в конце literate-overlays лежит оверлей самого верхнего уровня,
  тот самый который не удаляется после buffer-to-LP. Если он не прикреплён к буферу,
  то значит или буфер удалён, или удалён сам оверлей и обрабатывать уже нечего.

Обновить все файлы исходного кода, в которых произошли изменения:
@d Interactive @{
(defun literate-generate-all-files (&optional arg)
  (interactive "p")
  (setq literate-overlays (list))
  (let ((parse (literate-parse-file (concat literate-lp-directory "/"
                                            literate-lp-filename))))
    (let ((chunks (car parse))
          (files (caddr parse)))
      (when (and literate-lp-directory
                 literate-src-dir)
        (dolist (file files)
          (literate-expand-file file chunks (eql arg 4))
          (with-current-buffer (concat literate-buffer-prefix file)
            (let ((filename (concat literate-lp-directory "/"
                                    literate-src-dir "/"
                                    file)))
              (unless (literate-eq-buffer-and-file filename)
                (write-file filename)))
            (kill-buffer)))))))
@}
в emacs 23.3.1 literate-eq-buffer-and-file намного ускоряет генерацию
Параметр arg(C-u) -- если он равен 4, то при генерации исходного кода цели несуществующих
  чанков удаляются

Сравнивает текущий буфер и файл filename:
@d Helpers @{
(defun literate-eq-buffer-and-file (filename)
  (when (file-exists-p filename)
    (let ((buf-text (buffer-substring-no-properties (point-min) (point-max))))
      (with-temp-buffer
        (insert-file-literally filename)
        (string= buf-text (buffer-substring-no-properties (point-min) (point-max)))))))
@}
если файла нет, то возвращает nil. t -- только если буфер и файл равны.
