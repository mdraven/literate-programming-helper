

Создание и открытие LP-проекта
=============================================

Настройка:
@d Customs @{
(defcustom literate-project-filename "lp-project"
  "Project filename for literate-programming-helper"
  :type '(string))@}
literate-project-filename -- имя для проектных файлов по-умолчанию

Переменные проекта:
@d Variables @{
(defvar literate-lp-directory nil)
(defvar literate-lp-filename nil)
(defvar literate-src-dir nil)@}
literate-lp-directory -- путь до директории проекта. Остальные пути должны быть относительно
  этой директории. Если nil, то файл проекта не открыт.
literate-lp-filename -- файл с LP-текстом. Если нужно несколько независимых LP-текстов, то
  делать разные проекты. Если они зависимы, то создать файл, который includ'ит эти LP-файлы.
  Относительный путь от literate-lp-directory.
literate-src-dir -- директория в которую будут сохраняться сгенерированные исходные коды;
  относительный путь от literate-lp-directory.

Функция записи в файл проекта. Выглядит громозко, но на самом деле почти ничего не делает:
@d Project @{
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
@}
флаг, который принимает функция, заставляет её создавать файл, если его нет. Этот флаг
  устанавливает функция создания проекта. Функции настройки проекта,
  наоборот, его не используют.
Если какие-то параметры равны nil, то они не сохраняются в файл.
FIXME: не проверяет, что proj-file -- директория

Функция настройки. Нужна для выбора LP-файла:
@d Project @{
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
@}
если файл НЕ существует, то он будет открыт в буфере.

Функция настройки. Нужна для выбора директории, куда будет сохраняться
сгенирированный код:
@d Project @{
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
@}
если директории нет, то создаёт её.

Функция создания нового проекта:
@d Project @{
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
@}
благодаря literate-save-lp-config, literate-set-lp-file, literate-set-src-dir делает кучу
  записей в один файл подряд. Не очень красиво, но функция вызывается не так часто.

Вспомогательная функция-фильтр. Принимает строку с названием синтаксиса и
возвращает её, если он корректный, иначе возвращает nil:
@d Project @{
(defun literate-filter-correct-syntax (syntax)
  (let ((syntax-types (mapcar #'car literate-syntax-functions)))
    (when (member syntax syntax-types)
      syntax)))
@}

Функция загрузки файла проекта:
@d Project @{
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
@}
