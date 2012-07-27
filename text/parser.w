
Парсер
======

@d Variables @{
(defvar literate-lp-syntax nil)
(defvar literate-chunks-cache (make-hash-table :test #'equal))
(defvar literate-files-hash (make-hash-table :test #'equal))
(defvar literate-syntax-functions '(("nuweb" . (literate-nuweb-parser
                                                literate-nuweb-get-target
                                                literate-nuweb-generate-target))
                                    ("noweb" . (literate-noweb-parser
                                                literate-noweb-get-target
                                                literate-noweb-generate-target))))@}
literate-lp-syntax -- синтаксис текущего проекта. Если nil, то синтаксис не выбран
literate-chunks-cache -- в этой переменной кешируются чанки. Ключ -- абсолютный путь
  до файла, значение -- список структур чанков
literate-files-hash -- ключ -- абсолютное имя файла, значение -- метка времени модификации
literate-syntax-functions -- переменная в которой записаны функции интерфейса синтаксиса

Для начала определим функцию, которая будет принимать позицию в буфере
и возвращать чанк, который начинается с этой позиции:
@d Parser @{
(defun literate-nuweb-parser (beg-pos)
  (or (literate-closed-code-chunk-p (literate-nuweb-code-chunk-parser beg-pos))
      (literate-nuweb-include-chunk-parser beg-pos)
      (literate-nuweb-text-chunk-parser beg-pos)))
@}
вначале она пытается парсить как чанк с кодом, потом как чанк, который
подключает другой LP-файл, потом как чанк с текстом.

Далее нам пригодится макрос:
@d Helpers @{
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
@}
может такой есть в elisp, но я его не нашёл. Работает очень просто:
  expr -- выражение которое возвращает строку;
  clauses -- пары (<строка> <выражение>).
FIXME:Данная версия не принимает t

Данные чанка с кодом:
@d Parser @{
(defstruct literate-code-chunk
  subtype name body-beg body-end targets tags next-chunk)
@}
подтип чанка, имя чанка, начало кода, конец кода, цели внутри тела, ссылки(в виде
  строк), позиция начала следующего чанка

Функция для работы с хешем файлов:
@d Parser @{
(defun literate-check-hash-of-file (filename)
  (let ((modif-time (nth 6 (file-attributes filename))))
    (if (equal (gethash (expand-file-name filename) literate-files-hash)
               modif-time)
        nil
      (puthash (expand-file-name filename) modif-time literate-files-hash))))
@}
возвращает nil, если файл не изменялся в прошлой проверки, иначе записать новую
  метку в literate-files-hash и вернуть эту метку

Напишем парсер для кода:
@d Parser @{
(defun literate-nuweb-code-chunk-parser (beg-pos)
  (if (< (+ 2 beg-pos)
         (point-max))
      (let (subtype name body-beg body-end tags next-chunk)
        @<Code parser -- check tags@>
        (let (open tag close)
          @<Code parser -- find open, info-tag & close tags@>)
          @<Code parser -- fill body-end, tags & next-chunk@>
          @<Code parser -- fill name@>)
        (labels (@<Code parser -- get targets@>)
          (make-literate-code-chunk :subtype subtype :name name
                                    :body-beg body-beg :body-end body-end
                                    :targets (get-targets body-beg body-end)
                                    :tags tags
                                    :next-chunk next-chunk)))))
@}
Вначале сделаем проверку на наличие в буфере 2-х символов, без этих символов в
  буфере заведомо не помещается тег @[od] и следовательно нет кодового чанка.
Далее проверяем тег @[do], находим открывающий, закрывающий и тег отмечающий
  ссылки. Находим цели внутри тела блока. После этого запоняем поля.

Проверка тега. По нему можно определить тип чанка с кодом и, если тег не тот, то
это точно не чанк с кодом:
@d Code parser -- check tags
@{(let ((flag (buffer-substring-no-properties beg-pos (+ beg-pos 2))))
  (setq subtype
        (literate-case-string flag
          ("@o" 'file-chunk)
          ("@d" 'chunk))))@}
TODO: на данный момент @} в конце сам не становится(здесь я поставил его сам),
  вместо этого он переносит строку нижнюю -- и это плохо. Надо его научить.
TODO: быть может стоит не учитывать @{\n? Что-то это портит читаемость кода в LP-тексте. 

Найдем открывающий, закрывающий и тег отмечающий ссылки:
@d Code parser -- find open, info-tag & close tags
@{(save-excursion
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
              (not close)))))@}
Так как этот режим должен собрать сам себя, то тут используется костыль. Теги
  "@{", "@|", "@}" заключённые в двойные кавычки(причём открывающая и закрывающая кавычка
  лежат на одной строке) пропускаются парсером.

В зависимости от того, какие теги были найдены поля заполняются по разному:
@d Code parser -- fill body-end, tags & next-chunk
@{(or (and open tag close (< open tag) (< tag close)
         (setq body-end (- tag 2)
               next-chunk close
               tags (split-string
                     (buffer-substring-no-properties tag (- close 2)))))
    (and open close (< open close)
         (setq body-end (- close 2)
               next-chunk close)))@}
Теги занимают два символа, поэтому из позиции тега вычитают 2.
TODO: buffer-substring-no-properties -- не overhead ли здесь? Вроде используется
  сырой вывод в буфер.

Получаем имя чанка:
@d Code parser -- fill name
@{(if open
    (setq body-beg open
          name (literate-agressive-chomp
                (buffer-substring-no-properties (+ beg-pos 2)
                                                (- open 2)))))@}
Чтобы получить имя чанка достаточно найти открывающий тег. Тоесть этот парсер
  можно использовать для ещё недописанных чанков, и по наличию nil в некоторых
  полях результата определять: дописан тег или нет.

Возвращает список целей в буфере между позициями body-beg и body-end:
@d Code parser -- get targets
@{(get-targets (body-beg body-end)
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
                    targets)))@}


Теперь можно определить предикат, который будет определять это чанк с кодом или нет.
Причем чанк должен быть закрыт тегом:
@d Parser @{
(defun literate-closed-code-chunk-p (chunk)
  (and (literate-code-chunk-p chunk)
       (literate-code-chunk-subtype chunk)
       (literate-code-chunk-name chunk)
       (literate-code-chunk-body-beg chunk)
       (literate-code-chunk-body-end chunk)
       chunk))
@}
Если с чанком всё впорядке, то получим сам чанк. Если нет, то nil.

Имя чанка может содержать только одну строку, всё после знака переноса -- отбрасывается:
@d Helpers @{
(defun literate-agressive-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "^\n+\\|^\\s-+\\|\\s-+$\\|\n.*$" str)
    (setq str (replace-match "" t t str)))
  str)
@}

Структура для чанка с текстом:
@d Parser @{
(defstruct literate-text-chunk
  body-beg body-end)
@}

Парсер чанков с текстом:
@d Parser @{
(defun literate-nuweb-text-chunk-parser (beg-pos)
  (let (body-end)
    (setq body-end (or (let ((a (save-excursion
                                  (goto-char (+ beg-pos 1))
                                  (re-search-forward "^@[odi]" nil t))))
                         (and a (- a 2)))
                       (point-max)))
    (make-literate-text-chunk :body-beg beg-pos :body-end body-end)))
@}
Просто ищет следующий "^@[odi]", если не находит, то берёт конец буфера.
FIXME: у noweb тоже самое, но я переписал и стало выглядить лучше. Будет не лень,
  переписать и тут

Структура для чанков подключающих LP-файлы:
@d Parser @{
(defstruct literate-include-chunk
  path body-beg next-chunk)
@}
path -- путь до файла

Парсер чанков, которые подключают LP-файлы:
@d Parser @{
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
@}
Поверяет тег @i, получает имя файла. Возвращает имя файла, своё начало(похоже что оно
  не используется,TODO:проверить), начало следующего чанка.

Так как начало следующего чанка в разных типах чанков в разных местах, то
вместо того, чтобы переписать всё на структурах(а мне лень), определим функцию
которая вычленит это поле и вернёт содержимое:
@d Parser @{
(defun literate-next-chunk-begin (chunk)
  (cond
   ((literate-code-chunk-p chunk) (let ((subtype (literate-code-chunk-subtype chunk)))
                                    (when (or (eq subtype 'chunk)
                                              (eq subtype 'file-chunk))
                                      (literate-code-chunk-next-chunk chunk))))
   ((literate-text-chunk-p chunk) (literate-text-chunk-body-end chunk))
   ((literate-include-chunk-p chunk) (literate-include-chunk-next-chunk chunk))))
@}

Ищет и возвращает позицию и имя цели:
@d Parser @{
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
@}
пропускает цели в кавычках.

Генерирует имя цели:
@d Parser @{
(defun literate-nuweb-generate-target (name)
  (concat "@<" name "@>"))
@}

Интерфейс для всех функций парсера(literate-nuweb-parser),
поиска цели(вроде функции literate-nuweb-get-target) и
генерации стороки цели(literate-nuweb-generate-target)
у разных синтаксисов LP:
@d Parser @{
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
@}
для "nuweb" assoc вернёт ("nuweb" literate-nuweb-parser literate-nuweb-get-target)

Теперь можно написать функцию для парсинга файлов, которая использует
выше приведённые парсеры. Она будет возвращаеть хеш-таблицу с именами чанков
в качестве ключей(chunks-by-name); информации в этой таблице достаточно, чтобы из
чанков получить код программы; также будет возвращаться хеш вложености чанков
друг в друга(chunks-dependences), он нужен чтобы найти чанк с именем файла;
ещё список генерируемых файлов(те, что @o в nuweb), это список листьев
для chunks-dependences:
@d Parser @{
(defun literate-parse-file (filename)
  (let ((chunks-cache (make-hash-table :test #'equal)))
    (labels (@<Parse file -- hash helper@>)
      (helper filename)
      (setq literate-chunks-cache chunks-cache)))
  (let ((chunks-by-name (make-hash-table :test #'equal))
        (chunks-dependences (make-hash-table :test #'equal))
        (chunks-files (list)))
    (labels (@<Parse file -- concatenate to hash@>
             @<Parse file -- fill helper@>)
      (helper filename))
    (list chunks-by-name chunks-dependences chunks-files)))
@}
Так как внутри LP-файла подключаются другие, то стоит вызвать парсер рекурсивно.
  Именно для этого нужен helper(их два и они разные), который определяется в labels,
  он производит основную работу.
В первом блоке let происходит кеширование и извлечение из кеша чанков LP-файлов.
  Результат сохраняется в literate-chunks-cache
Во втором блоке let -- заполнение chunks-by-name chunks-dependences chunks-files
  с помощью содержимого literate-chunks-cache

Вспомогательная функция. Кеширует чанки по имени файла:
@d Parse file -- hash helper
@{(helper (filename)
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
                (helper (literate-include-chunk-path chunk)))))))@}

Парсеру придётся заполнять хеш-таблицу. Он делает это с помощью функции conc-to-hash:
@d Parse file -- concatenate to hash
@{(conc-to-hash (chunkname body-beg body-end targets filename)
              (let ((list (or (gethash chunkname chunks-by-name)
                              (list))))
                (push (list body-beg body-end filename) list)
                (puthash chunkname list chunks-by-name))
              (dolist (i targets)
                (let ((list (or (gethash i chunks-dependences)
                                (list))))
                  (add-to-list 'list chunkname)
                  (puthash i list chunks-dependences))))@}
Получив имя чанка, она находит его в таблице и добавляет информацию.
Последний чанк должен быть первым в списке, а первый последним.

Эта функция пробегает чанки и заполняет структуры, которые вернёт функция:
@d Parse file -- fill helper
@{(helper (filename)
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
              (helper (literate-include-chunk-path chunk)))))))@}
работа происходит с уже закешированными данными, чтение из файлов не происходит

Noweb
================================

В планах было сделать поддержку noweb. Но он оказался сильно не таким, как я расчитывал :(

1) Они перегенерируют noweb-код в промежуточный и пропускают его с помощью пайпов через скрипты,
  которые могут изменить этот промежуточный код. Чтобы была возможность повторить это, нужно
  писать не свой генератор, а тоже пропускать через скрипты, потом парсить не сам LP, а этот
  промежуточный язык. Какой скрипт для какого файла можно хранить в lp-project.
2) У них нет специального тега для файлов. С этим вообще плохо: похоже, что в основной реализации
  всегда генерируется с корневого тего <<*>>, но в некоторых не основных ищется что-то похожее на
  имя файла(с помощью regexp). Если <<*>>, то каждый LP-файл -- только один файл с кодом.
  Значит нужно усложнить считывание lp-project, ввести для каждого стиля свою вспомогательную
  функцию для создания/считывания файла с конфигами.
3) Нет тега для include. Это не проблема.

Что в итоге? Нужно сделать интерфейс для функций:
 - создания lp-project. Пункт "Syntax" заполняет не он.
 - считывания lp-project. "Syntax" может не учитывать.
 - функция которая возвращает относительный путь до LP-файла для текущей генерации.
   будет возвращать тот что открыт в текущем буфере, если он из нужной директории

Вместо <<*>> noweb будет возвращать имя файла. Вот только как узнать расширение этого файла с кодом?


TODO: А так как я ленивый и noweb не импользую, то пункты 1, 2 делать не буду и считывание конфига
  усложнять не стану. Пусть делают новый проект для каждого LP-файла.
  <<*>> обрабатываться не будет, хоть это и Ъ-вариант. Используем
  не Ъ-вариант с отгадыванием regexp'ом.

@d Parser @{
(defun literate-noweb-parser (beg-pos)
  (or (literate-noweb-code-chunk-parser beg-pos)
      (literate-noweb-text-chunk-parser beg-pos)))
@}

@d Parser @{
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
@}
FIXME: после написание кеширование LP-файлов структура literate-code-chunk изменилась

@d Parser @{
(defun literate-noweb-text-chunk-parser (beg-pos)
  (let (body-end)
    (setq body-end (or (save-excursion
                         (goto-char (+ beg-pos 1))
                         (when (re-search-forward "^<<.+?>>=\\|^@" nil t)
                           (match-beginning 0)))
                       (point-max)))
    (make-literate-text-chunk :body-beg beg-pos :body-end body-end)))
@}

@d Parser @{
(defun literate-noweb-get-target (pos)
  (let (target-pos target-name)
	(save-excursion
	  (goto-char pos)
	  (when (re-search-forward "<<\\(.+?\\)>>" nil t)
		(setq target-pos (match-beginning 0))
		(setq target-name (match-string 1))))
	(list target-pos target-name)))
@}
точно такая же как у nuweb -- но не учитывает цели взятые в кавычки; nuweb должен собирать
  этот файл, а у noweb можно забить

@d Parser @{
(defun literate-noweb-generate-target (name)
  (concat "<<" name ">>"))
@}
