

@o literate-programming-helper.el @{
@<License@>

@<Customs@>
@<Variables@>

@<Requires@>

@<Helpers@>
@<Parser@>
@<Tangle@>
@<BackConverter@>
@<Project@>
@<Interactive@>
@<Minor mode for code@>

@<Provide@>
@}

Кроме временных буферов, будут ещё и именнованные-временные. Чтобы не было
проблем, к ним будет приписываться префикс:
@d Customs @{
(defcustom literate-buffer-prefix "literate-"
  "Prefix string for literate-programming-helper"
  :type '(string))@}


Используются функции из CL:
@d Requires
@{(require 'cl)@}

Парсер
======

@d Variables @{
(defvar literate-lp-syntax nil)
(defvar literate-syntax-functions '(("nuweb" . (literate-nuweb-parser
                                                literate-nuweb-get-target
                                                literate-nuweb-generate-target))
                                    ("noweb" . (literate-noweb-parser
                                                literate-noweb-get-target
                                                literate-noweb-generate-target))))@}
literate-lp-syntax -- синтаксис текущего проекта. Если nil, то синтаксис не выбран
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
  subtype name body-beg body-end tags next-chunk)
@}
подтип чанка, имя чанка, начало кода, конец кода, ссылки(в виде
  строк), позиция начала следующего чанка

Напишем парсер для кода:
@d Parser @{
(defun literate-nuweb-code-chunk-parser (beg-pos)
  (if (< (+ 2 beg-pos)
         (point-max))
      (let (subtype name body-beg body-end tags next-chunk)
        @<Code parser -- check tags@>
        (let (open tag close)
          @<Code parser -- find open, info-tag & close tags@>
          @<Code parser -- fill body-end, tags & next-chunk@>
          @<Code parser -- fill name@>)
        (make-literate-code-chunk :subtype subtype :name name
                                  :body-beg body-beg :body-end body-end
                                  :tags tags :next-chunk next-chunk))))
@}
Вначале сделаем проверку на наличие в буфере 2-х символов, без этих символов в
  буфере заведомо не помещается тег @[od] и следовательно нет кодового чанка.
Далее проверяем тег @[do], находим открывающий, закрывающий и тег отмечающий
  ссылки. После этого запоняем поля.

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
              (not close))))))@}
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
  name body-beg next-chunk)
@}
name -- имя файла

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
            (make-literate-include-chunk :name :body-beg beg-pos :next-chunk next-chunk)))))
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
  (let ((chunks-by-name (make-hash-table :test #'equal))
        (chunks-dependences (make-hash-table :test #'equal))
        (chunks-files (list)))
    (labels (@<Parse file -- get targets@>
             @<Parse file -- concatenate to hash@>
             @<Parse file -- helper@>)
      (helper filename))
    (list chunks-by-name chunks-dependences chunks-files)))
@}
Так как внутри LP-файла подключаются другие, то стоит вызвать парсер рекурсивно.
  Именно для этого нужен helper, который определяется в labels, он производит основную
  работу.

Возвращает список целей в буфере между позициями body-beg и body-end:
@d Parse file -- get targets
@{(get-targets (body-beg body-end)
             (let ((targets (list))
                   (pos body-beg))
               (while
                   (let ((target (literate-get-target pos)))
                     (setq pos (car target))
                     (when (and pos
                                (< pos body-end))
                       (incf pos)
                       (add-to-list 'targets (cadr target)))))
               targets))@}


Парсеру придётся заполнять хеш-таблицу. Он делает это с помощью функции conc-to-hash:
@d Parse file -- concatenate to hash
@{(conc-to-hash (chunkname body-beg body-end filename)
              (let ((list (or (gethash chunkname chunks-by-name)
                              (list))))
                (push (list body-beg body-end filename) list)
                (puthash chunkname list chunks-by-name))
              (let ((targets (get-targets body-beg body-end)))
                (dolist (i targets)
                  (let ((list (or (gethash i chunks-dependences)
                                  (list))))
                    (add-to-list 'list chunkname)
                    (puthash i list chunks-dependences)))))@}
Получив имя чанка, она находит его в таблице и добавляет информацию.
Последний чанк должен быть первым в списке, а первый последним.

Основная функция парсера parse-file:
@d Parse file -- helper
@{(helper (filename)
        (with-temp-buffer
          (insert-file-contents-literally filename)
          (let ((next-chunk-pos 1) chunk)
            (while (progn
                     (setq chunk (literate-parser next-chunk-pos)
                           next-chunk-pos (literate-next-chunk-begin chunk))
                     (cond
                      ((literate-code-chunk-p chunk)
                       (let ((subtype (literate-code-chunk-subtype chunk)))
                         (when (or (eq subtype 'chunk)
                                   (eq subtype 'file-chunk))
                           (conc-to-hash (literate-code-chunk-name chunk)
                                         (literate-code-chunk-body-beg chunk)
                                         (literate-code-chunk-body-end chunk)
                                         filename)
                           (when (eq subtype 'file-chunk)
                             (add-to-list 'chunks-files
                                          (literate-code-chunk-name chunk))))))
                      ((literate-text-chunk-p chunk) ())
                      ((literate-include-chunk-p chunk)
                       (helper (literate-include-chunk-name chunk))))
                     (< next-chunk-pos (point-max)))))))@}
Пишет содержимое файла filename во временный буфер и, пробегая по буферу
  чанк за чанком, заполняет хеш-таблицу.

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

Создание буфера с исходным кодом
================================


literate-parse-file возвращает таблицу с информацией по чанкам. Но нам нужен сгенерированый
исходный код, для того чтобы была возможность его изменить и вернуть в LP-текст.

Для начала напишем функцию, которая определяет пустая это строка или нет:
@d Helpers @{
(defun literate-empty-line-p (&optional end)
  (let ((cur (point))
        ind)
    (back-to-indentation)
    (setq ind (point))
    (goto-char cur)
    (if end
        (>= ind (min (point-at-eol) end))
      (eql ind (point-at-eol)))))
@}
end -- необязательный параметр; иногда конец строки не является границей, тк
  конец, например буфера, может встретится раньше.

Основной функцией этой главы является:
@d Tangle @{
(defun literate-expand-file (filename chunks)
  (with-current-buffer (generate-new-buffer (concat literate-buffer-prefix filename))
    (literate-insert-parts-of-chunks chunks filename)
    (beginning-of-buffer)
    (literate-expand-targets chunks)))
@}
Она принимает имя файла, который нужно сгенерировать. Он должен быть определён, как
  имя одного из чанков. Вторым параметром является хеш-таблица
  полученная из literate-parse-file.
Она создаёт буфер соединив префикс и имя генерируемого файла, вставляет в него
  сожержимое чанка файла с помощью функции insert-parts-of-chunks, которая определена
  ниже, переводит курсор на начало файла(insert-parts-of-chunks устанавливает курсор
  после вставленного текста), и с помощью функции expand-targets распаковывает цели.


Функция для распаковки целей. Находит цели, вставляет в них содержимое чанка,
выравнивает код. Если установить параметр remove-unfound-chunks в t, то
ненайденые блоки будут удаляться, иначе они остануться в коде(по-умолчанию):
@d Tangle @{
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
              @<Expand targets -- insert & align text@>)
          (push target-name unfound-chunks))
        (forward-char)))
    unfound-chunks))
@}
Функция возвращает список ненайденных блоков.

Если блок найден, то нужно не только вставить текст из этого блока, но и
правильно выровнить его.
Текст в чанках выравнивается при вставке следующим образом:
  - символы в первой строке всегда начинаются с того же места, где начинается
    первый символ цели. Если первая строка содержала пробелы перед первым символом, то
    они отбрасываются;
  - остальные символы выравниваются относительно первого символа первой строки;
  - если остальные строки не помещаются(например тег цели был установлен в начале строки),
    то они вставляются с такими отступами, которые у них были. Ни один пробел не удаляется.

@d Expand targets -- insert & align text
@{(let ((spaces-first-line (literate-spaces-before-first-string chunks target-name))
      tabs tabs-str)
  (setq tabs (- (- target-pos target-beg-line)
                spaces-first-line))
  (if (< tabs 0)
      (setq tabs 0))
  (setq tabs-str (make-string tabs ?\s))

  (goto-char target-pos)
  (literate-insert-parts-of-chunks chunks target-name)

  @<Expand targets -- insert spaces@>

  (goto-char target-pos)
  (delete-char spaces-first-line))@}
Функция spaces-before-first-string возвращает число пробелов перед первой строкой;
  в spaces хранится число пробелов, которое будет вставлено перед каждой строкой,
  кроме первой.
Так как literate-insert-parts-of-chunks ставит курсор после вставленого текста,
  то вставляем пробелы снизу-вверх.
В конце удаляем пробелы перед первой строкой, тогда она будет выравнена по начальной
  позиции тега.
Так как literate-insert-parts-of-chunks помещает новые оверлеи на вершину списка
  в переменной literate-overlays, то нижележащие слои будут идти в списке после вышележащих.


@d Expand targets -- insert spaces
@{(let ((end-of-chunks-block (point)))
  (while (> (point-at-bol) target-beg-line)
    (move-beginning-of-line nil)
    (unless (literate-empty-line-p end-of-chunks-block)
      (insert tabs-str))
    (forward-line -1)))@}
Учитываются пустые строки, перед ними пробелы не ставятся.


Функция которая вставляет все кусочки кода из чанков с именем chunkname. Использует
то свойство, что самый нижний кусок кода будет первым в списке полученном из хеш-таблицы,
а первый -- последним.
Над вставлеными чанками создаются оверлеи, к которым добавляются свойство literate-chunk.
Это свойство содержит список из хеш-таблицы, но не всех чанков с именем chunkname, а только
этого; в добавок свойство содержит имя чанка.
@d Tangle @{
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
@}
Эта функция всегда устанавливает курсор после вставленного текста.
Созданные оверлеи помещаются в начало списка literate-overlays.
TODO: хук для преобразования из табов в пробелы

В этой переменной будет хранится список оверлеев:
@d Variables @{
(defvar literate-overlays nil)@}
на всякий случай напомню, что у оверлеев есть свойства и там хранится
  много информации.

Функция, которая возвращает число пробелов перед первой строкой чанка:
@d Tangle @{
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
@}
TODO: хук для преобразования из табов в пробелы



Замена чанков в LP-файле на отредактированные
=============================================


После того, как literate-expand-file создал нам буфер с кодом и мы внесли в код некоторые
изменения, возникает желание вернуть эти изменения в LP-файл.
Основной функцией этого раздела будет buffer-to-LP, которая руководствуясь переменной
literate-overlays и оверлеями, на которые указывает literate-overlays, будет заменять чанки
в LP-файле на их изменившиеся версии из буфера.


При замене в LP-файлах используются следующие правила выравнивания:
  - блок из чанков с одним именем выравнивается относительно первой строки
    первого чанка. Первые строки остальных чанков блока не учитываются;
  - цель, которая записывается вместо блока чанков, устанавливается
    там где находился первый символ первой строки первого чанка;
      Например:   @<old place@>    code-after-indentation
        Если после выравнивания перед кодом образовались пробелы, то
          цель будет сдвинута.
  - у всех строк, кроме первой, отбрасывается минимальное количество пробелов,
    которое есть в идентации;
  - к первой строке добавляют пробелы, если остальные строки нельзя разместить;
  - у первой строки удаляют N пробелов, если цель сдвинули на N пробелов вперёд.


Для начала рассмотрим вспомогательную функцию:
@d BackConverter @{
(defun literate-get-filenames-list-from-overlays ()
  (let (list)
    (dolist (i literate-overlays)
      (let ((chunk (car (overlay-get i 'literate-chunk))))
        (if chunk
            (push (caddr chunk) list))))
    (delete-dups list)))
@}
Она всего лишь вытягивает имена LP-файлов из оверлеев. Конечно можно было сохранить
  эти данные в какой-нибудь глобальной переменной, но раз данные уже были помещены
  в свойства оверлеев, то зачем повторяться?

Ещё одна полезная функция:
@d BackConverter @{
(defun literate-list-subtract (a b)
  (remove-if (lambda (x) (member x b)) a))
@}
Она вычитает из одного списка другой. Возможно такая функция уже есть в emacs, но
  я её не нашёл :(

Вот и функция buffer-to-LP. Хотя кажется, что она не принимает параметры, но это не так.
Внутри активно используется содержимое переменной literate-overlays.
@d BackConverter @{
(defun literate-buffer-to-LP ()
  (let ((literate-overlays literate-overlays)
        (files (literate-get-filenames-list-from-overlays)))
    ;; Create buffers
    @<Buffer to LP -- Create buffers@>
    ;; Create markers
    @<Buffer to LP -- Create markers@>
    ;; Update chunks in LP
    @<Buffer to LP -- Update chunks in LP@>
    ;; Save & kill buffers
    (save-current-buffer
      (dolist (i files)
        (set-buffer (concat literate-buffer-prefix i))
        (write-region (point-min) (point-max) i)
        (kill-buffer)))))
@}
write-region используется потому, как остальные функции слишком высокоуровненые.
  Часто возникает требование обновления буфера(revert-buffer).

На данном этапе нам потребуются буферы с LP-текстом для того, чтобы можно было
заменить код в чанках на новый. Оверлеи(которые играют роль отображений чанков)
содержат имена LP-файлов, поэтому мы извлекаем эти имена и создаём буферы:
@d Buffer to LP -- Create buffers
@{(dolist (i files)
  (with-current-buffer (generate-new-buffer (concat literate-buffer-prefix i))
    (insert-file-contents-literally i)))@}

Создаём в LP-файле маркеры. Маркеры отмечают начало и конец блока с кодом. Когда
мы расставим маркеры, нам будет уже не страшны изменения размеров блоков -- маркеры
будут менять своё положение вместе с изменениями текста:
@d Buffer to LP -- Create markers
@{(dolist (i literate-overlays)
  (let* ((chunk (car (overlay-get i 'literate-chunk)))
         (bufname (concat literate-buffer-prefix (caddr chunk)))
         (beg-marker (make-marker))
         (end-marker (make-marker)))
    (with-current-buffer bufname
      ;; (set-marker-insertion-type beg-marker t)
      (set-marker-insertion-type end-marker t)
      (set-marker beg-marker (car chunk))
      (set-marker end-marker (cadr chunk))
      (overlay-put i 'literate-marker (list beg-marker end-marker)))))@}
К оверлеям добавляется новое свойство literate-marker. В нём будут содержаться
  ссылки к начальному и конечному маркеру.

Начинаем перенос содержимого оверлеев в чанки. По первому взятому оверлею(который
представляет один чанк) можно получить всех его соседей имеющих одно имя,
этим занимается функция get-overlays-near-pos-with-chunkname. Далее с
помощью функции get-overlays-near-pos-with-chunkname получается информация о
том где удалить пробелы, а где добавить. Из literate-overlays вычитаются найденные
оверлеи-соседи. Вся эта фигня с поиском соседей сделана, чтобы лучше выровнять
код в чанках.
@d Buffer to LP -- Update chunks in LP
@{(while literate-overlays
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
          @<Buffer to LP -- insert code in LP & remove from tangled file@>)
        (delete-region beg-overlays end-overlays)
        (goto-char beg-overlays)
        (insert (concat (make-string (caddr rem-spaces) ?\s)
                        (literate-generate-target chunkname)))))))@}
Соседи-оверлеи обрабатываются в цикле. После чего их блоки удаляются и заменяются целью с
именем, которое объединяла эти оверлеи.

Этот фрагмент кода отвечает за вставку кода из оверлеев в чанка. Соответственно она
переключает буфер: с буфера оверлея на буфер чанка:
@d Buffer to LP -- insert code in LP & remove from tangled file
@{(let ((markers (overlay-get i 'literate-marker)))
  (let* ((beg-body (overlay-start i))
         (end-body (overlay-end i))
         (body (buffer-substring-no-properties beg-body end-body))
         (something-written-before-body))
    @<Buffer to LP -- set something-written-before-body@>

    (with-current-buffer (marker-buffer (car markers))
      (delete-region (marker-position (car markers))
                     (marker-position (cadr markers)))
      (save-excursion
        @<Buffer to LP -- insert body to LP@>

        @<Buffer to LP -- rm or ins spaces before first line of the block of chunks@>
        @<Buffer to LP -- remove spaces before chunk's first line@>
        @<Buffer to LP -- remove spaces before other chunk's lines@>))))@}
Напомню, что маркеры расположены в буфере с чанками, а оверлеи в буфере со сгренерированным
  кодом.
TODO: надо сделать хук для преобразования из пробелов в табы. Как раз после удаления/добавления
  всех пробелов.

Проверить есть ли перед первой строкой оверлея, что-то кроме пробелов.
Если есть, то установить флаг something-written-before-body:
@d Buffer to LP -- set something-written-before-body
@{(goto-char beg-body)
(back-to-indentation)
(if (< (point) beg-body)
    (setq something-written-before-body t))@}

Вставить код из оверлея в чанк:
@d Buffer to LP -- insert body to LP
@{(goto-char (marker-position (car markers)))
(insert body)
(goto-char (marker-position (car markers)))@}

Удалить или добавить пробелы перед первой строкой блоков из чанков с одним именем:
@d Buffer to LP -- rm or ins spaces before first line of the block of chunks
@{(when (= beg-body beg-overlays)
  (let ((delete-char (car rem-spaces)))
    (if (> delete-char 0)
        (delete-char delete-char)
      (insert (make-string (abs delete-char) ?\s))))
  (forward-line))@}

Так как весь блок чанков с одним именем центрируется по первой строке первого чанка, то,
казалось бы, первые строки остальных чанков можно не учитывать и обрабатывать как остальные.
Но бывают случаи когда перед первой строкой одного чанка есть последняя строка предыдущего:
в таком случае пробелы отбрасываться не будут, так как при вызове
literate-get-spaces-before-overlay они не учитывались:
@d Buffer to LP -- remove spaces before chunk's first line 
@{(when (and (/= beg-body beg-overlays)
           (< (point) (marker-position (cadr markers))))
  (unless (and something-written-before-body
               (literate-empty-line-p))
    (delete-char (cadr rem-spaces)))
  (forward-line))@}
Пустые строки учитываются, перед ними пробелы не ставятся.

Удаляем пробелы перед остальными строками в чанках:
@d Buffer to LP -- remove spaces before other chunk's lines
@{(while (< (point) (marker-position (cadr markers)))
  (unless (literate-empty-line-p)
    (delete-char (cadr rem-spaces)))
  (forward-line))@}
Пустые строки учитываются, перед ними пробелы не ставятся.

Функция которая возвращает блок оверлеев имеющих одно имя, и которые,
предположительно, были созданы при вставке чанков в одну цель:
@d BackConverter @{
(defun literate-get-overlays-near-pos-with-chunkname (pos overlays-list chunkname)
  (let (overlays beg-overlays end-overlays)
    @<Buffer to LP -- overlays under argument pos@>

    @<Buffer to LP -- get overlays before pos@>

    @<Buffer to LP -- get overlays after pos@>
    (list overlays beg-overlays end-overlays)))
@}
FIXME: примет две подряд идущие цели с одним именем, как одну

Оверлеи с именем chunkname включающие точку pos помещаются в список
overlays:
@d Buffer to LP -- overlays under argument pos
@{(let ((overlays-list overlays-list))
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
      (setq overlays-list (cdr overlays-list)))))@}
Заполняются переменные beg-overlays и end-overlays.
Про функцию overlays-at лучше не вспоминать, с её помощью не удаётся получить
  схлопнувшиеся оверлеи(у которых start = end).

С помощью переменной beg-overlays берём оверлеи с именем chunkname которые
лежат до лежащих в overlays, но имеющих с ними общую точку beg-overlays:
@d Buffer to LP -- get overlays before pos
@{(when (and beg-overlays
           (> pos beg-overlays))
  (let ((ret (literate-get-overlays-near-pos-with-chunkname
              beg-overlays
              (literate-list-subtract overlays-list
                                      overlays)
              chunkname)))
    (setq overlays (append overlays (car ret)))
    (when (and (cadr ret)
               (> beg-overlays (cadr ret)))
      (setq beg-overlays (cadr ret)))))@}
Если не выкидывать overlays из overlays-list, то рекурсивный вызов пойдёт и вниз, а
  потом опять вверх, и так до переполнения стека.

Аналогочно "get overlays before pos", но уже после overlays:
@d Buffer to LP -- get overlays after pos
@{(when (and end-overlays
           (< pos end-overlays))
  (let ((ret (literate-get-overlays-near-pos-with-chunkname
              end-overlays
              (literate-list-subtract overlays-list
                                      overlays)
              chunkname)))
    (setq overlays (append overlays (car ret)))
    (when (and (caddr ret)
               (< end-overlays (caddr ret)))
      (setq end-overlays (caddr ret)))))@}

Функция которая по переданным позициям в буфере вычисляет сколько пробелов
нужно удалить/добавить к первой строке, удалить у остальных, добавить к цели:
@d BackConverter @{
(defun literate-get-spaces-before-overlay (beg-pos end-pos)
  (let (rem-first rem-other shift-target)
    (save-excursion
      (let (beg-code target-num-spaces other-num-spaces first-num-spaces)
        (goto-char beg-pos)
        (setq target-num-spaces (- beg-pos (point-at-bol)))

        @<Buffer to LP -- skip whitespaces and set beg-pos@>

        @<Buffer to LP -- init first-num-spaces & other-num-spaces@>

        @<Buffer to LP -- find min other-num-spaces@>

        (if (and first-num-spaces other-num-spaces)
            @<Buffer to LP -- if first-num-spaces, other-num-spaces are defined@>
          (setq shift-target 0
                rem-other 0
                rem-first 0))))
    (list rem-first rem-other shift-target)))
@}

После выравнивания(indent) перед кодом в первой строке
могли образоваться пробелы; пропустим их:
@d Buffer to LP -- skip whitespaces and set beg-pos
@{(while (equal (char-after) ?\s)
  (forward-char))
(setq beg-code (point))@}

Если мы не вышли за пределы блока который обрабатываем, то вычислим количество
символов(пробелов) перед первой строкой и проинициализируем их смещение для остальных
строк:
@d Buffer to LP -- init first-num-spaces & other-num-spaces
@{(when (<= (point) end-pos)
  (setq first-num-spaces (- beg-code (point-at-bol)))
  (setq other-num-spaces first-num-spaces))
(forward-line)@}
Не стоит забывать, что перед первой строкой могут быть не только пробелы.


Найдём минимальное смещение для строк лежащих ниже первой:
@d Buffer to LP -- find min other-num-spaces
@{(while (< (point) end-pos)
  (let ((beg-line (point)))
    (back-to-indentation)
    (when (< (point) (min (point-at-eol)
                          end-pos))
      (let ((diff (- (point) beg-line)))
        (when (< diff other-num-spaces)
          (setq other-num-spaces diff)))))
  (forward-line))@}
Здесь также учитываются пустые строки.

Если first-num-spaces и other-num-spaces определены, то посчитаем смещения:
сколько добавить пробелов перед целью, сколько удалить пробелов у всех строк, кроме
первой и сколько добавить/удалить пробелов перед первой строкой:
@d Buffer to LP -- if first-num-spaces, other-num-spaces are defined
@{(progn
  (setq shift-target (- first-num-spaces target-num-spaces)
        rem-other (min first-num-spaces other-num-spaces)
        rem-first (if (<= first-num-spaces other-num-spaces)
                      shift-target
                    (- other-num-spaces first-num-spaces))))@}
Так как first-num-spaces указывает на первый символ первой строки, то
  смещение для цели можно определить простым вычитанием кол-ва символов перед
  целью из кол-ва символов перед первым символом.
Если пробелов перед первой строкой меньше чем перед остальными, то значит, что
  цель будет смещена на shift-target и у первой строки удаляют shift-target
  пробелов. Если больше:
   |     first line
   |  other line
  то после смещения цели к первой строке нужно добавить разницу. Она отмечается
  отрицательным числом. 


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
    (let ((proj-file (concat literate-lp-directory "/" literate-project-filename)))
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
  (let ((proj-file (concat dir-path "/" literate-project-filename)))
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
  (let ((proj-file (concat dir-path "/" literate-project-filename)))
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
(defun literate-generate-all-files ()
  (interactive)
  (setq literate-overlays (list))
  (let ((parse (literate-parse-file (concat literate-lp-directory "/"
                                            literate-lp-filename))))
    (let ((chunks (car parse))
          (files (caddr parse)))
      (when (and literate-lp-directory
                 literate-src-dir)
        (dolist (file files)
          (literate-expand-file file chunks)
          (with-current-buffer (concat literate-buffer-prefix file)
            (let ((filename (concat literate-lp-directory "/"
                                    literate-src-dir "/"
                                    file)))
              (unless (literate-eq-buffer-and-file filename)
                (write-file filename)))
            (kill-buffer)))))))
@}
в emacs 23.3.1 literate-eq-buffer-and-file намного ускоряет генерацию

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

Минорный режим для окна редактирования кода
================================

@d Minor mode for code @{
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
@}

Создать переменную для оверлеев-индикаторов и указатель на оверлей,
который отображается индикатором в данный момент:
@d Variables @{
(defvar literate-indicators (list))
(defvar literate-ind-current nil)@}

Функция удаления списка оверлеев-индикаторов:
@d Minor mode for code @{
(defun literate-remove-indicators ()
  (mapc #'delete-overlay literate-indicators)
  (setq literate-indicators (list)))
@}

Функция заполняющая индикатор для заданного оверлея:
@d Minor mode for code @{
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
          (while (< (point) end)
            (let ((ind (make-overlay (point) (point))))
              (push ind literate-indicators)
              (overlay-put ind 'before-string
                           (propertize " " 'display `((margin left-margin) "-"))))
            (forward-line)))))))
@}
FIXME: конфликтует с linum. А после того как они добавили lexical-binding хрен знает
  как исправить :(
FIXME: маркеры отваливаются после любого переключения в буфере -- margin восстанавливается в 0

Функция для создания отступа для индикатора:
@d Minor mode for code @{
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
@}


Функция возвращающая наименьший оверлей из literate-overlays в
заданной точке:
@d Minor mode for code @{
(defun literate-get-overlay-for-indication (pos)
  (dolist (i literate-overlays)
    (when (literate-num-between (overlay-start i)
                                pos
                                (overlay-end i))
      (return i))))
@}

Функция которая заполняет маркер для текущего оверлея, и
функция которая перезаполняет маркер для оверлея после его изменения:
@d Minor mode for code @{
(defun literate-code-ind-current-overlay ()
  (literate-fill-indicator
   (literate-get-overlay-for-indication (point))))

(defun literate-code-ind-after-change (&optional beg end len)
  (setq literate-ind-current nil)
  (literate-code-ind-current-overlay))
@}

@d Provide @{
(provide 'literate-programming-helper)@}

@d License
@{;; literate-programming-helper - support literate programming for emacs
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
@}