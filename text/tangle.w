
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
(defun literate-expand-file (filename chunks &optional remove-unfound-chunks)
  (with-current-buffer (generate-new-buffer (concat literate-buffer-prefix filename))
    (literate-insert-parts-of-chunks chunks filename)
    (beginning-of-buffer)
    (literate-expand-targets chunks remove-unfound-chunks))
  (dolist (i literate-overlays)
    (overlay-put i 'modification-hooks (list #'literate-overlay-modification))))
@}
Она принимает имя файла, который нужно сгенерировать. Он должен быть определён, как
  имя одного из чанков. Вторым параметром является хеш-таблица
  полученная из literate-parse-file.
Она создаёт буфер соединив префикс и имя генерируемого файла, вставляет в него
  содержимое чанка файла с помощью функции insert-parts-of-chunks, которая определена
  ниже, переводит курсор на начало файла(insert-parts-of-chunks устанавливает курсор
  после вставленного текста), и с помощью функции expand-targets распаковывает цели.
В самом конце мы устанавливаем функцию обработчик изменения чанка внутри оверлея.

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
Если remove-unfound-chunks равен t, то цели удаляются и при возвращении в LP, не восстанавливаются.

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

Когда в каком-то оверлее происходят изменения, вызывается эта фукнция:
@d Tangle @{
(defun literate-overlay-modification (overlay flag beg end &rest args)
  (when flag
    (overlay-put overlay 'literate-modify t)))
@}

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
