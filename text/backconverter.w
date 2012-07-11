
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

