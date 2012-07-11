

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


@i parser.w
@i tangle.w
@i backconverter.w
@i project.w

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
