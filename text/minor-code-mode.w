

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
          @<Fill indicator - put indicators@>))))))
@}
FIXME: конфликтует с linum. А после того как они добавили lexical-binding хрен знает
  как исправить :(
FIXME: маркеры отваливаются после любого переключения в буфере -- margin восстанавливается в 0

В этом участке расставляются символы на боковой панели:
@d Fill indicator - put indicators
@{(catch 'break
  (while (< (point) end)
    (let ((ind (make-overlay (point) (point))))
      (push ind literate-indicators)
      (overlay-put ind 'before-string
                   (propertize " " 'display `((margin left-margin) "-"))))
    (if (/= (forward-line) 0)
        (throw 'break t)))@}
проверка forward-line на неравенство сделана, чтобы обойти проблемы с yasnippet: может там используется
narrowing или что-то другое, но, если применить сниппет, то "(while (< (point) end)" не может завершится.
Поэтому чтобы можно было использовать одновременно yasnippet и literate-code-mode у forward-line проверяется
дошли мы до конца буфера или нет.

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

