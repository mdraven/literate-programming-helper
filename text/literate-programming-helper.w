

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
@i interactive.w
@i minor-code-mode.w

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
