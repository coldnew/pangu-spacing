;;; test.el --- pangu-spacing test

;; Copyright (C) 2016 by Yen-Chin, Lee

;; Author: Yen-Chin, Lee <coldnew.tw@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'pangu-spacing)

(defun verify (str1 str2)
  (should (string= (pangu-spacing str1) str2)))

(ert-deftest test-All ()
  (verify "新八的構造成分有95%是眼鏡、3%是水、2%是垃圾" "新八的構造成分有 95% 是眼鏡、3% 是水、2% 是垃圾")
  (verify "所以,請問Jackey的鼻子有幾個?3.14個!" "所以, 請問 Jackey 的鼻子有幾個? 3.14 個!")
  (verify "JUST WE就是JUST WE，既不偉大也不卑微！" "JUST WE 就是 JUST WE，既不偉大也不卑微！")
  (verify "搭載MP3播放器，連續播放時數最長達到124小時的超強利刃……菊一文字RX-7!" "搭載 MP3 播放器，連續播放時數最長達到 124 小時的超強利刃…… 菊一文字 RX-7!")
  (verify "Mr.龍島主道：「Let's Party!各位高明博雅君子！」" "Mr. 龍島主道：「Let's Party! 各位高明博雅君子！」"))

;;; test.el ends here
