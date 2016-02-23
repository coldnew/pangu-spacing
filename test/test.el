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

;; All
(ert-deftest test-All ()
  (verify "新八的構造成分有95%是眼鏡、3%是水、2%是垃圾" "新八的構造成分有 95% 是眼鏡、3% 是水、2% 是垃圾")
  (verify "所以,請問Jackey的鼻子有幾個?3.14個!" "所以, 請問 Jackey 的鼻子有幾個? 3.14 個!")
  (verify "JUST WE就是JUST WE，既不偉大也不卑微！" "JUST WE 就是 JUST WE，既不偉大也不卑微！")
  (verify "搭載MP3播放器，連續播放時數最長達到124小時的超強利刃……菊一文字RX-7!" "搭載 MP3 播放器，連續播放時數最長達到 124 小時的超強利刃…… 菊一文字 RX-7!")
  (verify "Mr.龍島主道：「Let's Party!各位高明博雅君子！」" "Mr. 龍島主道：「Let's Party! 各位高明博雅君子！」"))

;; Latin1 Supplement
(ert-deftest test-latin1 ()
  (verify "中文Ø漢字" "中文 Ø 漢字")
  (verify "中文 Ø 漢字" "中文 Ø 漢字"))

;; General Punctuation
(ert-deftest test-general-punctuation ()
  (verify "中文•漢字" "中文 • 漢字")
  (verify "中文 • 漢字" "中文 • 漢字"))

;; Number Forms
(ert-deftest test-number-forms ()
  (verify "中文Ⅶ漢字" "中文 Ⅶ 漢字")
  (verify "中文 Ⅶ 漢字" "中文 Ⅶ 漢字"))

;; CJK Radicals Supplement
(ert-deftest test-cjk-radicals-suppliement ()
  (verify "abc⻤123" "abc ⻤ 123")
  (verify "abc ⻤ 123" "abc ⻤ 123"))

;; Kangxi Radicals
(ert-deftest test-kangxi-radicals ()
  (verify "abc⾗123" "abc ⾗ 123")
  (verify "abc ⾗ 123" "abc ⾗ 123"))

;; Hiragana
(ert-deftest test-hiragana ()
  (vs "abcあ123" "abc あ 123")
  (vs "abc あ 123" "abc あ 123"))

;; Katakana
(ert-deftest test-katakana ()
  (vs "abcア123" "abc ア 123")
  (vs "abc ア 123" "abc ア 123"))

;; 處理 注音符號 (Bopomofo)
(ert-deftest test-bopomofo ()
  (vs "abcㄅ123" "abc ㄅ 123")
  (vs "abc ㄅ 123" "abc ㄅ 123"))

;; Enclosed cjk letters an months
(ert-deftest test-cjk-letters ()
  (vs "abc㈱123" "abc ㈱ 123")
  (vs "abc ㈱ 123" "abc ㈱ 123"))

;; Unified Ideographs
(ert-deftest test-unified-ideographs ()
  (vs "abc丁123" "abc 丁 123")
  (vs "abc 丁 123" "abc 丁 123")
  (vs "abc㐂123" "abc 㐂 123")
  (vs "abc 㐂 123" "abc 㐂 123"))

;; Compatibility Ideographs
(ert-deftest test-compatibility-ideographs ()
  (vs "abc車123" "abc 車 123")
  (vs "abc 車 123" "abc 車 123"))

;; 處理 ~ 符號 (tilde)
(ert-deftest test-tilde ()
  (vs "前面~後面" "前面~ 後面")
  (vs "前面 ~ 後面" "前面 ~ 後面")
  (vs "前面~ 後面" "前面~ 後面"))

;; 處理 ` 符號 (back quote)
(ert-deftest test-back-quote ()
  (vs "前面`後面" "前面 ` 後面")
  (vs "前面 ` 後面" "前面 ` 後面")
  (vs "前面` 後面" "前面 ` 後面"))

;; 處理 ! 符號 (exclamation mark)
(ert-deftest test-exclamation-mark ()
  (vs "前面!後面" "前面! 後面")
  (vs "前面 ! 後面" "前面 ! 後面")
  (vs "前面! 後面" "前面! 後面"))

;; 處理 @ 符號 (at)
(ert-deftest test-at ()
  ;; https://twitter.com/vinta
  (vs "前面@vinta後面" "前面 @vinta 後面")
  (vs "前面 @vinta 後面" "前面 @vinta 後面")
  ;; http://weibo.com/vintalines
  (vs "前面@陳上進 後面" "前面 @陳上進 後面")
  (vs "前面 @陳上進 後面" "前面 @陳上進 後面")
  (vs "前面 @陳上進tail" "前面 @陳上進 tail")
  ;; kmt-fu*k
  (vs "請@KMT吃大便" "請 @KMT 吃大便")
  (vs "請@馬英九 吃大便" "請 @馬英九 吃大便"))

;; 處理 # 符號 (hash)
(ert-deftest test-hash ()
  (vs "前面#H2G2後面" "前面 #H2G2 後面")
  (vs "前面#銀河便車指南 後面" "前面 #銀河便車指南 後面")
  (vs "前面#銀河便車指南tail" "前面 #銀河便車指南 tail")
  (vs "前面#銀河公車指南 #銀河拖吊車指南 後面" "前面 #銀河公車指南 #銀河拖吊車指南 後面")
  (vs "前面#H2G2#後面" "前面 #H2G2# 後面")
  (vs "前面#銀河閃電霹靂車指南#後面" "前面 #銀河閃電霹靂車指南# 後面"))

;;


;;; test.el ends here
