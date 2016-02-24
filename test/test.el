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
  (verify "abcあ123" "abc あ 123")
  (verify "abc あ 123" "abc あ 123"))

;; Katakana
(ert-deftest test-katakana ()
  (verify "abcア123" "abc ア 123")
  (verify "abc ア 123" "abc ア 123"))

;; 處理 注音符號 (Bopomofo)
(ert-deftest test-bopomofo ()
  (verify "abcㄅ123" "abc ㄅ 123")
  (verify "abc ㄅ 123" "abc ㄅ 123"))

;; Enclosed cjk letters an months
(ert-deftest test-cjk-letters ()
  (verify "abc㈱123" "abc ㈱ 123")
  (verify "abc ㈱ 123" "abc ㈱ 123"))

;; Unified Ideographs
(ert-deftest test-unified-ideographs ()
  (verify "abc丁123" "abc 丁 123")
  (verify "abc 丁 123" "abc 丁 123")
  (verify "abc㐂123" "abc 㐂 123")
  (verify "abc 㐂 123" "abc 㐂 123"))

;; Compatibility Ideographs
(ert-deftest test-compatibility-ideographs ()
  (verify "abc車123" "abc 車 123")
  (verify "abc 車 123" "abc 車 123"))

;; 處理 ~ 符號 (tilde)
(ert-deftest test-tilde ()
  (verify "前面~後面" "前面~ 後面")
  (verify "前面 ~ 後面" "前面 ~ 後面")
  (verify "前面~ 後面" "前面~ 後面"))

;; 處理 ` 符號 (back quote)
(ert-deftest test-back-quote ()
  (verify "前面`後面" "前面 ` 後面")
  (verify "前面 ` 後面" "前面 ` 後面")
  (verify "前面` 後面" "前面 ` 後面"))

;; 處理 ! 符號 (exclamation mark)
(ert-deftest test-exclamation-mark ()
  (verify "前面!後面" "前面! 後面")
  (verify "前面 ! 後面" "前面 ! 後面")
  (verify "前面! 後面" "前面! 後面"))

;; 處理 @ 符號 (at)
(ert-deftest test-at ()
  ;; https://twitter.com/vinta
  (verify "前面@vinta後面" "前面 @vinta 後面")
  (verify "前面 @vinta 後面" "前面 @vinta 後面")
  ;; http://weibo.com/vintalines
  (verify "前面@陳上進 後面" "前面 @陳上進 後面")
  (verify "前面 @陳上進 後面" "前面 @陳上進 後面")
  (verify "前面 @陳上進tail" "前面 @陳上進 tail")
  ;; kmt-fu*k
  (verify "請@KMT吃大便" "請 @KMT 吃大便")
  (verify "請@馬英九 吃大便" "請 @馬英九 吃大便"))

;; 處理 # 符號 (hash)
(ert-deftest test-hash ()
  (verify "前面#H2G2後面" "前面 #H2G2 後面")
  (verify "前面#銀河便車指南 後面" "前面 #銀河便車指南 後面")
  (verify "前面#銀河便車指南tail" "前面 #銀河便車指南 tail")
  (verify "前面#銀河公車指南 #銀河拖吊車指南 後面" "前面 #銀河公車指南 #銀河拖吊車指南 後面")
  (verify "前面#H2G2#後面" "前面 #H2G2# 後面")
  (verify "前面#銀河閃電霹靂車指南#後面" "前面 #銀河閃電霹靂車指南# 後面"))

;; 處理 $ 符號 (dollar)
(ert-deftest test-dollar ()
  (verify "前面$後面" "前面 $ 後面")
  (verify "前面 $ 後面" "前面 $ 後面")
  (verify "前面$100後面" "前面 $100 後面"))

;; 處理 % 符號 (percent)
(ert-deftest test-percent ()
  (verify "前面%後面" "前面 % 後面")
  (verify "前面 % 後面" "前面 % 後面")
  (verify "前面100%後面" "前面 100% 後面"))

;; 處理 ^ 符號 (caret)
(ert-deftest test-caret ()
  (verify "前面^後面" "前面 ^ 後面")
  (verify "前面 ^ 後面" "前面 ^ 後面")
  (verify "2^10" "2^10"))

;; 處理 & 符號 (ampersand)
(ert-deftest test-ampersand ()
  (verify "前面&後面" "前面 & 後面")
  (verify "前面 & 後面" "前面 & 後面")
  (verify "Vinta&Mollie" "Vinta&Mollie")
  (verify "Vinta&陳上進" "Vinta & 陳上進")
  (verify "陳上進&Vinta" "陳上進 & Vinta")
  (verify "得到一個A&B的結果" "得到一個 A&B 的結果"))

;; 處理 * 符號 (asterisk)
(ert-deftest test-asterisk ()
  (verify "前面*後面" "前面 * 後面")
  (verify "前面 * 後面" "前面 * 後面")
  (verify "Vinta*Mollie" "Vinta*Mollie")
  (verify "Vinta*陳上進" "Vinta * 陳上進")
  (verify "陳上進*Vinta" "陳上進 * Vinta")
  (verify "得到一個A*B的結果" "得到一個 A*B 的結果"))

;; 處理 ( ) 符號 (parentheses)
(ert-deftest test-parentheses ()
  (verify "前面(中文123漢字)後面" "前面 (中文 123 漢字) 後面")
  (verify "前面(中文123)後面" "前面 (中文 123) 後面")
  (verify "前面(123漢字)後面" "前面 (123 漢字) 後面")
  (verify "前面(中文123漢字) tail" "前面 (中文 123 漢字) tail")
  (verify "head (中文123漢字)後面" "head (中文 123 漢字) 後面")
  (verify "head (中文123漢字) tail" "head (中文 123 漢字) tail"))


;; 處理 - 符號 (minus)
(ert-deftest test-minus ()
  (verify "前面-後面" "前面 - 後面")
  (verify "前面 - 後面" "前面 - 後面")
  (verify "Vinta-Mollie" "Vinta-Mollie")
  (verify "Vinta-陳上進" "Vinta - 陳上進")
  (verify "陳上進-Vinta" "陳上進 - Vinta")
  (verify "得到一個A-B的結果" "得到一個 A-B 的結果"))

;; 處理 _ 符號 (underscore)
(ert-deftest test-underscore ()
  (verify "前面_後面" "前面_後面")
  (verify "前面 _ 後面" "前面 _ 後面"))

;; 處理 + 符號 (plus)
(ert-deftest test-plus ()
  (verify "前面+後面" "前面 + 後面")
  (verify "前面 + 後面" "前面 + 後面")
  (verify "Vinta+Mollie" "Vinta+Mollie")
  (verify "Vinta+陳上進" "Vinta + 陳上進")
  (verify "陳上進+Vinta" "陳上進 + Vinta")
  (verify "得到一個A+B的結果" "得到一個 A+B 的結果")
  (verify "得到一個C++的結果" "得到一個 C++ 的結果"))

;; 處理 = 符號 (equal)
(ert-deftest test-equal ()
  (verify "前面=後面" "前面 = 後面")
  (verify "前面 = 後面" "前面 = 後面")
  (verify "Vinta=Mollie" "Vinta=Mollie")
  (verify "Vinta=陳上進" "Vinta = 陳上進")
  (verify "陳上進=Vinta" "陳上進 = Vinta")
  (verify "得到一個A=B的結果" "得到一個 A=B 的結果"))

;; 處理 { } 符號 (braces)
(ert-deftest test-braces ()
  (verify "前面{中文123漢字}後面" "前面 {中文 123 漢字} 後面")
  (verify "前面{中文123}後面" "前面 {中文 123} 後面")
  (verify "前面{123漢字}後面" "前面 {123 漢字} 後面")
  (verify "前面{中文123漢字} tail" "前面 {中文 123 漢字} tail")
  (verify "head {中文123漢字}後面" "head {中文 123 漢字} 後面")
  (verify "head {中文123漢字} tail" "head {中文 123 漢字} tail"))

;; 處理 [ ] 符號 (brackets)
(ert-deftest test-brackets ()
  (verify "前面[中文123漢字]後面" "前面 [中文 123 漢字] 後面")
  (verify "前面[中文123]後面" "前面 [中文 123] 後面")
  (verify "前面[123漢字]後面" "前面 [123 漢字] 後面")
  (verify "前面[中文123漢字] tail" "前面 [中文 123 漢字] tail")
  (verify "head [中文123漢字]後面" "head [中文 123 漢字] 後面")
  (verify "head [中文123漢字] tail" "head [中文 123 漢字] tail"))

;; 處理 | 符號 (pipe)
(ert-deftest test-pipe ()
  (verify "前面|後面" "前面 | 後面")
  (verify "前面 | 後面" "前面 | 後面")
  (verify "Vinta|Mollie" "Vinta|Mollie")
  (verify "Vinta|陳上進" "Vinta | 陳上進")
  (verify "陳上進|Vinta" "陳上進 | Vinta")
  (verify "得到一個A|B的結果" "得到一個 A|B 的結果"))

;; 處理 \ 符號 (backslash)
(ert-deftest test-backslash ()
  (verify "前面\\後面" "前面 \\ 後面")
  (verify "前面 \\ 後面" "前面 \\ 後面"))

;; 處理 : 符號 (colon)
(ert-deftest test-colon ()
  (verify "前面:後面" "前面: 後面")
  (verify "前面 : 後面" "前面 : 後面")
  (verify "前面: 後面" "前面: 後面"))

;; 處理 ; 符號 (semicolon)
(ert-deftest test-semicolon ()
  (verify "前面;後面" "前面; 後面")
  (verify "前面 ; 後面" "前面 ; 後面")
  (verify "前面; 後面" "前面; 後面"))

;; 處理 " " 符號 (quote)
(ert-deftest test-quote ()
  (verify "前面\"中文123漢字\"後面" "前面 \"中文 123 漢字\" 後面")
  (verify "前面\"123漢字\"後面" "前面 \"123 漢字\" 後面")
  (verify "前面\"中文123漢字\" tail" "前面 \"中文 123 漢字\" tail")
  (verify "head \"中文123漢字\"後面" "head \"中文 123 漢字\" 後面")
  (verify "head \"中文123漢字\" tail" "head \"中文 123 漢字\" tail"))

;; 處理 ' 符號 (single quote)
(ert-deftest test-single-quote ()
  ;; TODO: https://github.com/vinta/pangu.py/blob/master/test_pangu.py#L222
  (verify "陳上進 likes 林依諾's status." "陳上進 likes 林依諾's status."))

;; 處理 ; 符號 (comma)
(ert-deftest test-comma ()
  (verify "前面,後面" "前面, 後面")
  (verify "前面 , 後面" "前面 , 後面")
  (verify "前面, 後面" "前面, 後面"))

;; 處理 < 符號 (less than)
(ert-deftest test-less-than ()
  (verify "前面<後面" "前面 < 後面")
  (verify "前面 < 後面" "前面 < 後面")
  (verify "Vinta<Mollie" "Vinta<Mollie")
  (verify "Vinta<陳上進" "Vinta < 陳上進")
  (verify "陳上進<Vinta" "陳上進 < Vinta")
  (verify "得到一個A<B的結果" "得到一個 A<B 的結果"))

;; 處理 > 符號 (greater than)
(ert-deftest test-greater-than ()
  (verify "前面>後面" "前面 > 後面")
  (verify "前面 > 後面" "前面 > 後面")
  (verify "Vinta>Mollie" "Vinta>Mollie")
  (verify "Vinta>陳上進" "Vinta > 陳上進")
  (verify "陳上進>Vinta" "陳上進 > Vinta")
  (verify "得到一個A>B的結果" "得到一個 A>B 的結果"))

;; 處理 < > 符號 (less and greater than)
(ert-deftest test-less-and-greater-than ()
  (verify "前面<中文123漢字>後面" "前面 <中文 123 漢字> 後面")
  (verify "前面<中文123>後面" "前面 <中文 123> 後面")
  (verify "前面<123漢字>後面" "前面 <123 漢字> 後面")
  (verify "前面<中文123漢字> tail" "前面 <中文 123 漢字> tail")
  (verify "head <中文123漢字>後面" "head <中文 123 漢字> 後面")
  (verify "head <中文123漢字> tail" "head <中文 123 漢字> tail"))

;; 處理 . 符號 (period)
(ert-deftest test-period ()
  (verify "前面.後面" "前面. 後面")
  (verify "前面 . 後面" "前面 . 後面")
  (verify "前面. 後面" "前面. 後面"))

;; 處理 ? 符號 (question mark)
(ert-deftest test-question-mark ()
  (verify "前面?後面" "前面? 後面")
  (verify "前面 ? 後面" "前面 ? 後面")
  (verify "前面? 後面" "前面? 後面"))

;; 處理 / 符號 (slash)
(ert-deftest test-slash ()
  (verify "前面/後面" "前面 / 後面")
  (verify "前面 / 後面" "前面 / 後面")
  (verify "Vinta/Mollie" "Vinta/Mollie")
  (verify "Vinta/陳上進" "Vinta / 陳上進")
  (verify "陳上進/Vinta" "陳上進 / Vinta")
  (verify "得到一個A/B的結果" "得到一個 A/B 的結果"))

;; 處理特殊字元 (special characters)
(ert-deftest test-special-characters ()
  ;; \u201c and \u201d
  (verify "前面“中文123漢字”後面" "前面 “中文 123 漢字” 後面")
  ;; \u2026
  (verify "前面…後面" "前面… 後面")
  (verify "前面……後面" "前面…… 後面")
  ;; \u2027
  (verify "前面‧後面" "前面 ‧ 後面"))

;;; test.el ends here
