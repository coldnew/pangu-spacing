;;; pangu-spacing-test.el --- Tests for pangu-spacing

(require 'ert)
(require 'pangu-spacing)

(ert-deftest pangu-spacing-test/modify ()
  "Test if modify works."
  (with-temp-buffer
    (pangu-spacing-mode 1)
    (insert "跟alex解释 task 弹性的问题。a。")
    (let ((pangu-spacing-real-insert-separtor t))
      (pangu-spacing-modify-buffer))
    (should (string-equal "跟 alex 解释 task 弹性的问题。a。" (buffer-string))))
  (with-temp-buffer
    (pangu-spacing-mode 1)
    (insert "2019年08月22日 星期四")
    (let ((pangu-spacing-real-insert-separtor t))
      (pangu-spacing-modify-buffer))
    (should (string-equal "2019 年 08 月 22 日 星期四" (buffer-string)))))

(ert-deftest pangu-spacing-test/org-mode-special-region ()
  (with-temp-buffer
    (insert "* English中文English [[https://github.com/][link中文link]]
   CLOSED: [2018-10-14日21:42]
   - State \"DONE\"       from \"TODO\"       [2018-10-14日21:42]
   [2018-09-17一]
   English中文English
* ~English中文English~
   <2018-09-17一>
   =/home/me/English中文English.conf=
   #+BEGIN_SRC conf :tangle English中文English.conf
     English中文English
   #+END_SRC")
    (pangu-spacing-mode 1)
    (org-mode)
    (let ((pangu-spacing-real-insert-separtor t))
      (pangu-spacing-modify-buffer))
    (should (string-equal (buffer-string)
                          "* English 中文 English [[https://github.com/][link中文link]]
   CLOSED: [2018-10-14日21:42]
   - State \"DONE\"       from \"TODO\"       [2018-10-14日21:42]
   [2018-09-17一]
   English 中文 English
* ~English中文English~
   <2018-09-17一>
   =/home/me/English中文English.conf=
   #+BEGIN_SRC conf :tangle English中文English.conf
     English中文English
   #+END_SRC"))))

(ert-deftest pangu-spacing-test/show ()
  "Test if showing works"
  (with-temp-buffer
    (pangu-spacing-mode 1)
    (insert "跟alex解释 task 弹性的问题。a。")
    (should (string-equal "跟alex解释 task 弹性的问题。a。" (buffer-string)))
    (pangu-spacing-check-overlay (point-min) (point-max))
    (let* ((overlay-list (overlays-in (point-min) (point-max)))
           (overlay-pos (mapcar (lambda (ov)
                               (when (pangu-spacing-overlay-p ov)
                                 (overlay-end ov)))
                                overlay-list))
           (sorted-pos (sort overlay-pos '>=)))
      (mapcar (lambda (pos)
                (goto-char pos)
                (insert " "))
              sorted-pos)
      (should (string-equal "跟 alex 解释 task 弹性的问题。a。" (buffer-string))))))

(ert-deftest pangu-spacing-test/japanese ()
  "Test Japanese and Korean pangu-spacing."
  (with-temp-buffer
    (pangu-spacing-mode 1)
    (insert "こんにちはhello worldこんにちは
コンニチハhello worldコンニチハ")
    (let ((pangu-spacing-real-insert-separtor t))
      (pangu-spacing-modify-buffer))
      (should (string-equal "こんにちは hello world こんにちは
コンニチハ hello world コンニチハ" (buffer-string)))))

;;; pangu-spacing-test.el ends here
