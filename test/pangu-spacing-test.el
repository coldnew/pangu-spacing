;;; pangu-spacing-test.el --- Tests for pangu-spacing

;;; pangu-spacing-test.el ends here

(ert-deftest pangu-spacing-test/modify ()
  "Test if modify works"
  (with-temp-buffer
    (pangu-spacing-mode 1)
    (insert "跟alex解释 task 弹性的问题。a。")
    (let ((pangu-spacing-real-insert-separtor t))
      (pangu-spacing-modify-buffer))
    (should (string-equal "跟 alex 解释 task 弹性的问题。a。" (buffer-string)))
    ))

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
