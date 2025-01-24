<a href="http://github.com/coldnew/pangu-spacing"><img src="https://www.gnu.org/software/emacs/images/emacs.png" alt="Emacs Logo" width="80" height="80" align="right"></a>
## pangu-spacing.el
*Minor-mode to add space between Chinese and English characters.*

---
[![License GPLv3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.html)
[![MELPA](http://melpa.org/packages/pangu-spacing-badge.svg)](http://melpa.org/#/pangu-spacing)
[![MELPA Stable](http://stable.melpa.org/packages/pangu-spacing-badge.svg)](http://stable.melpa.org/#/pangu-spacing)

### Screenshot

![Screenshot](https://github.com/coldnew/pangu-spacing/raw/master/screenshot/screenshot.gif)

### Commentary (English)

pangu-spacing-mode is an minor-mode to auto add `space` between Chinese
and English characters. Note that these whitespace characters are not really
added to your file. It's just a local visual change.

Take following sentance for example:

     你好，我是coldnew，我喜歡使用emacs。

After you use pangu-spacing-mode, you will see

     你好，我是 coldnew，我喜歡使用 emacs。

But the text doesn't get modified by this mode. After disabling
pangu-spacing-mode, or when using another text editor to open the file, you
will still see:

     你好，我是coldnew，我喜歡使用emacs。

pangu-spacing is named from [pangu.js](https://github.com/vinta/pangu.js)'s README.

     Translation of pangu.js's README [1]

     If you are the one who feel quiet ill when see Chinese,
     English and digits characters squeezed together and
     want to add whitespace to separate them, this plugin (support
     Chrome and Firefox) is what you need when surfing the
     internet. It will add whitespace between Chinese,
     half-width English, digits and symbols automatically.

     These spaces between English and Chinese characters are called
     pangu-spacing by a sinologist, since it breaks the confusion
     between full-width and half-width characters.
     Studies showed that people who dislike adding whitespace between
     English and Chinese characters also have relationship problems.
     Almost 70 percent of them will get married to the one they
     don't love, the rest are only left to their cat.
     Indeed, love and writing need some space in good time.

     Let's go for it.

     [1] https://github.com/vinta/pangu.js

### Commentary (Chinese)


pangu-spacing-mode 是一個可以自動幫你將中文與英文之間加上`空白`作為分隔的 minor-mode, 他的名稱來自於 [pangu.js](https://github.com/vinta/pangu.js) 上的 README。

     引述自 pangu.js README [1]

     如果你跟我一樣，每次看到網頁上的中文字和英文、數字、符號擠在一塊，就會
     坐立難安，忍不住想在它們之間加個空格。這個外掛（支援 Chrome 和 Firefox）
     正是你在網路世界走跳所需要的東西，它會自動替你在網頁中所有的中文字和半
     形的英文、數字、符號之間插入空白。

     漢學家稱這個空白字元為「盤古之白」，因為它劈開了全形字和半形字之間的混
     沌。另有研究顯示，打字的時候不喜歡在中文和英文之間加空格的人，感情路都
     走得很辛苦，有七成的比例會在 34 歲的時候跟自己不愛的人結婚，而其餘三成
     的人最後只能把遺產留給自己的貓。畢竟愛情跟書寫都需要適時地留白。

     與大家共勉之。

     [1] https://github.com/vinta/pangu.js

### Installation


If you have `melpa` and `emacs24` installed, simply type:

     M-x package-install pangu-spacing


For `cask` user, just add following lines in your `Cask` file

```elisp
(source melpa)

(depends-on "pangu-spacing")
```


### Configuration


In your .emacs

```elisp
(require 'pangu-spacing)
(global-pangu-spacing-mode 1)
```

pangu-spacing-mode does not actuall yinsert space between English and
Chinese by default. If you want this behaviour, enable this option manually.

```elisp
(setq pangu-spacing-real-insert-separtor t)
```

After enabling this, pangu-space will be inserted before you save file.

If you only want to insert whitespace in some specific mode, but add
virtual space in another mode, you can use following code to achieve
this: (take org-mode as example)

```elisp
(add-hook 'org-mode-hook
          '(lambda ()
             (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))
```




### Customization Documentation

#### `pangu-spacing-separator`

String to be displayed between Chinese and English.

#### `pangu-spacing-real-insert-separtor`

Set t or nil to make space show only on overlay or insert in file.
When you set t here, the space will be insert when you save file.

#### `pangu-spacing-inhibit-mode-alist`

Inhibit mode alist for pangu-spacing-mode.

#### `pangu-spacing-special-region-func-alist`

Alist mapping major-mode to the corresponding function to
  check for special region that shall not write real pangu-space

### Function and Macro Documentation

#### `(pangu-spacing-chinese-two-byte-category)`

Return the correct category name for Chinese two-byte characters.

#### `(pangu-spacing-search-buffer REGEXP START END FUNC)` (macro)

Helper macro to search buffer and do func according regexp for
pangu-spacing-mode.

#### `(pangu-spacing-search-overlay BEG END FUNC REGEXP)` (macro)

Helper macro to search and update overlay according func and regexp for
pangu-sapce-mode.

#### `(pangu-spacing-search-and-replace MATCH REGEXP)`

Replace regexp with match in buffer.

#### `(pangu-spacing-overlay-p OV)`

Determine whether overlay OV was created by space-between.

#### `(pangu-spacing-check-overlay BEG END)`

Insert a space between English words and Chinese characters in overlay.

#### `(pangu-spacing-modify-buffer)`

Real insert separator between English words and Chinese characters in buffer.

#### `(pangu-spacing-region-has-pangu-spacing-overlays BEG END)`

Check if region specified by BEG and END has overlay.
  Return t if it has at least one pangu-spacing overlay, nil if no overlay.

#### `(pangu-spacing-make-overlay BEG END)`

Allocate a pangu-spacing overlay in range.

#### `(pangu-spacing-delete-overlay BEG END)`

Delete all pangu-spacing-overlays in BUFFER.

#### `(pangu-spacing-delete-all-overlays &optional BEG END)`

Delete all pangu-spacing-overlays in BUFFER.

#### `(pangu-spacing-space-current-buffer)`

Space current buffer.
It will really insert separator, no matter what
`pangu-spacing-real-insert-separtor` is.

-----
<div style="padding-top:15px;color: #d0d0d0;">
Markdown README file generated by
<a href="https://github.com/mgalgs/make-readme-markdown">make-readme-markdown.el</a>
</div>
