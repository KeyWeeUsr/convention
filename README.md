# conventional
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![CI][ci-badge]][ci-workflow]
[![Coverage Status][cover-badge]][cover-link]
[![Buy me a coffee][bmc-badge]][bmc-link]
[![Liberapay][lp-badge]][lp-link]
[![PayPal][ppl-badge]][ppl-link]

A collection of minor modes supporting Conventional syntax.

Currently available:
* [conventional: comments][comments]
* [Conventional Commits][commits]

## How to

Clone and install manually, then simply:
* `M-x conventional-comments-mode`
* `M-x conventional-commits-mode`

### Enable for commit editing in Emacs

With `EDITOR` environment variable or [git `core.editor`][git-editor] set up
for Emacs a temporary file is available for editors external to `git` for
convenient commit message writing. Normally it's `./.git/COMMIT_EDITMSG` and in
ncombination with `find-file-hook` one can listen for buffer's name and enable
the mode for conventional commits like this:

```emacs-lisp
(add-hook
 'find-file-hook
 (lambda (&rest _)
   (if (string= (file-name-base buffer-file-name) "COMMIT_EDITMSG")
       (conventional-commits-mode)
     (conventional-comments-mode))))
```

[melpa-badge]: http://melpa.org/packages/conventional-badge.svg
[melpa-package]: http://melpa.org/#/conventional
[melpa-stable-badge]: http://stable.melpa.org/packages/conventional-badge.svg
[melpa-stable-package]: http://stable.melpa.org/#/conventional
[bmc-badge]: https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee
[bmc-link]: https://www.buymeacoffee.com/peterbadida
[ppl-badge]: https://img.shields.io/badge/-paypal-grey?logo=paypal
[ppl-link]: https://paypal.me/peterbadida
[lp-badge]: https://img.shields.io/badge/-liberapay-grey?logo=liberapay
[lp-link]: https://liberapay.com/keyweeusr
[ci-badge]: https://github.com/KeyWeeUsr/conventional/actions/workflows/test.yml/badge.svg
[ci-workflow]: https://github.com/KeyWeeUsr/conventional/actions/workflows/test.yml
[cover-badge]: https://coveralls.io/repos/github/KeyWeeUsr/conventional/badge.svg?branch=master
[cover-link]: https://coveralls.io/github/KeyWeeUsr/conventional?branch=master
[comments]: https://conventionalcomments.org
[commits]: https://www.conventionalcommits.org
[git-editor]: https://git-scm.com/book/en/v2/Customizing-Git-Git-Configuration#_core_editor
