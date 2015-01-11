navbar.el
=========

navbar.el is a navigation bar for Emacs.

navbar.el requires Emacs 24.3 or later.


Installation
------------

Put `navbar.el` and `navbarx-*.el` somewhere in your `load-path`.

To display ElScreen tabs with navbar.el, use the forked version of ElScreen from [papaeye/elscreen](https://github.com/papaeye/elscreen) for now.


Getting Started
---------------

Evaluate the following code.

```elisp
(require 'navbar)
(setq navbar-item-list '(navbarx-version "Hello, world!"))
(navbar-mode)
```
