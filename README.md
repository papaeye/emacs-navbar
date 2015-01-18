navbar.el
=========

[![Build Status](https://img.shields.io/travis/papaeye/emacs-navbar.svg?style=flat)](https://travis-ci.org/papaeye/emacs-navbar)
[![Coverage Status](https://img.shields.io/coveralls/papaeye/emacs-navbar.svg?style=flat)](https://coveralls.io/r/papaeye/emacs-navbar?branch=master)

navbar.el is a navigation bar for Emacs.
Just like the navbar component of [Bootstrap](http://getbootstrap.com/),
navbar.el can contain various components called navbar items.

navbar.el requires Emacs 24.3 or later.


Installation
------------

Put `navbar.el` and `navbarx-*.el` somewhere in your `load-path`.

To display ElScreen tabs with navbar.el, use the forked version of ElScreen
from [papaeye/elscreen](https://github.com/papaeye/elscreen) for now.


Getting Started
---------------

Evaluate the following code.

```elisp
(require 'navbar)
(setq navbar-item-list '(navbarx-version "Hello, world!"))
(navbar-mode)
```
