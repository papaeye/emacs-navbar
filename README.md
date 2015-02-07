navbar.el
=========

[![Build Status](https://img.shields.io/travis/papaeye/emacs-navbar.svg?style=flat)](https://travis-ci.org/papaeye/emacs-navbar)
[![Coverage Status](https://img.shields.io/coveralls/papaeye/emacs-navbar.svg?style=flat)](https://coveralls.io/r/papaeye/emacs-navbar?branch=master)

navbar.el is a navigation bar for Emacs.
Just like the navbar component of [Bootstrap](http://getbootstrap.com/),
navbar.el can contain various components called navbar items.

navbar.el requires Emacs 24.4 or later.


Installation
------------

Put `navbar.el` and `navbarx-*.el` somewhere in your `load-path`.


Getting Started
---------------

1. This is the "Hello, world!" example:
   ![](images/hello_world.png)

2. The element of `navbar-item-list` may be a function which returns a property list.  To reflect the change of `navbar-item-list`, run `navbar-sync`:
   ![](images/navbarx_time.png)

3. `navbarx-elscreen` displays ElScreen tabs in the navbar buffer:
   ![](images/navbarx_elscreen.png)
