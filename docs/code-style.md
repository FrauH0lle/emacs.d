# Code Style

## Documentation

- Add docstrings for functions and variables.
- The first docstring line should stay under 67 characters.
- Function docstrings should briefly answer: what does this function do?
- Variable docstrings should briefly answer: what does this value mean?
- Explanatory code comments should be plentiful, even every line when useful. They should explain what is happening and why.

## Emacs Lisp conventions

- Follow standard Emacs Lisp coding conventions.
- Useful references:
  - [The Emacs Lisp Style Guide](https://github.com/bbatsov/emacs-lisp-style-guide)
  - [How to Write Fast(er) Emacs Lisp](https://nullprogram.com/blog/2017/01/30/)
- Macros should declare a debug specification. If all arguments evaluate normally, `(declare (debug t))` is enough.
