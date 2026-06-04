# Byte Compilation

## Async byte compilation

Order matters during async byte compilation.

- Define macros before their use.
- If a macro expands into a call to a helper function, the helper may need `eval-and-compile`.
- The async compilation process is non-interactive.
- See the help for `zenit--fn-crawl` when debugging compile-time dependency crawling.

## Compile-time evaluation

`eval-when-compile` evaluates its body at compile time and at source load time, like `progn`:

- Loading the `.el` file evaluates the body.
- Compiling the `.el` file evaluates the body.
- Loading the `.elc` file does not evaluate the body because it is not part of the compiled output.

This is equivalent to `(cl-eval-when (compile load) ...)`.

`(cl-eval-when (compile) ...)` evaluates only at compile time. It needs `cl-lib` or `cl-macs` available at compile time, usually via:

```elisp
(eval-when-compile (require 'cl-lib))
(cl-eval-when (compile) ...)
```

`(cl-eval-when (compile) ...)` is stricter than `(eval-when-compile (require 'cl-lib))`.

## `declare-function`

`declare-function` does not understand extended `cl-defun` argument lists well. Instead of declaring the full extended signature:

```elisp
(declare-function zenit-async-byte-compile-file "zenit-lib-compile" (file &key (req-core-lib nil) (req-core nil) (req-core-libs nil) (req-extra nil) (modulep nil) (autoloads nil)))
```

use a conservative `&rest` signature:

```elisp
(declare-function zenit-async-byte-compile-file "zenit-lib-compile" (file &rest kwargs))
```

This can still expose a byte-compiler/check-declare mismatch because `cl-defun` expands to a plain `defun` with `&rest --cl-rest--`, while `check-declare` sees the original argument list. That limitation is acceptable.
