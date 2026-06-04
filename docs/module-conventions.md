# Module Conventions

## Files in `core/lib`

Files under `core/lib` are added to the load path but are subfeatures of `zenit-lib`.

- Require them with `zenit-require`.
- Use the concrete file name in `declare-function` declarations, e.g. `zenit-lib-files`.

## Module checks during bootstrap and compilation

Use `zenit-module-p` instead of `modulep!` for code compiled before generated `init.el` is processed. `modulep!` uses its own API to determine whether modules are available.

## Avoid `:after` in `use-package`

Avoid the `:after` keyword in `use-package` forms.

See: <https://github.com/jwiegley/use-package/issues/829>
