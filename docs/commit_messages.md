# Commit Messages

The guidelines are an adaptation from [Conventional Commits](https://www.conventionalcommits.org).

## Basic format:
```
type(component): brief description (50-72 chars)

* [filename].[ext] ([affected-items]): [Description of changes]
  [Additional context and rationale]
  [Limitations or future implications]
  
[Optional footer(s)]
```

## Guidelines

1. Start with the type, the optional package/component/scope name within
   parenthesis followed by a colon and a brief title. The first word of the
   title should be capitalized.
   - Example: `feat(parser): Add finite state machine`
   - Example: `fix(api): Prevent racing of requests`
   
   1. Append a `!` after the type/scope if the commit introduces a breaking
      change
   
2. For files changed, list them in parentheses followed by affected
   functions/variables:

   ```
   * file.ext (function1, function2, variable1): Description
   ```

3. Write the main description in present tense, explaining:
   - What changed
   - Why it changed (rationale)
   - Any limitations or caveats
   - Future implications if relevant

4. When multiple files are changed, list each one with its own bullet point and
   description:

   ```
   * file1.ext (functions): Description
   * file2.ext (functions): Related changes
   ```

5. For implementation details that affect multiple components, explain shared
   changes once and use "Ditto" for repeated similar changes:

   ```
   * component1.ext: Main change description
   * component2.ext: Ditto
   ```

6. Include important context about:
   - Experimental features or APIs
   - Testing configurations
   - Known limitations
   - Future plans or potential changes

7. Write descriptions that are clear enough to be understood without needing to
   look at the code, while still being concise

8. One or more footers may be provided one blank line after the body. Each
   footer must consist of a word token, followed by a `:<space>` separator,
   followed by a string value (see [git trailer
   convention](https://git-scm.com/docs/git-interpret-trailers)). A footer’s
   token must use `-` in place of whitespace characters, e.g., `Acked-by`. An
   exception is made for `BREAKING CHANGE`, which may also be used as a token.
   
   ```
   chore!: Drop support for Node 6

   BREAKING CHANGE: Use JavaScript features not available in Node 6.
   Reviewed-by: Z
   Refs: #123
   ```

## Notes:

- Start with type(scope): following Conventional Commits
- Include BREAKING CHANGE: in body for breaking changes
- Follow with detailed file-by-file changes
- Use present tense for descriptions
- Include rationale for non-obvious changes
- Mention experimental features or limitations
- Use "Ditto" for repeated changes across files
- Keep consistent formatting with blank lines between sections

## Conventional Commits Types:

| Type     | Description                                                       |
|:---------|:------------------------------------------------------------------|
| feat     | New feature addition                                              |
| fix      | Bug fix                                                           |
| docs     | Documentation changes                                             |
| style    | Code style/formatting changes (no code change)                    |
| refactor | Code refactoring                                                  |
| perf     | Performance improvements                                          |
| test     | Adding/fixing tests                                               |
| build    | Build system changes                                              |
| ci       | CI configuration changes                                          |
| chore    | General maintenance                                               |
| revert   | Revert previous changes                                           |
| pkg      | Addition of package/library or version change                     |
| tweak    | Code changes that change user-facing defaults but not drastically |


## Examples:

### Feature addition example

```
feat!(parser): Add async event handler system

* event_handler.py (AsyncEventManager, EventDispatcher, EventQueue): 
  Implement asynchronous event handling system using asyncio.
  This provides a more scalable approach to handling multiple
  concurrent events with support for priorities and custom callbacks.
  
  Currently experimental - API may evolve based on performance metrics.
  Known limitation: Does not support nested event triggering yet.

BREAKING CHANGE: Changes event dispatch flow, existing handlers need updates
```

### Bug fix example

```
fix(storage): Resolve race condition in concurrent writes

* src/storage/manager.rs (StorageManager::write_batch, acquire_lock): 
  Fix race condition when multiple threads attempt concurrent writes
  to the same storage partition. Implements proper mutex locking
  with deadlock prevention.
  
  Performance impact is minimal (<1ms per write operation).
```

### Multiple file changes example

```
feat!(auth): Implement OAuth2 provider integration

* auth/provider.js (createOAuthClient, validateToken): Add OAuth2
  client implementation with support for multiple providers.
  Includes automatic token refresh and session management.

* auth/middleware.js (authGuard, refreshMiddleware): Add Express
  middleware for protecting routes and handling token refresh.
  
* auth/storage.js (TokenStorage): Add secure token storage with
  encryption and automatic cleanup of expired tokens.
  Currently only supports in-memory storage, Redis support planned.

BREAKING CHANGE: Authentication flow now requires OAuth2 configuration
Refs: #234
Reviewed-by: @security-team
```


