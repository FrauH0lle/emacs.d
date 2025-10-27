;;; gptel-todos.el --- Todo system for gptel -*- lexical-binding: t; -*-

;; Storage: hash table per buffer + JSON persistence in /tmp
(defvar gptel-todos--storage-table (make-hash-table :test 'equal)
  "Hash table mapping buffer names to their todo storage hash tables.")

(defvar gptel-todos--file-table (make-hash-table :test 'equal)
  "Hash table mapping buffer names to their todo JSON file paths.")

(defun gptel-todos--get-storage ()
  "Get the todo storage hash table for the current buffer."
  (let* ((buf-name (buffer-name))
         (storage (gethash buf-name gptel-todos--storage-table)))
    ;; If storage exists but is not a hash table (corrupted data), recreate it
    (if (and storage (not (hash-table-p storage)))
        (let ((new-storage (make-hash-table :test 'equal)))
          (puthash buf-name new-storage gptel-todos--storage-table)
          new-storage)
      (or storage
          (let ((new-storage (make-hash-table :test 'equal)))
            (puthash buf-name new-storage gptel-todos--storage-table)
            new-storage)))))

(defun gptel-todos--get-file ()
  "Get the todo file path for the current buffer."
  (let ((buf-name (buffer-name)))
    (or (gethash buf-name gptel-todos--file-table)
        (let ((file (expand-file-name
                     (format "gptel-todos-%s.json"
                             (secure-hash 'md5 buf-name))
                     temporary-file-directory)))
          (puthash buf-name file gptel-todos--file-table)
          file))))

(defun gptel-todos--load-from-file ()
  "Load todos from the JSON file into the hash table."
  (let ((storage (gptel-todos--get-storage))
        (file (gptel-todos--get-file)))
    (when (file-exists-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'string)
                   (data (json-read-from-string (buffer-string)))
                   (todos (alist-get "todos" data nil nil #'equal)))
              ;; Ensure storage is a hash table before clearing
              (unless (hash-table-p storage)
                (setq storage (make-hash-table :test 'equal))
                (puthash (buffer-name) storage gptel-todos--storage-table))
              (clrhash storage)
              ;; Convert vector to list if needed
              (when (vectorp todos)
                (setq todos (append todos nil)))
              (dolist (todo todos)
                (when-let ((id (alist-get "id" todo nil nil #'equal)))
                  (puthash id todo storage)))))
        (error
         ;; On error, clear the corrupted file and reset storage
         (message "Error loading todos: %s" (error-message-string err))
         (ignore-errors (delete-file file))
         (clrhash storage))))))

(defun gptel-todos--save-to-file ()
  "Save todos from the hash table to the JSON file."
  (require 'json)
  (let ((storage (gptel-todos--get-storage))
        (file (gptel-todos--get-file)))
    (with-temp-buffer
      (let* ((todos (hash-table-values storage))
             (data `(("todos" . ,todos)))
             (json-encoding-pretty-print t))
        (insert (json-encode data))
        (write-region nil nil file nil 'silent)))))

(defun gptel-todos--get-all ()
  "Return all todos as a list."
  (gptel-todos--load-from-file)
  (hash-table-values (gptel-todos--get-storage)))

(defun gptel-todos--plist-to-alist (plist)
  "Convert a PLIST to an alist with string keys."
  (let (result)
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (push (cons (if (keywordp key)
                        (substring (symbol-name key) 1)
                      (format "%s" key))
                    val)
              result)))
    (nreverse result)))

(defun gptel-todos--update-all (todos)
  "Update todos with new TODOS list (can be vector or list)."
  (let ((storage (gptel-todos--get-storage))
        (counter 0))
    (clrhash storage)
    ;; Convert vector to list if needed
    (when (vectorp todos)
      (setq todos (append todos nil)))
    (dolist (todo todos)
      ;; Convert plist to alist if needed (check if first element is a keyword)
      (when (and (consp todo) (keywordp (car todo)))
        (setq todo (gptel-todos--plist-to-alist todo)))

      ;; Get existing ID or generate a new one
      (let* ((existing-id (alist-get "id" todo nil nil #'equal))
             (id (or existing-id
                     (format "todo-%d-%d" (float-time) (cl-incf counter))))
             ;; Build new todo with ID included
             (todo-with-id (if existing-id
                               todo
                             (cons (cons "id" id) todo))))
        (puthash id todo-with-id storage)))
    (gptel-todos--save-to-file)))

(defun gptel-todos--render-box (todos)
  "Render TODOS as a visual box with proper faces and styling."
  ;; Convert vector to list if needed
  (when (vectorp todos)
    (setq todos (append todos nil)))
  (let ((lines '()))
    ;; Header with underline
    (push (propertize "\n━━━ Current Tasks ━━━\n"
                      'face '(:inherit font-lock-string-face :weight bold))
          lines)
    (if (null todos)
        (push (propertize "  (no tasks)\n"
                          'face 'font-lock-comment-face)
              lines)
      (dolist (todo todos)
        (when-let* ((content (alist-get "content" todo nil nil #'equal))
                    (status (alist-get "status" todo nil nil #'equal)))
          (let* ((status-icon (pcase status
                                ("completed" "✓")
                                ("in_progress" "→")
                                ("pending" "○")
                                (_ "·")))
                 (status-face (pcase status
                                ("completed" 'success)
                                ("in_progress" 'warning)
                                ("pending" 'font-lock-comment-face)
                                (_ 'default)))
                 (active-form (alist-get "activeForm" todo nil nil #'equal))
                 (display-text (if (equal status "in_progress")
                                   (or active-form content)
                                 content)))
            (push (format "  %s %s\n"
                          (propertize status-icon 'face status-face)
                          (propertize display-text 'face status-face))
                  lines)))))
    ;; Footer with underline
    (push (propertize "\n"
                      'face '(:inherit font-lock-string-face :underline t :extend t))
          lines)
    (apply #'concat (nreverse lines))))

(defvar-local gptel-todos--last-insert-pos nil
  "Marker tracking where to insert the next todo visual display.")

(defvar-local gptel-todos--display-overlay nil
  "Overlay marking the current todo display box.")

(defun gptel-todos--remove-previous-display ()
  "Remove the previous todo display if it exists."
  (when (and gptel-todos--display-overlay
             (overlay-buffer gptel-todos--display-overlay))
    (delete-overlay gptel-todos--display-overlay)
    (setq gptel-todos--display-overlay nil)))

(defun gptel-todos--insert-visual-display (todos)
  "Display TODOS using an overlay in the gptel buffer.
Removes previous display and creates a new overlay at the end of the last
gptel response. Uses the overlay's after-string property to display the
todo list without inserting actual buffer text.
Skips display in agent buffers (buffers starting with *gptel-agent-)."
  ;; Skip display in agent buffers
  (unless (string-prefix-p "*gptel-agent-" (buffer-name))
    ;; Remove any previous todo display
    (gptel-todos--remove-previous-display)

    (let ((box (gptel-todos--render-box todos)))
      (save-excursion
        ;; Always position at the current end of buffer
        (goto-char (point-max))
        (skip-chars-backward " \t\r\n")
        (let* ((ov-start (point))
               ;; Create a zero-width overlay at the current buffer end
               (ov (make-overlay ov-start ov-start nil nil nil)))

          ;; Use after-string instead of inserting real text
          (overlay-put ov 'after-string box)
          (overlay-put ov 'gptel-todos t)
          (overlay-put ov 'evaporate nil)

          (setq gptel-todos--display-overlay ov)
          ;; Update marker for next insertion
          (setq gptel-todos--last-insert-pos (copy-marker ov-start)))))))

;;;###autoload
(defun gptel-todos-write (callback todos)
  "TodoWrite tool: Update the todo list with TODOS array.
CALLBACK is called with the result for the LLM."
  (require 'json)
  (condition-case err
      (progn
        ;; Convert vector to list if needed
        (when (vectorp todos)
          (setq todos (append todos nil)))

        ;; Update storage (this also adds IDs if missing)
        (gptel-todos--update-all todos)

        ;; Get updated todos from storage (includes auto-generated IDs)
        (let ((updated-todos (gptel-todos--get-all)))
          ;; Mark for display update - the hook will create/update the overlay
          (setq gptel-todos--pending-update updated-todos)

          ;; Call callback with success response for LLM
          (funcall callback
                   (with-temp-buffer
                     (insert
                      (json-encode
                       `(("status" . "success")
                         ("message" . "Todo list updated successfully")
                         ("count" . ,(length updated-todos)))))
                     (json-pretty-print (point-min) (point-max))
                     (buffer-string)))))
    (error
     (funcall callback
              (with-temp-buffer
                (insert
                 (json-encode
                  `(("status" . "error")
                    ("message" . ,(format "Failed to update todos: %s"
                                          (error-message-string err))))))
                (json-pretty-print (point-min) (point-max))
                (buffer-string))))))

;;;###autoload
(defun gptel-todos-read (callback)
  "TodoRead tool: Read the current todo list.
CALLBACK is called with the result for the LLM."
  (require 'json)
  (condition-case err
      (let* ((todos (gptel-todos--get-all))
             (data `(("todos" . ,todos))))

        ;; Mark for display update - the hook will create/update the overlay
        (setq gptel-todos--pending-update todos)

        ;; Call callback with todos JSON for LLM
        (funcall callback
                 (with-temp-buffer
                   (insert
                    (json-encode data))
                   (json-pretty-print (point-min) (point-max))
                   (buffer-string))))
    (error
     (funcall callback
              (with-temp-buffer
                (insert
                 (json-encode
                  `(("status" . "error")
                    ("message" . ,(format "Failed to read todos: %s"
                                          (error-message-string err))))))
                (json-pretty-print (point-min) (point-max))
                (buffer-string))))))

;;; Hook integration

(defvar-local gptel-todos--pending-update nil
  "Pending todos to display after the next stream update.")

(defun gptel-todos--reset-position-h ()
  "Reset the todo insert position for a new response.
Add this to `gptel-pre-response-hook'."
  (setq gptel-todos--last-insert-pos nil)
  ;; DON'T clear pending update or remove overlay here!
  ;; The pre-response hook fires after each tool execution,
  ;; but we want the overlay to persist across tool calls.
  ;; Only clear pending if there's no overlay (meaning we haven't created it yet)
  (unless (and gptel-todos--display-overlay
               (overlay-buffer gptel-todos--display-overlay))
    (setq gptel-todos--pending-update nil)))

(defun gptel-todos--maintain-display-h ()
  "Re-insert todo display after streaming if we have pending updates.
Add this to `gptel-post-stream-hook'.
Skips display in agent buffers."
  ;; Skip display in agent buffers
  (unless (string-prefix-p "*gptel-agent-" (buffer-name))
    ;; Always recreate overlay if we have pending updates to keep it at buffer end
    (when gptel-todos--pending-update
      (gptel-todos--remove-previous-display)
      (gptel-todos--insert-visual-display gptel-todos--pending-update))))

;; Reset position at the start of each new response
(add-hook 'gptel-pre-response-hook #'gptel-todos--reset-position-h)
;; Maintain display during streaming
(add-hook 'gptel-post-stream-hook #'gptel-todos--maintain-display-h)
(add-hook 'gptel-post-response-functions (lambda (_beg _end) (gptel-todos--remove-previous-display)))

;;;###autoload
(defun gptel-todos-reset ()
  "Reset the todo system by clearing all stored data.
This removes the in-memory storage and deletes the persistent JSON file."
  (interactive)
  (let ((buf-name (buffer-name))
        (file (gptel-todos--get-file)))
    ;; Clear in-memory storage
    (when-let ((storage (gethash buf-name gptel-todos--storage-table)))
      (when (hash-table-p storage)
        (clrhash storage)))
    ;; Delete persistent file
    (when (file-exists-p file)
      (delete-file file))
    ;; Remove overlay
    (gptel-todos--remove-previous-display)
    ;; Clear pending update
    (setq gptel-todos--pending-update nil)
    (message "Todo system reset for buffer: %s" buf-name)))

(gptel-make-tool
 :name "TodoWrite"
 :function #'gptel-todos-write
 :async t
 :description
 "Use this tool to create and manage a structured task list for your current coding session. This helps you track progress, organize complex tasks, and demonstrate thoroughness to the user.
It also helps the user understand the progress of the task and overall progress of their requests.

#### When to Use This Tool
Use this tool proactively in these scenarios:

1. Complex multi-step tasks - When a task requires 3 or more distinct steps or actions
2. Non-trivial and complex tasks - Tasks that require careful planning or multiple operations
3. User explicitly requests todo list - When the user directly asks you to use the todo list
4. User provides multiple tasks - When users provide a list of things to be done (numbered or comma-separated)
5. After receiving new instructions - Immediately capture user requirements as todos
6. When you start working on a task - Mark it as in_progress BEFORE beginning work. Ideally you should only have one todo as in_progress at a time
7. After completing a task - Mark it as completed and add any new follow-up tasks discovered during implementation

#### When NOT to Use This Tool

Skip using this tool when:
1. There is only a single, straightforward task
2. The task is trivial and tracking it provides no organizational benefit
3. The task can be completed in less than 3 trivial steps
4. The task is purely conversational or informational

NOTE that you should not use this tool if there is only one trivial task to do. In this case you are better off just doing the task directly.

#### Examples of When to Use the Todo List

<example>
User: I want to add a dark mode toggle to the application settings. Make sure you run the tests and build when you're done!
Assistant: I'll help add a dark mode toggle to your application settings. Let me create a todo list to track this implementation.
*Creates todo list with the following items:*
1. Creating dark mode toggle component in Settings page
2. Adding dark mode state management (context/store)
3. Implementing CSS-in-JS styles for dark theme
4. Updating existing components to support theme switching
5. Running tests and build process, addressing any failures or errors that occur
*Begins working on the first task*

<reasoning>
The assistant used the todo list because:
1. Adding dark mode is a multi-step feature requiring UI, state management, and styling changes
2. The user explicitly requested tests and build be run afterward
3. The assistant inferred that tests and build need to pass by adding \"Ensure tests and build succeed\" as the final task
</reasoning>
</example>

<example>
User: Help me rename the function getCwd to getCurrentWorkingDirectory across my project
Assistant: Let me first search through your codebase to find all occurrences of 'getCwd'.
*Uses grep or search tools to locate all instances of getCwd in the codebase*
Assistant: I've found 15 instances of 'getCwd' across 8 different files. Let me create a todo list to track these changes.
*Creates todo list with specific items for each file that needs updating*

<reasoning>
The assistant used the todo list because:
1. First, the assistant searched to understand the scope of the task
2. Upon finding multiple occurrences across different files, it determined this was a complex task with multiple steps
3. The todo list helps ensure every instance is tracked and updated systematically
4. This approach prevents missing any occurrences and maintains code consistency
</reasoning>
</example>


<example>
User: I need to implement these features for my e-commerce site: user registration, product catalog, shopping cart, and checkout flow.
Assistant: I'll help implement these features. First, let's add all the features to the todo list.
*Creates a todo list breaking down each feature into specific tasks based on the project architecture*
Assistant: Let's start with user registration. This will involve creating database models, API endpoints, and frontend forms that integrate with your existing authentication system.

<reasoning>
The assistant used the todo list because:
1. The user provided multiple complex features to implement in a comma separated list
2. The todo list helps organize these large features into manageable tasks
3. This approach allows for tracking progress across the entire implementation
</reasoning>
</example>

<example>
User: Can you help optimize my React application? It's rendering slowly and has performance issues.</user>
Assistant: I'll help optimize your React application. First, let me examine your codebase to identify potential performance bottlenecks.
*Reviews component structure, render patterns, state management, and data fetching*
Assistant: After analyzing your codebase, I've identified several performance issues. Let me create a todo list to track our optimization efforts.
*Creates todo list with items like: 1) Implementing memoization for expensive calculations in ProductList, 2) Adding virtualization for long lists in Dashboard, 3) Optimizing image loading in Gallery component, 4) Fixing state update loops in ShoppingCart, 5) Reviewing bundle size and implementing code splitting*
Let's start by implementing memoization for the expensive calculations in your ProductList component.</assistant>

<reasoning>
The assistant used the todo list because:
1. First, the assistant examined the codebase to identify specific performance issues
2. Based on this analysis, it identified multiple optimization opportunities
3. Performance optimization is a non-trivial task requiring multiple steps
4. The todo list helps methodically track improvements across different components
5. This systematic approach ensures all performance bottlenecks are addressed
</reasoning>
</example>

#### Examples of When NOT to Use the Todo List

<example>
User: How do I print 'Hello World' in Python?
Assistant: In Python, you can print \"Hello World\" with this simple code:

python
print(\"Hello World\")

This will output the text \"Hello World\" to the console when executed.</assistant>

<reasoning>
The assistant did not use the todo list because this is a single, trivial task that can be completed in one step. There's no need to track multiple tasks or steps for such a straightforward request.
</reasoning>
</example>

<example>
User: What does the git status command do?
Assistant: The git status command shows the current state of your working directory and staging area. It displays which changes have been staged, which haven't, and which files aren't being tracked by Git. This is a fundamental command for understanding the current state of your repository before making commits.

<reasoning>
The assistant did not use the todo list because this is an informational request with no actual coding task to complete. The user is simply asking for an explanation, not for the assistant to perform multiple steps or tasks.
</reasoning>
</example>

<example>
User: Can you add a comment to the calculateTotal function to explain what it does?
Assistant: Sure, let me add a comment to the calculateTotal function to explain what it does.
* Uses the Edit tool to add a comment to the calculateTotal function *

<reasoning>
The assistant did not use the todo list because this is a single, straightforward task confined to one location in the code. Adding a comment doesn't require tracking multiple steps or systematic organization.
</reasoning>
</example>

<example>
User: Run npm install for me and tell me what happens.
Assistant: I'll run the npm install command for you.

*Executes: npm install*

The command completed successfully. Here's the output:
[Output of npm install command]

All dependencies have been installed according to your package.json file.

<reasoning>
The assistant did not use the todo list because this is a single command execution with immediate results. There are no multiple steps to track or organize, making the todo list unnecessary for this straightforward task.
</reasoning>
</example>

#### Task States and Management

1. **Task States**: Use these states to track progress:
   - pending: Task not yet started
   - in_progress: Currently working on (limit to ONE task at a time)
   - completed: Task finished successfully

   **IMPORTANT**: Task descriptions must have two forms:
   - content: The imperative form describing what needs to be done (e.g., \"Run tests\", \"Build the project\")
   - activeForm: The present continuous form shown during execution (e.g., \"Running tests\", \"Building the project\")

2. **Task Management**:
   - Update task status in real-time as you work
   - Mark tasks complete IMMEDIATELY after finishing (don't batch completions)
   - Exactly ONE task must be in_progress at any time (not less, not more)
   - Complete current tasks before starting new ones
   - Remove tasks that are no longer relevant from the list entirely

3. **Task Completion Requirements**:
   - ONLY mark a task as completed when you have FULLY accomplished it
   - If you encounter errors, blockers, or cannot finish, keep the task as in_progress
   - When blocked, create a new task describing what needs to be resolved
   - Never mark a task as completed if:
     - Tests are failing
     - Implementation is partial
     - You encountered unresolved errors
     - You couldn't find necessary files or dependencies

4. **Task Breakdown**:
   - Create specific, actionable items
   - Break complex tasks into smaller, manageable steps
   - Use clear, descriptive task names
   - Always provide both forms:
     - content: \"Fix authentication bug\"
     - activeForm: \"Fixing authentication bug\"

When in doubt, use this tool. Being proactive with task management demonstrates attentiveness and ensures you complete all requirements successfully."
 :args (list '(:name "todos"
               :type array
               :description "Array of todo items to update"
               :items (:type object
                       :properties (:id (:type string
                                         :description "Unique identifier for the todo item (optional, auto-generated if not provided)"
                                         :optional t)
                                        :content (:type string
                                                  :description "The task description")
                                        :status (:type string
                                                 :enum ["pending" "in_progress" "completed"]
                                                 :description "Current status of the task")
                                        :activeForm (:type string
                                                     :description "Present continuous form (e.g., 'Running tests')")))))
 :category "task-management"
 :include t)

(gptel-make-tool
 :name "TodoRead"
 :function #'gptel-todos-read
 :async t
 :description (concat
               "Use this tool to read the current to-do list for the session. "
               "This tool should be used proactively and frequently to ensure that you are aware of the status of the current task list. "
               "You should make use of this tool as often as possible, especially in the following situations:"
               "\n\n"
               "- At the beginning of conversations to see what's pending\n"
               "- Before starting new tasks to prioritize work\n"
               "- When the user asks about previous tasks or plans\n"
               "- Whenever you're uncertain about what to do next\n"
               "- After completing tasks to update your understanding of remaining work\n"
               "- After every few messages to ensure you're on track\n"
               "\n"
               "Usage:\n"
               "- This tool takes in no parameters. So leave the input blank or empty. DO NOT include a dummy object, placeholder string or a key like \"input\" or \"empty\". LEAVE IT BLANK.\n"
               "- Returns a list of todo items with their status and content\n"
               "- Use this information to track progress and plan next steps\n"
               "- If no todos exist yet, an empty list will be returned")
 :args nil
 :category "task-management"
 :include t)
