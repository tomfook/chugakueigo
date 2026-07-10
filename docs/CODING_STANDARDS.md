# ChugakuEigo Coding Standards

Full coding standards for this repository. Referenced from [CLAUDE.md](../CLAUDE.md) § Coding Standards.

## Utility Functions

### Error Handling Infrastructure

The project uses a standardized error handling system through `utils.R` to ensure consistency across all modules.

#### utils.R
- **Location**: Project root (same level as `constants.R`)
- **Purpose**: Common utility functions shared across all modules
- **Status**: Infrastructure file (NOT a module - can be sourced by any module)

#### utils_safe_execute Function
Standardizes error handling: `utils_safe_execute(operation, error_message_prefix, success_message, fallback_data, use_result_as_message)`. Returns `list(success, data, message)`. Used in data_manager.R for consistent error handling.

Use for simple error handling. Source `utils.R` in modules. Not for complex control flows.

## Function Naming Convention
All functions MUST follow namespaced naming to prevent confusion about which module contains each function:

- **Data functions**: `data_*()` (e.g., `data_initialize()`, `data_read_score()`)
- **Learning functions**: `learning_*()` (e.g., `learning_calculate_probability()`, `learning_new_question()`)
- **UI functions**: `ui_*()` (e.g., `ui_render_welcome()`, `ui_with_user_add_progress()`)
- **State functions**: `state_*()` (e.g., `state_initialize()`, `state_update()`)
- **Security functions**: `security_*()` (e.g., `security_verify_password()`, `security_hash_password()`)

## Module Organization
- Each module file should contain only functions with the same namespace prefix
- Function names clearly indicate their module location (e.g., `data_*` functions are in `data_manager.R`)
- When adding new functions, follow the established namespace prefix for the target module

## File Structure
```
modules/
├── data_manager.R       # data_* functions
├── learning_engine.R    # learning_* functions
├── ui_helpers.R         # ui_* functions
├── state_manager.R      # state_* functions
└── security_manager.R   # security_* functions
```

## UI Styling Standards
All user interface styling MUST use Bootstrap classes instead of inline CSS for consistency and maintainability:

### Bootstrap Class Usage
- **Prefer Bootstrap classes**: Use standard Bootstrap classes (e.g., `btn-danger`, `text-muted`) over custom inline styles
- **No inline CSS**: Avoid `style=` attributes in UI elements; use `class=` instead
- **Consistent styling**: Follow Bootstrap's semantic naming conventions for colors and states
- **Standard button styles**: Use Bootstrap button classes (`btn-primary`, `btn-danger`, `btn-success`, etc.)
- **Text styling**: Use Bootstrap text utility classes (`text-muted`, `text-dark`, `text-danger`, etc.)

### Examples
```r
# ✅ Good: Bootstrap classes
actionButton("delete", "Delete User", class = "btn-danger")
p("Warning message", class = "text-danger font-weight-bold")

# ❌ Bad: Inline styles
actionButton("delete", "Delete User", style = "background-color: #d9534f; color: white;")
p("Warning message", style = "font-weight: bold; color: #d9534f;")
```

### Benefits
- **Consistency**: Uniform appearance across the application
- **Maintainability**: Easier to update and modify styles
- **Responsiveness**: Automatic responsive behavior from Bootstrap
- **Standards compliance**: Follows Shiny/Bootstrap best practices

## Module Independence Policy
All modules MUST maintain independence to ensure maintainability and testability:

### Independence Requirements
- **NO cross-module source() statements**: Modules must not directly source other modules
- **NO direct function calls between modules**: Modules should not call functions from other modules directly
- **Self-contained functionality**: Each module should contain all logic necessary for its responsibilities

### Dependency Management
- **server.R as orchestration layer**: All inter-module coordination happens in server.R
- **Shared constants only**: Modules may only share constants.R for configuration values
- **Pure function design**: Module functions should be pure when possible, accepting inputs and returning outputs without side effects

### Acceptable Patterns
```r
# ✅ Good: server.R orchestrates multiple modules
result1 <- data_read_score(...)
result2 <- learning_calculate_probability(...)
user_state <- state_update_user_data(user_state, result1$data)

# ✅ Good: Modules only source constants
source("constants.R")  # Acceptable in any module

# ❌ Bad: Direct module-to-module dependency
source("modules/state_manager.R")  # Inside another module - NOT ALLOWED
```

### Benefits of Independence
- **Easier testing**: Each module can be tested in isolation
- **Reduced coupling**: Changes in one module don't cascade to others
- **Better debugging**: Issues are contained within module boundaries
- **Improved maintainability**: Clear responsibility boundaries

## Submodule Standards
When a module becomes oversized, it may be refactored using dedicated submodules to maintain clean separation of concerns:

### Submodule Structure Requirements
- **Location**: `modules/parent_module_name/submodule_name.R`
- **Access Control**: Only accessible from parent module
- **Naming**: Follow parent module's namespace + specific functionality
- **Documentation**: Must declare submodule status in file header

### Submodule Implementation Guidelines
- **File Header**: Must clearly state submodule status and access restrictions
- **Function Prefix**: Use descriptive prefix for submodule functions (e.g., `storage_*`)
- **Parent Responsibility**: Parent module must source and manage submodule
- **Independence**: Submodules follow same independence rules (no cross-submodule dependencies)

### Example Structure
```
modules/
├── data_manager.R                    # Parent module
└── data_manager/
    └── data_storage.R                # Private submodule
```

### Correct Usage Patterns
```r
# ✅ Correct: Parent module sources submodule
# modules/data_manager.R
source("modules/data_manager/data_storage.R")
result <- storage_function(...)

# ❌ Incorrect: Other modules accessing submodule directly
# modules/learning_engine.R
source("modules/data_manager/data_storage.R")  # VIOLATION
```

### Benefits
- **Reduced Module Size**: Keep modules under ~200 lines
- **Clear Responsibility**: Separate concerns while maintaining cohesion
- **Maintained Independence**: No cross-module dependencies
- **Future Flexibility**: Easy to refactor implementation details

## Error Handling and Return Value Standards
All functions MUST follow standardized error handling and return value conventions:

### Standardized Return Format
Functions should return structured responses in the following format:
```r
# Success case
list(
  success = TRUE,
  data = <actual_result_data>,
  message = "Operation completed successfully"
)

# Error case  
list(
  success = FALSE,
  data = NULL,
  message = "Descriptive error message"
)
```

### Implementation Guidelines
- **NO stop() exceptions**: Replace `stop()` calls with structured error responses
  - **Exception**: Using `stop()` **within** `utils_safe_execute()` operations is acceptable because the `tryCatch` catches the exception and converts it to a structured response. This pattern keeps operation code clean while maintaining structured error handling.
- **Consistent error checking**: Always check `result$success` before using `result$data`
- **Meaningful messages**: Provide clear, user-friendly error messages
- **Data encapsulation**: Wrap result data in the `data` field for consistent access patterns

### Exception: stop() within utils_safe_execute
While the standard discourages `stop()` calls, using `stop()` **within** `utils_safe_execute()` operations is acceptable because:
- The `tryCatch` in `utils_safe_execute()` catches the exception and converts it to structured response
- This pattern keeps operation code clean and separates error handling logic
- The end result is still a structured `list(success, data, message)` response

Example of acceptable pattern:
```r
data_example <- function(input) {
  utils_safe_execute(
    operation = function() {
      if (invalid_condition) {
        stop("Validation failed")  # Acceptable - will be caught and converted
      }
      # ... normal processing
      return(result)
    },
    error_message_prefix = "Operation failed:"
  )
}
```

### Examples
```r
# Function implementation
data_example_function <- function(input) {
  if (invalid_input) {
    return(list(success = FALSE, data = NULL, message = "Invalid input provided"))
  }
  
  tryCatch({
    result <- process_data(input)
    return(list(success = TRUE, data = result, message = "Data processed successfully"))
  }, error = function(e) {
    return(list(success = FALSE, data = NULL, message = paste("Processing failed:", e$message)))
  })
}

# Calling code
result <- data_example_function(input)
if (!result$success) {
  handle_error(result$message)
  return()
}
actual_data <- result$data
```

### Migration Notes
- Legacy functions using `stop()` should be gradually migrated to this format
- When updating a function's return format, ALL calling code must be updated simultaneously
- Use consistent error message formatting across modules

## Function Parameter Order Standards
All functions MUST follow consistent parameter ordering to improve code readability and maintainability:

### Parameter Order Priority
Functions should order parameters according to the following hierarchy:

1. **Primary Operation Target**: The main object being modified or operated on (usually a state object that will be returned)
2. **Data Sources**: Read-only data inputs (e.g., `qa_data`, reference data)
3. **Configuration Parameters**: Settings and configuration objects (e.g., `config_state`)
4. **Additional Parameters**: Optional parameters, flags, and other inputs

### Examples of Correct Parameter Order
```r
# ✅ Good: Primary target first, then data, then config
learning_new_question <- function(learning_state, qa_data, config_state)
learning_update_probability <- function(learning_state, user_state, config_state)
learning_handle_feedback <- function(learning_state, qa_data, config_state, is_correct)

# ✅ Good: State object first for state modification functions  
state_update_user <- function(user_state, new_username, qa_data)
state_reset_learning <- function(learning_state)

# ✅ Good: Clear hierarchy of importance
data_save_user_score <- function(username, current_user, user_scores, qa_data)
```

### Benefits of Consistent Parameter Order
- **Improved Readability**: Function purpose is clear from parameter order
- **Reduced Cognitive Load**: Developers know what to expect
- **Better IDE Support**: Consistent auto-completion and parameter hints
- **Easier Refactoring**: Less confusion when modifying function calls
- **Code Consistency**: Uniform patterns across the entire codebase

### Implementation Guidelines
- **State Modification Functions**: Always put the state object being modified as the first parameter
- **Data Processing Functions**: Put the primary data being processed first
- **Configuration Parameters**: Always place configuration objects towards the end
- **Boolean Flags**: Place at the end unless they fundamentally change the function's operation

**IMPORTANT**: When creating new functions or refactoring existing ones, ALWAYS follow the namespaced naming convention and parameter order standards above. This is critical for maintainability and debugging.

## Shiny validate() Usage Rules
- **Primary use**: renderText(), renderTable(), reactive() contexts where validation messages are displayed in UI
- **In observeEvent()**:
  - ✅ Technically works but NOT recommended for user-facing validations
  - ❌ validate() error messages are NOT shown to users in observeEvent() context
  - ✅ Use explicit `if` statements with `showNotification()` for user feedback
  - Only use validate() in observeEvent() if you intentionally want silent failure without user notification
- **Recommended pattern for user-facing validation in observeEvent()**:
  ```r
  observeEvent(input$action, {
    if (validation_fails) {
      showNotification("Error message", type = "error", duration = NULL)
      return()
    }
    # ... proceed with operation
  })
  ```
- **Standard messages**: "Loading...", "Please [action]", "Data error: Cannot [action] - restart app"
- **Respect Shiny architecture**: Prefer patterns that provide clear user feedback
