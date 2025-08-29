# Pantry Common Lisp Client

A Common Lisp client library for the [Pantry JSON storage
service](https://getpantry.cloud), providing simple cloud-based JSON
storage for small projects and prototypes.

## Features

- Simple API for storing and retrieving JSON data
- Support for multiple "baskets" within a pantry
- Lightweight struct-based client
- Built-in error handling with custom conditions
- Uses `dexador` for HTTP requests and `jonathan` for JSON parsing
- No authentication required - just use your pantry ID

## Installation

### 1. Install Dependencies

```lisp
(ql:quickload '(:dexador :jonathan))
```

### 2. Load the Library

**Option A: Using ASDF (recommended)**
```lisp
;; Add the pantry directory to ASDF's search path
(push #P"/path/to/pantry/" asdf:*central-registry*)

;; Load the system
(asdf:load-system :pantry)
```

**Option B: Manual loading**
```lisp
(load "/path/to/pantry/package.lisp")
(load "/path/to/pantry/pantry.lisp")
```

### 3. Get Your Pantry ID

Visit [getpantry.cloud](https://getpantry.cloud) to get your free pantry ID.

## Dependencies

- `dexador` - HTTP client
- `jonathan` - JSON parser

## Usage

### Creating a Client

```lisp
(defparameter *client* (pantry:make-pantry-client :pantry-id "your-pantry-id-here"))
```

### Pantry Operations

#### Get Pantry Details
Get information about your pantry, including available baskets:

```lisp
(pantry:get-pantry-details *client*)
;; Returns:
;; (("name" . "My Pantry")
;;  ("description" . "Storage for my app")
;;  ("errors" . #())
;;  ("notifications" . t)
;;  ("percentFull" . 1)
;;  ("baskets" . ((("name" . "user-data") ("ttl" . 258915)))))
```

#### Update Pantry Details
Update your pantry's name and description:

```lisp
(pantry:update-pantry-details *client*
                              :name "My Updated Pantry"
                              :description "New description")
```

### Basket Operations

#### Create or Replace Basket
Store JSON data in a named basket:

```lisp
(pantry:create-basket *client* "user-settings"
                      '(("theme" . "dark")
                        ("language" . "en")
                        ("notifications" . t)
                        ("user-id" . 12345)))
;; Returns: "Your Pantry was updated with basket: user-settings!"
```

#### Get Basket Contents
Retrieve data from a basket:

```lisp
(pantry:get-basket *client* "user-settings")
;; Returns:
;; (("theme" . "dark")
;;  ("language" . "en")
;;  ("notifications" . t)
;;  ("user-id" . 12345))
```

#### Update Basket (Merge)
Update specific keys in a basket (merges with existing data):

```lisp
(pantry:update-basket *client* "user-settings"
                      '(("theme" . "light")
                        ("last-login" . "2023-12-01")))
;; Returns the merged basket contents
;;(("last-login" . "2023-12-01") ("user-id" . 12345) ("notifications" . T)
;;                               ("language" . "en") ("theme" . "light"))
```

#### Delete Basket
Remove a basket entirely:

```lisp
(pantry:delete-basket *client* "user-settings")
;; Returns: "user-settings was removed from your Pantry!"
```

## Error Handling

The library defines custom conditions for error handling:

```lisp
;; Catch all pantry errors
(handler-case
    (pantry:get-basket *client* "non-existent-basket")
  (pantry:pantry-error (e)
    (format t "Error: ~A~%" (pantry:pantry-error-message e))))

;; Catch HTTP-specific errors
(handler-case
    (pantry:create-basket *client* "test" invalid-data)
  (pantry:pantry-http-error (e)
    (format t "HTTP ~A: ~A~%"
            (pantry:pantry-http-error-status-code e)
            (pantry:pantry-error-message e))))
```

## API Reference

### Structures

- `pantry-client` - Main client structure containing pantry-id and base-url

### Functions

- `make-pantry-client (pantry-id)` - Create a new client instance
- `get-pantry-details (client)` - Get pantry information
- `update-pantry-details (client &key name description)` - Update pantry metadata
- `create-basket (client basket-name data)` - Create or replace a basket
- `get-basket (client basket-name)` - Get basket contents
- `update-basket (client basket-name data)` - Update basket (merge)
- `delete-basket (client basket-name)` - Delete a basket

### Conditions

- `pantry-error` - Base error condition
- `pantry-http-error` - HTTP-specific error with status code

## Example: Simple Key-Value Store

```lisp
(defparameter *storage* (pantry:make-pantry-client "your-pantry-id"))

;; Inspect the client structure
(pantry:pantry-client-pantry-id *storage*)     ;; => "your-pantry-id"
(pantry:pantry-client-base-url *storage*)      ;; => "https://getpantry.cloud/apiv1/pantry"

(defun store-value (key value)
  "Store a key-value pair"
  (pantry:create-basket *storage* key (list (cons "value" value)
                                           (cons "timestamp" (get-universal-time)))))

(defun get-value (key)
  "Retrieve a value by key"
  (let ((basket (pantry:get-basket *storage* key)))
    (cdr (assoc "value" basket :test #'string=))))

;; Usage
(store-value "user-123-preferences" '(("theme" . "dark") ("lang" . "en")))
(get-value "user-123-preferences")
```

## License

BSD License

## About Pantry

Pantry is a free JSON storage service perfect for:
- Prototypes and hackathon projects
- Small personal applications
- Temporary data storage
- Configuration management

Data is stored temporarily and expires after periods of inactivity. For production applications, consider using a dedicated database service.
