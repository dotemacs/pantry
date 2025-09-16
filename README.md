# `pantry`

A Common Lisp client library for the [Pantry JSON storage
service](https://getpantry.cloud), providing simple cloud-based JSON
storage for small projects and prototypes.

## Usage

Pantry, the service, has two places to store data:

* `pantry`
* `baskets`

`pantry` can be thought of as a global namespace, where only `name` &
`description` can be stored.

`baskets` are documents for key/values to be stored. This is what you
should use.

## Features

- Flexible JSON input: pass alists, plists, hash-tables, lists, or
  vectors
- Responses parsed to native types (hash-tables for objects,
  vectors/lists for arrays)

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
;; Single-file system: just load pantry.lisp
(load "/path/to/pantry/pantry.lisp")
```

### 3. Get Your Pantry ID

Visit [getpantry.cloud](https://getpantry.cloud) to get your free pantry ID.

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
;; => a hash-table mapping string keys to values
;; Example access:
(let ((details (pantry:get-pantry-details *client*)))
  (gethash "baskets" details))
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
CL-USER> (pantry:create-basket *client* "user-settings"
                      '(("theme" . "dark")
                        ("language" . "en")
                        ("notifications" . t)
                        ("user-id" . 12345)))
"Your Pantry was updated with basket: user-settings!"
```

You can now pass many common Lisp shapes and they’ll be normalized to
JSON automatically. Basket payloads must be JSON objects; if you pass
a non-object (like a list or vector), it is wrapped as {"value": ...}
to satisfy the API.

```lisp
;; Hash-table
(let ((ht (make-hash-table :test 'equal)))
  (setf (gethash :theme ht) :dark
        (gethash "language" ht) "en"
        (gethash 'notifications ht) t)
  (pantry:create-basket *client* "ht-basket" ht))

;; Plist -> JSON object
(pantry:create-basket *client* "plist-basket"
                      '(:theme :dark :language "en" :notifications t))

;; Vector / list wrapped under "value"
(pantry:create-basket *client* "array-basket" #(1 2 3 4))
;; Sends {"value": [1,2,3,4]}

(pantry:create-basket *client* "list-basket"  '(:a :b :c))
;; Sends {"value": ["a","b","c"]}

;; Dotted pair -> single-entry object
(pantry:create-basket *client* "pair-basket"  '(foo . 42))

;; Nested structures are fine too
(pantry:create-basket *client* "nested"
  '((user . ((id . 1) (name . "Ada")))
    (prefs . (:theme :dark :langs #("en" "fr")))))
```

#### Get Basket Contents
Retrieve data from a basket:

```lisp
(pantry:get-basket *client* "user-settings")
;; => a hash-table
(let ((ht (pantry:get-basket *client* "user-settings")))
  (multiple-value-bind (theme presentp) (gethash "theme" ht)
    (when presentp theme)))
```

#### Update Basket (Merge)
Update specific keys in a basket (merges with existing data):

```lisp
(pantry:update-basket *client* "user-settings"
                      '(("theme" . "light")
                        ("last-login" . "2023-12-01")))
;; => merged basket contents as a hash-table
```

`update-basket` accepts the same flexible inputs as `create-basket`
and applies the same wrapping rule.

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

### JSON Normalization Rules

- Object keys are converted to downcased strings (from
  strings/symbols/keywords/numbers).
- Hash-tables, alists, plists, and dotted pairs normalize to JSON
  objects.
- Vectors and proper lists normalize to JSON arrays.
- For basket payloads, non-objects are wrapped as `{ "value": ... }`
  to satisfy the API.
- Symbols/keywords as values encode as their downcased names;
  characters as 1-char strings; rationals as floats; pathnames as
  namestrings; other unknown types are stringified.

### Conditions

- `pantry-error` - Base error condition
- `pantry-http-error` - HTTP-specific error with status code


## License

BSD License
