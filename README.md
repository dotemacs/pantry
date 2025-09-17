# `pantry`

A Common Lisp client library for the [Pantry JSON storage
service](https://getpantry.cloud).

## Usage

Pantry, stores JSON data in objects which it calls `baskets`.

You can store JSON data in them like so:

```lisp
CL-USER> (pantry:create-basket *client* "user-settings"
                      '(("theme" . "dark")
                        ("language" . "en")
                        ("notifications" . t)
                        ("user-id" . 12345)))
"Your Pantry was updated with basket: user-settings!"
```

Or get the data out:

```lisp
(pantry:get-basket *client* "user-settings")
;; => a hash-table
```

You can also update your basket:

```lisp
(pantry:update-basket *client* "user-settings"
                      '(("theme" . "light")
                        ("last-login" . "2023-12-01")))
;; => merged basket contents as a hash-table
```
which merges your existing data with the new data.

Or you can delete it:

```lisp
(pantry:delete-basket *client* "user-settings")
;; Returns: "user-settings was removed from your Pantry!"
```

The place where all these baskets sit within Pantry, the service, is
called pantry, the object, which you can look up:

```lisp
(gethash "baskets" (pantry:get-pantry-details *client*))
```

To make it easier to work with data, You can pass many Common Lisp
data structures and they’ll be converted to JSON automatically. Basket
payloads must be JSON objects and you pass a non-object (like a list
or vector), it is wrapped as {"value": ...}  to satisfy the API.


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
(defparameter *client* (pantry:make-pantry-client "your-pantry-id-here"))
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

### Data structures that you can use

#### Hash-table
```lisp
(let ((ht (make-hash-table :test 'equal)))
  (setf (gethash :theme ht) :dark
        (gethash "language" ht) "en"
        (gethash 'notifications ht) t)
  (pantry:create-basket *client* "ht-basket" ht))
```

#### Plist -> JSON object
```lisp
(pantry:create-basket *client* "plist-basket"
                      '(:theme :dark :language "en" :notifications t))
```

#### Vector / list wrapped under "value"
```lisp
(pantry:create-basket *client* "array-basket" #(1 2 3 4))
;; Sends {"value": [1,2,3,4]}

(pantry:create-basket *client* "list-basket"  '(:a :b :c))
;; Sends {"value": ["a","b","c"]}
```

#### Dotted pair -> single-entry object
```lisp
(pantry:create-basket *client* "pair-basket"  '(foo . 42))
```

#### Nested structures are fine too

```lisp
(pantry:create-basket *client* "nested"
  '((user . ((id . 1) (name . "Ada")))
    (prefs . (:theme :dark :langs #("en" "fr")))))
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

- `pantry-client` - Main client structure containing pantry-id and base-url

### Functions

- `make-pantry-client (pantry-id)` - Create a new client instance
- `get-pantry-details (client)` - Get pantry information
- `update-pantry-details (client &key name description)` - Update pantry metadata
- `create-basket (client basket-name data)` - Create or replace a basket
- `get-basket (client basket-name)` - Get basket contents
- `update-basket (client basket-name data)` - Update basket (merge)
- `delete-basket (client basket-name)` - Delete a basket

## License

BSD License
