(defpackage :pantry
  (:use :cl)
  (:local-nicknames (#:http :dex))
  (:import-from :jonathan #:to-json #:parse)
  (:export #:pantry-client
           #:make-pantry-client
           #:pantry-client-pantry-id
           #:pantry-client-base-url
           #:get-pantry-details
           #:update-pantry-details
           #:create-basket
           #:get-basket
           #:update-basket
           #:delete-basket
           #:pantry-error
           #:pantry-http-error
           #:pantry-error-message
           #:pantry-http-error-status-code
           #:pantry-http-error-response-body))

(in-package :pantry)

(define-condition pantry-error (error)
  ((message :initarg :message :reader pantry-error-message))
  (:report (lambda (condition stream)
             (format stream "Pantry error: ~A" (pantry-error-message condition)))))

(define-condition pantry-http-error (pantry-error)
  ((status-code :initarg :status-code :reader pantry-http-error-status-code)
   (response-body :initarg :response-body :reader pantry-http-error-response-body))
  (:report (lambda (condition stream)
             (format stream "Pantry HTTP error ~A: ~A~%Response: ~A"
                     (pantry-http-error-status-code condition)
                     (pantry-error-message condition)
                     (pantry-http-error-response-body condition)))))

(defstruct pantry-client
  "Pantry client structure holding configuration for API requests."
  pantry-id
  (base-url "https://getpantry.cloud/apiv1/pantry"))

(defun build-url (client &rest path-components)
  "Build a complete URL for the Pantry API."
  (format nil "~A/~A~{/~A~}"
          (pantry-client-base-url client)
          (pantry-client-pantry-id client)
          path-components))

(defun %json-key-string (key)
  "Convert a Lisp key to a JSON object key string. Symbols/keywords are
downcased by name; numbers and others are stringified."
  (cond
    ((stringp key) key)
    ((symbolp key) (string-downcase (symbol-name key)))
    (t (string-downcase (princ-to-string key)))))

(defun %key-like-p (k)
  (or (stringp k) (symbolp k) (characterp k) (numberp k)))

(defun %looks-like-plist-p (lst)
  "Heuristically determine if LST is a property list (even length, keys in odd positions)."
  (when (listp lst)
    (loop with ptr = lst
          while ptr
          for key = (car ptr)
          do (when (or (null (cdr ptr)) (not (%key-like-p key)))
               (return-from %looks-like-plist-p nil))
             (setf ptr (cddr ptr))
          finally (return t))))

(defun %alist-p (lst)
  "True if LST looks like an alist: list of cons cells with key-like cars."
  (and (listp lst)
       (every (lambda (x) (and (consp x) (%key-like-p (car x)))) lst)))

(defun normalize-json (value)
  "Normalize arbitrary Lisp data into a structure that `jonathan:to-json`
can reliably encode. Supports hash-tables, alists, plists, lists, vectors,
numbers, strings, booleans, and dotted pairs. Object keys are converted to
downcased strings. Unknown types are stringified."
  (labels ((norm (v)
             (cond
               ;; primitives
               ((or (null v) (stringp v) (eq v t)) v)
               ((typep v 'ratio) (coerce v 'double-float))
               ((numberp v) v)
               ((characterp v) (string v))
               ((symbolp v) (string-downcase (symbol-name v)))
               ((pathnamep v) (namestring v))
               ;; hash table
               ((hash-table-p v)
                (let ((out (make-hash-table :test 'equal)))
                  (maphash (lambda (k val)
                             (setf (gethash (%json-key-string k) out) (norm val)))
                           v)
                  out))
               ;; dotted/improper cons to single-entry object
               ((and (consp v) (not (listp (cdr v))))
                (let ((out (make-hash-table :test 'equal)))
                  (setf (gethash (%json-key-string (car v)) out) (norm (cdr v)))
                  out))
               ;; alists
               ((%alist-p v)
                (let ((out (make-hash-table :test 'equal)))
                  (dolist (pair v out)
                    (setf (gethash (%json-key-string (car pair)) out)
                          (norm (cdr pair))))))
               ;; plists
               ((%looks-like-plist-p v)
                (let ((out (make-hash-table :test 'equal)))
                  (loop for (k val) on v by #'cddr do
                        (setf (gethash (%json-key-string k) out) (norm val)))
                  out))
               ;; arrays / vectors to JSON array
               ((arrayp v)
                (map 'vector #'norm v))
               ;; proper list to JSON array
               ((listp v)
                (mapcar #'norm v))
               ;; fallback, stringify
               (t (princ-to-string v)))))
    (norm value)))

(defun content-to-json (content)
  "Convert arbitrary CONTENT into a JSON string using `normalize-json`."
  (to-json (normalize-json content)))

(defun ensure-json-object (data)
  "Normalize DATA and ensure a JSON object (hash-table) result. If DATA
normalizes to a non-object (array or primitive), wrap it as
{\"value\": <normalized>}."
  (let ((n (normalize-json data)))
    (if (hash-table-p n)
        n
        (let ((ht (make-hash-table :test 'equal)))
          (setf (gethash "value" ht) n)
          ht))))

(defun parse-json-response (response)
  "Parse JSON response or return as-is if it's already parsed or a string."
  (typecase response
    (string (if (and (> (length response) 0)
                     (or (char= (char response 0) #\{)
                         (char= (char response 0) #\[)))
                (parse response :as  :hash-table)
                response))
    (t response)))

(defun make-request (method url &key content)
  "Make HTTP request and handle errors."
  (handler-case
      (multiple-value-bind (body status-code response-headers uri stream)
          (http:request url
                        :method method
                        :headers '(("Content-Type" . "application/json"))
                        :content (when content (content-to-json content)))
        (declare (ignore response-headers uri stream))
        (if (< status-code 400)
            (parse-json-response body)
            (error 'pantry-http-error
                   :status-code status-code
                   :message (format nil "HTTP request failed")
                   :response-body body)))
    (http:http-request-failed (e)
      (error 'pantry-http-error
             :status-code (http:response-status e)
             :message "HTTP request failed"
             :response-body (http:response-body e)))
    (error (e)
      (error 'pantry-error
             :message (format nil "Request failed: ~A" e)))))

(defun get-pantry-details (client)
  "Get details of the pantry including baskets and their TTL."
  (let ((url (build-url client)))
    (make-request :get url)))

(defun update-pantry-details (client &key name description)
  "Update pantry name and/or description."
  (let ((url (build-url client))
        (content (make-hash-table :test 'equal)))
    (when name (setf (gethash "name" content) name))
    (when description (setf (gethash "description" content) description))
    (when (> (hash-table-count content) 0)
      (make-request :put url :content content))))

(defun create-basket (client basket-name data)
  "Create or replace a basket with the given data."
  (let ((url (build-url client "basket" basket-name)))
    (make-request :post url :content (ensure-json-object data))))

(defun get-basket (client basket-name)
  "Get the contents of a basket."
  (let ((url (build-url client "basket" basket-name)))
    (make-request :get url)))

(defun update-basket (client basket-name data)
  "Update basket contents (merges with existing data)."
  (let ((url (build-url client "basket" basket-name)))
    (make-request :put url :content (ensure-json-object data))))

(defun delete-basket (client basket-name)
  "Delete a basket entirely."
  (let ((url (build-url client "basket" basket-name)))
    (make-request :delete url)))
