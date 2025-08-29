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

(defun alist-to-hash-table (alist)
  "Convert an alist to a hash table with string keys for JSON serialization."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (pair alist ht)
      (setf (gethash (if (stringp (car pair))
                         (car pair)
                         (string-downcase (string (car pair))))
                     ht)
            (cdr pair)))))

(defun parse-json-response (response)
  "Parse JSON response or return as-is if it's already parsed or a string."
  (typecase response
    (string (if (and (> (length response) 0)
                     (or (char= (char response 0) #\{)
                         (char= (char response 0) #\[)))
                (jonathan:parse response :as :alist)
                response))
    (t response)))

(defun make-request (method url &key content)
  "Make HTTP request and handle errors."
  (handler-case
      (multiple-value-bind (body status-code response-headers uri stream)
          (dexador:request url
                          :method method
                          :headers '(("Content-Type" . "application/json"))
                          :content (when content (jonathan:to-json content)))
        (declare (ignore response-headers uri stream))
        (if (< status-code 400)
            (parse-json-response body)
            (error 'pantry-http-error
                   :status-code status-code
                   :message (format nil "HTTP request failed")
                   :response-body body)))
    (dexador:http-request-failed (e)
      (error 'pantry-http-error
             :status-code (dexador:response-status e)
             :message "HTTP request failed"
             :response-body (dexador:response-body e)))
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
  (let ((url (build-url client "basket" basket-name))
        (content (if (hash-table-p data)
                     data
                     (alist-to-hash-table data))))
    (make-request :post url :content content)))

(defun get-basket (client basket-name)
  "Get the contents of a basket."
  (let ((url (build-url client "basket" basket-name)))
    (make-request :get url)))

(defun update-basket (client basket-name data)
  "Update basket contents (merges with existing data)."
  (let ((url (build-url client "basket" basket-name))
        (content (if (hash-table-p data)
                     data
                     (alist-to-hash-table data))))
    (make-request :put url :content content)))

(defun delete-basket (client basket-name)
  "Delete a basket entirely."
  (let ((url (build-url client "basket" basket-name)))
    (make-request :delete url)))
