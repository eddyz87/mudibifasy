(in-package :md)

(defvar *url* "http://icfpc2013.cloudapp.net/")
(defvar *auth* nil)

(defun auth (auth-string)
  "sets current authentification string to the proveded command id"
  (setf *auth* auth-string))

(defun mk-url (path)
  (concatenate 'string *url* path "?" "auth=" *auth* "vpsH1H"))

(defun do-request (path &optional post-json-param)
  (assert *auth* () "Authentification string is not set, use 'auth' function")
  (let ((url (mk-url path)))
    (multiple-value-bind (reply status) (drakma:http-request
                                          url
                                          :method :POST
                                          :content post-json-param)
      (unless (eq status 200)
        (error "The request for ~A had failed:~%  status: ~A~%  reply: ~A" url status reply))
      (if (stringp reply)
          reply
          (flexi-streams:octets-to-string reply)))))

(defmacro with-yason (&body body)
  `(let ((yason:*parse-object-as* :alist))
    ,@body))

(defun json2lisp (json-string)
  (with-yason
    (yason:parse json-string)))

(defun lisp2json (alist)
  (with-yason
    (with-output-to-string (stream)
      (yason:encode-alist alist stream))))

(defun problems ()
  "returns the list of available problems"
  (json2lisp (do-request "myproblems")))

(defun request-eval (arguments &key id program)
  "sends an eval request, parameters:
     arguments - list of the arguments (integer numbers)
     &key:
       id - string representing id of the problem
       program - string representing a program to evaluate
   either id or program have to be present"
  (unless (and (or id program)
              (not (and id program)))
    (error "Either :id or :program have to be specified, but not both"))
  (let ((request (lisp2json
                   (cons
                     (if id
                       (cons "id" id)
                       (cons "program" program))
                     (list
                       (cons "arguments" arguments))))))
    (json2lisp (do-request "eval" request))))

(defun guess (id program)
  "sends a guess request, parameters:
    id - string representing the id of the problem
    program - string representing a program"
  (json2lisp (do-request
               "guess"
               (lisp2json (acons "id" id (acons "program" program nil))))))

(defun train (&key size operators)
  "sends a train request, paramters:
    &key
      size - integer describing desired complexity
      operators - strings designating required operators"
  (json2lisp (do-request "train" (lisp2json
                                   (remove
                                     nil
                                     (list
                                       (when size (cons "size" size))
                                       (when operators (cons "operators" operators))))))))

(defun status ()
  "sends a status request"
  (json2lisp (do-request "status")))
