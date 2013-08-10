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
    (labels ((%try-do-request ()
	       (multiple-value-bind (reply status) (drakma:http-request
						    url
						    :method :POST
						    :content post-json-param)
		 (case status
		   (200 (if (stringp reply)
			    reply
			    (flexi-streams:octets-to-string reply)))
		   (429 (warn "Response: try later again. Sleeping for a second...")
			(sleep 1)
			(%try-do-request))
		   (otherwise 
		    (error "The request for ~A had failed:~%  status: ~A~%  reply: ~A" url status reply))))))
      (%try-do-request))))

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

(defun encode-arguments (arguments)
  "converts lisp integers to strings with radix 16
     arguments - list of the arguments (integer numbers)"
  (mapcar (lambda (arg)
	    (concatenate 'string
			 "0x"
			 (write-to-string arg :base 16)))
	  arguments))

(defun decode-arguments (arguments)
  "converts strings to list integers
     arguments - list of strings"
  (mapcar (lambda (arg)
	    (parse-integer arg :start 2 :radix 16))
	  arguments))

(defun request-eval (arguments &key id program)
  "sends an eval request, parameters:
     arguments - list of the arguments (integer numbers)
     &key:
       id - string representing id of the problem
       program - string representing a program to evaluate
   either id or program have to be present"
  (labels ((%find-in-response (key response)
	     (cdr (assoc key response :test #'string=))))
    (unless (and (or id program)
		 (not (and id program)))
      (error "Either :id or :program have to be specified, but not both"))
    (let* ((encoded-args (encode-arguments arguments))
	   (request (lisp2json
		     (cons
		      (if id
			  (cons "id" id)
			  (cons "program" program))
		      (list
		       (cons "arguments" encoded-args)))))
	   (response (json2lisp (do-request "eval" request)))
	   (status (%find-in-response "status" response)))
      (if (string= status "ok")
	  (decode-arguments (%find-in-response "outputs" response))
	  (error "~A" (%find-in-response "message" response))))))
	

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
