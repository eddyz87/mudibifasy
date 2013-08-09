(in-package :md)

(defvar *url* "http://icfpc2013.cloudapp.net/")
(defvar *auth* nil)

(defun auth (auth-string)
  "sets current authentification string to the proveded command id"
  (setf *auth* auth-string))

(defun mk-url (path)
  (concatenate 'string *url* path "?" "auth=" *auth* "vpsH1H"))

(defun do-request (path)
  (assert *auth* () "Authentification string is not set, use 'auth' function")
  (let ((url (mk-url path)))
    (multiple-value-bind (reply status) (drakma:http-request url)
      (unless (eq status 200)
        (error "The request for ~A had failed:~%  status: ~A~%  reply: ~A" url status reply))
      reply)))

(defun json2lisp (json-string)
  (let ((yason:*parse-object-as* :alist))
    (yason:parse json-string)))

(defun problems ()
  "returns the list of available problems"
  (json2lisp (flexi-streams:octets-to-string (do-request "myproblems"))))
