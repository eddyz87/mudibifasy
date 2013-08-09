(asdf:defsystem :mudibifasy
  :depends-on (:drakma :yason)
  :components ((:file "src/http")))

(asdf:load-system :mudibifasy)
