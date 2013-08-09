(asdf:defsystem :mudibifasy
  :depends-on (:drakma :yason)
  :components ((:file "src/packages")
               (:file "src/http" :depends-on ("src/packages"))
               (:file "src/help" :depends-on ("src/http" "src/packages"))))

(asdf:load-system :mudibifasy)
