(asdf:defsystem :mudibifasy
  :depends-on (:drakma :yason :optima)
  :components ((:file "src/packages")
               (:file "src/http"    :depends-on ("src/packages"))
               (:file "src/help"    :depends-on ("src/http" "src/packages"))
               (:file "src/bv"      :depends-on ("src/packages"))
               (:file "src/choose"  :depends-on ("src/packages"))
               (:file "src/glue"    :depends-on ("src/packages" "src/http"))
               (:file "src/solve"   :depends-on ("src/choose" "src/glue"))))

(asdf:load-system :mudibifasy)
