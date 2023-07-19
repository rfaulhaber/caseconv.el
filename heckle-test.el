;; (require 'heckle)
(require 'cl-lib)
(require 'ert)

(ert-deftest heckle-kebab ()
  (should (string= (heckle-to-kebab "CamelCase") "camel-case")))
