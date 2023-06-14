(define-module (guix-local-packages kotlin)
  #:use-module (ice-9 match)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages base)
  #:use-module (gnu packages java))

(define-public kotlin-compiler
  (package
    (name "kotlin-compiler")
    (version "1.8.22")
    (source
     (origin
      (method url-fetch/zipbomb)
      (uri (string-append "https://github.com/JetBrains/kotlin/releases/download/v"
                          version
                          "/kotlin-compiler-"
                          version
                          ".zip"))
      (sha256
       (base32 "19psrm905r7fli27cn5hykvjhizshpg2xzp1kbkv3pwybki0zxci"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan `(("./kotlinc" "."))))
    ;; kotlinc-jvm needs the java command to run
    (propagated-inputs
     `(("openjdk:jdk" ,openjdk)))
    (home-page "https://kotlinlang.org/")
    (synopsis "Kotlin compiler for the command line")
    (description "Kotlin compiler for the command line")
    (license license:expat))) ;; XXX: the source code used multiple licenses
