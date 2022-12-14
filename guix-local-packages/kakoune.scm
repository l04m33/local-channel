(define-module (guix-local-packages kakoune)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:export (kakoune))


(define-public kakoune
  (package
    (name "kakoune")
    (version "2022.10.31")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mawww/kakoune/")
             (commit (string-append "v" version))))
       (file-name (git-file-name "kakoune" version))
       (sha256
        (base32 "0ycxhmb9dzckzlpy4yr5c1719wy6syhb7zr0g7n2habld5lccv5y"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
                      (delete 'configure)
                      (add-before 'check 'fix-shell-location
                                  (lambda _
                                    (setenv "KAKOUNE_POSIX_SHELL" (which "sh")))))))
    (native-inputs (list pkg-config))
    (synopsis "Vim-inspired code editor")
    (description
     "Kakoune is a code editor heavily inspired by Vim, as such most of its
commands are similar to Vi's ones, and it shares Vi's \"keystrokes as a text
editing language\" model.  Kakoune has a strong focus on interactivity, most
commands provide immediate and incremental results, while still being
competitive (as in keystroke count) with Vim.")
    (home-page "https://kakoune.org/")
    (license license:unlicense)))
