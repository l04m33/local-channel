(define-module (guix-local-packages tic-80)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages python)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages sdl)
  #:export (tic-80))


(define tic-80
  (package
   (name "tic-80")
   (version "v1.1.0-dev-20230606")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/nesbox/TIC-80.git")
                  (commit "f5f3c15e")
                  (recursive? #t)))
            (file-name (git-file-name "tic-80" version))
            (sha256
             (base32
              "1gcsai47cvk8cxrfcklbyg8qgxnzbpcc8hi60sgv3kpjr1xz0jrg"))))
   (build-system cmake-build-system)
   (arguments (list
               #:build-type "Release"
               #:tests? #f
               #:configure-flags
               #~(list
                  "-DBUILD_WITH_JANET=TRUE"
                  "-DBUILD_PRO=ON")
               #:phases
               #~(modify-phases
                  %standard-phases
                  (add-before 'configure 'set-cc-for-janet (lambda _ (setenv "CC" "gcc"))))))
   (propagated-inputs (list libx11
                            libxext
                            libxcursor
                            mesa
                            alsa-lib
                            pulseaudio))
   (inputs (list pkg-config
                 openssl
                 ruby
                 python-wrapper
                 glu
                 libglvnd
                 freeglut))
   (synopsis "TIC-80 is a fantasy computer for making, playing and sharing tiny games.")
   (description "TIC-80 is a fantasy computer for making, playing and sharing tiny games.")
   (home-page "https://tic80.com/")
   (license license:expat))) ;; MIT
