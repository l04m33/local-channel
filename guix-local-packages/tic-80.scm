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
  #:use-module (gnu packages tls)
  #:export (tic-80))


(define tic-80
  (package
   (name "tic-80")
   (version "v1.1.2736")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/nesbox/TIC-80.git")
                  (commit version)
                  (recursive? #t)))
            (file-name (git-file-name "tic-80" version))
            (sha256
             (base32
              "1iqb48n1172m24649vyidbdy9iv9hj992whp9xhzbabcww4vbwy2"))))
   (build-system cmake-build-system)
   (arguments (list
               #:build-type "Release"
               #:tests? #f
               #:configure-flags
               #~(list
                  ;; SDL calls dlopen to locate backends at runtime.
                  ;; Make sure it can find them.
                  (string-append
                   "-DCMAKE_INSTALL_RPATH="
                   #$output "/lib;"
                   #$libx11 "/lib;"
                   #$libxext "/lib;"
                   #$libxcursor "/lib;"
                   #$mesa "/lib;"
                   #$alsa-lib "/lib;"
                   #$pulseaudio "/lib")
                  "-DBUILD_PRO=ON")
               #:phases
               #~(modify-phases
                  %standard-phases
                  (add-before 'configure 'set-cc-for-janet (lambda _ (setenv "CC" "gcc"))))))
   ;; All things in RUNPATH should be in propagated-inputs too,
   ;; or `guix gc` would purge them.
   (propagated-inputs (list libx11
                            libxext
                            libxcursor
                            mesa
                            alsa-lib
                            pulseaudio))
   (inputs (list pkg-config
                 ruby
                 python-wrapper
                 glu
                 libglvnd
                 freeglut))
   (synopsis "TIC-80 is a fantasy computer for making, playing and sharing tiny games.")
   (description "TIC-80 is a fantasy computer for making, playing and sharing tiny games.")
   (home-page "https://tic80.com/")
   (license license:expat))) ;; MIT
