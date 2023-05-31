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
  #:export (tic-80))


(define tic-80
  (package
   (name "tic-80")
   (version "v1.1.0-dev-20230601")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/nesbox/TIC-80.git")
                  (commit "5dd72cf0")
                  (recursive? #t)))
            (file-name (git-file-name "tic-80" version))
            (sha256
             (base32
              "1xr1i2yv85whb4c9484sgvnsx3mx4zvwkliljx55qvi8ss2q0xja"))))
   (build-system cmake-build-system)
   (arguments `(#:build-type "Release"
                #:configure-flags
                ("-DBUILD_WITH_JANET=TRUE"
                 "-DBUILD_PRO=ON")))
   ;; (arguments `(#:tests? #f
   ;;              #:phases (modify-phases %standard-phases
   ;;                         (replace 'configure
   ;;                           (lambda _
   ;;                             (let* ((tmpdir (getenv "TMPDIR"))
   ;;                                    (build-srcdir (string-append tmpdir "/source"))
   ;;                                    (build-dir (string-append build-srcdir "/build"))
   ;;                                    (outdir (assoc-ref %outputs "out")))
   ;;                               (chdir build-dir)
   ;;                               (invoke "cmake"
   ;;                                       ".."
   ;;                                       "-DCMAKE_BUILD_TYPE=Release"
   ;;                                       (string-append "-DCMAKE_INSTALL_PREFIX=" outdir)
   ;;                                       "-DCMAKE_INSTALL_LIBDIR=lib"
   ;;                                       "-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE"
   ;;                                       (string-append "-DCMAKE_INSTALL_RPATH=" outdir "/lib")
   ;;                                       "-DCMAKE_VERBOSE_MAKEFILE=ON"
   ;;                                       "-DBUILD_PRO=On")))))))
   (inputs (list pkg-config
                 ruby
                 python-wrapper
                 glu
                 mesa
                 libxcursor
                 libglvnd
                 freeglut
                 pulseaudio))
   (synopsis "TIC-80 is a fantasy computer for making, playing and sharing tiny games.")
   (description "TIC-80 is a fantasy computer for making, playing and sharing tiny games.")
   (home-page "https://tic80.com/")
   (license license:expat))) ;; MIT
