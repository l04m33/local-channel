(define-module (guix-local-packages tic-80)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages cmake)
  #:export (tic-80))  


(define tic-80
  (package
   (name "tic-80")
   (version "v1.0.2164")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/nesbox/TIC-80.git")
                  (commit version)
                  (recursive? #t)))
            (file-name (git-file-name "tic-80" version))
            (sha256
             (base32
              "0xr1i2yv85whb4c9484sgvnsx3mx4zvwkliljx55qvi8ss2q0xja"))))
   (build-system trivial-build-system)
   (arguments `(#:modules ((guix build utils))
                #:builder
                (begin
                  (use-modules (guix build utils)
                               (ice-9 match))
                  (let* ((srcdir (assoc-ref %build-inputs "source"))
                         (outdir (assoc-ref %outputs "out"))
                         (tmpdir (getenv "TMPDIR"))
                         (build-srcdir (string-append tmpdir "/source"))
                         (build-dir (string-append build-srcdir "/build"))
                         (bin-outdir (string-append outdir "/bin")))
                    (copy-recursively srcdir build-srcdir)
                    (chdir build-dir)
                    (set-path-environment-variable
                      "PATH"
                      '("bin")
                      (map (match-lambda ((_ . input) input)) %build-inputs))
                    (invoke "cmake"
                            ".."
                            "-DCMAKE_BUILD_TYPE=Release"
                            (string-append "-DCMAKE_INSTALL_PREFIX=" outdir)
                            "-DCMAKE_INSTALL_LIBDIR=lib"
                            "-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE"
                            (string-append "-DCMAKE_INSTALL_RPATH=" outdir "/lib")
                            "-DCMAKE_VERBOSE_MAKEFILE=ON"
                            "-DBUILD_PRO=On")
                    (invoke "make" "-j8")
                    (install-file (string-append build-dir "/bin/tic80") bin-outdir)
                    #t))))
   (inputs (list coreutils
                 gcc-toolchain
                 glibc
                 cmake
                 ruby
                 glu
                 mesa
                 libglvnd
                 freeglut
                 alsa-lib))
   (synopsis "TIC-80 is a fantasy computer for making, playing and sharing tiny games.")
   (description "TIC-80 is a fantasy computer for making, playing and sharing tiny games.")
   (home-page "https://tic80.com/")
   (license license:expat))) ;; MIT
