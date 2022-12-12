(define-module (guix-local-packages zig)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libffi)
  #:export (zig
            llvm-for-zig
            zls))


(define-public llvm-for-zig
  (package
    (name "llvm-for-zig")
    (version "15.0.6")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/llvm/llvm-project")
            (commit (string-append "llvmorg-" version))))
      (file-name (git-file-name "llvm-project" version))
      (sha256 (base32 "13vj0q6if5095a3awq8d97l17qdsc7fks6avkai9k80kl0kzxpv6"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         ;; Flags from https://github.com/ziglang/zig/wiki/How-to-build-LLVM,-libclang,-and-liblld-from-source
         "-DLLVM_ENABLE_PROJECTS=lld;clang"
         "-DLLVM_ENABLE_LIBXML2=OFF"
         "-DLLVM_ENABLE_TERMINFO=OFF"
         "-DLLVM_ENABLE_LIBEDIT=OFF"
         "-DLLVM_ENABLE_ASSERTIONS=ON"
         "-DLLVM_PARALLEL_LINK_JOBS=1") ;cater to smaller build machines
      ;; Don't use '-g' during the build, to save space.
      #:build-type "Release"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'change-directory
            (lambda _
              (chdir "llvm"))))))
    (native-inputs (list python-wrapper perl))
    (inputs (list libffi))
    (propagated-inputs (list zlib))     ;to use output from llvm-config
    (home-page "https://www.llvm.org")
    (synopsis "Optimizing compiler infrastructure")
    (description
     "LLVM is a compiler infrastructure designed for compile-time, link-time,
runtime, and idle-time optimization of programs from arbitrary programming
languages.  It currently supports compilation of C and C++ programs, using
front-ends derived from GCC 4.0.1.  A new front-end for the C family of
languages is in development.  The compiler infrastructure includes mirror sets
of programming tools as well as libraries with equivalent functionality.")
    (license license:asl2.0)))


(define zig
  (package
    (name "zig")
    (version "0.11.x-20221210-d10fd78d4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ziglang/zig.git")
                     (commit "d10fd78d4")
                     (recursive? #t)))
              (sha256
               (base32
                "0829wymcwph71zlwql6v7i7j9gr1m96acyp2xsr69vq2h98wmlap"))
              (patches
               (parameterize
                   ((%patch-path
                     (map (lambda (directory)
                            (string-append directory "/guix-local-packages/patches/"))
                          %load-path)))
                 (search-patches "fix_dyn_linker_locator.patch")))))
    (build-system cmake-build-system)
    (arguments '(#:configure-flags '()
                 #:build-type "Release"
                 #:tests? #f
                 #:phases (modify-phases %standard-phases
                            (add-before ':configure 'fix-zig-cache-dir
                              (lambda _
                                (let ((cache-dir (string-append (getenv "TMPDIR") "/cache")))
                                  (mkdir cache-dir)
                                  (setenv "XDG_CACHE_HOME" cache-dir))
                                #t)))))
    (inputs `(("llvm-for-zig" ,llvm-for-zig)
              ("zlib" ,zlib)))
    (synopsis
     "A general-purpose programming language and toolchain.")
    (description
     "Zig is a general-purpose programming language and toolchain for
maintaining robust, optimal, and reusable software.")
    (home-page "https://ziglang.org")
    (license license:expat))) ;; MIT


(define zls
  (package
    (name "zls")
    (version "20221208-9e658cdbb")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/zigtools/zls.git")
                     (commit "9e658cdbb")
                     (recursive? #t)))
              (sha256
               (base32
                "979mk1ii6nzwlzcjv9vwxla4gpiqkjljvwd31qcn9y14sbvajbqy"))))
    (build-system trivial-build-system)
    (arguments `(#:modules ((guix build utils))
                 #:builder
                 (begin
                   (use-modules (guix build utils))
                   (let* ((srcdir (assoc-ref %build-inputs "source"))
                          (outdir (assoc-ref %outputs "out"))
                          (tmpdir (getenv "TMPDIR"))
                          (build-srcdir (string-append tmpdir "/source"))
                          (bin-outdir (string-append outdir "/bin"))
                          (zig-out (assoc-ref %build-inputs "zig")))
                     (copy-recursively srcdir build-srcdir)
                     (chdir build-srcdir)
                     ;; The global zig-cache needs this to be set.
                     (setenv "XDG_CACHE_HOME" tmpdir)
                     (set-path-environment-variable "PATH" '("bin") `(,zig-out))
                     (invoke "zig"
                             "build"
                             "-Drelease-safe")
                     (install-file (string-append build-srcdir "/zig-out/bin/zls") bin-outdir)
                     #t))))
    (propagated-inputs `(("zig" ,zig)))
    (synopsis "The Zig Language Server")
    (description "")
    (home-page "https://github.com/zigtools/zls")
    (license license:expat))) ;; MIT
