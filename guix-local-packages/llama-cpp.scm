(define-module (guix-local-packages llama-cpp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:export (llama-cpp))


(define clblast
  (let ((commit "9535155ad8a5c3254900329aeba4f16910ab5278")
        (revision "563"))
    (package
     (name "clblast")
     (version (git-version "1.2.0_rc1" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/CNugteren/CLBlast")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y7q21cacvg5czx05zr3gqp9v49z04cwsd8v7is168h46yk8d2r5"))))
     (build-system cmake-build-system)
     (arguments
      (list
       #:build-type "Release"
       #:tests? #f
       #:configure-flags
       #~(list)))
     (inputs (list
              opencl-icd-loader
              opencl-headers
              pkg-config
              git))
     (home-page "https://github.com/CNugteren/CLBlast")
     (synopsis "The tuned OpenCL BLAS library")
     (description "CLBlast is a lightweight, performant and tunable OpenCL BLAS library written in C++11.")
     (license license:asl2.0))))


(define llama-cpp
  (let ((commit "97c1549808d2742d37584a3c9df28154bdf34417")
        (revision "624"))
    (package
     (name "llama-cpp")
     (version (git-version "0.4.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ggerganov/llama.cpp")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zpad37nw6wcjfbfbdw00iciv4a4yj955nwbxq5gdx5h44cpxncw"))))
     (build-system cmake-build-system)
     (arguments
      (list
       #:build-type "Release"
       #:tests? #f
       #:configure-flags
       #~(list
          "-DLLAMA_CLBLAST=ON"
          (string-append "-DCLBlast_DIR=" #$clblast))))
     (inputs (list
              opencl-headers
              pkg-config
              git))
     (propagated-inputs (list
                         opencl-icd-loader
                         clblast))
     (home-page "https://github.com/ggerganov/llama.cpp")
     (synopsis "Port of Facebook's LLaMA model in C/C++")
     (description "This package provides a port to Facebook's LLaMA collection
of foundation language models.  It requires models parameters to be downloaded
independently to be able to run a LLaMA model.")
     (license license:expat))))
