(define-module (guix-local-packages llama-cpp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages pkg-config)
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
        (base32 "0ys6n53n032zq1ll9f3vgxk8sw0qq7x3fi7awsyy13adzp3hn08p"))))
     (build-system cmake-build-system)
     (arguments
      (list
       #:configure-flags
       #~(list)))
     (inputs (list
              opencl-icd-loader
              opencl-headers
              pkg-config))
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
        (base32 "0ys6n53n032zq1ll9f3vgxk8sw0qq7x3fi7awsyy13adzp3hn08p"))))
     (build-system cmake-build-system)
     (arguments
      (list
       #:configure-flags
       #~(list
          "-DLLAMA_CLBLAST=ON"
          (string-append "-DCLBlast_DIR=" #$clblast))))
     (inputs (list
              opencl-icd-loader
              opencl-headers
              clblast
              pkg-config))
     (home-page "https://github.com/ggerganov/llama.cpp")
     (synopsis "Port of Facebook's LLaMA model in C/C++")
     (description "This package provides a port to Facebook's LLaMA collection
of foundation language models.  It requires models parameters to be downloaded
independently to be able to run a LLaMA model.")
     (license license:expat))))