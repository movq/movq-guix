(define-module (movq packages buildstream)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages virtualization)
  )

(define-public python-pyroaring
  (package
    (name "python-pyroaring")
    (version "0.3.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyroaring" version))
              (sha256
               (base32
                "1ds61f3rncgi82i9cnn1f0ylncm5kpjxgnbia64as7bsvp5z8ar3"))))
    (build-system python-build-system)
    (home-page "https://github.com/Ezibenroc/PyRoaringBitMap")
    (synopsis "Fast and lightweight set for unsigned 32 bits integers.")
    (description "Fast and lightweight set for unsigned 32 bits integers.")
    (license license:expat)))

(define-public python-buildstream
(package
  (name "python-buildstream")
  (version "1.95.2")
  (source (origin
            (method git-fetch)
            (uri (git-reference
                   (url "https://github.com/apache/buildstream")
                   (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0wspszl23xavcwsk47gdi83m37v65r6bz5q32li4qgajpckvfi3w"))))
  (build-system python-build-system)
  (arguments (list #:tests? #f))
  (native-inputs (list python-cython
                       python-packaging))
                       ;python-pyroaring
                       ;python-pytest))
  (propagated-inputs (list bubblewrap
                           python-click
                           python-dateutil
                           python-grpcio
                           python-jinja2
                           python-pluginbase
                           python-protobuf
                           python-psutil
                           python-pyroaring
                           python-ruamel.yaml
                           python-setuptools
                           python-ujson))
  (home-page "https://gitlab.com/BuildStream/buildstream")
  (synopsis "A framework for modelling build pipelines in YAML")
  (description
   "This package provides a framework for modelling build pipelines in YAML")
  (license #f)))

(define-public buildbox-common
  (package
    (name "buildbox-common")
    (version "0.0.62")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/BuildGrid/buildbox/buildbox-common")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1y58wnn4glphbm68anm96sinzv1158kggyxv41pf40qdrc6rz33r"))))
    (build-system cmake-build-system)
    (native-inputs (list googletest pkg-config))
    (inputs (list abseil-cpp
                  c-ares
                  glog
                  grpc
                  openssl
                  protobuf
                  re2
                  `(,util-linux "lib")
                  zlib))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public buildbox-casd
  (package
    (name "buildbox-casd")
    (version "0.0.62")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/BuildGrid/buildbox/buildbox-casd.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1yxmbsihsib35vdj1pxhpbnx3i0a9zjg10zf7kndgrd2378gnsdg"))))
    (build-system cmake-build-system)
    (inputs (list buildbox-common))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))
