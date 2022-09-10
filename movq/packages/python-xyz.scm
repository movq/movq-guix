(define-module (movq packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization))

(define-public python-mautrix
  (package
    (name "python-mautrix")
    (version "0.17.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "mautrix" version))
              (sha256
               (base32
                "0ark59zb3phn899977gv7qi6ss4rys4bz4cik38757b67q0a6mhc"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f))
    (propagated-inputs (list python-aiohttp
                             python-attrs
                             python-commonmark
                             python-ruamel.yaml.clib
                             python-ruamel.yaml
                             python-yarl))
    (native-inputs (list python-aiosqlite
                         python-asyncpg
                         python-olm
                         python-pycryptodome
                         python-pytest
                         python-pytest-asyncio
                         python-sqlalchemy
                         python-unpaddedbase64))
    (home-page "https://github.com/mautrix/python")
    (synopsis "A Python 3 asyncio Matrix framework.")
    (description "This package provides a Python 3 asyncio Matrix framework.")
    (license #f)))

(define-public python-tulir-telethon
  (package
    (name "python-tulir-telethon")
    (version "1.25.0a20")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "tulir-telethon" version))
              (sha256
               (base32
                "15z8bcsc4rs2q5kvjviiwiln1lgm4brgq6pl4qms8cldh3wjinjz"))))
    (build-system python-build-system)
    (propagated-inputs (list python-pyaes python-rsa))
    (home-page "https://github.com/tulir/Telethon")
    (synopsis "Full-featured Telegram client library for Python 3")
    (description "Full-featured Telegram client library for Python 3")
    (license license:expat)))
