(use-modules
  ((guix licenses) #:prefix license:)
  (guix packages)
  (guix download)
  (guix git-download)
  (guix build-system haskell)
  (gnu packages pkg-config)
  (gnu packages tls)
  (gnu packages gsasl)
  (gnu packages libidn)
  (gnu packages xml)
  (gnu packages haskell)
  (gnu packages haskell-check)
  (gnu packages haskell-crypto)
  (gnu packages haskell-web)
  (gnu packages haskell-xyz)
  (ice-9 rdelim)
  (ice-9 popen))

(define-public ghc-cache
  (package
    (name "ghc-cache")
    (version "0.1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/cache/cache-"
               version
               ".tar.gz"))
        (sha256
          (base32 "0d75257kvjpnv95ja50x5cs77pj8ccfr0nh9q5gzvcps83qdksa2"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-clock" ,ghc-clock)
        ("ghc-hashable" ,ghc-hashable)
        ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (native-inputs
      `(("ghc-hspec" ,ghc-hspec)
        ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/hverr/haskell-cache#readme")
    (synopsis "An in-memory key/value store with expiration support")
    (description
      "An in-memory key/value store with expiration support, similar to patrickmn/go-cache for Go. . The cache is a shared mutable HashMap implemented using STM and with support for expiration times.")
    (license license:bsd-3)))

(define-public ghc-scanner
  (package
    (name "ghc-scanner")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/scanner/scanner-"
               version
               ".tar.gz"))
        (sha256
          (base32 "1mhqh94qra08zidqfsq0gxi83cgflqldnk9rr53haynbgmd5y82k"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-fail" ,ghc-fail)))
    (native-inputs
      `(("ghc-hspec" ,ghc-hspec)
        ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/Yuras/scanner")
    (synopsis
      "Fast non-backtracking incremental combinator parsing for bytestrings")
    (description
      "Parser combinator library designed to be fast. It doesn't support backtracking.")
    (license license:bsd-3)))

(define-public ghc-hedis
  (package
    (name "ghc-hedis")
    (version "0.12.11")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/hedis/hedis-"
               version
               ".tar.gz"))
        (sha256
          (base32 "1n83zwg011n9w2v1zz4mwpms9jh3c8mk700zya4as1jg83748xww"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ; The main tests require redis-server running, but not doctest
             (invoke "runhaskell" "Setup.hs" "test" "doctest")
             #t)))))
    (inputs
      `(("ghc-scanner" ,ghc-scanner)
        ("ghc-async" ,ghc-async)
        ("ghc-bytestring-lexing" ,ghc-bytestring-lexing)
        ("ghc-unordered-containers" ,ghc-unordered-containers)
        ("ghc-network" ,ghc-network)
        ("ghc-resource-pool" ,ghc-resource-pool)
        ("ghc-tls" ,ghc-tls)
        ("ghc-vector" ,ghc-vector)
        ("ghc-http" ,ghc-http)
        ("ghc-errors" ,ghc-errors)
        ("ghc-network-uri" ,ghc-network-uri)))
    (native-inputs
      `(("ghc-hunit" ,ghc-hunit)
        ("ghc-test-framework" ,ghc-test-framework)
        ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
        ("ghc-doctest" ,ghc-doctest)))
    (home-page "https://github.com/informatikr/hedis")
    (synopsis
      "Client library for the Redis datastore: supports full command set, pipelining.")
    (description
      "Redis is an open source, advanced key-value store. It is often referred to as a data structure server since keys can contain strings, hashes, lists, sets and sorted sets. This library is a Haskell client for the Redis datastore. Compared to other Haskell client libraries it has some advantages: . [Compatibility with Latest Stable Redis:] Hedis is intended to be used with the latest stable version of Redis (currently 5.0). Most redis commands (<http://redis.io/commands>) are available as haskell functions, although MONITOR and SYNC are intentionally omitted. Additionally, a low-level API is exposed that  makes it easy for the library user to implement further commands, such as new commands from an experimental Redis version. . [Automatic Optimal Pipelining:] Commands are pipelined (<http://redis.io/topics/pipelining>) as much as possible without any work by the user. See <http://informatikr.com/2012/redis-pipelining.html> for a technical explanation of automatic optimal pipelining. . [Enforced Pub\\/Sub semantics:] When subscribed to the Redis Pub\\/Sub server (<http://redis.io/topics/pubsub>), clients are not allowed to issue commands other than subscribing to or unsubscribing from channels. This library uses the type system to enforce the correct behavior. . [Connect via TCP or Unix Domain Socket:] TCP sockets are the default way to connect to a Redis server. For connections to a server on the same machine, Unix domain sockets offer higher performance than the standard TCP connection. . For detailed documentation, see the \"Database.Redis\" module. .")
    (license license:bsd-3)))

(define-public ghc-libxml-sax
  (package
    (name "ghc-libxml-sax")
    (version "0.7.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/libxml-sax/libxml-sax-"
               version
               ".tar.gz"))
        (sha256
          (base32 "0lbdq6lmiyrnzk6gkx09vvp928wj8qnqnqfzy14mfv0drj21f54r"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-xml-types" ,ghc-xml-types)
        ("libxml2" ,libxml2)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://john-millikin.com/software/haskell-libxml/")
    (synopsis "Bindings for the libXML2 SAX interface")
    (description "")
    (license license:expat)))

(define-public ghc-gsasl
  (package
    (name "ghc-gsasl")
    (version "0.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/gsasl/gsasl-"
               version
               ".tar.gz"))
        (sha256
          (base32 "11i12r9s30jrq8hkgqagf2fd129r6ya607s9ibw549ablsxgr507"))))
    (build-system haskell-build-system)
    (arguments
      `(#:cabal-revision
        ("1" "1c806a82qd1hkxxfh1mwk0i062bz6fkaap5ys3n4x9n6wjv7ilin")))
    (inputs
      `(("ghc-monad-loops" ,ghc-monad-loops)
        ("gsasl" ,gsasl)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://git.sr.ht/~singpolyma/gsasl-haskell")
    (synopsis "Bindings for GNU libgsasl")
    (description "")
    (license license:gpl3)))

(define-public ghc-gnutls
  (package
    (name "ghc-gnutls")
    (version "0.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/gnutls/gnutls-"
               version
               ".tar.gz"))
        (sha256
          (base32 "1c5pm0d80wpgh2bkcgbvmc72agf89h8ghfnrn1m1x3fljbgzvrn0"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-monads-tf" ,ghc-monads-tf)
        ("gnutls" ,gnutls)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://john-millikin.com/software/haskell-gnutls/")
    (synopsis "Bindings for GNU libgnutls")
    (description
      "You almost certainly don't want to depend on this release. . This is a pre-alpha, almost useless release; its only purpose is to enable TLS support in some of my other libraries. More complete bindings for GNU TLS will be released at a later date.")
    (license license:gpl3)))

(define-public ghc-gnuidn
  (package
    (name "ghc-gnuidn")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/gnuidn/gnuidn-"
               version
               ".tar.gz"))
        (sha256
          (base32 "0vxrcp9xz5gsvx60k12991zn5c9nk3fgg0yw7dixbsjcfqgnnd31"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'less-strict-dependencies
           (lambda _
             (substitute* "gnuidn.cabal"
               (("chell >= 0.4 && < 0.5") "chell <0.6"))
             #t)))))
    (inputs `(("libidn" ,libidn)))
    (native-inputs
      `(("ghc-chell" ,ghc-chell)
        ("ghc-c2hs" ,ghc-c2hs)
        ("ghc-chell-quickcheck" ,ghc-chell-quickcheck)
        ("ghc-quickcheck" ,ghc-quickcheck)
        ("pkg-config" ,pkg-config)))
    (home-page "https://john-millikin.com/software/haskell-gnuidn/")
    (synopsis "Bindings for GNU IDN")
    (description "")
    (license license:gpl3)))

(define-public ghc-network-simple
  (package
    (name "ghc-network-simple")
    (version "0.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "network-simple" version))
        (sha256
          (base32 "17hpgcwrsx2h8lrb2wwzy0anp33mn80dnwcgnqmb8prajwjvz807"))))
    (build-system haskell-build-system)
    (inputs (list ghc-network ghc-network-bsd ghc-safe-exceptions ghc-socks))
    (home-page "https://github.com/k0001/network-simple")
    (synopsis "Simple network sockets usage patterns.")
    (description
      "This module exports functions that abstract simple network socket usage
patterns. .  See the @changelog.md@ file in the source distribution to learn
about any important changes between versions.")
    (license license:bsd-3)))

(define-public ghc-network-protocol-xmpp
  (package
    (name "ghc-network-protocol-xmpp")
    (version "0.4.10")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/network-protocol-xmpp/network-protocol-xmpp-"
               version
               ".tar.gz"))
        (sha256
          (base32 "03xlw8337lzwp7f5jvbvgirf546pfmfsfjvnik08qjjy1rfn5jji"))))
    (build-system haskell-build-system)
    (inputs
      (list
        ghc-gnuidn
        ghc-gnutls
        ghc-gsasl
        ghc-libxml-sax
        ghc-monads-tf
        ghc-network
        ghc-network-simple
        ghc-xml-types))
    (home-page "https://git.sr.ht/~singpolyma/network-protocol-xmpp")
    (synopsis "Client library for the XMPP protocol.")
    (description "")
    (license license:gpl3)))

;;;;

(define %source-dir (dirname (current-filename)))
(define %git-dir (string-append %source-dir "/.git"))

; double-escaped template of the cheogram-sip sexp
; This allows us to bake the expression without doing a full eval to a record,
; so it can be written
(define-public cheogram-sip-template
  '(package
    (name "cheogram-sip")
    (version (read-line (open-pipe* OPEN_READ "git" "--git-dir" %git-dir "describe" "--always" "--dirty")))
    (source
     `(origin
       (method git-fetch)
       (uri (git-reference
             (recursive? #t)
             (url "https://git.singpolyma.net/cheogram-sip")
             (commit ,(read-line (open-pipe* OPEN_READ "git" "--git-dir" %git-dir "rev-parse" "HEAD")))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         ,(read-line (open-pipe* OPEN_READ "guix" "hash" "-rx" %source-dir))))))
    (build-system 'haskell-build-system)
    (inputs
      '(list
        ghc-attoparsec
        ghc-base64-bytestring
        ghc-basic-prelude
        ghc-cache
        ghc-clock
        ghc-errors
        ghc-hedis
        ghc-http
        ghc-http-types
        ghc-monad-loops
        ghc-network
        ghc-network-protocol-xmpp
        ghc-network-uri
        ghc-safe
        ghc-sha
        ghc-xml-types))
    (home-page "https://git.singpolyma.net/cheogram-sip")
    (synopsis "XMPP to SIP bridge")
    (description "")
    (license 'license:agpl3)))

; Baked version of jmp-pay-template with leaves eval'd
(define-public cheogram-sip-baked
 (cons
  (car cheogram-sip-template)
  (map
   (lambda (x) (list (car x) (eval (cadr x) (current-module))))
   (cdr cheogram-sip-template))))

; Build clean from git the version from a local clone
; To build whatever is sitting in local use:
; guix build --with-source=$PWD -f guix.scm

(eval cheogram-sip-baked (current-module))
