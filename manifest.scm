(use-modules (guix packages)
	     (guix git-download)
	     (guix build-system cmake)
	     (guix build-system copy)
	     (guix utils)
	     (guix gexp)
	     (gnu packages racket)
	     (gnu packages emacs-xyz)
	     ((guix licenses) #:prefix license:))

(define paho-mqtt-c
  (package
    (name "paho-mqtt-c")
    (version "1.3.13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/eclipse/paho.mqtt.c")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1y5lsh41kszyjcrxrjshs838l23ncdyssxk848ij1bq0jix2g93l"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ;; Disable tests
    (synopsis "Eclipse Paho MQTT C client library")
    (description
     "Eclipse Paho MQTT C client library.")
    (home-page "https://www.eclipse.org/paho/")
    (license license:epl1.0)))

(define (make-racket! package-to-inherit)
  (package
    (inherit package-to-inherit)
    (arguments
     (substitute-keyword-arguments (package-arguments package-to-inherit)
       ((#:phases those-phases #~%standard-phases)
	#~(modify-phases #$those-phases
            (add-after 'install 'wrap-racket
              (lambda* (#:key outputs inputs #:allow-other-keys)
		(let* ((out (assoc-ref outputs "out"))
		       (bin (string-append out "/bin/racket"))
		       (libpaho (string-append (assoc-ref inputs "paho-mqtt-c")
					       "/lib")))
		  (wrap-program bin
                    `("LD_LIBRARY_PATH" ":" prefix
                      ,(list libpaho))))))))))
    (inputs
     (modify-inputs (package-inputs package-to-inherit)
		    (prepend paho-mqtt-c)))))

(define racket-minimal! (make-racket! racket-minimal))
(define racket! (make-racket! racket))

(define smart-relay
  (package
    (name "smart-relay")
    (version "0.1.0")
    (source
     (local-file "src" #:recursive? #t))
    (build-system copy-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
	 (add-before 'install 'change-paths
           (lambda* (#:key outputs #:allow-other-keys)
	     (let ((out  (assoc-ref outputs "out")))
               (substitute* "smart-relay.sh"
		 (("smart-relay.rkt")
                  (string-append out "/share/smart-relay.rkt"))))))
	 (add-before 'install 'move-files
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (let* ((out  (assoc-ref outputs "out"))
		    (bin  (string-append out "/bin/"))
		    (share  (string-append out "/share/")))
	       (mkdir-p bin)
	       (mkdir-p share)
	       (chmod "smart-relay.sh" #o555)
	       (copy-recursively "smart-relay.sh"
				 (string-append bin "smart-relay"))
	       (copy-recursively "smart-relay.rkt"
				 (string-append share "smart-relay.rkt"))
	       (delete-file-recursively "smart-relay.sh")
	       (delete-file-recursively "smart-relay.rkt")))))))
    (synopsis "Lisp program to control usb relay over mqtt")
    (description "Lisp program to control usb relay over mqtt")
    (home-page "https://git.jdlugosz.com/hsp/smart-relay")
    (license license:unlicense)))

(define emacs-geiser-racket!
  (package
    (inherit emacs-geiser-racket)
    (inputs
     (modify-inputs (package-inputs emacs-geiser-racket)
		    (delete "racket")
		    (prepend racket!)))))


(if (getenv "DEV_SHELL")
    (concatenate-manifests
     (list (packages->manifest
	    (list racket!
		  emacs-geiser-racket!))
	   (specifications->manifest
            (list "racket"
		  "emacs-racket-mode"
		  "emacs"))))
    (concatenate-manifests
     (list (packages->manifest
	    (list racket!
		  smart-relay))
	   (specifications->manifest
            (list "bash"
		  "nss-certs")))))
