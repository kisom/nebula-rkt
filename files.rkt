#lang racket

(require (planet mordae/sha2))

(define nebula-root "/tmp/nebula")

(define (sha256-digest input) 
  (cond 
   [(bytes? input) (bytes->sha256 input)]
   [(string? input) (string->sha256/utf-8 input)]
   [(input-port? input) (sha256-bytes input)]
   [else (print input)
         (error "Invalid input.")]))

(define (file-parent digest)
  (string-join (list
                nebula-root
                (substring digest 0 4))
               "/"))

(define (write-file digest contents)
  (let ([parent (file-parent digest)])
    (unless (directory-exists? parent)
      (make-directory* parent)))
  (let ([path (file-path digest)])
    (if (file-exists? path)
      #t
      (and (display-to-file contents path) #t))))

(define (file-path digest)
  (string-join (list
                nebula-root
                (substring digest 0 4)
                (substring digest 4))
               "/"))

(define (store-file in-port)
  (let* ([contents (port->bytes in-port)]
         [digest (sha256-digest contents)])
    (unless (write-file digest contents)
      (error "Couldn't write to file."))
    digest))

(define (network-listener portno)
  (let ([listener (tcp-listen portno)])
    (let-values ([(in out) (tcp-accept listener)])
      (write (store-file in) out)
      (close-input-port in)
      (close-output-port out))))
