#lang racket

(require (planet mordae/sha2))

(define nebula-root "/tmp/nebula")
(define nebula-file-root (string-join (list nebula-root "files") "/"))

(define (sha256-digest input) 
  (cond 
   [(bytes? input) (bytes->sha256 input)]
   [(string? input) (string->sha256/utf-8 input)]
   [(input-port? input) (sha256-bytes input)]
   [else (print input)
         (error "Invalid input.")]))

(define (file-parent digest)
  (string-join (list
                nebula-file-root
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
                nebula-file-root
                (substring digest 0 4)
                (substring digest 4))
               "/"))

(define (store-file in-port)
  (let* ([contents (port->bytes in-port)]
         [digest (sha256-digest contents)])
    (unless (write-file digest contents)
      (error "Couldn't write to file."))
    digest))

(define (retrieve-file in-port)
  (let* ([digest (string-trim (port->string in-port))]
         [path (file-path digest)])
    (if (file-exists? path)
      (file->bytes path)
      (printf "File ~s does not exist.\n" path))))

(define (connection-handler in out)
  (printf "New connection!\n")
  (let ([op (read-char in)])
    (display op) (newline)
    (cond 
     [(eqv? op #\S) (display (store-file in) out)]
     [(eqv? op #\L) (display (retrieve-file in) out)]
     [else (printf "unknown operation ~s\n" op)])))

(define (network-handler listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread
     (lambda ()
       (connection-handler in out)
       (printf "closing connection!\n")
       (close-input-port in)
       (close-output-port out)))
    (network-handler listener)))

(define (network-listener portno)
  (let ([listener (tcp-listen portno)])
    (define t (thread
               (lambda ()
                 (network-handler listener))))
    (lambda ()
      (kill-thread t)
      (tcp-close listener))))
