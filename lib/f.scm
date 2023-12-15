(define-module (f)
  #:export (read-lines))

(use-modules (ice-9 rdelim))

(define (read-lines file-path)
  (define (accum-lines port lines)
    (let ((line (read-line port)))
      (if (eof-object? line)
          (reverse lines)
          (accum-lines port (cons line lines)))))

  (call-with-input-file file-path
    (lambda (input-port)
      (accum-lines input-port '()))))
