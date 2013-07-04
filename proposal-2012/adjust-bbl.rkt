#lang racket

(define prefix 
  #<<--
\item[] 
\hskip-\leftmargin
\begin{minipage}{\textwidth} 
\centerline{Authors names for team members are shown in \textbf{boldface}.} 
\end{minipage}
--
  )

(define last-names-to-bold
  '("Belknap"
    "Brooks"
    "Flatt"
    "Findler"
    "West"
    "Metzger"
    "Niznik"
    "Peaceman"
    "Spring"
    "Vozenilek"
    "Eller"
    "Yarnold"
    ))

(define (merge-lines in out)
  (let loop ([working-line #f])
    (let ([l (read-line in)])
      (cond
        [(eof-object? l) 
         (when working-line
           (display working-line out)
           (newline out))]
        [(not working-line) (loop l)]
        [(regexp-match #rx"^ *$" working-line)
         (display working-line out)
         (newline out)
         (loop l)]
        [(regexp-match #rx"bibitem" working-line)
         (cond
           [(regexp-match #rx"[]]" working-line)
            (display working-line out)
            (newline out)
            (loop l)]
           [else
            (loop (combine working-line l))])]
        [else
         (cond
           [(separator-line? l)
            (display working-line out)
            (newline out)
            (loop l)]
           [else
            (loop (combine working-line l))])]))))

(define (combine working-line l)
  (string-append 
   working-line 
   (regexp-replace #rx"^[ \t]+" l " ")))

(define (separator-line? l)
  (or (regexp-match #rx"^ *$" l)
      (regexp-match #rx"newblock" l)
      (regexp-match #rx"bibitem" l)))


(define (add-header in out)
  (let loop ()
    (let ([l (read-line in)])
      (cond
        [(regexp-match #rx"begin{thebiblio" l)
         (display l out)
         (newline out)
         (display prefix out)
         (newline out)
         (copy-port in out)]
        [else 
         (display l out)
         (newline out)
         (loop)]))))

(define author-reg "^(.*)((^| )[^ ]*( |~~)~a)(.*)$")

(define (add-bold in out)
  (for ([line (in-lines in)])
    (define new-line
      (for/fold ([line line])
        ([name (in-list last-names-to-bold)])
        (cond
          [(and (not (regexp-match #rx"bibitem" line))
                (regexp-match (format author-reg (regexp-quote name))
                              line))
           =>
           (λ (m)
             (define prefix (list-ref m 1))
             (define name (list-ref m 2))
             (define suffix (list-ref m 5))
             (format "~a\\textbf{~a}~a" prefix name suffix))]
          [else line])))
    (display new-line out)
    (newline out)))
         
(call-with-input-file "proposal.bbl"
  (λ (orig-in)
    (call-with-output-file "proposal-new.bbl"
      (λ (final-out)
        (let loop ([fns (list merge-lines add-header add-bold)]
                   [in orig-in]
                   [out final-out])
          (cond
            [(null? (cdr fns))
             ((car fns) in out)]
            [else
             (let-values ([(pin pout) (make-pipe)])
               (thread (λ () 
                         ((car fns) in pout)
                         (close-output-port pout)))
               (loop (cdr fns) pin out))])))
      #:exists 'truncate)))

(delete-file "proposal.bbl")
(copy-file "proposal-new.bbl" "proposal.bbl")
