#lang racket
(require racket/cmdline)

(define (parse x)
  (
   cond
    [(list? x)
       (match x
         [(list (? number?)) x]
         [(list (? symbol?)) x]
         [(list '+ a b)
          (define parsedA (parse a))
           (define parsedB (parse b))
          `(,parsedA + ,parsedB)
          ]         
         [(list '* a b)
           (define parsedA (parse a))
           (define parsedB (parse b))
          `(,parsedA * ,parsedB)
           ]         
         [(list 'λ a b)
            (define parsedB(parse b))
            (define parsedA(parse a))
            (append `(function ( ,@parsedA)) '(|{|) `(return ,parsedB) '(|}|) )
         ]         
         [(list 'println M)
          (define parsedM (parse M))
          `(console.log(,parsedM))
         ]
         [(list a b)
          (define parsedA (parse a))
          (define parsedB (parse b))
          `(,parsedA ( ,parsedB )) 
         ]         
         [(list 'ifleq0 a b c)
          (define parsedA (parse a))
          (define parsedB (parse b))
          (define parsedC (parse c))
          `(,parsedA <= 0? ,parsedB : ,parsedC)
         ]
         )
    ]
    [(number? x) x]
    [(symbol? x) x]
    [else (error 'parse "err" )]
   )
)

(define file_in (command-line #:args (filename_in filename_out) filename_in))
(define file_out (command-line #:args (filename_in filename_out) filename_out))
(define file_out_port (open-output-file file_out #:exists 'replace))
(define to_translate_port (open-input-file file_in #:mode 'text))
(define parseThis (read to_translate_port))
(define translated_prog (parse parseThis))
(define string_prog (~a translated_prog))
(printf "Target Program: ")
(print parseThis)
(printf "\n" )
(printf "JS version: ")
(print string_prog)
(printf "\n" )
;; Send translated_prog to output file
(fprintf file_out_port string_prog)
