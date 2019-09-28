#lang at-exp racket

(provide define-icons-from
         
         define-assets-from 
         doc-all
         also-for-asset-docs)

(require (for-syntax 2htdp/image (only-in scribble/manual image para))
         (for-syntax racket)
         syntax/parse/define)

(define-for-syntax (get-png-names-from path)
  (map
   (compose
    string->symbol
    (curryr string-replace ".png" ""))
   (filter (curryr string-suffix? ".png")
           (map ~a (directory-list path)))))


(define (module-ids module-path-sym)
  (define-values (ids sts) 
    (module->exports module-path-sym))
  (map first (rest (first ids))))


(define-syntax (doc-all stx)
  (syntax-parse stx
    [(_ path)
    #`(let ()
        (define ids (module-ids (syntax->datum #'path)))
        (define stuffs 
          (map 
            (lambda(i) (dynamic-require (syntax->datum #'path) i))
            ids))
        stuffs )])) 


(define-syntax (define-assets-from stx)
  (define root (apply build-path (reverse
                                  (rest (reverse (explode-path (syntax-source stx)))))))

  (define path (second (syntax->datum stx)))

  (define ids (get-png-names-from (build-path root path)))

  (define (define-asset i)
    (define p (build-path root path (~a i ".png") ))
    `(begin
       (require (for-doc scribble/manual))
       (provide
         ;Use srcdoc for compatibility with include-extracted-assets
        (thing-doc ,i image?
                   @{@para[,(string-titlecase (string-replace (~a i) "-" " "))]{ Image}
                     @image[,p]}
                   ))

       (define ,i
         (bitmap/file ,p))
       
       ))

  (define (define-asset-doc i)
    (define p (build-path root path (~a i ".png") ))
    `(begin
       (provide ,i)
       (define ,i
         @defthing[,i image?]{
           @para[,(string-titlecase (string-replace (~a i) "-" " "))]{ Image }
           @image[,p]})))

  (datum->syntax stx
   `(begin
      (require 2htdp/image 
               scribble/srcdoc)
      
      ,@(map define-asset ids)
      
       ;Use asset-doc for newer, fancier doc management
       (module+ asset-docs 
         (require scribble/manual
                  (only-in 2htdp/image image?))

         ,@(map define-asset-doc ids) ))))

(define-syntax-rule (also-for-asset-docs #:asset-modules (canonicals ...)
                                         stuff ...)
  (begin
    stuff ...
    (for-asset-docs 
      (canonicals ...)
      stuff ...)))

(define-syntax (for-asset-docs stx)
  (syntax-parse stx
    [(_ (canonicals ...) stuff ...)
     #`(module asset-docs racket 
         #,@(generate-require-provides 
             (syntax->datum #'(canonicals ...))     
             (syntax->datum #'(stuff ...))))]))


(define-for-syntax (generate-require-provides replacements original)
  (define (do-replacement rep tree)
    (if (eq? rep tree)
      `(submod ,rep asset-docs)
      (if (not (list? tree))
        tree
        (map (curry do-replacement rep) tree))))

  (foldl do-replacement
         original
         replacements))

(define-syntax (define-icons-from stx)
  (define root (apply build-path (reverse
                                  (rest (reverse (explode-path (syntax-source stx)))))))

  (define path (second (syntax->datum stx)))

  (define ids (map (compose string->symbol
                            (curryr ~a "-icon"))
                   (get-png-names-from (build-path root path))))

  (define (define-asset i)
    (define i-no-icon (string-replace (~a i) "-icon" "" #:all? #f))
    (define p (build-path root path (~a i-no-icon ".png") ))
    `(begin
       (require (for-doc scribble/manual))
       (provide
        (thing-doc ,i image?
                   @{@para[,(string-titlecase (string-replace (~a i) "-" " "))]{ Image}
                     @image[,p]}
                   ))

       
       (define ,i
         (bitmap/file ,p))))

  (datum->syntax stx
   `(begin
      (require 2htdp/image scribble/srcdoc)

      
      ,@(map define-asset ids))))



