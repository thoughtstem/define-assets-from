#lang at-exp racket

(provide define-icons-from
         
         define-assets-from 
         doc-all
         doc-asset
         also-for-asset-docs)

(require (for-syntax 2htdp/image (only-in scribble/manual image para))
         (only-in scribble/manual defthing)
         (for-syntax racket)
         syntax/parse/define)

(define-for-syntax (get-png-names-from path)
  (map
   (compose
    string->symbol
    (curryr string-replace ".png" ""))
   (filter (curryr string-suffix? ".png")
           (map ~a (directory-list path)))))

(define-syntax-rule (doc-asset docable)
  (defthing docable image?
            docable))

(define-for-syntax (module-ids module-path-sym)
  (define-values (ids sts) 
    (module->exports module-path-sym))
  (map first (rest (first ids))))


(define-syntax (doc-all stx)
  (syntax-parse stx
    [(_ path)
    (define ids (module-ids (syntax->datum #'path)))
    (define all-docs
      (map 
        (lambda (i) `(doc-asset ,i))
        ids))
    (datum->syntax stx
      `(list
         ,@all-docs))]))

;This macro got ugly.
;  Please prettify it with syntax-parse before adding more grossness to it!
(define-syntax (define-assets-from stx)
  (define root (apply build-path (reverse
                                  (rest (reverse (explode-path (syntax-source stx)))))))

  (define stuff (syntax->datum stx))   
  (define path (second stuff))

  (define ids (get-png-names-from (build-path root path)))

  (define extra-docs 
    (make-hash (map (curry apply cons) 
                    (drop stuff 2))))

  (define (define-asset i)
    (define p (build-path root path (~a i ".png") ))
    `(begin
       (require (for-doc scribble/manual)
                syntax/parse/define)
       (provide
         ;Use srcdoc for compatibility with include-extracted-assets
        (thing-doc ,i image?
                   @{@para[,(string-titlecase (string-replace (~a i) "-" " "))]{ Image}
                     @image[,p]}
                   ))

       #;
       (define ,i
         (bitmap/file ,p))
       
       (define-syntax (,i stx)
         (syntax-parse stx
           [val:identifier 
             #`(bitmap/file ,p)])) 
       ))

  (define (define-asset-doc i)
    (define p (build-path root path (~a i ".png") ))
    `(begin
       (provide ,i)
       (define ,i
          ,(list 
              'list
               `(para ,(string-titlecase (string-replace (~a i) "-" " " ))
                      " Image")
               `(image ,p)
                (hash-ref extra-docs 'for-all-assets '(list))
                (hash-ref extra-docs i '(list))
               )
          )))

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
  (define flattened (flatten original))
  (for ([r replacements])
    (define (similar? s)
      (string-contains? s (~a r)))
    (when (not (member r flattened))
      (define similars
        (filter similar?  (map ~a (remove-duplicates flattened))))
      (error 
        (~a "The module \"" r "\" was not found in the (also-for-asset-docs ...) block.\n"
            (if (not (empty? similars))
              (~a "Suggestion(s): " similars)
              "")))))

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



