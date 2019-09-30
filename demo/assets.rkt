#lang at-exp racket

(require "../main.rkt" scribble/manual)

(define-assets-from "assets"
                    (for-all-assets
                      @para{This asset was created by Stephen R. Foster and is in the public domain})
                    (question-mark
                      @para{Additional meta data added via @racket[(define-assets-from ...)]}))


(module+ test
  (require 2htdp/image
           rackunit) 

  (check-pred
    image?
    (above earth earth)))
  
