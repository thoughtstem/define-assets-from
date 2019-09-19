#lang racket

(require "../main.rkt")

(define-assets-from "assets")

(module+ test
  (require 2htdp/image
           rackunit) 

  (check-pred
    image?
    (above earth earth)))
