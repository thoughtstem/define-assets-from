#lang racket

(require define-assets-from)

(also-for-asset-docs
  #:asset-modules (define-assets-from/demo/assets
                   define-assets-from/demo/assets2)

  (provide 
    (all-from-out define-assets-from/demo/assets)
    (all-from-out define-assets-from/demo/assets2))

  (require (only-in define-assets-from/demo/assets earth) 
           (only-in define-assets-from/demo/assets2 logo)))
