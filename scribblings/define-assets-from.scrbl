#lang scribble/manual
@require[@for-label[define-assets-from
                    racket/base]
         scribble/extract]

@title{define-assets-from}
@author{thoughtstem}

@defmodule[define-assets-from]

This module makes it easy to turn PNG files in a directory
into provided identifiers that correspond to @racket[image?] values.

It eases the burden of creating and maintaining Racket packages that
involve providing image assets.

@defform[(define-assets-from path)
         #:contracts ([path path-string?])]{
  Looks in the relative @racket[path] for any PNG files.
  For any found, it: 

  @itemize{
   @item{Defines an identifier based on the PNG name}
   @item{Provides the identifier}
   @item{Provides documentation in a submodule called @racket[srcdoc], suitable for inclusion with Racket's in-source documentation
         system (i.e. @racket[scribble/extract]'s @racket[include-extracted])}
  } 

  For example, in some file, try:

  @codeblock{
    #lang racket
    (require define-assets-from)

    (define-assets-from "assets")
  }

  Put some a PNG file in a folder called @racket[assets].
  
  See the demo in this package. 
}

The below documentation was generated with

@codeblock{
  (require scribble/extract)
  (include-extracted "../demo/assets.rkt")
}

@(include-extracted "../demo/assets.rkt")
