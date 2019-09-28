#lang scribble/manual
@require[@for-label[define-assets-from
                    racket/base]
         scribble/extract]

@title{define-assets-from}
@author{thoughtstem}

@(require define-assets-from)

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
  @item{Provides documentation in a submodule called @racket[asset-docs], where (unlike @racket[srcdoc]) each identifer is provided out as a list with the original identifier (as a symbol), the type of asset (currently only @racket[image?]) and the appropriate way to document the image.}
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

@section{Generating Docs}

@subsection{One Way}
The below documentation was generated with @racket[srcdoc]'s @racket[include-extracted].

@codeblock{
  (require scribble/extract)
  (include-extracted "../demo/assets.rkt")
}

@(include-extracted "../demo/assets.rkt")

@subsection{New Way}

Using @racket[define-asset-doc] in a module creates a submodule that is its Asset Doc Module.  For every module that supplies assets, there is a associated documentation that describes (for example) how that asset looks, its type, its creator, its license, etc.  It is stored as a value that is suitable for direct inclusion in a Scribble document.

Use those identifiers directly in scribble doc, simply by requiring the Asset Doc Module first.

@(require (submod define-assets-from/demo/assets asset-docs))
@nested[#:style 'inset]{
@codeblock{
  (require scribble/extract)
  (require (submod define-assets-from/demo/assets asset-docs))

  earth
}
}

This gives:

@nested[#:style 'inset]{
@earth
}

If you want to include all the assets in an Asset Doc Module without knowing their names, you can use @racket[doc-all]

@nested[#:style 'inset]{
@codeblock{
  (require (submod define-assets-from/demo/assets asset-docs))
  (doc-all (submod define-assets-from/demo/assets asset-docs))
}
}

This gives us:


@nested[#:style 'inset]{
@(doc-all (submod define-assets-from/demo/assets asset-docs))
}



@section{Asset Modules}

If all you want to do turn a folder full of images into Racket identifiers with documentation, you don't even need to keep reading.  Just use the above tools.

But often, you may want to make higher-level Asset Modules that are composed from other Asset Modules using Racket's Module System.  If you use regular @racket[require] and @racket[provide] for that, you may lose track of the associated Asset Doc Modules.

Let's call an Asset Module with an Asset Doc Module in a submodule called @racket[asset-docs] a @bold{Canonical Asset Module} iff the image ids in the main module match 1-to-1 with documentation ids in the Asset Doc Module.

If you want to combine two such modules you must take care to combine their two Asset Doc Modules in isomorphic ways inside a single Asset Doc Module called @racket[asset-docs].

Example:


@nested[#:style 'inset]{
@codeblock{
  (provide 
    (all-from-out define-assets-from/demo/assets)
    (all-from-out define-assets-from/demo/assets2))

  (require (only-in define-assets-from/demo/assets earth) 
           (only-in define-assets-from/demo/assets2 logo)) 

  (module asset-docs racket 
    (provide (all-from-out 
               (submod define-assets-from/demo/assets asset-docs)
               (submod define-assets-from/demo/assets2 asset-docs)))

    (require 
       (only-in (submod define-assets-from/demo/assets asset-docs) earth) 
       (only-in (submod define-assets-from/demo/assets2 asset-docs) logo)))
}

Now this file is also a Canonical Asset Module because the ids in the main module still match 1-to-1 with their intended documentation, and the Asset Doc Module is called @racket[asset-docs].  All links to meta-data have been preserved.

}

To make that easier for most use-cases, wrap all your @racket[require] and @racket[provide] code in @racket[also-for-asset-docs] to will ensure that whatever you do to the main module's identifiers, you also do for the Asset Doc Module.

For example @racket[demo2/assets] is defined this way

@codeblock{
  #lang racket 
  (require define-asset-docs)

  (also-for-asset-docs
   (provide 
    (all-from-out define-assets-from/demo/assets)
    (all-from-out define-assets-from/demo/assets))

   (require (only-in define-assets-from/demo/assets earth) 
    (only-in define-assets-from/demo/assets logo)))
}

It assumes that all module paths being handled in the requires and provides pertain to cnnonical asset modules (i.e. that they have submodules called @racket[asset-docs]).  It makes isomorphic actions in the local @racket[asset-docs] submodule to maintain canonicity.


Thus, higher-level modules can be documented just as their imported modules could have -- because they are still canonical.  Documentation follows identifiers up to whatever document wants to document them, allowing the original credits (for example) to be maintained. 

@codeblock{
  (require (submod define-assets-from/demo2/assets asset-docs))
  logo
}

Gives the doc for a known identifier -- as if its source had been @racket[demo2].

@(require (only-in (submod define-assets-from/demo2/assets asset-docs) logo))
@nested[#:style 'inset]{
  @logo
}

Likewise the asset itself can be accessed from the main module:

@codeblock{
  (require define-assets-from/demo2/assets)
  logo
}

And to doc all:

@codeblock{
  (require (submod define-assets-from/demo2/assets asset-docs))
  (doc-all (submod define-assets-from/demo2/assets asset-docs))
}


This gives:

@nested[#:style 'inset]{
  @(doc-all (submod define-assets-from/demo2/assets asset-docs))
}





