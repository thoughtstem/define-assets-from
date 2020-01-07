#lang scribble/manual
@require[@for-label[define-assets-from
                    racket/base]
         scribble/extract]

@title{define-assets-from}
@author{thoughtstem}


@defmodule[define-assets-from]

@(require define-assets-from)

This module makes it easy to turn PNG files in a directory
into provided identifiers that correspond to @racket[image?] values.

More importantly, it makes it easier to accompany those Racket values with meta-data 
about where those assets came from -- for example, to credit the artists.  Meta-data
can (if you use the tools documented on this page) follow those Racket values even as
they pass through other modules, become renamed, modified, etc.

It eases the burden of creating and maintaining Racket packages that
involve providing image assets whose license may require certain care to be taken
with regard to crediting the original artist.

@defform[(define-assets-from path [id extra-meta-data] ...)
         #:contracts ([path path-string?])]{
  Looks in the relative @racket[path] for any PNG files.
  For any found, it: 

  @itemize{
   @item{Defines an identifier based on the PNG name}
   @item{Provides the identifier}
   @item{Provides documentation in a submodule called @racket[asset-docs], where (unlike @racket[srcdoc]) each identifer is provided out as a list with the original identifier (as a symbol), the type of asset (currently only @racket[image?]) and the appropriate way to document the image.}
  } 

  For example, in some file, try:

  @codeblock{
    #lang racket
    (require define-assets-from)

    (define-assets-from "assets")
  }

  Put some a PNG file in a folder called @racket[assets].

  You can add extra documentation for individual assets by passing in tuples after the folder name:

  @codeblock{
    #lang racket
    (require define-assets-from)

    (define-assets-from "assets" 
                         (for-all-assets 
                           (para "Please credit Thomas Edison."))
                         (earth 
                           (para "Additionally credit Thomas Edison's cat.")))
  }

  The @racket[for-all-assets] is a special value, which will cause the provided scribble to be displayed whenever that asset is documented.  Adding such text to individual assets (like @racket[earth]) can be accomplished by listing its identifier along with some associated scribble.
}

@section{Generating Docs}

Using @racket[define-assets-from] in a module creates a submodule that is its Asset Doc Module.  In other words, the main module supplies the assets; but the sub Asset Doc Module has associated documentation.  By default the documentation just contains the original image and its name, but it can (for example) also describe how that asset looks, its type, its creator, its license, links to the artist's homepage, etc.  These must be values that are suitable for inclusion in a Scribble document, and can be converted to a @racket[defthing] by using @racket[doc-asset] or @racket[doc-all].

@(require (submod define-assets-from/demo/assets asset-docs))

@defform[(doc-asset id)]{
  The @racket[id] should be something that comes from a @racket[asset-docs] submodule (which, remember, is created for you automatically if you used @racket[define-assets-from])
   
  @nested[#:style 'inset]{
    @codeblock{
      (require define-assets-from)
      (require (submod define-assets-from/demo/assets asset-docs))

      (doc-asset earth)
    }
  }

  This gives:

    @nested[#:style 'inset]{
      @(doc-asset earth)
    }
}

@defform[(doc-all asset-doc-module)]{
  If you want to include all the assets in an Asset Doc Module without knowing their names, you can use @racket[doc-all] and pass in a module path to any Asset Doc Module.  Usually, this will be a submodule automatically created by @racket[define-assets-from].  But it doesn't technically have to be.

  The code will usually look something like this (yes, you should require the Asset Doc Module first):

  @codeblock{
    (require (submod define-assets-from/demo/assets asset-docs))
    (doc-all (submod define-assets-from/demo/assets asset-docs))
  }

  This gives us:
    @nested[#:style 'inset]{
      @(doc-all (submod define-assets-from/demo/assets asset-docs))
    }
}



@section{Asset Modules and Asset Doc Modules}

If all you want to do turn a folder full of images into Racket identifiers with documentation, you don't even need to keep reading.  Just use the above tools.

But often, you may want to make higher-level Asset Modules that are composed from other Asset Modules using Racket's Module System.  If you use regular @racket[require] and @racket[provide] for that, you may lose track of the associated Asset Doc Modules.

Let's call an Asset Module with an Asset Doc Module in a submodule called @racket[asset-docs] a @bold{Canonical Asset Module} iff the image ids in the main module match 1-to-1 with documentation ids in the Asset Doc Module.

That might sound complicated, but remember that this is exactly what @racket[define-assets-from] creates for you.  Any module whose identifiers are created thusly is automatically canonical. 

If you want to combine two such modules you must take care to combine their two Asset Doc Modules in isomorphic ways inside a single Asset Doc Module called @racket[asset-docs].

This would be the long way, just to illustrate the point:
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

Now this file is also a Canonical Asset Module because the ids in the main module still match 1-to-1 with their intended documentation, and the Asset Doc Module is called @racket[asset-docs].  In other words, all implied links to meta-data have been preserved.  

To make that easier for most use-cases, wrap all your @racket[require] and @racket[provide] code in @racket[also-for-asset-docs] to will ensure that whatever you do to the main module's identifiers, you also do for the Asset Doc Module.  The names must match in order for the links to documentation to be preserved.

@(require (only-in (submod define-assets-from/demo2/assets asset-docs) logo))

@defform[(also-for-asset-docs #:asset-modules (module ...) require-and-provide-code ...)]{

Each @racket[module] given should be a canonical asset module.  The given @racket[require-and-provide-code]
is passed through as given to the @racket[main] module.  But it is also pushed into a @racket[asset-docs]
submodule with the various @racket[module] being replaced by @racket[(submod module asset-docs)].

This helps ensure that the modules that require and provide canonical asset modules are themselves canonical.

@codeblock{
  #lang racket 
  (require define-asset-docs)


  (also-for-asset-docs
    #:asset-modules (define-assets-from/demo/assets
                     define-assets-from/demo/assets2)

    (provide 
      (all-from-out define-assets-from/demo/assets)
      (all-from-out define-assets-from/demo/assets2))

    (require (only-in define-assets-from/demo/assets earth) 
             (only-in define-assets-from/demo/assets2 logo)))
}


Now @racket[demo2/assets] provides @racket[logo], which originally game from @racket[demo/assets2].
Because we used @racket[also-for-asset-docs], the documentation has been passed along.

We can use @racket[doc-asset] and @racket[doc-all] as if @racket[demo2/assets] was the original
source of @racket[logo].

@codeblock{
  (require (submod define-assets-from/demo2/assets asset-docs))
  (doc-asset logo)
}

This gives:

@nested[#:style 'inset]{
  @(doc-asset logo)
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


And of course, if you just want the @racket[logo] @racket[image?] asset, you
don't need to worry about @racket[asset-docs] or documentation. 

@codeblock{
  (require define-assets-from/demo2/assets)
  logo
}

This gives the image:

@logo

}





