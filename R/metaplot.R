#' Boxplot Method for Folded
#'
#' Boxplot for folded. Converts to data.frame with defined column attributes and calls data.frame method.
#' @param x folded
#' @param ... passed to \code{\link{boxplot.data.frame}}
#' @export
#' @family bivariate plots
#' @family boxplot
#' @import metaplot
#' @examples
#' data(eventsf)
#' boxplot(eventsf, SEX, WT, ref = 68)
boxplot.folded <- function(x, ...)boxplot(pack(x),...)

#' Densplot Method for Folded
#'
#' Densplot method for folded. Converts to data.frame with defined column attributes and calls data.frame method.
#' @param x folded
#' @param ... passed to \code{\link{boxplot.data.frame}}
#' @export
#' @family univariate plots
#' @family densplot
#' @examples
#' data(eventsf)
#' library(metaplot)
#' densplot(eventsf, DV)
densplot.folded <- function(x, ...)densplot(pack(x),...)

#' Correlated Splom for Folded
#'
#' Creates a scatterplot matrix with correlations for folded.
#' Categoricals in \dots are currently ignored. dots (\dots) are
#' names of items in VARIABLE to be plotted, or named arguments
#' to be passed to data.frame method.
#' @import lattice
#' @export
#' @family multivariate plots
#' @family corsplom
#' @param x folded
#' @param ... unquoted names of variables to plot, or other named arguments
corsplom.folded <- function(x, ...)corsplom(pack(x,...),...)

#' Scatterplot for Folded
#'
#' Scatterplot for class 'folded'.
#' @export
#' @family bivariate plots
#' @family scatter
#' @param x folded
#' @param ... unquoted names of variables to plot, or other named arguments

scatter.folded <- function(x, ...)scatter(pack(x,...),...)


#' Axis Label for Folded
#'
#' Axis label for folded.
#' @param x folded
#' @param var item of interest
#' @param log whether this is for a log scale
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @import magrittr
#' @return character
axislabel.folded <- function(x, var, log = FALSE, ...){
  x <- x[x$VARIABLE == var & is.defined(x$META),,drop = FALSE]
  lab <- unique(x$VALUE[x$META =='LABEL'])
  guide <- unique(x$VALUE[x$META =='GUIDE'])
  res <- var
  if(length(lab) == 1){
    if(lab %>% is.defined){
      res <- lab
    }
  }
  if(length(guide) == 1){
    if(!encoded(guide)){
      if(guide %>% is.defined){
        guide <- paste0('(',guide,')')
        res <- paste(res,guide)
      }
    }
  }
  if(log) res <- paste0(res,'\n(log)')
  res
}

#' Extract Guide
#'
#' Extracts guide.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family generic functions
guide <- function(x,...)UseMethod('guide')

#' Extract Guide for Folded
#'
#' Extracts guide for class folded, given a variable.
#' @param x folded
#' @param var length-one character
#' @param ... ignored arguments
#' @return length-one character, possibly NA
#' @export
#' @keywords internal
guide.folded <- function(x,var,...){
  stopifnot(length(var) == 1)
  y <- x[is.defined(x$META) & x$META =='GUIDE' & x$VARIABLE == var,'VALUE']
  y <- unique(y) #
  if(length(y) > 1)stop('conflicting guides for ', var)
  if(length(y) == 0) y <- NA_character_
  y
}

#' Extract Label
#'
#' Extracts label.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family generic functions
label <- function(x,...)UseMethod('label')

#' Extract Label for Folded
#'
#' Extracts label for class folded, given a variable.
#' @param x folded
#' @param var length-one character
#' @param ... ignored arguments
#' @return length-one character, possibly NA
#' @export
#' @keywords internal
label.folded <- function(x,var, ...){
  stopifnot(length(var) == 1)
  y <- x[is.defined(x$META) & x$META =='LABEL' & x$VARIABLE == var,'VALUE']
  y <- unique(y)
  if(length(y) > 1)stop('conflicting guides for ', var)
  if(length(y) == 0) y <- NA_character_
  y
}

#' Create Metaplot from Folded
#'
#' Creates a plot from folded.  Packs metadata into attributes and calls next method.
#'

#' @param x object
#' @param ... passed arguments
#' @family metaplot
#' @family univariate plots
#' @family bivariate plots
#' @family multivariate plots
#' @importFrom graphics boxplot
#' @importFrom stats as.formula cor density loess.smooth median
#' @importFrom dplyr filter
#' @export

metaplot.folded <- function(x, ...)metaplot(pack(x,...),...)

#' Normalize a Folded Data Frame
#'
#' Convert folded data.frame to conventional format with column attributes. Scalar metadata is converted to column attributes. Other metadata left unfolded.
#' @export
#' @family pack
#' @return data.frame
#' @seealso \code{\link[fold]{fold.data.frame}}
#' @param x folded
#' @param tolower whether to coerce attribute names to lower case
#' @param ... other arguments
#' @examples
#' data(eventsf)
#' library(metaplot)
#' head(pack(eventsf))
#' attributes(pack(eventsf)$BLQ)
#'
pack.folded <- function(x, tolower = TRUE, ...){
  y <- unfold(x)
  for (col in names(y)){
    if(grepl('_',col)){
      target <- sub('_.*','',col)
      attrib <- sub('[^_]+_','',col)
      if(tolower) attrib <- tolower(attrib)
      if(target %in% names(y)){
        val <- unique(y[[col]])
        spar <- unique(y[,c(target,col)])
        spar <- spar[order(spar[[target]]),]
        spar[[target]] <- paste(spar[[target]]) # guarranteed nonmissing
        if(length(val) == 1){
          attr(y[[target]], attrib) <- val
        } else {
          if(length(spar[[target]]) == length(unique(spar[[target]]))){
            attr(y[[target]], attrib) <- encode(spar[[target]], labels = spar[[col]])
          }
        }
        y[[col]] <- NULL
      }

    }
  }
  y
}

#' Unpack a Folded Data Frame
#'
#' Convert folded data.frame to unpacked format with scalar metadata as row entries.
#' @export
#' @family unpack
#' @return data.frame
#' @seealso \code{\link{fold.data.frame}} \code{\link{pack.folded}}
#' @param x folded
#' @param tolower whether to coerce attribute names to lower case
#' @param ... other arguments
#' @examples
#' data(eventsf)
#' library(metaplot)
#' head(unpack(eventsf))
#'
unpack.folded <- function(x, tolower = TRUE, ...)unpack(pack(x, tolower = tolower, ...), ...)

