globalVariables('VALUE')
globalVariables('LABEL')
globalVariables('GUIDE')
globalVariables('META')
globalVariables('VARIABLE')

.informative <- function (x, ...) x[, sapply(x, function(col) any(!is.na(col))), drop = FALSE]

#' Save Folded Format as CSV
#' 
#' Saves folded format as CSV.
#' 
#' Simply calls as.csv.data.frame.
#' 
#' @inheritParams csv::as.csv
#' @return invisible folded (x)
#' @import csv
#' @export
as.csv.folded <- function(x,...){
  class(x) <- data.frame
  csv::as.csv(x,...)
}


#' Coerce to Folded Format
#' 
#' Coerces to folded format.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
as.folded <- function(x,...)UseMethod('as.folded')

#' Coerce to Folded Format from Filepath
#' 
#' Coerces to folded format from character, treating \code{x} as a filepath.
#' 
#' @param x length-one character
#' @param ... passed arguments
#' @return folded
#' @import csv
#' @export
as.folded.character <- function(x,...){
  y <- csv::as.csv(x,...)
  y <- as.folded(y,...)
  y
}
#' Coerce to Folded Format from Folded Format
#' 
#' Coerces to folded format from folded.  A non-operation.
#' 
#' 
#' @inheritParams as.folded
#' @return folded
#' @export
#' @keywords internal
as.folded.folded <- function(x,...)x


#' Coerce to Folded from Data Frame
#' 
#' Coerces to folded from data.frame.
#' 
#' Expects columns VARIABLE, META, and VALUE. Remaining columns are for classification and may be NA. Coerces VALUE to character. Removes duplicate records with warning. Sorts on non-value columns by default.
#' 
#' @inheritParams as.folded
#' @param sort Should the result be sorted?
#' @return folded
#' @import dplyr
#' @export
as.folded.data.frame <- function(x, sort = TRUE, ...){
  constitutive <- c('VARIABLE','META','VALUE')
  extras <- setdiff(names(x),constitutive)
  last <- if(length(extras)) rev(extras)[[1]] else character(0)
  stopifnot(
    all(constitutive %in% names(x))
  )
  x <- x[,c(constitutive,extras),drop=FALSE]
  if(any(duplicated(x))){
    warning('removing duplicates')
    x <- unique(x)
  }
  if(sort) x <-  dplyr::arrange_(x,.dots=setdiff(names(x),'VALUE'))
  VALUE <- NULL # squelch R CMD CHECK NOTE
  d <- dplyr::select(x,-VALUE)
  d <- d[duplicated(d),,drop=FALSE]
  if(nrow(d)){
    eg <- d[1,,drop=FALSE]
    nms <- paste(names(eg),collapse=', ')
    eg <- do.call(paste,c(as.list(eg),list(collapse=', ')))
    warning('removing duplicates of ',nms,' e.g. ',eg)
  }
  d <- dplyr::select(x,-VALUE)
  if(ncol(d) > 2) d <- d[,1:(ncol(d) - 1),drop=FALSE] # select all but last
  d <- d[duplicated(d),,drop=FALSE]
  if(!nrow(d)){
    col <- names(d)[ncol(d)]
    warning('records not duplicated even without ',col)
  }
  class(x) <- union('folded',class(x))
  x
}


#' Distill a Component of an Object
#' 
#' Distills a component of an object.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
distill <- function(x,...)UseMethod('distill')

#' Distill a Variable of Folded
#' 
#' Distills a variable of folded.
#' 
#' @inheritParams distill
#' @export
#' @keywords internal
distill.folded <- function(x,...){
  class(x) <- setdiff(class(x),'folded')
  distill(x,...)
}



#' Distill a Variable of a Data Frame
#' 
#' Distills a variable of a data.frame.
#' 
#' Everything known about variable x, recursively mining metadata.
#' @inheritParams distill
#' @param mission character
#' @param parent character
#' @import dplyr
#' @importFrom tidyr spread
#' @importFrom tidyr spread_
#' @export
#' @keywords internal

distill.data.frame <- function(
  x,
  mission,
  parent=character(0),
  ...
){
  res=data.frame()
  data <-  dplyr::filter(x, VARIABLE == mission & META %>% is.na)
  if(nrow(data)) {
    data <- tidyr::spread(data,VARIABLE,VALUE,convert=TRUE)
    data <- dplyr::select(data,-META) 
    data <- .informative(data)
    res <- data
  }
  meta <-  dplyr::filter(x, VARIABLE == mission & is.defined(META))
  if(nrow(meta)){
    for(m in unique(meta$META)){
     # message('processing attribute ',m)
      me <- meta
      me <- dplyr::filter(me, META == m) 
      me <- tidyr::spread(me, META,VALUE) 
      me <- dplyr::select(me, -VARIABLE)
      me <-  .informative(me)
      lineage <- c(parent,mission)
      canonical <- c(lineage,m)
      canonical <- paste(canonical,collapse='_')
      names(me)[names(me) == m] <- canonical
      mo <- distill.folded(x,mission=m,parent=lineage,...)
      me <- weld(me, mo)
      enc <- all(encoded(me[[canonical]])) & length(me[[canonical]]) == 1
      if(!nrow(res))res <- me
      if(nrow(res) & !enc) res <- weld(res,me)
      if(nrow(res) &  enc) res <- decode(res,
        encoded=mission,
        encoding=me[[canonical]][[1]],
        decoded = canonical,
        ...
      )
    }
  }
  res
}


#' Unfold an Object
#' 
#' Unfolds an object.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
unfold <- function(x,...)UseMethod('unfold')

#' Unfold an Object, Standard Evaluation
#' 
#' Unfolds an object, standard evaluation.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
unfold_ <- function(x,...)UseMethod('unfold_')

#' Unfold a Folded Data.frame
#' 
#' Unfolds a folded data.frame, or part thereof.
#' 
#' By default, the entire data.frame is unfolded, possibly giving back something originally passed to fold().  If \dots is specified, only selected items are unfolded.  Values stored as encodings are converted to factor.
#' 
#' @param x folded data.frame
#' @param ... variables to unfold, given as unquoted names
#' @seealso fold_.data.frame
#' @return data.frame
#' @export
unfold.folded <- function(  
  x,
  ...
)unfold_(x, var = dots_capture(...))
  
#' Unfold a Folded Data.frame, Standard Evaluation
#' 
#' Unfolds a folded data.frame, or part thereof, using standard evaluation.
#' 
#' By default, the entire data.frame is unfolded, possibly giving back something originally passed to fold().  If \code{var} is specified, only selected items are unfolded.  Values stored as encodings are converted to factor.
#' 
#' @param x folded data.frame
#' @param var variables to unfold, given as unquoted names
#' @return data.frame
#' @keywords internal
#' @seealso fold.data.frame
#' @export
unfold_.folded <- function(  
  x,
  var
){
  if(length(var) == 0) var = unique(x$VARIABLE[is.na(x$META)])
  groups <- setdiff(names(x),c('VARIABLE','META','VALUE'))
  y <- lapply(var,function(v)distill(x,mission=v,...))
  z <- metaMerge(y)
  groups <- intersect(groups,names(z))
  z <- dplyr::group_by_(z,.dots=groups)
  z
}

#' Filter Folded
#' 
#' Filters folded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param .dots passed to next method
#' @return folded
#' @keywords internal                       
#' @export      
                  
filter_.folded <- function(.data, ..., .dots){
  x <- NextMethod()
  class(x) <- union('folded',class(x))
  x
}
#' Group Folded
#' 
#' Groups folded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param .dots passed to next method
#' @return folded
#' @export 
#' @keywords internal                       
group_by_.folded <- function(.data, ..., .dots){
  x <- NextMethod()
  class(x) <- union('folded',class(x))
  x
}
#' Anti-join Folded
#' 
#' Anti-joins folded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param .dots passed to next method
#' @return folded
#' @export 
#' @keywords internal                       
anti_join.folded <- function(.data, ..., .dots){
  x <- NextMethod()
  class(x) <- union('folded',class(x))
  x
}
#' Ungroup Folded
#' 
#' Ungroups folded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param .dots passed to next method
#' @return folded
#' @export                        
#' @keywords internal                       
ungroup.folded <- function(.data, ..., .dots){
  x <- NextMethod()
  class(x) <- union('folded',class(x))
  x
}
#' Mutate Folded
#' 
#' Mutates folded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param .dots passed to next method
#' @return folded
#' @export                        
#' @keywords internal                       
mutate_.folded <- function(.data, ..., .dots){
  x <- NextMethod()
  class(x) <- union('folded',class(x))
  x
}
#' Left-join Folded
#' 
#' Left-joins folded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param .dots passed to next method
#' @return folded
#' @export                        
#' @keywords internal                       
left_join.folded <- function(.data, ..., .dots){
  x <- NextMethod()
  class(x) <- union('folded',class(x))
  x
}
#' Select Folded
#' 
#' Selects folded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param .dots passed to next method
#' @return folded
#' @import dplyr
#' @export                        
#' @keywords internal                       
select_.folded <- function(.data, ..., .dots){
  x <- NextMethod()
  class(x) <- union('folded',class(x))
  x
}
#' Arrange Folded
#' 
#' Arranges folded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param .dots passed to next method
#' @return folded
#' @export
#' @keywords internal                    
arrange_.folded <- function(.data, ..., .dots){
  x <- NextMethod()
  class(x) <- union('folded',class(x))
  x
}

#' Fold an Object
#' 
#' Folds an object.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
fold <- function(x, ... )UseMethod('fold')

#' Fold a Data Frame
#' 
#' Folds a data.frame.  Stacks columns, while isolating metadata and capturing keys.
#' 
#' A Folded data.frame is formalized re-presentation of a conventional data.frame.  Items in the conventional form are of three conceptual types: data, metadata, and keys.  Data items contain the primary values, to be described.  Metadata gives additional details about the data items or values. Keys are grouping items; combinations of grouping values should uniquely identify each conventional record.
#' 
#' In the result, names of data items appear in VARIABLE, while values of data items are stacked in VALUE. Data items are all columns from the input not otherwise identified as metadata or keys.
#' 
#' Likewise, names of metatdata items appear in META, while the name of the described data item appears in VARIABLE.  Values of metadata items appear in VALUE.  If the item is a factor, the metadata VALUE will be an encoding (see package: encode). Metadata items are identified explicitly using a list of formulas, or implicitly by means of column naming conventions.
#' 
#' Grouping items in the input persist in the result and serve as keys.  Both data and metadata values may have keys, but neither require them.  Keys are identified explicitly by supplying a group_by argument (or unnamed, unquoted arguments for non-standard evaluation), or implicitly when \code{x} is a grouped data.frame (\code{grouped_df}).   By default, superflous keys (those that do not help distinguish data items) are removed on a per-data-item basis.
#'
#' Note that metadata items may describe other metadata items, recursively.  In practice, however, such complexity could be problematic and is best avoided if possible.
#' 
#' There are cases where a real grouping item may appear superfluous, e.g. for a one-record dataset.  Enforce the groups by setting \code{simplify} to FALSE. 
#' 
#' The folded format supports mixed object types, as inferred from differences in relevant grouping items on a per record basis.  Mixed typing works best when object types form a nested hierarchy, i.e. all keys are left-subsets of the full key. Thus the order of grouping values is considered informative, e.g. for sorting.

#' @param x data.frame
#' @param ... unquoted names of grouping columns; overrides \code{group_by}
#' @param group_by a vector of column names serving as key: included in result but not stacked
#' @param meta a list of formulas in the form object ~ attribute. Pass something with length 0 to suppress guessing.
#' @param simplify set to NA any group_by values that do not help distinguish values, and remove resulting duplicate records
#' @param sort whether to sort the result
#' @return folded data.frame with columns VARIABLE, META, VALUE and any supplied grouping items.
#' @seealso \code{\link{obj_attr.data.frame}} \code{\link{fold_.data.frame}}
#' @export
fold.data.frame <- function(
  x,
  ... ,
  group_by = groups(x),
  meta = obj_attr(x),
  simplify = TRUE,
  sort = TRUE
){
  fold_.data.frame(
    x,
    args = dots.capture(...),
    group_by = group_by,
    meta = meta,
    simplify = simplify,
    sort = sort
  )
}
#' Fold an Object, Standard Evaluation
#' 
#' Folds an object using standard evaluation.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
fold_ <- function(x, ... )UseMethod('fold_')

#' Fold a Data.frame, Standard Evaluation
#' 
#' Folds a data.frame using standard evaluation. See also \code{\link{fold.data.frame}}.
#' @param x data.frame
#' @param args list of formulas representing grouping columns
#' @param group_by a vector of column names serving as key: included in result but not stacked
#' @param meta a list of formulas in the form object ~ attribute. Pass something with length 0 to suppress guessing.
#' @param simplify set to NA any group_by values that do not help distinguish values, and remove resulting duplicate records
#' @param sort whether to sort the result
#' @return folded data.frame with columns VARIABLE, META, VALUE and any supplied grouping items.
#' @import magrittr dplyr
#' @importFrom tidyr spread
#' @importFrom tidyr spread_
#' @importFrom tidyr gather
#' @importFrom tidyr gather_
#' @seealso \code{\link{obj_attr.data.frame}} \code{\link{fold.data.frame}}
#' @keywords internal
#' @export

fold_.data.frame <- function(
  x,
  args,
  group_by = groups(x),
  meta = obj_attr(x),
  simplify = TRUE,
  sort = TRUE
){
  if(length(group_by)) x <- group_by_(x, group_by)
  if(length(args))args <- args[is.na(names(args))]
  if(length(args))x <- group_by(x, args)
  if(length(groups(x)) == 0)warning('Nothing to group by.  Do you need to supply groups?')
  # meta
  VARIABLE <- sapply(meta,function(f)f %>% as.list %>% `[[`(2) %>% as.character)
  META     <- sapply(meta,function(f)f %>% as.list %>% `[[`(3) %>% as.character)
  COL      <- names(meta)
  table <- cbind(VARIABLE,META,COL) %>% data.frame(stringsAsFactors = FALSE)
  # data
  d <- x[,setdiff(names(x),COL),drop=F]
  d <- tidyr::gather_(d,'VARIABLE','VALUE',setdiff(names(d),group_by))
  d <- mutate(d,META=NA_character_)
  d <- mutate(d,VALUE = VALUE %>% as.character)
  d <- as.folded(d)
  if(simplify) d <- unique(reduce(d))
  if(nrow(table)){
    m <- x
    for(i in seq_along(m))if(is.factor(m[[i]]))m[[i]] <- as.character(m[[i]]) 
    # prevent tidyr warning about differing attributes.  
    # Factor levels to be recovered later from x.
    m %<>% 
      dplyr::select_(.dots=c(group_by,COL)) %>% 
      tidyr::gather_('COL','VALUE',COL) %>%
      unique
    m <- left_join(m,table,by='COL') %>% dplyr::select(-COL)
    m <- dplyr::select_(m,.dots=c('VARIABLE','META','VALUE',group_by))
    m <- as.folded(m)
    for(i in 1:nrow(table)){
      var <- table[i,'VARIABLE']
      met <- table[i,'META']
      col <- table[i,'COL']
      if(is.factor(x[[col]])){
        decodes <- levels(x[[col]])
        codes <- x[[var]][match(decodes,x[[col]])]
        encoding <- encode(codes,decodes)
        m$VALUE[m$VARIABLE == var & m$META == met] <- encoding
      }
    }
    if(simplify) m <- unique(reduce(m))
    m <- dplyr::select_(m,.dots=c('VARIABLE','META','VALUE',group_by))
    d <- bind_rows(m,d)
  }
  d <- as.folded(d, sort = sort, ...)
  d
}
  

#' Infer Object-Attribute Relationships
#' 
#' Infers object-attribute relationships.
#' @param x character
#' @param ... passed arguments
#' @export
#' @keywords internal
obj_attr <- function(x,...)UseMethod('obj_attr')

#' Infer Object Attribute Relationships from Character
#' 
#' Infers object ~ attribute relationships from character.
#' 
#' Character is, for example, the names of a data.frame.  Processes the names, splitting on underscore and converting to formulas, with names same as the values in x that contained any underscores.
#' @inheritParams obj_attr
#' @return a named list of formulas in the form object ~ attribute
#' @export
#' @keywords internal
obj_attr.character <- function(x,...){
  x <- x[grepl( '_.',x)]
  y <- strsplit(x,'_') # all these should have two elements
  z <- lapply(y,paste,collapse='~')
  z <- lapply(z,as.formula)
  names(z) <- x
  z
}

#' Infer Object Attribute Relationships from Data Frame
#' 
#' Infers object ~ attribute relationships from data.frame.  Processes the names of a data.frame.
#' @inheritParams obj_attr
#' @return a list of formulas in the form object ~ attribute
#' @import encode
#' @seealso \code{\link{obj_attr.character}}
#' @keywords internal
#' @export
obj_attr.data.frame <- function(x,...)obj_attr(names(x),...)

print.folded <- function(x,...){
  x[] <- lapply(x,shortOrNot)
  NextMethod()
}

shortOrNot <- function(x){
  if(!is.character(x)) return(x)
  if(any(nchar(x[is.defined(x)]) > 8)){
    if(any(encoded(x[is.defined(x)]))){
      return(short(x))
    }
  }
  return(x)
}

short <- function(x){
  y <- substr(x,1,8)
  nchar <- nchar(x)
  y <- paste0(y,ifelse(nchar>8,'...',''))
  y <- as.character(y)
  y
}

weld <- function(x,y,...){
  stopifnot(
    x %>% inherits('data.frame'),
    y %>% inherits('data.frame')
  )
  if(!nrow(x) & !nrow(y))return(x)
  if(!nrow(x) &  nrow(y))return(y)
  if( nrow(x) & !nrow(y))return(x)
  if( nrow(x) &  nrow(y))merge(x,y,all=T)
}

decode.data.frame <- function(
  x, 
  encoded, 
  encoding,
  decoded,
  ...
){
  stopifnot(length(encoding) == 1)
  if(!encoded %in% names(x))return(x)
  if(decoded %in% names(x)){
    warning(decoded,' already present, skipping decode ')
    return(x)
  }
  #if(length(unique(x[[encoding]])) > 1)warning(encoding, 'unsing only the first value of ',encoding)
  #enc <- x[1,encoding]
  if(!encoded(encoding)){
    warning(encoding, ' appears not to be encoded, no decode attempted')
    return(x)
  }
  codes <- codes(encoding)
  decodes <- decodes(encoding)
  x[[decoded]] <- map(x[[encoded]], from=codes, to = decodes)
  x[[decoded]] <- factor(x[[decoded]],levels=decodes)
  #x[[encoded]] <- factor(x[[encoded]],levels=codes)
  x
}

reduce <- function(x,...)UseMethod('reduce')
reduce.folded <- function(x,ignore=character(0),protect=FALSE,...){
  classifiers <- setdiff(names(x),c('VARIABLE','META','VALUE'))
  classifiers <- setdiff(classifiers,ignore)
  if(!length(classifiers))return(x)
  test <- rev(classifiers)[[1]]
  remaining <- setdiff(classifiers,test)
  y <- x %>% dplyr::group_by_(.dots=c('VARIABLE','META',remaining))
  y <- mutate(y, count = length(unique(VALUE)))
  y$protect <- protect
  target <- y$count == 1 & !y$protect
  y[[test]][target] <- NA
  y$count <- NULL
  y$protect <- NULL
  reduce.folded(y,ignore=c(test,ignore),protect = !target, ...)
}

is.defined <- function(x,...)!is.na(x)
map <- function (x, from, to, strict = TRUE, ...) 
{
  stopifnot(length(to) == length(from))
  res <- to[match(x, table = from)]
  if (!strict) 
    res[!(x %in% from)] <- x[!(x %in% from)]
  res
}

unique.folded <- function(x, incomparables = FALSE,...){
  y <- unique.data.frame(x)
  class(y) <- union('folded',class(y))
  y
}

#  #' Interpret something
#  #'
#  #' Interprets something.
#  #'
#  #' @param x object
#  #' @param ... passed arguments
#  #' @export
#  #' @keywords internal
#  interpret <- function(x,...)UseMethod('interpret')
#  
#  #' Interpret Data in Folded Format
#  #'
#  #' Interprets data in folded format.  Specifically, substitutes decodes for codes
#  #' when presenting encoded variables.
#  #' @inheritParams interpret
#  #' @importFrom stats as.formula
#  #' @importFrom utils read.table
#  #' @return folded data.frame
#  #' @export
#  interpret.folded <- function(x,...){
#    meta <- x %>%
#      dplyr::filter(META %in% c('GUIDE','LABEL')) %>%
#      tidyr::spread(META,VALUE) %>%
#      dplyr::select(VARIABLE,GUIDE,LABEL) %>%
#      mutate(encoded = encoded(GUIDE))
#    x <- dplyr::filter(x,!META %in% c('GUIDE','LABEL'))
#    x <- left_join(x,meta,by='VARIABLE')
#    x <- mutate(x,VARIABLE = if_else(
#      encoded,
#      LABEL,
#      paste(LABEL,paste0('(',GUIDE,')'))
#    )) %>% dplyr::select(-encoded,-LABEL)
#    x <-
#      dplyr::group_by(x,VARIABLE) %>%
#      mutate(VALUE = decode(VALUE,GUIDE[[1]])) %>%
#      ungroup %>%
#      dplyr::select(-GUIDE)
#    x
#  }

metaMerge <- function(x,...)UseMethod('metaMerge')
metaMerge.list <- function(x,all=TRUE,...){
  if(length(x)==0)return(x)
  if(length(x)==1)return(x[[1]])
  metaMerge(x=metaMerge(x[-length(x)]),y=x[[length(x)]],all=all,...)
}
metaMerge.character <- function(x,import=read.table,all=TRUE,...){
  miss <- x[!file.exists(x)]
  if(length(miss))stop('cannot find, e.g.,',miss[[1]])
  import <- match.fun(import)
  x <- lapply(x,import,...)
  metaMerge(x,all=all,...)
}
metaMerge.default <- function(x,y,all=TRUE,...)merge(x,y,all=all,...)
metaMerge.data.frame <- function(x,y,all=TRUE,...){
  if(is.null(y))warning('merging data.frame with NULL object')
  merge(x,y,all=all,...)
}
metaMerge.NULL <- function(x,y,all=TRUE,...){
  warning('merging NULL object')
  merge(x,y,all=all,...)
}
