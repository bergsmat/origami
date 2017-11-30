#' Filter Folded
#' 
#' Filters folded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @return folded
#' @keywords internal                       
#' @export      
filter.folded <- function(.data, ...){
  x <- NextMethod()
  class(x) <- union('folded',class(x))
  x
}

#' Group Folded
#' 
#' Groups folded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param add passed to next method
#' @return folded
#' @export 
#' @keywords internal                       
group_by.folded <- function(.data, ..., add = FALSE){
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

#' Semi-join Folded
#' 
#' Semi-joins folded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param .dots passed to next method
#' @return folded
#' @export 
#' @keywords internal                       
semi_join.folded <- function(.data, ..., .dots){
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
mutate.folded <- function(.data, ...){
  x <- NextMethod()
  class(x) <- union('folded',class(x))
  x
}

#' Transmutate Folded
#' 
#' Transmutates folded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param .dots passed to next method
#' @return folded
#' @export                        
#' @keywords internal                       
transmutate.folded <- function(.data, ...){
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
#' @return folded
#' @import dplyr
#' @export                        
#' @keywords internal                       
select.folded <- function(.data, ...){
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
arrange.folded <- function(.data, ...){
  x <- NextMethod()
  class(x) <- union('folded',class(x))
  x
}
