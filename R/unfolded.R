#' Filter Unfolded
#' 
#' Filters unfolded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @return unfolded
#' @keywords internal                       
#' @export      
filter.unfolded <- function(.data, ...){
  x <- NextMethod()
  attr(x,'groups') <- attr(.data,'groups')
  class(x) <- union('unfolded',class(x))
  x
}

#' Group Unfolded
#' 
#' Groups unfolded. Calls next method, removes \code{groups} attribute and 'unfolded' class.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param add passed to next method
#' @return unfolded
#' @export 
#' @keywords internal                       
group_by.unfolded <- function(.data, ..., add = FALSE){
  x <- NextMethod()
  attr(x,'groups') <- NULL
  class(x) <- setdiff(class(x), 'unfolded')
  x
}

#' Anti-join Unfolded
#' 
#' Anti-joins unfolded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param .dots passed to next method
#' @return unfolded
#' @export 
#' @keywords internal                       
anti_join.unfolded <- function(.data, ..., .dots){
  x <- NextMethod()
  attr(x,'groups') <- attr(.data,'groups')
  class(x) <- union('unfolded',class(x))
  x
}

#' Semi-join Unfolded
#' 
#' Semi-joins unfolded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param .dots passed to next method
#' @return unfolded
#' @export 
#' @keywords internal                       
semi_join.unfolded <- function(.data, ..., .dots){
  x <- NextMethod()
  attr(x,'groups') <- attr(.data,'groups')
  class(x) <- union('unfolded',class(x))
  x
}

#' Ungroup Unfolded
#' 
#' Ungroups unfolded. Calls next method, removes \code{groups} attribute and 'unfolded' class.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param .dots passed to next method
#' @return unfolded
#' @export                        
#' @keywords internal                       
ungroup.unfolded <- function(.data, ..., .dots){
  x <- NextMethod()
  attr(x,'groups') <- NULL
  class(x) <- setdiff(class(x), 'unfolded')
  x
}

#' Mutate Unfolded
#' 
#' Mutates unfolded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param .dots passed to next method
#' @return unfolded
#' @export                        
#' @keywords internal                       
mutate.unfolded <- function(.data, ...){
  x <- NextMethod()
  attr(x,'groups') <- attr(.data,'groups')
  class(x) <- union('unfolded',class(x))
  x
}

#' Transmutate Unfolded
#' 
#' Transmutates unfolded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param .dots passed to next method
#' @return unfolded
#' @export                        
#' @keywords internal                       
transmutate.unfolded <- function(.data, ...){
  x <- NextMethod()
  attr(x,'groups') <- attr(.data,'groups')
  class(x) <- union('unfolded',class(x))
  x
}

#' Left-join Unfolded
#' 
#' Left-joins unfolded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param .dots passed to next method
#' @return unfolded
#' @export                        
#' @keywords internal                       
left_join.unfolded <- function(.data, ..., .dots){
  x <- NextMethod()
  attr(x,'groups') <- attr(.data,'groups')
  class(x) <- union('unfolded',class(x))
  x
}

#' Select Unfolded
#' 
#' Selects unfolded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @return unfolded
#' @import dplyr
#' @export                        
#' @keywords internal                       
select.unfolded <- function(.data, ...){
  x <- NextMethod()
  attr(x,'groups') <- attr(.data,'groups')
  class(x) <- union('unfolded',class(x))
  x
}

#' Arrange Unfolded
#' 
#' Arranges unfolded.
#' @param .data passed to next method
#' @param ... passed to next method
#' @param .dots passed to next method
#' @return unfolded
#' @export
#' @keywords internal                    
arrange.unfolded <- function(.data, ...){
  x <- NextMethod()
  attr(x,'groups') <- attr(.data,'groups')
  class(x) <- union('unfolded',class(x))
  x
}
