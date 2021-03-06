#' fold: A Self-Describing Dataset Format and Interface
#'
#' The fold package defines a compact, table-based, tool-neutral data format 
#' designed to accommodate embedded metadata.  Not surprisingly, it also implements 
#' an interface for this format in R.  
#' 
#' The goal is to store metadata along with data. We do this by transforming 
#' tabular data into a folded format -- still a table, but with a META column 
#' that associates attributes (metadata) with the data items they describe.
#' 
#' This all works much better when the data is clean: that is, there are a set 
#' of grouping columns, the interaction of which makes each record unique. The 
#' fold package can guess a lot of things, but you need to specify the groups 
#' -- most general first.  
#' 
#' Here we supply a quick-start micro-vignette.  See also \code{\link{fold.data.frame}}.
#' 
#' @examples 
#' library(magrittr)
#' library(wrangle)
#' library(dplyr)
#' 
#' data(events)
#' x <- events
#' 
#' # Step 0.  Rename columns to remove semantic (non-syntactic) underscores.
#' 
#' # Step 1.  De-interlace the data.  Limit to a subset so that each column means only one thing. 
#' 
#' x %<>% filter(CMT == 2) %>% select(-EVID,-CMT,-AMT)
#' 
#' # Step 2.  Describe the groups (unique key).  Order is important. (Start with most general.)
#' 
#' x %<>% group_by(USUBJID, TIME)
#' x %>% status
#' 
#' # Step 3.  Supply metadata as values or factors. Hardcode or merge from source.
#' 
#' x %<>% mutate(
#'   ID_LABEL      = 'subject identifier',
#'   C_LABEL       = 'comment flag',
#'   USUBJID_LABEL = 'universal subject identifier',
#'   TIME_LABEL    = 'time',
#'   DV_LABEL      = 'parent drug',
#'   BLQ_LABEL     = 'below limit of quantitation',
#'   LLOQ_LABEL    = 'lower limit of quantitation',
#'   TAD_LABEL     = 'time since most recent dose',
#'   SEX_LABEL     = 'sex',
#'   WT_LABEL      = 'weight',
#'   PRED_LABEL    = 'population prediction'
#' )
#' 
#' x %<>% mutate(
#'   C_GUIDE       = factor(paste(C), exclude = NULL,
#'    levels       = c('NA','C'),
#'    labels       = c('not commented','commented')),
#'   TIME_GUIDE    = 'h',
#'   DV_GUIDE      = 'ng/mL',
#'   BLQ_GUIDE     = factor(BLQ,
#'    levels       = 0:1,
#'    labels       = c('not quantifiable','quantifiable')),
#'   LLOQ_GUIDE    = 'ng/mL',
#'   TAD_GUIDE     = 'h',
#'   SEX_GUIDE     = factor(SEX,
#'    levels       = 0:1,
#'    labels       = c('female','male')),
#'   WT_GUIDE      = 'kg',
#'   PRED_GUIDE    = 'ng/mL'
#' )
#' 
#' # Step 4. Fold and unfold your data.
#' 
#' x %>% fold
#' x %>% fold %>% unfold
#' x %>% fold %>% unfold %>% fold
#' x %>% fold %>% unfold(PRED,TIME,WT)
#' 
#' data(eventsf)
#' stopifnot(identical(x %>% fold, eventsf) )

#' @docType package
#' @name foldpkg
#' @aliases fold-package
NULL