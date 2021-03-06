#' Analysis Dataset of Pharmacological Events
#'
#' A dataset in a format typical for nonlinear mixed effects modeling
#' of clinical trial data.  
#'
#' @format A data frame with 9 rows and 14 variables:
#' \describe{
#'   \item{C}{comment flag: ignore record if C is defined}
#'   \item{ID}{integer subject identifier}
#'   \item{USUBJID}{universal subject identifier}
#'   \item{TIME}{time (h)}
#'   \item{EVID}{event type identifier: 0 is pharmacokinetic observation, 1 is dose}
#'   \item{CMT}{physiological compartment: 1: gut, 2: central, 4: metabolite}
#'   \item{AMT}{dose amount of imaginary drug (mg)}
#'   \item{DV}{dependent value: plasma drug concentration (ng/mL) }
#'   \item{BLQ}{DV is below limit of quantification of BLQ is 1}
#'   \item{LLOQ}{lower limit of quantification (ng/mL) }
#'   \item{TAD}{time after dose (time since most recent dose) (h) }
#'   \item{SEX}{0: female, 1: male}
#'   \item{WT}{weight (kg) }
#'   \item{PRED}{model prediction of plasma drug concentration (ng/mL)}
#' }
#' @source Completely fabricated by the author.
"events"