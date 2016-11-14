#' Data for a single time trend over three years
#'
#' A dataset of mean responses of study participants over a three years span.
#'
#' @format A data frame with 9 observations and 4 variables:
#' \describe{
#'   \item{cycle}{cycle number}
#'   \item{cycle_time}{time since the start of the cycle, in weeks}
#'   \item{start_time}{time since the start of the study, in weeks}
#'   \item{response}{outcome measure}
#' }
"mean_change"

#' Repeated measures data over three years
#'
#' A dataset of longitudinal responses of 36 study participants over a three years span.
#'
#' @format A data frame with 234 observations and 5 variables:
#' \describe{
#'   \item{id}{subject identifier}
#'   \item{cycle}{cycle number}
#'   \item{cycle_time}{time since the start of the cycle, in weeks}
#'   \item{start_time}{time since the start of the study, in weeks}
#'   \item{response}{outcome measure}
#' }
"indv_change"
