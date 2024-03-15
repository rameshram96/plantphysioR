## code to prepare `yield_data` dataset goes here
#' @title Example data
#' @description Yield data of rice in kg/ha under two different growth conditions
#' @format A data frame with 50 rows and 3 variables:
#' \describe{
#'   \item{\code{Genotype}}{character Genotype}
#'   \item{\code{Yp}}{integer Yield under control condition}
#'   \item{\code{Ys}}{integer Yield under drought condition}
#'}
#' @source Simulated data, no external source were used
#' @references No external reference
#' @example \dontrun{yield_data}
"yield_data"
usethis::use_data(yield_data, overwrite = TRUE)
