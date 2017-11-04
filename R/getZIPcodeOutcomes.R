#' Simulated MEDICARE outcome data
#'
#' A more detailed description of what the function is and how
#' it works. It may be a paragraph that should not be separated
#' by any spaces.
#'
#' @param inputParameter1 A description of the input parameter \code{inputParameter1}
#' @param inputParameter2 A description of the input parameter \code{inputParameter2}
#'
#' @return output A description of the object the function outputs
#'
#' @keywords keywords
#'
#' @export
#'
#' @examples
#' R code here showing how your function works

getZIPcodeOutcomes <- function(outcome = "IHD"){

  require(arepa)
  require(data.table)
  zipcode <- get_zip_codes() #these zip codes are the rows in the dataset
  n.zips <- nrow(zipcode)

  #Add some outcome here
  zipcode[ , IHD := as.integer(rnorm(n.zips, 30, 2))]
  zipcode[ , person_years := as.integer(rnorm(n.zips, 1000, 15))]

  return(zipcode[ , .(zip,IHD, person_years)])
}
