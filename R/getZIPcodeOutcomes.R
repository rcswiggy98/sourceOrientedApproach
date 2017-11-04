#' Simulated MEDICARE outcome data
#'
#' This function returns a data table containing simulated
#' MEDICARE data aggregated to the ZIP code level.  IHD is the number
#' of ischemic heart disease hospitalizations in 2005 and person_years
#' is the number of observed person years.
#'
#' @param inputParameter1 A description of the input parameter \code{inputParameter1}
#' @param inputParameter2 A description of the input parameter \code{inputParameter2}
#'
#' @return output A data table containing ZIP code level IHD hospitalizations and person-years
#'
#' @examples
#' outcome <- getZIPcodeOutcomes()

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
