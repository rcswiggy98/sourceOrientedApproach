#' Get US ZIP Code Covariates
#'
#' Return a data table consisting of CENSUS 2000 variables, smoking rates,
#' and weather variables for every US ZIP code
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

getZIPcodeCovariates <- function(){

  require(arepa)
  require(data.table)
  require(noncensus)
  zipcode <- get_zip_codes() #these zip codes are the rows in the dataset
  data("zip_codes") #these zip codes contain the fips code

  #add fips code to larger zip code data frame
  zipcode <- data.table(merge(zipcode, zip_codes[ ,c(1,6)], by = "zip", all.x = TRUE))
  setkey(zipcode, zip)

  #Add census data
  load(file = "data/CENSUS2000.RData")
  zipcode <- merge(zipcode, CENSUS, by.x = "zip", by.y = "ZIP")

  #Add county level smoking rates
  load(file = "data/smoking.RData")
  zipcode <- merge(zipcode, smoking, by.x = "fips", by.y = "FIPS")

  #Add weather data here


  zipcode$region <- getRegion(zipcode$state)

  return(zipcode)
}
