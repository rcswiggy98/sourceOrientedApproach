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


  #returns region
  getRegion <- function(states){
    Northeast = c("ME", "NH", "VT", "NY", "PA", "DE", "NJ", "MD", "DC", "VA", "MA", "CT", "RI")
    IndustrialMidwest = c("WV", "OH", "KY", "IN", "IL", "WI", "MI")
    Southeast = c("FL", "GA", "SC", "NC", "TN", "AL", "MS", "AR","LA")
    UpperMidwest = c("MN", "IA", "MO", "KS", "NE", "SD", "ND")
    Southwest = c("TX", "OK", "NM", "AZ")
    SouthernCalifornia = c("CA")
    Northwest = c("NV", "UT", "CO", "WY", "MT", "ID", "OR", "WA")
    regions <- ifelse(states %in% Northeast,"Northeast",
                      ifelse(states %in% IndustrialMidwest, "IndustrialMidwest",
                             ifelse(states %in% Southeast, "Southeast",
                                    ifelse(states %in% UpperMidwest, "UpperMidwest",
                                           ifelse(states %in% Southwest, "Southwest",
                                                  ifelse(states %in% SouthernCalifornia, "SouthernCalifornia",
                                                         ifelse(states %in% Northwest,"Northwest", NA)))))))
    return(regions)
  }

  zipcode$region <- getRegion(zipcode$state)

  return(zipcode)
}
