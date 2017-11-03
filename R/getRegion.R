#' Get US Region from state
#'
#' Returns the United States Region given the two letter state abbreviation
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
#' getRegion("NY")

getRegion <- function(state){
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
