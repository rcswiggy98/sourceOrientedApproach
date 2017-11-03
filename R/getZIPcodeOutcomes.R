#' A one sentence description of what your function does
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

getZIPcodeOutcomes <- function(year = 2005){

    print("Getting MEDICARE data...")

    ##----- Read in (real or simulated) annual zip code level Medicare data
    BIGMED <- fread("/Users/kfcummiskey/Data/death_Num_den_zipcode-09-03-2015.csv")
    ## -- Notes on the Denominator Variables in Medicare:
    #   - Person_year_FFS: use this for the hospitalization outcomes, which are only observed for FFS beneficiaries
    #   - Total_den_FFS: use this as the denominator for the _rate variables (e.g., to get the number of Females calculate Female_rate*Total_den_FFS)
    #   - Total_den_for_death_MA_FFS: use this as the denominator for Total_death_FFS_MA, since death is measured for all FFS and managed care beneficiaries

    # Use only 2003 data for now
    outcome.year = year
    BIGMED <- subset(BIGMED, year == outcome.year)

    ## -----  Make a separate data set for each condition
    conditions = names(with(BIGMED, table(Condition)))
    condition_list = list()
    for (i in 1:length(conditions)){
      condition_list[[i]] = subset(BIGMED, Condition == conditions[i])
      condition_list[[i]] = condition_list[[i]][, c("zipcode_R", "year", "Total_admission_FFS"), with = FALSE]
      setnames(condition_list[[i]], "Total_admission_FFS", conditions[i])
    }

    ## ----- Now make a data set for all_cause death and covariates, which all get their on fields (and are repeated for each value of 'Condition').
    ## ----- Do this by taking only unique zip/year combinations
    setkeyv(BIGMED, c("zipcode_R", "year"))
    BIGMED_unique_zip_year = unique(BIGMED)
    BIGMED_unique_zip_year = BIGMED_unique_zip_year[, !(names(BIGMED_unique_zip_year) %in% c("Condition", "Total_admission_FFS")), with = FALSE]

    uzips = rep(NA, length(condition_list))
    for (i in 1:length(condition_list))
      uzips[i] = length(unique(condition_list[[i]]$zipcode_R))


    MEDICARE = merge(BIGMED_unique_zip_year, condition_list[[1]], by = c("zipcode_R", "year"), all.x = TRUE)
    for (i in 2:length(condition_list))
      MEDICARE = merge(MEDICARE, condition_list[[i]], by = c("zipcode_R", "year"), all.x = TRUE)

    ## -- Unscramble the zip code (zip code is scrambled for data storage purposes)
    temp_zip<-MEDICARE$zipcode_R
    temp_zip<-formatC(temp_zip, width = 5, format = "d", flag = "0")
    zip<-unlist(lapply(temp_zip, function(x) as.numeric(paste(rev(strsplit(as.character(x),"")[[1]]),collapse=""))))
    zip<-formatC(zip, width = 5, format = "d", flag = "0")
    MEDICARE = cbind(MEDICARE, zip)

    MEDICARE <- MEDICARE[ , None := NULL]
    MEDICARE <- MEDICARE[ , excluded := NULL]
    MEDICARE <- MEDICARE[ , HRP := NULL]

    #Replace NAs with zero
    for (i in names(MEDICARE)) {MEDICARE[is.na(get(i)), (i):=0]}

    MEDICARE <- unique(MEDICARE)
    MEDICARE <- MEDICARE[ , c(-1,-2)]

    #...add in zipcode with missing data
    MEDICARE <- merge(zipcode, MEDICARE, by = "zip", all.x = TRUE, all.y = FALSE)
    MEDICARE <- data.table(MEDICARE[ , -1*2:6])
    setkey(MEDICARE, "zip")
}
