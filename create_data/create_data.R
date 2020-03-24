## gather datasets
library( data.table)

## ======================================================== ##
#             PM2.5
## ======================================================== ##
file.start <- '/Users/lhenneman/Dropbox/Harvard/ARP/Data_Exposure/pm25_month_zips2/pm25_zips2005_'
pm2005 <- rbindlist( lapply( 1:12,
                             function( m){
                               M <- formatC( m, width = 2, flag = '0')
                               file.in <- paste0( file.start, M, '.csv')
                               data.in <- fread( file.in, keepLeadingZeros = T, drop = 'V1')
                               return( data.in[, .( ZIP, STATE, month, PM25)])
                             }))
pm25_2005 <- pm2005[, .(PM25 = mean( PM25)), by = .( ZIP, STATE)]
save( pm25_2005, file = '~/Dropbox/Rpackages/sourceOrientedApproach/data/pm25_2005.RData')

## ======================================================== ##
#             Sulfate (where is it?)
## ======================================================== ##
file.start <- '/Users/lhenneman/Dropbox/Harvard/ARP/Data_Exposure/so4_zips2/so4_zips2005_'
so42005 <- rbindlist( lapply( 1:12,
                             function( m){
                               M <- formatC( m, width = 2, flag = '0')
                               file.in <- paste0( file.start, M, '.csv')
                               data.in <- fread( file.in, keepLeadingZeros = T, drop = 'V1')
                               return( data.in[, .( ZIP, STATE, month, SO4)])
                             }))
so4_2005 <- so42005[, .(SO4 = mean( SO4)), by = .( ZIP, STATE)]
save( so4_2005, file = '~/Dropbox/Rpackages/sourceOrientedApproach/data/so4_2005.RData')



## ======================================================== ##
#             HyADS
## ======================================================== ##
hyads_raw.f <- '~/Dropbox/Harvard/RFMeval_Local/HyADS/disperseR_hyads/zips_exposures_total_2005.fst'
hyads_pm25.f <- '~/Dropbox/Harvard/RFMeval_Local/HyADS/disperseR_hyads/zips_pm25_total_2005.fst'

hyads_raw <- read.fst( hyads_raw.f, as.data.table = TRUE, columns = c( 'ZIP', 'hyads'))[, year := 2005]
hyads_pm25 <- read.fst( hyads_pm25.f, as.data.table = TRUE)[, year := 2005]

save( hyads_raw, file = '~/Dropbox/Rpackages/sourceOrientedApproach/data/hyads_raw_2005.RData')
save( hyads_pm25, file = '~/Dropbox/Rpackages/sourceOrientedApproach/data/hyads_pm25_2005.RData')

#hyads_cmaq <- merge( hyads_pm25, cmaqddm2005, by = c( 'ZIP', 'year'))

## ======================================================== ##
#             CMAQ-DDM
## ======================================================== ##
cmaqddm2005 <- fread( '~/Dropbox/Harvard/RFMeval_Local/Comparisons_Intermodel/evaluate_RFMs_intermediates/CMAQ-DDM_zip_links.csv',
                  keepLeadingZeros = T, drop = 'V1')[year == 2005]

save( cmaqddm2005, file = '~/Dropbox/Rpackages/sourceOrientedApproach/data/cmaqddm2005.RData')












