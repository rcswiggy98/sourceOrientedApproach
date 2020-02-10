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



## ======================================================== ##
#             CMAQ-DDM
## ======================================================== ##
cmaqddm2005 <- fread( '~/Dropbox/Harvard/RFMeval_Local/Comparisons_Intermodel/evaluate_RFMs_intermediates/CMAQ-DDM_zip_links.csv',
                  keepLeadingZeros = T, drop = 'V1')[year == 2005]

save( cmaqddm2005, file = '~/Dropbox/Rpackages/sourceOrientedApproach/data/cmaqddm2005.RData')












