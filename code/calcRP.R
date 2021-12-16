########## This files contains the code to calculate rank percentile indicators based on the citation dataset ##########
########## The calculations rely on the data.table package, which supports multithreads that speed things up  ##########
########## The results are saved in the "data/rp/" folder                                                     ##########
########################################################################################################################

## base directories
setwd('SciImpactRanking/')
base = getwd() 

## source the R packages and functions
source(paste0(base, '/code/utils.R'))
setDTthreads(4) # change the number of threads for data.table based on your system

## read data
## read the data
citations = read.csv(paste0(base,'/data/raw/citations.csv'))
authors = read.csv(paste0(base, '/data/raw/authors.csv'), stringsAsFactors = FALSE)

### Calculate the rank percentiles --------
## publication rank percentile
DT_pub = calc.pubrp(citations, authors)
saveRDS(DT_pub, paste0(base, '/data/rp/pub.rds'))
## scholar rank percentile
DT_aut = calc.autrp(DT_pub)
saveRDS(DT_aut, paste0(base, '/data/rp/aut.rds'))
## scholar rank percentile based on just publications between age t1 and t2
DT_aut_future = calc.autrp.future(DT_pub, DT_aut)
saveRDS(DT_aut_future, paste0(base, '/data/rp/aut_future.rds'))


### Calculate author rank percentiles based on median publication impacts --------
DT_aut_median = calc.autrp(DT_pub, fun.agg=median)
saveRDS(DT_aut_median, paste0(base, '/data/rp/aut_median.rds'))
DT_aut_future_median = calc.autrp.future(DT_pub, DT_aut, fun.agg=median)
saveRDS(DT_aut_future_median, paste0(base, '/data/rp/aut_future_median.rds'))
