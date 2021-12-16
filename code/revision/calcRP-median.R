########## This files contains the code to calculate rank percentile indicators based on the citation dataset ##########
########## The calculations rely on the data.table package, which supports multithreads that speed things up  ##########
########## The results are saved in the "data/rp/" folder                                                     ##########
########################################################################################################################

## base directories
setwd('SciImpactRanking/')
base = getwd() 

## source the R packages and functions
source(paste0(base, '/code/utils.R'))

### Calculate the rank percentiles for publications --------
calc.pubrp <- function(){
  ## read the data
  citations = read.csv(paste0(base,'/data/raw/citations.csv'))
  authors = read.csv(paste0(base, '/data/raw/authors.csv'), stringsAsFactors = FALSE)
  
  DT = data.table(citations)
  ## process the data
  # citations in 2017 are incomplete, only takes citaiton info until 2016
  DT[, `:=`(year = start+age-1)]
  DT = DT[year <= 2016, ]
  # take publications that have at least 10 citations
  DT_tmp = DT[, sum(citations), by='pub.id']
  DT = DT[pub.id %in% DT_tmp$pub.id[DT_tmp$V1 >=10], ]
  # remove the publications (false positives) published before the start of careers given in the authors (tenured) dataset
  DT_tmp = DT[, list(start.aut = min(start)), by='aut.id']
  DT_tmp = merge(DT_tmp, authors[,c('aut.id', 'start.tenured')], by='aut.id', all.x=TRUE)
  DT_tmp$start.aut = DT_tmp[, list(start.aut = ifelse(is.na(start.tenured), start.aut, start.tenured))]
  DT_tmp$start.tenured = NULL
  DT = merge(DT, DT_tmp, by='aut.id', all.x=TRUE)
  DT = DT[start >= start.aut, ]
  DT$start.aut = NULL
  # put zero citations to years where a publication doesn't get any citations (ignored in the raw dataset)
  DT_tmp = DT[, list(aut.id=unique(aut.id), age=setdiff(seq(1, 2016-unique(start)+1), age), citations=0, start=unique(start)), by='pub.id']
  DT_tmp = DT_tmp[!is.na(age), ]
  DT_tmp[, `:=`(year = start+age-1)]
  DT = rbind(DT, DT_tmp)
  DT = DT[order(pub.id, age),]
  
  ## calculate the rank percentiles when benchmark is all
  # cumulative citations
  DT[, `:=`(cum.citations = cumsum(citations)), by='pub.id']
  # P.C.all means publication rank percentile indicators based on citations and benchmark is all
  DT[, `:=`(P.c.all = perc.rank(cum.citations)), by='age']
  
  ## calculate the rank percentiles when the benchmark is biology
  DT_tmp = DT[aut.id %in% authors$aut.id[authors$is.bio == 1], ]
  DT_tmp[, `:=`(P.c.bio = perc.rank(cum.citations)), by='age']
  # merge into one dataset 
  DT = merge(DT, DT_tmp[, c('aut.id', 'pub.id', 'age', 'P.c.bio')], by=c('aut.id', 'pub.id', 'age'), all.x = TRUE)
  
  ## calculate the rank percentiles when the benchmark is tenured professors
  DT_tmp = DT[aut.id %in% authors$aut.id[authors$is.tenured == 1], ]
  DT_tmp[, `:=`(P.c.tenured = perc.rank(cum.citations)), by='age']
  # merge into one dataset 
  DT = merge(DT, DT_tmp[, c('aut.id', 'pub.id', 'age', 'P.c.tenured')], by=c('aut.id', 'pub.id', 'age'), all.x = TRUE)
  return(DT)
}
DT_pub = calc.pubrp()
# save the results
saveRDS(DT_pub, paste0(base, '/data/rp/pub.rds'))

### Calculate the rank percentiles for scholars --------
DT_pub = readRDS(paste0(base, '/data/rp/pub.rds'))
calc.autrp <- function(DT_pub){
  ## calculate the metrics for each scholar at each age
  DT_aut = do.call(rbind, mapply(calc.metrics.auti, unique(DT_pub$aut.id), MoreArgs = list(DT_pub), SIMPLIFY = FALSE, USE.NAMES = FALSE))
  ## calculate rank percentiles
  DT_aut = calc.rp.aut(DT_aut)
  return(DT_aut)
}
DT_aut = calc.autrp(DT_pub)
# save the results
saveRDS(DT_aut, paste0(base, '/data/rp/aut.rds'))


### Calculate the rank percentiles for scholars, future impact ---------
### At age t_1, we predict the future impact of the papers published between t_1 and t_2 from a scholar

DT_aut = readRDS(paste0(base, '/data/rp/aut.rds'))
DT_pub = readRDS(paste0(base, '/data/rp/pub.rds'))

calc.autrp.future <- function(DT_pub, DT_aut){
  # the starting year of the career
  DT_pub[, `:=`(start.aut = min(start)), by='aut.id']
  
  DT_aut_future = list()
  # the ages from which we start generating the future impact of scholars
  for(age_training in seq(5, 25, by=5)){
    print(age_training)
    # remove publications published before age_training
    DT_pub_subset = DT_pub[start >= start.aut + age_training, ]
    # calculate the metrics for each scholar at each age
    DT = do.call(rbind, mapply(calc.metrics.auti, unique(DT_pub_subset$aut.id), MoreArgs = list(DT_pub_subset), SIMPLIFY = FALSE, USE.NAMES = FALSE))
    # fill the metrics in years where the scholar doesn't have any publications, with zeroes
    DT = merge(DT, DT_pub[, list(start.aut = min(start)), by='aut.id'], by='aut.id')
    tmp_function <- function(aut.id.i){
      DT_subset = DT[aut.id == aut.id.i, ]
      DT_subset[, list(year = setdiff(seq(unique(start.aut) + age_training, 2016), year), 
                       cum.citations = 0,
                       cum.publications = 0,
                       h.index = 0,
                       P5.all = 0,
                       P5.bio = ifelse(any(is.na(P5.bio)),  NA, 0),
                       P5.tenured = ifelse(any(is.na(P5.tenured)),  NA, 0),
                       start.aut = unique(start.aut)), by='aut.id']
    }
    DT_tmp = do.call(rbind, lapply(unique(DT$aut.id), tmp_function))
    DT_tmp = DT_tmp[!is.na(year), ]
    DT = rbind(DT, DT_tmp)
    DT = DT[order(aut.id, year),]
    # calculate rank percentiles
    DT = calc.rp.aut(DT)
    # correct some metrics
    DT[, `:=`(age = (age_training+1):(age_training+.N)), by='aut.id']
    DT[, `:=`(start = start.aut)]
    DT$start.aut = NULL
    
    # add the current impact
    DT = rbind(DT, 
               DT_aut[aut.id %in% DT$aut.id & age==age_training,])
    DT = DT[order(aut.id, year), ]
    
    DT_aut_future[[paste0('T1=', age_training)]] = DT
  }
  return(DT_aut_future)
}
DT_aut_future = calc.autrp.future(DT_pub, DT_aut)
# save the results
saveRDS(DT_aut_future, paste0(base, '/data/rp/aut_future.rds'))
