########## This file loads the required R packages and some functions utilized in other scripts ##########
##########################################################################################################

### R packages
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(pbapply)
library(ggthemes)
library(zoo)
library(Cairo)
library(tseries)
library(urca)
library(parallel)
library(reshape2)

### Functions
## main function to calculate publication rank percentiles
calc.pubrp <- function(citations, authors){
  
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

## summarize metrics for author i, e.g. h-index, cumulative citations, cumulative # publications, etc
calc.metrics.auti <- function(aut.i.id, DT_pub, fun.agg){
  # DT_pub is a data.table object and aut.i.id specifies the id of scholar i
  # subset the data for scholar i
  DT_auti = DT_pub[aut.id == aut.i.id & start <= 2012, ]
  if(nrow(DT_auti) == 0){
    return(data.table(
      year = integer(),
      aut.id = integer(),
      cum.citations = integer(),
      cum.publications = integer(),
      h.index = integer(),
      P5.all = numeric(),
      P5.bio = numeric(),
      P5.tenured = numeric()
    ))
  }
  
  DT_P5 = DT_auti[age == 5, c('pub.id', 'P.c.all', 'P.c.bio', 'P.c.tenured')]
  # summarize the metrics for scholar i
  DT_metrics = DT_auti[year<=2012, list( aut.id = unique(aut.id), cum.citations = sum(cum.citations), cum.publications = .N, 
                               h.index = max(which(seq(1, .N) <= sort(cum.citations, decreasing = TRUE))),
                               P5.all = fun.agg(DT_P5$P.c.all[DT_P5$pub.id %in% pub.id]), 
                               P5.bio = fun.agg(DT_P5$P.c.bio[DT_P5$pub.id %in% pub.id]), 
                               P5.tenured = fun.agg(DT_P5$P.c.tenured[DT_P5$pub.id %in% pub.id])
  ), by='year']
  
  DT_metrics = DT_metrics[order(year), ]
  return(DT_metrics)
}

## calculate author rank percentiles
calc.rp.aut <- function(DT){
  # DT is a data.table object
  DT[, `:=`(age = 1:.N), by='aut.id'] # scholar's age
  ## calculate the rank percentile
  DT[, `:=`(h.index.bio = h.index, h.index.tenured = h.index, 
            cum.citations.bio = cum.citations, cum.citations.tenured = cum.citations)]
  DT$h.index.bio[is.na(DT$P5.bio)] = DT$cum.citations.bio[is.na(DT$P5.bio)] = NA
  DT$h.index.tenured[is.na(DT$P5.tenured)] = DT$cum.citations.tenured[is.na(DT$P5.tenured)] = NA
  DT_tmp = DT[, lapply(.SD, perc.rank), by='age', .SDcols=c('cum.citations', 'cum.citations.bio', 'cum.citations.tenured', 
                                                            'h.index', 'h.index.bio', 'h.index.tenured', 
                                                            'P5.all', 'P5.bio', 'P5.tenured'
                                                            )]
  DT_tmp$aut.id = DT[, aut.id, by='age']$aut.id
  names(DT_tmp)[2:10] = c(paste0('S.c.', c('all', 'bio', 'tenured')), paste0('S.h.', c('all', 'bio', 'tenured')), 
                          paste0('S.P5.', c('all', 'bio', 'tenured'))
                          )
  DT = merge(DT, DT_tmp, by=c('aut.id', 'age'))
  DT$h.index.bio = DT$h.index.tenured = DT$cum.citations.bio = DT$cum.citations.tenured = NULL
  DT[,`:=`(start = min(year)), by='aut.id']
  DT
}

## main function to calculate author rank percentiles
calc.autrp <- function(DT_pub, fun.agg=sum){
  ## calculate the metrics for each scholar at each age
  DT_aut = do.call(rbind, pbmapply(calc.metrics.auti, unique(DT_pub$aut.id), MoreArgs = list(DT_pub, fun.agg), SIMPLIFY = FALSE, USE.NAMES = FALSE))
  ## calculate rank percentiles
  DT_aut = calc.rp.aut(DT_aut)
  return(DT_aut)
}

## main function to calculate author rank percentiles for future impact
## At age t_1, we predict the future impact of the papers published between t_1 and t_2 from a scholar
calc.autrp.future <- function(DT_pub, DT_aut, fun.agg=sum){
  # the starting year of the career
  DT_pub[, `:=`(start.aut = min(start)), by='aut.id']
  
  DT_aut_future = list()
  # the ages from which we start generating the future impact of scholars
  for(i in seq(1,5)){
    print(paste0(i, "/5 is being processed"))
    age_training = i*5
    # remove publications published before age_training
    DT_pub_subset = DT_pub[start >= start.aut + age_training, ]
    # calculate the metrics for each scholar at each age
    DT = do.call(rbind, mapply(calc.metrics.auti, unique(DT_pub_subset$aut.id), MoreArgs = list(DT_pub_subset, fun.agg), SIMPLIFY = FALSE, USE.NAMES = FALSE))
    # fill the metrics in years where the scholar doesn't have any publications, with zeroes
    DT = merge(DT, DT_pub[, list(start.aut = min(start)), by='aut.id'], by='aut.id')
    tmp_function <- function(aut.id.i){
      DT_subset = DT[aut.id == aut.id.i, ]
      DT_subset[, list(year = setdiff(seq(unique(start.aut) + age_training, 2012), year), 
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


## extract the legend in the plot, to be placed somewhere else
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

## calculate rank percentile
perc.rank <- function(x){
  # From Hazen, 1914
  # i is the rank, n is the length
  # RP = (i-0.5)/n
  # See Bornmann (2013) page 159 for the advantage
  ind_notna = (!is.na(x))
  tmp = rep(NA, length(x))
  i = rank(x[ind_notna])
  n = length(x[ind_notna])
  tmp[ind_notna] = (i-0.5) / n
  tmp[x == 0] = 0
  tmp
}

## different error measures
rmse <- function(x,y){
  n = length(x)
  if(n != length(y)){stop('two vectors shall have the same length')}
  return( sqrt(sum((x-y)^2)/n) )
}
mae <- function(x, y){
  n = length(x)
  if(n != length(y)){stop('two vectors shall have the same length')}
  sum(abs(x-y)) / n
}
rmedse <- function(x, y){
  n = length(x)
  if(n != length(y)){stop('two vectors shall have the same length')}
  return( sqrt(median((x-y)^2)) )
}

## round decimal to the 3rd place, convert the number to character and remove the leading '0
numformat = function(val) { sub("^(-?)0.", "\\1.", sprintf("%.3f", val)) }
