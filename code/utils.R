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


### Functions
## summarize metrics for author i, e.g. h-index, cumulative citations, cumulative # publications, etc
calc.metrics.auti <- function(aut.i.id, DT_pub){
  # DT_pub is a data.table object and aut.i.id specifies the id of scholar i
  # subset the data for scholar i
  DT_auti = DT_pub[aut.id == aut.i.id, ]
  # get P5 for each publication
  tmp_function_inside <- function(x){
    if(length(x)>=5){
      x[5]
    }else{
      x[length(x)]
    }
  }
  DT_P5 = DT_auti[, lapply(.SD, tmp_function_inside), by='pub.id', .SDcols=c('P.c.all', 'P.c.bio', 'P.c.tenured')]
  # summarize the metrics for scholar i
  DT_metrics = DT_auti[, list( aut.id = unique(aut.id), cum.citations = sum(cum.citations), cum.publications = .N, 
                               h.index = max(which(seq(1, .N) <= sort(cum.citations, decreasing = TRUE))),
                               P5.all =  sum(DT_P5$P.c.all[DT_P5$pub.id %in% pub.id]), 
                               P5.bio =  sum(DT_P5$P.c.bio[DT_P5$pub.id %in% pub.id]), 
                               P5.tenured =  sum(DT_P5$P.c.tenured[DT_P5$pub.id %in% pub.id]) ), by='year']
  DT_metrics = DT_metrics[order(year), ]
  return(DT_metrics)
}
## calculate author rank percentiles
calc.rp.aut <- function(DT){
  # DT is a data.table object
  DT[, `:=`(age = 1:.N), by='aut.id'] # scholar's age
  ## calculate the rank percentile
  DT[, `:=`(h.index.bio = h.index, h.index.tenured = h.index, cum.citations.bio = cum.citations, cum.citations.tenured = cum.citations)]
  DT$h.index.bio[is.na(DT$P5.bio)] = DT$cum.citations.bio[is.na(DT$P5.bio)] = NA
  DT$h.index.tenured[is.na(DT$P5.tenured)] = DT$cum.citations.tenured[is.na(DT$P5.tenured)] = NA
  DT_tmp = DT[, lapply(.SD, perc.rank), by='age', .SDcols=c('cum.citations', 'cum.citations.bio', 'cum.citations.tenured', 
                                                            'h.index', 'h.index.bio', 'h.index.tenured', 
                                                            'P5.all', 'P5.bio', 'P5.tenured')]
  DT_tmp$aut.id = DT[, aut.id, by='age']$aut.id
  names(DT_tmp)[2:10] = c(paste0('S.c.', c('all', 'bio', 'tenured')), paste0('S.h.', c('all', 'bio', 'tenured')), paste0('S.P5.', c('all', 'bio', 'tenured')))
  DT = merge(DT, DT_tmp, by=c('aut.id', 'age'))
  DT$h.index.bio = DT$h.index.tenured = DT$cum.citations.bio = DT$cum.citations.tenured = NULL
  DT[,`:=`(start = min(year)), by='aut.id']
  DT
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
