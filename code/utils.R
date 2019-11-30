## R packages required
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(pbapply)
library(ggthemes)
library(zoo)

## extract the legend in the plot, to be placed somewhere else
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

## calculate rank percentile
# Bornmann(2013) has details of the calculation
# here we make sure 0 citation papers get 0 rp
# also, the median rank get 0.5 rp so the tails are treated symmetrically
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
  if(length(x) != length(y)){stop('two vectors shall have the same length')}
  n = length(x)
  return( sqrt(sum((x-y)^2)/n) )
}
mae <- function(x, y){
  if(length(x) != length(y)){stop('two vectors shall have the same length')}
  sum(abs(x-y)) / length(x)
}
medse <- function(x, y){
  if(length(x) != length(y)){stop('two vectors shall have the same length')}
  return( median((x-y)^2) )
}

# a function round decimal to the 3rd place, convert the number to character and remove the leading '0
numformat = function(val) { sub("^(-?)0.", "\\1.", sprintf("%.3f", val)) }