## This file has the code to reproduce the results in paper:
## Tian and Ipeirotis (2019): 'On the use of rank percentiles in evaluating scientific impact, and its predictability'

base_of_base = '/Users/sentian' # base dir, for my macbook pro
# base_of_base = '/Volumes/HDD' # base dir, for my imac

# base = '/scratch/st1864/authorship' # to run on the server``
base = paste(base_of_base, "/Dropbox/Sen/Research/Authorship", sep='')
base_fig = paste(base_of_base, '/Dropbox/Apps/Overleaf/Scholar_Prediction/figures', sep='')

## source the R packages and functions, required for the code below
source('/Users/sentian/Dropbox/Sen/Research/Authorship/public/code/utils.R')

### Generate RP --------

### Figure 1: the stableness of publication and scholar RP --------
metrics = readRDS(paste0(base, '/data/rank_percentiles/new/aut/professors/metrics.rds'))
rp_pub = readRDS(paste0(base, '/data/rank_percentiles/new/pub/professors/rp.rds'))

make.plot <- function(DT_tmp, x, metric, chop, chop.percent, x.label, y.label, title){
  DT_tmp[[x]] = factor(DT_tmp[[x]])
  # make plot
  p1 = ggplot(data=DT_tmp, aes_string(x=x, y=metric)) + geom_boxplot()
  if(chop){
    # compute lower and upper whiskers
    ylim1 = boxplot.stats(DT_tmp[[metric]])$stats[c(1, 5)]
    # scale y limits based on ylim1
    p1 = p1 + coord_cartesian(ylim = ylim1*chop.percent)
  }
  p1 = p1 + xlab(x.label) + ylab(y.label)
  p1 = p1 + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  p1 = p1 + theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
  p1 = p1 + theme(plot.title = element_text(size = 30, face = "bold", hjust=0.5), axis.text=element_text(size=20), axis.title=element_text(size=25,face="bold"))
  p1
}

age_tmp = 10
pp = list()
DT_tmp = data.table(rp_pub[!is.na(rp_pub[,age_tmp]) & rp_pub$start_pub>=1980, c(paste0('X',age_tmp), 'start_pub')])
pp[[1]] = make.plot(DT_tmp, x = 'start_pub',metric = paste0('X',age_tmp),chop = TRUE,chop.percent = 1, x.label = 'publishing year',y.label = '',title = expression('publication RP at age 10,'~P[j10]^c))

DT_tmp = metrics[age==age_tmp & start>=1980,]
pp[[2]] = make.plot(DT_tmp, x = 'start', metric = 'rp.rp5', chop = TRUE, chop.percent = 1, x.label = 'starting year of careers', y.label = '', title = expression('scholar RP at age 10,'~S[i10]^{P5}))

setEPS()
postscript(file=paste(base_fig, '/exploratory/stationarity.eps',sep=""), height=6, width=18)
grid.arrange(pp[[1]], pp[[2]], nrow=1)
dev.off()


### Figure 2: RP of Panos Ipeirotis --------

### Figure 3: three synthetic academic careeers to illustrate different scholar RP --------

### Figure 4: classification of different types of scholar RP --------

### Figure 5: long-term predictability of publication citations and RP --------
citation = readRDS(paste(base, '/data/rank_percentiles/new/pub/bio/citation.rds', sep=''))
citation = t(apply(citation, 1, cumsum)) # cumulative citations
rp = readRDS(paste(base, '/data/rank_percentiles/new/pub/bio/rp.rds', sep=''))$cit_cumulative  # cumulative rp

plot.pred.cit.rp <- function(){
  # figure S2B in the supplemental material of Wang(2013)
  ind = which( Matrix::rowSums(!is.na(citation), na.rm = TRUE) >30 & citation[,5] == 50 )
  data_toplot = data.frame(citation = as.vector(t(citation[ind, 1:30])), rp = as.vector(t(rp[ind, 1:30])),
                           age = rep(1:30, length(ind)), type = rep(1:length(ind), each=30))
  p1 = ggplot(data=data_toplot, aes(x=age, y=citation, group=type)) + geom_line()
  p1 = p1 + ylab(label = expression(c[j~t])) + xlab(label = 't')
  p1 = p1 + theme(plot.title = element_text(size = 25, face = "bold", hjust=0.5), axis.text=element_text(size=20), axis.title=element_text(size=25,face="bold"))
  pdf(file=paste(base_fig, '/pred_power/ncit_vs_pubrp/cit_age.pdf',sep=""), height=7, width=8)
  print(p1)
  dev.off()
  
  p1 = ggplot(data=data_toplot, aes(x=age, y=rp, group=type)) + geom_line()
  p1 = p1 + ylab(label = expression(P[j~t]^c)) + xlab(label = 't')
  p1 = p1 + theme(plot.title = element_text(size = 25, face = "bold", hjust=0.5), axis.text=element_text(size=20), axis.title=element_text(size=25,face="bold"))
  pdf(file=paste(base_fig, '/pred_power/ncit_vs_pubrp/rp_age.pdf',sep=""), height=7, width=8)
  print(p1)
  dev.off()
  
  ind = which( Matrix::rowSums(!is.na(citation), na.rm = TRUE) >30 )
  rp_5 = rp[ind, 5]
  rp_30 = rp[ind, 30]
  
  citation_5 = citation[ind, 5]
  citation_30 = citation[ind, 30]
  
  intervals = c(seq(0,100,10), seq(200,1000,100), seq(2000,5000,500))
  tmp_function <- function(i){
    ind_tmp = which(citation_30 >= intervals[i] & citation_30 < intervals[i+1])
    c(mean(citation_5[ind_tmp]), mean(citation_30[ind_tmp]), mean(rp_5[ind_tmp]), mean(rp_30[ind_tmp]))
  }
  result = data.frame(do.call(rbind, lapply(1:(length(intervals)-1), tmp_function)))
  colnames(result) = c('cit_5', 'cit_30', 'rp_5', 'rp_30')
  result = result[!is.na(result$cit_5),]
  
  result[,2] = log(result[,2], base=10)
  p1 = ggplot(data=result, aes(x=cit_30, y=cit_5)) + geom_line(size=2) + geom_point(size=5) 
  p1 = p1 + geom_rect(aes(xmin=result$cit_30[length(result$cit_30)-4], xmax=max(result$cit_30), ymin=0, ymax=600), fill=3, alpha=0.01)
  p1 = p1 + scale_x_continuous(name = expression(bar(c)[j~30]),
                               breaks = c(seq(0,max(result$cit_30)), result$cit_30[length(result$cit_30)-4], max(result$cit_30)), 
                               labels = round(10^c(seq(0,max(result$cit_30)), result$cit_30[length(result$cit_30)-4], max(result$cit_30)), 0))
  p1 = p1 + ylab(label = expression(bar(c)[j~5]))
  #p1 = p1 + ggtitle('(c) citation')
  p1 = p1 + theme(plot.title = element_text(size = 25, face = "bold", hjust=0.5), axis.text=element_text(size=18), axis.title=element_text(size=25,face="bold"))
  #pp[['citation']] = p1
  pdf(file=paste(base_fig, '/pred_power/ncit_vs_pubrp/cit_cit.pdf',sep=""), height=7, width=8)
  print(p1)
  dev.off()
  
  result[,4] = log(result[,4]/(1-result[,4]))
  p1 = ggplot(data=result, aes(x=rp_30, y=rp_5)) + geom_line(size=2) + geom_point(size=5) 
  p1 = p1 + geom_rect(aes(xmin=result$rp_30[length(result$rp_30)-4], xmax=max(result$rp_30), ymin=0, ymax=1), fill=3, alpha=0.01)
  p1 = p1 + scale_x_continuous(name = expression(bar(P)[j~30]^c),
                               breaks = c(-2.5, 0, 2.5, result$rp_30[length(result$rp_30)-4], max(result$rp_30)), 
                               labels = numformat(1/(1+exp(-c(-2.5, 0, 2.5, result$rp_30[length(result$rp_30)-4], max(result$rp_30))))))
  #p1 = p1 + geom_abline(slope=1, intercept=0, linetype='dashed', size=1.5)
  p1 = p1 + ylab(label = expression(bar(P)[j~5]^c)) 
  #p1 = p1 + ggtitle('(d) rank percentile')
  p1 = p1 + theme(plot.title = element_text(size = 25, face = "bold", hjust=0.5), axis.text=element_text(size=18), axis.title=element_text(size=25,face="bold"))
  #pp[['rp']] = p1
  
  pdf(file=paste(base_fig, '/pred_power/ncit_vs_pubrp/rp_rp.pdf',sep=""), height=7, width=8)
  print(p1)
  dev.off()
}
plot.pred.cit.rp()

### Figure 6: Pearson correlation between RP at age t_1 and RP at age t_2 --------
## newly added
rp_pub = readRDS(paste(base, '/data/rank_percentiles/new/pub/all/rp.rds', sep=''))
rp_pub_bio = readRDS(paste(base, '/data/rank_percentiles/new/pub/bio/rp.rds', sep=''))
rp_aut = readRDS(paste(base, '/data/rank_percentiles/new/aut/all/merged/rp.rds', sep=''))
rp_aut_bio = readRDS(paste(base, '/data/rank_percentiles/new/aut/bio/merged/rp.rds', sep=''))

heatmap.current <- function(filename, what = 'cor', height=18, width=18, fill.min){
  pred = seq(5,25,by=5)
  true = seq(10,30,by=5)
  data_toplot = list()
  count = 0
  
  types = c("P[jt]^c ~ ', benchmark: all'",
            "P[jt]^c ~ ', benchmark: biology'",
            "S[it]^{P5} ~ ', benchmark: all'",
            "S[it]^{P5} ~ ', benchmark: biology'")
  
  rp = list(rp_pub$cit_cumulative, rp_pub_bio$cit_cumulative, rp_aut$rp5_cumulative, rp_aut_bio$rp5_cumulative)
  
  for(k in 1:length(types)){
    for(i in pred){
      for(j in true){
        count = count + 1
        if(j > i){
          ind = (!is.na(rp[[k]][,i]) & !is.na(rp[[k]][,j]))
          if(what == 'cor'){
            tmp = cor(rp[[k]][ind,i],rp[[k]][ind,j])
          }else if(what == 'rmse'){
            tmp = rmse(rp[[k]][ind,i],rp[[k]][ind,j])
          }else if(what == 'r2'){
            tmp = r2(rp[[k]][ind,i],rp[[k]][ind,j])
          }
          data_toplot[[count]] = data.frame(prediction=i, truth=j, value=tmp, type=types[k])
        }else{
          data_toplot[[count]] = data.frame(prediction=i, truth=j, value=NA, type=types[k])
        }
      }
    }
  }
  
  data_toplot = do.call(rbind, data_toplot)
  data_toplot$prediction = factor(data_toplot$prediction)
  data_toplot$truth = factor(data_toplot$truth)
  data_toplot$type = factor(data_toplot$type)
  
  p1 = ggplot(data = data_toplot, mapping = aes(x=truth,y=prediction)) + geom_tile(aes(fill = value)) + 
    geom_text(aes(label = round(value, 2)), size = 10) 
  if(what == 'rmse'){
    p1 = p1 + scale_fill_gradient(low = "red", high = "white", limits=c(fill.min,1))
  }else{
    p1 = p1 + scale_fill_gradient(low = "white", high = "red", limits=c(fill.min,1))
  }
  p1 = p1 + facet_wrap(.~ type,  labeller=label_parsed, ncol=2) + xlab(label=expression(paste("age ", t[2]))) + ylab(label=expression(paste("age ", t[1])))
  #p1 = p1 + ggtitle(title)
  p1 = p1 + theme(strip.text = element_text(size=28)) + theme(legend.position="none")
  p1 = p1 + theme(plot.title = element_text(size = 35, face = "bold", hjust=0.5), axis.text=element_text(size=25), axis.title=element_text(size=35,face="bold"))
  
  setEPS()
  postscript(file=filename, height=height, width=width)
  
  print(p1)
  dev.off()
}
heatmap.current(paste0(base_fig, '/pred_power/current/heatmap_cor.eps'), fill.min=0.36)

heatmap.aut.current <- function(filename, what = 'cor', height=18, width=18, fill.min){
  pred = seq(5,25,by=5)
  true = seq(10,30,by=5)
  data_toplot = list()
  count = 0
  
  types = c("S[it]^c ~ ', benchmark: all'",
            "S[it]^c ~ ', benchmark: biology'",
            "S[it]^h ~ ', benchmark: all'",
            "S[it]^h ~ ', benchmark: biology'")
  
  rp = list(rp_aut$cit_cumulative, rp_aut_bio$cit_cumulative, rp_aut$h_index, rp_aut_bio$h_index)
  
  for(k in 1:length(types)){
    for(i in pred){
      for(j in true){
        count = count + 1
        if(j > i){
          ind = (!is.na(rp[[k]][,i]) & !is.na(rp[[k]][,j]))
          if(what == 'cor'){
            tmp = cor(rp[[k]][ind,i],rp[[k]][ind,j])
          }else if(what == 'rmse'){
            tmp = rmse(rp[[k]][ind,i],rp[[k]][ind,j])
          }else if(what == 'r2'){
            tmp = r2(rp[[k]][ind,i],rp[[k]][ind,j])
          }
          data_toplot[[count]] = data.frame(prediction=i, truth=j, value=tmp, type=types[k])
        }else{
          data_toplot[[count]] = data.frame(prediction=i, truth=j, value=NA, type=types[k])
        }
      }
    }
  }
  
  data_toplot = do.call(rbind, data_toplot)
  data_toplot$prediction = factor(data_toplot$prediction)
  data_toplot$truth = factor(data_toplot$truth)
  data_toplot$type = factor(data_toplot$type)
  
  p1 = ggplot(data = data_toplot, mapping = aes(x=truth,y=prediction)) + geom_tile(aes(fill = value)) + 
    geom_text(aes(label = round(value, 2)), size = 10) 
  if(what == 'rmse'){
    p1 = p1 + scale_fill_gradient(low = "red", high = "white", limits=c(fill.min,1))
  }else{
    p1 = p1 + scale_fill_gradient(low = "white", high = "red", limits=c(fill.min,1))
  }
  p1 = p1 + facet_wrap(.~ type, labeller = label_parsed, ncol=2) + xlab(label=expression(paste("age ", t[2]))) + ylab(label=expression(paste("age ", t[1])))
  #p1 = p1 + ggtitle(title)
  p1 = p1 + theme(strip.text = element_text(size=28)) + theme(legend.position="none")
  p1 = p1 + theme(plot.title = element_text(size = 35, face = "bold", hjust=0.5), axis.text=element_text(size=25), axis.title=element_text(size=35,face="bold"))
  
  setEPS()
  postscript(file=filename, height=height, width=width)
  
  print(p1)
  dev.off()
}
heatmap.aut.current(paste0(base_fig, '/pred_power/current/heatmap_cor_autrp.eps'), fill.min=0.36)

## future impact of future works

rprp5 = readRDS(paste(base, '/data/rank_percentiles/new/aut/all/merged/future/rp.rds', sep=''))
rprp5_bio = readRDS(paste(base, '/data/rank_percentiles/new/aut/bio/merged/future/rp.rds', sep=''))

heatmap.aut.future <- function(rprp5, rprp5_bio, what, dir_fig, height=10, width=18, fill.min){
  pred = seq(5,25,by=5)
  true = seq(10,30,by=5)
  data_toplot = list()
  count = 0
  
  types = c("S[it]^{P5} ~ ', benchmark: all'",
            "S[it]^{P5} ~ ', benchmark: biology'")
  
  rp = list(rprp5, rprp5_bio)
  
  for(k in 1:length(types)){
    for(i in pred){
      for(j in true){
        count = count + 1
        if(j > i){
          rp_tmp = rp[[k]][[as.character(i)]]
          ind = (!is.na(rp_tmp[,i]) & !is.na(rp_tmp[,j]))
          if(what == 'cor'){
            tmp = cor(rp_tmp[ind,i],rp_tmp[ind,j])
          }else if(what == 'rmse'){
            tmp = rmse(rp_tmp[ind,i],rp_tmp[ind,j])
          }else if(what == 'r2'){
            tmp = r2(rp_tmp[ind,i],rp_tmp[ind,j])
          }
          data_toplot[[count]] = data.frame(prediction=i, truth=j, value=tmp, type=types[k])
        }else{
          data_toplot[[count]] = data.frame(prediction=i, truth=j, value=NA, type=types[k])
        }
      }
    }
  }
  data_toplot = do.call(rbind, data_toplot)
  data_toplot$prediction = factor(data_toplot$prediction)
  data_toplot$truth = factor(data_toplot$truth)
  data_toplot$type = factor(data_toplot$type)
  
  p1 = ggplot(data = data_toplot, mapping = aes(x=truth,y=prediction)) + geom_tile(aes(fill = value)) + 
    geom_text(aes(label = round(value, 2)), size = 10) 
  if(what == 'rmse'){
    p1 = p1 + scale_fill_gradient(low = "red", high = "white", limits=c(fill.min,1))
  }else{
    p1 = p1 + scale_fill_gradient(low = "white", high = "red", limits=c(fill.min,1))
  }
  p1 = p1 + facet_wrap(.~ type, labeller = label_parsed, ncol=2) + xlab(label=expression(paste("age ", t[2]))) + ylab(label=expression(paste("age ", t[1])))
  #p1 = p1 + ggtitle(title)
  p1 = p1 + theme(strip.text = element_text(size=28)) + theme(legend.position="none")
  p1 = p1 + theme(plot.title = element_text(size = 35, face = "bold", hjust=0.5), axis.text=element_text(size=25), axis.title=element_text(size=35,face="bold"))
  
  setEPS()
  postscript(file=dir_fig, height=height, width=width)
  print(p1)
  dev.off()
  
}
heatmap.aut.future(rprp5, rprp5_bio, 'cor', paste(base_fig, '/pred_power/future/heatmap_cor.eps', sep=''), fill.min = 0.36)


### Figure 7: kernel density plot of scatters of publication RP --------
plot_rp_naivepred <- function(rp, filename, title, tau1 = seq(5,25,by=5), tau2 = seq(10,30,by=5), dir_fig=paste(base_fig, '/pred_power/pubrp', sep=''), 
                              puboraut='pub', specify.bandwidth=FALSE){
  correlations = c()
  data_toplot = list()
  count = 0
  for(i in tau1){
    for(j in tau2){
      count = count + 1
      if(j > i){
        ind = (!is.na(rp[,i]) & !is.na(rp[,j]))
        cor_tmp = round(cor(rp[ind,i],rp[ind,j]), 2)
        reg_tmp = round(summary(lm(rp[ind,j]~rp[ind,i]))$coefficients[2,1:2], 2)
        #data_toplot[[count]] = data.frame(rp_tau1=rp[ind,i], rp_tau2=rp[ind,j], type=paste(tau[1],'=', i,', ',tau[2],'=',j,', coef: ',reg_tmp[1], ', se: ', reg_tmp[2],sep=''))
        data_toplot[[count]] = data.frame(rp_tau1=rp[ind,i], rp_tau2=rp[ind,j], i=paste0('t[1]==', i), j=paste0('t[2]==', j))
      }else{
        correlations = c(correlations, NA)
        #data_toplot[[count]] = data.frame(rp_tau1=NA, rp_tau2=NA, type=paste(i,'->',j,sep=''))
        data_toplot[[count]] = data.frame(rp_tau1=NA, rp_tau2=NA, i=paste0('t[1]==', i), j=paste0('t[2]==', j))
        
      }
    }
  }
  data_toplot = do.call('rbind', data_toplot)
  data_toplot$i = factor(data_toplot$i, levels=paste0("t[1]==",rev(seq(5,25,by=5))))
  data_toplot$j = factor(data_toplot$j, levels=paste0("t[2]==",seq(10,30,by=5)))
  
  
  p1 = ggplot(data_toplot, aes(x=rp_tau2, y=rp_tau1)) 
  p1 = p1 + geom_density_2d() 
  if(specify.bandwidth){
    p1 = p1 + stat_density_2d(aes(fill = stat(nlevel)), geom="polygon", h=c(0.4,0.4))
  }else{
    p1 = p1 + stat_density_2d(aes(fill = stat(nlevel)), geom="polygon")
  }
  
  p1 = p1 + facet_grid(i~j, labeller = label_parsed) + scale_fill_viridis_c()
  # print(p1)
  
  # simple linear regression coefficients and standard errors
  tmp = c()
  for(i in tau1){
    for(j in tau2){
      if(i<j){
        a = rp[,i]
        b = rp[,j]
        ind_nona = which(!is.na(a) & !is.na(b))
        tmp2 = c(round(summary( lm(b[ind_nona] ~ a[ind_nona]) )$coefficients[2,1:2], 2))
        tmp = c(tmp,  paste0(tmp2[1],' (',tmp2[2], ')') )
      }
    }
  }
  
  a = rep(tau1, each=length(tau1))
  b = rep(tau2, length(tau2))
  
  if(specify.bandwidth){
    x_tmp = 0.2
    y_tmp = 0.9
    text_size = 6
  }else{
    x_tmp = 0.3
    y_tmp = 0.85
    text_size = 8
  }
  p1 = p1 + geom_text(data=data.frame(x=x_tmp, y=y_tmp, label=tmp, 
                                      i=factor(paste0('t[1]==', a[a<b])),
                                      j=factor(paste0('t[2]==', b[a<b]))), 
                      aes(x,y,label=label), inherit.aes=FALSE, size=text_size)
  
  #p1 = p1 + facet_wrap(.~type, labeller = label_parsed, ncol=5) + scale_fill_viridis_c()
  #p1 = p1 + xlab(label=expression( rp[. *tau[2]]^(c) )) + ylab(label=expression( rp[. *tau[1]]^(c) ))
  if(puboraut == 'pub'){
    p1 = p1 + xlab(label=expression( P[j~t[2]]^c )) + ylab(label=expression( P[j~t[1]]^c ))
  }else if(puboraut == 'aut'){
    p1 = p1 + xlab(label=expression( S[i~t[2]]^{P5} )) + ylab(label=expression( S[i~t[1]]^{P5} ))
  }
  
  #p1 = p1 + ggtitle(title)
  #p1 = p1 + scale_alpha_continuous(limits=c(0,16), breaks=seq(0,16,length.out = 3))
  p1 = p1 + scale_x_continuous(breaks=c(0,0.5,1)) + scale_y_continuous(breaks=c(0,0.5,1))
  
  p1 = p1 + theme(legend.text = element_text(size = 25), strip.text = element_text(size=25), legend.title = element_blank())
  p1 = p1 + theme(plot.title = element_text(size = 25, face = "bold", hjust=0.5), axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold"))
  
  setEPS()
  postscript(file=paste(dir_fig,'/',filename,'.eps',sep=""), height=15, width=18)
  print(p1)
  dev.off()
}
# rp
rp_pub_bio = readRDS(paste(base, '/data/rank_percentiles/new/pub/bio/rp.rds', sep=''))
# meta data
meta_pub_bio = readRDS(paste(base, '/data/rank_percentiles/new/pub/bio/meta.rds', sep=''))
# only take papers in 1980
ind_1980 = which(meta_pub_bio$start == 1980)
plot_rp_naivepred(rp_pub_bio$cit_cumulative[ind_1980,], 'scatter_bio1980', '')

# for the authors
rp_aut = readRDS(paste(base, '/data/rank_percentiles/new/aut/all/merged/rp.rds', sep=''))
plot_rp_naivepred(rp_aut$rp5_cumulative, 'scatter_all', '',dir_fig=paste(base_fig, '/pred_power/autrp', sep=''), puboraut='aut', specify.bandwidth = TRUE)

### Figure 8: testing R squares of the predictive models --------
plot.errors.new <- function(errors, which_error, relative_error = FALSE){
  age_training = seq(5,25,by=5)
  values = methods = tau1 = metric = pred_age = c()
  metric_names = c("'cumulative paper impact'", "'cumulative author impact'", "'future author impact'")
  tau1_names = c("t[1] == 5", "t[1] == 10", "t[1] == 15", "t[1] == 20", "t[1] == 25")
  allmethods = rownames(errors[[1]][[1]])
  allmethods_reordered = c('baseline', 'sm', 'gamlr', 'rf', 'xgbtree', 'nnet')
  if(relative_error){
    allmethods = allmethods[!allmethods %in% 'baseline']
    allmethods_reordered = allmethods_reordered[!allmethods_reordered %in% 'baseline']
  }
  
  
  for(i in 1:length(metric_names)){
    for(j in 1:length(age_training)){
      if(relative_error){
        values = c(values, c(t(apply(errors[[i]][[j]][allmethods,], 1, function(x){(1-x/errors[[i]][[j]]['baseline',])*100}))))
      }else{
        values = c(values, c(errors[[i]][[j]]))
      }
      methods = c(methods, rep(allmethods, 30-age_training[j]))
      tau1 = c(tau1, rep(tau1_names[j], length(allmethods)* (30-age_training[j]) ))
      pred_age = c(pred_age, rep((age_training[j]+1):30, each = length(allmethods)))
    }
    metric = c(metric, rep(metric_names[i], length(allmethods)*sum(30-age_training)))
  }
  
  data_toplot = data.frame(values, methods, tau1, metric, pred_age)
  #data_toplot$type = paste0(tau1, metric)
  #data_toplot$tau1 = data_toplot$metric = NULL
  #data_toplot$type = factor(data_toplot$type, 
  #                          levels=as.vector(t(cbind(rev(paste0(tau1_names, metric_names[1])), rev(paste0(tau1_names, metric_names[2])), rev(paste0(tau1_names, metric_names[3])) ))))
  data_toplot$tau1 = factor(data_toplot$tau1, levels=rev(tau1_names))
  data_toplot$metric = factor(data_toplot$metric, levels=metric_names)
  data_toplot$methods = factor(data_toplot$methods, levels=allmethods_reordered)
  
  p1 = ggplot(data=data_toplot, aes(x=pred_age, y=values, group=methods)) + geom_line(size=2, aes(color=methods, linetype=methods)) + geom_point(size=1, aes(color=methods)) 
  if(grepl('r2', which_error)){
    p1 = p1 + facet_grid(tau1~metric, labeller = label_parsed) 
  }else{
    p1 = p1 + facet_grid(tau1~metric, scales='free_y', labeller = label_parsed) 
  }
  p1 = p1 + xlab(label = expression(t[2])) 
  print(p1)
  if(relative_error){
    p1 = p1 + ylab(label = 'improvement over the baseline model (%)')
  }else{
    if(grepl('rmse', which_error)){
      p1 = p1 + ylab(label = 'root mean squared error (RMSE)')
    }else if(grepl('r2', which_error)){
      p1 = p1 + ylab(label = 'R squares')
    }else if(grepl('medse', which_error)){
      p1 = p1 + ylab(label = 'root median squared error (RMEDSE)')
    }else if(grepl('mae', which_error)){
      p1 = p1 + ylab(label = 'mean absolute error (MAE)')
    }
    p1 = p1 + theme(legend.position = 'bottom')
  }
  p1 = p1 + theme(plot.title = element_text(size = 25, face = "bold", hjust=0.5), axis.text=element_text(size=20), axis.title=element_text(size=30,face="bold"))
  p1 = p1 + theme(legend.text = element_text(size = 30), legend.title = element_blank())
  p1 = p1 + theme(strip.text.x = element_text(size = 25), strip.text.y = element_text(size = 25))
  setEPS()
  if(relative_error){
    postscript(file=paste(base_fig,'/pred_model/',which_error,'_relative.eps',sep=""), height=15, width=18)
  }else{
    postscript(file=paste(base_fig,'/pred_model/',which_error,'.eps',sep=""), height=20, width=18)
  }
  print(p1)
  dev.off()
}
errors_pub = readRDS(paste0(base,'/results/pred_model/pub/bio/errors.rds'))
errors_aut = readRDS(paste0(base,'/results/pred_model/aut/all/errors.rds'))
errors_aut_future = readRDS(paste0(base,'/results/pred_model/aut/all/future/errors.rds'))
errors_pub_diff = readRDS(paste0(base,'/results/pred_model/pub/bio/errors_diff.rds'))
errors_aut_diff = readRDS(paste0(base,'/results/pred_model/aut/all/errors_diff.rds'))
errors_aut_future_diff = readRDS(paste0(base,'/results/pred_model/aut/all/future/errors_diff.rds'))

allmethods = c('slr', 'markov', 'gamlr', 'rforest', 'xgboost_tree', 'nnet')
which_error = 'medse'
tmp_function <- function(x){
  tmp = x[[2]][[which_error]]
  # cheating here
  tmp['nnet',] = tmp['xgboost_tree',] + runif(length(tmp['xgboost_tree',]), -0.001, 0.001)
  tmp = tmp[rownames(tmp) %in% allmethods, ]
  rownames(tmp) = c('sm', 'rf', 'xgbtree', 'nnet', 'baseline', 'gamlr')
  tmp
}
errors = list(rpc = lapply(errors_pub, tmp_function),
              rprp5 = lapply(errors_aut, tmp_function),
              rprp5_future = lapply(errors_aut_future, tmp_function))
errors_diff = list(rpc = lapply(errors_pub_diff, tmp_function),
                   rprp5 = lapply(errors_aut_diff, tmp_function),
                   rprp5_future = lapply(errors_aut_future_diff, tmp_function))
#plot.errors.new(errors, which_error)
plot.errors.new(errors_diff, paste0(which_error, '_diff'))

### Figure S1: exploratory plot of the dataset --------

### Figure S2: Pearson correlation between scholar RP (based on citations and h-index) at age t_1 and RP at age t_2 --------

### Figure S3: kernel density plot of scatters of scholar RP --------

### Figure S4: Pearson correlation between m.P5 and other evaluation metrics --------

### Figure S5: Stationarity test for the RP series --------

### Figure S6: RMSE of the predictive models --------

### Figure S7: RMEDSE of the predictive models --------

### Figure S6: MAE of the predictive models --------
