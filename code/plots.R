########## This files contains the code to reproduce all the figures in the manuscript ##########
########## The results are saved in the "paper/figures/" folder                        ##########
#################################################################################################

## base directories
setwd('SciImpactRanking/')
base = getwd()
base_fig = paste0(base, '/paper/figures') # for the figure outputs

## source the R packages and functions, required for the code below
source(paste0(base, '/code/utils.R'))
setDTthreads(4) # change the number of threads for data.table based on your system

## read data
# rank percntiles for publications and scholars
DT_pub = readRDS(paste0(base, '/data/rp/pub.rds'))
DT_aut = readRDS(paste0(base, '/data/rp/aut.rds'))
DT_aut_future = readRDS(paste0(base, '/data/rp/aut_future.rds'))

### Figure 1: RP of a random scholar --------
plot.scholar.rp.auti <- function(id, DT_aut, benchmark, file_path){
  n = nrow(DT_aut[aut.id == id])
  data_toplot = data.frame(rp = as.vector(as.matrix(DT_aut[aut.id == id, paste0(c('S.c.', 'S.h.', 'S.P5.'), benchmark), with=FALSE])),
                           type = rep(c('S.c', 'S.h', 'S.P5'), each = n),
                           year = rep(DT_aut[aut.id == id]$year, 3)  )
  data_toplot$type = factor(data_toplot$type)
  
  p1 = ggplot(data_toplot, aes(x=year, y=rp, linetype=type, color=type, shape=type)) + geom_line(stat="identity", size=2) + geom_point(size=2, stroke=2)
  p1 = p1 + scale_colour_manual(name  = "",
                                breaks=c('S.c', 'S.h', 'S.P5'),
                                # labels=list(bquote(S[i~t]^{c}), bquote(S[i~t]^{h}), bquote(S[i~t]^{P5})),
                                labels=list(bquote(S[c]), bquote(S[h]), bquote(S[P5])),
                                values=c("#F8766D", "#00BA38", "#619CFF")) +
    scale_shape_manual(name  = "",
                       breaks=c('S.c', 'S.h', 'S.P5'),
                       # labels=list(bquote(S[i~t]^{c}), bquote(S[i~t]^{h}), bquote(S[i~t]^{P5})),
                       labels=list(bquote(S[c]), bquote(S[h]), bquote(S[P5])),
                       values=c(1, 2, 3)) +
    scale_linetype_manual(name  = "",
                          breaks=c('S.c', 'S.h', 'S.P5'),
                          # labels=list(bquote(S[i~t]^{c}), bquote(S[i~t]^{h}), bquote(S[i~t]^{P5})),
                          labels=list(bquote(S[c]), bquote(S[h]), bquote(S[P5])),
                          values=c(1, 2, 3)) 
  # p1 = p1 + ylab(expression(S[i~t]))
  p1 = p1 + ylab('rank percentiles')
  p1 = p1 + theme(legend.text=element_text(size=15), legend.text.align = 0, legend.position = 'bottom')
  p1 = p1 + theme(axis.text=element_text(size=15), axis.title=element_text(size=15))
  
  setEPS()
  postscript(file=paste0(base_fig, file_path), height=4, width=6)
  print(p1)
  dev.off()
}

# authors = read.csv(paste0(base, '/data/raw/authors.csv'), stringsAsFactors = FALSE)
# id_leskovec = authors$aut.id[ which(grepl('Leskovec', authors$name)) ]
id_auti = 545 # id of a random author
plot.scholar.rp.auti(id_auti, DT_aut, 'tenured', file_path = '/compare_autrp/auti.eps')

### Figure 2: three synthetic academic careers to illustrate different scholar RP --------
## generate the synthetic careers
gen.career <- function(DT_pub, DT_aut){
  ## take scholars who start their careers in 1990 and are in biology
  autid_bio1990 = DT_aut[start == 1990 & !is.na(S.c.bio)]$aut.id
  DT_pub_bio1990 = DT_pub[aut.id %in% autid_bio1990 & year<=2012]
  DT_pub_bio1990[, c('P.c.all', 'P.c.bio', 'P.c.tenured')] = NULL
  DT_aut_bio1990 = DT_aut[aut.id %in% autid_bio1990]
  DT_aut_bio1990[, `:=`(publications = c(cum.publications[1], diff(cum.publications)) ), by='aut.id']
  ## generate synthetic careers
  # Author A publishes many publications but all of them are rarely cited
  npub_autA = round(DT_aut_bio1990[, quantile(publications, 0.8), by='age']$V1)
  npub_autA[1] = round(quantile(DT_aut_bio1990[age==1, ]$publications, 0.99))
  ncit_autA = matrix(NA, nrow=sum(npub_autA))
  
  DT_pub_autA = data.table()
  set.seed(66)
  count = 0
  for(i in 1:23){
    for(j in 1:npub_autA[i]){
      count = count + 1
      q = runif(1, min=0, max=0.1)
      DT_tmp = DT_pub_bio1990[start == 1990+i-1, list(citations = round(quantile(citations, q))), by='age']
      DT_tmp[, `:=`(pub.id = -count, aut.id = -1, start = 1990+i-1, year = (1990+i-1):2012, cum.citations=cumsum(citations))]
      DT_pub_autA = rbind(DT_pub_autA, DT_tmp)
    }
  }
  
  # Author B publishes one highly cited paper throughout the entire career
  q = 0.999
  DT_pub_autB = DT_pub_bio1990[start == 1990, list(citations = round(quantile(citations, q))), by='age']
  DT_pub_autB[, `:=`(pub.id = -sum(npub_autA)-1, aut.id = -2, start = 1990, year = 1990:2012, cum.citations=cumsum(citations))]
  
  # Author C publishes one medium-impact paper throughout the entire career
  q = 0.4
  DT_pub_autC = DT_pub_bio1990[start == 1990, list(citations = round(quantile(citations, q))), by='age']
  DT_pub_autC[, `:=`(pub.id = -sum(npub_autA)-2, aut.id = -3, start = 1990, year = 1990:2012, cum.citations=cumsum(citations))]
  
  ## merge and calculate scholar rank percentiles
  DT_pub_bio1990 = rbind(DT_pub_bio1990, DT_pub_autA, DT_pub_autB, DT_pub_autC)
  DT_pub_bio1990[, `:=`(P.c.bio = perc.rank(cum.citations), P.c.all=NA, P.c.tenured=NA), by='age']
  return(DT_pub_bio1990)
}
DT_syn_pub = gen.career(DT_pub, DT_aut)
DT_syn_aut = calc.autrp(DT_syn_pub)
DT_syn_aut = DT_syn_aut[aut.id < 0]

## make the plot
plot.simulated.authors <- function(DT_syn_aut, file_path){
  benchmark = 'bio'
  auts = c('scholar A', 'scholar B', 'scholar C')
  metrics = c('S.c','S.h','S.P5')
  data_toplot = data.frame()
  for(i in 1:length(metrics)){
    DT_tmp = DT_syn_aut[age<=20, c('aut.id', paste(metrics[i], benchmark, sep='.'), 'age'), with=FALSE]
    colnames(DT_tmp)[2] = 'rp'
    DT_tmp[, `:=`(type = metrics[i])]
    DT_tmp$aut = auts[-DT_tmp$aut.id]
    data_toplot = rbind(data_toplot, DT_tmp)
  }
  data_toplot$type = factor(data_toplot$type)
  data_toplot$aut = factor(data_toplot$aut)
  
  p1 = ggplot(data=data_toplot, aes(x=age, y=rp, col=type, linetype=type, shape=type)) + geom_line(size=2) + geom_point(size=2, stroke=2)
  p1 = p1 + scale_colour_manual(name  = "",
                                breaks=c('S.c', 'S.h', 'S.P5'),
                                # labels=list(bquote(S[i~t]^{c}), bquote(S[i~t]^{h}), bquote(S[i~t]^{P5})),
                                labels=list(bquote(S[c]), bquote(S[h]), bquote(S[P5])),
                                values=c("#F8766D", "#00BA38", "#619CFF")) +
    scale_shape_manual(name  = "",
                       breaks=c('S.c', 'S.h', 'S.P5'),
                       # labels=list(bquote(S[i~t]^{c}), bquote(S[i~t]^{h}), bquote(S[i~t]^{P5})),
                       labels=list(bquote(S[c]), bquote(S[h]), bquote(S[P5])),
                       values=c(1, 2, 3)) +
    scale_linetype_manual(name  = "",
                          breaks=c('S.c', 'S.h', 'S.P5'),
                          # labels=list(bquote(S[i~t]^{c}), bquote(S[i~t]^{h}), bquote(S[i~t]^{P5})),
                          labels=list(bquote(S[c]), bquote(S[h]), bquote(S[P5])),
                          values=c(1, 2, 3)) 
  # p1 = p1 + ylab(expression(S[i~t]))
  p1 = p1 + xlab('t')
  p1 = p1 + ylab('rank percentiles')
  #p1 = p1 + facet_wrap(.~ aut, ncol=1) 
  p1 = p1 + facet_wrap(.~ aut, ncol=3) 
  #p1 = p1 + scale_color_discrete(labels=c(expression(rp.c^(c)), expression(rp.rp^(c)), expression(rp.h)))
  p1 = p1 + theme(strip.text = element_text(size=28)) 
  p1 = p1 + theme(legend.title=element_text(size=25), legend.text=element_text(size=25), legend.text.align = 0)
  p1 = p1 + theme(legend.position = 'bottom')
  p1 = p1 + theme(plot.title = element_text(size = 35, face = "bold", hjust=0.5), axis.text=element_text(size=25), axis.title=element_text(size=25))
  
  setEPS()
  postscript(file=paste0(base_fig, file_path), height=7, width=15)
  print(p1)
  dev.off()
}
plot.simulated.authors(DT_syn_aut, file_path = '/compare_autrp/simulated_authors.eps')

### Figure 3: Stationarity of scholar RP --------
plot.stationarity.tenured <- function(DT_pub, DT_aut, file_path, age.fix=5, chop.percent=1){
  make.plot <- function(data_toplot, chop.percent, x.label, y.label, title){
    names(data_toplot)[2] = 'metric'
    data_toplot$start = factor(data_toplot$start)
    p1 = ggplot(data=data_toplot, aes(x=start, y=metric)) + geom_boxplot()
    if(!is.null(chop.percent)){
      # compute lower and upper whiskers
      ylim1 = boxplot.stats(data_toplot$metric)$stats[c(1, 5)]
      # scale y limits based on ylim1
      p1 = p1 + coord_cartesian(ylim = ylim1*chop.percent)
    }
    p1 = p1 + xlab(x.label) + ylab(y.label)
    p1 = p1 + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
    p1 = p1 + theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
    p1 = p1 + theme(plot.title = element_text(size = 30, face = "bold", hjust=0.5), 
                    axis.text=element_text(size=20), axis.title=element_text(size=20))
    p1
  }
  
  pp = make.plot(DT_aut[age == age.fix & start >= 1980 & !is.na(S.P5.tenured), c('start', 'S.P5.tenured')], 
                 chop.percent=chop.percent,
                 x.label = 'starting year of careers', 
                 y.label = bquote(.(paste0('rank percentiles at age ', age.fix, ','))~S[P5]^i~(.(age.fix))), 
                 title = '')
  
  # print to file
  setEPS()
  postscript(file=paste0(base_fig, file_path), height=6, width=9)
  print(pp)
  dev.off()
}
plot.stationarity.tenured(DT_pub, DT_aut, age.fix=5, file_path = '/stationarity/rp_stationarity.eps')
### Figure 4 & 5: long-term predictability of publication citations and RP --------
plot.predictability.cit.rp <- function(DT_pub, file_dir, width = 10, height = 8, label.size = 16){
  make.plot <- function(p1, file.name){
    p1 = p1 + theme(plot.title = element_text(size = 25, face = "bold", hjust=0.5), axis.text=element_text(size=label.size), axis.title=element_text(size=25))
    cairo_ps(filename = paste0(base_fig, file_dir, file.name, '.eps'),
             width = width, height = height, fallback_resolution = 300)
    print(p1)
    dev.off()
  }
  # benchmark is biology
  DT_pub = DT_pub[!is.na(P.c.bio), ]
  # take publications with more than 30 years old and 50 citations at age 5
  subset_pubid = intersect( DT_pub[year == 2016 & age >= 30, ]$pub.id, DT_pub[age == 5 & cum.citations == 50, ]$pub.id )
  # plot their citations as a function of age
  data_toplot = DT_pub[pub.id %in% subset_pubid & age <= 30, ]
  data_toplot$pub.id = factor( data_toplot$pub.id )
  pp = list()
  p1 = ggplot(data=data_toplot, aes(x=age, y=cum.citations, group=pub.id)) + geom_line()
  # p1 = p1 + ylab(label = expression(c[j~t])) + xlab(label = 't')
  p1 = p1 + ylab(label = 'number of citations') + xlab(label = 't')
  make.plot(p1, 'cit_t')
  
  # plot their rank percentiles as a function of age
  p1 = ggplot(data=data_toplot, aes(x=age, y=P.c.bio, group=pub.id)) + geom_line()
  # p1 = p1 + ylab(label = expression(P[j~t]^c)) + xlab(label = 't')
  p1 = p1 + ylab(label = bquote('rank percentiles,' ~ P[c]^j~(t))) + xlab(label = 't')
  make.plot(p1, 'rp_t')
  
  # specify the intervals based on which we separate the publications
  intervals = c(seq(0,100,10), seq(200,1000,100), seq(2000,3400,200))
  subset_pubid = intersect( DT_pub[year == 2016 & age >= 30, ]$pub.id, DT_pub[age==30 & cum.citations < 3400, ]$pub.id )
  DT_pub_subset = DT_pub[pub.id %in% subset_pubid & age %in% c(5,30), ]
  # assign publications into different groups based on the specified intervals
  DT_pub_subset[, `:=`(group = min(which(intervals > max(cum.citations)))-1), by='pub.id']
  
  data_toplot = merge( DT_pub_subset[age==5, lapply(.SD, mean), by = 'group', .SDcols = c('cum.citations', 'P.c.bio')],
                       DT_pub_subset[age==30, list(cum.citations.30 = log(mean(cum.citations), base=10), P.c.bio.30 = log(mean(P.c.bio)/(1-mean(P.c.bio)))), by = 'group'],
                       by = 'group')
  
  
  p1 = ggplot(data=data_toplot, aes(x=cum.citations.30, y=cum.citations)) + geom_line(size=2) + geom_point(size=5) 
  region_left = data_toplot[group == nrow(data_toplot)-5,]$cum.citations.30
  region_right = max(data_toplot$cum.citations.30)
  p1 = p1 + geom_rect(aes(xmin=region_left, xmax=region_right, ymin=0, ymax=max(data_toplot$cum.citations)+20 ), fill=3, alpha=0.01)
  p1 = p1 + scale_x_continuous(# name = expression(bar(c)[j~30]),
    name = 'average citations at age 30',
    breaks = c(seq(0,region_right), region_left, region_right), 
    labels = round(10^c(seq(0,region_right), region_left, region_right), 0))
  # p1 = p1 + ylab(label = expression(bar(c)[j~5]))
  p1 = p1 + ylab(label = 'average citations at age 5')
  make.plot(p1, 'cit_cit')
  
  
  p1 = ggplot(data=data_toplot, aes(x=P.c.bio.30, y=P.c.bio)) + geom_line(size=2) + geom_point(size=5) 
  region_left = data_toplot[group == nrow(data_toplot)-5,]$P.c.bio.30
  region_right = max(data_toplot$P.c.bio.30)
  p1 = p1 + geom_rect(aes(xmin=region_left, xmax=region_right, ymin=0, ymax=1), fill=3, alpha=0.01)
  p1 = p1 + scale_x_continuous(# name = expression(bar(P)[j~30]^c),
    name = bquote('average rank percentiles at age 30,' ~ P[c]^j~(30)),
    breaks = c(-2.5, 0, 2.5, region_left, region_right), 
    labels = numformat(1/(1+exp(-c(-2.5, 0, 2.5, region_left, region_right)))))
  # p1 = p1 + ylab(label = expression(bar(P)[j~5]^c)) 
  p1 = p1 + ylab(label = bquote('average rank percentiles at age 5,' ~ P[c]^j~(5)))
  make.plot(p1, 'rp_rp')
}
plot.predictability.cit.rp(DT_pub, file_dir = '/pred_power/')

### Figure 6 & S2: Pearson correlation between RP at two different ages --------
## heatmap for the predictibility of RP, e.g. correlation between RP at age t1 and RP at age t2
plot.heatmap <- function(rp, types, file_path, future=FALSE, which.metric = 'cor', height=18, width=18, fill.min=NULL){
  pred = seq(5,25,by=5)
  true = seq(10,30,by=5)
  
  # which metric to calculate
  f = match.fun(which.metric)
  # update the names
  tmp_function <- function(x){
    names(x) = c('id', 'age', 'metric')
    x
  }
  if(future){
    rp = lapply(rp, function(xx){lapply(xx, tmp_function)})
  }else{
    rp = lapply(rp, tmp_function)
  }
  
  # data frame for ggplot
  data_toplot = list()
  count = 0 
  for(k in 1:length(types)){
    rp_tmp = rp[[k]]
    for(i in pred){
      if(future){
        rp_tmp = rp[[k]][[paste0('T1=', i)]]
      }
      for(j in true){
        count = count + 1
        if(j > i){
          subset_id = rp_tmp[age == j, ]$id
          tmp = f(rp_tmp[id %in% subset_id & age==i, ]$metric, rp_tmp[id %in% subset_id & age==j, ]$metric)
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
  
  # make the plot
  p1 = ggplot(data = data_toplot, mapping = aes(x=truth,y=prediction)) + geom_tile(aes(fill = value)) + 
    geom_text(aes(label = round(value, 2)), size = 10) 
  if(is.null(fill.min)){
    fill.min = min(data_toplot[!is.na(data_toplot$value),]$value)
  }
  if(which.metric == 'rmse'){
    p1 = p1 + scale_fill_gradient(low = "red", high = "white", limits=c(fill.min,1))
  }else{
    p1 = p1 + scale_fill_gradient(low = "white", high = "red", limits=c(fill.min,1))
  }
  p1 = p1 + facet_wrap(.~ type,  labeller=label_parsed, ncol=2) + xlab(label=expression(paste("age ", t[2]))) + ylab(label=expression(paste("age ", t[1])))
  p1 = p1 + theme(strip.text = element_text(size = 28)) + theme(legend.position="none")
  p1 = p1 + theme(plot.title = element_text(size = 35, face = "bold", hjust=0.5),
                  axis.text=element_text(size=25), axis.title=element_text(size=35))
  
  # write to file
  setEPS()
  postscript(file=paste0(base_fig, file_path), height=height, width=width)
  print(p1)
  dev.off()
}

types = c("'benchmark: all'",
          "'benchmark: biology'")
# Figure 6a
rp = list(DT_pub[, c('pub.id', 'age', 'P.c.all')], 
          DT_pub[!is.na(P.c.bio), c('pub.id', 'age', 'P.c.bio')] )
plot.heatmap(rp, types, fill.min=0, height=9, file_path = '/pred_power/heatmap_cor_pub.eps') 
# Figure 6b
rp = list(DT_aut[, c('aut.id', 'age', 'S.P5.all')], 
          DT_aut[!is.na(S.P5.bio), c('aut.id', 'age', 'S.P5.bio')] )
plot.heatmap(rp, types, fill.min=0, height=9, file_path = '/pred_power/heatmap_cor_aut.eps') 

# Figure 6c
rp = list(lapply(DT_aut_future, function(x){x[, c('aut.id', 'age', 'S.P5.all')]}),
          lapply(DT_aut_future, function(x){x[!is.na(S.P5.bio), c('aut.id', 'age', 'S.P5.bio')]}))
plot.heatmap(rp, types, future=TRUE, fill.min=0, height=9, file_path = '/pred_power/heatmap_cor_aut_future.eps') 

# Figure S2
types = c("S[c] ~ ', benchmark: all'",
          "S[c] ~ ', benchmark: biology'",
          "S[h] ~ ', benchmark: all'",
          "S[h] ~ ', benchmark: biology'")

rp = list(DT_aut[, c('aut.id', 'age', 'S.c.all')], 
          DT_aut[!is.na(S.c.bio), c('aut.id', 'age', 'S.c.bio')],
          DT_aut[, c('aut.id', 'age', 'S.h.all')], 
          DT_aut[!is.na(S.h.bio), c('aut.id', 'age', 'S.h.bio')])
plot.heatmap(rp, types, fill.min=0, file_path = '/pred_power/heatmap_cor_aut_cithindex.eps') 


### Figure 7 & S6: kernel density plot of scatter points for rank percentiles --------
plot.scatter <- function(DT, puboraut, file_path, specify.bandwidth=FALSE){
  tau1 = seq(5,25,by=5)
  tau2 = seq(10,30,by=5)
  data_toplot = list()
  reg_result = c()
  count = 0
  for(i in tau1){
    for(j in tau2){
      count = count + 1
      if(j > i){
        if(puboraut == 'pub'){
          rp_tau1 = DT[age == i & start == 1980 & !is.na(P.c.bio), 'P.c.bio'][[1]]
          rp_tau2 = DT[age == j & start == 1980 & !is.na(P.c.bio), 'P.c.bio'][[1]]
        }else if(puboraut == 'aut'){
          rp_tau1 = DT[age == i & start == 1980 & !is.na(S.P5.all), 'S.P5.all'][[1]]
          rp_tau2 = DT[age == j & start == 1980 & !is.na(S.P5.all), 'S.P5.all'][[1]]
        }
        data_toplot[[count]] = data.frame(rp_tau1 = rp_tau1, rp_tau2 = rp_tau2, i = paste0('t[1]==', i), j = paste0('t[2]==', j))
        tmp = c(round(summary( lm(rp_tau2 ~ rp_tau1) )$coefficients[2,1:2], 2))
        reg_result = c(reg_result, paste0(tmp[1],' (',tmp[2], ')'))
      }else{
        data_toplot[[count]] = data.frame(rp_tau1=NA, rp_tau2=NA, i=paste0('t[1]==', i), j=paste0('t[2]==', j))
      }
    }
  }
  data_toplot = do.call(rbind, data_toplot)
  data_toplot$i = factor(data_toplot$i, levels=paste0("t[1]==",rev(seq(5,25,by=5))))
  data_toplot$j = factor(data_toplot$j, levels=paste0("t[2]==",seq(10,30,by=5)))
  
  
  p1 = ggplot(data_toplot, aes(x=rp_tau2, y=rp_tau1)) 
  p1 = p1 + geom_density_2d() 
  if(specify.bandwidth){
    p1 = p1 + stat_density_2d(aes(fill = stat(nlevel)), geom="polygon", h=c(0.7,0.7))
  }else{
    p1 = p1 + stat_density_2d(aes(fill = stat(nlevel)), geom="polygon")
  }
  
  p1 = p1 + facet_grid(i~j, labeller = label_parsed) + scale_fill_viridis_c()
  # print(p1)
  
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
  p1 = p1 + geom_text(data=data.frame(x=x_tmp, y=y_tmp, label=reg_result, 
                                      i=factor(paste0('t[1]==', a[a<b])),
                                      j=factor(paste0('t[2]==', b[a<b]))), 
                      aes(x,y,label=label), inherit.aes=FALSE, size=text_size)
  
  #p1 = p1 + facet_wrap(.~type, labeller = label_parsed, ncol=5) + scale_fill_viridis_c()
  #p1 = p1 + xlab(label=expression( rp[. *tau[2]]^(c) )) + ylab(label=expression( rp[. *tau[1]]^(c) ))
  if(puboraut == 'pub'){
    # p1 = p1 + xlab(label=expression( P[j~t[2]]^c )) + ylab(label=expression( P[j~t[1]]^c ))
    p1 = p1 + xlab(label=expression( P[c]^j~(t[2]) )) + ylab(label=expression( P[c]^j~(t[1]) ))
  }else if(puboraut == 'aut'){
    # p1 = p1 + xlab(label=expression( S[i~t[2]]^{P5} )) + ylab(label=expression( S[i~t[1]]^{P5} ))
    p1 = p1 + xlab(label=expression( S[P5]^i~(t[2]) )) + ylab(label=expression( S[P5]^i~(t[1]) ))
  }
  
  #p1 = p1 + ggtitle(title)
  #p1 = p1 + scale_alpha_continuous(limits=c(0,16), breaks=seq(0,16,length.out = 3))
  p1 = p1 + scale_x_continuous(breaks=c(0,0.5,1)) + scale_y_continuous(breaks=c(0,0.5,1))
  
  p1 = p1 + theme(legend.text = element_text(size = 25), strip.text = element_text(size=25), legend.title = element_blank())
  p1 = p1 + theme( axis.text=element_text(size=15), axis.title=element_text(size=25))
  
  setEPS()
  postscript(file=paste0(base_fig, file_path), height=15, width=18)
  print(p1)
  dev.off()
}
plot.scatter(DT_pub, 'pub', file_path = '/pred_power/scatter_pubrp_bio1980.eps')
plot.scatter(DT_aut, 'aut', file_path = '/pred_power/scatter_autrp_all.eps', TRUE)


### Figure 8 & S7-S9: testing errors (R^2, MAE, RMSE, RMEDSE) of the predictive models --------
plot.errors <- function(errors, which_error, file_dir){
  age_training = seq(5,25,by=5)
  values = methods = tau1 = metric = pred_age = c()
  metric_names = c("'cumulative paper impact'", "'cumulative scholar impact'", "'future scholar impact'")
  tau1_names = c("t[1] == 5", "t[1] == 10", "t[1] == 15", "t[1] == 20", "t[1] == 25")
  allmethods = c('baseline', 'sm', 'gamlr', 'rf', 'xgbtree', 'nnet')
  
  for(i in 1:length(metric_names)){
    for(j in 1:length(age_training)){
      values = c(values, as.vector(errors[[i]][[j]][[which_error]][allmethods,])  )
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
  data_toplot$methods = factor(data_toplot$methods, levels=allmethods)
  
  p1 = ggplot(data=data_toplot, aes(x=pred_age, y=values, group=methods)) + geom_line(size=2, aes(color=methods, linetype=methods)) + geom_point(size=1, aes(color=methods)) 
  if(grepl('r2', which_error)){
    p1 = p1 + facet_grid(tau1~metric, labeller = label_parsed) 
  }else{
    p1 = p1 + facet_grid(tau1~metric, scales='free_y', labeller = label_parsed) 
  }
  p1 = p1 + xlab(label = expression(t[2])) 
  
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
  
  p1 = p1 + theme(plot.title = element_text(size = 25, face = "bold", hjust=0.5), axis.text=element_text(size=20), axis.title=element_text(size=30,face="bold"))
  p1 = p1 + theme(legend.text = element_text(size = 30), legend.title = element_blank())
  p1 = p1 + theme(strip.text.x = element_text(size = 25), strip.text.y = element_text(size = 25))
  setEPS()
  postscript(file=paste0(base_fig, file_dir, which_error,'.eps'), height=20, width=18)
  print(p1)
  dev.off()
}
errors = readRDS(paste0(base,'/results/errors.rds'))
for(which_error in names(errors$P.c$`5`)){
  plot.errors(errors, which_error, file_dir = '/pred_model/')
}

### Figure S1: classification of different types of scholar RP --------
# assign each author rp into a class with 4 levels (0-0.25, 0.25-0.5, 0.5-0.75, 0.75-1)
# and calculate the agreement (in percentage) in each of the 4 levels, between each 2 types of rp 
zero.range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}

classes.pivot <- function(DT_aut, age.fixed, benchmark){
  names_scholar_rp = c('S.c', 'S.h', 'S.P5')
  # split into 4 different classes
  value.into.class <- function(x){
    tmp = x
    tmp[x>=0 & x<0.25] = 1
    tmp[x>=0.25 & x<0.5] = 2
    tmp[x>=0.5 & x<0.75] = 3
    tmp[x>=0.75 & x<=1] = 4
    tmp
  }
  DT_aut_classes = DT_aut[!is.na(S.P5.bio) & age == age.fixed, lapply(.SD, value.into.class), .SDcols = paste(c('S.c', 'S.h', 'S.P5'), benchmark, sep='.')]
  names(DT_aut_classes) = names_scholar_rp
  output = list()
  allcases = combn(names_scholar_rp, 2)
  for(j in 1:ncol(allcases)){
    output[[paste0(allcases[1,j], '-', allcases[2,j])]] = 
      prop.table( table(DT_aut_classes[, allcases[,j], with=FALSE]) )
  }
  print(paste0("The agreement of all three types is ", round(100*sum(apply(DT_aut_classes, 1, zero.range)) / nrow(DT_aut_classes), 0), "%"))
  output
}
result = list()
result[['age: 5']] = classes.pivot(DT_aut, 5, 'bio')
result[['age: 30']] = classes.pivot(DT_aut, 30, 'bio')

heatmap.class.agreement <- function(result, file_path){
  agreement = lapply(result, function(x){round(100*unlist(lapply(x, function(y){sum(diag(y))} )))})
  name.to.expression <- function(type_name){
    if(type_name == 'S.c'){
      # "S'[i~t]^{c}"
      "S'[c]"
    }else if(type_name == 'S.h'){
      # "S'[i~t]^{h}"
      "S'[h]"
    }else if(type_name == 'S.P5'){
      # "S'[i~t]^{P5}"
      "S'[P5]"
    }
  }
  
  data_toplot = data.frame()
  for(j in 1:length(result[[1]])){
    for(age_name in names(result)){
      tmp = melt(result[[age_name]][[j]], value.name = 'percentage')
      types = colnames(tmp)[1:2]
      colnames(tmp)[1:2] = c('x', 'y')
      tmp$type = paste0("'x: ", name.to.expression(types[1]), " ~ ', y: ",  
                        name.to.expression(types[2]), " ~ ', ", age_name, ", agreed: ", 
                        round(100*sum(diag(result[[age_name]][[j]])), 0), "%'")
      data_toplot = rbind(data_toplot, tmp)
    }
  }
  data_toplot$percentage = round(100*data_toplot$percentage, 0)
  data_toplot$type = factor(data_toplot$type, levels = unique(data_toplot$type))
  
  
  p1 = ggplot(data = data_toplot, mapping = aes(x=x,y=y)) + geom_tile(aes(fill = percentage)) + 
    geom_text(aes(label = percentage), size = 10)
  p1 = p1 + scale_fill_gradient(low = "white", high = "red")
  p1 = p1 + facet_wrap(.~ type, labeller = label_parsed, ncol=2) + xlab(label='class of x') + ylab(label='class of y')
  #p1 = p1 + facet_wrap(.~ type, labeller = label_parsed, ncol=2) + xlab(label='class of x') + ylab(label='class of y')
  # p1 = p1 + ggtitle('classifications (in %) of various types of scholar RP')
  p1 = p1 + theme(strip.text = element_text(size=28)) + theme(legend.position="none")
  p1 = p1 + theme(plot.title = element_text(size = 35, hjust=0.5), axis.text=element_text(size=25), axis.title=element_text(size=35))
  
  setEPS()
  postscript(file=paste0(base_fig, file_path), height=25, width=18)
  print(p1)
  dev.off()
}
heatmap.class.agreement(result, file_path = '/compare_autrp/heatmap_class_agreement.eps')


### Figure S3: Pearson correlation between m.P5 and other evaluation metrics --------
## different ways to summarize the RP metric at each year of a scholar
calc.metrics.rp.auti <- function(aut.i.id, DT_pub){
  # subset the data for scholar i
  DT_auti = DT_pub[aut.id == aut.i.id, ]
  # evaluate each publication
  get.m.pub <- function(x, type){
    if(type == 'P5'){
      if(length(x)>=5){
        x[5]
      }else{
        x[length(x)]
      }
    }else if(type == 'P10'){
      if(length(x)>=10){
        x[10]
      }else{
        x[length(x)]
      }
    }else if(type == 'max'){
      max(x)
    }else if(type == 'mean'){
      mean(x)
    }else if(type == 'median'){
      median(x)
    }
  }
  DT_m_pub = DT_auti[, list(P5 = get.m.pub(P.c.all, 'P5'), P10 = get.m.pub(P.c.all, 'P10'),
                            mean = get.m.pub(P.c.all, 'mean'), median = get.m.pub(P.c.all, 'median'),
                            max = get.m.pub(P.c.all, 'max'), start = unique(start)), by='pub.id']
  # sum the metric of publications at each year
  start_year = min(DT_m_pub$start)
  get.m.aut <- function(metric){
    unlist(lapply(start_year:2016, function(t){sum(DT_m_pub[start<=t, metric, with=FALSE])}))
  }
  allmetrics = c('P5', 'P10', 'mean', 'median', 'max')
  DT_metrics = data.table(do.call(cbind, lapply(allmetrics, get.m.aut)))
  colnames(DT_metrics) = allmetrics
  # DT_metrics$year = start_year:2016
  DT_metrics[, `:=`(age = 1:.N, aut.id = aut.i.id)]
  return(DT_metrics)
}
calc.metrics.rp <- function(DT_pub){
  DT_tmp = do.call(rbind, mapply(calc.metrics.rp.auti, unique(DT_pub$aut.id), MoreArgs = list(DT_pub), SIMPLIFY = FALSE, USE.NAMES = FALSE))
  return(DT_tmp)
}
DT_metrics_rp = calc.metrics.rp(DT_pub)
## plot the correlations
plot.cor.metrics.rp <- function(DT_metrics_rp, file_path){
  # calculate the correlation at each age for the evaluation metrics (m_it)
  correlations = DT_tmp[,lapply(.SD, function(x){cor(x, P5)}), by='age',.SDcols=c('mean', 'max', 'median', 'P10')]
  data_toplot = data.frame(values = unlist(correlations[1:30,2:5]), 
                           age = rep(1:30, 4),
                           type = rep(c('mean', 'max', 'median', 'age10'), each=30)) 
  data_toplot$type = factor( data_toplot$type )
  p1 = ggplot(data=data_toplot, aes(x=type, y=values)) + geom_boxplot()
  p1 = p1 + xlab('') + ylab('')
  p1 = p1 + theme(axis.text=element_text(size=25))
  setEPS()
  postscript(file=paste0(base_fig, file_path), height=6, width=10)
  print(p1)
  dev.off()
}
plot.cor.metrics.rp(DT_metrics_rp, file_path = '/robustness/cor.eps')
### Figure S4: Stationarity test for the RP series --------
calc.stationarity.teststat <- function(DT_pub, DT_aut){
  ## take a subset of the publications or scholars that have no more than age 30
  ## calculate the differenced series
  DT_pub_1980s = DT_pub[!is.na(P.c.bio) & start %in% seq(1980,1987) & age<=30]
  DT_pub_1980s[, `:=`(P.c.bio.diff = c(NA, diff(P.c.bio)) ), by='pub.id']
  DT_aut_1980s = DT_aut[!is.na(S.P5.bio) & start %in% seq(1980,1987) & age<=30]
  DT_aut_1980s[, `:=`(S.P5.bio.diff = c(NA, diff(S.P5.bio)) ), by='aut.id']
  
  ## calculate the test statistics
  DT_pub_teststat = DT_pub_1980s[, list(df = ur.df(P.c.bio, type='drift', lags=0)@teststat[1,'tau2'],
                                        df.diff = ur.df(P.c.bio.diff[!is.na(P.c.bio.diff)], type='drift', lags=0)@teststat[1,'tau2'],
                                        kpss = kpss.test(P.c.bio, null = 'Level')$statistic,
                                        kpss.diff = kpss.test(P.c.bio.diff[!is.na(P.c.bio.diff)], null = 'Level')$statistic), 
                                 by='pub.id']
  DT_pub_teststat$pub.id = NULL
  
  DT_aut_teststat = DT_aut_1980s[, list(df = ur.df(S.P5.bio, type='drift', lags=0)@teststat[1,'tau2'],
                                        df.diff = ur.df(S.P5.bio.diff[!is.na(S.P5.bio.diff)], type='drift', lags=0)@teststat[1,'tau2'],
                                        kpss = kpss.test(S.P5.bio, null = 'Level')$statistic,
                                        kpss.diff = kpss.test(S.P5.bio.diff[!is.na(S.P5.bio.diff)], null = 'Level')$statistic), 
                                 by='aut.id']
  DT_aut_teststat$aut.id = NULL
  return(list(pub=DT_pub_teststat, aut=DT_aut_teststat))
}
DT_teststat = calc.stationarity.teststat(DT_pub, DT_aut)

# function to plot test statistics with critical values
plot.teststat <- function(DT_pub_teststat, DT_aut_teststat, testcritical, file_path){
  n_pub = nrow(DT_pub_teststat)
  n_aut = nrow(DT_aut_teststat)
  data_toplot = data.frame(
    values = c(as.vector(as.matrix(DT_pub_teststat)), as.vector(as.matrix(DT_aut_teststat))),
    type = c(rep(rep(c('P.c', 'P.c.diff'), each=n_pub), 2), rep(rep(c('S.P5', 'S.P5.diff'), each=n_aut), 2)),
    which_test = rep(rep(c("Dicky-Fuller test", "KPSS test"), 2), times = c(rep(2*n_pub, 2), rep(2*n_aut, 2)))
  )
  data_toplot$type = factor(data_toplot$type, level = c('P.c', 'S.P5', 'P.c.diff', 'S.P5.diff'))
  data_toplot$which_test = factor(data_toplot$which_test)
  
  ylim1 = boxplot.stats(data_toplot$values[data_toplot$which_test=='Dicky-Fuller test'])$stats[c(1, 5)]
  ylim2 = boxplot.stats(data_toplot$values[data_toplot$which_test=='KPSS test'])$stats[c(1, 5)]
  data_toplot = data_toplot[data_toplot$values >= min(c(ylim1, ylim2)) & data_toplot$values <= max(c(ylim1, ylim2)),]
  
  dummy = data.frame(which_test = factor(c("Dicky-Fuller test", "KPSS test")), z = testcritical)
  p1 = ggplot(data=data_toplot, aes(x=type, y=values)) + geom_boxplot()
  p1 = p1 + facet_wrap(.~which_test, ncol=2, scale='free_y') 
  p1 = p1 + ylab('test statistics')
  p1 = p1 + scale_x_discrete(
    name = '',
    # labels = c(expression( P[j~t]^c ),
    #            expression( S[i~t]^{P5} ),
    #            expression( Delta~P[j~t]^c),
    #            expression( Delta~S[i~t]^{P5}))
    labels = c(expression( P[c] ),
               expression( S[P5] ),
               expression( Delta~P[c] ),
               expression( Delta~S[P5] ))
  )
  p1 = p1 + geom_hline(data = dummy, aes(yintercept = z), linetype='dashed')
  p1 = p1 + theme(plot.title = element_text(size = 25, face = "bold", hjust=0.5), axis.text=element_text(size=20), axis.title=element_text(size=30,face="bold"))
  p1 = p1 + theme(strip.text.x = element_text(size = 25))
  setEPS()
  postscript(file=paste0(base_fig, file_path), height=8, width=15)
  print(p1)
  dev.off()
}
# critical values for both tests at 5% significance level
testcritical = c(adf = -2.93, kpss = 0.463)
plot.teststat(DT_teststat$pub, DT_teststat$aut, testcritical, file_path = '/stationarity/df_kpss.eps')





### Figure S5: exploratory plot of the dataset --------
# group publications or scholars into different periods based on the starting year
n.group <- function(DT, type, benchmark){
  groupnames = c('1902-1976', '1977-1981', '1982-1986', '1987-1991', '1992-1996', '1997-2001', '2002-2006', '2007-2011', '2012-2016')
  group_boundary = seq(1976, 2016, by=5)
  ngroup = length(groupnames)
  
  DT_tmp = unique(DT[, c(paste0(type, '.id'), 'start'), with=FALSE])
  DT_tmp[, `:=`(group = unlist( lapply(DT_tmp$start, function(t){groupnames[min(which(group_boundary >= t))]}) ))]
  DT_tmp = DT_tmp[, .N, by='group']
  DT_tmp[, `:=`(benchmark = benchmark)]
  return(DT_tmp)
}

plot.exploratory <- function(DT_pub, DT_aut, file_path){
  make.plot <- function(data_toplot, title, type){
    p1 = ggplot(data=data_toplot, aes(x=group, y=N, fill=factor(benchmark, levels=c('tenured', 'biology', 'all')))) 
    p1 = p1 + geom_bar(stat="identity", position=position_dodge()) 
    p1 = p1 + scale_fill_brewer(palette="Reds", direction=-1)
    p1 = p1 + theme(axis.text.x = element_text(angle = 45, hjust = 0.6, vjust = 0.5), axis.title.x = element_blank())
    if(type == 'naut'){
      p1 = p1 + scale_y_continuous(name = element_blank(), breaks = seq(1,3), labels=expression(10^1,10^2,10^3))
    }else if(type == 'npub'){
      p1 = p1 + scale_y_continuous(name = element_blank(), breaks = seq(1,5), labels=expression(10^1,10^2,10^3,10^4,10^5))
    }else if(type == 'ncit'){
      p1 = p1 + scale_y_continuous(name = element_blank())
    }
    p1 = p1 + ggtitle(title) 
    p1 = p1 + guides(fill=guide_legend(title = 'benchmark', title.position = "left", title.theme = element_text(size=20), label.theme = element_text(size=25), nrow=1))
    p1 = p1 + theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5), axis.text=element_text(size=20), axis.title=element_text(size=25,face="bold"))
    p1
  }
  # number of publications in each period
  npub_group = do.call(rbind, list(
    n.group(DT_pub, 'pub', 'all'),
    n.group(DT_pub[!is.na(P.c.bio)], 'pub', 'biology'),
    n.group(DT_pub[!is.na(P.c.tenured)], 'pub', 'tenured')
  ))
  npub_group$N = log(npub_group$N, base=10)
  # number of scholars who start their careers in a given period
  naut_group = do.call(rbind, list(
    n.group(DT_aut, 'aut', 'all'),
    n.group(DT_aut[!is.na(S.c.bio)], 'aut', 'biology'),
    n.group(DT_aut[!is.na(S.c.tenured)], 'aut', 'tenured')
  ))
  naut_group$N = log(naut_group$N, base=10)
  # plot
  pp = list()
  pp[['npub']] = make.plot(npub_group, '# publications', 'npub')
  pp[['naut']] = make.plot(naut_group, '# scholars (starting years of career)', 'naut')
  
  mylegend = g_legend(pp$npub)
  
  setEPS()
  postscript(file=paste0(base_fig, file_path), height=8, width=12)
  pp0 <- grid.arrange(arrangeGrob(pp$npub+ theme(legend.position="none"),
                                  pp$naut+ theme(legend.position="none"),
                                  nrow=1,ncol=2,heights=rep(5,1)),
                      mylegend,nrow=2,heights=c(9,1))
  print(pp0)
  dev.off()
}
plot.exploratory(DT_pub, DT_aut, file_path = '/exploratory/npub_naut.eps')

### Table S4: exploratory info of the dataset --------
print.exploratory.info <- function(DT_pub, DT_aut){
  for(benchmark in c('all', 'bio', 'tenured')){
    pub_id_benchmark = unique( DT_pub[which(!is.na(DT_pub[, paste0('P.c.', benchmark), with=FALSE]))]$pub.id )
    aut_id_benchmark = unique( DT_aut[which(!is.na(DT_aut[, paste0('S.c.', benchmark), with=FALSE]))]$aut.id )
    print( paste0( "Benchmark ", benchmark, ", # pulications: ", 
                   length(pub_id_benchmark)  ))
    print( paste0( "Benchmark ", benchmark, ", # scholars: ", 
                   length(aut_id_benchmark)  ))
    print( paste0( "Benchmark ", benchmark, ", # citations per publication by age 5: ",
                   round(mean( DT_pub[age == 5 & pub.id %in% pub_id_benchmark, ]$cum.citations ))))
    print( paste0( "Benchmark ", benchmark, ", # citations per scholar by age 5: ",
                   round(mean( DT_aut[age == 5 & aut.id %in% aut_id_benchmark, ]$cum.citations ))))
  }
}
print.exploratory.info(DT_pub, DT_aut)




### Figure S4, S5 & S6: use median publication impact to formulate scholar rank percentile --------
# read data
DT_aut_median = readRDS(paste0(base, '/data/rp/aut_median.rds'))
DT_aut_future_median = readRDS(paste0(base, '/data/rp/aut_future_median.rds'))
# Figure S4
plot.stationarity.tenured(DT_pub, DT_aut_median, age.fix=5, file_path = '/stationarity/rp_stationarity_median.eps')

# Figure S5
# generate artificial scholars
DT_syn_pub = gen.career(DT_pub, DT_aut)
DT_syn_aut_median = calc.autrp(DT_syn_pub, fun.agg = median)
DT_syn_aut_median = DT_syn_aut_median[aut.id < 0]
plot.simulated.authors(DT_syn_aut_median, file_path = '/compare_autrp/simulated_authors_median.eps')

# Figure S6a
types = c("'benchmark: all'",
          "'benchmark: biology'")
rp = list(DT_aut_median[, c('aut.id', 'age', 'S.P5.all')], 
          DT_aut_median[!is.na(S.P5.bio), c('aut.id', 'age', 'S.P5.bio')] )
plot.heatmap(rp, types, fill.min=0, height=9, file_path = '/pred_power/heatmap_cor_aut_median.eps') 

# Figure S6b
rp = list(lapply(DT_aut_future_median, function(x){x[, c('aut.id', 'age', 'S.P5.all')]}),
          lapply(DT_aut_future_median, function(x){x[!is.na(S.P5.bio), c('aut.id', 'age', 'S.P5.bio')]}))
plot.heatmap(rp, types, future=TRUE, fill.min=0, height=9, file_path = '/pred_power/heatmap_cor_aut_future_median.eps') 

# autid_bio1990 = DT_aut[start == 1990 & !is.na(S.c.bio)]$aut.id
# DT_pub_bio1990 = DT_pub[aut.id %in% autid_bio1990 & year<=2012]
# DT_pub_bio1990[, c('P.c.all', 'P.c.bio', 'P.c.tenured')] = NULL
# 
# 
# DT_lastyear = DT_pub_bio1990[, max(start), by='aut.id']
# table(DT_lastyear[,'V1'])
