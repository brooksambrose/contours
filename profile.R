if(!'dbl2bel'%in%ls()) load('d/p/dbl2bel.RData')
try(dbl2bel<-dbl2bel[!(pend|loop),.(ut,cr)])
library(data.table)
library(magrittr)
library(igraph)

nc<-floor(parallel::detectCores()*.75)
data.table::setDTthreads(nc)

withr::with_seed(12345,i<-dbl2bel[,ut %>% unique %>% {sample(.,round(length(.)/100))}])
rm(pf)
pf<-profvis::profvis({
  
  x<-dbl2bel[i,.(ut,cr)][,data.table(do.call(rbind,lapply(1:(length(cr) - 1), function(y) matrix(cr[c(rep(y,length(cr) - y), (y + 1):length(cr))], ncol = 2)))),by = ut]
  
  dbl2bel[i,.(ut,cr)][,{data.table(do.call(rbind,lapply(1:(length(cr) - 1), function(y) matrix(cr[c(rep(y,length(cr) - y), (y + 1):length(cr))], ncol = 2)))) %>% setnames(c('cr1','cr2')) %>% fwrite(file='d/b/bel2mel.txt.gz',quote=F,sep='\t',append=T)},by = ut];y<-fread(file = 'd/b/bel2mel.txt.gz')
  
})

pf

#  {g<-graph_from_edgelist(dbl2bel %>% as.matrix);V(g)$type<-grepl('^WOS:',V(g)$name);g<-bipartite_projection(g,which='false',)}

#y<-dbl2bel[i,.(ut,cr)][,rbindlist(lapply(1:(length(cr) - 1), function(y) data.table(matrix(cr[c(rep(y,length(cr) - y), (y + 1):length(cr))], ncol = 2)))), by = ut]

#w<-pbapply::pblapply(i,function(j) dbl2bel[j,.(ut,cr)][,rbindlist(lapply(1:(length(cr) - 1), function(y) data.table(matrix(cr[c(rep(y,length(cr) - y), (y + 1):length(cr))], ncol = 2)))), by = ut],cl=nc) %>% rbindlist

#z<-pbapply::pblapply(split(dbl2bel[i,.(ut,cr)],by='ut'),function(j) j[,rbindlist(lapply(1:(length(cr) - 1), function(y) data.table(matrix(cr[c(rep(y,length(cr) - y), (y + 1):length(cr))], ncol = 2)))), by = ut],cl=nc) %>% rbindlist
