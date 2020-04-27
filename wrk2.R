library(magrittr)
library(data.table)
library(ggplot2)
library(igraph)
load('Out/wok2dbl.RData')
load('Out/crm.RData')

# network time series of cluster relationships ----------------------------
setkey(wok2dbl,field)

wts<-merge(
  wok2dbl['CR',.(id,cr=val)]
  ,wok2dbl['PY',.(id,py=val %>% as.integer)]
  ,by='id'
) # wok time series
wts<-merge(wts,crm[,.(cr,m,mh)],by='cr')
save(wts,file='Out/wts.RData')

# network
load('Out/wts.RData')
load('Out/cgr_culled.RData')
load('Out/gcr_cull.RData')
setkey(crm,cr)
nts<-list() #network time series
nodew<-function(x) (1+sqrt(1+8*x))/2
interval<-55 #interval of years for the final printout — make sure to pick an even number of years!
for(i in seq(1910,2015,interval)){ # 
  yr<-c(i,i+(interval-1))
  cat(yr,'...')
  x<-wts[between(py,yr[1],yr[2])] %>% unique
  x<-graph_from_edgelist(x[,.(id,cr)] %>% as.matrix,directed = F)
  V(x)$type<-grepl('^WOS:',V(x)$name)
  x<-bipartite_projection(x,which='false')
  x<-subgraph.edges(x,which(E(x)$weight>1))
  el<-x %>% as_edgelist() %>% data.table %>% setnames(c('s','r'))
  el<-el[,.(s=crm[s,m],r=crm[r,m])]
  el<-el %>% apply(1,sort) %>% t %>% data.table %>% setnames(c('s','r'))
  el<-el[,.N,by=.(s,r)]
  vs<-el[s==r,.(m=s,N)] # it internal tie
  el<-el[s!=r]
  y<-graph_from_edgelist(el[,!'N'] %>% as.matrix,directed = F)
  E(y)$weight<-el[,nodew(N)] # el[,N/max(N)*10] # update with current edge counting
  setkey(vs,m)
  vs<-vs[V(y)$name,N]
  vs[is.na(vs)]<-0
  V(y)$size<-nodew(vs) # vs/max(vs)*10 # update with current node counting
  y$name<-paste(yr,collapse='-')
  nts[[y$name]]<-y
  cat(' done.\n')
}
save(nts,file='Out/nts.RData')
#if(F){
#   pdf('nts.pdf')
#   lapply(nts,function(y) 
#     plot(
#       y
#       ,vertex.size=V(y)$size %>% lintran(c(1,8)) # node size, default 15 lintran, first number is floor, second ceiling
#       ,edge.width=.5 #E(y)$weight
#       ,main=y$name
#       ,edge.arrow.size=0
#       ,vertex.label.cex=.2
# ,vertex.frame.color=NA
# ,edge.curved=.1 # smaller number less curvature 0, 1
# ,edge.color=E(y)$weight %>% lintran(c(.1,.5)) %>% sapply(gray,level=0) # opacity lintran 0 1
#     ))
#   dev.off()
# }
