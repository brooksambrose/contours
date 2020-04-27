library(data.table)
library(magrittr)
library(igraph)
# # load('Out/wok2dbl.RData')
# # # setkey(wok2dbl,id)
# # # wok2dbl<-wok2dbl[wts$id %>% unique]
# # # load('Out/cgr_culled.RData')
# # # setkey(wok2dbl,field,val)
# # # wok2dbl<-wok2dbl[expand.grid('CR',cgr$names)]
# # source('https://raw.githubusercontent.com/brooksambrose/pack-dev/master/plagiat/R/dbl2bel.f.R')
# # bel<-dbl2bel.f(wok2dbl,'Out',check.for.saved.output = T)
# # source('https://raw.githubusercontent.com/brooksambrose/pack-dev/master/plagiat/R/bel2mel.f.R')
# # # mel<-bel2mel.f(bel[!(loop|pend)][,.(ut,cr)],type = 'crel',out = 'Out',check.for.saved.output = T)
# # load('Out/bel2mel.RData')
# # setkey(mel$crel,ew)
# # mel$crel<-mel$crel[!.(1)]
#
# # TODO rebuild from scratch later, starting with mercury (Brooks's server) results now
#
# load('Out/cgr_culled.RData')
# load('Out/crm.RData')
# # filter out all nodes that don't appear in top X (50) clusters
# # keep<-crm[as.integer(m)%in%1:50,cr] # list of every node in top 50 clusters
#
# # apply top 50 filter to high level color coding
# hlm<-table(cgr$memberships[4,]) %>% sort(decreasing=T)
# hlm<-cgr$memberships[4,] %>% factor(levels = names(hlm)) # hlm high level membership
# set.seed(12345)
# #hlm<-sample(viridisLite::viridis(10))[hlm %>% as.numeric]
# hlm<-RColorBrewer::brewer.pal(12,'Set3')[hlm %>% as.numeric]
# setkey(crm,cr)
# hlm<-data.table(crm[cgr$names,.(cr,m)],h=hlm)[!is.na(h),.N,keyby=.(h,m)] #[keep,on='cr'][!is.na(h),.N,keyby=.(h,m)]
# save(hlm,file='Out/hlm.RData')
# #
# load('Out/gcr_cull.RData')
# g<-induced_subgraph(gcr,keep)
# gorder(g)/gorder(gcr) # proportion of graph we're keeping
#
# # construct the cluster network (how clusters are related)
# # weight of node is count of internal coreferences
# # weight of edge is count of boundary crossing coreferences
#
# load('Out/crm.RData')
# el<-as_edgelist(g) %>% data.table %>% setnames(c('cr1','cr2'))
#
# el<-merge(el,crm[!duplicated(cr),.(cr,m)],by.x='cr1',by.y='cr') %>% setnames('m','m1')
# el<-merge(el,crm[!duplicated(cr),.(cr,m)],by.x='cr2',by.y='cr') %>% setnames('m','m2')
# el<-el[,.N,by=.(m1,m2)]
#
# nodew<-function(x) (1+sqrt(1+8*x))/2
# nl<-el[m1==m2,.(m=m1,ew=N,nw=nodew(N))] %>% setorder(-ew) # ew = edgeweight nw = nodeweight
#
# load('Out/wts.RData')
# nl<-merge(nl,wts[,.(my=mean(py)),by=m],by='m') # my = mean year, could change mean to any central tendency (e.g. median)
#
# el<-el[m1!=m2,.(m1,m2,ew=N,nw=nodew(N))]
#
# cln<-graph_from_data_frame(d = el[nw>quantile(nw,.5)],directed = F,vertices = nl) # cln = cluster network
# save(cln,file='Out/cln.RData')
#
# # # review size of clusters at each level
# # load('Out/cgr_culled.RData')
# # cgr$memberships %>% apply(1,function(x) x %>% table %>% sort(T) %>% head(50))
#
# source('https://raw.githubusercontent.com/brooksambrose/pack-dev/master/tilit/R/lintran.R')
# # single picture, temporal heatmap
# load('Out/cln.RData')
# lyt<-layout_with_fr(cln,weights = E(cln)$nw)
# clr<-nl$my %>% range %>% diff %>% {.*10} %>% round %>% viridisLite::inferno(.)
# cls<-nl$my %>% {.*10} %>% round %>% {clr[.-min(.)+1]}
# plot(
#   cln
#   ,layout=lyt
#   ,vertex.size=V(cln)$nw %>% lintran(c(1,15))
#   ,edge.arrow.size=0
#   ,vertex.label.cex=.5
#   ,vertex.color=cls
#   #  ,vertex.label.color='lightgray'
#   ,edge.color=E(cln)$nw %>% lintran(c(.1,.5)) %>% sapply(gray,level=0)
#   ,edge.curved=F
# )
#
# # high level clusters color coding
# hlc<-hlm[V(cln)$name,on='m'][,h]
# hlc[is.na(hlc)]<-'lightgray'
# plot(
#   cln
#   ,layout=lyt
#   ,vertex.size=V(cln)$nw %>% lintran(c(1,15))
#   ,edge.arrow.size=0
#   ,vertex.label.cex=.5
#   ,vertex.color=hlc
#   #  ,vertex.label.color='lightgray'
#   ,edge.color=E(cln)$nw %>% lintran(c(.1,.5)) %>% sapply(gray,level=0)
#   ,edge.curved=F
# )
#
# load('Out/latlay.RData')
# pdf('figures/latent.pdf',h=8.5,w=11)
# plot(
#   cln
#   ,layout=latlay
#   ,vertex.size=V(cln)$nw %>% lintran(c(1,8))
#   ,edge.arrow.size=0
#   ,vertex.label.cex=.2
#   ,vertex.frame.color=NA
#   # ,vertex.label.color='gray'
#   ,vertex.color=hlc
#   ,edge.width=.5
#   #  ,vertex.label.color='lightgray'
#   ,edge.color=E(cln)$nw %>% lintran(c(0,.5)) %>% sapply(gray,level=0)
#   ,edge.curved=F
# )
# dev.off()


# by year
#cp<-tilit::ov2chpt.f(top %>% head(50) %>% cumsum,min.period = 2,drv = 1)
{
	source('https://raw.githubusercontent.com/brooksambrose/pack-dev/master/tilit/R/ov2chpt.f.R')
	source('https://raw.githubusercontent.com/brooksambrose/pack-dev/master/tilit/R/lintran.R')
	load('Out/nts.RData')
  load('Out/wts.RData')
  load('Out/hlm.RData')
  setkey(wts,m)
  tab<-list()
  thr<-list()
  net<-list()
  pdf('figures/decades-cutoff.pdf',h=8.5,w=11)
  for(i in names(nts)){
    cat(i,'')
    yr<-i %>% strsplit('-') %>% unlist %>% as.integer
    w<-wts[between(py,yr[1],yr[2])]
    id<-V(nts[[i]])$name
    tab[[i]]<-data.table(
      id=id
      ,within=V(nts[[i]])$size
      ,without=sapply(id,function(x) E(nts[[i]])$weight[E(nts[[i]]) %>% unclass %>% attr('vnames') %>% grep(x,.)] %>% sum)
      ,nrefs=sapply(id,function(x) w[x,cr %>% unique %>% length]) # articles (citing)
      ,narts=sapply(id,function(x) w[x,id %>% unique %>% length]) # articles (citing)
    ) %>% setkey(id)

    if(gorder(nts[[i]])>50) {
      # keep<-w[,.N,by=m] %>% setorder(-N) %>% .[1:min(50,.N),m]
      # tnet<-induced_subgraph(nts[[i]],vids = intersect(keep %>% as.character,V(nts[[i]])$name))

      x<-E(nts[[i]])$weight %>% round %>%  table %>% ov2chpt.f(drv = 1,inc.ov = T) %>% setnames('.','edge.weight')
      thr[[i]]<-x
      x[,plot(x=edge.weight,y=N,col=g,type='b',main=i)]
      if(x[,length(levels(g))]>1) {
      	tw<-x[g=='2'][1,edge.weight]
      	tnet<-subgraph.edges(nts[[i]], which(E(nts[[i]])$weight>=tw), delete.vertices = TRUE)
      	while(!gorder(tnet)) {
      		tw<-tw-1
      		tnet<-subgraph.edges(nts[[i]], which(E(nts[[i]])$weight>=tw), delete.vertices = TRUE)
      	}
      } else {
      	tnet<-nts[[i]]
      }
    } else {
      tnet<-nts[[i]]
    }
    lyt<-layout_with_fr(tnet,weights = E(tnet)$weight)
    hlc<-hlm[V(tnet)$name,on='m',h]
    hlc[is.na(hlc)]<-'lightgray'
    V(tnet)$group<-hlc
    net[[i]]<-tnet
    plot(
      tnet
      ,main=paste0(i,'s')
      ,layout=lyt
      ,vertex.size=V(tnet)$size %>% lintran(c(1,8))
      ,edge.arrow.size=0
      ,vertex.label.cex=.2
      ,vertex.frame.color=NA
      # ,vertex.label.color='gray'
      ,vertex.color=V(tnet)$group
      ,edge.width=.5
      # ,vertex.label.color='lightgray'
      ,edge.color=if(gorder(tnet)>2) E(tnet)$weight %>% lintran(c(.1,.9)) %>% sapply(gray,level=0) else 'gray'
      ,edge.curved=.1
    )
  }
  dev.off()

  save(tab,file='Out/tab.RData')
  save(net,file='Out/net.RData')
}
# Descriptives

load('Out/tab.RData')
#tab[['period']]['cluster id']
names(tab) #periods

tab[["1950-1954"]]['OffRehab']

# edge.distribution -- all edge weights corresponding to 1 in the 'g' column are dropped
thr[['2010-2014']]

# interactive network
#install.packages('networkD3')
library(networkD3)
load('Out/net.RData')
inet<-lapply(net,function(x) {
	n<-networkD3::igraph_to_networkD3(x,group = V(x)$group)
	n$nodes$size<-V(x)$size
	n$links$width<-1
	n
	})

col2hex <- function(cname){colMat <- col2rgb(cname);rgb(red=colMat[1,]/255,green=colMat[2,]/255,blue=colMat[3,]/255)}
period<-names(inet["2015-2019"
])
forceNetwork(
	Links = inet[[period]]$links
	,Nodes = inet[[period]]$nodes
	,colourScale = inet[[period]]$nodes %>% .$group %>% levels %>% {paste0('d3.scaleOrdinal().domain(["',paste0(.,collapse='","'),'"]).range(["',paste0(col2hex(.),collapse='","'),'"])',collapse='')} %>% JS
	,Source = 'source'
	,Target = 'target'
	,Value = 'width'
	,Group = 'group'
	,NodeID = 'name'
	,Nodesize = 'size'
	,radiusCalculation = JS(" Math.sqrt(d.nodesize/Math.PI)+3")
	,zoom = T
	,legend=F
	,opacity=1
	,opacityNoHover = .2
	,linkColour = colorRampPalette(c('lightgray','black'))(100)[round(lintran(inet[[period]]$links$value,c(1,100)))]
	,linkDistance = 15 # sets the length of each edge
	,charge=-50 #the more negative the number, the more each node repels one another
	,bounded = F
	,fontSize = 18
	# ,height = 600
	# ,width = 800
) # %>% htmlwidgets::onRender("function(el,x) { d3.selectAll('.node').on('mouseover', null); }")

# can't stop annoying popping effect on mouseover
# https://github.com/christophergandrud/networkD3/blob/9c0a9c9ff32c53212d2d43e0ae3cc664137315e6/inst/htmlwidgets/forceNetwork.js#L232

