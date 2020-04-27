library(data.table)
library(magrittr)
library(igraph)
wf<-fread('workflow.txt',sep = ':')
'[^\'\"]+[\'\"]([^ \'\"]+\\.(RData|pdf)).+' %>% {
  wf[grepl(.,code),out:=sub(.,'\\1',code)][,out:=sub('.+/(.+)','\\1',out)]
}
wf<-wf[!is.na(out)]
wf[grep('save',code),ls:='save'][grep('load',code),ls:='load'][grep('pdf',out),ls:='plot']
wf[,ls:=factor(ls,levels=c('load','save','plot'))]
wf<-wf[!grepl('Rhistory',file)]
wf<-wf[!grepl('^[ \\t]*#',code)]
setorder(wf,file,line)
wf[,sub:=cumsum(ls=='load'),by=file]
el<-wf[,.(s=out[1],r=out[-1]),by=.(file,sub)][!is.na(r),.(s,r,f=basename(file))]
vn<-el[,c(s,r) %>% unique] %>% {c(.,setdiff(wf[,unique(out)],.))}
g<-graph_from_data_frame(el[,.(s,r,f)],vertices = data.table(name=vn))

(72*1.5) %>% {pdf('workflow.pdf',height=900/.,width=1440/.)}
plot.igraph(
  g
  ,layout=data.table(layout_with_sugiyama(g)$layout) %>% setnames(c('x','y')) %>% .[,cbind(-.5*y,-1.5*x)]
  ,vertex.label.cex=1
  ,vertex.shape='crectangle'
  ,vertex.size=30
  ,vertex.size2=8
  ,vertex.color='yellow'
  ,vertex.frame.color=NA
  ,edge.label=E(g)$f
  ,edge.label.cex=.75
  ,edge.arrow.size=.25
  ,edge.arrow.width=.5
  ,asp=900/1440
  ,frame=F
  ,margin=-.2
  )
dev.off()
