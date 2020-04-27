library(data.table)
library(magrittr)
library(crimCV)

load('Out/wts.RData')
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html
# membership by year
wts %>% setorder(py,m)
mby<-dcast(data = wts[,.N,by=.(m,py)],formula = m~py,value.var = 'N',fun.aggregate = sum,fill = 0)
save(mby,file='Out/mby.RData')
mhby<-dcast(data = wts[,.N,by=.(mh,py)],formula = mh~py,value.var = 'N',fun.aggregate = sum,fill = 0)
save(mhby,file='Out/mhby.RData')

set.seed(12345)
system.time(gtm<-crimCV(Dat = mby[sample(1:.N,100),!'m'] %>% as.matrix,ng = 5,init=5)) # mby is low level, change to mhby for high level
save(gtm,file='Out/gtm.RData')

# visualize
load('Out/gtm.RData')
plot(gtm)
summary(gtm)

# normalize
load('Out/mby.RData')
nmby<-mby[,!'m'] %>% apply(1,function(x) round(prop.table(x)*100)) %>% t
set.seed(12345)
#system.time(ngtm<-crimCV(Dat = nmby[sample(1:nrow(nmby),100),],ng = 5,init=5))
system.time(ngtm12<-crimCV(Dat = nmby,ng = 12,init=5,rcv=TRUE)) #play around with the ng number
save(ngtm12,file='Out/ngtm.RData')

# visualize
load('Out/ngtm.RData')
plot(ngtm)
summary(ngtm)
ngtm$prob

# clusters by trajectory group
grp<-summary(ngtm) %>% apply(2,function(x) mby[which(as.logical(x)),as.character(m)])

# code they use to make the plot
View(crimCV:::plot.dmZIPt)

# colors
palette()

ngtm2plot<-function(ngtm,nmby){
  require(ggplot2)
  require(data.table)
  require(magrittr)
  
  year<-colnames(nmby) %>% as.integer
  all<-year %>% range %>% {.[1]:.[2]}
  gap<- all %>% {setdiff(.,year)}
  
  Xb <- ngtm$X %*% ngtm$beta
  xb<-matrix(NA,ncol=ncol(Xb),nrow=length(year)+length(gap))
  
  {c<-0;for(i in which(all %in% gap %>% `!`)){
    c<-c+1
    xb[i,]<-Xb[c,]
  }}
  Xb<-xb
  
  lambda <- exp(Xb)
  p <- exp(-ngtm$tau * t(Xb))
  p <- t(p)
  p <- p/(1 + p)
  mu <- data.table( (1 - p) * lambda)
  mu[,year:=all]

  mu<-melt(mu,'year',variable.name = 'Group',value.name='mu')
  mu[,Group:=factor(Group) %>% as.integer %>% factor]
  levels(mu$Group) %<>% paste0(' (',round(ngtm$prob * 100,1),'%)')
  p<-ggplot(mu[year>1900],aes(x=year,y=mu,color=Group)) + geom_line() + #shifts start date
     scale_x_continuous(breaks=seq(1910,2018,10)) +
    ylab('Citation Share') + xlab('Year') +
    theme_bw() #+ scale_color_brewer(palette = 'Dark2')
  p
  list(p=p,dat=mu)
}

p1<-ngtm2plot(ngtm,nmby) #change the ngtm object for each iteration with different group numbers. this line requires first running line 45!

# identify biggest groups at the peak of each trajectory
p1$p
pks<-p1$dat[,.(peak=year[which.max(mu)]),by=Group]
grp

top<-pks[,
  .(top=list(mby[grp[[Group %>% as.integer]],on='m',.SD,.SDcols=c('m',as.character(peak))] %>% setnames(c('m','peak')) %>% setorder(-peak) %>% head(10)))
,by=Group] %>% .$top
names(top)<-paste(pks$Group,pks$peak)
top
