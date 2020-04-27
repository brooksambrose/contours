library(data.table)
library(magrittr)
library(crimCV)
library(cluster)
library(ggplot2)
library(dplyr)
library(gghighlight)
library(tidyr)
library(devtools)
library(ggrepel)
library(directlabels)
library(factoextra)
library(ggthemes)

load('Out/mhby.RData')
ktraj<-function(mby,normalize=F,k=seq(5,50,5)){
  library(data.table)
  library(magrittr)
  library(cluster)
  if(normalize) {
    nmby<-mby[,!'m'] %>% apply(1,function(x) round(prop.table(x)*100)) %>% t
  } else {
    nmby<-mby[,!'m']
  }
  dmby<-dist(nmby)
  names(k)<-paste0('k',k)
  kmc<-lapply(k,function(x) {
    t<-system.time(x<-kmeans(x=nmby,centers=x,iter.max =50))
    print(t)
    x
  })
  sil<-lapply(kmc,function(x) {
    t<-system.time(x<-silhouette(x$cluster,dist=dmby))
    print(t)
    x[,"sil_width"] %>% mean
  })
  plot(x = k,y=sil,type='b')
  kmc
}

kmc<-ktraj(mby,normalize = F,k = seq(2,40,2))
save(kmc,file='Out/kmc.RData')
nkmc<-ktraj(mby,normalize = T,k = seq(2,40,2))

pdf('figures/sillhouette.pdf')
kmhc<-ktraj(mhby,normalize = F,k = seq(2,40,2))
dev.off()
save(kmhc,file='Out/kmhc.RData')

# pick a solution
load('Out/mhby.RData')
load('Out/kmhc.RData')
pick<-'k32'

# these are the sizes of the trajectory groups
#kmd<-data.table(m=mby$m,mby[,!'m'],kmc=factor(kmc[[pick]]$cluster)) %>% melt(id.vars=c('m','kmc'),variable.name='year',value.name='count')
kmd2<-data.table(m=mhby$mh,mhby[,!'mh'],kmc=factor(kmhc[[pick]]$cluster)) %>% melt(id.vars=c('m','kmc'),variable.name='year',value.name='count')
kmd2[,year:=year %>% as.character %>% as.integer]
kmd2<-kmd2[,.(count=sum(count)),by=.(kmc,year)]

kmd2[,kmc:=factor(kmc) %>% as.integer %>% factor]
levels(kmd2$kmc) %<>% paste0(' (',round(prop.table(kmhc[[pick]]$cluster %>% table) * 100,1),'%)')


ggplot(kmd2[between(year,1910,2017)],aes(x=year,y=count,color=kmc)) + 
  geom_point(show.legend = F, size=0.5) + 
  #geom_line(show.legend = F) +
  geom_smooth(se=F) + 
  facet_wrap(~kmc,scales = 'free_y') +
  ylab('Citation Count') + xlab('Year') +
  scale_y_continuous(trans='log10') +
  scale_x_continuous(breaks=seq(1910,2018,10)) +
  theme_bw()
ggsave(filename = 'figures/high-level-raw.pdf')

ggplot({kmd %>% setorder(year) %>% .[,.(year=year[-1],count=diff(count)),by=kmc]}[
  between(year,1910,2017)],aes(x=year,y=count,color=kmc)) + 
  geom_point(show.legend = F, size = 0.5) + 
  #geom_line(show.legend = F) +
    geom_smooth(se=F, show.legend = F) + 
  facet_wrap(~kmc,scales = 'free_y') +
  ylab('Citation Count') + xlab('Year') +
  theme_bw()
ggsave(filename = 'figures/high-level-first-difference.pdf')


kmc[[pick]]$cluster %>% table %>% cbind

# logged facet panel
try <- kmd2
try2 <- try



try$trjgp[try$kmc=='11 (0.1%)'] <-'a) High'
try$trjgp[try$kmc=='8 (0.2%)'] <-'a) High'
try$trjgp[try$kmc=='14 (0.3%)'] <-'a) High'
try$trjgp[try$kmc=='12 (7.9%)'] <-'d) High-to-Low'
try$trjgp[try$kmc=='1 (7.6%)'] <-'b) Mid'
try$trjgp[try$kmc=='3 (7.2%)'] <-'b) Mid'
try$trjgp[try$kmc=='4 (7.1%)'] <-'b) Mid'
try$trjgp[try$kmc=='10 (7.9%)'] <-'e) Mid-to-Low (Early)'
try$trjgp[try$kmc=='13 (7.6%)'] <-'e) Mid-to-Low (Early)'
try$trjgp[try$kmc=='6 (7.9%)'] <-'e) Mid-to-Low (Early)'
try$trjgp[try$kmc=='7 (7.9%)'] <-'e) Mid-to-Low (Early)'
try$trjgp[try$kmc=='15 (7.8%)'] <-'e) Mid-to-Low (Early)'
try$trjgp[try$kmc=='5 (7.6%)'] <-'f) Mid-to-Low (Late)'
try$trjgp[try$kmc=='16 (7.7%)'] <-'f) Mid-to-Low (Late)'
try$trjgp[try$kmc=='9 (7.7%)'] <-'c) Low'
try$trjgp[try$kmc=='2 (7.7%)'] <-'c) Low'

try2$trjgp2[try$kmc=='11 (0.1%)'] <-'a) Stable'
try2$trjgp2[try$kmc=='8 (0.2%)'] <-'a) Stable'
try2$trjgp2[try$kmc=='14 (0.3%)'] <-'a) Stable'
try2$trjgp2[try$kmc=='12 (7.9%)'] <-'b) Descending'
try2$trjgp2[try$kmc=='1 (7.6%)'] <-'a) Stable'
try2$trjgp2[try$kmc=='3 (7.2%)'] <-'a) Stable'
try2$trjgp2[try$kmc=='4 (7.1%)'] <-'a) Stable'
try2$trjgp2[try$kmc=='10 (7.9%)'] <-'b) Descending'
try2$trjgp2[try$kmc=='13 (7.6%)'] <-'b) Descending'
try2$trjgp2[try$kmc=='6 (7.9%)'] <-'b) Descending'
try2$trjgp2[try$kmc=='7 (7.9%)'] <-'b) Descending'
try2$trjgp2[try$kmc=='15 (7.8%)'] <-'b) Descending'
try2$trjgp2[try$kmc=='5 (7.6%)'] <-'b) Descending'
try2$trjgp2[try$kmc=='16 (7.7%)'] <-'b) Descending'
try2$trjgp2[try$kmc=='9 (7.7%)'] <-'a) Stable'
try2$trjgp2[try$kmc=='2 (7.7%)'] <-'a) Stable'

try4$trjgp3[try3$kmc=='14 (0.2%)'] <-'a) Stable'
try4$trjgp3[try3$kmc=='18 (0.2%)'] <-'a) Stable'
try4$trjgp3[try3$kmc=='27 (0.1%)'] <-'a) Stable'
try4$trjgp3[try3$kmc=='32 (0.2%)'] <-'a) Stable'
try4$trjgp3[try3$kmc=='31 (3.5%)'] <-'b) Descending'
try4$trjgp3[try3$kmc=='30 (3.8%)'] <-'a) Stable'
try4$trjgp3[try3$kmc=='29 (4.2%)'] <-'b) Descending'
try4$trjgp3[try3$kmc=='28 (3.9%)'] <-'a) Stable'
try4$trjgp3[try3$kmc=='26 (3.5%)'] <-'b) Descending'
try4$trjgp3[try3$kmc=='25 (4%)'] <-'b) Descending'
try4$trjgp3[try3$kmc=='24 (3.4%)'] <-'a) Stable'
try4$trjgp3[try3$kmc=='23 (3.5%)'] <-'b) Descending'
try4$trjgp3[try3$kmc=='22 (3.4%)'] <-'a) Stable'
try4$trjgp3[try3$kmc=='21 (4.3%)'] <-'b) Descending'
try4$trjgp3[try3$kmc=='20 (4.3%)'] <-'b) Descending'
try4$trjgp3[try3$kmc=='19 (3.4%)'] <-'b) Descending'
try4$trjgp3[try3$kmc=='17 (2.8%)'] <-'b) Descending'
try4$trjgp3[try3$kmc=='16 (4.2%)'] <-'a) Stable'
try4$trjgp3[try3$kmc=='15 (3.4%)'] <-'b) Descending'
try4$trjgp3[try3$kmc=='13 (3.4%)'] <-'b) Descending'
try4$trjgp3[try3$kmc=='12 (3.8%)'] <-'a) Stable'
try4$trjgp3[try3$kmc=='11 (3.4%)'] <-'a) Stable'
try4$trjgp3[try3$kmc=='10 (3.4%)'] <-'b) Descending'
try4$trjgp3[try3$kmc=='9 (3.6%)'] <-'a) Stable'
try4$trjgp3[try3$kmc=='8 (0.2%)'] <-'a) Stable'
try4$trjgp3[try3$kmc=='7 (3.5%)'] <-'b) Descending'
try4$trjgp3[try3$kmc=='6 (3.8%)'] <-'b) Descending'
try4$trjgp3[try3$kmc=='5 (3.9%)'] <-'a) Stable'
try4$trjgp3[try3$kmc=='4 (3.4%)'] <-'b) Descending'
try4$trjgp3[try3$kmc=='3 (4.2%)'] <-'b) Descending'
try4$trjgp3[try3$kmc=='2 (3.8%)'] <-'a) Stable'
try4$trjgp3[try3$kmc=='1 (3.5%)'] <-'b) Descending'



grpd <- ggplot(try) + facet_wrap(~trjgp, scales="free_x") +
  geom_vline(xintercept=1965, linetype="dashed", size=0.2) +
  geom_vline(xintercept=1995, linetype="dashed", size=0.2) +
  geom_smooth(aes(year,count,group=kmc), data=kmd, colour=alpha("grey",0.2), se=F) +
  geom_point(aes(year,count,group=kmc),data=kmd, colour="grey", size =0.05) +
  geom_smooth(aes(year,count,colour=kmc),data=try, se=F) + 
  geom_jitter(aes(year,count,colour=kmc),data=try, size =0.5, height=0.1) +
  ylab('Citation Count') + xlab('Year') +
  scale_y_continuous(trans='log10') +
  scale_x_continuous(breaks=seq(1910,2018,20)) +
  annotate("text", x = 1930, y = 50000, label = "Crystallization", size=2.5) +
  annotate("text", x = 1980, y = 50000, label = "Stratification", size=2.5) +
  annotate("text", x = 2010, y = 50000, label = "Institutionalization", size=2.5) +
  theme_bw()
direct.label(grpd, list("smart.grid", "draw.rects"))

grpd <- ggplot(try2) + facet_wrap(~trjgp2, scales="free_x") +
  geom_vline(xintercept=1965, linetype="dashed", size=0.2) +
  geom_vline(xintercept=1995, linetype="dashed", size=0.2) +
  geom_smooth(aes(year,count,group=kmc), data=kmd, colour=alpha("grey",0.2), se=F) +
  geom_point(aes(year,count,group=kmc),data=kmd, colour="grey", size =0.05) +
  geom_smooth(aes(year,count,colour=kmc),data=try2, se=F) + 
  geom_jitter(aes(year,count,colour=kmc),data=try2, size =0.5, height=0.1) +
  ylab('Citation Count') + xlab('Year') +
  scale_y_continuous(trans='log10') +
  scale_x_continuous(breaks=seq(1910,2029,20)) +
  annotate("text", x = 1930, y = 50000, label = "Crystallization", size=3.5) +
  annotate("text", x = 1980, y = 50000, label = "Stratification", size=3.5) +
  annotate("text", x = 2010, y = 50000, label = "Institutionalization", size=3.5) +
  theme_bw() +
  theme(legend.position="none") 
  # scale_color_manual(values = c("#D55E00", # Trajectory 1
  #                              "#F0E442", # Trajectory 2
  #                              "#D55E00", # Trajectory 3
  #                              "#D55E00", # Trajectory 4
  #                              "#009E73", # Trajectory 5    6
  #                              "#CC79A7", # Trajectory 6
  #                              "#CC79A7", # Trajectory 7
  #                              "#56B4E9", # Trajectory 8
  #                              "#F0E442", # Trajectory 9
  #                              "#CC79A7", # Trajectory 10
  #                              "#56B4E9", # Trajectory 11
  #                              "#E69F00", # Trajectory 12
  #                              "#CC79A7", # Trajectory 13
  #                              "#56B4E9", # Trajectory 14
  #                              "#CC79A7", # Trajectory 15
  #                              "#009E73")) # Trajectory 16   6

direct.label(grpd, list("last.polygons"))

grpd <- ggplot(try4) + facet_wrap(~trjgp3, scales="free_x") +
  geom_vline(xintercept=1965, linetype="dashed", size=0.2) +
  geom_vline(xintercept=1995, linetype="dashed", size=0.2) +
  geom_smooth(aes(year,count,group=kmc), data=kmd2, colour=alpha("grey",0.2), se=F) +
  geom_point(aes(year,count,group=kmc),data=kmd2, colour="grey", size =0.05) +
  geom_smooth(aes(year,count,colour=kmc),data=try4, se=F) + 
  geom_jitter(aes(year,count,colour=kmc),data=try4, size =0.5, height=0.1) +
  ylab('Citation Count') + xlab('Year') +
  scale_y_continuous(trans='log10') +
  scale_x_continuous(breaks=seq(1910,2029,20)) +
  annotate("text", x = 1930, y = 50000, label = "Crystallization", size=3.5) +
  annotate("text", x = 1980, y = 50000, label = "Stratification", size=3.5) +
  annotate("text", x = 2010, y = 50000, label = "Institutionalization", size=3.5) +
  theme_bw() +
  theme(legend.position="none") +
  scale_color_manual(values = c("#CC79A7",# Trajectory 1 x
                               "#D55E00", # Trajectory 2 x
                               "#009E73", # Trajectory 3 x
                               "#CC79A7", # Trajectory 4 x
                               "#F0E442", # Trajectory 5 x  
                               "#CC79A7", # Trajectory 6 x
                               "#CC79A7", # Trajectory 7 x
                               "#D55E00", # Trajectory 8 x
                               "#D55E00", # Trajectory 9 x
                               "#CC79A7", # Trajectory 10 x
                               "#F0E442", # Trajectory 11 x
                               "#F0E442", # Trajectory 12 x
                               "#CC79A7", # Trajectory 13 x
                               "#56B4E9", # Trajectory 14 x
                               "#CC79A7", # Trajectory 15 x
                               "#F0E442", # Trajectory 16 x
                               "#E69F00", # Trajectory 17 x
                               "#56B4E9", # Trajectory 18 x
                               "#CC79A7", # Trajectory 19 x
                               "#CC79A7", # Trajectory 20 x  
                               "#CC79A7", # Trajectory 21 x
                               "#F0E442", # Trajectory 22 x
                               "#CC79A7", # Trajectory 23 x
                               "#F0E442", # Trajectory 24 x
                               "#CC79A7", # Trajectory 25 x
                               "#CC79A7", # Trajectory 26 x
                               "#56B4E9", # Trajectory 27 x
                               "#F0E442", # Trajectory 28 x
                               "#009E73", # Trajectory 29 x
                               "#F0E442", # Trajectory 30 x
                               "#CC79A7", # Trajectory 31 x
                               "#56B4E9")) # Trajectory 32 x  
