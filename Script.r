library(magrittr)
library(data.table)
library(ggplot2)
library(skmeans)
library(slam)
library(ggnetwork)
library(network)
library(sna)
library(directlabels)
source("Importer.r")
dat<-wok2dbl.f(dir = 'WoS.TextFiles.19Sept2018.61551/', out = 'Out')
dat<-wok2dbl
dat[,o:=NULL] #if we ever need to remember first author, we would need this back
head(dat[field=="CR"], 50)
setkey(dat,field,id) # key the database on the field to let me query things quickly by field


# Data Audit --------------------------------------------------------------
dat['UT'][duplicated(val),val] %>% cat(sep='\n')
dat<-dat[!duplicated(dat)]      #deletes duplicates

# Descriptives ------------------------------------------------------------

# average page numbers and number of works cited by year

  des<-dcast(data = dat[c("PY", "PG", "NR")]
             ,formula = id~field
             ,value.var = "val",
             ,fun.aggregate = function(x) x[1]
  )[,`:=`(
               NR=as.numeric(NR)
               ,PG=as.numeric(PG)
               ,PY=as.numeric(PY)
             )]
  
  des<-des[,.(References=mean(NR),Pages=mean(PG)),by=PY]
  
ggplot(data=melt(des,id.vars = "PY")) + geom_line(aes(x=PY,y=value,color=variable))
pages<- ggplot(data=melt(des,id.vars = "PY")) + geom_smooth(aes(x=PY,y=value,color=variable), se=F) + 
        geom_point(aes(x=PY,y=value,color=variable)) +
        scale_x_continuous(breaks=seq(1910,2018,10)) +
        ylab('Frequency') + xlab('Year') +
        theme_bw()
direct.label(pages, list("top.bumptwice"))
  
ggsave("citespages.png")


# convert to edgelist ---------------------------------------------------
# scan titles of articles that are dropped due to not having any citations
setkey(dat,field)
# we want to drop all isolates
el<-dat['CR',!'field'] # el is short for edgelist
keep<-el[,table(val)]

keep<-keep[keep>1] # keeps texts that have been cited more than once 

# sniff test for highly cited items
keep[keep>500] %>% sort(decreasing=T) #displays the number of times the top cited articles were cited
prop.table(keep)[keep>500] %>% `*`(100) %>% sort(decreasing=T) #displays the proportion of times, 
                                                               # relative to the whole dataset, the top-cited texts were cited 


setkey(el,val)
el<-el[names(keep)] #el is now the 1.3m edges in the dataset 
el$id %>% unique %>% length # number of citing articles remaining
el$val %>% unique %>% length #number of cited articles remaining

el[,val:=factor(val)]   # convert CR into a factor
el[,id:=factor(id)]    # convert citing article into a factor

sm<-el[,simple_triplet_matrix(as.numeric(val),as.numeric(id),rep(1,nrow(el)))] # creates a sparse matrix

system.time(km10<-skmeans(sm,10)) #create 10 clusters
system.time(km35<-skmeans(sm,35)) #create 35 clusters 
system.time(km25<-skmeans(sm,25)) #create 25 clusters
system.time(km50<-skmeans(sm,50)) #create 50 clusters
system.time(km100<-skmeans(sm,100)) #create 100 clusters

km<-km10
m<-merge(dat['CR',.(id,cr=val)],dat['PY',.(id,py=as.integer(val))]) %>% merge(data.table(cr=el$val %>% levels,m=km$cluster),by = 'cr')

m[,jc:=NA_character_]
m[grepl('^Mannheim',cr,ignore.case = T),jc:='Pre-War']
m[grepl('^Grunhut',cr,ignore.case = T),jc:='Pre-War']
m[grepl('^Radzinowicz',cr,ignore.case = T),jc:='Pre-War']
m[grepl('^Sutherland',cr,ignore.case = T),jc:='Sutherland']
m[grepl('^Glueck',cr,ignore.case = T),jc:='Glueck']
m[grepl('^Rusche',cr,ignore.case = T),jc:='Marx']
m[grepl('^Kirchheimer',cr,ignore.case = T),jc:='Marx']
m[grepl('^Marx',cr,ignore.case = T),jc:='Marx']
m[grepl('^Schwendinger',cr,ignore.case = T),jc:='Neo-Marxists']
m[grepl('^Platt',cr,ignore.case = T),jc:='Neo-Marxists']
m[grepl('^Melossi',cr,ignore.case = T),jc:='Neo-Marxists']
m[grepl('^Pavarini',cr,ignore.case = T),jc:='Neo-Marxists']
m[grepl('^Quinney',cr,ignore.case = T),jc:='Neo-Marxists']
m[grepl('^Foucault',cr,ignore.case = T),jc:='Foucault']

jct<-m[,table(py,jc)]
jct

denom <- m[,.N,by=.(py)]
setkey(m,py)
setkey(denom,py)
m<-merge(m,denom,by='py')
setnames(m,'N','denom')
ggplot(m[!is.na(jc),.N,by=.(jc,py)],aes(x=py,y=N,color=jc)) + geom_smooth()
ggplot(m[!is.na(jc),.(p=.N/denom[1]),by=.(jc,py)],aes(x=py,y=p,color=jc)) + 
  geom_smooth() + geom_point() + 
  scale_y_continuous(trans='log10') +
  theme_bw()
m[,table(jc,m)]

# network clustering solution 898 clusters
nm<-data.table(m=cgr$memberships[1,],cr=cgr$names)

m<-merge(m,nm,by = 'cr')
m[!is.na(jc),.N,by=.(m.y,jc)]

ggplot(m[!is.na(jc),.N,by=.(py,m.y,jc)],aes(x=py,y=N,color=jc)) + geom_line()
s<-m[,.N,by=m.y][N>100000,m.y]
#s<-m[,.N,by=m.y][between(N,lower,upper),m.y] 

ggplot(m[m.y%in%s,.N,by=.(py,m.y)],aes(x=py,y=N,color=factor(m.y))) + geom_line()

ggplot(m[m.y%in%s][!is.na(jc),.(p=.N/denom[1]),by=.(jc,py,m)],aes(x=py,y=p,color=jc)) + geom_smooth() + geom_point() + facet_wrap(~m)

# view hierarchical clusters
ctr<-apply(cgr$memberships,1,function(x) x %>% table %>% sort(decreasing = T))
View(ctr)
top<-ctr[[1]] %>% prop.table %>% `*`(100) %>% round(3) 
top %>% head(30)

#cp<-tilit::ov2chpt.f(top %>% head(50) %>% cumsum,min.period = 2,drv = 1)
#top %>% head(50) %>% cumsum %>% as.vector %>%  plot(col=cp$g)

# authors
auth<-m[,.(m.y,name=sub(',.+','',cr) %>% toupper)][,.N,by=.(m.y,name)]
setkey(auth,m.y,N)
auth<-auth[,.(N=tail(N,5),name=tail(name,5)),by=m.y]
setkey(auth,m.y)
s<-m[,.N,by=m.y][N>100000,m.y]
auth[.(s)] # lists top five authors from within each of the top five clusters
