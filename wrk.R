load('Out/wok2dbl.RData')
load('Out/cgr_culled.RData') # omit _culled to see clustering with idiosyncratic ties
library(magrittr)
library(data.table)
library(ggplot2)

# top citations in each cluster -------------------------------------------

crm<-data.table(cr=cgr$names,m=cgr$memberships[1,],mh=cgr$memberships[nrow(cgr$memberships),]) # cr membership table
setkey(wok2dbl,field)
crm<-merge(wok2dbl['CR',.N,by=val],crm,by.x='val',by.y='cr') %>% setnames('val','cr') # membership table with citation counts!
crm[,
    m:=factor(m,levels = table(m) %>% sort(decreasing = T) %>% names)
    ][
      ,mh:=factor(mh,levels = table(mh) %>% sort(decreasing = T) %>% names)]
setorder(crm,-N)
crm[,.(cr=list(cr %>% head(6))),keyby=m #Read the top six articles within each cluster
    ][61:70][,cat('\nCluster:',m %>% as.character,cr %>% unlist,sep='\n'),by=m] #See the top clusters

# take time to label the clusters
levels(crm$m)[1]<-'OffRehab'
levels(crm$m)[2]<-'FamViol'
levels(crm$m)[3]<-'DSM'
levels(crm$m)[4]<-'AdminCrim'
levels(crm$m)[5]<-'OffTheor'
levels(crm$m)[6]<-'CritCrim'
levels(crm$m)[7]<-'Policing'
levels(crm$m)[8]<-'LifeCourse'
levels(crm$m)[9]<-'RxCrime'
levels(crm$m)[10]<-'Strain'
levels(crm$m)[11]<-'RxSentenDis'
levels(crm$m)[12]<-'PrisEthnog'
levels(crm$m)[13]<-'SxVictimzn'
levels(crm$m)[14]<-'ProcJust'
levels(crm$m)[15]<-'YthGang'
levels(crm$m)[16]<-'Desist'
levels(crm$m)[17]<-'RapeMyth'
levels(crm$m)[18]<-'ReintShame'
levels(crm$m)[19]<-'SxOffen'
levels(crm$m)[20]<-'SelfContr'
levels(crm$m)[21]<-'Deter'
levels(crm$m)[22]<-'BullyAggr'
levels(crm$m)[23]<-'COStress'
levels(crm$m)[24]<-'FearCrime'
levels(crm$m)[25]<-'PubOpin'
levels(crm$m)[26]<-'OrgnzdCri'
levels(crm$m)[27]<-'Aggressn'
levels(crm$m)[28]<-'Deceptn'
levels(crm$m)[29]<-'KidTestimon'
levels(crm$m)[30]<-'CopStress'
levels(crm$m)[31]<-'IntlHomic'
levels(crm$m)[32]<-'PrivPolic'
levels(crm$m)[33]<-'WhiteColl'
levels(crm$m)[34]<-'GreenCrim'
levels(crm$m)[35]<-'SxHomic'
levels(crm$m)[36]<-'EyeWitness'
levels(crm$m)[37]<-'SxRiskAss'
levels(crm$m)[38]<-'JuvCourt'
levels(crm$m)[39]<-'Stalking'
levels(crm$m)[40]<-'MigratnCrim'
levels(crm$m)[41]<-'TeenSxOff'
levels(crm$m)[42]<-'CogniDistor'
levels(crm$m)[43]<-'KidSxAbuse'
levels(crm$m)[44]<-'TrustInCops'
levels(crm$m)[45]<-'SxORegistry'
levels(crm$m)[46]<-'Profiling'
levels(crm$m)[47]<-'YouthPTSD'
levels(crm$m)[48]<-'PolicingDV'
levels(crm$m)[49]<-'AdolOff'
levels(crm$m)[50]<-'Interrogtn'
levels(crm$m)[51]<-'JuryDecis'
levels(crm$m)[52]<-'GxEnvInter'
levels(crm$m)[53]<-'SubstViol'
levels(crm$m)[54]<-'FamFX'
levels(crm$m)[55]<-'WildlTraff'
levels(crm$m)[56]<-'CopWomen'
levels(crm$m)[57]<-'ChildPorn'
levels(crm$m)[58]<-'CyberBully'
levels(crm$m)[59]<-'Terror'
levels(crm$m)[60]<-'Attachment'
levels(crm$m)[61]<-'GxEnvSusce'
levels(crm$m)[62]<-'Firesetting'
# levels(crm$m)[63]<-''

# rename by cluster id
# crm[,.(cr=list(cr %>% head(6))),keyby=m #Read the top six articles within each cluster
#     ][m=='old'][,cat('\nCluster:',m %>% as.character,cr %>% unlist,sep='\n'),by=m]
# levels(crm$m)[levels(crm$m)=='old']<-'new'
crm[,.(cr=list(cr %>% head(6))),keyby=m #Read the top six articles within each cluster
    ]['1478'][,cat('\nCluster:',m %>% as.character,cr %>% unlist,sep='\n'),by=m]
levels(crm$m)[levels(crm$m)=='2224']<-'ParRecPred'
levels(crm$m)[levels(crm$m)=='4559']<-'MMPI'
levels(crm$m)[levels(crm$m)=='809']<-'PrdxnVldxn'
levels(crm$m)[levels(crm$m)=='647']<-'JustDesert'
levels(crm$m)[levels(crm$m)=='2308']<-'DeliEpid'
levels(crm$m)[levels(crm$m)=='1994']<-'BioPersnlty'
levels(crm$m)[levels(crm$m)=='691']<-'PersMeas'
levels(crm$m)[levels(crm$m)=='6901']<-'TypolOff'
levels(crm$m)[levels(crm$m)=='1140']<-'DeliSubc'
levels(crm$m)[levels(crm$m)=='1141']<-'TypolCrim'
levels(crm$m)[levels(crm$m)=='5771']<-'TypolOcc'
levels(crm$m)[levels(crm$m)=='5228']<-'InterpStats'
levels(crm$m)[levels(crm$m)=='5889']<-'MethoRefin'
levels(crm$m)[levels(crm$m)=='1244']<-'ValidGlukSc'
levels(crm$m)[levels(crm$m)=='4735']<-'SubCulVals'
levels(crm$m)[levels(crm$m)=='2438']<-'PsyPathPers'
levels(crm$m)[levels(crm$m)=='4989']<-'ApplPredxn'
levels(crm$m)[levels(crm$m)=='2638']<-'CorrxnEffx'
levels(crm$m)[levels(crm$m)=='91']<-'ExcluRule'
levels(crm$m)[levels(crm$m)=='477']<-'ArrestJurisp'
levels(crm$m)[levels(crm$m)=='104']<-'DueProcLaw'
levels(crm$m)[levels(crm$m)=='115']<-'R2Counsel'
levels(crm$m)[levels(crm$m)=='5601']<-'DiscretnJuris'
levels(crm$m)[levels(crm$m)=='1142']<-'ConfesnJuris'
levels(crm$m)[levels(crm$m)=='535']<-'AdmissJuris'
levels(crm$m)[levels(crm$m)=='2580']<-'PenalCode'
levels(crm$m)[levels(crm$m)=='5514']<-'BailReform'
levels(crm$m)[levels(crm$m)=='3541']<-'XYYchromo'
levels(crm$m)[levels(crm$m)=='845']<-'XYYcrimlty'
levels(crm$m)[levels(crm$m)=='7928']<-'HomicPatter'
levels(crm$m)[levels(crm$m)=='79']<-'8ACapPun'
levels(crm$m)[levels(crm$m)=='70']<-'8ACrimnlzn'
levels(crm$m)[levels(crm$m)=='186']<-'JuvCtRights'
# levels(crm$m)[levels(crm$m)=='']<-''

# same labeling but at high level 
crm[,.(cr=list(cr %>% head(6))),keyby=mh #Read the top six articles within each cluster
    ]['1'][,cat('\nCluster:',mh %>% as.character,cr %>% unlist,sep='\n'),by=mh]
# levels(crm$mh)[levels(crm$mh)=='767']<-'New name for biggest supercluster'
# levels(crm$mh)[1]<-'New name for biggest supercluster'

levels(crm$mh) %>% head # names of top 6 high level clusters

save(crm,file='Out/crm.RData')


# break out by year -------------------------------------------------------

crm[,cy:=sapply(cr,function(x) sub(pattern='^.+, ([0-9]{4}), .+$',replacement='\\1',x) %>% as.integer)]
by<-merge( # by = by year
  wok2dbl['PY',.(id,py=val %>% as.integer)]
  ,wok2dbl['CR',.(id,cr=val)]
  ,by='id'
)
by<-merge(by,crm,by='cr')
save(by,file='Out/by.RData')

# recency bias (citation age) -------------------------------------------

by[,nic:=py-cy] # citation age
setkey(by,m)
agep<-ggplot(
  data = by[c('CritCrim','FamViol', 'OffRehab', 'Concentrn', 'OffTheor', 'DSM', 'Policing', 'LifeCourse', 'Deter', 'JuvCourt')][py>1960][nic>0]
  ,mapping = aes(x=py,y=nic,color=m)
) + geom_smooth(se=F)
agep 

spread<-by[!is.na(nic),.(
  `25%`=quantile(nic,.25)
  ,`50%`=quantile(nic,.5)
  , mean=mean(nic)
  ,`75%`=quantile(nic,.75)
),by=.(m,py)]
spread<-melt(spread,id.vars = c('m','py'),value.name = 'nic',variable.name = 'quant')
setkey(spread,m)
pspread<-ggplot(
  data = spread[c('CritCrim','FamViol', 'OffRehab', 'Concentrn', 'OffTheor', 'DSM', 'Policing', 'LifeCourse', 'Deter', 'JuvCourt')][py>1960][nic>0]
  ,mapping = aes(x=py,y=nic,color=m,linetype=quant)
) + geom_line() + facet_wrap(~m,ncol=1) # + geom_smooth()  #+ geom_point()
pspread





# discipline labels -------------------------------------------------------

# wok2dbl['SC',.N,by=val] # these are all the discipline labels that exist, might need to clean it up
# wok2dbl[val=='Women\'s Studies Criminology & Penology',val:='Women\'s Studies'] # example of recoding
# 
# merge(wok2dbl['CR',.(ut=list(id)),by=val],crm)
# 
# wok2dbl['CR']
# wok2dbl['SC']
