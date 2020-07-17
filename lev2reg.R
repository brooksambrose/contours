library(magrittr)
library(data.table)
library(pbapply)
pboptions(type='timer')
thr<-.01
f<-'d/q/lev2reg.txt.gz'
  f %>% unlink
  lev2reg.f<-function(){
    lev<-fread('d/q/dbl2crm-lev.txt.gz',sep='\n')
    lev[,{
      m<-list()
      pbapply::pblapply(1:.N,function(i) if(!cr[i]%in%m) {
        r<<-cr[-i][stringdist::amatch(x = cr[i],table = cr[-i],method = 'jw',maxDist = thr,nomatch = NA_character_)]
        if(!is.na(r)) {data.table(cr=cr[i],adst=r) %>% fwrite(f,append = T,quote=F,sep='\t');m[[length(m)+1]]<<-r}
      },cl = 3)}]
  }
lev2reg.f()
