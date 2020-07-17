
# use google compute engine GCE region cluster us-west2-c
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh

# make docker group
sudo groupadd docker
sudo usermod -aG docker $USER
# log out and back in the first time

# install git
sudo apt install git htop

# install pip https://linuxize.com/post/how-to-install-pip-on-debian-9/
sudo apt install python3-pip

# https://repo2docker.readthedocs.io/en/latest/install.html
python3 -m pip install jupyter-repo2docker

# update PATH, which doesn't seem to work anyway
export PATH=$PATH:/home/bambrose/.local/bin

# build image, hopefully launch image
jupyter-repo2docker --image-name contours https://github.com/brooksambrose/contours

# clone repo to have it

# when can't connect over web, just go command line
cd ~/joh2bro
docker run -it --rm -v "$(pwd)":/home/rstudio/figures -w /home/rstudio/ -e UID=$UID contours bash

# inside container
R 

set.seed(12345)
#stop('Don\'t run me dummy')
#rm(list=ls()) # clear workspace
cat('\014') # clear console
knitr::opts_chunk$set(
  eval=T
  ,include=F
  ,paged.print=F
)
library(tilit);library(magrittr)
'data.table,disk.frame,ggplot2,igraph,network,intergraph' %>% ec %>% sapply(library,character.only = T) %>% invisible # load packages
#if(!dir.exists('d')) system('mkdir d && (cd figures && mkdir q d p b)') # create data directory structure if it doesn't exist

DF<-F

f<-'Out/wok2dbl.RData'
# create link to original location in clean data structure 
if(!file.exists(f)) system('ln -s Out/wok2dbl.RData d/p/wok2dbl.RData')
if(file.exists(f)){
  load(f)
} else {
  # Code that would generate database, but load saved data instead. Note original WOK records absent from repository.
  wok2dbl<-plagiat::wok2dbl.f(dir = 'WoS.TextFiles.19Sept2018.61551/', out = 'Out')
  save(wok2dbl,file=f)
}
rm(f)
wok2dbl<-unique(wok2dbl)
if(DF) {wok2dfl<-as.disk.frame(wok2dbl);rm(wok2dbl)}

f<-'d/q/dbl2crl.txt.gz'
if(file.exists(f)){
  warning(sprintf('%s already exists. Manually delete to replace.',f))
} else {
  dbl2crl.f<-function(){
    crl<-data.table(cro={if(DF) srckeep(wok2dfl,ec('field,val')) else wok2dbl}['CR',on='field',val %>% unique])[,cr:=cro %>%  toupper %>% sub(', DOI.+','',.) %>% gsub('<[^>]+>','',.) %>% sub('^[ANONYMOUS]','',.,fixed=T)] %>% setcolorder(2:1) 
    # write original keyed to trimmed
    fwrite(x = crl[cr!=cro],file=sub('crl','cro',f),quote = F,sep = '\t')
    crl[,cro:=NULL]
    if(anyDuplicated(crl)) crl<-unique(crl)
    # write just levels
    fwrite(x = crl,file=f,quote = F,sep = '\t')
  }
  dbl2crl.f()
}
rm(f)

f<-'figures/q/crl2reg.txt.gz'
if(file.exists(f)){
  crl2reg<-fread(f)
} else {
  crl2reg.f<-function(thr=.1,cores=parallel::detectCores()/2){
    crl<-fread(sub('crl2reg','dbl2crl',f),sep='\n')
    crl2reg<-try(fread(f))
    if(inherits(crl2reg,'try-error')) {minit<-list();ix<-0} else {
      ix<-try(crl[,which(cr==crl2reg[.N,cr])])
      if(inherits(ix,'try-error')) {minit<-list();ix<-0} else {minit<-crl2reg[,as.list(cr)]}
    }
  ix<-ix+1
    crl[,{
      m<-minit
      pbapply::pblapply(ix:.N,function(i) if(!cr[i]%in%m) {
        r<<-cr[-i][stringdist::amatch(x = cr[i],table = cr[-i],method = 'jw',maxDist = thr,nomatch = NA_character_)]
        if(!is.na(r)) {data.table(cr=cr[i],cra=r) %>% fwrite(f,append = T,quote=F,sep='\t');m[[length(m)+1]]<<-r}
      },cl = cores)}]
    fread(f)
    
  }
  crl2reg<-crl2reg.f(cores=6)
}
rm(f)