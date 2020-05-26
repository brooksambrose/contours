# lib<-path.expand('~/.local/R')
# dir.create(lib)
# Sys.setenv(R_LIBS=paste(lib,Sys.getenv('R_LIBS'),sep=':'),R_LIBS_USER=lib)
# .libPaths(Sys.getenv("R_LIBS_USER"))
if(!require(devtools)) {install.packages('devtools');library(devtools)}
#if(!require(kableExtra)) {install.packages('kableExtra')}
{devtools::install_github(
  'brooksambrose/pack-dev',subdir='tilit',dependencies=T
  #,lib=lib
);library(tilit)}
if(!require(magrittr)) {install.packages('magrittr');library(magrittr)}
tilit::ec(
'data.table
epubr
cowplot
igraph
viridisLite
viridis
RColorBrewer
networkD3
htmlwidgets
crimCV
ggplot2
cluster
dplyr
gghighlight
tidyr
ggrepel
directlabels
factoextra
ggthemes
skmeans
slam
ggnetwork
network
sna',s='\n') %>% {
  inst<-setdiff(.,installed.packages() %>% rownames)
  #load<-intersect(.,installed.packages() %>% rownames)
  install.packages(inst,Ncpus=max(1,parallel::detectCores()-1))
}
# special packages
devtools::install_github('bstewart/stm',ref = 'development')
