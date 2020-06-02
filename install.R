nc<-max(1,parallel::detectCores()-1)
if(!require(devtools)) {install.packages('devtools',Ncpus=nc);library(devtools)}
# special packages
devtools::install_github('bstewart/stm',ref = 'development',Ncpus=nc)
{devtools::install_github('brooksambrose/pack-dev',subdir='tilit',Ncpus=nc);library(tilit)}
devtools::install_github('brooksambrose/pack-dev',subdir = 'plagiat',Ncpus=nc)
# CRAN packages
if(!require(magrittr)) {install.packages('magrittr',Ncpus=nc);library(magrittr)}
tilit::ec(
'data.table
doBy
microbenchmark
disk.frame
profvis
epubr
pbapply
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
  install.packages(inst,Ncpus=nc)
}
