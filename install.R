nc<-as.integer(system('echo $CPU_LIMIT',intern=T)) # use binder cpu_limit on build pod
cat(sprintf('CPU_LIMIT: %s\n',nc))
if(is.na(nc)) nc<-max(1,parallel::detectCores()-1)
cat(sprintf('Usable cores: %s\n',nc))
if(!require(devtools)) {install.packages('devtools',Ncpus=nc);library(devtools)}
# special packages
try(devtools::install_github('bstewart/stm',ref = 'development',Ncpus=nc))
try(devtools::install_github("larmarange/JLutils",Ncpus=nc))
{devtools::install_github('brooksambrose/pack-dev',subdir='tilit',Ncpus=nc);library(tilit)}
devtools::install_github('brooksambrose/pack-dev',subdir = 'plagiat',Ncpus=nc)
try(devtools::install_github('wleepang/shiny-pager-ui',Ncpus=nc))
# CRAN packages
if(!require(magrittr)) {install.packages('magrittr',Ncpus=nc);library(magrittr)}
tilit::ec(
'data.table
psych
lspline
ggiraph
reactlog
raster
rgdal
formatR
Rfast
shinyjs
ggdendro
miniUI
reprex
imager
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
ggrepel
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
# h2o https://docs.h2o.ai/h2o/latest-stable/h2o-r/docs/articles/getting_started.html
for (pkg in c("RCurl","jsonlite")) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))
