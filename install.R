if(require(devtools)) {install.packages('devtools');library(devtools)}
{devtools::install_github('brooksambrose/pack-dev',subdir='tilit');library(tilit)}
if(require(magrittr)) {install.packages('magrittr');library(magrittr)}
tilit::ec(
'data.table
cowplot
magrittr
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
install.packages(.,Ncpus=parallel::detectCores()-1)
}
