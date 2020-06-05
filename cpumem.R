library(data.table)
library(magrittr)
library(lubridate)
library(directlabels)
library(cowplot)
library(ggplot2)
graphics.off()
binPower <- 20
binUnit <- "Gb"

if(!dir.exists('GorjancShell')) system("git clone https://github.com/gregorgorjanc/GorjancShell && sed -i 's/ETIME\\ COMMAND/ETIME\\ COMMAND\\ 1\\ 2\\ 3/g' GorjancShell/cpumemlog")
if(!file.exists('start')) system('touch start &&  bash GorjancShell/cpumemlog 1',wait = F);Sys.sleep(5)

cpulim<-Sys.getenv('CPU_LIMIT') %>% as.numeric %>%  `*`(100)
memlim<-Sys.getenv('MEM_LIMIT') %>% as.numeric %>% {./2^30}

cm<-fread('cpumemlog_1.txt',fill=T)[
  ,.(pcu=sum(PCPU),gbm=sum(RSS)/2^binPower),by=.(DATE,TIME,PID,COMMAND)][
    ,t:=ymd_hms(paste(DATE,TIME))+hours(5)][,rt:=floor_date(t,unit='10 mins') %>% factor][
      ,PID:=sprintf('%s (%s)',COMMAND,PID)] %>% setkey(PID) %>% na.omit
k<-cm[,.(gbM=max(gbm),t=max(t),pcU=max(pcu)),keyby=PID] %>% setorder(-gbM)

{tthr<-
    30 %>% {lubridate::now()-minutes(.)+hours(5)}
  p<-cm[k[t>=tthr][c(order(gbM,decreasing=T)[1:5],order(pcU,decreasing=T)[1:5]) %>% unique %>% head(10),PID]][
    t>=tthr
    ,
    {ggplot(data=.SD,aes(x=t,color=PID)) + xlab(NULL)} %>% {list(
      {. + geom_line(aes(y=gbm),size=1,alpha=.75) + theme(legend.position = 'none',axis.text.x = element_blank(),axis.ticks.x = element_blank())} %>% {if(is.na(memlim)) {.} else {. + geom_hline(yintercept=memlim,linetype='dashed')}}
      ,{. + geom_line(aes(y=pcu),size=1,alpha=.75) + theme(legend.position = 'bottom')} %>% {if(is.na(cpulim)) {.} else {. + geom_hline(yintercept=cpulim,linetype='dashed')}}
      # . + geom_area(aes(y=gbm),size=1,alpha=.5,color=NA,linetype='solid',orientation = 'x',position = 'stack') + theme(legend.position = 'none',axis.text.x = element_blank(),axis.ticks.x = element_blank()) + ylim(0,1)
      # ,. + geom_area(aes(y=pcu),size=1,alpha=.5,color=NA,linetype='solid',orientation = 'x',position = 'stack') + theme(legend.position = 'bottom') + ylim(0,100)
    )} %>% {
      plot_grid(
        plot_grid(
          .[[1]]
          ,.[[2]] + theme(legend.position = 'none')
          ,axis = 'lr'
          ,align = 'v'
          ,ncol = 1)
        ,get_legend(.[[2]])
        ,rel_heights = c(1,.1)
        ,ncol = 1)
    }
    
  ]
  
  
}
72 %>% {ggsave('cpumem.png',p,dpi=.,units = 'in',width = 1400/.,height=900/.)}
