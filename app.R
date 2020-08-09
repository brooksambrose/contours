#' Supervision of Training Data with Picker Application
#'
#'
#'
#' @param reg2trn
#' @param cg2sm
#' @param out
#' @param thr
#' @param fntsz Size in points, defaults to 12 pt. 'auto' will pick it for you.
#' @param raster
#' @param maxlist
#' @param samp if FALSE code entire dataset else if integer(1) sample of components of size samp else if integer(>1) vector of component ids
#' @param aspect
#' @param cache
#' @param dcache
#' @param brwsr FALSE opens in RStudio browser, TRUE in system browser, rstudioapi::viewer in viewer pane
#' @param space
#'
#' @import data.table tilit shiny miniUI shinyPagerUI ggplot2 dendextend ggdendro ggiraph
#' @return
#' @export
#'
#' @examples
trn2sup.f <- function(reg2trn, cg2sm, out = NULL, thr = 0.1, fntsz = "auto",raster=F,
  maxlist = Inf, samp = F, aspect = 1,space=1.5,cache=NULL,dcache=F,brwsr=F,hd=400) {
  reg2trn <- copy(reg2trn) %>% {
    if (samp[1] == FALSE)
      . else if (length(samp) == 1)
        .[.(sample(cmp, samp))] else .[.(samp), on = "cmp"]
  }
  mrows <- reg2trn[, .N, by = cmp][, max(N)]
  if (fntsz == "auto") fntsz <- min(500/(min(mrows, 28)), 12) * (1 + aspect/5)
  pls<-sub(".txt.gz", ".RData", out, fixed = T)
  if(dcache) {
    af<-sub(".txt.gz", "-args-hash.RData", out, fixed = T)
    at<-digest::digest(evalq(allargs()))
    attr(at,'info')<-data.table(file.info(pls))[,!'atime']
    if(file.exists(af)&file.exists(pls)){
      as<-get(load(af))
      if(at==as) {load(pls);build<-F;warning('\nLoading cached plot data.')} else {build<-T}
      if(!all(is.na(attributes(as)$info))) if(!all(attributes(at)$info==attributes(as)$info)) {
        warning(paste0('\n',pls,' file info does not match.\n'))
        print(rbindlist(list(attributes(as)$info,attributes(at)$info)))
      }
    } else {build<-T}
    as<-at
    save(as,file = af)
  } else {build<-T}

  t<-Sys.time()
  if(build) {
    withr::with_package('pbapply',{
      grpn <- uniqueN(reg2trn$cmp);int<-ceiling(grpn/100);pb <- timerProgressBar(min = 0, max = grpn,char='+',width=50)
      pl <- suppressMessages(reg2trn[, .(pl = list({
        sd <- stringdist::stringdistmatrix(cr, method = "jw", p = thr,
          useNames = "strings")
        hc <- sd %>% hclust
        hc$height %<>% scales::rescale(.,to=c(.01,1)) #{scales::rescale_max(., to = c(0, max_depth(hc %>% as.dendrogram))) *2}
        hd <- hc %>% as.dendrogram
        hd <- seriate_dendrogram(hd, sd)
        hs <- get_nodes_attr(hd, "height", simplify = T)
        l<-get_nodes_attr(hd, "leaf", simplify = T)
        n <- which(is.na(l))
        l <- which(l)
        oc <- sapply(as.list(l), function(x) hs[n[which.max(which(n < x))]])
        names(oc) <- labels(hd)
        c <- num2vir(oc, option = "E", end = 0.5)
        p <- dendro_data(hd, type = "triangle") %>% {
          .$leaf_labels <- .$labels
          .$segments$yend %<>% {
            .[!.] <- -.1
            .
          }
          .$labels$y %<>% {
            .[!.] <- -.1
            .
          }
          .
        } %>% {
          n <- nrow(.$labels)
          p <- ggdendrogram(., labels = T, leaf_labels = T, rotate = F,
            color = c, family = "mono", size = fntsz/ggplot2:::.pt,alpha=0
            ,theme_dendro = F
          )
          p$layers[[3]]$aes_params$angle <- 0
          p <- p +
            coord_flip(
              clip = "off", xlim = c(n, 1)
            ) +
            scale_x_continuous(
              breaks=1:n,labels=.$leaf_labels$label # %>% gsub('.','|',.)
            ) +
            cowplot::theme_nothing(
              font_size = fntsz,font_family = 'mono',rel_small = 1
            ) +
            # theme_bw(base_size = fntsz,base_family = 'mono') +
            theme(
              axis.text.y = element_text(angle = 0,hjust=1,color='darkgray'
                ,margin=margin(t = 0, r = .05, b = 0, l = 0, unit = "npc")
              )
              ,aspect.ratio = n/8
              #,plot.margin = unit(c(fntsz,0,fntsz,0),'points')
              #,panel.border=element_rect(fill = NA)
            )
          p
        }
        if(difftime(Sys.time(),t)>1) {setTimerProgressBar(pb,.GRP);t<<-Sys.time()}
        r<-data.table(pl=list(p) #, hd=list(hd)
          ,md=dendextend::max_depth(hd)
          ,nlv=dendextend::nleaves(hd)
          ,nds=list(dendextend::get_nodes_xy(hd, type = "triangle") %>%
              rbind %>% data.table %>% setnames(ec("x,y")) %>%
              .[,lf:=dendextend::get_nodes_attr(hd,"leaf") %>% {replace(.,is.na(.),F)}] %>%
              .[,did:=paste(round(x,3),round(y,3),sep=',')])
          ,hk=list(dendextend::heights_per_k.dendrogram(hd))
        )
        r[,hkm:=lapply(hk,function(x) dendextend::cutree(hd,k=x %>% names %>% as.integer,order_clusters_as_data = F,use_labels_not_values = F))]
        r
      })), keyby = cmp])
      setTimerProgressBar(pb,grpn)
      closepb(pb)
      pl[, `:=`(ec("pl,md,nlv,nds,hk,hkm"), pl %>% rbindlist)]
    })
    save(pl, file = pls)
  }

  rbc <- c(LETTERS %>% head(5), "–")
  if (file.exists(out)) {
    pd <- fread(out, strip.white = F)
  } else {
    pd <- pl[, .(pd = lapply(pl, function(p) {
      pd <- p$plot_env$data$leaf_labels %>% data.table
      pd[, `:=`(group, '_')]
      pd[,did:=paste(round(x,3),round(y,3),sep=',')]
      pd
    })), by = cmp] %>% apply(1, function(x) data.table(cmp = x[[1]],
      x[[2]])) %>% rbindlist
    if (!missing(cg2sm))
      pd[cg2sm[, .(id, h1, str, deg, mut)], on = "label==id", `:=`(h1 = h1,
        str = str, deg = deg, mut = round(mut, 1))]
  }
  # calculate plot height and width in pixels
  pl[pd[,.(ht={(.N > 2) * fntsz * aspect * .N * space} %>% ceiling %>% `+`(10)),by=cmp],on='cmp',height:=ht]
  pl[pd[, .(wd=label %>% as.character %>%
      nchar %>% max),by=cmp],on='cmp',width:=ceiling(wd * fntsz) + md * 2]
  setkey(pd, cmp)
  cp <- pd[, unique(cmp)]
  pc <- if ("time" %in% names(pd)) {
    if (pd[, all(is.na(.SD)), .SDcols = "time"])
      1 else pd[max(which(time != "")), which(cp == cmp)]
  } else 1
  # assign groups
  pd[, `:=`(group, factor(group, levels = rbc %>% c('_')))]
  # set key
  setkey(pd,cmp,x,did)
  pd[,xi:=.I]

  # output functions
  pdup <- function(ix,d_id,lab) {
    pd[CJ(cmp=ix,did=d_id,sorted=F),on=ec('cmp,did'), `:=`(group = lab,
      user = usr(), time = tme())]
  }
  pdwr <- function() {
    fwrite(x = pd %>% setorder(cmp), file = out, quote = F, sep = "\t")
  }
  pdpr <- function(ix) {
    withr::with_options(list(width = 10000), prnablank(
      pd[.(ix), on = 'cmp',!ec("x,y,cmp,did,xi")]))
    cat("\n\n")
  }
  pldr <- function(ix) {
    if (pl[.(ix), on = "cmp",  nlv== 2]) return(qplot(geom = "blank"))
    p<-pl[.(ix), on = "cmp", pl[[1]] +
        geom_point_interactive(
          data = nds[[1]][!(lf)]
          , aes(x = x, y = y, data_id=did)
          , pch = 21, size = 3,fill = "white", inherit.aes = F
        )] +
      geom_label_interactive(
        data = pd[.(ix),on = "cmp", ]
        , aes(x = x, y = y - .1
          , fill = group, label = group, data_id=did)
        ,color = "white", family = "mono", na.rm = T, show.legend = F
      ) +
      discrete_scale(aesthetics = "fill", scale_name = "trainer"
        ,palette = function(n) c(RColorBrewer::brewer.pal(n-2, name = "Dark2"),'pink' %>% rep(2))
        , name = "Groups", drop = F)
    p
  }
  pldd <- function(ix) {
    renderGirafe(expr=girafe(ggobj={
      if(
        (!'time'%in%names(pd))|(length(pd$cmp %>% {.[.<ix]} %>% unique)<3)
      ) return(
        ggplot() + cowplot::theme_nothing()
      )
      d<-pd[
        ,suppressWarnings(max(lubridate::ymd_hms(time),na.rm=T)) %>%
          data.table(first(.N)) %>% setnames(ec('l,n')),by=cmp][cmp<ix]
      d[,xi:=.I][,t:=c(NA,diff(l))]
      d[,sess:=as.logical(t%/%(10*60))] # session start
      d[1,sess:=TRUE]
      d[,sesi:=cumsum(sess)] # session id
      d[,t:=replace(t,sess,{sum(t[!sess])/sum(n[!sess])}*n[sess]) %>% round]
      d[,cmt:=sapply(xi,function(x) weighted.mean(t[1:x],n[1:x]))]
      ts<-d[,paste(sep='',first(l) %>% format('%b %d %I:%M %p%n') %>% gsub(' 0',' ',.),l %>% range %>% diff %>% round(2),' mins'),by=sesi]$V1
      d[,lm(t~xi,weights=1/n) %>% {
        ggplot(aes(x=xi,y=t),data=.)+
          annotate_interactive('segment',x=xi[sess]-.5,xend=xi[sess]-.5,y=0,yend=.95*max(t),color='gray',size=1)+
          annotate_interactive('text',x=xi[sess]-.5,y=max(t),label=ts,color='gray',hjust='inward')+
          geom_smooth_interactive(formula=y~x,data=.SD[-.N],aes(weight=1/n,group=factor(sesi),tooltip=stat(round(y,1))),color='red',linetype='dashed',method='lm',se = F,size=1)+
          geom_line_interactive(aes(y=cmt,data_id=xi,tooltip=cmt),color='green',size=1,linetype='dotted')+
          geom_smooth_interactive(formula=y~x,aes(weight=1/n,group=factor(sesi),tooltip=stat(round(y,1))),color='blue',method='lm',se = F,size=1)+
          geom_point_interactive(aes(size=n,color=abs(.stdresid),tooltip=n))+theme_dark(base_family = 'serif')+
          theme(legend.position = 'none',panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank())+
          scale_color_viridis_c(direction=1,option = 'plasma',limits=qnorm(p=c(.01,.99)))+
          ylab(NULL)+xlab(NULL)+scale_x_continuous(labels=cmp,breaks=xi)
      }]
    },height=hd/72,width=wd/72))
  }
  usr<-function() {try(system("echo $USER", intern = T)) %>% {if (inherits(., "try-error")) "?" else .}}
  tme<-function() {format(Sys.time(), usetz = T)}

  onStop(function() {
    cat("\nSupervised training sets saved to:", out)
    close(lf)
    stopApp()
  })
  lf<-file(description = sub('.txt.gz','-log.txt',f,fixed = T),open = 'a')
  cache<-diskCache(dir=cache,logfile=lf)
  wd<-hd*16/9
  runApp(shinyApp(
    ui = fluidPage(
      tags$head( tags$style(type="text/css", "text { white-space: pre; }"))
      ,uiOutput("title"), div(style = "text-align:center;",miniButtonBlock(
        fluidRow(column(6,actionButton("none","None",width = '100%')),column(6,actionButton("all", "All",width = '100%')))
        ,column(1)
        ,pageruiInput("pager", page_current = pc, pages_total = length(cp))
        ,column(1)
        ,radioButtons("labeler", "Groups", rbc, selected = first(rbc), inline = T)
      ))
      , div(style = "text-align:center;margin: auto;max-height:500px;overflow-y:scroll;max-width:900px;overflow-x:scroll;"
        ,uiOutput('girOut')
      )
      , br(), verbatimTextOutput("table"),girafeOutput('dplot',width=sprintf('%spx',wd),height=sprintf('%spx',hd)))
    , server = function(input,output, session) {
      init1<-F
      init0<-F
      # use to update output$plot in a variety of contexts, only use in observeEvent and eventReactive
      sops<-list(opts_selection(type='single',css=girafe_css(css="fill:red;stroke:white;",text="color:red;stroke:none;")))
      rG<-function() renderGirafe({isolate({
        pl[.(i$x),girafe(
          ggobj = pldr(i$x)
          ,width_svg=width/72,height_svg=height/72
          ,options = sops
        )]
      })})
      ## 2 input$pager i$x pager changes -> update global reactive index then
      ## redraw plot reprint table title
      i <- reactiveValues(x=cp[pc])

      output$title <- renderUI({
        tagList(miniTitleBar(sprintf("Component %s", i$x)))
      })

      observeEvent(label = 'pager->index',input$pager, {
        i$x<<-cp[input$pager$page_current]
      },ignoreInit=T,ignoreNULL=T,priority = 3)

      observeEvent(label = "reset labeler",i$x, {
        if(input$labeler!=first(rbc)) updateRadioButtons(session, inputId = "labeler", selected = first(rbc))
      },ignoreInit=T,ignoreNULL=init1,priority=1)


      output$table <- renderPrint({
        pdpr(i$x)
      })

      ## 3 input$plot_click output$table
      observeEvent(label = 'click',input$plot_selected, {
        output$table <- renderPrint({isolate({
          ips<-input$plot_selected
          nd<-copy(pl[.(i$x),nds[[1]]])
          r<-nd[.(ips),on='did',list(x=x,y=y)]
          # branch node selected
          if (!grepl(',0$',ips)) {
            nd[(lf),k:=pl[.(i$x), on = "cmp",
              (hk[[1]]-r$y) %>% abs %>% which.min %>% {hkm[[1]][,.-1]}
            ]]
            ips <- nd[(lf), .(b = do.call(between, c(r$x, as.list(range(x))))),by = k] %>%
              .[(b), k] %>% {nd[.(.), on = "k", did]}
          }
          if(!is.null(ips)) {
            pdup(i$x,ips,input$labeler);pdwr()}
          pdpr(i$x)
        })})
        output$plot <- rG()
      },ignoreInit=init1,ignoreNULL=T, priority = 2)

      ## 6 input$all output$table
      observeEvent(label = 'all',input$all, {
        b<-pd[.(i$x), on = "cmp",group]
        pd[.(i$x), on = "cmp", `:=`(group = rbc[1], user = usr(), time = tme())]
        pdwr()
        output$table <- renderPrint(pdpr(i$x))
        if(pd[.(i$x), on = "cmp",!identical(group,(b))]) output$plot <- rG()
      },ignoreInit=T,ignoreNULL=T, priority = 2)

      ## 8 input$none output$table
      observeEvent(label = 'none',input$none, {
        b<-pd[.(i$x), on = "cmp",group]
        pd[.(i$x), on = "cmp", `:=`(group = last(rbc), user = usr(), time = tme())]
        pdwr()
        output$table <- renderPrint(pdpr(i$x))
        if(pd[.(i$x), on = "cmp",!identical(group,(b))]) output$plot <- rG()
      },ignoreInit=T,ignoreNULL=T, priority = 2)

      observeEvent(label='plot width',i$x,{
        output$girOut<-renderUI({
          girafeOutput('plot',height='auto',width=sprintf('%spx',pl[.(i$x),on='cmp',width]))
        })
      })

      #https://groups.google.com/forum/#!topic/shiny-discuss/EjEJE6_ZMvU
      observeEvent(label='plot',i$x,{
        output$plot <- rG()
      },ignoreInit = F,ignoreNULL = T,priority = -1)

      observeEvent(label = 'brndwn',i$x,
        output$dplot <- pldd(i$x)
        ,ignoreInit=F,ignoreNULL=init1,priority = -1)
    }), launch.browser = brwsr)
}
