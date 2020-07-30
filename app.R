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
#' @param gather time in seconds to delay plotting while multiple clicks are gathered (disabled, not working)
#'
#' @import data.table tilit shiny miniUI shinyPagerUI ggplot2 dendextend ggdendro
#' @return
#' @export
#'
#' @examples
trn2sup.f <- function(reg2trn, cg2sm, out = NULL, thr = 0.1, fntsz = "auto",raster=F,
  maxlist = Inf, samp = F, aspect = 1.5,cache=NULL,dcache=F,brwsr=F,gather=1) {
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
        hc$height %<>% {
          scales::rescale_max(., to = c(0, max_depth(hc %>% as.dendrogram))) *
            2
        }
        hd <- hc %>% as.dendrogram
        hd <- seriate_dendrogram(hd, sd)
        hs <- get_nodes_attr(hd, "height", simplify = T)
        l <- which(hs == 0)
        n <- which(hs != 0)
        oc <- sapply(as.list(l), function(x) hs[n[which.max(which(n < x))]])
        names(oc) <- labels(hd)
        c <- num2vir(oc, option = "E", end = 0.5)
        p <- dendro_data(hd, type = "triangle") %>% {
          .$leaf_labels <- .$labels
          .$labels$label %<>% as.character %>% gsub(".", " ", .)
          .$segments$yend %<>% {
            .[!.] <- -2
            .
          }
          .$labels$y %<>% {
            .[!.] <- -2
            .
          }
          .
        } %>% {
          n <- nrow(.$labels)
          p <- ggdendrogram(., labels = T, leaf_labels = T, rotate = F,
            color = c, family = "mono", size = fntsz/ggplot2:::.pt,
            nudge_y = -3)
          p$layers[[3]]$aes_params$angle <- 0
          p + coord_flip(clip = "off", xlim = c(n, 1), ylim = c(-max(nchar(.$labels$label)) -
              5, max(.$segments[, ec("y,yend")]))) + scale_y_continuous(position = "right",
                expand = expansion(add = 1)) + scale_x_continuous(expand = expansion(add = 1)) +
            cowplot::theme_nothing() + theme(axis.text.y = element_text(angle = 0,
              margin = margin(t = 0, r = 2, b = 0, l = 0, unit = "native"),
              family = "mono", size = fntsz/ggplot2:::.pt), axis.text.x = element_blank(),
              aspect.ratio = aspect)
        }
        if(difftime(Sys.time(),t)>1) {setTimerProgressBar(pb,.GRP);t<<-Sys.time()}
        r<-data.table(pl=list(p) #, hd=list(hd)
          ,md=dendextend::max_depth(hd)
          ,nlv=dendextend::nleaves(hd)
          ,nds=list(dendextend::get_nodes_xy(hd, type = "triangle") %>% rbind %>% data.table %>% setnames(ec("x,y")) %>% .[,lf:=dendextend::get_nodes_attr(hd,"leaf")])
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

  rbc <- c(LETTERS %>% head(5), "_")
  if (file.exists(out)) {
    pd <- fread(out, strip.white = F)
  } else {
    pd <- pl[, .(pd = lapply(pl, function(p) {
      pd <- p$plot_env$data$leaf_labels %>% data.table
      pd[, `:=`(group, last(rbc))]
      pd
    })), by = cmp] %>% apply(1, function(x) data.table(cmp = x[[1]],
      x[[2]])) %>% rbindlist
    if (!missing(cg2sm))
      pd[cg2sm[, .(id, h1, str, deg, mut)], on = "label==id", `:=`(h1 = h1,
        str = str, deg = deg, mut = round(mut, 1))]
  }
  # calculate plot height and width in pixels
  pl[pd[,.(ht={(.N > 2) * fntsz * aspect * .N} %>% ceiling %>% `+`(10)),by=cmp],on='cmp',height:=ht]
  pl[pd[, .(wd=label %>% as.character %>%
      nchar %>% max),by=cmp],on='cmp',width:=ceiling(wd * fntsz) + md * 2]
  setkey(pd, cmp)
  cp <- pd[, unique(cmp)]
  pc <- if ("time" %in% names(pd)) {
    if (pd[, all(is.na(.SD)), .SDcols = "time"])
      1 else pd[max(which(time != "")), which(cp == cmp)]
  } else 1
  # generate rasters
  if(raster) pl[height>10,rst:=list({
    tmp<-tempfile(fileext = '.png')
    ggsave(filename = tmp,plot = pl[[1]],width = width/72,height=height/72,units='in',dpi = 72)
    list(grid::rasterGrob(png::readPNG(tmp), interpolate=TRUE))
  }),by=cmp]
  # assign groups
  pd[, `:=`(group, factor(group, levels = rbc %>% sort))]
  # output functions
  pdup <- function(r, input, ix) {
    pd[data.table(cmp = ix, r[, .(x, y)]), on = ec("cmp,x,y"), `:=`(group = isolate(input$labeler),
      user = try(system("echo $USER", intern = T)) %>% {
        if (inherits(., "try-error"))
          "?" else .
      }, time = format(Sys.time(), usetz = T))]
  }
  pdwr <- function() {
    fwrite(x = pd %>% setkey(cmp), file = out, quote = F, sep = "\t")
  }
  pdpr <- function(ix) {
    withr::with_options(list(width = 10000), prnablank(pd[.(ix), on = "cmp",
      !ec("x,y,cmp")]))
    cat("\n\n")
  }
  pldr <- function(ix) {
    if (pl[.(ix), on = "cmp",  nlv== 2])
      return(qplot(geom = "blank"))
    p<-pl[.(ix), on = "cmp", pl][[1]]
    if(raster) p$layers<-(ggplot()+pl[.(ix),on='cmp',pl[[1]]$coordinates$limits %>% {do.call(annotation_custom,list(grob=rst[[1]]
      #,xmin = .$x[1], xmax = .$x[2], ymin = .$y[1], ymax = .$y[2]
      ,xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    ))}])$layers
    p + geom_point(data = pl[.(ix),nds[[1]][is.na(lf)]], aes(x = x, y = y), pch = 21, size = 3,
      fill = "white", inherit.aes = F) + geom_label(data = pd[.(ix),
        on = "cmp", ], aes(x = x, y = y - 2, fill = group, label = group),
        color = "white", family = "mono", na.rm = T, show.legend = F) +
      discrete_scale(aesthetics = "fill", scale_name = "trainer",
        palette = function(n) c("pink", RColorBrewer::brewer.pal(n -
            1, name = "Dark2")), name = "Groups", drop = F)
  }
  # https://gist.github.com/jcheng5/0beeaf94468814dc35f3
  onStop(function() {
    cat("\nSupervised training sets saved to:", out)
    close(lf)
    stopApp()
  })
  lf<-file(description = sub('.txt.gz','-log.txt',f,fixed = T),open = 'a')
  cache<-diskCache(dir=cache,logfile=lf)

  runApp(shinyApp(
    ui = fluidPage(uiOutput("title"), miniButtonBlock(actionButton("none",
      "None", width = "60px"), actionButton("all", "All", width = "60px"),
      pageruiInput("pager", page_current = pc, pages_total = pd[, uniqueN(cmp)]),
      radioButtons("labeler", "Groups", rbc, selected = "A", inline = T,
        width = "300px")), div(style = "text-align:center;margin: auto;max-height:500px;overflow-y:scroll;max-width:900px;overflow-x:scroll;",
          plotOutput("plot", brush = brushOpts("plot_brush", clip = F, delayType = "debounce",
            direction = "y", resetOnNew = T, delay = 10000), click = clickOpts("plot_click",
              clip = F), dblclick = dblclickOpts("plot_dbl", clip = F), height = "auto",
            inline = F)), br(), verbatimTextOutput("table"),plotOutput('dplot',width='100%',height='200px'))
    , server = function(input,
      output, session) {
      # observers will execute in the order they are given
      p <- 0
      ## 1 input$plot_dbl input$labeler
      init1<-F
      init0<-F
      observeEvent(label = "dbl->labeler",input$plot_dbl, {
        updateRadioButtons(session, inputId = "labeler", selected = rbc[(which(isolate(input$labeler) ==
            rbc) + 1) %>% {
              j <- .
              (length(rbc) + 1) %>% {
                (j%/%.) + (j%%.)
              }
            }])
      },ignoreInit=init1,ignoreNULL=init1, priority = p)
      #p %<>% -1
      ## 2 input$pager i$x pager changes -> update global reactive index then
      ## redraw plot reprint table title
      i <<- reactiveValues()
      observeEvent(label = 'pager->index',input$pager, {
        i$x <- cp[isolate(input$pager$page_current)]
      },ignoreInit=init0,ignoreNULL=init0, priority = p)
      p %<>% -1

      observeEvent(label = "reset labeler",i$x, {
        updateRadioButtons(session, inputId = "labeler", selected = first(rbc))
      },ignoreInit=init1,ignoreNULL=init1, priority = p)
      #p %<>% -1

      observeEvent(label = 'title',i$x, {
        output$title <- renderUI({
          tagList(miniTitleBar(sprintf("Component %s", pd[.(isolate(i$x)),
            cmp[1]])))
        })
      },ignoreInit=init1,ignoreNULL=init1,priority = p)
      #p %<>% -1

      observeEvent(label = 'index->table',i$x, {
        output$table <- renderPrint({
          pdpr(isolate(i$x))
        })
      },ignoreInit=init1,ignoreNULL=init1, priority = p)
      p %<>% -1  # won't work without observer

      ## 3 input$plot_click output$table
      observeEvent(label = 'click',input$plot_click, {
        output$table <- renderPrint({
          nd <- pl[.(isolate(i$x)), on = "cmp", nds[[1]]]
          r <- nearPoints(nd, isolate(input$plot_click), threshold = Inf,
            maxpoints = 1, xvar = "y", yvar = "x", allRows = F) %>%
            data.table
          # branch node selected
          if (r$y) {
            nd[(lf), `:=`(k, {pl[.(isolate(i$x)), on = "cmp",
              (hk[[1]]-r$y) %>% abs %>% which.min %>% {hkm[[1]][,.-1]}
            ]}
            )]
            r <- nd[(lf), .(b = do.call(between, c(r$x, as.list(range(x))))),
              by = k] %>% .[(b), k] %>% {
                nd[.(.), on = "k", !"k"]
              }
          }
          pdup(r, input, isolate(i$x))
          pdwr()
          pdpr(isolate(i$x))
        })
      },ignoreInit=init1,ignoreNULL=init1, priority = p)
      #p %<>% -1

      ## 6 input$all output$table
      observeEvent(label = 'all',input$all, {
        output$table <- renderPrint({
          pd[.(isolate(i$x)), on = "cmp", `:=`(group = rbc[1], user = try(system("echo $USER",
            intern = T)) %>% {
              if (inherits(., "try-error"))
                "?" else .
            }, time = format(Sys.time(), usetz = T))]
          pdwr()
          pdpr(isolate(i$x))
        })
      },ignoreInit=init1,ignoreNULL=init1, priority = p)
      #p %<>% -1
      ## 8 input$none output$table
      observeEvent(label = 'none',input$none, {
        output$table <- renderPrint({
          pd[.(isolate(i$x)), on = "cmp", `:=`(group = tail(rbc,
            1), user = try(system("echo $USER", intern = T)) %>%
              {
                if (inherits(., "try-error"))
                  "?" else .
              }, time = format(Sys.time(), usetz = T))]
          pdwr()
          pdpr(isolate(i$x))
        })
      },ignoreInit=init1,ignoreNULL=init1, priority = p)
      #p %<>% -1
      ## 10 input$plot_brush output$table
      observeEvent(label = 'brush',input$plot_brush, {
        output$table <- renderPrint({
          r <- brushedPoints(pd[.(isolate(i$x)), on = "cmp"], isolate(input$plot_brush),
            xvar = "y", yvar = "x", allRows = F) %>% data.table
          pdup(r, input, isolate(i$x))
          pdwr()
          pdpr(isolate(i$x))
        })
      },ignoreInit=init1,ignoreNULL=init1, priority = p)
      p %<>% -1

      #https://groups.google.com/forum/#!topic/shiny-discuss/EjEJE6_ZMvU

      t<-Sys.time()
      Sys.sleep(gather)
      plt <-eventReactive(label = 'plot',{
        input$plot_click
        input$plot_brush
        input$none
        input$all
        i$x
      }, {
        session$resetBrush("plot")
        t<<-Sys.time()
        pldr(isolate(i$x)) + theme_void()  #+ theme(panel.border = element_rect(fill=NA))
      },ignoreInit=init1,ignoreNULL=init1#,priority = p
      )
      #p %<>% -1



      observeEvent(label = 'plotter',{plt();if(difftime(Sys.time(),t)<gather) invalidateLater(1000)}
        , {
          if(difftime(Sys.time(),t)>gather) output$plot<-renderCachedPlot(
            expr = plt()
            ,cacheKeyExpr = pd[.(isolate(i$x)), .(label,group)]
            ,cache=cache
            ,sizeGrowthRatio(height = pl[.(isolate(i$x)),height], width = pl[.(isolate(i$x)),width], growthRate = 1)
          )
        }
        ,priority = p)


      observeEvent(label = 'brndwn',i$x, {
        output$dplot <- renderCachedPlot(expr = {
          d<-fread(out)
          if(!'time'%in%names(d)|length(d$cmp %>% {.[.<isolate(i$x)]} %>% unique)<3) return(geom_blank())
          d<-d[,max(lubridate::ymd_hms(time),na.rm=T) %>%  data.table(first(.N)) %>% setnames(ec('l,n')),by=cmp][,ix:=.I][,t:=c(NA,diff(l))][,cmt:=c(NA,sapply(.I[-1],function(x) weighted.mean(t[2:x],n[2:x])))]
          d<-d[cmp<isolate(i$x)]
          d[-1][,lm(t~ix,weights=1/n) %>% {ggplot(aes(x=ix,y=t),data=.)+geom_smooth(data=.SD[-.N],aes(weight=1/n),color='red',linetype='dashed',method='lm',se = F,size=1)+geom_line(aes(y=cmt),color='green',size=1,linetype='dashed')+annotate(size=1,geom='segment',x=min(ix),xend=max(ix),y=min(.$fitted),yend=max(.$fitted),color='blue')+geom_point(aes(size=n,color=abs(.stdresid)))+theme_dark(base_family = 'serif')+theme(legend.position = 'none',panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank())+scale_color_viridis_c(direction=1,option = 'plasma',limits=qnorm(p=c(.01,.99)))+ylab(NULL)+xlab(NULL)+scale_x_continuous(labels=cmp,breaks=ix)}]
        },cacheKeyExpr = pd[cmp<isolate(i$x), time], cache=cache
          , sizeGrowthRatio(height = 200, width = 400, growthRate = 1.25)
        )
      },ignoreInit=init1,ignoreNULL=init1,priority = p)
      #p %<>% -1

    }), launch.browser = brwsr)
}
