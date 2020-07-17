#' Interactive group picker for short strings clustered by string distance
#'
#' @param hd
#' @param s1
#' @param s2
#' @param lightest.val
#' @param maxlist
#' @param must.be.this.short.to.ride
#' @param out
#' @param instruct
#'
#' @import data.table tilit shiny miniUI shinyPagerUI ggplot2 dendextend ggdendro
#' @return
#' @export
#'
#' @examples
reg2pck.f <-
  function(reg2trn,
           cg2sm,
           thr=.1,
           s1 = c(0, .075),
           s2 = c(0, 1),
           lightest.val = c(">" = .5, "=" = .8),
           maxlist = Inf,
           must.be.this.short.to.ride = .075,
           out = NULL,
           instruct = F,
           fntsz = 12) {
    reg2trn<-copy(reg2trn)
    rows <- reg2trn[, .N, by = cmp][, max(N)]
    pl<-suppressMessages(reg2trn[,.(pl=list(
      {
        sd <-
          stringdist::stringdistmatrix(cr,
                                       method = 'jw',
                                       p = thr,
                                       useNames = 'strings')
        hc <- sd %>% hclust
        hd <- hc %>% as.dendrogram
        hd <- seriate_dendrogram(hd, sd)
        #plagiat::strdist.dend.picker(hd,out=sub('.RData','',f) %>% {if(!dir.exists(.)) dir.create(.);.},instruct = T)
        
        
        hs <- get_nodes_attr(hd, "height", simplify = T)
        l <- which(hs == 0)
        n <- which(hs != 0)
        oc <- sapply(as.list(l), function(x)
          hs[n[which.max(which(n < x))]])
        names(oc) <- labels(hd)
        # c<-scales::rescale(oc,s1,s2)
        # c[c>lightest.val[">"]]<-lightest.val["="]
        c <- num2vir(oc, option = 'E', end = .5)
        fntsz <- min(200 / rows,15)
        p <- dendro_data(hd, type = 'triangle') %>% {
          .$leaf_labels <- .$labels
          .$labels$label %<>% as.character %>% gsub('.', ' ', .)
          .
        } %>% {
          n <- nrow(.$labels)
          p <- ggdendrogram(
            .,
            labels = T,
            leaf_labels = T,
            rotate = F,
            color = c,
            family = 'mono',
            size = fntsz / ggplot2:::.pt
          )
          p$layers[[3]]$aes_params$angle <- 0
          p$layers[[3]]$aes_params$hjust <- 1.05
          p + coord_flip(
            clip = 'off',
            xlim = c(rows, 0),
            ylim = max(.$segments[, ec('y,yend')]) %>% {
              c(-., .)
            }
          ) + scale_y_continuous(position = 'right') +
            theme(axis.text.y = element_text(
              angle = 0,
              hjust = 1.1,
              size = fntsz,
              family = 'mono'
            ))
        }
        labels_colors(hd) <- c
        
        hk <- suppressWarnings(heights_per_k.dendrogram(hd)) %>% {if(is.infinite(.)[1]) hk<-sd %>% as.vector %>% {names(.)<-'1';.} else .}
        
        for (i in 1:length(hk))
          if (max(table(
            cutree_1k.dendrogram(
              hd,
              k = as.integer(names(hk[i])),
              use_labels_not_values = FALSE,
              dend_heights_per_k = hk
            )
          )) <= maxlist)
            break
        #if(i==1) maxlist<-as.integer(names(hk[i]))
        hdc <-
          cutree_1k.dendrogram(
            hd,
            k = as.integer(names(hk[i])),
            use_labels_not_values = T,
            dend_heights_per_k = hk
          )
        if (length(hdc) == 2)
          hdc[2] <- 1
        j <- i - 1
        hkh <- hk[ifelse(j != 0, j, 1)]
        
        df <-
          data.frame(get_nodes_xy(hd), get_nodes_attr(hd, "label"))
        names(df) <- c("x", "y", "lab")
        df <- df[!is.na(df$lab), ]
        rownames(df) <- df$lab
        df[names(oc), "lh"] <- oc
        df[names(hdc), "hdc"] <- hdc
        rownames(df) <- NULL
        
        df <- data.table(df, key = "lab")
        
        hdc <- split(hdc, hdc)
        hd <- color_branches(hd, k = length(hdc))
        lhdc <- length(hdc)
        
        # invisible({fig <- image_graph(width = 400, height = 400, res = 96)
        # p
        # dev.off()})
        data.table(list(p),list(hd))
      })),keyby=cmp][,ec('pl,hd'):=pl %>% rbindlist])
    unlink(out) # TODO get rid of me
    if(file.exists(out)) pd<-fread(out,strip.white = F) else {
      pd<-pl[,.(pd=lapply(pl,function(p) {
        pd <- p$plot_env$data$leaf_labels %>% data.table
        pd[, group := ''] 
        pd
      })),by=cmp] %>% apply(1,function(x) data.table(cmp=x[[1]],x[[2]])) %>% rbindlist
      if(!missing(cg2sm)) pd[cg2sm[,.(id,h1,str,deg,mut)],on='label==id',`:=`(h1=h1,str=str,deg=deg,mut=round(mut,1))]
    }
    setkey(pd,cmp)
    cp<-pd[,unique(cmp)]
    pc<-if('time'%in%names(pd)) {if(pd[,all(is.na(.SD)),.SDcols='time']) 1 else pd[max(which(time!='')),which(cp==cmp)]} else 1
    ix<-cp[pc]
    rbc<-c(LETTERS %>% head(5), '-')
    runApp(shinyApp(
      ui = fluidPage(
        miniTitleBar(
          sprintf('Component %s',pd[,cmp[1]]) #,
          #left = miniTitleBarButton('prev', 'Prev'),
          #right =  miniTitleBarButton('next', 'Next', primary = T)
        ),column(width = 12,align='center',pageruiInput('pager',page_current = pc,pages_total = pd[,uniqueN(cmp)])),
        miniButtonBlock(
          actionButton('none', 'None', width = '60px'),
          radioButtons(
            'labeler',
            'Groups',
            rbc,
            selected = 'A',
            inline = T
          ),
          actionButton('all', 'All', width = '60px'),
          border = 'bottom'
        ),
        column(
          width = 12,
          align = 'center',
          plotOutput(
            "plot1",
            brush = "plot_brush",
            click = "plot_click",
            # dblclick = 'plot_dbl',
            height = '100%'
          )
        ),
        position = 'right',
        br(),
        verbatimTextOutput("info"),
        tags$style(type = "text/css", ".recalculating { opacity: 1.0; }")
      )
      ,
      server = function(input, output, session) {
        output$info <- renderPrint({
          input$none
          input$all
          input$pager
          input$labeler
          input$plot_click
          input$plot_brush
          input$pager$page_current
          
          ix<<-cp[isolate(input$pager$page_current)]
          bp <-
            brushedPoints(
              pd[.(ix),on='cmp'],
              isolate(input$plot_brush),
              xvar = "y",
              yvar = "x",
              allRows = F
            ) %>% data.table
          np <-
            nearPoints(
              pd[.(ix),on='cmp'],
              isolate(input$plot_click),
              threshold = Inf,
              maxpoints = 1,
              xvar = "y",
              yvar = "x",
              allRows = F
            ) %>% data.table
          r<-data.table()
          if (nrow(bp)) r<-bp
          if(nrow(np)) r<-np
          rm(bp, np)
          
          if(nrow(r)) pd[data.table(cmp=ix,r[, .(x, y)]), on = ec('cmp,x,y'), `:=`(group = isolate(input$labeler),user=try(system('echo $USER',intern = T)) %>% {if(inherits(.,'try-error')) '?'else .},time=format(Sys.time(),usetz=T))]
          if(isolate(input$all)) pd[.(ix),on='cmp', `:=`(group = rbc[1],user=try(system('echo $USER',intern = T)) %>% {if(inherits(.,'try-error')) '?'else .},time=format(Sys.time(),usetz=T))]
          if(isolate(input$none)) pd[.(ix),on='cmp', `:=`(group = tail(rbc,1),user=try(system('echo $USER',intern = T)) %>% {if(inherits(.,'try-error')) '?'else .},time=format(Sys.time(),usetz=T))]
          fwrite(x = pd %>% setkey(cmp),file = out,quote=F,sep='\t')
          {function() {prnablank(pd[.(ix),on='cmp', !ec('x,y,cmp')]);cat('\n\n')}}() # some foolishness to get an extra break
        })
        output$plot1 <- renderPlot({
          input$none
          input$all
          input$pager
          input$labeler
          input$plot_click
          input$plot_brush
          input$pager$page_current
          
          pl[.(ix),on='cmp',pl][[1]] +
            geom_point(
              data = dendextend::get_nodes_xy(pl[.(ix),on='cmp',hd][[1]], type = 'triangle')[-which(dendextend::get_nodes_attr(pl[.(ix),on='cmp',hd][[1]], 'leaf')), ] %>% rbind %>% data.table %>% setnames(ec('x,y')),
              aes(x = x, y = y),
              pch = 21,
              size = 3,
              fill = 'white',
              inherit.aes = F
            ) +
            geom_label(
              data = pd[.(ix),on='cmp',],
              aes(
                x = x,
                y = y * .9,
                color = group,
                label = group
              ),
              family = 'mono',
              na.rm = T,
              show.legend = F
            )
        },height=fntsz*rows+125)
        # output$labeler <- renderUI({
        #   input$pager
        #   input$plot_dbl
        #   updateRadioButtons(session, inputId = 'labeler', selected = rbc[(which(input$labeler ==
        #                                                                            rbc) + 1) %% length(rbc)])
        # })
      }
    ),
    launch.browser = rstudioapi::viewer)
  }
f<-"d/qq/reg2pck.txt.gz"
unlink(f)
reg2pck.f(reg2trn[.(sample(cmp,10))],cg2sm=cg2sm,out = f)

