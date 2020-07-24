#' Supervision of Training Data with Picker Application
#'
#'
#' @param reg2trn
#' @param cg2sm
#' @param out
#' @param thr
#' @param fntsz Size in points, defaults to 12 pt. 'auto' will pick it for you.
#' @param space
#' @param maxlist
#' @param samp if FALSE code entire dataset else if integer(1) sample of components of size samp else if integer(>1) vector of component ids
#'
#' @import data.table tilit shiny miniUI shinyPagerUI ggplot2 dendextend ggdendro
#' @return
#' @export
#'
#' @examples
trn2sup.f <- function(reg2trn, cg2sm, out = NULL, thr = 0.1, fntsz = "auto",
  maxlist = Inf, samp = F, aspect = 1.5) {
  reg2trn <- copy(reg2trn) %>% {
    if (samp[1] == FALSE)
      . else if (length(samp) == 1)
        .[.(sample(cmp, samp))] else .[.(samp), on = "cmp"]
  }
  mrows <- reg2trn[, .N, by = cmp][, max(N)]
  if (fntsz == "auto")
    fntsz <- min(500/(min(mrows, 28)), 12) * (1 + aspect/5)
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
    labels_colors(hd) <- c
    hk <- suppressWarnings(heights_per_k.dendrogram(hd)) %>% {
      if (is.infinite(.)[1]) hk <- sd %>% as.vector %>% {
        names(.) <- "1"
        .
      } else .
    }
    for (i in 1:length(hk)) if (max(table(cutree_1k.dendrogram(hd,
      k = as.integer(names(hk[i])), use_labels_not_values = FALSE,
      dend_heights_per_k = hk))) <= maxlist) break
    hdc <- cutree_1k.dendrogram(hd, k = as.integer(names(hk[i])), use_labels_not_values = T,
      dend_heights_per_k = hk)
    if (length(hdc) == 2) hdc[2] <- 1
    j <- i - 1
    hkh <- hk[ifelse(j != 0, j, 1)]
    df <- data.frame(get_nodes_xy(hd), get_nodes_attr(hd, "label"))
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
    data.table(list(p), list(hd))
  })), keyby = cmp][, `:=`(ec("pl,hd"), pl %>% rbindlist)])
  save(pl, file = sub(".txt.gz", ".RData", out, fixed = T))
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
  setkey(pd, cmp)
  cp <- pd[, unique(cmp)]
  pc <- if ("time" %in% names(pd)) {
    if (pd[, all(is.na(.SD)), .SDcols = "time"])
      1 else pd[max(which(time != "")), which(cp == cmp)]
  } else 1
  pd[, `:=`(group, factor(group, levels = rbc %>% sort))]
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
    if ((pl[.(ix), on = "cmp", hd[[1]]] %>% nleaves) == 2)
      return(qplot(geom = "blank"))
    pl[.(ix), on = "cmp", pl][[1]] + geom_point(data = get_nodes_xy(pl[.(ix),
      on = "cmp", hd][[1]], type = "triangle")[-which(get_nodes_attr(pl[.(ix),
        on = "cmp", hd][[1]], "leaf")), ] %>% rbind %>% data.table %>%
        setnames(ec("x,y")), aes(x = x, y = y), pch = 21, size = 3,
      fill = "white", inherit.aes = F) + geom_label(data = pd[.(ix),
        on = "cmp", ], aes(x = x, y = y - 2, fill = group, label = group),
        color = "white", family = "mono", na.rm = T, show.legend = F) +
      discrete_scale(aesthetics = "fill", scale_name = "trainer",
        palette = function(n) c("pink", RColorBrewer::brewer.pal(n -
            1, name = "Dark2")), name = "Groups", drop = F)
  }
  onStop(function() {
    cat("\nSupervised training sets saved to:", out)
  })
  runApp(shinyApp(ui = fluidPage(uiOutput("title"), miniButtonBlock(actionButton("none",
    "None", width = "60px"), actionButton("all", "All", width = "60px"),
    pageruiInput("pager", page_current = pc, pages_total = pd[, uniqueN(cmp)]),
    radioButtons("labeler", "Groups", rbc, selected = "A", inline = T,
      width = "300px")), div(style = "text-align:center;margin: auto;max-height:500px;overflow-y:scroll;max-width:900px;overflow-x:scroll;",
        plotOutput("plot", brush = brushOpts("plot_brush", clip = F, delayType = "debounce",
          direction = "y", resetOnNew = T, delay = 10000), click = clickOpts("plot_click",
            clip = T), dblclick = dblclickOpts("plot_dbl", clip = F), height = "auto",
          inline = F)), br(), verbatimTextOutput("table")), server = function(input,
            output, session) {
            # observers will execute in the order they are given
            p <- 0
            ## 1 input$plot_dbl input$labeler
            observeEvent(input$plot_dbl, {
              updateRadioButtons(session, inputId = "labeler", selected = rbc[(which(isolate(input$labeler) ==
                  rbc) + 1) %>% {
                    j <- .
                    (length(rbc) + 1) %>% {
                      (j%/%.) + (j%%.)
                    }
                  }])
            }, priority = p)
            p %<>% -1
            ## 2 input$pager i$x pager changes -> update global reactive index then
            ## redraw plot reprint table title
            i <<- reactiveValues()
            observeEvent(input$pager, {
              i$x <- cp[isolate(input$pager$page_current)]
            }, priority = p)
            p %<>% -1
            observeEvent(i$x, {
              updateRadioButtons(session, inputId = "labeler", selected = first(rbc))
            }, priority = p)
            p %<>% -1
            observeEvent(i$x, {
              output$table <- renderPrint({
                pdpr(isolate(i$x))
              })
            }, priority = p)
            p %<>% -1  # won't work without observer
            observeEvent(i$x, {
              output$title <- renderUI({
                tagList(miniTitleBar(sprintf("Component %s", pd[.(isolate(i$x)),
                  cmp[1]])))
              })
            }, priority = p)
            p %<>% -1
            ## 3 input$plot_click output$table
            observeEvent(input$plot_click, {
              output$table <- renderPrint({
                nd <- get_nodes_xy(pl[.(isolate(i$x)), on = "cmp", hd[[1]]],
                  type = "triangle") %>% data.table %>% setnames(ec("x,y"))
                r <- nearPoints(nd, isolate(input$plot_click), threshold = Inf,
                  maxpoints = 1, xvar = "y", yvar = "x", allRows = F) %>%
                  data.table
                # branch node selected
                if (r$y) {
                  nd[(!y), `:=`(k, cutree_1h.dendrogram(pl[.(isolate(i$x)),
                    on = "cmp", hd[[1]]], r$y, order_clusters_as_data = F,
                    use_labels_not_values = F))]
                  r <- nd[!is.na(k), .(b = do.call(between, c(r$x, as.list(range(x))))),
                    by = k] %>% .[(b), k] %>% {
                      nd[.(.), on = "k", !"k"]
                    }
                }
                pdup(r, input, isolate(i$x))
                pdwr()
                pdpr(isolate(i$x))
              })
            }, priority = p)
            p %<>% -1
            ## 6 input$all output$table
            observeEvent(input$all, {
              output$table <- renderPrint({
                pd[.(isolate(i$x)), on = "cmp", `:=`(group = rbc[1], user = try(system("echo $USER",
                  intern = T)) %>% {
                    if (inherits(., "try-error"))
                      "?" else .
                  }, time = format(Sys.time(), usetz = T))]
                pdwr()
                pdpr(isolate(i$x))
              })
            }, priority = p)
            p %<>% -1
            ## 8 input$none output$table
            observeEvent(input$none, {
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
            }, priority = p)
            p %<>% -1
            ## 10 input$plot_brush output$table
            observeEvent(input$plot_brush, {
              output$table <- renderPrint({
                r <- brushedPoints(pd[.(isolate(i$x)), on = "cmp"], isolate(input$plot_brush),
                  xvar = "y", yvar = "x", allRows = F) %>% data.table
                pdup(r, input, isolate(i$x))
                pdwr()
                pdpr(isolate(i$x))
              })
            }, priority = p)
            p %<>% -1
            observeEvent({
              input$none
              input$all
              input$plot_click
              input$plot_brush
              i$x
            }, {
              session$resetBrush("plot")
              output$plot <- renderPlot({
                pldr(isolate(i$x)) + theme_void()  #+ theme(panel.border = element_rect(fill=NA))
              }, height = 10 + {
                pd[.(isolate(i$x)), on = "cmp", .N > 2] * (fntsz * aspect *
                    pd[.(isolate(i$x)), .N])
              }, width = {
                pl[.(isolate(i$x)), on = "cmp", hd[[1]]] %>% dendextend::max_depth(.) *
                  2 + pd[.(isolate(i$x)), on = "cmp", label %>% as.character %>%
                      nchar %>% max] * fntsz
              })
            }, priority = p)
            p %<>% -1
          }), launch.browser = rstudioapi::viewer)
}
