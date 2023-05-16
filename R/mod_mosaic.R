#' mosaic UI Function
#'
#' @description A shiny Module for the Mosaic Plot in Subscreen.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mosaic_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidPage(
      shiny::fluidRow(
          shiny::column(3,
          # Option and variable panel
          shiny::uiOutput(ns('PanelMosaic'))
        ),
        shiny::column(8,
          shiny::div(style = "position:relative",
            # Mosaic plot
            shiny::plotOutput(
              outputId = ns("mosaic"),
              # hover options
              hover = hoverOpts(id = ns('plot_hover'), delay = 10, delayType = 'debounce'),
              height = 600,
              width = 1000
            ),
            shiny::uiOutput(ns("hover_info"))
          )#,
          #shiny::br(),
          # Table with hover information
          #DT::dataTableOutput(ns("tmp_info"))
        )
      )
    )
  )
}

#' mosaic Server Function
#'
#' @noRd
mod_mosaic_server <- function(input, output, session,
    results = scresults,
    ColorBGplot = colthemeCol$ColorBGplot,
    nice_Numbers) {

  ns <- session$ns

  # helper functions
   roundUpNice <- function(x, nice = nice_Numbers) {
    if (length(x) != 1) stop("'x' must be of length 1")
    if (x >= 0) 10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) *nice)[[1]]]]
    else -1 * (roundDownNice(-x, nice = nice_Numbers))
  }

  roundDownNice <- function(x, nice = nice_Numbers) {
    if (length(x) != 1) stop("'x' must be of length 1")
    if (x >= 0) 10^floor(log10(x)) * nice[[max(which(x >= 10^floor(log10(x)) * nice))]]
    else -1 * (roundUpNice(-x, nice = nice_Numbers))
  }

  output$PanelMosaic <- shiny::renderUI({
    style.panel <- paste('background-color: ', ColorBGplot(), ';padding: 9px;')

    shiny::wellPanel(
      style = style.panel,
      shiny::selectInput(
        inputId = ns("var1"),
        label = "First subgroup variable (x)",
        choices = results()$factors,
        selected = results()$factors[1]
      ),
      shiny::selectInput(
        inputId = ns("var2"),
        label = "Second subgroup variable (y)",
        choices = c('no selection', results()$factors),
        selected = 'no selection'
      ),
      shiny::conditionalPanel(condition = paste0('output[\'', ns('show_var22'), "\'] == true"),
        shiny::selectInput(
          inputId = ns("var22"),
          label = "Third subgroup variable (y2)",
          choices = c('no selection', results()$factors),
          selected = 'no selection'
        )
      ),
      shiny::selectInput(
        inputId = ns("var3"),
        label = "Reference variable (color)",
        choices = setdiff(names(results()$results_total),'N.of.subjects'),
        selected = input$y
      ),
      shiny::radioButtons(
        inputId = ns("logmosaic"),
        label = "Plot Type",
        choices = c(linear = "lin", logarithmic = "log"),
        selected = "lin",
        inline = TRUE
      ),
      "Use mouse hover to get further information about the subgroup(s)!"
    )
  })

  shiny::observeEvent(c(input$var1, input$var22), {
    shiny::updateSelectInput(
      inputId ="var2",
      selected = input$var2,
      choices = c('no selection', results()$factors[!c(results()$factors) %in% c(input$var1, input$var22)])
    )
  })

  shiny::observeEvent(c(input$var1, input$var2), {
    shiny::updateSelectInput(
      inputId ="var22",
      selected = input$var22,
      choices = c('no selection', results()$factors[!c(results()$factors) %in% c(input$var1, input$var2)])
    )
  })

  shiny::observeEvent(c(input$var2, input$var22), {
    shiny::updateSelectInput(
      inputId ="var1",
      selected = input$var1,
      choices = c(results()$factors[!c(results()$factors) %in% c(input$var2,input$var22)])
    )
  })

  show_var22_val <- shiny::reactiveValues(val = FALSE)
  output$show_var22 <- shiny::reactive({
    show_var22_val$val
  })
  shiny::observeEvent(input$var2, {
    if (input$var2 != "no selection") {
      show_var22_val$val <- TRUE
    } else {
    shiny::updateSelectInput(
      inputId ="var22",
      selected = 'no selection'
    )
      show_var22_val$val <- FALSE
    }
  })
  outputOptions(output, "show_var22", suspendWhenHidden = FALSE)

  #### renderPlot mosaic ####
  output$mosaic <- shiny::renderPlot({
    shiny::req(results())
    # use subscreen_mosaicPlot to draw mosaic plot

    subscreen_mosaicPlot(
      res = results(),
      mos.x = shiny::req(input$var1),
      mos.y = shiny::req(input$var2),
      mos.y2 = shiny::req(input$var22),
      mos.z = shiny::req(input$var3),
      col.bg = ColorBGplot(),
      col.txt = font_color(ColorBGplot()),
      colrange.z = c('#00BCFF','gray89','#89D329'),
      scale = input$logmosaic
    )

  }, bg = "transparent")

  hoverlabel <- shiny::reactiveValues(value = NULL)

  shiny::observeEvent(c(input$plot_hover$x, input$plot_hover$y, input$var1, input$var2, input$var22,
    input$var3), ignoreNULL = FALSE, {
    if (!is.null(input$plot_hover$x) & !is.null(input$plot_hover$y)) {

    mos.x <- shiny::req(input$var1)
    mos.y <- shiny::req(input$var2)
    mos.y2 <- shiny::req(input$var22)
    mos.z <- shiny::req(input$var3)
    col.bg <- ColorBGplot()
    col.txt <- font_color(ColorBGplot())
    colrange.z <- c('#00BCFF','gray89','#89D329')
    not.used <- 'Not used'

    if (mos.y == 'no selection') {
      mos.y <- NULL
    }
    if (mos.y2 == 'no selection' | is.null(mos.y)) {
      mos.y2 <- NULL
    }
    if (!is.null(mos.y)) {
      if (mos.x == mos.y) {
        mos.y <- NULL
      }
    }
    if (!is.null(mos.y2)) {
      if (mos.x == mos.y2 | mos.y == mos.y2) {
        mos.y2 <- NULL
      }
    }


    tmp_x <- results()$sge[results()$sge$nfactors == 1 & !results()$sge[, mos.x] %in% not.used, ]
    tmp_x2 <- dplyr::arrange(tmp_x, !!rlang::sym(mos.x))
    prop.x <- cumsum(tmp_x2[, 'N.of.subjects'])
    prop.x <- c(0,prop.x) / max(prop.x)
    mid.x <- (prop.x[-length(prop.x)] + prop.x[-1])/2
    names(mid.x) <- paste0(mos.x, ' = ', tmp_x2[, mos.x])
    hov.x <- as.character(tmp_x2[, mos.x])
    prop.y <- c(0, 1)
    mid.y <- 0.5
    if (!is.null(mos.y)) {
      tmp_y <- results()$sge[results()$sge$nfactors == 1 & !results()$sge[, mos.y] %in% not.used, ]
      tmp_y <- dplyr::arrange(tmp_y, !!rlang::sym(mos.y))
      prop.y <- cumsum(tmp_y[, 'N.of.subjects'])
      prop.y <- c(0,prop.y) / max(prop.y)
      mid.y <- (prop.y[-length(prop.y)] + prop.y[-1])/2
      names(mid.y) <- paste0(mos.y, ' = ',tmp_y[, mos.y])
      hov.y <- tmp_y[, c(mos.y)]
      if (!is.null(mos.y2)) {

        tmp_y <- results()$sge[results()$sge$nfactors == 2 & !results()$sge[, mos.y] %in% not.used &
                               !results()$sge[, mos.y2] %in% not.used, ]
        tmp_y <- dplyr::arrange(tmp_y, !!!rlang::syms(c(mos.y, mos.y2)))
        prop.y <- cumsum(tmp_y[, 'N.of.subjects'])
        prop.y <- c(0, prop.y)/max(prop.y)
        mid.y <- (prop.y[-length(prop.y)] + prop.y[-1])/2
        names(mid.y) <- paste0(mos.y, ' = ', tmp_y[, mos.y],' & ',
          mos.y2, ' = ', tmp_y[,mos.y2])
        hov.y <- tmp_y[, c(mos.y, mos.y2)]
      }
    }
    if (shiny::req(input$logmosaic) == "lin") {
      rg.z <- range(results()$sge[, mos.z], na.rm = TRUE)
    }
    if (shiny::req(input$logmosaic) == "log") {
      rg.z <- log(
        range(
          results()$sge[, mos.z], na.rm = TRUE
        )
      )
    }

    if (is.null(mos.y)) {
     # tmp_1factors <- results()$sge[results()$sge$nfactors == 1 & !results()$sge[, mos.x] %in% not.used, ]

      tmp1_factors <- tmp_x

    } else {
      if (is.null(mos.y2)) {
        tmp_2factors <- results()$sge[results()$sge$nfactors == 2 & !results()$sge[, mos.x] %in% not.used & !results()$sge[, mos.y] %in% not.used,]
      } else {
        tmp_3factors <- results()$sge[results()$sge$nfactors == 3 & !results()$sge[, mos.x] %in% not.used &
                               !results()$sge[, mos.y] %in% not.used & !results()$sge[, mos.y2] %in% not.used, ]
        tmp_3factors <- dplyr::arrange(tmp_3factors, !!!rlang::syms(c(mos.x,mos.y,mos.y2)))
      }
    }

      if (!is.null(mos.y2)) {
        val.z <- data.frame(matrix(NA, nrow = length(mid.y), ncol = length(mid.x)))
        colnames(val.z) <- names(mid.x)
        rownames(val.z) <- names(mid.y)
        for (i in 1:length(mid.x)) {
          tmp <- tmp_3factors %>% dplyr::filter(!! rlang::sym(mos.x) == tmp_x[i, mos.x])
          for (j in 1:length(mid.y)) {
            tmp2 <- tmp_y[j, c(mos.y,mos.y2)]
            if (dim(dplyr::filter(tmp,!! rlang::sym(mos.y) == tmp2[,1] & !!rlang::sym(mos.y2) == tmp2[,2]))[1] > 0) {
              tmp3 <- dplyr::filter(tmp,!! rlang::sym(mos.y) == tmp2[,1] & !!rlang::sym(mos.y2) == tmp2[,2])
              tmp3 <- ifelse(shiny::req(input$logmosaic) == "lin", tmp3[, mos.z], log(tmp3[, mos.z]))
              val.z [j,i] <- tmp3
            } else {
              val.z [j,i] <- NA
            }
          }
        }
        tmp <- results()$sge[results()$sge$nfactors == 3 & !results()$sge[, mos.x] %in% not.used &
                               !results()$sge[, mos.y] %in% not.used & !results()$sge[, mos.y2] %in% not.used, ]
        tmp <- dplyr::arrange(tmp, !!!rlang::syms(c(mos.x,mos.y,mos.y2)))
      } else if (!is.null(mos.y)) {
        val.z <- data.frame(matrix(NA, nrow = length(mid.y), ncol = length(mid.x)))
        colnames(val.z) <- names(mid.x)
        rownames(val.z) <- names(mid.y)

        for (i in 1:length(mid.x)) {
          tmp <- tmp_2factors %>% dplyr::filter(!! rlang::sym(mos.x) == tmp_x[i, mos.x])
          for (j in 1:length(mid.y)) {
            level <- tmp_y[j, mos.y]
            if (dim(dplyr::filter(tmp,!! rlang::sym(mos.y) == level))[1] > 0) {
              tmp1 <- dplyr::filter(tmp,!! rlang::sym(mos.y) == level)
              tmp1 <- ifelse(shiny::req(input$logmosaic) == "lin", tmp1[, mos.z], log(tmp1[, mos.z]))
              val.z [j,i] <- tmp1
            } else {
              val.z [j,i] <- NA
            }
          }
        }

        tmp <- results()$sge[results()$sge$nfactors == 2 & !results()$sge[, mos.x] %in% not.used & !results()$sge[, mos.y] %in% not.used,]

      } else {
        tmp <- results()$sge[results()$sge$nfactors == 1 & !results()$sge[, mos.x] %in% not.used, ]
        val.z <- matrix(ifelse(shiny::req(input$logmosaic) == "lin", tmp[, mos.z], log(tmp[, mos.z])), ncol = length(prop.x) - 1, byrow = FALSE)
      }

      col.disp <- c("SGID",mos.z, mos.x, mos.y, mos.y2, "N.of.subjects")
      if (is.null(mos.y)) {
        tmp <- tmp #%>% dplyr::arrange(!!rlang::sym(input$var3))
        #%>% dplyr::arrange(!!rlang::sym(input$var3))
        tmp2 <- tmp_x2[hov.x == (hov.x[cut(input$plot_hover$x, prop.x, labels = FALSE)]), col.disp]

        hoverlabel$value <- tmp2
      } else {
        if (is.null(mos.y2)) {
          hoverlabel$value <- tmp[tmp[,mos.x] == (hov.x[cut(input$plot_hover$x, prop.x, labels = FALSE)]) &
                                    tmp[,mos.y] == (hov.y[cut(input$plot_hover$y, prop.y, labels = FALSE)]),col.disp]
        } else {
          hoverlabel$value <- tmp[
                                tmp[,mos.x] == (hov.x[cut(input$plot_hover$x, prop.x, labels = FALSE)]) &
                                tmp[,mos.y] == (hov.y[,mos.y][cut(input$plot_hover$y, prop.y, labels = FALSE)]) &
                                tmp[,mos.y2] == (hov.y[,mos.y2][cut(input$plot_hover$y, prop.y, labels = FALSE)])
                              ,col.disp]
        }
      }
      hoverlabel$value <- hoverlabel$value[, !startsWith(colnames(hoverlabel$value), "FCID_")]
      hoverlabel$value <- hoverlabel$value[, !startsWith(colnames(hoverlabel$value), "Complement_")]

    #   if (!is.null(hoverlabel$value) & dim(hoverlabel$value)[1] > 0) {
    #     dt.sginfo <- DT::datatable(
    #       data = hoverlabel$value,
    #       extensions = 'Buttons',
    #       escape = FALSE,
    #       options = list(
    #         initComplete = JS(
    #           "function(settings, json) {",
    #           paste0(
    #             "$(this.api().table().header()).css({'background-color': '",
    #              ColorBGplot(),
    #              "', 'color': '",
    #              font_color(different_hues(ColorBGplot())),
    #              "'});"
    #           ),
    #           "}"
    #         ),
    #         dom = 'rtp',
    #         paging = FALSE,
    #         pageLength = 1,
    #         bSort = FALSE
    #       ),
    #       class = 'cell-border stripe',
    #       rownames = FALSE,
    #       caption = 'Subgroup information',
    #       filter = 'none'
    #     )
    #
    #     col.tabFont <- font_color(different_hues(ColorBGplot()))
    #     dt.sginfo <- DT::formatStyle(
    #       table = dt.sginfo,
    #       columns = 1:ncol(hoverlabel$value),
    #       backgroundColor = different_hues(ColorBGplot()),
    #       border = paste0('.5px solid ', ColorBGplot()),
    #       color = col.tabFont
    #     )
    #
    #     output$tmp_info <- DT::renderDataTable(dt.sginfo)
    #   }
    # } else {
    #   output$tmp_info <- DT::renderDataTable(NULL)
     }
  })

  #09JUN2022
  output$hover_info <- shiny::renderUI({
    shiny::req(input$plot_hover, hoverlabel$value)
    val.z.ij <- NA

    input$plot_hover
    hover <- input$plot_hover
    hover$mapping <- list(xintercept = "xintercept", x = "x", y = "y")

    colrange.z = c('#00BCFF','gray89','#89D329')
    if (shiny::req(input$logmosaic) == "lin") {
      rg.z <- range(results()$sge[, input$var3], na.rm = TRUE)
    }
    if (shiny::req(input$logmosaic) == "log") {
      rg.z <- log(
        range(
          results()$sge[, input$var3], na.rm = TRUE
        )
      )
    }
    mean.z <- ifelse(shiny::req(input$logmosaic) == "lin",
                     mean.z <- results()$results_total[,input$var3],
                     log(results()$results_total[,input$var3]))
    tr.mean.z <- (mean.z-rg.z[1])/diff(rg.z)
    f_colZ <- colorRamp(colrange.z, bias = log(tr.mean.z, base = 0.5))


    if (input$var3 %in% colnames(hoverlabel$value)) {
      val.z.ij <- hoverlabel$value[input$var3]

    if (shiny::req(input$logmosaic) == "log") {
     val.z.ij <- log(hoverlabel$value[input$var3])
    }

    if (dim(val.z.ij)[1] > 0 & !is.na(as.numeric(val.z.ij))) {
      hoverColor <- grDevices::rgb(f_colZ((val.z.ij - rg.z[1])/diff(rg.z)), maxColorValue = 255)

      left_pct <- (hover$coords_img$x - hover$range$left) / (hover$range$right - hover$range$left)
      top_pct <- (hover$domain$top - hover$y ) / (hover$domain$top - hover$domain$bottom)
      left_px <- (hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x) + 3
      top_px <- (hover$range$top + top_pct * (hover$range$bottom - hover$range$top) / hover$img_css_ratio$y) + 3

      style <- paste0("position:absolute; z-index:100; background-color: rgba(",
        grDevices::col2rgb(hoverColor)[1],",",grDevices::col2rgb(hoverColor)[2],",",grDevices::col2rgb(hoverColor)[3],",0.95); ",
                      "left:", left_px, "px; top:", top_px, "px; border-width: 1px; border-color: #424242;")

      shiny::wellPanel(
       style = style,
       shiny::p(
          shiny::HTML(
            paste0(
               "<b style = 'color: black;'>",
            paste(
                paste0(
                  colnames(hoverlabel$value)," = ", data.frame(lapply(hoverlabel$value, as.character), stringsAsFactors=FALSE)
                ), collapse = "</br>"
              )
            ,"</b>")
            # ifelse(length(tmp1)>0,

            # paste0(
            #   "<b style = 'color: ",
            #   font_color(point$color),
            #   "'> ",
            #   y(),
            #   ": ",
            #   point$x,
            #   "</br>",
            #   "<b style = 'color: ",
            #   font_color(point$color),
            #   "'> ",
            #   y2(),
            #   ": ",
            #   point$y,
            #   "</br>",
            #   "<b style = 'color: ",
            #   font_color(point$color),
            #   "'> Factors(",
            #   length(tmp1),
            #   "): ",
            #   paste(
            #     paste0(
            #       tmp1," = ", tmp2
            #     ), collapse = ", "
            #   ),
            #   "</br>"
            # ),
            # paste0(
            #   "<b style = 'color: ",
            #   font_color(point$color),
            #   "'> ",
            #   x(),
            #   ": ",
            #   point$x,
            #   "</br>",
            #   "<b style = 'color: ",
            #   font_color(point$color),
            #   "'> ",
            #   y(),
            #   ": ",
            #   point$y
            # )
            )
          )

         )
    }
    }
    # )
  })

  shiny::observeEvent(input$var3, {
    if (roundDownNice(min(results()$sge[, input$var3], na.rm = TRUE)) <= 0) {
      shiny::updateRadioButtons(
        inputId = "logmosaic",
        label = "Plot Type",
        choices = c(linear = "lin"),
        selected = "lin",
        inline = TRUE
      )
    } else {
      shiny::updateRadioButtons(
        inputId = "logmosaic",
        label = "Plot Type",
        choices = c(linear = "lin", logarithmic = "log"),
        selected = "lin",
        inline = TRUE
      )
    }
  })
}

