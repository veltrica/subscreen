#' bubble UI Function
#'
#' @description A shiny Module for the bubble plot in Subscreen.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bubble_ui <- function(id, plotHeight, plotWidth) {
  ns <- NS(id)
  tagList(
    shiny::div(style = "position:relative",
      shiny::plotOutput(
        outputId = ns("bubble"),
        click = ns("plot_click"),
        hover = hoverOpts(
          ns("plot_hover"),
          delay = 300,
          delayType = "debounce"
        ),
        height = plotHeight,
        width = plotWidth
      ),
      shiny::uiOutput(ns("hover_info"))
    )
  )

}

#' bubble Server Function
#'
#' @noRd
mod_bubble_server <- function(input, output, session,
    results = scresults,
    plot_point,
    XRange = input$YRange,
    YRange = input$YRange,
    plot_type = input$plot_type,
    plot_type2 = input$plot_type,
    point_size = input$pointsize,
    pch_value = input$pch_value,
    color,
    ColorBGplot = ColorBGplot(),
    ColorClicked = colthemeCol$ColorClicked,
    ColorTabClicked = colthemeCol$ColorTabClicked,
    ColorReference = colthemeCol$ColorReference,
    ColorPoints = colthemeCol$ColorPoints,
    VarChosen = input$VarChosen,
    x = input$x,
    y = input$y,
    y2 = input$y2,
   # SG_tit = SG_tit(),
    plot_points_data_complement,
    key = input$key,
    nice_Numbers,
    xlabel = input$xlabel,
    circlestyle = input$circlestyle,
    grid = input$grid,
    pickradius = input$pickradius
  ) {

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

  ns <- session$ns


  output$bubble <- shiny::renderPlot({

    # if logarithmic x-axis is selected
    if (plot_type() == "lin" & plot_type2() == "lin") {
      graphics::par(
        oma = c(0, 0, 0, 0),
        mar = c(3, 3, 1, 1),
        bg = ColorBGplot()
      )

      plot(
        x = 0,
        y = 0,
        xlim = XRange(),
        ylim = YRange(),
        xlab = '',
        ylab = '',
        type = 'n',
        axes = FALSE,
        log = ""
      )

      graphics::rect(
        xleft = graphics::grconvertX(0,'ndc','user'),
        xright = graphics::grconvertX(1,'ndc','user'),
        ybottom = graphics::grconvertY(0,'ndc','user') ,
        ytop = graphics::grconvertY(1,'ndc','user') ,
        border = NA,
        col = ColorBGplot(),
        xpd = TRUE
      )

      if (key()[1] == key()[2]) SG_tit <- paste(key()[1], "-Factorial Subgroups (", length(plot_point()$x), ")", sep = "")
      else SG_tit <- paste(key()[1], " to ", key()[2], "-Factorial Subgroups (", length(plot_point()$x), ")", sep = "")

      suppressWarnings(
        graphics::symbols(
          main = SG_tit,
          col.main = font_color(ColorBGplot()),
          x = plot_point()$x,
          y = plot_point()$y,
          circles = sqrt((results()$sge[, c('N.of.subjects')][results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]] )/ pi ),
          inches = 1/3,
          xlim = XRange(),
          ylim = YRange(),
          fg = "grey",
          bg = color(),
          log = "",
          add = TRUE
        )
      )
    }

    if (plot_type() == "log" & plot_type2() == "lin") {
      graphics::par(
        oma = c(0, 0, 0, 0),
        mar = c(3, 3, 1, 1),
        bg = ColorBGplot()
      )

      plot(
        x = 1,
        y = 0,
        xlim = XRange(),
        ylim = YRange(),
        xlab = '',
        ylab = '',
        type = 'n',
        axes = FALSE,
        log = "x"
      )

      graphics::rect(
        xleft = graphics::grconvertX(0,'ndc','user') - ifelse(plot_type() == "lin", 1000, 0),
        xright = graphics::grconvertX(1,'ndc','user') + ifelse(plot_type() == "lin", 1000, 0),
        ybottom = graphics::grconvertY(0,'ndc','user') - ifelse(plot_type() == "lin", 1000, 0),
        ytop = graphics::grconvertY(1,'ndc','user') + ifelse(plot_type() == "lin", 1000, 0),
        border = NA,
        col = ColorBGplot(),
        xpd = TRUE
      )

      suppressWarnings(
        graphics::symbols(
          main = SG_tit(),
          col.main = font_color(ColorBGplot()),
          x = plot_point()$x,
          y = plot_point()$y,
          circles = sqrt(( results()$sge[,c('N.of.subjects')][results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]] )/ pi),
          inches = 1/3,
          xlim = XRange(),
          ylim = YRange(),
          fg = "grey",
          bg = color(),
          log = "x",
          add = TRUE
        )
      )
    }

    if (plot_type() == "lin" & plot_type2() == "log") {
      graphics::par(
        oma = c(0, 0, 0, 0),
        mar = c(3, 3, 1, 1),
        bg = ColorBGplot()
      )

      plot(
        x = 0,
        y = 1,
        xlim = XRange(),
        ylim = YRange(),
        xlab = '',
        ylab = '',
        type = 'n',
        axes = FALSE,
        log = "y"
      )

      graphics::rect(
        xleft = graphics::grconvertX(0,'ndc','user'),
        xright = graphics::grconvertX(1,'ndc','user'),
        ybottom = graphics::grconvertY(0,'ndc','user'),
        ytop = graphics::grconvertY(1,'ndc','user'),
        border = NA,
        col = ColorBGplot(),
        xpd = TRUE
      )

      suppressWarnings(
        graphics::symbols(
          main = SG_tit(),
          col.main = font_color(ColorBGplot()),
          x = plot_point()$x,
          y = plot_point()$y,
          circles = sqrt(( results()$sge[,c('N.of.subjects')][results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]] )/ pi ),
          inches = 1/3,
          xlim = XRange(),
          ylim = YRange(),
          fg = "grey",
          bg = color(),
          log =  "y",
          add = TRUE
        )
      )
    }

    if (plot_type() == "log" & plot_type2() == "log") {
      graphics::par(
        oma = c(0, 0, 0, 0),
        mar = c(3, 3, 1, 1),
        bg = ColorBGplot()
      )

      plot(
        x = 1,
        y = 1,
        xlim = XRange(),
        ylim = YRange(),
        xlab = '',
        ylab = '',
        type = 'n',
        axes = FALSE,
        log = "yx"
      )

      graphics::rect(
        xleft = graphics::grconvertX(0, 'ndc', 'user'),
        xright = graphics::grconvertX(1, 'ndc', 'user'),
        ybottom = graphics::grconvertY(0, 'ndc', 'user'),
        ytop = graphics::grconvertY(1, 'ndc', 'user'),
        border = NA,
        col = ColorBGplot(),
        xpd = TRUE
      )

      suppressWarnings(
        graphics::symbols(
          main = SG_tit(),
          col.main = font_color(ColorBGplot()),
          x = plot_point()$x,
          y = plot_point()$y,
          circles = sqrt((results()$sge[, c('N.of.subjects')][results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]] )/ pi),
          inches = 1/3,
          xlim = XRange(),
          ylim = YRange(),
          fg = "grey",
          bg = color(),
          log = "yx",
          add = TRUE
        )
      )
    }

    graphics::box(col = font_color(ColorBGplot()))
    axis(
      1,
      col = font_color(ColorBGplot()),
      col.ticks = font_color(ColorBGplot()),
      col.axis = font_color(ColorBGplot()),
      cex.axis = 1
    )
    axis(
      2,
      col = font_color(ColorBGplot()),
      col.ticks = font_color(ColorBGplot()),
      col.axis = font_color(ColorBGplot()),
      cex.axis = 1
    )
    graphics::mtext(
      text = y(),
      side = 1,
      line = 3,
      col = font_color(ColorBGplot()),
      cex = 1
    )

    graphics::mtext(
      text = y2(),
      side = 2,
      line = 3,
      col = font_color(ColorBGplot()),
      cex = 1
    )
  })



  #click event handler
  click_points_data <- shiny::reactiveValues(xy = data.frame(x = NULL, y = NULL))

  shiny::observeEvent(c(input$plot_click), {
    curr_x <- shiny::req(x())
    start_radius <- pickradius()

    clicked <- shiny::nearPoints(
      results()$sge[which(results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]),],
      input$plot_click,
      xvar = y(),
      yvar = y2(),
      threshold = start_radius,
      maxpoints = NULL
    )

    clicked <- subset(
      clicked,
      select = c("SGID", x = curr_x, y = y(), "nfactors", results()$factors)
    )

    click_points_data$xy <- clicked[, unlist(lapply(clicked, function(x) !all(is.na(x))))]
  })




  ## hover information window
  output$hover_info <- shiny::renderUI({
    shiny::req(input$plot_hover, plot_point())

    input$plot_hover
    all_points <- cbind(plot_point(), color(), stringsAsFactors = FALSE)
    colored_points <- all_points#[!startsWith(all_points$color, ColorPoints()),  ]

    hover <- input$plot_hover
    hover$mapping <- list(xintercept = "xintercept", x = "x", y = "y")

    point <- nearPoints(colored_points, hover)

    if (nrow(point) == 0) return(NULL)

      left_pct <- (hover$coords_img$x - hover$range$left) / (hover$range$right - hover$range$left)
      #left_pct <- (hover$coords_img$x - hover$range$left) / (hover$domain$right - hover$domain$left)

      top_pct <- (hover$domain$top - ifelse(plot_type() == "lin", hover$y, log10(hover$y))) / (hover$domain$top - hover$domain$bottom)

      # left_px <- ifelse(left_pct <= 0.75,
      #                   20 + hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x,
      #                   - 175 + hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x)
      left_px <- (hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x) + 3
      top_px <- (hover$range$top + top_pct * (hover$range$bottom - hover$range$top) / hover$img_css_ratio$y) + 3


    style <- paste0("position:absolute; z-index:100;background-color: rgba(",
      grDevices::col2rgb(point$color)[1],",",grDevices::col2rgb(point$color)[2],",",grDevices::col2rgb(point$color)[3],",0.85); ",
                    "left:", left_px, "px; top:", top_px, "px; border: 0px;")
    point <- point[1,]

    tmp1 <- colnames(results()$sge[which(results()$sge$SGID == point$ID), results()$factors])[which(results()$sge[which(results()$sge$SGID == point$ID), results()$factors] != "Not used")]

    tmp2 <- results()$sge %>%
      dplyr::filter(SGID %in% point$ID) %>%
      dplyr::select(colnames(results()$sge[which(results()$sge$SGID == point$ID), results()$factors])[which(results()$sge[which(results()$sge$SGID == point$ID), results()$factors] != "Not used")])

    tmp2 <- data.frame(lapply(tmp2, as.character), stringsAsFactors = FALSE)

    shiny::wellPanel(
      style = style,
     shiny::p(
        shiny::HTML(
          ifelse(length(tmp1)>0,
          paste0(
            "<b style = 'color: ",
            font_color(point$color),
            "'> ",
            y(),
            ": ",
            point$x,
            "</br>",
            "<b style = 'color: ",
            font_color(point$color),
            "'> ",
            y2(),
            ": ",
            point$y,
            "</br>",
            "<b style = 'color: ",
            font_color(point$color),
            "'> Factors(",
            length(tmp1),
            "): ",
            paste(
              paste0(
                tmp1," = ", tmp2
              ), collapse = ", "
            ),
            "</br>"
          ),
          paste0(
            "<b style = 'color: ",
            font_color(point$color),
            "'> ",
            x(),
            ": ",
            point$x,
            "</br>",
            "<b style = 'color: ",
            font_color(point$color),
            "'> ",
            y(),
            ": ",
            point$y
          )
          )
        )
      )
    )
  })

 return(
    list(
      clicked_points = reactive({ click_points_data$xy })
    )
  )

}
