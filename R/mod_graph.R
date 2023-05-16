#' graph UI Function
#'
#' @description A shiny Module for the graph in Subscreen.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_graph_ui <- function(id, plotHeight, plotWidth) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(style = "position:relative",
      shiny::plotOutput(
        outputId = ns("graph"),
        click = ns("plot_click"),
        hover = hoverOpts(
          ns("plot_hover"),
          delay = 50,
          delayType = "debounce"
        ),
        height = plotHeight,
        width = plotWidth
      ),
      shiny::uiOutput(ns("hover_info"))
    )
  )
}

#' graph Server Function
#'
#' @noRd

mod_graph_server <- function(
  input,
  output,
  session,
  results = scresults,
  plot_point,
  YRange = input$YRange,
  XRange = input$XRange,
  plot_type = input$plot_type,
  point_size = input$pointsize,
  pch_value = input$pch_value,
  color,
  ColorBGplot = colthemeCol$ColorBGplot,
  ColorClicked = colthemeCol$ColorClicked,
  ColorTabClicked = colthemeCol$ColorTabClicked,
  ColorReference = colthemeCol$ColorReference,
  ColorPoints = colthemeCol$ColorPoints,
  ColorMemorized = colthemeCol$ColorMemorized,
  VarChosen = input$VarChosen,
  click_points_data = NULL,
  select_points_data = NULL,
  factorial_context_data = NULL,
  parents_data = NULL,
  x = input$x,
  y = input$y,
  key = input$key,
  nice_Numbers,
  xlabel = input$xlabel,
  circlestyle = input$circlestyle,
  grid = input$grid,
  pickradius = input$pickradius,
  memorized_Data = df_m$data,
  memorized_labels_on_off = shiny::reactive({FALSE}),
  subTitle = NULL,
  plot_points_data_complement = NULL,
  remove_levels,
  CI = "none", # "par" / "nonpar",
  alpha = 0.05,
  span = 0.25,
  show_points = FALSE,
  values = NULL
  ) {

  ns <- session$ns

  #### graph ####
  output$graph <- shiny::renderPlot({
    #require an not empty y-range value
    if (!is.null(YRange())) {
      #make reactive for all colors
      color()

      # create the title with subgroup and factor level information
      if (key()[1] == key()[2]) {
        title <- paste(key()[1], "-Factorial Subgroups (", length(results()$sge[[x()]]), ")", sep = "")
      } else {
        title <- paste(key()[1], " to ", key()[2], "-Factorial Subgroups (", length(results()$sge[[x()]]), ")", sep = "")
      }

      # filter data by factor levels
      data <- results()$sge[results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2],]

      # add column for color information
      data$point_color <- color()[1:dim(data)[1]]

      #add column with memorized subgroup label ("" for no label as default)
      data$memorizedText <- ""

      if (dim(memorized_Data())[1] > 0) {
        tmp <- memorized_Data()
        tmp$memorizedText <- ""
        for (i in 1:dim(memorized_Data())[1]) {
          tmp[i,"memorizedText"] <- paste0(
            "N: ", tmp[i,x()], "\n",
            y(), ": ", tmp[i,y()],"\n",
            paste(
              names(
                tmp[,results()$factors]
              )[tmp[i,results()$factors]!="Not used"],
              ":",
              tmp[i,results()$factors][tmp[i,results()$factors]!="Not used"],
              collapse = "\n")
          )
        }
        data <- merge(tmp[,-1], data, all = TRUE)
      }

      # check for completeness of the data
      if (y() %in% colnames(data)) {
        data <- data[complete.cases(data[[y()]]),]

        minix <- roundDownNice(min(data[[x()]], na.rm=TRUE),nice = nice_Numbers)
        maxix <- roundUpNice(max(data[[x()]], na.rm=TRUE),nice = nice_Numbers)

        stepx <- roundUpNice((maxix - minix)/8, nice = nice_Numbers)

        # set stripes (8 stripes) values
        stripesxp <- stripesx <- (0:10 * stepx) #+ minix

        #add percentage values if x is Number of subjects
        if (x() == "N.of.subjects" && xlabel()) {
          tot <- results()$results_total[["N.of.subjects"]]
          perc <- round((stripesx / tot )*100)
          perc[perc <= 0 | perc >100] <- ""
          stripesxp <- paste0(stripesx, " (",perc,"%)")
          stripesxp <- stripesxp %>%
            stringr::str_remove(pattern = "[(]%[)]")
        }

        XRange <- XRange()
        if (!is.null(XRange)) {
          index <- stripesx >= XRange[1] & stripesx <= XRange[2]
          stripesx <- stripesx[index]
          stripesxp <- stripesxp[index]
        }

        if (length(stripesx) >= 2) {
          odd_list <- stripesx[seq(2, length(stripesx), by = 2)]
          rect_data <- data.frame(
            xmin = stripesx[seq(1,length(stripesx),by = 2)[1:length(odd_list)]],
            xmax = odd_list
          )
          if(!is.null(XRange)) {
            rect_data <- rect_data %>%
              dplyr::filter(xmin >= XRange[1], xmax <= XRange[2])
          }
        } else {
          rect_data <- NULL
        }

        size_tmp <- ifelse(circlestyle() == "standard", "point_size", "N.of.subjects")

        YRange <- YRange()

        if (!is.null(XRange)) {
          data <- data %>%
            dplyr::filter(
              !!rlang::sym(x()) >= XRange[1] &
              !!rlang::sym(x()) <= XRange[2]
            )
        }
        # filter data by yrange to avoid warnings in console
        data <- data %>%
          dplyr::filter(
            !!rlang::sym(y()) >= YRange[1] &
            !!rlang::sym(y()) <= YRange[2]
          )

      ##########################
      ##test code to use a mixture of contour plots and geom_points (06JUL2022)
      #library(MASS)
      # data_test <<- data
      # x_test <<- x()
      # y_test <<- y()
      #
      # data_tmp <- data[complete.cases(data[,c(x(),y())]),]
      # dens <- kde2d(as.vector(unlist(data_tmp[x()])), as.vector(unlist(data_tmp[y()])), n = 100)
      # # the contours to plot
      # dens_test <<- dens
      # prob <- c(remove_levels())
      # dx <- diff(dens$x[1:2])
      # dy <- diff(dens$y[1:2])
      # sz <- sort(dens$z)
      # c1 <- cumsum(sz) * dx * dy
      # levels <- sapply(prob, function(x) {
      #   approx(c1, sz, xout = 1 - x)$y
      # })
      # ls <- contourLines(dens, level=levels)
      # # inner <- sp::point.in.polygon(x, y, ls[[2]]$x, ls[[2]]$y)
      # out <- sp::point.in.polygon(as.vector(unlist(data_tmp[x()])), as.vector(unlist(data_tmp[y()])), ls[[1]]$x, ls[[1]]$y)
      # data_tmp$belonging <- factor(out)
      # test <<- data_tmp
      # data_tmp <- data_tmp %>%
      #   dplyr::filter(belonging == 0)
      # data2 <- data_tmp
      ##########################

      p <- ggplot2::ggplot(
         data %>% dplyr::arrange(desc(point_color)),
         ggplot2::aes(
           x = !!rlang::sym(x()),
           y = !!rlang::sym(y()),
           label = memorizedText
          )
      )

      if (!is.null(XRange)) {
        p <- p + ggplot2::scale_x_continuous(
          limits = XRange,
          labels = stripesxp,
          breaks = stripesx
        )
      }
      p <- p +
      ggplot2::theme_classic() +
        ggplot2::scale_y_continuous(
          limits = YRange,
          trans = ifelse(plot_type() == "lin", "identity","log10")
        )

      if (!is.null(rect_data)) {
        p <- p +
          ggplot2::geom_rect(
            data = rect_data,
            mapping = ggplot2::aes(xmin = xmin, xmax = xmax),
            ymin = -Inf,
            ymax = Inf,
            colour = ColorBGplot(),
            size = 0.5,
            alpha = 0.3,
            inherit.aes = FALSE
          )
      }

      p <- p +
        ggplot2::labs(
          title = title,
          subtitle = subTitle
        )

      if (circlestyle() == "standard") {
        p <- p +
          ggplot2::geom_point(
            data = data %>% dplyr::arrange(desc(point_color)) %>% dplyr::filter(!is.na(point_color)),
            ggplot2::aes(
              colour = point_color
            ),
            size = point_size(),
            pch = as.numeric(pch_value())
          )
      } else {
        p <- p +
          ggplot2::geom_point(
            data = data %>% dplyr::filter(!is.na(point_color)),
            ggplot2::aes(
              colour = point_color,
              size = !!rlang::sym(x())
            ),
            pch = as.numeric(pch_value())
          )
      }

    if (memorized_labels_on_off()) {
      p <- p +
        ggrepel::geom_text_repel(
          colour = ColorMemorized(), max.overlaps = Inf,
          min.segment.length = 0,
          bg.color = "black",
          bg.r = 0.15
          )
      }
      p <- p + ggplot2::scale_colour_identity() +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(color = NA, fill = NA),
          axis.title = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(color = "white", size = 10),
          axis.line = ggplot2::element_line(color = "white"),
          plot.background = ggplot2::element_rect(fill = ColorBGplot()),
          legend.background = ggplot2::element_rect(fill = ColorBGplot()),
          legend.position = "none",
          plot.title = ggplot2::element_text(size = 18, face = "bold", color = "white"),
          plot.subtitle = ggplot2::element_text(size = 16, color = "white")
        )

      if (YRange[1] <= results()$results_total[, c(y())] &
          YRange[2] >= results()$results_total[, c(y())]) {
        p <- p +
        ggplot2::geom_hline(
          yintercept = results()$results_total[, c(y())],
          color = ColorReference()
        )

        p <- p +
        ggplot2::annotate(geom = "label",
          x = ifelse(!is.null(XRange()),XRange[2],max(rect_data)),
          y = results()$results_total[, c(y())],
          label = results()$results_total[, c(y())],
          color = "white",
          fill = ColorReference()
        )
      }
      ## Confidence Intervals


      if(CI == "nonpar"){

          alphas <- names(quantile(c(0), probs = c(alpha/2, 1-alpha/2)))
          lower <- data.frame(x = as.vector(results()$nsamp), y = as.vector(results()$intervals[alphas[1],]), memorizedText = "")
          upper <- data.frame(x = as.vector(results()$nsamp), y = as.vector(results()$intervals[alphas[2],]), memorizedText = "")

          pred_lower <- predict(loess(y ~ x, lower, span = span)
                                , data.frame(x = results()$sge$N.of.subjects))
          pred_upper <- predict(loess(y ~ x, upper, span = span)
                                , data.frame(x = results()$sge$N.of.subjects))

          lower_full <- data.frame(x = as.vector(results()$sge$N.of.subjects), y = pred_lower, memorizedText = "")
          upper_full <- data.frame(x = as.vector(results()$sge$N.of.subjects), y = pred_upper, memorizedText = "")

          p <- p + ggplot2::geom_line(data = lower_full, aes(x = x, y = y, colour = "black"))
          p <- p + ggplot2::geom_line(data = upper_full, aes(x = x, y = y, colour = "black"))


          if(show_points){
            p <- p + ggplot2::geom_point(data = lower, aes(x = x, y = y, colour = "red"))
            p <- p + ggplot2::geom_point(data = upper, aes(x = x, y = y, colour = "red"))

          }

          # points outside

          below <- pred_lower > values
          above <- pred_upper < values
          outside <- below + above
          ratio <- sum(outside, na.rm = TRUE)/ length(outside)
          print("points outside:")
          print(ratio)



      }
      # for plotting
      g <<- p

      # add grid when option is selected
      if (grid()) {
       p <- p +
        ggplot2::theme(
          panel.ontop = TRUE,
          panel.grid.minor = ggplot2::element_line(colour = "white", size = 0.2),
          panel.grid.major = ggplot2::element_line(colour = "white", size = 0.1)
        )
      }

        if (!is.null(plot_points_data_complement())) {

          if (YRange[1] <= plot_points_data_complement()[[paste0("Complement_", y())]] &
              YRange[2] >= plot_points_data_complement()[[paste0("Complement_", y())]] &
              ifelse(!is.null(XRange()),XRange[2],max(rect_data)) >= plot_points_data_complement()[["N.of.subjects.complement"]]
          ){
            p <- p +
              ggplot2::annotate(
                "point",
                x = plot_points_data_complement()[["N.of.subjects.complement"]],
                y = plot_points_data_complement()[[paste0("Complement_", y())]],
                size =point_size(),
                colour = "yellow",
                pch = 13
              )
          }
        }
        p
      }
    }
  })

  #click event handler
  click_points_data <- shiny::reactiveValues(xy = data.frame(x = NULL, y = NULL))

  shiny::observeEvent(c(input$plot_click), {
    curr_x <- shiny::req(x())
    start_radius <- pickradius()

    clicked <- shiny::nearPoints(
      results()$sge[which(results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]),],
      input$plot_click,
      xvar = curr_x,
      yvar = y(),
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
    all_points <- cbind(plot_point(), color = color(), stringsAsFactors = FALSE)

    colored_points <- all_points#[!startsWith(all_points$color, ColorPoints()),  ]

    tmp_complement <- data.frame(
      plot_points_data_complement()["ID"],
      x = unname(plot_points_data_complement()["N.of.subjects.complement"]),
      y = unname(plot_points_data_complement()[paste0("Complement_",y())]),
      plot_points_data_complement()["color"]
    )

    colored_points <- rbind(colored_points,tmp_complement)

    hover <- input$plot_hover
    hover$mapping <- list(xintercept = "xintercept", x = "x", y = "y")

    point <- nearPoints(colored_points, hover)

    if (nrow(point) == 0) return(NULL)

    left_pct <- (hover$coords_img$x - hover$range$left) / (hover$range$right - hover$range$left)

    top_pct <- (hover$domain$top - ifelse(plot_type() == "lin", hover$y, log10(hover$y))) / (hover$domain$top - hover$domain$bottom)

    left_px <- (hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x) + 3
    top_px <- (hover$range$top + top_pct * (hover$range$bottom - hover$range$top) / hover$img_css_ratio$y) + 3

    style <- paste0(
      "position:absolute;
      z-index:100;background-color: rgba(",
        grDevices::col2rgb(point$color)[1],",",
        grDevices::col2rgb(point$color)[2],",",
        grDevices::col2rgb(point$color)[3],",0.85); ",
        "left:", left_px, "px; top:", top_px, "px; border: 0px;"
    )
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
              "<b style = 'font-size: 12px; color: ",
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
      clicked_points = shiny::reactive({click_points_data$xy }),
      plot_click = shiny::reactive({input$plot_click})
    )
  )
}
