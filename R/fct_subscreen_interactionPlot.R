#' Creates an interaction plot used in Explorer and ASMUS-tab in Subgroup Explorer
#'
#' @param df_data data frame with factorial context
#' @param fac1 name of factor level 1
#' @param fac2 name of factor level 2 (default: NULL)
#' @param fac3 name of factor level 3 (default: NULL)
#' @param response target variable
#' @param bg.col background color
#' @param bg.col2 second background color
#' @param font.col font color
#' @param y.min y axis mininum
#' @param y.max y axis maximum
#' @param box.col box color
#' @param sg_green hex code for color palette creation
#' @param sg_blue hex code for color palette creation
#' @param plot_type linear or logarithmic y axis (default: "" (equals linear))
#'
#'

interaction_plot2 <- function (
    df_data,
    fac1,
    fac2 = NULL,
    fac3 = NULL,
    response,
    bg.col ="#6B6B6B",
    bg.col2 = NULL,
    font.col = "white",
    y.min = "NA",
    y.max = "NA",
    box.col = "white",
    sg_green = "#5cb85c",
    sg_blue = "#3a6791",
    plot_type = ""
  ) {

  #create color palette with grey as 'middle' color
  f_col <- grDevices::colorRamp(c(sg_blue, "gray89", sg_green))

  #if limits are missing, calculate from data set
  if (y.min != "NA") {
    v_min <- y.min
  } else if (y.min == "NA") {
    v_min <- min(df_data[response],na.rm=TRUE)
  }
  if (y.max != "NA") {
    v_max <- y.max
  } else if (y.max == "NA") {
    v_max <- max(df_data[response],na.rm=TRUE)
  }

  #assign levels from first (up to third) factor
  # set NULL if factors are not available
  lev1 <- as.character(unique(df_data[, fac1]))
  lev1 <- lev1[lev1 != "Not used"]

  if (!is.null(fac2)) {
    if (!is.na(fac2)) {
      lev2 <- as.character(unique(df_data[, fac2]))
      lev2 <- lev2[lev2 != "Not used"]
    } else {
      fac2 <- NULL
    }
  }

  if (!is.null(fac3)) {
    if (!is.na(fac3)) {
      lev3 <- as.character(unique(df_data[, fac3]))
      lev3 <- lev3[lev3 != "Not used"]
    } else {
      fac3 <- NULL
    }
  }


  # sort factor levels with most levels in case of two (or three) factors
  #  used to create fewer plots than lines
  if (!is.null(fac2) & is.null(fac3)) {
    if (length(lev2) == 1 & length(lev1) != 1) {
      lev2_tmp <- lev2
      fac2_tmp <- fac2
      lev2 <- lev1
      fac2 <- fac1
      lev1 <- lev2_tmp
      fac1 <- fac2_tmp
    }
  }
  if (!is.null(fac2) & !is.null(fac3)) {
    #save current levels temporary
    lev1_tmp <- lev1
    fac1_tmp <- fac1
    lev2_tmp <- lev2
    fac2_tmp <- fac2
    lev3_tmp <- lev3
    fac3_tmp <- fac3
    ord <- order(c(length(lev1),length(lev2),length(lev3)))
    fac3 <- eval(rlang::sym(paste0("fac",ord[1],"_tmp")))
    fac2 <- eval(rlang::sym(paste0("fac",ord[2],"_tmp")))
    fac1 <- eval(rlang::sym(paste0("fac",ord[3],"_tmp")))
    lev3 <- eval(rlang::sym(paste0("lev",ord[1],"_tmp")))
    lev2 <- eval(rlang::sym(paste0("lev",ord[2],"_tmp")))
    lev1 <- eval(rlang::sym(paste0("lev",ord[3],"_tmp")))
  }

  #create plots depending on number of factors:
  if (is.null(fac2) & is.null(fac3)) {
    plot(
      as.numeric(factor(lev1)),
      df_data[[response]],
      type = "b",
      ylim = c(v_min, v_max),
      axes = FALSE,
      log = plot_type
    )

    graphics::rect(
      xleft = graphics::grconvertX(0,'ndc','user') - 1000,
      xright = graphics::grconvertX(1,'ndc','user') + 1000,
      ybottom = graphics::grconvertY(0,'ndc','user'),
      ytop = graphics::grconvertY(1,'ndc','user'),
      border = NA,
      col = bg.col,
      xpd = TRUE
    )

    if (!is.null(bg.col2)) {
      #workaround with rect() since col=bg.col is not working
      graphics::rect(
        xleft = graphics::grconvertX(0,'npc','user'),
        xright = graphics::grconvertX(1,'npc','user'),
        ybottom = graphics::grconvertY(0,'npc','user') ,
        ytop = graphics::grconvertY(1,'npc','user'),
        border = NA,
        col = bg.col2,
        xpd = TRUE
      )
    }

    points(
      as.numeric(factor(lev1)),
      df_data[[response]],
      type = "l",
      ylim = c(v_min, v_max),
      lwd = 3,
      cex = 1.4,
      col = sg_green
    )

    graphics::box(col = box.col)

    axis(
      1,
      at = seq_along(as.numeric(factor(lev1))),
      labels = as.character(factor(lev1)),
      col = font.col,
      col.axis = font.col
    )

    axis(
      2,
      col = font.col,
      col.axis = font.col
    )

    title(ylab = response,
          xlab = fac1,
          col.main = font.col,
          col.lab = font.col
    )
  } else if (!is.null(fac2) & is.null(fac3)) {

    layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE), heights = c(8, 2))
    data_cols <- grDevices::rgb(f_col(seq(0, 1, length = length(lev1))), maxColorValue = 255)

    for (i in 1:length(lev1)) {
      dat <- df_data[df_data[fac1] == lev1[i], ]

      if (i == 1) {
        plot(
          as.numeric(factor(lev2)),
          dat[[response]],
          type = "b",
          ylim = c(v_min, v_max),
          axes = FALSE,
          log = plot_type
        )
        graphics::rect(
          xleft = graphics::grconvertX(0,'ndc','user') - 1000,
          xright = graphics::grconvertX(1,'ndc','user') + 1000,
          ybottom = graphics::grconvertY(0,'ndc','user'),
          ytop = graphics::grconvertY(1,'ndc','user'),
          border = NA,
          col = bg.col,
          xpd = TRUE
        )

        if (!is.null(bg.col2)) {
          graphics::rect(
            xleft = graphics::grconvertX(0, 'npc', 'user'),
            xright = graphics::grconvertX(1, 'npc', 'user'),
            ybottom = graphics::grconvertY(0, 'npc', 'user'),
            ytop = graphics::grconvertY(1, 'npc', 'user'),
            border = NA,
            col = bg.col2,
            xpd = TRUE
          )
        }
      }
      points(
        as.numeric(factor(lev2)),
        dat[[response]],
        type = "l",
        ylim = c(v_min, v_max),
        lwd = 3,
        cex = 1.4,
        col = data_cols[i]
      )

      if (i == 1) {
        graphics::box(col = box.col)

        axis(
          1,
          at = seq_along(as.numeric(factor(lev2))),
          labels = as.character(factor(lev2)),
          col = font.col,
          col.axis = font.col
        )

        axis(
          2,
          col = font.col,
          col.axis = font.col
        )

        title(
          ylab = response,
          xlab = fac2,
          col.main = font.col,
          col.lab = font.col
        )
      }
    }

    graphics::par(mar = c(0, 0, 0, 0))
    plot(
      NULL,
      NULL,
      xlim = c(0,1),
      ylim = c(0,1),
      bg = "grey",
      axes = FALSE
    )
    graphics::rect(
      xleft = graphics::grconvertX(0,'ndc','user') - 1000,
      xright = graphics::grconvertX(1,'ndc','user') + 1000,
      ybottom = graphics::grconvertY(0,'ndc','user'),
      ytop = graphics::grconvertY(1,'ndc','user'),
      border = NA,
      col = bg.col,
      xpd = TRUE
    )
    legend(
      "center",
      legend = paste0(fac1, " = ", lev1),
      col = data_cols,
      lwd = 3,
      horiz = FALSE,
      bg = bg.col2,
      box.col = font.col,
      text.col = font.col
    )

    graphics::par(mfrow = c(1,1), mar = c(5.1, 4.1, 4.1, 2.1))

  } else if (!is.null(fac2) & !is.null(fac3)) {
    data_cols <- grDevices::rgb(f_col(seq(0, 1, length = length(lev1))), maxColorValue = 255)
    graphics::layout(matrix(c(1:length(lev3),rep(length(lev3)+1,length(lev3))),2,length(lev3), byrow=TRUE) , heights = c(8, 2))
    for (j in 1:length(lev3)) {
      df_data_tmp <- df_data[df_data[fac3] == lev3[j], ]
      for (i in 1:length(lev1)) {
        dat <- df_data_tmp[df_data_tmp[fac1] == lev1[i], ]
        if (i == 1) {
          plot(
            as.numeric(factor(lev2)),
            dat[[response]],
            type = "b",
            ylim = c(v_min, v_max),
            axes = FALSE,
            log = plot_type
          )
          graphics::rect(
            xleft = graphics::grconvertX(0,'ndc','user') - 1000,
            xright = graphics::grconvertX(1,'ndc','user') + 1000,
            ybottom = graphics::grconvertY(0,'ndc','user'),
            ytop = graphics::grconvertY(1,'ndc','user'),
            border = NA,
            col = bg.col,
            xpd = TRUE
          )

          if (!is.null(bg.col2)) {
            graphics::rect(
              xleft = graphics::grconvertX(0, 'npc', 'user'),
              xright = graphics::grconvertX(1, 'npc', 'user'),
              ybottom = graphics::grconvertY(0, 'npc', 'user'),
              ytop = graphics::grconvertY(1, 'npc', 'user'),
              border = NA,
              col = bg.col2,
              xpd = TRUE
            )
          }
        }

        if (length(as.numeric(factor(lev2))) == length(dat[[response]])) {
        graphics::points(
          as.numeric(factor(lev2)),
          dat[[response]],
          type = "l",
          ylim = c(v_min, v_max),
          lwd = 3,
          cex = 1.4,
          col = data_cols[i]
        )

      }
        if (i == 1) {
          graphics::box(col = box.col)
          axis(
            1,
            at = seq_along(as.numeric(factor(lev2))),
            labels = as.character(factor(lev2)),
            col = font.col,
            col.axis = font.col
          )
          axis(
            2,
            col = font.col,
            col.axis = font.col
          )

          title(
            main = paste0(fac3, " = ", lev3[j]),
            ylab = response,
            xlab = fac2,
            col.main = font.col,
            col.lab = font.col
          )
        }
      }
    }
    graphics::par(mar = c(0, 0, 0, 0))
    plot(
      NULL,
      NULL,
      xlim = c(0,10),
      ylim = c(0,1),
      bg = "grey",
      axes = FALSE
    )
    graphics::rect(
      xleft = graphics::grconvertX(0,'ndc','user') - 1000,
      xright = graphics::grconvertX(1,'ndc','user') + 1000,
      ybottom = graphics::grconvertY(0,'ndc','user'),
      ytop = graphics::grconvertY(1,'ndc','user'),
      border = NA,
      col = bg.col,
      xpd = TRUE
    )
    legend(
      "center",
      legend = paste0(fac1, " = ", lev1),
      col = data_cols,
      lwd = 3,
      bg = bg.col2,
      box.col = font.col,
      text.col = font.col,
      horiz = FALSE
    )
    graphics::par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))
  }
}

