#' #' sg_explorer_plot (created with ggplot2)
#' #'
#' #' @param data results data
#' #' @param x
#' #' @param y
#' #' @param key
#' #' @param factorial_context_data
#' #' @param parents_data
#' #'
#' #' @param title
#' #' @param subtitle
#' #' @param ylim
#' #' @param xlim
#' #' @param y_scale
#' #' @param add_grid
#' #' @param number_stripes
#' #' @param point_color
#' #' @param bg_color
#' #' @param ref_line_color
#' #' @param text_color
#' #' @param click_color
#' #' @param select_color
#' #' @param factorial_context_color
#' #' @param parents_color
#' #' @param filter_color
#' #' @param variable_importance_color
#' #'
#'
#' sg_explorer_plot <- function(
#'   data,
#'   x = "N.of.subjects",
#'   y,
#'   key,
#'   click_points_data = NULL,
#'   select_points_data = NULL,
#'   factorial_context_data = NULL,
#'   parents_data = NULL,
#'   #title = NULL,
#'   subtitle = NULL,
#'   ylim = NULL,
#'   xlim = NULL,
#'   ref_line = NULL,
#'   point_size = 2,
#'   filter_variable_selected = NULL,
#'   filter_variable = NULL,
#'   y_scale = "lin",
#'   add_grid = FALSE,
#'   number_stripes = 7,
#'   pch = 19,
#'   circlestyle = "standard",
#'   point_color = "#FFFFFF",
#'   bg_color = "#383838",
#'   ref_line_color = "#0091DF",
#'   text_color = "white",
#'   click_color = "red",
#'   select_color = "gold3",
#'   factorial_context_color = "blue",
#'   parents_color = "orange",
#'   filter_color = "green",
#'   variable_importance_color = "purple"
#' ) {
#'
#'     if (key[1] == key[2]) {
#'       title <- paste(key[1], "-Factorial Subgroups (", length(data$x), ")", sep = "")
#'     } else {
#'       title <- paste(key[1], " to ", key[2], "-Factorial Subgroups (", length(data$x), ")", sep = "")
#'     }
#'   # filter
#'   data$sge <- data$sge[data$sge$nfactors >= key[1] & data$sge$nfactors <= key[2],]
#'   #complete data :
#'   data$sge <- data$sge[complete.cases(data$sge[[y]]),]
#'   minix <- roundDownNice(min(data$sge[[x]], na.rm=TRUE))
#'   maxix <- roundUpNice(max(data$sge[[x]], na.rm=TRUE))
#'
#'   stepx <- roundUpNice((maxix - minix)/(number_stripes +  1))
#'
#'     if (minix < stepx)
#'     minix <- 0
#'     stripesx <- 0:(number_stripes + 1)
#'     stripesx <- lapply(stripesx, function(x) x * stepx)
#'     stripesx <- lapply(stripesx, function(x) x + minix)
#'     stripesxp <- lapply(stripesx, function(x1) paste0(floor(x1/data$results_total[[x]] * 100), "%"))
#'
#'
#'     odd_list <- unlist(stripesx)[seq(2,length(stripesx),by = 2)]
#'
#'     points_col <- c(
#'         grDevices::adjustcolor(point_color, alpha = 1),
#'         grDevices::adjustcolor(point_color, alpha = 0.75),
#'         grDevices::adjustcolor(point_color, alpha = 0.5),
#'         grDevices::adjustcolor(point_color, alpha = 0.25),
#'         grDevices::adjustcolor(point_color, alpha = 0.1),
#'         grDevices::adjustcolor(point_color, alpha = 0.1),
#'         grDevices::adjustcolor(point_color, alpha = 0.1),
#'         grDevices::adjustcolor(point_color, alpha = 0.1)
#'     )
#'
#'     rect_data <- data.frame(
#'       xmin = unlist(stripesx)[seq(1,length(stripesx),by = 2)[1:length(odd_list)]],
#'       xmax = odd_list,
#'       ymin = ylim[1]+ylim[1]*0.5,
#'       ymax = ylim[2]+ylim[2]*0.5
#'     )
#'
#'     size_tmp <- ifelse(circlestyle == "standard", "point_size", "N.of.subjects")
#'
#'   p <- ggplot2::ggplot(
#'     data$sge,
#'     ggplot2::aes(
#'       x = !!rlang::sym(x),
#'       y = !!rlang::sym(y),
#'       colour = factor(nfactors),
#'       size = !!rlang::sym(size_tmp)
#'     )
#'   ) +
#'   #density() + scale_color_viridis_c()
#'   # stat_density_2d(aes(fill = stat(density)), geom = 'raster', contour = FALSE) +
#'   #       scale_fill_viridis_c() +
#'   #       coord_cartesian(expand = FALSE) +
#'   #      #geom_point(shape = '.', col = 'white')
#'   ggplot2::scale_x_continuous(
#'     labels = function(x) {
#'       paste0(x, " (", stripesxp, ")")
#'     },
#'     breaks = unlist(stripesx)
#'   ) +
#'     ggplot2::theme_classic() +
#'      scale_y_continuous(
#'       limits = ylim,
#'       trans = ifelse(y_scale == "lin", "identity","log10")) +
#'     ggplot2::geom_rect(
#'       data = rect_data,
#'       mapping = aes(xmin = xmin, xmax = xmax),
#'       ymin = ifelse(y_scale == "lin", ylim[1], log(ylim[1])),
#'       ymax = ifelse(y_scale == "lin", ylim[2], log(ylim[2])),
#'       colour = bg_color,
#'       size = 0.5,
#'       alpha = 0.3,
#'       inherit.aes = FALSE
#'     ) +
#'      ggplot2::labs(
#'       title = title,
#'       subtitle = subtitle
#'     ) +
#'     #scale_x_continuous(limits = xlim) +
#'      #scale_y_continuous(trans='log2')
#'
#'     ggplot2::scale_color_manual(values = points_col) +
#'     ggplot2::geom_point(
#'       col = 'white'
#'     ) +
#'     ggplot2::theme(
#'       # panel.grid.minor = ifelse(add_grid == TRUE, element_line(colour = "white", size=0.2), element_blank()),
#'       # panel.grid.major = ifelse(add_grid == TRUE, element_line(colour = "white", size=0.1), element_blank()),
#'       panel.background = element_rect(color = NA, fill = NA),
#'       axis.title = element_blank(),
#'       panel.border = element_blank(),
#'       axis.text = element_text(color = "white", size = 10),
#'       axis.line = element_line(color = "white"),
#'       plot.background = element_rect(fill = bg_color),
#'       legend.background = element_rect(fill = bg_color),
#'       legend.position = "none",
#'       plot.title = element_text(size = 18, face = "bold", color = "white"),
#'       plot.subtitle = element_text(size = pch, color = "white")
#'     ) +
#'     ggplot2::geom_hline(
#'       yintercept = ref_line,
#'       color = ref_line_color
#'     ) +
#'     ggplot2::geom_text(
#'       label = ref_line,
#'       x = 400,
#'       y = ref_line - diff(ylim)/50,
#'       color = ref_line_color
#'     ) # +
#'     #scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#999999", "#E69F00", "#56B4E9"))
#'
#'    if (add_grid) {
#'      p <- p +
#'       ggplot2::theme(
#'         panel.ontop = TRUE,
#'         panel.grid.minor = element_line(colour = "white", size=0.2),
#'         panel.grid.major = element_line(colour = "white", size=0.1)
#'       )
#'    }
#'
#'     if (!is.null(click_points_data)) {
#'      if (dim(click_points_data)[1] > 0) {
#'      p <- p + geom_point(
#'        data = click_points_data,
#'        aes(x = !!rlang::sym(x), y = !!rlang::sym(y)),
#'        color = click_color,
#'        size = point_size + point_size*0.1,
#'        pch = pch
#'        )
#'      }
#'     }
#'      if (!is.null(factorial_context_data)) {
#'        if (dim(factorial_context_data$Factorial)[1] > 0) {
#'      p <- p + geom_point(data = factorial_context_data$Factorial,
#'        aes(x = !!rlang::sym(x), y = !!rlang::sym(y)),
#'        color = ifelse(factorial_context_data$Status == "Complete", "darkblue","cyan"),
#'        size = point_size + point_size*0.1
#'       )
#'        }
#'      }
#'
#'    if (!is.null(parents_data) ) {
#'
#'     if(dim(parents_data$Parents)[1] > 0) {
#'      p <- p + geom_point(
#'        data = parents_data$Parents,
#'        aes(
#'          x = !!rlang::sym(x),
#'          y = !!rlang::sym(y)
#'         ),
#'         size = point_size,
#'         color = parents_color
#'       )
#'     }
#'   }
#'
#'   if (filter_variable_selected != "no selection") {
#'     p <- p + geom_point(
#'      data = data$sge[data$sge[filter_variable_selected] == filter_variable,],
#'      aes(
#'       x = !!rlang::sym(x),
#'        y = !!rlang::sym(y)
#'       ),
#'       size = point_size,
#'       color = filter_color
#'     )
#'   }
#'
#'
#'   if (!is.null(select_points_data)) {
#'      if (dim(select_points_data)[1] > 0) {
#'      p <- p + geom_point(
#'        data = select_points_data,
#'        aes(x = !!rlang::sym(x), y = !!rlang::sym(y)),
#'        color = select_color,
#'        size = point_size + point_size*0.1,
#'        pch = pch
#'        )
#'      }
#'     }
#'   p
#'
#' }
