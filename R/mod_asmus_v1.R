# # User interface part of module 'mod_old_asmus'
# # used in R shiny App 'Subgroup Explorer' in R package 'subscreen'
# # Last update: 2021-04-22
#
# #### SCREENING MODULE USER INTERFACE ####
# screeningModule_UI <- function(id) {
#   ns <- shiny::NS(id)
#   shiny::tagList(
#     shinyjs::useShinyjs(debug = TRUE),
#     shiny::column(4,
#       shiny::uiOutput(ns("screening_size"))
#     ),
#     shiny::column(4,
#       shiny::uiOutput(ns("screening_satis"))
#     )
#   )
# }
#
# #### SCREENING MODULE SERVER ####
# screeningModule_Server <- function(input, output, session, label, module_input) {
#   shiny::observe({module_input})
#   shiny::observe({label})
#   output$screening_size <- shiny::renderUI({
#     shinyWidgets::prettyRadioButtons(
#       inputId = session$ns("screening_size"),
#       label = "Is the Subgroup size big enough?",
#       choices = c("No", "N/A", "Yes"),
#       selected = module_input[which(rownames(module_input) == paste0("Subgroup ID: ", label)), 1],
#       inline = TRUE,
#       status = "success",
#       icon = icon("check-circle")
#     )
#   })
#
#   output$screening_satis <- shiny::renderUI({
#     shinyWidgets::prettyRadioButtons(
#       inputId = session$ns("screening_satis"),
#       label = "Is the effect remarkable?",
#       choices = c("No","N/A", "Yes"),
#       selected = module_input[which(rownames(module_input) == paste0("Subgroup ID: ", label)), 2],
#       inline = TRUE,
#       status = "success",
#       icon = icon("check-circle")
#     )
#   })
#   return(
#     list(
#       size = shiny::reactive({input$screening_size}),
#       satis = shiny::reactive({input$screening_satis}),
#       label = shiny::reactive({label})
#     )
#   )
# }
#
#
# asmus_old_module_ui <- function(id, results = scresults) {
#   ns <- shiny::NS(id)
#   shiny::tagList(
#
#     shiny::fluidRow(
#       shiny::column(8,
#         shiny::HTML(
#           "<h3> <b style='color: #e2b007'>A</b>utomatic <b style='color: #e2b007'>S</b>creening of one- or <b style='color: #e2b007'>MU</b>lti-factorial <b style='color: #e2b007'>S</b>ubgroups - <b style='color: #e2b007'>ASMUS</b> </h3>"
#         )
#       ),
#       shiny::column(1,
#        shiny::tags$style(".btn-custom {background-color: #e2b007; color: #FFF;}"),
#         ####... 67. mydropdown_bgcolor (uiOutput)####
#         shiny::uiOutput('mydropdown_bgcolor'),
#         ####... 68. MyDropDown (dropdownButton)####
#         shinyWidgets::dropdownButton(
#           inputId = ns("MyDropDown"),
#           shiny::tags$h3("Settings"),
#           ####... 69. plot_type_asmus (uiOutput)####
#           shiny::uiOutput(ns("plot_type_asmus")),
#           ####... 70. yrange_asmus (uiOutput)####
#           shiny::uiOutput(ns('yrange_asmus')),
#           ####... 71. keys_asmus (sliderInput)####
#           shiny::sliderInput(
#             inputId = ns("keys_asmus"),
#             label = "Subgroup level(s)",
#             min = results$min_comb,
#             max = results$max_comb,
#             ticks = FALSE,
#             value = c(1, min(c(3, results$max_comb), na.rm = TRUE)),
#             step = 1
#           ),
#           ####... 72. y_Interaction_Button2 (uiOutput)####
#           shiny::uiOutput(ns('y_Interaction_Button2')),
#           circle = TRUE,
#           status = "custom",
#           icon = icon("gear"),
#           width = "300px",
#           tooltip = tooltipOptions(title = "Click to see inputs!")
#         )
#       ),
#       shiny::column(3,
#         shiny::tags$style(".btn-custom {background-color: #e2b007; color: #FFF;}"),
#         ####... 73. mydropdown_bgcolor2 (uiOutput)####
#         shiny::uiOutput(ns('mydropdown_bgcolor2')),
#         ####... 74. MyDropDown2 (dropdownButton)####
#         shinyWidgets::dropdownButton(
#           inputId = ns("MyDropDown2"),
#           shiny::tags$h4("About ASMUS:"),
#           shiny::tags$h5(shiny::tags$b(shiny::tags$u("When is a subgroup interesting?"))),
#           "If the treatment effect is remarkable or noticeable and if the size of the subgroup is not too small to give a reliable estimate",
#           shiny::tags$h5(shiny::tags$b(shiny::tags$u("What is a factorial context?"))),
#           "For a subgroup defined by one factor the context consists of all levels of that factor. For multi-factorial subgroups the context is the set of all combinations of levels of the respective factors.",
#           shiny::tags$h5(shiny::tags$b(shiny::tags$u("Completeness of factorial contexts"))),
#           "Complete factorial context: For all possible factor level combinations there is an estimate",
#           "Incomplete factorial context: For at least one factor level combination no estimate is available",
#           "Pseudo(-complete) factorial context: An incomplete factorial context that can be made complete ignoring certain factor levels",
#           circle = TRUE,
#           status = "custom",
#           icon = icon("info"),
#           width = "300px",
#           tooltip = tooltipOptions(title = "Click to see further Information!")
#         )
#       )
#     ),
#     shiny::fluidRow(
#       shiny::column(7,
#         ####... 75. graph5 (plotOutput)####
#         shiny::div(style = "position:relative",
#           shiny::plotOutput(
#             outputId = ns("graph5"),
#             hover = hoverOpts(ns("plot_hover5"), delay = 300, delayType = "debounce")
#           ),
#           ####... 76. hover_info5 (uiOutput)####
#           shiny::uiOutput(ns("hover_info5"))
#         )
#       ),
#       shiny::column(5,
#        ####... 77. interaction2 (plotOutput)####
#        shiny::plotOutput(outputId = ns('interaction2'))
#       )
#     ),
#     ####... 78. legend4 (uiOutput)####
#     shiny::uiOutput(ns('legend4')),
#     shiny::fluidRow(
#       shiny::wellPanel(class = "myclass10", id = "myid10",
#         ####... 79. cont_well3 (uiOutput)####
#         shiny::uiOutput('cont_well10'),
#         shiny::fluidRow(
#           shiny::column(1,
#             shiny::column(6,
#               ####... 80. screening_backward (circleButton)####
#               shinyWidgets::circleButton(
#                 inputId = ns("screening_backward"),
#                 icon = icon("step-backward"),
#                 size = "sm",
#                 status = "default"
#               )
#             ),
#             shiny::column(6,
#               ####... 81. screening_forward (circleButton)####
#               shinyWidgets::circleButton(
#                 inputId = ns("screening_forward"),
#                 icon = icon("step-forward"),
#                 size = "sm",
#                 status = "default"
#               )
#             )
#           ),
#           shiny::column(2,
#             ####... 82. header1 (uiOutput)####
#             shiny::uiOutput(ns('header1')),
#             ####... 83. header2 (uiOutput)####
#             shiny::uiOutput(ns('header2'))
#           ),
#           ####... 84. screening_ui (uiOutput)####
#           shiny::column(8,
#             shiny::uiOutput(ns('screening_ui')),
#             shiny::radioButtons(
#               inputId = ns("direction"),
#               label = "Sorting direction",
#               choices = c("Descending" = "desc",
#                           "Ascending" = "asc"),
#               selected = "desc"
#             )
#           )
#         )
#       )
#     ),
#     shiny::tabPanel("Subgroup Assessment",
#         ####... 85. assessment (dataTableOutput)####
#         DT::dataTableOutput(ns("assessment")),
#         icon = icon("clipboard")
#         ), icon = icon("tasks")
#       )
#     #)
# }
#
# # Server part of module 'mod_asmus'
# asmus_old_module_server <- function(
#     input, output, session,
#     results = scresults,
#     x = input$x,
#     y = input$y,
#     ColorReference = colthemeCol$ColorReference,
#     ColorBGplot = colthemeCol$ColorBGplot,
#     ColorPoints = colthemeCol$ColorPoints,
#     ColorClicked = colthemeCol$ColorClicked,
#     ColorSelected = colthemeCol$ColorSelected,
#     ColorParents = colthemeCol$ColorParents,
#     ColorTabClicked = colthemeCol$ColorTabClicked,
#     ColorImportance = colthemeCol$ColorImportance,
#     ColorFactCont = colthemeCol$ColorFactCont,
#     nice_Numbers = NiceNumbers,
#     ref_line = ref_line(),
#     YRange = input$YRange,
#     xlabel = input$xlabel,
#     circlestyle = input$circlestyle,
#     grid = input$grid,
#     pch_value = input$pch_value
#   ) {
#
#
#   output$y_Interaction_Button2 <- shiny::renderUI({
#     shiny::radioButtons(
#       inputId = 'y_Interaction_Button2',
#       label = 'Synchronise y-axes with main plot',
#       selected = ("Synchron"),
#       choices = c("Synchron", "Optimal"),
#       inline = TRUE
#     )
#   })
#
#   plot_points_data5 <- shiny::reactive({
#     data.frame(
#       x = results$sge[, c(x())][results$sge$nfactors >= input$keys_asmus[1] & results$sge$nfactors <= input$keys_asmus[2]],
#       y = results$sge[, c(y())][results$sge$nfactors >= input$keys_asmus[1] & results$sge$nfactors <= input$keys_asmus[2]],
#       ID = results$sge[, "SGID"][results$sge$nfactors >= input$keys_asmus[1] & results$sge$nfactors <= input$keys_asmus[2]]
#     )
#   })
#
#   SG_tit3 <- shiny::reactive({
#     key <- shiny::req(input$keys_asmus)
#     if (key[1] == key[2])
#       paste(key[1], "-Factorial Subgroups (",
#             length(plot_points_data5()$x), ")", sep = "")
#     else paste(key[1], " to ", key[2], "-Factorial Subgroups (", length(plot_points_data5()$x),
#                ")", sep = "")
#   })
#
#   log_type_asmus <- shiny::reactiveValues(graph5 = '')
#
#   shiny::observeEvent(input$plot_type_asmus, {
#     log_type_asmus$graph5 <- ifelse(input$plot_type_asmus == "log", "y", "")
#   })
#
#   setcolor2 <- function() {
#     if (screening_index$val > 0 ) {
#       f <- results$sge[which(results$sge$nfactors >= input$keys_asmus[1] & results$sge$nfactors <= input$keys_asmus[2]),]
#       p.col <- ColorPoints()
#       bright <- 1
#       f$colour <- as.character(
#         c(
#           grDevices::adjustcolor(p.col, alpha = 1 * bright),
#           grDevices::adjustcolor(p.col, alpha = 0.75 * bright),
#           grDevices::adjustcolor(p.col, alpha = 0.5 * bright),
#           grDevices::adjustcolor(p.col, alpha = 0.25 * bright),
#           grDevices::adjustcolor(p.col, alpha = 0.1 * bright),
#           grDevices::adjustcolor(p.col, alpha = 0.1 * bright),
#           grDevices::adjustcolor(p.col, alpha = 0.1 * bright),
#           grDevices::adjustcolor(p.col, alpha = 0.1 * bright)
#         )
#       )[match(f$nfactors, 1:8)]
#
#       tmp <- factorialContext(results, sorting_index()[screening_index$val])
#       if (all(tmp$Factorial$FCID_incomplete == "Complete")) {
#         f[f$SGID %in% tmp$Factorial$SGID, 'colour'] <- ColorFactCont()
#       } else {
#         f[f$SGID %in% tmp$Factorial$SGID, 'colour'] <- different_hues(ColorFactCont(), value = 89)
#       }
#       f[f$SGID %in% parents(results, sorting_index()[screening_index$val])$Parents$SGID, 'colour'] <- ColorParents()
#       f[f$SGID %in% sorting_index()[screening_index$val], 'colour'] <- ColorTabClicked()
#       color2 <<- f$colour
#     }
#   }
#
#
#   ####... XX. interaction2 ####
#   output$interaction2 <- shiny::renderPlot({
#     shiny::req(screening_index_new$val)
#     if (is.null(input$y_Interaction_Button2)) {
#       ##
#       y_axe <- c(YRange()[1],YRange()[2])
#     } else {
#       if (input$y_Interaction_Button2 == "Synchron") {
#         if (is.null(input$yrange_asmus)) {
#           y_axe <- YRange()
#         } else {
#           y_axe <- input$yrange_asmus
#         }
#       }
#       if (input$y_Interaction_Button2 == "Optimal") {
#         y_axe <- c("NA","NA")
#       }
#     }
#
#     if (is.null(input$plot_type_asmus)) {
#       pl_typ <- "lin"
#     } else {
#       pl_typ <- input$plot_type_asmus
#     }
#     tmp1 <- colnames(
#       results$sge[which(results$sge$SGID == screening_index_new$val), results$factors]
#     )[which(results$sge[which(results$sge$SGID == screening_index_new$val), results$factors] != "Not used")]
#
#     tmp2 <- results$sge %>%
#       dplyr::filter(SGID %in% screening_index_new$val) %>%
#       dplyr::select(colnames(results$sge[which(results$sge$SGID == screening_index_new$val), results$factors])[which(results$sge[which(results$sge$SGID == screening_index_new$val), results$factors] != "Not used")])
#     tmp2 <- data.frame(lapply(tmp2, as.character), stringsAsFactors = FALSE)
#     df_factorial <- factorialContext(results, screening_index_new$val)
#
#     if(is.null(df_factorial$Variables[1]) || is.na(df_factorial$Variables[1])) {
#       plot(
#         NULL,
#         xlim = c(0, 1),
#         ylim = c(0, 1),
#         axes = FALSE,
#         xlab = "",
#         ylab = ""
#       )
#       graphics::rect(
#         xleft = graphics::grconvertX(0, 'ndc', 'user') - 1000,
#         xright = graphics::grconvertX(1, 'ndc', 'user') + 1000,
#         ybottom = graphics::grconvertY(0, 'ndc', 'user') - 1000,
#         ytop = graphics::grconvertY(1,'ndc','user') + 1000,
#         border = NA,
#         col = ColorBGplot(),
#         xpd = TRUE
#       )
#
#       text(
#         0.5,
#         0.5,
#         "Please select a Subgroup!",
#         col = font_color(ColorBGplot()),
#         cex = 1.4
#       )
#       text(
#         0.5,
#         0.4,
#         "(Click on a point in the graphic",
#         col = font_color(ColorBGplot()),
#         cex = 0.9
#       )
#       text(
#         0.5,
#         0.3,
#         "and then select a subgroup in the",
#         col = font_color(ColorBGplot()),
#         cex = 0.9
#       )
#
#       text(
#         0.5,
#         0.2,
#         "'Selected Subgroup'-table by clicking on)",
#         col = font_color(ColorBGplot()),
#         cex = 0.9
#       )
#
#
#     } else if (!is.null(df_factorial$Variables[1]) &
#               !is.na(df_factorial$Variables[1]) &
#               any(is.na(df_factorial$Factorial[y()])) &
#               df_factorial$`Number Factors` <= 3) {
#       plot(
#         NULL,
#         xlim = c(0, 1),
#         ylim = c(0, 1),
#         axes = FALSE,
#         xlab = "",
#         ylab = ""
#       )
#
#       graphics::rect(
#         xleft = graphics::grconvertX(0, 'ndc', 'user') - 1000,
#         xright = graphics::grconvertX(1, 'ndc', 'user') + 1000,
#         ybottom = graphics::grconvertY(0,'ndc','user') - 1000,
#         ytop = graphics::grconvertY(1, 'ndc', 'user') + 1000,
#         border = NA,
#         col = ColorBGplot(),
#         xpd = TRUE
#       )
#       text(
#         0.5,
#         0.5,
#         "Incomplete factorial context!",
#         col = font_color(ColorBGplot()),
#         cex = 1.4
#       )
#       text(
#         0.5,
#         0.4,
#         "(This graphic is not available",
#         col = font_color(ColorBGplot()),
#         cex = 0.9
#       )
#       text(
#         0.5,
#         0.3,
#         "for pseudo factorial contexts)",
#         col = font_color(ColorBGplot()),
#         cex = 0.9
#       )
#
#     } else if (df_factorial$`Number Factors` > 3) {
#       plot(
#         NULL,
#         xlim = c(0, 1),
#         ylim = c(0, 1),
#         axes = FALSE,
#         xlab = "",
#         ylab = ""
#       )
#       graphics::rect(
#         xleft = graphics::grconvertX(0, 'ndc', 'user') - 1000,
#         xright = graphics::grconvertX(1, 'ndc', 'user') + 1000,
#         ybottom = graphics::grconvertY(0, 'ndc', 'user') - 1000,
#         ytop = graphics::grconvertY(1, 'ndc', 'user') + 1000,
#         border = NA,
#         col = ColorBGplot(),
#         xpd = TRUE
#       )
#       text(
#         0.5,
#         0.6,
#         "Interaction plots are only implemented ",
#         col = font_color(ColorBGplot())
#       )
#       text(
#         0.5,
#         0.4,
#         "for 3 or less Subgroup levels!",
#         col = font_color(ColorBGplot())
#       )
#
#     } else if (!any(is.na(df_factorial$Factorial[y()])) &
#               df_factorial$`Number Factors` == 1) {
#
#       interaction_plot2(
#         df_data = df_factorial$Factorial,
#         fac1 = df_factorial$Variables[1],
#         response = y(),
#         bg.col = ColorBGplot(),
#         bg.col2 = different_hues(ColorBGplot()),
#         font.col = font_color(ColorBGplot()),
#         y.min = y_axe[1],
#         y.max = y_axe[2],
#         box.col = font_color(ColorBGplot()),
#         plot_type = ifelse(pl_typ == "log", "y", "")
#       )
#
#     } else if (!any(is.na(df_factorial$Factorial[y()])) &
#               df_factorial$`Number Factors` == 2) {
#       interaction_plot2(
#         df_data = df_factorial$Factorial,
#         fac1 = df_factorial$Variables[1],
#         fac2 = df_factorial$Variables[2],
#         response = y(),
#         bg.col = ColorBGplot(),
#         bg.col2 = different_hues(ColorBGplot()),
#         font.col = font_color(ColorBGplot()),
#         y.min = y_axe[1],
#         y.max = y_axe[2],
#         box.col = font_color(ColorBGplot()),
#         plot_type = ifelse(pl_typ == "log", "y", "")
#       )
#     } else if (!any(is.na(df_factorial$Factorial[y()])) &
#                df_factorial$`Number Factors` == 3) {
#
#       interaction_plot2(
#         df_data = df_factorial$Factorial,
#           fac1 = df_factorial$Variables[1],
#           fac2 = df_factorial$Variables[2],
#           fac3 = df_factorial$Variables[3],
#           response = y(),
#           bg.col = ColorBGplot(),
#           bg.col2 = different_hues(ColorBGplot()),
#           font.col = font_color(ColorBGplot()),
#           y.min = y_axe[1],
#           y.max = y_axe[2]
#       )
#     }
#   })
#
#    DT_values <- shiny::reactive({
#     # if (input$navpanel == "subscreenasmus") {
#     shinyInput_goto <- function(FUN, len, id, ...) {
#       inputs <- character(len)
#       for (i in seq_len(len)) {
#         inputs[i] <- as.character(FUN(paste0(id, i), ...))
#       }
#       inputs
#     }
#
#     df <- as.data.frame(Module_input2())
#     df_add <- rbind(results$sge[results$sge$nfactors %in% c(1, 2, 3, 4) & results$sge$FCID_incomplete == "Complete",],
#                     results$sge[results$sge$nfactors %in% c(2, 3, 4) & results$sge$FCID_pseudo != "No Pseudo",])$nfactors
#     df_add2 <- df_add[order(as.numeric(Module_input$dat[,3]))]
#     df <- cbind(df, df_add2)
#     colnames(df)[colnames(df) == "df_add2"] <- "Factors"
#     df <- df %>%
#       dplyr::select("Factors", dplyr::everything())
#     df <- cbind(df, data.frame(
#       Change_to_subgroup_ID = shinyInput_goto(actionButton, dim(df)[1], 'button_', label = "Switch to Subgroup",
#                              onclick = 'Shiny.onInputChange(\"goto_button\",  this.id)' )
#     ))
#     df
#     # }
#   })
#
#
#     output$assessment <- DT::renderDataTable(
#
#     DT::datatable(
#       shiny::isolate(DT_values()),
#       escape = FALSE,
#       filter = 'top',
#       selection = 'none',
#       extensions = 'Buttons',
#       options = list(
#         initComplete = DT::JS(
#           "function(settings, json) {",
#           paste0("$(this.api().table().header()).css({'background-color': '",
#                  ColorBGplot(),
#                  "', 'color': '",
#                  font_color(different_hues(ColorBGplot())),
#                  "'});"
#           ),
#           "}"
#         ),
#         processing = FALSE,
#         deferRender = TRUE
#         ,  dom = 'Brtip', buttons = c('copy','print','pageLength', I('colvis')),
#         lengthMenu = list(c(6, 12, -1),
#         c("6", "12", "All")),
#         pageLength = 6,
#         rowCallback = DT::JS(
#           "function(row, data) {\n
#           // Bold cells for those >= 5 in the first column\n
#           if (parseFloat(data[1]) >= 15.0)\n
#           $(\"td:eq(1)\", row).css(\"font-weight\", \"bold\");\n
#           }"
#         )
#       )
#     ) %>%
#     DT::formatStyle(
#       1, target = "row",
#       backgroundColor = different_hues(ColorBGplot())
#     ) %>%
#     DT::formatStyle(
#       c(names(shiny::isolate(DT_values()))),
#       backgroundColor = DT::styleEqual(
#         levels = c("No","N/A","Yes"),
#         values = c("#6b5050", different_hues(ColorBGplot()), "#506b50")
#       ),
#       color =  font_color(different_hues(ColorBGplot()))
#     )
#   )
#
#   proxy = DT::dataTableProxy('assessment')
#
#   shiny::observe({
#     shiny::req(DT_values())
#     DT::replaceData(
#       proxy,
#       data = DT_values()
#     )
#   })
#
#   shiny::observeEvent(input$goto_button, {
#     screening_index$val <- as.numeric(strsplit(input$goto_button, "_")[[1]][2])
#   })
# #
# #
#  output$plot_type_asmus <- shiny::renderUI({
#     shiny::radioButtons(
#       inputId = "plot_type_asmus",
#       label = "Plot Type",
#       selected = "lin",
#       inline = TRUE,
#       choiceNames = list("linear", "logarithmic"),
#       choiceValues = c("lin", "log")
#     )
#   })
# #
#   output$yrange_asmus <- shiny::renderUI({
#     shiny::req(y())
#
#     if (req(input$plot_type_asmus) == "lin") {
#     shiny::sliderInput(
#       inputId = ns("yrange_asmus"),
#       label = "Y Range",
#       min = roundDownNice(min(results$sge[, y()], na.rm = TRUE)),
#       max = roundUpNice(max(results$sge[, y()], na.rm = TRUE)),
#       value = c(min(results$sge[, names(results$results_total)[1]], na.rm = TRUE), max(results$sge[, names(results$results_total)[1]], na.rm = TRUE)),
#       step = roundUpNice((max(results$sge[, y()], na.rm = TRUE) - min(results$sge[,shiny::req(y())], na.rm = TRUE))/100)
#     )
#     } else {
#       rg.z <- log(
#         range(
#           roundDownNice(min(results$sge[, y()], na.rm = TRUE)),
#           roundUpNice(max(results$sge[, y()], na.rm = TRUE))
#         )
#       )
#
#       choices <- unique(
#         unlist(
#           lapply(
#             exp(seq(rg.z[1], rg.z[2], length.out = 20)),
#             function(x) {roundUpNice(x = x, nice = c(2,4,8,16))}
#           )
#         )
#       )
#
#       shinyWidgets::sliderTextInput(
#         inputId = "yrange_asmus",
#         label = "Y Range",
#         hide_min_max = TRUE,
#         choices = choices,
#         selected = c(choices[1], choices[length(choices)]),
#         grid = TRUE
#       )
#     }
#   })
# #
# #
# #     ####... 70. graph5 ####
# #   if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(results$sge))) {
#   output$graph5 <- shiny::renderPlot({
#     shiny::req(plot_points_data5())
#     input$screening_forward
#     input$screening_backward
#     graphics::par(
#       oma = c(0, 0, 0, 0),
#       mar = c(0, 3, 0, 0),
#       bg = ColorBGplot()
#     )
#     plot_point <- plot_points_data5()
#     setcolor2()
#     all_points <- cbind(plot_point, color2, stringsAsFactors = FALSE)
#
#     gold_points_ID <- all_points[all_points$color %in% ColorTabClicked(), ]$ID
#
#     white_points <- all_points[all_points$color %in% c("#FFFFFFFF", "#FFFFFFBF", "#FFFFFF80", "#FFFFFF40", "#FFFFFF1A"), ]
#     colored_points <- all_points[!all_points$color %in% c("#FFFFFFFF", "#FFFFFFBF", "#FFFFFF80", "#FFFFFF40", "#FFFFFF1A"), ]
#
#     if (is.null(input$yrange_asmus)) {
#       y_lim <- YRange()
#     } else {
#       y_lim <- input$yrange_asmus
#     }
#
#     if (is.null(input$plot_type_asmus)) {
#       pl_typ <- "lin"
#     } else {
#       pl_typ <- input$plot_type_asmus
#     }
#
#     plot(
#       all_points$x,
#       all_points$y,
#       xlab = "",
#       ylab = "",
#       ylim = y_lim,
#       log = ifelse(pl_typ == "log", "y", ""),
#       cex.axis = 1.5,
#       cex.lab = 1.5,
#       type = "n",
#       axes = FALSE
#     )
#     graphics::rect(
#       xleft = graphics::grconvertX(0,'ndc','user') - ifelse(pl_typ == "lin", 1000, 0),
#       xright = graphics::grconvertX(1,'ndc','user') + ifelse(pl_typ == "lin", 1000, 0),
#       ybottom = graphics::grconvertY(0,'ndc','user') - ifelse(pl_typ == "lin", 1000, 0),
#       ytop = graphics::grconvertY(1,'ndc','user') + ifelse(pl_typ == "lin", 1000, 0),
#       border = NA,
#       col = ColorBGplot(),
#       xpd = TRUE
#     )
#     if (ifelse(pl_typ == "log", "y", "") == "y") {
#       miniy <- 10^graphics::par("usr")[3]
#       maxiy <- 10^graphics::par("usr")[4]
#       lowy <- 10^(graphics::par("usr")[3] + (graphics::par("usr")[4] - graphics::par("usr")[3])/40)
#       lowyp <- 10^(graphics::par("usr")[3] + (graphics::par("usr")[4] - graphics::par("usr")[3])/15)
#       minplustinyy <- 10^(graphics::par("usr")[3] + (graphics::par("usr")[4] -
#                                              graphics::par("usr")[3])/1400)
#     } else {
#       miniy <- graphics::par("usr")[3]
#       maxiy <- graphics::par("usr")[4]
#       lowy <- miniy + (maxiy - miniy)/40
#       lowyp <- miniy + (maxiy - miniy)/15
#       minplustinyy <- miniy + (maxiy - miniy)/1400
#     }
#
#     minix <- roundDownNice(graphics::par("usr")[1])
#     maxix <- roundUpNice(graphics::par("usr")[2])
#
#     nr <- 7
#     stepx <- roundUpNice((maxix - minix)/(nr +  1))
#     if (minix < stepx)
#       minix <- 0
#     stripesx <- 0:(nr + 1)
#     stripesx <- lapply(stripesx, function(x) x * stepx)
#     stripesx <- lapply(stripesx, function(x) x + minix)
#     stripesxp <- lapply(stripesx, function(x) paste(floor(x/results$results_total[,c(x())] * 100), "%"))
#
#     for (i in seq(1, nr, 2)) graphics::rect(stripesx[i],miniy, stripesx[i + 1], maxiy, col = different_hues(ColorBGplot()), border = NA)
#
#
#     if (xlabel() == TRUE) {
#       text(stripesx, lowy, stripesx, cex = 1.2, col = font_color(ColorBGplot()))
#       text(stripesx, lowyp, stripesxp, cex = 1.2, col = font_color(ColorBGplot()))
#     }
#
#     graphics::box(col = font_color(ColorBGplot()))
#
#     axis(
#       2,
#       col = font_color(ColorBGplot()),
#       col.ticks = font_color(ColorBGplot()),
#       col.axis = font_color(ColorBGplot()),
#       cex.axis = 1
#     )
#
#     title(
#       main = SG_tit3(),
#       line = -2,
#       col = "#8b8b8b",
#       col.main = font_color(ColorBGplot())
#     )
#
#     pch_ <- ifelse(pch_value() == "19", 19, pch_value())
#
#
#     if (circlestyle() == "standard") {
#       graphics::points(
#         white_points$x,
#         white_points$y,
#         pch = pch_,
#         cex = input$pointsize,
#         col = white_points$color
#       )
#       graphics::points(
#         colored_points$x,
#         colored_points$y,
#         pch = pch_,
#         cex = input$pointsize,
#         col = colored_points$color
#       )
#     }
#
#     if (circlestyle() == "groupsize") {
#       graphics::points(
#         white_points$x,
#         white_points$y,
#         pch = pch_,
#         cex = input$pointsize * sqrt(results$sge[results$sge$SGID %in% white_points$ID, 'N.of.subjects'][results$sge$nfactors >= input$keys_asmus[1] & results$sge$nfactors <= input$keys_asmus[2]]/pi),
#         col = white_points$color
#       )
#       graphics::points(
#         colored_points$x,
#         colored_points$y,
#         pch = pch_,
#         cex = input$pointsize * sqrt(results$sge[results$sge$SGID %in% colored_points$ID , 'N.of.subjects'][results$sge$nfactors >= input$keys_asmus[1] & results$sge$nfactors <= input$keys_asmus[2]]/pi),
#         col = colored_points$color
#       )
#     }
#
#     graphics::abline(
#       h = ref_line(),
#       lwd = 3,
#       col = ColorReference()
#     )
#
#     graphics::text(
#       x = graphics::grconvertX(0.97, from = 'nfc', to = 'user'),
#       y = ref_line() + diff(YRange())/50,
#       paste0(shiny::isolate(ref_line())),
#       col = ColorReference()
#     )
#
#     if (grid() == TRUE) {
#       graphics::abline(h = axTicks(2), lty = 2, col = font_color(ColorBGplot()), lwd = 0.3)
#       graphics::abline(v = axTicks(1), lty = 2, col = font_color(ColorBGplot()), lwd = 0.3)
#     }
#   })
# #   }
# #
#     output$hover_info5 <- shiny::renderUI({
#     shiny::req(input$plot_hover5, plot_points_data5())
#     input$plot_hover5
#
#     plot_point <- plot_points_data5()
#
#     all_points <- cbind(plot_point, color2, stringsAsFactors = FALSE)
#
#     colored_points <- all_points[!startsWith(all_points$color, ColorPoints()),]
#
#     hover <- input$plot_hover5
#     hover$mapping <- list(xintercept = "xintercept", x = "x", y = "y")
#
#     point <- nearPoints(colored_points, hover)
#
#     if (nrow(point) == 0) return(NULL)
#
#     left_pct <- (hover$coords_img$x - hover$range$left) / (hover$range$right - hover$range$left)
#
#     top_pct <- (hover$domain$top - ifelse(input$plot_type_asmus == "log",log10(hover$y), hover$y)) / (hover$domain$top - hover$domain$bottom)
#     left_px <- ifelse(left_pct <= 0.75,
#                       20 + hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x,
#                       - 175 + hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x)
#
#     top_px <- ifelse(top_pct <= 0.5,
#                      20 + hover$range$top + top_pct * (hover$range$bottom - hover$range$top),
#                      - 115 + hover$range$top + top_pct * (hover$range$bottom - hover$range$top))
#     style <- paste0("position:absolute; z-index:100; background-color: rgba(", grDevices::col2rgb(point$color)[1],",", grDevices::col2rgb(point$color)[2],",",grDevices::col2rgb(point$color)[3],",0.85); ",
#                     "left:", left_px, "px; top:", top_px, "px; border: 0px;")
#     point <- point[1,]
#
#
#     tmp1 <- colnames(results$sge[which(results$sge$SGID == point$ID), results$factors])[which(results$sge[which(results$sge$SGID == point$ID), results$factors] != "Not used")]
#
#     tmp2 <- results$sge %>%
#       dplyr::filter(SGID %in% point$ID) %>%
#       dplyr::select(colnames(results$sge[which(results$sge$SGID == point$ID), results$factors])[which(results$sge[which(results$sge$SGID == point$ID), results$factors] != "Not used")])
#     tmp2 <- data.frame(lapply(tmp2,as.character), stringsAsFactors = FALSE)
#     wellPanel(
#       style = style,
#       shiny::p(shiny::HTML(paste0("<b style = 'color: ", font_color(point$color),"'> ", x(),": ", point$x, "</br>",
#                     "<b style = 'color: ", font_color(point$color),"'> ", y(),": ", point$y, "</br>",
#                     "<b style = 'color: ", font_color(point$color),"'> Factors(",
#                     length(tmp1),
#                     "): ", paste(paste0(tmp1," = ", tmp2), collapse = ", "), "</br>")))
#     )
#   })
# #
#   context_ids <- shiny::reactive({
#     tmp <- rbind(results$sge[results$sge$nfactors %in% c(1, 2, 3, 4) & results$sge$FCID_incomplete == "Complete",],
#                  results$sge[results$sge$nfactors %in% c(2, 3, 4) & results$sge$FCID_pseudo != "No Pseudo",]
#     )
#     list('SGID' = tmp$SGID, 'FCID' = unique(tmp$FCID_all))
#   })
# #
# #
# roundUpNice <- function(x, nice = nice_Numbers) {
#     if (length(x) != 1) stop("'x' must be of length 1")
#     if (x >= 0) 10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) *nice)[[1]]]]
#     else -1 * (roundDownNice(-x, nice = nice_Numbers))
#   }
#
#   roundDownNice <- function(x, nice = nice_Numbers) {
#     if (length(x) != 1) stop("'x' must be of length 1")
#     if (x >= 0) 10^floor(log10(x)) * nice[[max(which(x >= 10^floor(log10(x)) * nice))]]
#     else -1 * (roundUpNice(-x, nice = nice_Numbers))
#   }
#   screening_ids <- shiny::reactive({
#     shiny::req(context_ids())
#     if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(results$sge))) {
#     ind <- sorting_index()
#     }
#     tmp <- context_ids()$SGID
#     if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(results$sge))) {
#     tmp <- tmp[rank(ind)]
#     }
#     tmp
#   })
# #   if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(results$sge))) {
#    screening_index <- shiny::reactiveValues(val = 0)
# #
#   screening_index_new <- shiny::reactiveValues(val = NULL)
# #
#   shiny::observeEvent(c(screening_index$val,input$direction, y()), {
#    shiny::req(sorting_index())
#     sort_ind <- sorting_index()
#     screening_index_new$val <- sort_ind[screening_index$val]
#   })
# #
#   shiny::observeEvent(input$screening_backward, {
#     if (screening_index$val > 1) {
#       screening_index$val <- screening_index$val - 1
#     }
#   })
# #
#   shiny::observeEvent(c(input$screening_forward), {
#    shiny::req(sorting_index())
#     sort_ind <- sorting_index()
#     if(screening_index$val < length(sort_ind)) {
#       screening_index$val <- screening_index$val + 1
#     }
#   })
# #
#
#   shiny::observeEvent(c(input$screening_forward, input$screening_backward, input$goto_button), {
#     # if (input$navpanel == "subscreenasmus") {
#       setcolor2()
#     # } else if (input$navpanel %in% c("1", "2")) {
#
#     # }
#   })
# #   }
# #
# #
# #   #### MODULE ####
# #   if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(results$sge))) {
#   Module_input <- shiny::reactiveValues(
#     dat = matrix(
#       c(rep("N/A",
#           2 * length(rbind(results$sge[results$sge$nfactors %in% c(1, 2, 3, 4) & results$sge$FCID_incomplete == "Complete",],
#           results$sge[results$sge$nfactors %in% c(2, 3, 4) & results$sge$FCID_pseudo != "No Pseudo",])$SGID)
#       ),rbind(results$sge[results$sge$nfactors %in% c(1, 2, 3, 4) & results$sge$FCID_incomplete == "Complete",],
#               results$sge[results$sge$nfactors %in% c(2, 3, 4) & results$sge$FCID_pseudo != "No Pseudo",])$SGID),
#       length(rbind(results$sge[results$sge$nfactors %in% c(1, 2, 3, 4) & results$sge$FCID_incomplete == "Complete",],
#                    results$sge[results$sge$nfactors %in% c(2, 3, 4) & results$sge$FCID_pseudo != "No Pseudo",])$SGID
#       ),
#       3,
#       dimnames = list(
#         paste0(
#           "Subgroup ID: ",
#           rbind(results$sge[results$sge$nfactors %in% c(1, 2, 3, 4) & results$sge$FCID_incomplete == "Complete",],
#           results$sge[results$sge$nfactors %in% c(2, 3, 4) & results$sge$FCID_pseudo != "No Pseudo",])$SGID
#         ),
#         c("Is the Subgroup size big enough?",
#           "Is the effect remarkable?",
#           "Subgroup_ID"
#         )
#       )
#     )
#   )
# #
#   Module_input2 <- shiny::reactive({
#     Module_input <- Module_input$dat[order(as.numeric(Module_input$dat[,3])),]
#     Module_input[rank(sorting_index()),]
#   })
# #
#   shiny::observeEvent(call_Mod()$size(), {
#     if (!is.null(call_Mod()$size())) {
#       Module_input$dat[which(Module_input$dat[, 3] == as.character(screening_index_new$val)) , 1] <- call_Mod()$size()
#     }
#   }, ignoreNULL = TRUE
#   )
# #
#   shiny::observeEvent(call_Mod()$satis(), {
#     if (!is.null(call_Mod()$satis())) {
#       Module_input$dat[which(Module_input$dat[, 3] == as.character(screening_index_new$val)), 2] <- call_Mod()$satis()
#     }
#   }, ignoreNULL = TRUE
#   )
# #
#   call_Mod <- shiny::reactive({
#     Val <- shiny::callModule(screeningModule_Server,
#                              id = as.character(screening_index_new$val),
#                              label = screening_index_new$val,
#                              module_input = Module_input2()
#     )
#     Val
#   })
# #
#   shiny::observe({(call_Mod())})
# #   }
# #
# #     if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(results$sge))) {
#   output$legend4 <- shiny::renderUI({
#     shiny::req(plot_points_data5())
#     plot_point <- plot_points_data5()
#     setcolor2()
#     all_points <- cbind(plot_point, color2, stringsAsFactors = FALSE)
#     gold_points_ID <- all_points[all_points$color %in% ColorTabClicked(),]$ID
#     colored_points <- all_points[!startsWith(all_points$color, ColorPoints()),]
#     active_colors <- unique(colored_points$color)
#     shiny::tagList(
#       if (length(gold_points_ID) > 0) {
#         bsplus::bs_embed_tooltip(
#           tag = shiny::span(shiny::tagList(shiny::tags$i(class = "fa fa-circle", style = paste0("color: ", ColorTabClicked())),"Selected Subgroup")),
#           title = ".",
#           placement = "top",
#           expanded = TRUE
#         )
#       },
#       if (ColorParents() %in% active_colors) {
#         bsplus::bs_embed_tooltip(
#           tag = shiny::span(shiny::tagList(shiny::tags$i(class = "fa fa-circle", style = paste0("color: ", ColorParents())),"Parent Subgroup(s)")),
#           title = ".",
#           placement = "top",
#           expanded = TRUE
#         )
#       },
#
#
#       if (ColorFactCont() %in% active_colors) {
#         bsplus::bs_embed_tooltip(
#           tag = shiny::span(shiny::tagList(shiny::tags$i(class = "fa fa-circle", style = paste0("color: ", ColorFactCont())),"Factorial Context")),
#           title = ".",
#           placement = "top",
#           expanded = TRUE
#         )
#       },
#       if (different_hues(ColorFactCont(), value = 89) %in% active_colors) {
#         bsplus::bs_embed_tooltip(
#           tag = shiny::span(shiny::tagList(shiny::tags$i(class = "fa fa-circle", style = paste0("color: ", different_hues(ColorFactCont(), value = 89))),"Pseudo factorial Context")),
#           title = ".",
#           placement = "top",
#           expanded = TRUE
#         )
#       }
#     )
#   })
# #   if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(results$sge))) {
#     sorting_index <- shiny::reactive({
#      sort_df <- rbind(results$sge[results$sge$nfactors %in% c(1, 2, 3, 4) & results$sge$FCID_incomplete == "Complete",],
#             results$sge[results$sge$nfactors %in% c(2, 3, 4) & results$sge$FCID_pseudo != "No Pseudo",]) %>%
#         dplyr::arrange(nfactors, !! rlang::sym(shiny::req(y())))
#       if (!is.null(input$direction) & input$direction == "desc") {
#         sort_df <- sort_df %>%
#           dplyr::arrange(nfactors, desc(!! rlang::sym(shiny::req(y()))))
#      }
#      sort_df$SGID
#     })
# #   }
# #
#   output$header1 <- shiny::renderUI({
#     shiny::req(screening_index_new$val)
#     shiny::tags$h5(
#       id = "header4",
#       paste0("Subgroup: ", screening_index_new$val)
#     )
#   })
# #
#   output$header2 <- shiny::renderUI({
#     input$y
#     shiny::req(screening_index$val)
#     tmp1 <- colnames(results$sge[which(results$sge$SGID == screening_index_new$val), results$factors])[which(results$sge[which(results$sge$SGID == screening_index_new$val), results$factors] != "Not used")]
#     tmp2 <- results$sge %>%
#       dplyr::filter(SGID %in% screening_index_new$val) %>%
#       dplyr::select(colnames(results$sge[which(results$sge$SGID == screening_index_new$val), results$factors])[which(results$sge[which(results$sge$SGID == screening_index_new$val), results$factors] != "Not used")])
#     tmp2 <- data.frame(lapply(tmp2, as.character), stringsAsFactors = FALSE)
#     shiny::tags$h5(
#       id = "header4",
#       paste0("Factors(", length(tmp1),"): ", paste(paste0(tmp1, " = ", tmp2), collapse = ", "))
#     )
#   })
# #
# #   ####... 35. screening_ui ####
#   output$screening_ui <- shiny::renderUI({
#     purrr::map(screening_index_new$val, ~ screeningModule_UI(id = session$ns(.x)))
#   })
# #
# #   ####... 39.+46. interaction+factorial ####
# #
#   df_factorial <- shiny::reactiveValues(data = data.frame(NULL))
#   shiny::observeEvent(
#     c(input$showPanel1, input$screening_forward, input$screening_backward), ignoreNULL = FALSE, {
# #       if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(results$sge))) {
#     shiny::req(screening_index$val)
#     if (screening_index$val != 0) {
#       df_factorial <- factorialContext(results, screening_index$val)
#       if (is.null(dim(df_factorial$Factorial))) {
#         tmp <- NULL
#       } else {
#
#         df_fac <- subset(
#           df_factorial$Factorial,
#           select = c("SGID", x = x(), y = input$y, "nfactors", results$factors)
#         )
#
#         tmp <- DT::datatable(
#           data = df_fac,
#           extensions = 'Buttons',
#           options = list(
#             initComplete = DT::JS(
#               "function(settings, json) {",
#               paste0("$(this.api().table().header()).css({'background-color': '",
#                      ColorBGplot(),
#                      "', 'color': '",
#                      font_color(different_hues(ColorBGplot())),
#                      "'});"
#               ),
#               "}"
#             ),
#             dom = 'Brtip',
#             buttons = c('copy','print','pageLength',I('colvis')),
#             lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
#             pageLength = 6
#           ),
#           class = 'cell-border stripe',
#           rownames = FALSE,
#           caption = '',
#           filter = 'top'
#         )
#
#         tmp <- DT::formatStyle(
#           table = tmp,
#           columns = 1:(ncol(df_fac)
#                        ),
#           target = "cell",
#           backgroundColor = different_hues(ColorBGplot()),
#           border = paste0('.5px solid ', ColorBGplot())
#         )
#       }
#     }
# #     }
# #     ####... 46. factorial ####
# #
#     y_axe_Int <- shiny::reactive({
#       shiny::req(input$y_Interaction_Button2)
#
#       if (input$y_Interaction_Button2 == "Synchron") {
#         tmp <- c(YRange()[1], YRange()[2])
#       }
#       if (input$y_Interaction_Button2 == "Optimal") {
#         tmp <- c("NA","NA")
#       }
#       tmp
#     })
# #
# #     ####... 38b. legend ####
#     output$legend <- shiny::renderUI({
#       shiny::req(plot_points_data5())
#       plot_point <- plot_points_data5()
#
#       if (length(color) == dim(plot_point)[1]) {
#
#       all_points <- cbind(plot_point, color, stringsAsFactors = FALSE)
#       gold_points_ID <- all_points[all_points$color %in% ColorTabClicked(),]$ID
#       colored_points <- all_points[!startsWith(all_points$color, ColorPoints()),]
#       active_colors <- unique(colored_points$color)
#       shiny::tagList(
#         if (ColorClicked() %in% active_colors) {
#                shiny::p(
#                  shiny::span(
#                   shiny::tagList(
#                     shiny::tags$i(
#                       class = "fa fa-circle",
#                       style = paste0("color: ",
#                                      ColorClicked()
#                       )
#                     ),"Clicked Subgroup(s)"
#                   )
#                  )
#                )
#         },
#         if (length(gold_points_ID) > 0) {
#                shiny::p(
#                  shiny::span(
#                    shiny::tagList(
#                     shiny::tags$i(
#                       class = "fa fa-circle",
#                       style = paste0("color: ", ColorTabClicked())
#                     ),"Selected Subgroup(s)"
#                   )
#                  )
#                )
#         },
#         if (ColorSelected() %in% active_colors) {
#                shiny::p(
#                  shiny::span(
#                   shiny::tagList(
#                     shiny::tags$i(
#                       class = "fa fa-circle",
#                       style = paste0("color: ", ColorSelected())
#                     ),"Filtered Subgroup(s)"
#                   )
#                  )
#            )
#         },
#         if (ColorImportance()%in% active_colors) {
#                shiny::p(
#                  shiny::span(
#                   shiny::tagList(
#                     shiny::tags$i(
#                       class = "fa fa-circle",
#                       style = paste0("color: ", ColorImportance())
#                     ),"Importance"
#                   )
#                  )
#            )
#         },
#         if (ColorParents() %in% active_colors) {
#                shiny::h5(
#                  shiny::span(
#                   shiny::tagList(
#                     shiny::tags$i(
#                       class = "fa fa-circle",
#                       style = paste0("color: ", ColorParents())
#                     ), "Parent Subgroup(s)"
#                   )
#                  )
#            )
#         },
#         if (ColorFactCont() %in% active_colors) {
#              tag = shiny::p(
#                shiny::span(
#                 shiny::tagList(
#                   shiny::tags$i(
#                     class = "fa fa-circle",
#                     style = paste0("color: ", ColorFactCont())
#                   ),"Factorial Context"
#                 )
#                )
#          )
#         },
#         if (!is.null(plot_points_data_complement())) {
#           if (dim(plot_points_data_complement())[1] > 0) {
#                tag = shiny::p(
#                  shiny::span(
#                   shiny::tagList(
#                     shiny::tags$i(
#                       class = "fa fa-times-circle",
#                       style = paste0("color: #fffb00")
#                     ),"Subgroup Complement"
#                   )
#                  )
#              )
#           }
#         },
#         if (different_hues(ColorFactCont(), value = 89) %in% active_colors) {
#           tag = shiny::p(
#             shiny::span(
#               shiny::tagList(
#                 shiny::tags$i(
#                   class = "fa fa-circle",
#                   style = paste0("color: ", different_hues(ColorFactCont(), value = 89))
#                 ),"Incomplete factorial Context"
#               )
#            )
#           )
#         })
#       }
#     })
# #
# #
#     output$legend2 <- shiny::renderUI({
#       shiny::req(plot_points_data5())
#       plot_point <- plot_points_data5()
#       all_points <- cbind(plot_point, color, stringsAsFactors = FALSE)
#       gold_points_ID <- all_points[all_points$color %in% ColorTabClicked(),]$ID
#       colored_points <- all_points[!startsWith(all_points$color, ColorPoints()),]
#       active_colors <- unique(colored_points$color)
#       shiny::tagList(
#         if (ColorClicked() %in% active_colors) {
#             shiny::span(
#               shiny::tagList(
#                 shiny::tags$i(
#                   class = "fa fa-circle", style = paste0("color: ", ColorClicked())
#                 ),"Clicked Subgroup(s)"
#            )
#           )
#         },
#         if (length(gold_points_ID) > 0) {
#             shiny::span(
#               shiny::tagList(
#                 shiny::tags$i(
#                   class = "fa fa-circle", style = paste0("color: ", ColorTabClicked())
#                 ),"Selected Subgroup(s)"
#            )
#             )
#         },
#         if (ColorSelected() %in% active_colors) {
#             shiny::span(
#               shiny::tagList(
#                 shiny::tags$i(
#                   class = "fa fa-circle",
#                   style = paste0("color: ", ColorSelected())
#                 ),"Filtered Subgroup(s)"
#            )
#           )
#         },
#         if (ColorImportance()%in% active_colors) {
#             shiny::span(
#               shiny::tagList(
#                 shiny::tags$i(
#                   class = "fa fa-circle",
#                   style = paste0("color: ", ColorImportance())
#                 ),"Importance"
#               )
#           )
#         },
#         if (ColorParents() %in% active_colors) {
#             shiny::span(
#               shiny::tagList(
#                 shiny::tags$i(
#                   class = "fa fa-circle",
#                   style = paste0("color: ", ColorParents())
#                 ),"Parent Subgroup(s)"
#            )
#           )
#         },
#         if (ColorFactCont() %in% active_colors) {
#             shiny::span(
#               shiny::tagList(
#                 shiny::tags$i(
#                   class = "fa fa-circle",
#                   style = paste0("color: ", ColorFactCont())
#                 ),"Factorial Context"
#            )
#             )
#         },
#         if (different_hues(ColorFactCont(), value = 89) %in% active_colors) {
#             shiny::span(
#               shiny::tagList(
#                 shiny::tags$i(
#                   class = "fa fa-circle",
#                   style = paste0("color: ", different_hues(ColorFactCont(), value = 89))
#                 ),"Incomplete factorial Context"
#            )
#           )
#         }
#       )
#     })
#
#     output$legend3 <- shiny::renderUI({
#       shiny::req(plot_points_data5())
#       plot_point <- plot_points_data5()
#       all_points <- cbind(plot_point, color, stringsAsFactors = FALSE)
#       gold_points_ID <- all_points[all_points$color %in% ColorTabClicked(),]$ID
#       colored_points <- all_points[!startsWith(all_points$color, ColorPoints()),]
#       active_colors <- unique(colored_points$color)
#       shiny::tagList(
#         if (ColorClicked() %in% active_colors) {
#             shiny::span(
#               shiny::tagList(
#                 shiny::tags$i(
#                   class = "fa fa-circle",
#                   style = paste0("color: ", ColorClicked())
#                 ),"Clicked Subgroup(s)"
#               )
#            )
#         },
#         if (length(gold_points_ID) > 0) {
#             shiny::span(
#               shiny::tagList(
#                 shiny::tags$i(
#                   class = "fa fa-circle",
#                   style = paste0("color: ", ColorTabClicked())
#                 ),"Selected Subgroup(s)"
#               )
#             )
#         },
#         if (ColorSelected() %in% active_colors) {
#             shiny::span(
#               shiny::tagList(
#                 shiny::tags$i(
#                   class = "fa fa-circle",
#                   style = paste0("color: ", ColorSelected())
#                 ),"Filtered Subgroup(s)"
#               )
#           )
#         },
#         if (ColorImportance() %in% active_colors) {
#             shiny::span(
#               shiny::tagList(
#                 shiny::tags$i(
#                   class = "fa fa-circle",
#                   style = paste0("color: ", ColorImportance())
#                 ), "Importance"
#               )
#           )
#         },
#         if (ColorParents() %in% active_colors) {
#             shiny::span(
#               shiny::tagList(
#                 shiny::tags$i(
#                   class = "fa fa-circle",
#                   style = paste0("color: ", ColorParents())
#                 ),"Parent Subgroup(s)"
#               )
#           )
#         },
#         if (ColorFactCont() %in% active_colors) {
#             shiny::span(
#               shiny::tagList(
#                 shiny::tags$i(
#                   class = "fa fa-circle",
#                   style = paste0("color: ", ColorFactCont())
#                 ),"Factorial Context"
#               )
#            )
#         },
#         if (different_hues(ColorFactCont(), value = 89) %in% active_colors) {
#             shiny::span(
#               shiny::tagList(
#                 shiny::tags$i(
#                   class = "fa fa-circle",
#                   style = paste0("color: ", different_hues(ColorFactCont(), value = 89))
#                 ),"Incomplete factorial Context"
#               )
#             )
#         }
#       )
#     })
# #
# #     ####... 39. interaction ####
#     output$interaction <- shiny::renderPlot({
#
#       shiny::req(y_axe_Int())
#       y_axe <- y_axe_Int()
#       df_factorial <- factorialContext(
#         results,
#         click_points_data$xy[input$selectedSG_rows_selected,'SGID']
#       )
#
#       if (is.null(df_factorial$Variables[1]) || is.na(df_factorial$Variables[1])) {
#         plot(
#           NULL,
#           xlim = c(0, 1),
#           ylim = c(0, 1),
#           axes = FALSE,
#           xlab = "",
#           ylab = ""
#         )
#         graphics::rect(
#           xleft = graphics::grconvertX(0,'ndc','user') - 1000,
#           xright = graphics::grconvertX(1,'ndc','user') + 1000,
#           ybottom = graphics::grconvertY(0,'ndc','user') - 1000,
#           ytop = graphics::grconvertY(1,'ndc','user') + 1000,
#           border = NA,
#           col = ColorBGplot(),
#           xpd = TRUE
#         )
#         text(
#           0.5,
#           0.5,
#           "Please select a Subgroup!",
#           col = font_color(ColorBGplot()),
#           cex = 1.4
#         )
#         text(
#           0.5,
#           0.4,
#           "(Click on a point in the graphic",
#           col = font_color(ColorBGplot()),
#           cex = 0.9
#         )
#         text(
#           0.5,
#           0.3,
#           "and then select a subgroup in the",
#           col = font_color(ColorBGplot()),
#           cex = 0.9
#         )
#
#         text(
#           0.5,
#           0.2,
#           "'Selected Subgroup'-table by clicking on)",
#           col = font_color(ColorBGplot()),
#           cex = 0.9
#         )
#
#       } else if (!is.null(df_factorial$Variables[1]) &
#                  !is.na(df_factorial$Variables[1]) &
#                  any(is.na(df_factorial$Factorial[input$y])) &
#                  df_factorial$`Number Factors` <= 3) {
#         plot(
#           NULL,
#           xlim = c(0, 1),
#           ylim = c(0, 1),
#           axes = FALSE,
#           xlab = "",
#           ylab = ""
#         )
#         graphics::rect(
#           xleft = graphics::grconvertX(0,'ndc','user') - 1000,
#           xright = graphics::grconvertX(1, 'ndc', 'user') + 1000,
#           ybottom = graphics::grconvertY(0,'ndc','user') - 1000,
#           ytop = graphics::grconvertY(1, 'ndc', 'user') + 1000,
#           border = NA,
#           col = ColorBGplot(),
#           xpd = TRUE
#         )
#         text(
#           0.5,
#           0.5,
#           "Incomplete factorial context!",
#           col = font_color(ColorBGplot()),
#           cex = 1.4
#         )
#         text(
#           0.5,
#           0.4,
#           "(This graphic is not available",
#           col = font_color(ColorBGplot()),
#           cex = 0.9
#         )
#         text(
#           0.5,
#           0.3,
#           "for pseudo factorial contexts)",
#           col = font_color(ColorBGplot()),
#           cex = 0.9
#         )
#
#       } else if (df_factorial$`Number Factors` > 3) {
#         plot(
#           NULL,
#           xlim = c(0, 1),
#           ylim = c(0, 1),
#           axes = FALSE,
#           xlab = "",
#           ylab = ""
#         )
#
#         graphics::rect(
#           xleft = graphics::grconvertX(0,'ndc','user') - 1000,
#           xright = graphics::grconvertX(1, 'ndc', 'user') + 1000,
#           ybottom = graphics::grconvertY(0,'ndc','user') - 1000,
#           ytop = graphics::grconvertY(1, 'ndc', 'user') + 1000,
#           border = NA,
#           col = ColorBGplot(),
#           xpd = TRUE
#         )
#         text(
#           0.5,
#           0.5,
#           "Too many factors!",
#           col = font_color(ColorBGplot()),
#           cex = 1.4
#         )
#         text(
#           0.5,
#           0.4,
#           "(This graphic is not available",
#           col = font_color(ColorBGplot()),
#           cex = 0.9
#         )
#         text(
#           0.5,
#           0.3,
#           "for 4 or more subgroup levels)",
#           col = font_color(ColorBGplot()),
#           cex = 0.9
#         )
#
#       } else if (!any(is.na(df_factorial$Factorial[input$y])) &
#                  df_factorial$`Number Factors` == 1 &
#                  df_factorial$Status == "Complete") {
#
#         interaction_plot2(
#           df_data = df_factorial$Factorial,
#           fac1 = df_factorial$Variables[1],
#           response = input$y,
#           bg.col = ColorBGplot(),
#           bg.col2 = different_hues(ColorBGplot()),
#           font.col = font_color(ColorBGplot()),
#           y.min = y_axe[1],
#           y.max = y_axe[2],
#           box.col = font_color(ColorBGplot())
#         )
#
#       } else if (!any(is.na(df_factorial$Factorial[input$y])) &
#                  df_factorial$`Number Factors` == 2 &
#                  df_factorial$Status == "Complete") {
#
#         interaction_plot2(
#           df_data = df_factorial$Factorial,
#           fac1 = df_factorial$Variables[1],
#           fac2 = df_factorial$Variables[2],
#           response = input$y,
#           bg.col = ColorBGplot(),
#           bg.col2 = different_hues(ColorBGplot()),
#           font.col = font_color(ColorBGplot()),
#           y.min = y_axe[1], y.max = y_axe[2],
#           box.col = font_color(ColorBGplot())
#         )
#       } else if (!any(is.na(df_factorial$Factorial[input$y])) &
#                  df_factorial$`Number Factors` == 3 &
#                  df_factorial$Status == "Complete") {
#         interaction_plot2(
#           df_data = df_factorial$Factorial,
#           fac1 = df_factorial$Variables[1],
#           fac2 = df_factorial$Variables[2],
#           fac3 = df_factorial$Variables[3],
#           response = input$y,
#           bg.col = ColorBGplot(),
#           bg.col2 = different_hues(ColorBGplot()),
#           font.col = font_color(ColorBGplot()),
#           y.min = y_axe[1],
#           y.max = y_axe[2]
#         )
#       } else if (!any(is.na(df_factorial$Factorial[input$y])) & df_factorial$Status == "Incomplete") {
#         plot(
#           NULL,
#           xlim = c(0, 1),
#           ylim = c(0, 1),
#           axes = FALSE,
#           xlab = "",
#           ylab = ""
#         )
#
#         graphics::rect(
#           xleft = graphics::grconvertX(0,'ndc','user') - 100,
#           xright = graphics::grconvertX(1, 'ndc', 'user') + 100,
#           ybottom = graphics::grconvertY(0,'ndc','user') - 100,
#           ytop = graphics::grconvertY(1, 'ndc', 'user') + 100,
#           border = NA,
#           col = ColorBGplot(),
#           xpd = TRUE
#         )
#         text(
#           0.5,
#           0.5,
#           "Incomplete factorial context!",
#           col = font_color(ColorBGplot()),
#           cex = 1.4
#         )
#         text(
#           0.5,
#           0.4,
#           "(This graphic is not available",
#           col = font_color(ColorBGplot()),
#           cex = 0.9
#         )
#         text(
#           0.5,
#           0.3,
#           "for pseudo factorial contexts)",
#           col = font_color(ColorBGplot()),
#           cex = 0.9
#         )
#       }
#     })
#   })
# # }
# }
#
# ## To be copied in the UI
# # asmus_old_module_ui("asmus_old")
#
# ## To be copied in the server
# # callModule(asmus_old_module_server, "asmus_old")
