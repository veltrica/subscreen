#' Application file of Subgroup Explorer used in subscreen package
#'
#'

if (!requireNamespace("shiny", quietly = TRUE)) {
  cat("Error: subscreenshow requires the package shiny to be installed")
  stop()
}
if (!requireNamespace("shinyjs", quietly = TRUE)) {
  cat("Error: subscreenshow requires the package shinyjs to be installed")
  stop()
}
if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
  cat("Error: subscreenshow requires the package shinyWidgets to be installed")
  stop()
}
if (!requireNamespace("bsplus", quietly = TRUE)) {
  cat("Error: subscreenshow requires the package bsplus to be installed")
  stop()
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("Error: subscreenshow requires the package jsonlite to be installed")
  stop()
}
if (!requireNamespace("colourpicker", quietly = TRUE)) {
  cat("Error: subscreenshow requires the package colourpicker to be installed")
  stop()
}
if (!requireNamespace("DT", quietly = TRUE)) {
  cat("Error: subscreenshow requires the package DT to be installed")
  stop()
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  cat("Error: subscreenshow requires the package dplyr to be installed")
  stop()
}

suppressMessages(library(shiny))
suppressMessages(library(shinyjs))
suppressMessages(library(bsplus))
suppressMessages(library(jsonlite))
suppressMessages(library(colourpicker))
suppressMessages(library(DT))
suppressMessages(library(shinyWidgets))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))


#### JavaScript Code ####
jscode <- "shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value='+name+']');
  tab.bind('click.tab', function(e) {
  e.preventDefault();
  return false;
  });
  tab.addClass('disabled');
  }
  shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value='+name+']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
  }"

#### CSS code ####
css <- ".nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
  }"

#### SGEAPP ####
# subscreen_theme <- bslib::bs_theme(
#     #main colors:
#     bootswatch = "materia",
#     bg = "#424242",
#     fg = "white",
#     #accent colors:
#     primary = "#10384F",
#     secondary = "#00617F",
#     success = "#66B512",
#     info = "#0091DF",
#     warning = "#DBAC37",
#     danger = "#D30F4B",
#     #options:
#     `enable-shadows` = TRUE
#   )

#### UI ####
ui <- shiny::navbarPage(
  #theme = subscreen_theme,
  title = shiny::div(
    shiny::img(
      src = 'www/subscreen_logo.png',
      style = "margin-top: 3px; padding-right:10px;padding-bottom:10px",
      height = 55
    )
  ),
  windowTitle = windowTitle,
  id = "navpanel",
  ##### SUBSCREEN EXPLORER TAB ####
  shiny::tabPanel(
    "Explorer",
    value = "SubscreenExplorer",
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          '.navbar-nav > li > a, .navbar-brand {
          padding-top:4px !important;
          padding-bottom:0 !important;
          height: 60px;
          }
          .navbar {min-height:25px !important;}'
        )
      )
    ),
    shiny::tags$style(
      shiny::HTML(
        "#header4{color: #e2b007;}"
      )
    ),
    shiny::tags$style(
      shiny::HTML(
        ".navbar-default .navbar-brand:hover {color: #a3a3a3; background-color: #393939;}
        .navbar { background-color: #383838;}
        .navbar-default .navbar-nav > li > a {color:#7a7a7a;}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color: #dedede; background-color: #404040;}
        .navbar-default .navbar-nav > li > a:hover {color: #999999;}
        "
      )
    ),
    # color information of navbar page
    shiny::uiOutput('cont_nav'),
    shiny::fluidPage(
      # color information for app background
      shiny::uiOutput('cont'),
      # color information for the symbols
      shiny::uiOutput('cont2'),
      shiny::fluidRow(
        # subgoup explorer logo
        shiny::uiOutput('logo'),
        shiny::column(3,
          #### VARIABLE OPTIONS TAB ####
          shiny::tabsetPanel(type = "tabs",
            shiny::tabPanel("Variable Options",
              shiny::wellPanel(class = "myclass1", id = "myid1",
                # color information for wellpanel
                shiny::uiOutput('cont_well'),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("question"),
                    title = "Variable plotted on the y-axis.",
                    placement = "top"
                  )
                ),
                shiny::selectInput(
                  inputId = "y",
                  label = "Target variable",
                  choices = NULL,
                  selected = NULL
                ),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("question"),
                    title = "Variable plotted on the x-axis.",
                    placement = "top",
                    expanded = TRUE
                  )
                ),
                shiny::selectInput(
                  inputId = "x",
                  label = "Reference variable",
                  choices = NULL,
                  selected = NULL
                ),
                shiny::fluidRow(
                  shiny::column(6,
                    shiny::div(style = "position:absolute;right:2em;",
                      bsplus::bs_embed_tooltip(
                        tag = shiny_iconlink("question"),
                        title = "Select a filter variable. Subgroups containing this variable are displayed in green (default color).",
                        placement = "top"
                      )
                    ),
                    shiny::selectInput(
                      inputId = "filter",
                      label = "Subgroup Filter (1)",
                      choices = c("no selection"),
                      selected = c("no selection")
                    ),
                    bsplus::use_bs_popover(),
                    bsplus::use_bs_tooltip()
                  ),
                  shiny::column(6,
                    shiny::conditionalPanel(
                      condition = "input.filter != 'no selection'",
                      shiny::uiOutput("VarChosen"),
                      selectize = FALSE
                    )
                  )
                ),
                shiny::fluidRow(
                  shiny::column(6,
                    shiny::conditionalPanel(condition = "input.filter != 'no selection'",
                      shiny::selectInput(
                        inputId = "filter2",
                        label = "Subgroup Filter (2)",
                        choices = c("no selection"),
                        selected = c("no selection")
                      )
                    )
                  ),
                  shiny::column(6,
                    shiny::conditionalPanel(
                      condition = "input.filter2 != 'no selection' & input.filter != 'no selection'",
                      shiny::uiOutput("VarChosen2"),
                      selectize = FALSE
                    )
                  )
                ),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("question "),
                    title = "Subgroups containing selected number(s) of factor(s) are displayed in the plot.",
                    placement = "top",
                    expanded = TRUE
                  )
                ),
                shiny::fluidRow(
                  shiny::column(8,
                    shiny::sliderInput(
                      inputId = "key",
                      label = "Subgroup level(s)",
                      min = 1,
                      max = 3,
                      ticks = FALSE,
                      value = c(1, 3),
                      step = 1
                    )
                  )
                ),
                shiny::fluidRow(
                  shiny::column(8,
                    shiny::uiOutput('YRange'),
                  ),
                  shiny::column(4,
                    shiny::radioButtons(
                      inputId = "plot_type",
                      label = "",
                      selected = "lin",
                      inline = FALSE,
                      choiceNames = list("lin", "log"),
                      choiceValues = c("lin", "log")
                    )
                  )
                ),
                shiny::fluidRow(
                  shiny::column(8,
                    shiny::uiOutput("XRange")
                  )
                )
              ), icon = icon("wrench")
            ),
            #### IMPORTANCE TAB ####
            shiny::tabPanel("Importance Tab", value = "ImportanceTab",
              mod_variable_importance_ui("vi"),
              shinyjs::useShinyjs(debug = TRUE),
              shinyjs::extendShinyjs(
                text = jscode,
                functions = c("disableTab", "enableTab")
                ),
              shinyjs::inlineCSS(css),
              icon = icon("exclamation")
            ),
            #### DISPLAY OPTIONS TAB ####
            shiny::tabPanel("Display Options",
              shiny::wellPanel(class = "myclass6", id = "myid6",
                #### color information for wellpanel
                shiny::uiOutput('cont_well6'),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("question"),
                    title = "Maximum distance to the click dot (in pixel).",
                    placement = "top",
                    expanded = TRUE
                  )
                ),
                shiny::sliderInput(
                  inputId = "pickradius",
                  label = "Choose distance to the click point",
                  min = 1,
                  max = 30,
                  value = 5,
                  step = 1 ,
                  ticks = FALSE
                ),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("question "),
                    title = "Change the dot size.
                    Combinable with dot style option.",
                    placement = "top",
                    expanded = TRUE
                  )
                ),
                shiny::sliderInput(
                  inputId = "pointsize",
                  label = "Choose dot size" ,
                  min = 0.1,
                  max = 4,
                  value = 2,
                  step = 0.2
                ),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("question"),
                    title = "Use the Subgroup size as given size or display
                    all dots with equal size.",
                    placement = "top",
                    expanded = TRUE
                  )
                ),
                shiny::radioButtons(
                  inputId = "circlestyle",
                  label = "Point Style",
                  choiceNames = list("Standard", "Subgroup size"),
                  choiceValues = c("standard", "groupsize"),
                  selected = "standard",
                  inline = TRUE
                ),
                shiny::radioButtons(
                  inputId = "pch_value",
                  label = "Plotting character",
                  choiceNames = list("Circles"),
                  choiceValues = c(19),# '.'),
                  selected = 19,
                  inline = TRUE
                ),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("question"),
                    title = "Adjust brightness of unmarked dots.",
                    placement = "top",
                    expanded = TRUE
                  )
                ),
                shiny::sliderInput(
                  inputId = "point_brightness",
                  label = "Adjust dot brightness",
                  min = 0.05,
                  max = 1,
                  value = 0.5,
                  step = 0.05
                ),
                bsplus::use_bs_popover(),
                bsplus::use_bs_tooltip()
              ),
              shiny::wellPanel(class = "myclass7", id = "myid7",
                #color information for wellpanel
                shiny::uiOutput('cont_well7'),
                shiny::checkboxInput(
                  inputId = "xlabel",
                  label = "Show label of X-Axis",
                  value = TRUE
                ),
                shiny::checkboxInput(
                  inputId = "grid",
                  label = "Display a grid",
                  value = FALSE
                ),
                bsplus::use_bs_popover(),
                bsplus::use_bs_tooltip()
              ),
              icon = icon('eye')
            ),
            #### COLOUR OPTIONS TAB####
            shiny::tabPanel("Colour Options",
              shiny::wellPanel(class = "myclass9", id = "myid9",
                #color information for wellpanel
                shiny::uiOutput('cont_well9'),
                mod_color_ui("color")
                #shiny::uiOutput('Panel_Colour')
              ),
              bsplus::use_bs_popover(),
              bsplus::use_bs_tooltip(),
              icon = icon("paint-brush")
            )
          )
        ),
        #### Module call: graph_ui ####
        shiny::column(6,
          mod_graph_ui("graph1",
            plotHeight = 700,
            plotWidth = "100%"
          )
        ),
        shiny::column(3,
          shinyWidgets::prettyToggle(
            inputId = 'showPanel2',
            label_off = 'Interaction Plot',
            label_on = 'Interaction Plot',
            value = FALSE,
            outline = TRUE,
            status_on = "default",
            status_off = "default",
            plain = TRUE,
            icon_off = icon("chart-line"),
            icon_on = icon ("times")
          ),
          shiny::conditionalPanel(
            condition = 'input.showPanel2',
            shiny::uiOutput("interaction_panel")
          ),
          bsplus::use_bs_popover(),
          bsplus::use_bs_tooltip(),
          shinyWidgets::prettyToggle(
            inputId = 'showPanelLegend',
            label_off = 'Legend',
            label_on = 'Legend',
            value = TRUE,
            outline = TRUE,
            status_on = "default",
            status_off = "default",
            plain = TRUE,
            icon_off = icon("list-ul"),
            icon_on = icon ("times")
          ),
          shiny::conditionalPanel(
            condition = 'input.showPanelLegend',
             mod_legend_ui("legend1")
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(12,
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel(
              "Selected Subgroups",
                DT::dataTableOutput("selectedSG"),
              icon = icon("circle")
            ),
            shiny::tabPanel(
              title = "Filtered Subgroups",
              DT::dataTableOutput("filteredSG"),
              icon = icon("filter")
            ),
            shiny::tabPanel(
              title = "Parent Subgroups",
              value = "ParentSubgroup",
              DT::dataTableOutput("parents"),
              icon = icon("sitemap")
            ),
            shiny::tabPanel(
              title = "Factorial Contexts",
              value = "FactorialSubgroup",
              DT::dataTableOutput("factorial"),
              icon = icon("list")
            ),
            shiny::tabPanel(
              title ="Subgroup Complement",
              value = "ComplementSubgroup",
              DT::dataTableOutput("complement"),
              icon = icon("times-circle")
            ),
            shiny::tabPanel(
              title = "Memorized Subgroups",
              shiny::column(12,
                shinyWidgets::prettySwitch(
                  inputId = "memorized_labels_on_off",
                  label = "Show labels for memorized subgroups",
                  value = FALSE,
                  status = "info"
                )
              ),
              shiny::column(12,
                DT::dataTableOutput("memorizedSG")
              ),
              icon = icon("edit")
            )
          )
        )
      )
    ), fluid = FALSE, position = c("static-top"), inverse = FALSE, icon = icon("braille")
  ),
  #### Module call: comparer_ui ####
  shiny::tabPanel(
    title = "Comparer",
    value = "SubscreenComparer",
    mod_comparer_ui("comparer"),
    icon = icon("object-group")
  ),
   #### Module call: mosaic_ui ####
  shiny::tabPanel(
    title = "Mosaic",
    value = "SubscreenMosaic",
    mod_mosaic_ui("mosaic"),
    icon = icon("th-list")
  ),
  shiny::tabPanel(
    title = "ASMUS",
    value = "SubscreenAsmus",
    asmus2_module_ui("asmus2"),
    icon = icon("tasks")
  ),
  shiny::tabPanel(
    title = "Upload",
    value = "SubscreenUpload",
    app_background_color = "#393939",
    shiny::uiOutput('cont0'),
    upload_tab_ui("upload_tab_ui_1", bg.col = "#424242"),
    icon = icon("upload")
  )
)


#### SERVER ####
server <- function(input, output, session) {

  options(shiny.maxRequestSize = 300*1024^2)

  js$disableTab("SubscreenExplorer")
  js$disableTab("SubscreenComparer")
  js$disableTab("SubscreenMosaic")
  js$disableTab("SubscreenAsmus")

  ## If the upload tab is actived, update TabsetPanel to start with Explorer-tab
  shiny::observeEvent(scresults_tmp$dat, {
    if (is.null(scresults_tmp$dat)) {
    shiny::updateTabsetPanel(
      session,
      inputId = "navpanel",
      selected = "SubscreenUpload"
    )
    } else {
      shiny::updateTabsetPanel(
      session,
      inputId = "navpanel",
      selected = "SubscreenExplorer"
    )
    }
  }, ignoreNULL = FALSE)
  # update slider when data are available
  # (app can than start without data)
  shiny::observeEvent(scresults_tmp$dat, {
    shiny::updateSelectInput(
      session,
      inputId = "y",
      choices = names(scresults_tmp$dat$results_total[ ,!is.na(scresults_tmp$dat$results_total)]),
      selected = names(scresults_tmp$dat$results_total[ ,!is.na(scresults_tmp$dat$results_total)])[1]
    )
    shiny::updateSelectInput(
      session,
      inputId = "x",
      choices = names(scresults_tmp$dat$results_total[ ,!is.na(scresults_tmp$dat$results_total)]),
      selected = "N.of.subjects"
    )
    shiny::updateSelectInput(
      session,
      inputId = "filter",
      choices = c("no selection", scresults_tmp$dat$factors)
    )
    shiny::observeEvent(input$y, {
      shiny::req(input$y)
      shiny::req(scresults_tmp$dat)
      if (roundDownNice(min(scresults_tmp$dat$sge[, input$y], na.rm = TRUE), nice = apppars$NiceNumbers) <= 0) {
        shiny::updateRadioButtons(
          inputId = "plot_type",
          label = "Plot Type",
          choices = c(linear = "lin"),
          selected = "lin",
          inline = TRUE
        )
      } else {
        shiny::updateRadioButtons(
          inputId = "plot_type",
          label = "Plot Type",
          choices = c(linear = "lin", logarithmic = "log"),
          selected = "lin",
          inline = TRUE
        )
      }
    })
    shiny::updateSelectInput(
      session,
      inputId = "filter2",
      choices = c("no selection", scresults_tmp$dat$factors)
    )
    shiny::updateSliderInput(
      session,
      inputId = "key",
      min = scresults_tmp$dat$min_comb,
      max = scresults_tmp$dat$max_comb,
      value = c(1, min(c(3, scresults_tmp$dat$max_comb), na.rm = TRUE))
    )
  })

  #Helper Function for Memorize Subgroup Table
  shinyInput_remove <- function (FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, as.numeric(strsplit(select_button_reac$val, "_")[[1]][2])), ...))
    }
    inputs
  }

  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, click_points_data$xy$SGID[i]), ...))
    }
    inputs
  }

  #### DATA FRAMES ####
  plot_points_data_complement <- shiny::reactive({
    shiny::req(input$y)
    if (!is.null(scresults_tmp$dat)) {
      if (input$y != "N.of.subjects" & any(startsWith(colnames(scresults_tmp$dat$sge), "Complement_"))
            & !is.null(input$selectedSG_rows_selected)) {
        IDs <- selected_ids$val
        if (!is.null(IDs)) {
          if (!is.integer0(IDs)) {
            if (!is.na(IDs)) {
              tmp <- scresults_tmp$dat$sge[,c("SGID","N.of.subjects", colnames(scresults_tmp$dat$sge[startsWith(colnames(scresults_tmp$dat$sge), "Complement_")]))]
              tmp$color <- "#fffb00"
              tmp <- tmp[tmp$SGID %in% IDs, ]
              tmp$ID <- paste0("Complement of SGID ", IDs)
              tmp$N.of.subjects.complement <- scresults_tmp$dat$results_total$N.of.subjects - tmp$N.of.subjects
              tmp
            } else {NULL}
          } else {NULL}
        } else {NULL}
      } else {NULL}
    }
  })

  plot_points_data <- shiny::reactive({
    shiny::req(input$x, input$y, input$key)
    data.frame(
      x = scresults_tmp$dat$sge[, c(input$x)][scresults_tmp$dat$sge$nfactors >= input$key[1] & scresults_tmp$dat$sge$nfactors <= input$key[2]],
      y = scresults_tmp$dat$sge[, c(input$y)][scresults_tmp$dat$sge$nfactors >= input$key[1] & scresults_tmp$dat$sge$nfactors <= input$key[2]],
      ID = scresults_tmp$dat$sge[, "SGID"][scresults_tmp$dat$sge$nfactors >= input$key[1] & scresults_tmp$dat$sge$nfactors <= input$key[2]]
    )
  })

  #### selected_ids$val ####
  # create a reactiveValue with the SGID, which is selected in the
  # selected subgroups table

  selected_ids <- shiny::reactiveValues(val = NULL)

  shiny::observeEvent(c(click_points_data$xy,scresults_tmp$dat), {
    selected_ids$val <- NULL
  })

  shiny::observeEvent(input$selectedSG_rows_selected, {
    if (is.integer0(click_points_data$xy[input$selectedSG_rows_selected,'SGID'])) {
      selected_ids$val <- NULL
    } else {
      selected_ids$val <- click_points_data$xy[input$selectedSG_rows_selected,'SGID']
    }
  }, ignoreNULL = FALSE)

  # reactive parent subgroup data frame (start value: NULL)
  df_parent <- shiny::reactiveValues(data = data.frame(NULL))

  shiny::observeEvent(c(input$selectedSG_rows_selected, click_points_data$xy), ignoreNULL = FALSE, {

    SGID_clicked <- selected_ids$val

    if (!is.null(SGID_clicked)) {
      if (!is.integer0(SGID_clicked)) {
        if (!is.na(SGID_clicked)) {
          df_parent <- parents(scresults_tmp$dat, SGID_clicked)
        } else { df_parent <- NULL}
      } else { df_parent <- NULL}
    } else { df_parent <- NULL}

    if (is.null(dim(df_parent$Parents))) {

      empty_data <- scresults_tmp$dat$sge[0, c("SGID", colnames(scresults_tmp$dat$results_total))]
      if (!is.null(empty_data)) {
        if (dim(empty_data)[2] > 5) {
            empty_data <- empty_data[,1:5]
        }
      }
      tmp <- DT::datatable(
        data = empty_data,
        extensions = 'Buttons',
        options = list(
          language = list(emptyTable = 'Select a subgroup by clicking on a row in the "Selected Subgroups"-list!'),
          initComplete = DT::JS(
          "function(settings, json) {",
          paste0("$(this.api().table().header()).css({'background-color': '",
            colthemeCol$col.bg,
            "', 'color': '",
            font_color(different_hues(colthemeCol$col.bg)),
            "'});"
          ),"}"
        ),
        dom = 'Brtip',
        buttons = c('copy','print','pageLength',I('colvis')),
        lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
        pageLength = 6
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of Parent Subgroups',
        filter = 'top'
      )
    } else {
      if (input$navpanel == "SubscreenExplorer") {
        curr_x <- shiny::req(input$x)
      } else if (input$navpanel == "SubscreenComparer") {
        curr_x <- shiny::req(input$x2)
      }

      df_par <- subset(
        df_parent$Parents,
        select = c("SGID", x = curr_x, y = input$y, "nfactors", scresults_tmp$dat$factors)
      )

      col2hide <- which(sapply(df_par, FUN = function(x){all(x == 'Not used')})) - 1
      names(col2hide) <- NULL

      tmp <- DT::datatable(
        data = df_par,
        extensions = 'Buttons',
        options = list(initComplete = DT::JS(
          "function(settings, json) {",
          paste0("$(this.api().table().header()).css({'background-color': '",
           colthemeCol$col.bg,
           "', 'color': '",
           font_color(different_hues(colthemeCol$col.bg)),
           "'});"
          ),"}"
        ),
        columnDefs = list(list(targets = col2hide, visible = FALSE)),
        dom = 'Brtip',
        buttons = c('copy','print','pageLength',I('colvis')),
        lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
        pageLength = 6
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of Parent Subgroups',
        filter = 'top'
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = 1:(ncol(df_par) + 1),
        target = "cell",
        backgroundColor = different_hues(colthemeCol$col.bg),
        border = paste0('.5px solid ', colthemeCol$ColorBGplot)
      )

      tmp.sglev <- levels(relevel(factor(unlist(lapply(df_par[, scresults_tmp$dat$factors], as.character))), ref = 'Not used'))
      colXY <- which(colnames(df_par) %in% c('SGID', names(scresults_tmp$dat$results_total), 'nfactors'))

      col.tabFont <- font_color(different_hues(colthemeCol$col.bg))
      col.tabBack <- colthemeCol$col.bg
      tmp <- DT::formatStyle(
        table = tmp,
        columns = colXY,
        color = col.tabFont
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = scresults_tmp$dat$factors,
        color = DT::styleEqual(
          tmp.sglev, c(col.tabBack, rep(col.tabFont, length(tmp.sglev) - 1))
        )
      )
    }
    output$parents<- DT::renderDataTable(tmp)
  })

  output$interaction_panel <- shiny::renderUI({
    shiny::wellPanel(
      style = paste0("background:" , colthemeCol$col.bg),
      shiny::fluidRow(
        shiny::plotOutput(outputId = 'interaction')
      ),
      shiny::fluidRow(
        shiny::column(12,
          shiny::radioButtons(
            inputId = 'y_Interaction_Button',
            label = 'Synchronise y-axes with main plot',
            selected = ("Synchron"),
            choices = c("Synchron","Optimal"),
            inline = TRUE
          )
        )
      )
    )
  })

  js$disableTab("ImportanceTab")
  shiny::observe({
    if (!is.null(variable_importance_tmp$dat)) {
      js$enableTab("ImportanceTab")
    }
  })

  js$disableTab("ComplementSubgroup")

  shiny::observe({
    if(!is.null(scresults_tmp$dat)) {
      if (any(startsWith(colnames(scresults_tmp$dat$sge), "Complement_"))) {
        js$enableTab("ComplementSubgroup")
      }
    }
  })

  js$disableTab("ParentSubgroup")
  shiny::observe({
    if(!is.null(scresults_tmp$dat)) {
      if (scresults_tmp$dat$max_comb > 1) {
        js$enableTab("ParentSubgroup")
      }
    }
  })

  shinyjs::useShinyjs(debug = TRUE)
  shinyjs::disable("ColorImportance")

  shiny::observe({
    if (!is.null(variable_importance_tmp$dat)) {
      shinyjs::enable("ColorImportance")
    }
  })

  shinyjs::disable("ColorParents")

  shiny::observe({
    if (!is.null(scresults_tmp$dat)) {
      if (scresults_tmp$dat$max_comb > 1) {
        shinyjs::enable("ColorParents")
      }
    }
  })

  output$cont_nav <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        paste0(
          ".navbar { background-color:",
          colthemeCol$col.bg,
          " ;font-family: Arial;font-size: 15px; color: ",
          font_color(colthemeCol$col.bg),
          "; }',
          '.navbar-default .navbar-brand {
          color: ",
          font_color(colthemeCol$col.bg),
          ";
          font-size: 40px;
          font-family: Arial;}"
        )
      )
    )
  })

  output$cont <- shiny::renderUI({
    list(
      shiny::tags$head(
        shiny::tags$style(
          paste(
            "body {background-color: ",
            colthemeCol$col.bg,
            "; color: ",
            font_color(colthemeCol$col.bg),
            "}",
             "
          .tabbable > .nav > li[class=active]    > a {background-color: black; color:white}
          }",
            sep = ""
          )
        )
      )
    )
  })

  output$cont2 <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        paste0(
          ".fa-bug {color:#D30F4B}",
          ".fa-times-circle{color: #fffb00}",
          ".fa-th-list {color:grey}",
          ".fa-circle {color:", colthemeCol$ColorClicked, "}",
          "fa-info-circle {color:#DE0043FF}",
          ".fa-filter {color:", colthemeCol$ColorSelected, "}",
          ".fa-delicious {color:#00aaff}",
          ".fa-braille {color: grey}",
          ".fa-list {color:", colthemeCol$ColorFact, "}",
          ".fa-sitemap {color: ", colthemeCol$ColorParents, "}",
          ".fa-clipboard {color: #e2b007}",
          ".fa-edit {color: ", colthemeCol$ColorMemorized, "}",
          ".fa-object-group {color: grey}",
          shiny::HTML(
            ".selectize-input.input-active, .selectize-input.input-active:hover, .selectize-control.multi .selectize-input.focus {border-color: red !important;}\n                                     .selectize-dropdown .active {background: #FF3162FF !important;}"
          ), sep = ","
        )
      )
    )
  })

  output$cont_well  <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        type = 'text/css',
        paste0(".myclass1 {background-color: ", colthemeCol$col.bg, ";}")
      )
    )
  })

  output$VarChosen <- shiny::renderUI({
    if (input$filter != 'no selection') {
      choices <- c(as.character(unique(scresults_tmp$dat$sge[, input$filter])))
      choices <- choices[-which(choices == "Not used")]
      selected <- choices[1]
      shiny::selectInput(
        inputId = "VarChosen",
        label = "Choose a value (1)",
        choices = choices,
        selected = selected
      )
    }
  })

  output$VarChosen2 <- shiny::renderUI({
    if (input$filter2 != 'no selection') {
      choices <- c(as.character(unique(scresults_tmp$dat$sge[, input$filter2])))
      choices <- choices[-which(choices == "Not used")]
      selected <- choices[1]
      shiny::selectInput(
        inputId = "VarChosen2",
        label = "Choose a value (2)",
        choices = choices,
        selected = selected
      )
    }
  })

  output$YRange <- shiny::renderUI({
    shiny::req(input$y)
     if (input$plot_type == "lin") {

      shiny::sliderInput(
        inputId = "YRange",
        label = "Y Range",
        min = roundDownNice(min(scresults_tmp$dat$sge[, input$y], na.rm = TRUE), nice = apppars$NiceNumbers),
        max = roundUpNice(max(scresults_tmp$dat$sge[, input$y], na.rm = TRUE), nice = apppars$NiceNumbers),
        value = c(roundDownNice(min(scresults_tmp$dat$sge[, input$y], na.rm = TRUE), nice = apppars$NiceNumbers), roundUpNice(max(scresults_tmp$dat$sge[, input$y], na.rm = TRUE), nice = apppars$NiceNumbers)),
        step = roundUpNice((max(scresults_tmp$dat$sge[, input$y], na.rm = TRUE) - min(scresults_tmp$dat$sge[, input$y], na.rm = TRUE))/100, nice = apppars$NiceNumbers)
      )
     } else {
      rg.z <- log(
        range(
          roundDownNice(
            min(scresults_tmp$dat$sge[, input$y], na.rm = TRUE), nice = apppars$NiceNumbers
          ),
          roundUpNice(
            max(scresults_tmp$dat$sge[, input$y], na.rm = TRUE), nice = apppars$NiceNumbers
          )
        )
      )
      choices <- unique(unlist(lapply(exp(seq(rg.z[1], rg.z[2], length.out = 20)), function(x){signif(x, 2)})))
      shinyWidgets::sliderTextInput(
        inputId = "YRange",
        label = "Log Y Range:",
        hide_min_max = TRUE,
        choices = choices,
        selected = c(choices[1],choices[length(choices)]),
        grid = TRUE
      )
     }
  })

  output$XRange <- shiny::renderUI({
    shiny::req(input$x)
      mini <- roundDownNice(min(scresults_tmp$dat$sge[, input$x], na.rm = TRUE) ,nice = apppars$NiceNumbers)
      maxi <- roundUpNice(max(scresults_tmp$dat$sge[, input$x], na.rm = TRUE) ,nice = apppars$NiceNumbers)

      twentyPercent <- roundUpNice(abs(diff(c(maxi,mini))/5), nice = apppars$NiceNumbers)
      mini_20percent <- mini - twentyPercent
      maxi_20percent <- maxi + twentyPercent
      shiny::sliderInput(
        inputId = "XRange",
        label = "X Range",
        min = mini_20percent,
        max = maxi_20percent,
        value = c(mini, maxi),
        step = roundUpNice(diff(c(mini,maxi))/100,nice = apppars$NiceNumbers)
      )
  })



  vi_names <- shiny::reactive({
    if (is.data.frame(variable_importance_tmp$dat)) {
      "NULL"
    } else if (is.list(variable_importance_tmp$dat)) {
      names(variable_importance_tmp$dat)
    } else  {
      "NULL"
    }
  })

  output$cont_well6 <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        type = 'text/css',
        paste0(".myclass6 {background-color: ",
               colthemeCol$col.bg,";}"
        )
      )
    )
  })

  output$cont_well7 <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        type = 'text/css',
        paste0(".myclass7 {background-color: ", colthemeCol$col.bg, ";}")
      )
    )
  })

  output$cont_well9 <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        type = 'text/css',
        paste0(".myclass9 {background-color: ", colthemeCol$col.bg, ";}")
      )
    )
  })

  # Interaction plot
  y_axe_Int <- shiny::reactive({
    shiny::req(input$y_Interaction_Button)

    if (input$y_Interaction_Button == "Synchron") {
      tmp <- c(input$YRange[1], input$YRange[2])
    }
    if (input$y_Interaction_Button == "Optimal") {
      tmp <- c("NA","NA")
    }
    tmp
  })

  df_factorial <- shiny::reactive({
    shiny::req(input$y)

    SGID_clicked <- selected_ids$val
    if (!is.null(SGID_clicked)) {
      if(!is.integer0(SGID_clicked)) {
        if(!is.na(SGID_clicked)) {
           tmp <- scresults_tmp$dat$sge[scresults_tmp$dat$sge$FCID_all == scresults_tmp$dat$sge[scresults_tmp$dat$sge$SGID == SGID_clicked,]$FCID_all,]
        } else { tmp <- scresults_tmp$dat$sge[0,]}
      } else { tmp <- scresults_tmp$dat$sge[0,]}
    } else { tmp <- scresults_tmp$dat$sge[0,]}

    if (!any(startsWith(colnames(tmp),"FCID_complete_")) & dim(tmp)[1] != 0) {
      tmp <- pseudo_contexts(tmp, input$y, scresults_tmp$dat$factors)
    }
    if (dim(tmp)[1] == 0) {
      df_factorial <- NULL
    } else {
    # 1. case: Context complete:
    if (all(tmp[paste0("FCID_incomplete_", input$y)] == "Complete")) {
      df_factorial <- tmp
    }
    #2. case: Context incomplete:
    if (all(tmp[paste0("FCID_incomplete_", input$y)] == "Incomplete")) {
      df_factorial <- NULL
    }
    #3. case: Context pseudo complete
      if (all(tmp[paste0("FCID_incomplete_", input$y)] == "Pseudo complete")) {
        df_factorial <- tmp[tmp[paste0("FCID_pseudo_", input$y)] != "No Pseudo",]
      }
    }
    df_factorial
  })

  shiny::observe({
    if (!is.null(scresults_tmp$dat)) {
    output$interaction <- shiny::renderPlot({

      shiny::req(y_axe_Int())
      y_axe <- y_axe_Int()

      df_factorial <- df_factorial()

      # if no subgroup is selected, return a help text
      if (is.null(input$selectedSG_rows_selected)) {
        plot(
          NULL,
          xlim = c(0, 1),
          ylim = c(0, 1),
          axes = FALSE,
          xlab = "",
          ylab = ""
        )
        graphics::rect(
          xleft = graphics::grconvertX(0,'ndc','user') - 1000,
          xright = graphics::grconvertX(1,'ndc','user') + 1000,
          ybottom = graphics::grconvertY(0,'ndc','user') - 1000,
          ytop = graphics::grconvertY(1,'ndc','user') + 1000,
          border = NA,
          col = colthemeCol$ColorBGplot,
          xpd = TRUE
        )
        text(
          0.5,
          0.5,
          "Please select a Subgroup!",
          col = font_color(colthemeCol$col.bg),
          cex = 1.4
        )
        text(
          0.5,
          0.4,
          "(Click on a point in the graphic",
          col = font_color(colthemeCol$col.bg),
          cex = 0.9
        )
        text(
          0.5,
          0.3,
          "and then select a subgroup in the",
          col = font_color(colthemeCol$col.bg),
          cex = 0.9
        )

        text(
          0.5,
          0.2,
          "'Selected Subgroup'-table by clicking on)",
          col = font_color(colthemeCol$col.bg),
          cex = 0.9
        )


       # Insert new if query, changed on 27-JUL-2022
        # if old data (< 4.0.0 subscreencalc) are uploaded
      } else if(!any(startsWith(colnames(scresults_tmp$dat$sge),"FCID_complete_"))
           & ("FCID_complete" %in% colnames(scresults_tmp$dat$sge))){
          plot(
            NULL,
            xlim = c(0, 1),
            ylim = c(0, 1),
            axes = FALSE,
            xlab = "",
            ylab = ""
          )
          graphics::rect(
            xleft = graphics::grconvertX(0,'ndc','user') - 1000,
            xright = graphics::grconvertX(1,'ndc','user') + 1000,
            ybottom = graphics::grconvertY(0,'ndc','user') - 1000,
            ytop = graphics::grconvertY(1,'ndc','user') + 1000,
            border = NA,
            col = colthemeCol$ColorBGplot,
            xpd = TRUE
          )
          text(
            0.5,
            0.5,
           "Please use package version (> 4.0.0)",
            col = font_color(colthemeCol$col.bg),
            cex = 1.4
          )
          text(
            0.5,
            0.4,
            " of subscreencalc to use the Interaction Plot.",
            col = font_color(colthemeCol$col.bg),
            cex = 1.4
          )

          # if df_factorial is null
        } else if (is.null(df_factorial)) {

        plot(
          NULL,
          xlim = c(0, 1),
          ylim = c(0, 1),
          axes = FALSE,
          xlab = "",
          ylab = ""
        )
        graphics::rect(
          xleft = graphics::grconvertX(0,'ndc','user') - 1000,
          xright = graphics::grconvertX(1, 'ndc', 'user') + 1000,
          ybottom = graphics::grconvertY(0,'ndc','user') - 1000,
          ytop = graphics::grconvertY(1, 'ndc', 'user') + 1000,
          border = NA,
          col = colthemeCol$ColorBGplot,
          xpd = TRUE
        )
        text(
          0.5,
          0.5,
          "Incomplete factorial context!",
          col = font_color(colthemeCol$col.bg),
          cex = 1.4
        )
        text(
          0.5,
          0.4,
          "(This graphic is not available",
          col = font_color(colthemeCol$col.bg),
          cex = 0.9
        )
        text(
          0.5,
          0.3,
          "for incomplete factorial contexts)",
          col = font_color(colthemeCol$col.bg),
          cex = 0.9
        )

      } else if (all(df_factorial$nfactors > 3)) {

        plot(
          NULL,
          xlim = c(0, 1),
          ylim = c(0, 1),
          axes = FALSE,
          xlab = "",
          ylab = ""
        )

        graphics::rect(
          xleft = graphics::grconvertX(0,'ndc','user') - 1000,
          xright = graphics::grconvertX(1, 'ndc', 'user') + 1000,
          ybottom = graphics::grconvertY(0,'ndc','user') - 1000,
          ytop = graphics::grconvertY(1, 'ndc', 'user') + 1000,
          border = NA,
          col = colthemeCol$ColorBGplot,
          xpd = TRUE
        )
        text(
          0.5,
          0.5,
          "Too many factors!",
          col = font_color(colthemeCol$col.bg),
          cex = 1.4
        )
        text(
          0.5,
          0.4,
          "(This graphic is not available",
          col = font_color(colthemeCol$col.bg),
          cex = 0.9
        )
        text(
          0.5,
          0.3,
          "for 4 or more subgroup levels)",
          col = font_color(colthemeCol$col.bg),
          cex = 0.9
        )

      } else {

        factor_used <- names(which(apply(df_factorial[,scresults_tmp$dat$factors], 2, function(x){all(x != "Not used")})))

        if (input$plot_type == "log") {
          type <- "y"
        } else {
          type <- ""
        }

        interaction_plot2(
          df_data = df_factorial,
          fac1 = factor_used[1],
          fac2 = factor_used[2],
          fac3 = factor_used[3],
          response = input$y,
          bg.col = colthemeCol$ColorBGplot,
          bg.col2 = different_hues(colthemeCol$col.bg),
          font.col = font_color(colthemeCol$col.bg),
          y.min = y_axe[1],
          y.max = y_axe[2],
          box.col = font_color(colthemeCol$col.bg),
          plot_type = type
        )

      }
    })
    }
  })

  shiny::observeEvent(c(input$selectedSG_rows_selected), ignoreNULL = FALSE, {

    if (!is.null(scresults_tmp$dat)) {

      SGID_clicked <- selected_ids$val

        if (!is.null(SGID_clicked)) {
          if(!is.integer0(SGID_clicked)) {
            if(!is.na(SGID_clicked)) {
                df_factorial <- scresults_tmp$dat$sge[scresults_tmp$dat$sge$FCID_all == scresults_tmp$dat$sge[scresults_tmp$dat$sge$SGID == selected_ids$val,]$FCID_all,]
            } else { df_factorial <- scresults_tmp$dat$sge[0,]}
          } else { df_factorial <- scresults_tmp$dat$sge[0,]}
        } else { df_factorial <- scresults_tmp$dat$sge[0,]}


      if (dim(df_factorial)[1] == 0) {
        empty_data <- scresults_tmp$dat$sge[0,c("SGID", colnames(scresults_tmp$dat$results_total))]
        if (dim(empty_data)[2] > 5) {
          empty_data <- empty_data[,1:5]
        }
        tmp <- DT::datatable(
          data = empty_data,
          extensions = 'Buttons',
          options = list(
            language = list(emptyTable = 'Select a subgroup by clicking on a row in the "Selected Subgroups"-list!'),
            initComplete = DT::JS(
             "function(settings, json) {",
             paste0("$(this.api().table().header()).css({'background-color': '",
                    colthemeCol$col.bg,
                    "', 'color': '",
                    font_color(different_hues(colthemeCol$col.bg)),
                    "'});"
             ),
             "}"
            ),
            dom = 'Brtip',
            buttons = c('copy','print','pageLength',I('colvis')),
            lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
            pageLength = 6
          ),
          class = 'cell-border stripe',
          rownames = FALSE,
          caption = 'Table of Factorial Contexts',
          filter = 'top'
        )
      } else {

        tmp.sglev <- levels(
          relevel(
            factor(
              unlist(
                lapply(df_factorial[, scresults_tmp$dat$factors], as.character)
              )
            ), ref = 'Not used'
          )
        )

        col.tabFont <- font_color(different_hues(colthemeCol$col.bg))
        col.tabBack <- colthemeCol$col.bg

        if(input$navpanel == "SubscreenExplorer") {
          curr_x <- shiny::req(input$x)
        } else if (input$navpanel == "SubscreenComparer") {
          curr_x <- shiny::req(input$x2)
        }

        df_fac <- subset(
          df_factorial,
          select = c("SGID", x = curr_x, y = input$y, "nfactors", scresults_tmp$dat$factors)
        )

        colXY <- which(colnames(df_fac) %in% c('SGID', names(scresults_tmp$dat$results_total), 'nfactors'))

        tmp <- DT::datatable(
          data = df_fac,
          extensions = 'Buttons',
          options = list(
            initComplete = DT::JS(
             "function(settings, json) {",
             paste0("$(this.api().table().header()).css({'background-color': '",
                    colthemeCol$col.bg,
                    "', 'color': '",
                    font_color(different_hues(colthemeCol$col.bg)),
                    "'});"
             ),
             "}"
            ),
            dom = 'Brtip',
            buttons = c('copy','print','pageLength',I('colvis')),
            lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
            pageLength = 6
          ),
          class = 'cell-border stripe',
          rownames = FALSE,
          caption = 'Table of Factorial Contexts',
          filter = 'top'
        )

        tmp <- DT::formatStyle(
          table = tmp,
          columns = colXY,
          color = col.tabFont
        )

        tmp <- DT::formatStyle(
          table = tmp,
          columns = scresults_tmp$dat$factors,
          color = DT::styleEqual(
            tmp.sglev,
            c(col.tabBack, rep(col.tabFont,length(tmp.sglev) - 1))
          )
        )

        tmp <- DT::formatStyle(
          table = tmp,
          columns = 1:(ncol(df_fac)),
          target = "cell",
          backgroundColor = different_hues(colthemeCol$col.bg),
          border = paste0('.5px solid ', colthemeCol$ColorBGplot)
        )
      }
      output$factorial <- DT::renderDataTable(tmp)

      y_axe_Int <- shiny::reactive({
        shiny::req(input$y_Interaction_Button)

        if (input$y_Interaction_Button == "Synchron") {
          tmp <- c(input$YRange[1], input$YRange[2])
        }
        if (input$y_Interaction_Button == "Optimal") {
          tmp <- c("NA", "NA")
        }
        tmp
      })
    }
  })

  # create filtered subgroups table in subgroup explorer tab
  shiny::observeEvent(c(input$filter, input$filter2, input$VarChosen, input$VarChosen2, scresults_tmp$dat, input$y), {
    if(!is.null(scresults_tmp$dat)) {

    filt <- input$filter
    filt2 <- input$filter2

    key <- shiny::req(input$key)

    if (filt != "no selection" & filt2 == "no selection") {
      choice <- input$VarChosen
      select_points_data <- scresults_tmp$dat$sge[which(scresults_tmp$dat$sge$nfactors >=
                                                  input$key[1] & scresults_tmp$dat$sge$nfactors <=
                                                   input$key[2] &
                                                   scresults_tmp$dat$sge[, c(filt)] == choice),]
    } else if (filt != "no selection" & filt2 != "no selection") {
      choice <- input$VarChosen
      choice2 <- input$VarChosen2
      select_points_data <- scresults_tmp$dat$sge[which(scresults_tmp$dat$sge$nfactors >=
                                                  input$key[1] & scresults_tmp$dat$sge$nfactors <=
                                                   input$key[2] &
                                                   scresults_tmp$dat$sge[, c(filt)] == choice &
                                                   scresults_tmp$dat$sge[, c(filt2)] == choice2
                                                  ),]
    } else {
      select_points_data <- data.frame(x = numeric(), y = numeric(), SGID = numeric())
    }

    if (filt == "no selection") {

    empty_data <- scresults_tmp$dat$sge[0,c("SGID", colnames(scresults_tmp$dat$results_total))]
    if (dim(empty_data)[2] > 5) {
      empty_data <- empty_data[,1:5]
    }


     tmp <- DT::datatable(
        data = empty_data ,
        extensions = 'Buttons',
        options = list(
          language = list(emptyTable = 'To get specific subgroups listed here, use the "Subgroup Filter"-option in the "Variable Options"-tab!'),
          initComplete = DT::JS(
            "function(settings, json) {",
            paste0("$(this.api().table().header()).css({'background-color': '",
              colthemeCol$col.bg,
              "', 'color': '",
              font_color(different_hues(colthemeCol$col.bg)),
              "'});"
            ),
            "}"
          ),
          dom = 'Brtip',buttons = c('copy', 'print', 'pageLength', I('colvis')),
          lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
          pageLength = 6
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of Filtered Subgroups',
        filter = 'top'
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = 1:(ncol(empty_data) + 1),
        target = "cell",
        backgroundColor = different_hues(colthemeCol$col.bg),
        border = paste0('.5px solid ', colthemeCol$col.bg)
      )
      output$filteredSG <- DT::renderDataTable(tmp)
    }

    if (filt != "no selection") {
      df_filt <- subset(select_points_data, select = c(x = input$x, y = input$y, "nfactors", scresults_tmp$dat$factors))

      col2hide <- which(sapply(df_filt, FUN = function(x){all(x == 'Not used')})) - 1
      names(col2hide) <- NULL
      tmp <- DT::datatable(
        data = df_filt ,
        extensions = 'Buttons',
        options = list(
          initComplete = DT::JS(
            "function(settings, json) {",
            paste0("$(this.api().table().header()).css({'background-color': '",
              colthemeCol$col.bg,
              "', 'color': '",
              font_color(different_hues(colthemeCol$col.bg)),
              "'});"
            ),
            "}"
          ),
          columnDefs = list(list(targets = col2hide, visible = FALSE)),
          dom = 'Brtip',buttons = c('copy', 'print', 'pageLength', I('colvis')),
          lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
          pageLength = 6
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of Filtered Subgroups',
        filter = 'top'
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = 1:ncol(df_filt),
        target = "cell",
        backgroundColor = different_hues(colthemeCol$col.bg),
        border = paste0('.5px solid ',colthemeCol$ColorBGplot)
      )

      if (dim(df_filt)[1] != 0) {
        tmp.sglev <- levels(
          relevel(
            factor(
              unlist(
                unique(
                  lapply(df_filt, as.character)
                )
              )
            ), ref = "Not used"
          )
        )
        colXY <- which(
          colnames(
            subset(
              df_filt,
              select = c(x = shiny::req(input$x), y = input$y, 'nfactors', scresults_tmp$dat$factors)
            )
          ) %in% c('SGID', names(scresults_tmp$dat$results_total), 'nfactors')
        )

        col.tabFont <- font_color(different_hues(colthemeCol$col.bg))
        col.tabBack <- colthemeCol$col.bg

        tmp <- DT::formatStyle(
          table = tmp,
          columns = colXY,
          color = col.tabFont
        )

        tmp <- DT::formatStyle(
          table = tmp,
          columns = scresults_tmp$dat$factors,
          color = DT::styleEqual(
            tmp.sglev, c('black', rep(col.tabFont, length(tmp.sglev) - 1))
          )
        )
      }
      output$filteredSG <- DT::renderDataTable(tmp)
    }
  }
  })

  shiny::observeEvent(c(selected_ids$val, input$selectedSG_rows_selected, plot_points_data_complement()),{
    if(!is.null(scresults_tmp$dat)) {
      if (shiny::req(input$y) != "N.of.subjects" & !is.null(plot_points_data_complement())) {

       dat <- data.frame(
          plot_points_data_complement()["ID"],
          plot_points_data_complement()["N.of.subjects.complement"],
          plot_points_data_complement()[paste0("Complement_",input$y)]
        )

        tmp <- DT::datatable(
          data = dat,
          extensions = 'Buttons',
          options= list(
            initComplete = DT::JS(
              "function(settings, json) {",
              paste0("$(this.api().table().header()).css({'background-color': '",
                colthemeCol$col.bg,
                "', 'color': '",
                font_color(different_hues(colthemeCol$col.bg)),
                "'});"
              ),
              "}"
            ),
            dom = 'Brtip',
            buttons=c('copy','print','pageLength',I('colvis')),
            lengthMenu = list(c(6, 12, -1), c("6", "12", "All")), pageLength = 6
          ),
          class = 'cell-border stripe', rownames = FALSE,
          caption = 'Table of Complement Subgroup(s)', filter = 'top'
        )
        tmp <- DT::formatStyle(
          table = tmp,
          columns = 1:(ncol(dat) + 1),
          target = "cell",
          backgroundColor = different_hues(colthemeCol$col.bg),
          border = paste0('.5px solid ', colthemeCol$ColorBGplot)
        )
        col.tabFont <- font_color(different_hues(colthemeCol$col.bg))
        col.tabBack <- colthemeCol$col.bg

        tmp <- DT::formatStyle(
          table = tmp,
          columns = colnames(dat),
          color = col.tabFont
        )
      } else {
         empty_data <- scresults_tmp$dat$sge[0,c("SGID", colnames(scresults_tmp$dat$results_total))]
          if (dim(empty_data)[2] > 5) {
           empty_data <- empty_data[,1:5]
          }
         tmp <- DT::datatable(
          data = empty_data,
          extensions = 'Buttons',
          options = list(
            language = list(emptyTable = 'Select a subgroup by clicking on a row in the "Selected Subgroups"-list!'),
            initComplete = DT::JS(
              "function(settings, json) {",
              paste0("$(this.api().table().header()).css({'background-color': '",
                     colthemeCol$col.bg,
                     "', 'color': '",
                     font_color(different_hues(colthemeCol$col.bg)),
                     "'});"
              ),
              "}"
            ),
            dom = 'Brtip',
            buttons=c('copy','print','pageLength',I('colvis')),
            lengthMenu = list(c(6, 12, -1), c("6", "12", "All")), pageLength = 6
          ),
          class = 'cell-border stripe', rownames = FALSE,
          caption = 'Table of Complement Subgroup(s)', filter = 'top'
        )
      }
      output$complement <- DT::renderDataTable(tmp)
    }
  },ignoreNULL = FALSE)

  # memorized data frame
  df_m <- shiny::reactiveValues(data = NULL)

  shiny::observeEvent(scresults_tmp$dat, {
    df_m$data <- NULL
    df_m$data <- shiny::req(scresults_tmp$dat$sge)[0,]
  })


  select_button_reac <- shiny::reactiveValues(val = NULL)

  shiny::observeEvent(input$select_button, {
    select_button_reac$val <- input$select_button
  })
   shiny::observeEvent(scresults_tmp$dat, {
    select_button_reac$val <- NULL
  })

  shiny::observeEvent(c(input$remove_button), {
    selectedRow <- as.numeric(strsplit(input$remove_button, "_")[[1]][2])
    df_m$data <- df_m$data[df_m$data$SGID != selectedRow,]
    selectRow <- NULL
  })

  shiny::observeEvent(c(select_button_reac$val,scresults_tmp$dat), {
    if (!is.null(shiny::req(select_button_reac$val))){

      selectedRow <- as.numeric(strsplit(select_button_reac$val, "_")[[1]][2])

      del <- cbind(
        data.frame(
          Delete = shinyInput_remove(
            actionButton,
            1,
            'button_',
            label = "Remove",
            onclick = 'Shiny.onInputChange(\"remove_button\",  this.id)'
          )
        ),
        scresults_tmp$dat$sge[scresults_tmp$dat$sge$SGID == selectedRow, ]
      )
      df_m$data <- rbind(df_m$data, del)
    } else {
      df_m$data <- NULL
    }
  })

  shiny::observeEvent(c(select_button_reac$val, input$remove_button, scresults_tmp$dat), {
    if (!is.null(scresults_tmp$dat)) {
      if (dim(df_m$data)[1] == 0) {
        empty_data <- scresults_tmp$dat$sge[0,c("SGID", colnames(scresults_tmp$dat$results_total))]
        if (dim(empty_data)[2] > 5) {
          empty_data <- empty_data[,1:5]
        }
        tmp <- DT::datatable(
        data = empty_data,
        extensions = 'Buttons',
        escape = FALSE,
        selection = 'none',
        options = list(
          language = list(emptyTable = 'Select a subgroup by clicking on a row in the "Selected Subgroups"-list!'),
          initComplete = DT::JS(
            "function(settings, json) {",
            paste0("$(this.api().table().header()).css({'background-color': '",
              colthemeCol$col.bg,
              "', 'color': '",
              font_color(different_hues(colthemeCol$col.bg)),
              "'});"
            ),
            "}"
          ),
          dom = 'Brtip',
          buttons = c('copy', 'print', 'pageLength', I('colvis')),
          lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
          pageLength = 6
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of Memorized Subgroups',
        filter = 'top'
      )
      } else {

      col2hide <- which(sapply(df_m$data[,-1], FUN = function(x){all(x == 'Not used')})) - 1
      names(col2hide) <- NULL

      tmp <- DT::datatable(
        data = df_m$data,
        extensions = 'Buttons',
        escape = FALSE,
        #selection = 'none',
        options = list(
          initComplete = DT::JS(
            "function(settings, json) {",
            paste0("$(this.api().table().header()).css({'background-color': '",
              colthemeCol$col.bg,
              "', 'color': '",
              font_color(different_hues(colthemeCol$col.bg)),
              "'});"
            ),
            "}"
          ),
          dom = 'Brtip',
          columnDefs = list(list(targets = col2hide, visible = FALSE)),
          buttons = c('copy', 'print', 'pageLength', I('colvis')),
          lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
          pageLength = 6
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of Memorized Subgroups',
        filter = 'top'
      )

      if (dim(df_m$data)[1] != 0) {

        tmp <- DT::formatStyle(
          table = tmp,
          columns = 1:(ncol(df_m$data[, -1]) + 1),
          target = "cell",
          backgroundColor = different_hues(colthemeCol$col.bg),
          border = paste0('.5px solid ',colthemeCol$ColorBGplot)
        )

        tmp.sglev <- levels(
          relevel(
            factor(
              unlist(
                lapply(df_m$data[, scresults_tmp$dat$factors], as.character)
              )
            ),
            ref = 'Not used'
          )
        )

        colXY <- which(colnames(df_m$data[, -1]) %in% c('SGID', names(scresults_tmp$dat$results_total), 'nfactors')) + 1

        col.tabFont <- font_color(different_hues(colthemeCol$col.bg))
        col.tabBack <- colthemeCol$col.bg

        tmp <- DT::formatStyle(
          table = tmp,
          columns = names(df_m$data[, -1]),
          color = col.tabFont
        )

        tmp <- DT::formatStyle(
          table = tmp,
          columns = scresults_tmp$dat$factors,
          color = DT::styleEqual(
            tmp.sglev,
            c(col.tabBack, rep(col.tabFont, length(tmp.sglev) - 1))
          )
        )
      }
    }
    output$memorizedSG <- DT::renderDataTable(tmp)
    }
  }, ignoreNULL = FALSE)

  output$cont_well5 <- shiny::renderUI({
   shiny::tags$head(
     shiny::tags$style(
        type = 'text/css',
        paste0(".myclass5 {background-color: ",colthemeCol$col.bg,";}")
      )
    )
  })

  output$mydropdown_bgcolor <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          paste0(
            "#dropdown-menu-MyDropDown {
            background-color: ",
            colthemeCol$ColorBGplot,
            " !important;}
            "
          )
        )
      )
    )
  })

  output$mydropdown_bgcolor2 <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          paste0(
            "#dropdown-menu-MyDropDown2 {
             background-color: ",
             colthemeCol$ColorBGplot,
             " !important;}
             "
          )
        )
      )
    )
  })

  #reference line value
  ref_line <- shiny::reactive({
    shiny::req(scresults_tmp$dat)
    scresults_tmp$dat$results_total[, c(input$y)]
  })

  #### MODULES ####
  #Module call: color_server
  mod_color_vars <- shiny::callModule(
    mod_color_server,
    "color"
  )


  colthemeCol <- shiny::reactiveValues(
    col.bg = '#383838',
    font.col = '#ffffff',
    panel.col = '#6b6b6b',
    ColorClicked = "#D30F4B",
    ColorSelected = "#89D329",
    ColorParents = "#ff6c00",
    ColorTabClicked = "#e2b007",
    ColorImportance = "#FA1BDC",
    ColorReference = "#0091DF60",
    ColorFactCont = "#0350E0",
    ColorBGplot = "#383838",
    ColorPoints = "#FFFFFF"
  )

  shiny::observeEvent(
    c(upload_data$parameter1(),
      mod_color_vars$colthemeCol()$ColorBGplot,
      mod_color_vars$colthemeCol()$ColorClicked,
      mod_color_vars$colthemeCol()$ColorSelected,
      mod_color_vars$colthemeCol()$ColorFactCont,
      mod_color_vars$colthemeCol()$ColorParents,
      mod_color_vars$colthemeCol()$ColorTabClicked,
      mod_color_vars$colthemeCol()$ColorImportance,
      mod_color_vars$colthemeCol()$ColorReference,
      mod_color_vars$colthemeCol()$ColorBGplot,
      mod_color_vars$colthemeCol()$ColorPoints,
      mod_color_vars$colthemeCol()$ColorMemorized
    ), {
    colthemeCol$col.bg <- mod_color_vars$colthemeCol()$ColorBGplot
    colthemeCol$ColorClicked <- mod_color_vars$colthemeCol()$ColorClicked
    colthemeCol$ColorSelected <- mod_color_vars$colthemeCol()$ColorSelected
    colthemeCol$ColorFactCont <- mod_color_vars$colthemeCol()$ColorFactCont
    colthemeCol$ColorParents <- mod_color_vars$colthemeCol()$ColorParents
    colthemeCol$ColorTabClicked <- mod_color_vars$colthemeCol()$ColorTabClicked
    colthemeCol$ColorImportance <- mod_color_vars$colthemeCol()$ColorImportance
    colthemeCol$ColorReference <- mod_color_vars$colthemeCol()$ColorReference
    colthemeCol$ColorBGplot <- mod_color_vars$colthemeCol()$ColorBGplot
    colthemeCol$ColorPoints <- mod_color_vars$colthemeCol()$ColorPoints
    colthemeCol$ColorMemorized <- mod_color_vars$colthemeCol()$ColorMemorized
  })

  if (asmus_version == 1) {

  #       #### Module call: asmus_server ####
  #       callModule(
  #         asmus_old_module_server,
  #         "asmus_old",
  #         results = scresults,
  #         x = shiny::reactive({input$x}),
  #         y = shiny::reactive({input$y}),
  #         ColorReference = shiny::reactive({colthemeCol$ColorReference}),
  #         ColorBGplot = shiny::reactive({colthemeCol$ColorBGplot}),
  #         ColorPoints = shiny::reactive({colthemeCol$ColorPoints}),
  #         ColorClicked = shiny::reactive({colthemeCol$ColorClicked}),
  #         ColorSelected = shiny::reactive({colthemeCol$ColorSelected}),
  #         ColorParents = shiny::reactive({colthemeCol$ColorParents}),
  #         ColorTabClicked = shiny::reactive({colthemeCol$ColorTabClicked}),
  #         ColorImportance = shiny::reactive({colthemeCol$ColorImportance}),
  #         ColorFactCont = shiny::reactive({colthemeCol$ColorFactCont}),
  #         nice_Numbers = apppars$NiceNumbers,
  #         ref_line = shiny::reactive({ref_line()}),
  #         YRange = shiny::reactive({input$YRange}),
  #         xlabel = shiny::reactive({input$xlabel}),
  #         circlestyle = shiny::reactive({input$circlestyle}),
  #         grid = shiny::reactive({input$grid}),
  #         pch_value = shiny::reactive({input$pch_value})
  #       )
  #     }
  }

  if (asmus_version == 2) {
    # shiny::observeEvent(c(scresults_tmp$dat,upload_data$parameter3()), {
      shiny::callModule(
        asmus2_module_server,
        #paste0("asmus2",upload_data$parameter3()),
        "asmus2",
        results = shiny::reactive({scresults_tmp$dat}),
        ColorReference = colthemeCol$ColorReference,
        ColorBGplot = shiny::reactive({colthemeCol$ColorBGplot}),
        ColorPoints = shiny::reactive({colthemeCol$ColorPoints}),
        nice_Numbers = apppars$NiceNumbers
      )
    # })
  }

  #### Module call: legend_server ####
  shiny::observe({
    shiny::callModule(
      mod_legend_server,
      "legend1",
      plot_color = shiny::reactive({plot_color$val}),
      rowwise = FALSE,
      colthemeCol = shiny::reactive({colthemeCol}),
      complement = shiny::reactive({ifelse(!is.null(plot_points_data_complement()),TRUE,FALSE)}),
      point_brightness = shiny::reactive({input$point_brightness})
    )
  })

  #### Module call: comparer_server ####
  mod_comparer_vars <- shiny::callModule(
    mod_comparer_server,
    "comparer",
    results = shiny::reactive({scresults_tmp$dat}),
    YRange = shiny::reactive({input$YRange}),
    XRange = shiny::reactive({input$XRange}),
    plot_type = shiny::reactive({input$plot_type}),
    point_size = shiny::reactive({input$pointsize}),
    pch_value = shiny::reactive({input$pch_value}),
    color = shiny::reactive({plot_color$val}),
    colthemeCol = shiny::reactive({colthemeCol}),
    ColorBGplot = shiny::reactive({colthemeCol$ColorBGplot}),
    ColorClicked = shiny::reactive({colthemeCol$ColorClicked}),
    ColorTabClicked = shiny::reactive({colthemeCol$ColorTabClicked}),
    ColorReference = shiny::reactive({colthemeCol$ColorReference}),
    ColorPoints = shiny::reactive({colthemeCol$ColorPoints}),
    ColorMemorized = shiny::reactive({colthemeCol$ColorMemorized}),
    VarChosen = shiny::reactive({input$VarChosen}),
    x = shiny::reactive({input$x}),
    y = shiny::reactive({input$y}),
    plot_points_data_complement = shiny::reactive({plot_points_data_complement()}),
    key = shiny::reactive({input$key}),
    pickradius = shiny::reactive({input$pickradius}),
    nice_Numbers = apppars$NiceNumbers,
    xlabel = shiny::reactive({input$xlabel}),
    grid = shiny::reactive({input$grid}),
    circlestyle = shiny::reactive({input$circlestyle}),
    memorized_Data = shiny::reactive({df_m$data}),
    point_brightness = shiny::reactive({input$point_brightness})
  )

  #### last click on selection list ####
  pare <- shiny::reactiveValues(val = NULL)


  shiny::observeEvent(c(input$selectedSG_rows_selected,input$selectedSG_row_last_clicked), {
    pare$val <- input$selectedSG_rows_selected
  })

  shiny::observeEvent(c(scresults_tmp$dat,mod_graph_vars1$clicked_points(), mod_comparer_vars$clicked_points2(),mod_comparer_vars$clicked_points3(), mod_comparer_vars$clicked_points4()), {
    pare$val <- 0
  })

  #### Module call: mosaic_server ####
  #shiny::observe({
  #  if (!is.null(scresults_tmp$dat)) {
      shiny::callModule(
        mod_mosaic_server,
        "mosaic",
        results = shiny::reactive({scresults_tmp$dat}),
        ColorBGplot = shiny::reactive({colthemeCol$ColorBGplot}),
        nice_Numbers = apppars$NiceNumbers
      )
  #  }
  #})

  # Get click information of the 3 graphs and save the last click
  # as reactivevalue click_points_data$xy

  click_points_data <- shiny::reactiveValues(xy = data.frame(x = NULL, y = NULL))

    shiny::observeEvent(scresults_tmp$dat, {
       click_points_data$xy <- data.frame(x = NULL, y = NULL)
    })

    shiny::observeEvent(mod_graph_vars1$clicked_points(), {
       click_points_data$xy <- mod_graph_vars1$clicked_points()
    })
    shiny::observeEvent(mod_comparer_vars$plot_clicked2(), {
         click_points_data$xy <- mod_comparer_vars$clicked_points2()
    })
    observeEvent(mod_comparer_vars$plot_clicked3(), {
         click_points_data$xy <- mod_comparer_vars$clicked_points3()
    })
    shiny::observeEvent(mod_comparer_vars$clicked_points4(), {
         click_points_data$xy <- mod_comparer_vars$clicked_points4()
    })

  shiny::observeEvent(c(click_points_data$xy, scresults_tmp$dat), {

    Memorize = shinyInput(
      actionButton,
      dim(click_points_data$xy)[1],
      'button_',
      label = "Memorize",
      onclick = 'Shiny.onInputChange(\"select_button\",  this.id)'
    )
    if (dim(click_points_data$xy)[1] == 0) {
      if (!is.null(scresults_tmp$dat)) {
        empty_data <- scresults_tmp$dat$sge[0,c("SGID", colnames(scresults_tmp$dat$results_total))]

        if (dim(empty_data)[2] > 5) {
          empty_data <- empty_data[,1:5]
        }

        #empty_data <- data.frame(row.names=c("Memorize","SGID","N.of.subjects"))
      } else {
        empty_data <- NULL
      }

      tmp <- DT::datatable(
        data = empty_data,
        selection = 'single',
        extensions = 'Buttons',
        escape = FALSE,
        options = list(
          language = list(emptyTable = 'Select a subgroup by clicking on a point in the graph above!'),
          columnDefs = list(list(targets = 1, visible = TRUE)),
          initComplete = DT::JS(
            "function(settings, json) {",
            paste0("$(this.api().table().header()).css({'background-color': '",
               colthemeCol$col.bg,
               "', 'color': '",
               font_color(different_hues(colthemeCol$col.bg)),
               "'});"
            ),"}"
          ),
          dom = 'Brtip',
          buttons = c('copy','print','pageLength', I('colvis')),
          lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
          pageLength = 6,
          rowCallback = DT::JS(
            "function(row, data) {
            \n
            // Bold cells for those >= 5 in the first column\n
            if (parseFloat(data[1]) >= 15.0)\n
            $(\"td:eq(1)\", row).css(\"font-weight\", \"bold\");\n
            }"
          )
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of Selected Subgroups',
        filter='top'
      )

    if (!is.null(empty_data)) {
      tmp <- DT::formatStyle(
        table = tmp,
        columns = 1:(ncol(empty_data)),
        target = "cell",
        backgroundColor = different_hues(colthemeCol$col.bg),
        border = paste0('.5px solid ', colthemeCol$col.bg)
      )
    }
    output$selectedSG <- DT::renderDataTable(tmp)

    }

    if (dim(click_points_data$xy)[1] > 0) {

      col2hide <- which(sapply(click_points_data$xy, FUN = function(x){all(x == 'Not used')})) - 1

      names(col2hide) <- NULL

      tmp <- DT::datatable(
        data = cbind(Memorize, click_points_data$xy),
        selection = 'single',
        extensions = 'Buttons',
        escape = FALSE,
        options = list(
          columnDefs = list(list(targets = col2hide + 1, visible = FALSE)),
          initComplete = DT::JS(
            "function(settings, json) {",
            paste0("$(this.api().table().header()).css({'background-color': '",
                   colthemeCol$col.bg,
                   "', 'color': '",
                   font_color(different_hues(colthemeCol$col.bg)),
                   "'});"
            ),"}"
          ),
          dom = 'Brtip',
          buttons = c('copy','print','pageLength', I('colvis')),
          lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
          pageLength = 6,
          rowCallback = DT::JS(
            "function(row, data) {
            \n
            // Bold cells for those >= 5 in the first column\n
            if (parseFloat(data[1]) >= 15.0)\n
            $(\"td:eq(1)\", row).css(\"font-weight\", \"bold\");\n
            }"
          )
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of Selected Subgroups',
        filter='top'
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = 1:(ncol(click_points_data$xy) + 1),
        target = "cell",
        backgroundColor = different_hues(colthemeCol$col.bg),
        border = paste0('.5px solid ', colthemeCol$col.bg)
      )

      tmp.sglev <- levels(
        relevel(
          factor(unlist(lapply(click_points_data$xy[, scresults_tmp$dat$factors], as.character))),
                ref = 'Not used'
        )
      )

      colXY <- which(colnames(click_points_data$xy) %in% c('SGID', names(scresults_tmp$dat$results_total), 'nfactors')) + 1

      col.tabFont <- font_color(different_hues(colthemeCol$col.bg))
      col.tabBack <- colthemeCol$col.bg

      tmp <- DT::formatStyle(
        table = tmp,
        columns = names(click_points_data$xy),
        color = col.tabFont
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = scresults_tmp$dat$factors,
        color = DT::styleEqual(
          tmp.sglev,
          c(col.tabBack, rep(col.tabFont, length(tmp.sglev) - 1))
        )
      )
      output$selectedSG <- DT::renderDataTable(tmp)
    }
  })

  plot_color <- shiny::reactiveValues(val = NULL)


  #### Point color observer ####
  shiny::observe({
    if(!is.null(scresults_tmp$dat)) {
    shiny::req(input$y)
    selected_ids$val
    input$selectedSG_row_last_clicked
    input$selectedSG_rows_selected
    # update color if number of factor level is changed
    input$key

    f <- scresults_tmp$dat$sge[which(scresults_tmp$dat$sge$nfactors >= input$key[1] & scresults_tmp$dat$sge$nfactors <= input$key[2]), ]

    p.col <- colthemeCol$ColorPoints
    bright <- input$point_brightness
    f$colour <- as.character(
      c(
        grDevices::adjustcolor(p.col, alpha = 1 * bright),
        grDevices::adjustcolor(p.col, alpha = 0.75 * bright),
        grDevices::adjustcolor(p.col, alpha = 0.5 * bright),
        grDevices::adjustcolor(p.col, alpha = 0.25 * bright),
        grDevices::adjustcolor(p.col, alpha = 0.1 * bright),
        grDevices::adjustcolor(p.col, alpha = 0.1 * bright),
        grDevices::adjustcolor(p.col, alpha = 0.1 * bright),
        grDevices::adjustcolor(p.col, alpha = 0.1 * bright)
      )
    )[match(f$nfactors, 1:8)]


    if (input$filter != "no selection" & input$filter2 == "no selection") {
      if (!is.null(input$VarChosen)) {
        select_points_data <- scresults_tmp$dat$sge[which(scresults_tmp$dat$sge$nfactors >=
                                                     input$key[1] & scresults_tmp$dat$sge$nfactors <=
                                                     input$key[2] &
                                                     scresults_tmp$dat$sge[, c(input$filter)] == input$VarChosen),]

        f$colour[f$SGID %in% select_points_data$SGID] <- colthemeCol$ColorSelected
        #f$colour[f$colour != shiny::isolate(colthemeCol$ColorSelected)] <- grDevices::adjustcolor(p.col, alpha = 0.1)
      }
    } else if (input$filter != "no selection" & input$filter2 != "no selection") {
       select_points_data <- scresults_tmp$dat$sge[which(scresults_tmp$dat$sge$nfactors >=
                                                     input$key[1] & scresults_tmp$dat$sge$nfactors <=
                                                     input$key[2] &
                                                     scresults_tmp$dat$sge[, c(input$filter)] == input$VarChosen &
                                                     scresults_tmp$dat$sge[, c(input$filter2)] == input$VarChosen2
                                                      ),]

        f$colour[f$SGID %in% select_points_data$SGID] <- colthemeCol$ColorSelected
    }

    #variable importance
    if(!is.null(importance_$val())) {
      f[f$SGID %in% importance_$val(), 'colour'] <- colthemeCol$ColorImportance
    }

    #clicked points
    f[f$SGID %in% shiny::isolate(click_points_data$xy$SGID),'colour'] <- colthemeCol$ColorClicked

    #selected points
    f[f$SGID %in% click_points_data$xy[pare$val,'SGID'],'colour'] <- colthemeCol$ColorTabClicked

    #factorial contexts

    #factorialContext_result <- factorialContext(scresults, click_points_data$xy[shiny::isolate(pare$val),'SGID'])

    #context

    SGID_clicked <- selected_ids$val
    if (!is.null(SGID_clicked)) {
      if(!is.integer0(SGID_clicked)) {
        if(!is.na(SGID_clicked)) {

        tmp <- scresults_tmp$dat$sge[scresults_tmp$dat$sge$FCID_all == scresults_tmp$dat$sge[scresults_tmp$dat$sge$SGID == SGID_clicked,]$FCID_all,]

        if (!any(startsWith(colnames(tmp),"FCID_complete_")) & dim(tmp)[1] != 0) {
          tmp <- pseudo_contexts(tmp, input$y, scresults_tmp$dat$factors)
        }
        #only when pseudo calculation was performed
        #if (any(startsWith(colnames(scresults$sge),"FCID_complete"))) {
        # 1. case: Context complete:
        if (all(tmp[paste0("FCID_incomplete_",shiny::req(input$y))] == "Complete")) {
          f[f$SGID %in% tmp$SGID, 'colour'] <- shiny::isolate(colthemeCol$ColorFactCont)
        }
        #2. case: Context incomplete:
        if (all(tmp[paste0("FCID_incomplete_",input$y)] == "Incomplete")) {
           f[f$SGID %in% tmp$SGID, 'colour'] <- different_hues(colthemeCol$ColorFactCont, value = 150)
        }
        #3. case: Context pseudo complete
          if (all(tmp[paste0("FCID_incomplete_",input$y)] == "Pseudo complete")) {
             f[f$SGID %in% tmp[tmp[paste0("FCID_pseudo_", input$y)] != "No Pseudo",]$SGID, 'colour'] <- different_hues(colthemeCol$ColorFactCont, value = 89)
             f[f$SGID %in% tmp[tmp[paste0("FCID_pseudo_", input$y)] == "No Pseudo",]$SGID, 'colour'] <- different_hues(colthemeCol$ColorFactCont, value = 180)
          }
        #}
        }
      }
    }
    # if (!is.null(factorialContext_result)) {
    #
    #   # if (!any(is.na(factorialContext_result$Factorial[[shiny::isolate(input$y)]])) & factorialContext_result$Status == "Complete") {
    #   #   f[f$SGID %in% factorialContext_result$Factorial$SGID, 'colour'] <- shiny::isolate(colthemeCol$ColorFactCont)
    #   # } else {
    #   #   f[f$SGID %in% factorialContext_result$Factorial$SGID, 'colour'] <- different_hues(colthemeCol$ColorFactCont, value = 89)
    #   # }
    # }

      f[f$SGID %in% parents(scresults_tmp$dat, shiny::isolate(click_points_data$xy)[(pare$val),'SGID'])$Parents$SGID,'colour'] <- shiny::isolate(colthemeCol$ColorParents)
      f[f$SGID %in% shiny::isolate(click_points_data$xy$SGID),'colour'] <- shiny::isolate(colthemeCol$ColorClicked)

      f[f$SGID %in% shiny::isolate(click_points_data$xy)[shiny::isolate(pare$val),'SGID'],'colour'] <- shiny::isolate(colthemeCol$ColorTabClicked)

      if (!is.null(df_m$data[['SGID']])) {
        if (!is.integer0(df_m$data[['SGID']])) {
          f[f$SGID %in% df_m$data[['SGID']], 'colour'] <- colthemeCol$ColorMemorized
        }
      }
      plot_color$val <- f$colour
    }
  })

## call arguments for confidence intervals

  mod_graph_vars1 <- shiny::callModule(
        mod_graph_server,
        "graph1",
        results = shiny::reactive({scresults_tmp$dat}),
        plot_point = shiny::reactive({req(plot_points_data())}),
        YRange = shiny::reactive({input$YRange}),
        XRange = shiny::reactive({input$XRange}),
        plot_type = shiny::reactive({input$plot_type}),
        point_size = shiny::reactive({input$pointsize}),
        pch_value = shiny::reactive({input$pch_value}),
        color = shiny::reactive({plot_color$val}),
        ColorBGplot = shiny::reactive({colthemeCol$ColorBGplot}),
        ColorClicked = shiny::reactive({colthemeCol$ColorClicked}),
        ColorTabClicked = shiny::reactive({colthemeCol$ColorTabClicked}),
        ColorPoints = shiny::reactive({colthemeCol$ColorPoints}),
        ColorReference = shiny::reactive({colthemeCol$ColorReference}),
        ColorMemorized = shiny::reactive({colthemeCol$ColorMemorized}),
        VarChosen = shiny::reactive({input$VarChosen}),
        x = shiny::reactive({input$x}),
        y = shiny::reactive({input$y}),
        plot_points_data_complement = shiny::reactive({plot_points_data_complement()}),
        key = shiny::reactive({input$key}),
        nice_Numbers = apppars$NiceNumbers,
        pickradius = shiny::reactive({input$pickradius}),
        xlabel = shiny::reactive({input$xlabel}),
        grid = shiny::reactive({input$grid}),
        circlestyle = shiny::reactive({input$circlestyle}),
        memorized_Data = shiny::reactive({df_m$data}),
        memorized_labels_on_off = shiny::reactive({input$memorized_labels_on_off}),
        subTitle = graphSubtitle,
        remove_levels = shiny::reactive({input$remove_levels}),
        CI = CI,
        alpha = alpha,
        span = span,
        show_points = show_points,
        values = scresults$sge[eval_name]
      )
    importance_ <- shiny::callModule(
      mod_variable_importance_server,
      "vi",
      variable_importance = shiny::reactive({variable_importance_tmp$dat}),
      results = shiny::reactive({scresults_tmp$dat}),
      panel_colour = shiny::reactive({colthemeCol$col.bg})
    )

  #### DATA UPLOAD ####
  upload_data <- shiny::callModule(
    upload_tab_server,
    "upload_tab_ui_1",
    dat = apppars$scresults,
    dat_name = apppars$scresults_name,
    vi = apppars$variable_importance
  )

  shiny::observeEvent(upload_data$parameter1(), {
    scresults_tmp$dat <- upload_data$parameter1()
  })
  shiny::observe({
  if(!is.null(scresults_tmp$dat)) {
  js$enableTab("SubscreenExplorer")
  js$enableTab("SubscreenComparer")
  js$enableTab("SubscreenMosaic")
  js$enableTab("SubscreenAsmus")
  }
  })

  scresults_tmp <- shiny::reactiveValues(
    dat = apppars$scresults
  )

  shiny::observeEvent(upload_data$parameter2(), {
    variable_importance_tmp$dat <- upload_data$parameter2()
  })

  variable_importance_tmp <- shiny::reactiveValues(
    dat = apppars$variable_importance
  )

}


SGEApp <- shiny::shinyApp(ui = ui, server = server)
