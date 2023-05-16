#' upload_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param bg.col background color
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#'

upload_tab_ui <- function(id, bg.col) {

  ns <- NS(id)
  shiny::tagList(
    list(
      shiny::tags$head(
        shiny::tags$style(
          paste(
            "body { color: ",
            font_color(bg.col),
            "}",
            sep = ""
          )
        )
      )
    ),
    shiny::h3("Welcome to"),
    shiny::uiOutput(ns("myImage")),
    shiny::column(6,
      shiny::h5("Insert short description what SGS can do."),
      shiny::h4("Please upload your prepared data or use the demo data set."),
      shiny::uiOutput(ns("mode")),
      shiny::fluidPage(
        shiny::conditionalPanel(condition = "input.mode == 'rdata'", ns = ns,
          shiny::fileInput(
            inputId = ns("results_file"),
            label = "Choose results data file (created with subscreencalc())",
            multiple = TRUE,
            accept = c(".RData",".rds")
          ),
          shinyWidgets::materialSwitch(
            inputId = ns("switch_vi_file"),
            label = HTML("<span style = 'color: white;'> Add variable importance file or press 'Upload data' </span>"),
            status = "success"
          ),
          shiny::conditionalPanel(condition = "input.switch_vi_file == true", ns = ns,
            shiny::fileInput(
              inputId = ns("vi_file"),
              label = "Choose variable importance file (optional/created with subscreenvi())",
              multiple = TRUE,
              accept = c(".RData",".rds")
            )
          ),
          shiny::actionButton(
            inputId = ns('apply_rdata_files'),
            label = 'Upload data',
            icon = icon("upload"),
            style = "color: #fff; background-color: #5cb85c; border-color: #fff"
          )
        ),
      shiny::conditionalPanel(condition = "input.mode == 'demo'", ns = ns,
        shiny::actionButton(
          inputId = ns('apply_demo_data'),
          label = 'Use demo data',
          icon = icon("hdd"),
          style = "color: #fff; background-color: #5cb85c; border-color: #fff"
        )
      ),
      shiny::conditionalPanel(condition = "input.mode == 'uploaded'", ns = ns,
        shiny::actionButton(
          inputId = ns('apply_uploaded_data'),
          label = 'Use uploaded data',
          icon = icon("download"),
          style = "color: #fff; background-color: #5cb85c; border-color: #fff"
        )
      )
      )

      # shiny::conditionalPanel(condition = "input.mode == 'server'",  ns = ns,
      #   shiny::selectInput(
      #     inputId = ns("studies"),
      #     label = "Select a study",
      #     choices = studies
      #   ),
      #   shiny::textInput(
      #     inputId = ns("user"),
      #     label = "Username:"
      #   ),
      #   shiny::passwordInput(
      #     inputId = ns("password"),
      #     label = "Password:"
      #   ),
      #   shiny::tags$br(),
      #   shiny::actionButton(
      #     inputId = ns('apply_retrieve_data'),
      #     label = 'Retrieve data',
      #     icon = icon("server"),
      #     style = "color: #fff; background-color: #5cb85c; border-color: #fff"
      #   )
      # )
    ),
    shiny::column(6,
      shiny::uiOutput(ns("list_output"))
    )
  )
}

#' upload_tab Server Function
#'
#' @noRd
upload_tab_server <- function(input, output, session, dat, dat_name, vi) {
  ns <- session$ns

  output$mode <- shiny::renderUI({

    if (!is.null(dat)) {
      choices <- c(
        ".RData file(s) (from Disc)" = "rdata",
        "Demo data" = "demo",
        "Uploaded data via function call" = "uploaded"
      )
    } else {
      choices <- c(
        ".RData file(s) (from Disc)" = "rdata",
        "Demo data" = "demo"
      )
    }

    if(exists("studies")) {
      choices <- c(choices, "Upload from server" = "server")
    }

      shiny::radioButtons(
      inputId = ns("mode"),
      label = "Input mode:",
      choices = choices
    )
  })

  preview_scresults_tmp <- reactiveValues(dat = NULL)
  preview_variable_importance_tmp <- reactiveValues(dat = NULL)
  buttons_clicked <- reactiveValues(dat = 0)
  shiny::observeEvent(c(input$apply_rdata_files,input$apply_demo_data, input$apply_uploaded_data),{
    buttons_clicked$dat <- buttons_clicked$dat + 1
  })
  shiny::observeEvent(c(input$mode,input$results_file), {
    if (input$mode == "rdata") {
      if (!is.null(input$results_file$datapath)) {
        if (tail(strsplit(input$results_file$datapath,"/.")[[1]], n = 1) %in% c(".rdata",".RData")) {
          preview_scresults_tmp$dat <- get(load(input$results_file$datapath))
        }
        if (tail(strsplit(input$results_file$datapath,"/.")[[1]], n = 1) == ".rds") {
          preview_scresults_tmp$dat <- readRDS(input$results_file$datapath)
        }
      } else {
        preview_scresults_tmp$dat <- NULL
      }
      #vi
      if (!is.null(input$vi_file$datapath)) {
        if (tail(strsplit(input$vi_file$datapath,"/.")[[1]], n = 1) %in% c(".rdata",".RData")) {
          preview_variable_importance_tmp$dat <- get(load(input$vi_file$datapath))
        }
        if (tail(strsplit(input$vi_file$datapath,"/.")[[1]], n = 1) == ".rds") {
          preview_variable_importance_tmp$dat <- readRDS(input$vi_file$datapath)
        }
      } else {
        preview_variable_importance_tmp$dat <- NULL
      }
    } else if (input$mode == "demo") {
      preview_scresults_tmp$dat <- readRDS(paste0(getwd(),"/data/results_factorial_complement_true.rds"))
      preview_variable_importance_tmp$dat <- readRDS(paste0(getwd(),"/data/importance.rds"))

    } else if (input$mode == "uploaded") {
      preview_scresults_tmp$dat <- dat
      preview_variable_importance_tmp$dat <- vi
    }
  })

  output$list_output <- shiny::renderUI({
    input$mod
    shiny::req(preview_scresults_tmp$dat)
    input$results_file
    if(!is.null(preview_scresults_tmp$dat)) {
      if(class(preview_scresults_tmp$dat) == "SubScreenResult") {
      shinyjs::enable("apply_rdata_files")
      shiny::HTML(
        paste0("
        <p style = 'color: #e3e3e3'>
          Dataset: <b style='font-size: 130%; color: #428bca'> ",
          if (input$mode == "demo") {
            "results_factorial_complement_true.rds"
          } else if (input$mode == "rdata") {
            input$results_file$name
          } else if (input$mode == "uploaded") {
            dat_name
          }
          ,"</b><br>
          Number of subjects: <b style='font-size: 130%; color: #428bca'>", preview_scresults_tmp$dat$results_total$N.of.subjects," </b><br>
          Number of subgroups: <b style='font-size: 130%; color: #428bca'>",max(preview_scresults_tmp$dat$sge$SGID),"</b><br>
          Number target variables: <b style='font-size: 130%; color: #428bca'>",length(preview_scresults_tmp$dat$results_total)-1,"</b>  <b style='font-size: 100%; color: #e3e3e3'>(",paste(names(preview_scresults_tmp$dat$results_total)[names(preview_scresults_tmp$dat$results_total)!="N.of.subjects"], collapse = ", "),")</b><br>
          Number factors: <b style='font-size: 130%; color: #428bca'>",length(preview_scresults_tmp$dat$factors),"</b> <b style='font-size: 100%; color: #e3e3e3'>(",paste(preview_scresults_tmp$dat$factors, collapse = ", "),")</b><br>
          Number factor combinations: <b style='font-size: 130%; color: #428bca'>",length(preview_scresults_tmp$dat$min_comb:preview_scresults_tmp$dat$max_comb)," </b> (",preview_scresults_tmp$dat$min_comb,"-",preview_scresults_tmp$dat$max_comb,") <br>

          <br>

          Factorial context calculation performed: ",
          if (any(startsWith(colnames(preview_scresults_tmp$dat$sge),"FCID_complete_"))) {
            "<i class='fa fa-check' style ='color: #5cb85c; font-size: 150%'></i>"
          } else if (any(colnames(preview_scresults_tmp$dat$sge) == "FCID_complete")) {
            "<i class='fa fa-exclamation' style ='color: #ffffff; font-size: 150%'></i> (Results structure outdated! Please use subscreencalc version >4.0.0)"
          } else {
            "<i class='fa fa-times' style ='color: #ffffff; font-size: 150%'></i>"
          } ,"<br>
          Subgroup complement calculation performed: ",
          if (any(startsWith(colnames(preview_scresults_tmp$dat$sge),"Complement_"))) {
            "<i class='fa fa-check' style ='color: #5cb85c; font-size: 150%'></i>"
          } else {
            "<i class='fa fa-times' style ='color: #fffff; font-size: 150%'></i>"
          } ,"<br>

          <br>

          Check for list input: <b style='font-size: 150%;'>",
          ifelse(
            is.list(preview_scresults_tmp$dat),
            "<i class='fa fa-check' style ='color: #5cb85c'></i>",
            "<i class='fa fa-times'></i>"
          )
          ,"</b><br>
          Check for non-empty list input sge: <b style='font-size: 150%;'>",
          ifelse(
            dim(preview_scresults_tmp$dat$sge)[1]>0,
            "<i class='fa fa-check' style ='color: #5cb85c'></i>",
            "<i class='fa fa-times'></i>"
          )
          ,"</b><br>
          Check for class SubScreenResult: <b style='font-size: 150%;'>",
            "<i class='fa fa-check' style ='color: #5cb85c'></i>
            </b><br>
        </p>
      ")
      )
    } else {
      shinyjs::disable("apply_rdata_files")
      shiny::HTML(
        paste0("
          <p style = 'color: #e3e3e3'>
            Check for class SubScreenResult: <b style='font-size: 150%;'>",
            "<i class='fa fa-times' style ='color: #f71b4b'></i>

          </b>
        </p>"))
    }
  }
  })

  output$myImage <- shiny::renderUI({
    list(shiny::HTML("<img src = 'www/subscreen_logo.png' alt = 'Subgroup Explorer Logo' width = '423' height = '140'>"))
  })

  scresults_tmp <- shiny::reactiveValues(
    dat = dat
  )

   variable_importance_tmp <- shiny::reactiveValues(
    dat = vi
  )

  #### Press demo data button ####
  shiny::observeEvent(input$apply_demo_data, {
    scresults_tmp$dat <- readRDS(paste0(getwd(),"/data/results_factorial_complement_true.rds"))
  })

  shiny::observeEvent(input$apply_demo_data, {
    variable_importance_tmp$dat <- readRDS(paste0(getwd(),"/data/importance.rds"))
  })

  #### Press uploaded data button ####
  shiny::observeEvent(input$apply_uploaded_data, {
    scresults_tmp$dat <- dat
  })

  # shiny::observeEvent(input$results_file, {
  #   if (tail(strsplit(input$results_file$datapath,"/.")[[1]], n = 1) == ".RData") {
  #     shinyjs::enable("apply_rdata_files")
  #   }
  # })
  #


  shiny::observeEvent(input$apply_rdata_files, {
    if (!is.null(input$results_file$datapath)) {
      if (tail(strsplit(input$results_file$datapath,"/.")[[1]], n = 1) %in% c(".rdata",".RData")) {
        scresults_tmp$dat <- get(load(input$results_file$datapath))
      }
      if (tail(strsplit(input$results_file$datapath,"/.")[[1]], n = 1) == ".rds") {
        scresults_tmp$dat <- readRDS(input$results_file$datapath)
      }
      ##vi
      ### add checks!
      if (!is.null(input$vi_file$datapath)) {
        if (tail(strsplit(input$vi_file$datapath,"/.")[[1]], n = 1) %in% c(".rdata",".RData")) {
          variable_importance_tmp$dat <- get(load(input$vi_file$datapath))
        }
        if (tail(strsplit(input$vi_file$datapath,"/.")[[1]], n = 1) == ".rds") {
          variable_importance_tmp$dat <- readRDS(input$vi_file$datapath)
        }
      }
    }
  })

  # shiny::observeEvent(input$apply_retrieve_data, {
  #
  #   if(!is.null(shiny::isolate(input$user)) & !is.null(shiny::isolate(input$password))) {
  #   out1 <- samba(
  #     datapath = studyfile$path[studyfile$study==input$studies],
  #     user = shiny::isolate(input$user),
  #     password = shiny::isolate(input$password)
  #   )
  #
  #   scresults_tmp$dat <- get(load(rawConnection(out1)))
  #   }
  # })
  return(
    list(
      parameter1 = shiny::reactive({scresults_tmp$dat}),
      parameter2 = shiny::reactive({variable_importance_tmp$dat}),
      parameter3 = shiny::reactive({buttons_clicked$dat})
    )
  )
}

## To be copied in the UI
# upload_tab_ui("upload_tab_ui_1")

## To be copied in the server
# callModule(upload_tab_server, "upload_tab_ui_1")

