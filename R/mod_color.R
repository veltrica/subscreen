#' color UI Function
#'
#' @description A shiny Module for the color options in Subscreen.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_color_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorClicked"),
          label = "Choose a colour for the clicked subgroup(s)",
          #colthemeCol$ColorClicked,
          value = "#D30F4B",
          allowTransparent = TRUE

        )
      ),
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorSelected"),
          label = "Choose a colour for the filtered subgroup(s)",
          value  = "#89D329",
          #value = colthemeCol$ColorSelected,
          allowTransparent = TRUE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorParents"),
          label = "Choose a colour for the parent subgroup(s)",
          value = "#ff6c00",
          #value = colthemeCol$ColorParents,
          allowTransparent = TRUE
        )
      ),
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorTabClicked"),
          label = "Choose a colour for the selected subgroup(s)",
          value = "#e2b007",
          #value = colthemeCol$ColorTabClicked,
          allowTransparent = TRUE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorImportance"),
          label = "Choose a colour for the subgroup(s) with important variable(s) ",
          value = "#FA1BDC",
          #value = colthemeCol$ColorImportance,
          allowTransparent = TRUE
        )
      ),
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorReference"),
          label = "Choose a colour for the reference line",
          value = "#0091DF60",
          #value = colthemeCol$ColorReference,
          allowTransparent = TRUE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorFactCont"),
          label = "Choose a colour for the factorial context",
          value = "#0350E0",
          #value = colthemeCol$ColorFactCont,
          allowTransparent = TRUE
        )
      ),
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorBGplot"),
          label = "Choose background colour (Plot)",
          value = "#383838"
          #colthemeCol$ColorBGplot
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorPoints"),
          "Choose a colour for the points",
          value = "#FFFFFF"
          #colthemeCol$ColorPoints
        )
      ),
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorMemorized"),
          "Choose a colour for the memorized subgroups",
          value = "#57D48B"
        )
      ),
      shiny::fluidRow(
        shiny::column(6,
          shiny::selectInput(
            inputId = ns('select_col'),
            label = "Select standard color theme:",
            choices = list('app version', 'print version'),
            selected = 'app version'
          )
        ),

        shiny::column(6, offset = 6,
          shiny::actionButton(
            inputId = ns('settheme'),
            label = 'Apply / Refresh',
            width = NULL
          )
        )
      )
    ),
    use_bs_popover(),
    use_bs_tooltip()
  )
}


#' color Server Function
#'
#' @noRd
mod_color_server <- function(input, output, session) {

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
    ColorPoints = "#FFFFFF",
    ColorMemorized = "#57D48B"
  )

  shiny::observeEvent(input$settheme, {
    if (input$select_col == 'app version') {
      colthemeCol$col.bg <- '#383838'
      colthemeCol$ColorBGplot <- "#383838"
      colthemeCol$ColorPoints <- "#FFFFFF"
    } else if (input$select_col == 'print version') {
      colthemeCol$col.bg <- '#ffffff'
      colthemeCol$ColorReference <- "#0091DF"
      colthemeCol$ColorBGplot <- "#ffffff"
      colthemeCol$ColorPoints <- "#000000"
    }
  })

  # shiny::observeEvent(
  #   c(input$FontColour,
  #     input$ColorClicked,
  #     input$ColorSelected,
  #     input$ColorParents,
  #     input$ColorTabClicked,
  #     input$ColorImportance,
  #     input$ColorReference,
  #     input$ColorBGplot,
  #     input$ColorPoints,
  #     input$ColorFactCont,
  #     input$ColorMemorized
  #   ), {
  shiny::observeEvent(input$ColorBGplot,{
      colthemeCol$col.bg <- input$ColorBGplot
  })
  shiny::observeEvent(input$ColorClicked, {
      colthemeCol$ColorClicked <- input$ColorClicked
  })
  shiny::observeEvent(input$ColorSelected, {
      colthemeCol$ColorSelected <- input$ColorSelected
  })
  shiny::observeEvent(input$ColorFactCont, {
      colthemeCol$ColorFactCont <- input$ColorFactCont
  })
  shiny::observeEvent(input$ColorParents, {
      colthemeCol$ColorParents <- input$ColorParents
  })
  shiny::observeEvent(input$ColorTabClicked, {
      colthemeCol$ColorTabClicked <- input$ColorTabClicked
  })
  shiny::observeEvent(input$ColorImportance, {
      colthemeCol$ColorImportance <- input$ColorImportance
  })

  shiny::observeEvent(input$ColorReference, {
      colthemeCol$ColorReference <- input$ColorReference
  })
  shiny::observeEvent(input$ColorBGplot, {
      colthemeCol$ColorBGplot <- input$ColorBGplot
  })
  shiny::observeEvent(input$ColorPoints, {
      colthemeCol$ColorPoints <- input$ColorPoints
  })
  shiny::observeEvent(input$ColorMemorized, {
      colthemeCol$ColorMemorized <- input$ColorMemorized
  })
    # }
  # )

  ColorBGplotlight <- shiny::reactiveValues(
    col = grDevices::adjustcolor(
      "#383838",
      red.f = 1.3,
      green.f = 1.3,
      blue.f = 1.3
    )
  )

  shiny::observeEvent(input$ColorBGplot, {
    ColorBGplotlight$col <- grDevices::adjustcolor(
      colthemeCol$ColorBGplot,
      red.f = 1.3,
      green.f = 1.3,
      blue.f = 1.3
    )
  })

  return(
    list(
      colthemeCol = shiny::reactive({colthemeCol})
    )
  )
}
