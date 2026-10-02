#' input_files_buttons UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_files_buttons_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_file_select_button_ui(
      ns("raw_pictures_select_button"),
      label = "Select pictures to analyze",
      title = "Please select raw pictures to analyze",
      multiple = TRUE
    ),

    mod_file_select_button_ui(
      ns("expe_file_select_button"),
      label = "Select Excel protocol file",
      title = "Please select the .xlsx file containing the protocol"
    ),
  fluidRow(box(
    selectInput(
      ns("format"),
      "Select filename format:",
      c("Time_Strain_ATB" = "format_1",
        "Time_Genus_Species_Strain_ATB_Replicate" = "format_2"),
      selected = "format_1"
    ),
    width = 12
  )),
  fluidRow(box(
    selectInput(
      ns("plate_format"),
      "Select plate format:",
      c("Seq2Diag format" = "format_1"),
      selected = "format_1"
    ),
    htmlOutput(ns("plateFormatImage")),
    width = 12
  )))
}

#' input_files_buttons Server Functions
#'
#' @noRd
mod_input_files_buttons_server <- function(id,
                                           file_paths,
                                           file_paths_name = deparse(substitute(file_paths)),
                                           variable_containing_input_file_format,
                                           tibble_object_name) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    volumes <-
      if (golem::app_dev()) {
        c(test = "C:/Users/UMR-1070/Documents/Vincent/Seq2Diag/test/")
      } else {
        shinyFiles::getVolumes()
      }


    mod_file_select_button_server(
      "raw_pictures_select_button",
      file_paths = file_paths,
      variable_to_store_filelist = "picpaths",
      gargoyle_trigger_name = "fileSelection",
      root = volumes,
      tibble_object_name = "fileTable"
    )
    mod_file_select_button_server(
      "expe_file_select_button",
      file_paths = file_paths,
      variable_to_store_filelist = "expepath",
      gargoyle_trigger_name = "expeFileSelection",
      root = volumes,
      tibble_object_name = "fileTable"
    )

    observeEvent(input$format, {
      eval(parse(
        text = paste0(
          file_paths_name,
          "$",
          variable_containing_input_file_format,
          " <- input$format"
        )
      ))
    })

    output$plateFormatImage <- renderUI({
      tags$img(src = "www/image_format_1_plate.png")
    })


  })
}

## To be copied in the UI
# mod_input_files_buttons_ui("input_files_buttons_1")

## To be copied in the server
# mod_input_files_buttons_server("input_files_buttons_1")
