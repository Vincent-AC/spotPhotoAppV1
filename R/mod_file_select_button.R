#' file_select_button UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_file_select_button_ui <- function(id,
                                      label,
                                      title,
                                      multiple = FALSE) {
  ns <- NS(id)
  fluidRow(box(tagList(
    div(
      style = "display:inline-block",
      shinyFiles::shinyFilesButton(
        ns('filebutton'),
        label = label,
        title = title,
        multiple = multiple
      )
    ),
    div(style = "display:inline-block", uiOutput(ns('tickmark')))
  ), width = 12))
}

#' file_select_button Server Functions
#'
#' @noRd
mod_file_select_button_server <- function(id,
                                          file_paths,
                                          file_paths_name = deparse(substitute(file_paths)),
                                          variable_to_store_filelist,
                                          gargoyle_trigger_name,
                                          root,
                                          tibble_object_name) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    root <- root
    rawPicFiles <- reactive(input$filebutton)
    shinyFiles::shinyFileChoose(input,
                                id = 'filebutton',
                                roots = root,
                                session = session)

    observeEvent(input$filebutton, {
      # eval(parse(
      #   text = paste0(
      #     file_paths_name,
      #     "$",
      #     variable_to_store_filelist,
      #     "<-stringr::str_sort(as.character(shinyFiles::parseFilePaths(root, rawPicFiles())$datapath),numeric=T)"
      #   )
      # ))
      eval(parse(
        text = paste0(
          "if (length(stringr::str_sort(as.character(shinyFiles::parseFilePaths(root, rawPicFiles())$datapath),numeric=T))==0){} else if(length(",
          file_paths_name,
          "$",
          tibble_object_name,
          "$",
          variable_to_store_filelist,
          ")==0){",
          file_paths_name,
          "$",
          tibble_object_name,
          "<-dplyr::full_join(",
          file_paths_name,
          "$",
          tibble_object_name,
          ",tibble::tibble(",
          variable_to_store_filelist,
          "=stringr::str_sort(as.character(shinyFiles::parseFilePaths(root, rawPicFiles())$datapath),numeric=T)))}else{",
          file_paths_name,
          "$",
          tibble_object_name,
          "$",
          variable_to_store_filelist,
          "<-stringr::str_sort(as.character(shinyFiles::parseFilePaths(root, rawPicFiles())$datapath),numeric=T)}"
        )
      ))
      gargoyle::trigger(gargoyle_trigger_name)
    },
    ignoreInit = T)

    gargoyle::on(gargoyle_trigger_name, {
      # print(eval(parse(
      #   text = paste0(file_paths_name, "$", variable_to_store_filelist)
      # )))
      print(eval(parse(
        text = paste0(file_paths_name,
                      "$",
                      tibble_object_name)
      )))
    })
    output$tickmark <- renderUI({
      gargoyle::watch(gargoyle_trigger_name)
      if (eval(parse(
        text = paste0(
          "length(",
          file_paths_name,
          "$",
          tibble_object_name,
          "$",
          variable_to_store_filelist,
          ")==0"
        )
      ))) {
        print(NULL)
      } else {
        p(icon("check"), "Selection successful")
      }
    })

    # output$chosenfileslist <- renderText({
    #   gargoyle::watch(gargoyle_trigger_name)
    #   eval(parse(text=paste0(file_paths_name,"$",variable_to_store_filelist)))
    # })
  })
}

## To be copied in the UI
# mod_file_select_button_ui("file_select_button_1")

## To be copied in the server
# mod_file_select_button_server("file_select_button_1")
