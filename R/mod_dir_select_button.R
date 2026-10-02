#' dir_select_button UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dir_select_button_ui <- function(id,label,
                                     title){
  ns <- NS(id)
  tagList(
    shinyFiles::shinyDirButton(
      ns('dirbutton'),
      label = label,
      title = title
    ),
    verbatimTextOutput(ns("chosendir"))
  )
}

#' dir_select_button Server Functions
#'
#' @noRd
mod_dir_select_button_server <- function(id,
                                         file_paths,
                                         file_paths_name=deparse(substitute(file_paths)),
                                         variable_to_store_filelist,
                                         gargoyle_trigger_name,
                                         root){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    rawDirPath <- reactive(input$dirbutton)
    shinyFiles::shinyDirChoose(input, id='dirbutton', roots = root(), session = session)

    observeEvent(input$dirbutton, {
      eval(parse(text=paste0(file_paths_name,"$",variable_to_store_filelist,"<-as.character(shinyFiles::parseDirPath(root, rawDirPath()))")))
      gargoyle::trigger(gargoyle_trigger_name)
    })

    gargoyle::on(gargoyle_trigger_name,{
      golem::print_dev(eval(parse(text=paste0(file_paths_name,"$",variable_to_store_filelist))))
    })

    output$chosendir <- renderText({
      gargoyle::watch(gargoyle_trigger_name)
      eval(parse(text=paste0(file_paths_name,"$",variable_to_store_filelist)))
    })

  })
}

## To be copied in the UI
# mod_dir_select_button_ui("dir_select_button_1")

## To be copied in the server
# mod_dir_select_button_server("dir_select_button_1")
