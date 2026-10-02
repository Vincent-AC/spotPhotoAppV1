#' save_load_R6_object UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_save_load_R6_object_ui <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("saveButton"), label = "Save"),
    div(
      style = "display:inline-block",
      shinyFiles::shinyFilesButton(
        ns('loadButton'),
        label = "Load",
        title = "Please select the .Rds file containing the saved session",
        icon = shiny::icon("upload"),
        multiple = FALSE
      )
    )
  )
}

#' save_load_R6_object Server Functions
#'
#' @noRd
mod_save_load_R6_object_server <- function(id,
                                           file_paths,
                                           file_paths_name = deparse(substitute(file_paths)),
                                           tibble_object_name,
                                           root) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$saveButton <- downloadHandler(
      filename = function(){
        "spotPhotosAppSession.Rds"
      },
      content = function(file) {
        saveRDS(file_paths, file = file)
      }
    )

    # output$loadButton <- downloadHandler(
    #   filename = function(){
    #     "spotPhotosAppSession.Rds"
    #   },
    #   content = function(file) {
    #     saveRDS(file_paths, file = file)
    #   }
    # )

    rdsFile <- reactive(input$loadButton)
    shinyFiles::shinyFileChoose(input,
                                id = 'loadButton',
                                roots = root(),
                                session = session)
  # observeEvent(input$saveButton,
  #              {
  #                eval(parse(
  #                  text = paste0(
  #                    "saveRDS(",
  #                    file_paths_name,
  #                    ",file=\"C:/Users/UMR-1070/Desktop/test.RDS\")"
  #                  )
  #                ))
  #                golem::print_dev("Session saved!")
  #              })
  observeEvent(input$loadButton,
               {
                 if(length(as.character(shinyFiles::parseFilePaths(root, rdsFile())$datapath))==0){
                   NULL
                 } else {
                   x <- readRDS(file = as.character(shinyFiles::parseFilePaths(root, rdsFile())$datapath))
                   golem::print_dev(x)
                   # file_paths$fileTable <- x$fileTable
                   x_name <- deparse(substitute(x, env = .GlobalEnv))
                   # eval(parse(
                   #   text = paste0(file_paths_name, "$fileTable",
                   #                 "<-", x_name, "$fileTable")
                   # ))
                   replace_values_in_R6 <-
                     function(R6_object, field_name, new_object) {
                       eval(parse(
                         text = paste0(
                           R6_object,
                           "$",
                           field_name,
                           "<-",
                           new_object,
                           "$",
                           field_name
                         )
                       ))
                     }
                   purrr::walk(names(x)[!(names(x) %in% c(".__enclos_env__", "clone"))],
                               ~ replace_values_in_R6(file_paths_name,
                                                      .,
                                                      x_name))
                   # gargoyle::trigger("dataLoaded")
                   # eval(parse(
                   #   text = paste0(
                   #     file_paths_name,
                   #     "<- readRDS(file = \"C:/Users/UMR-1070/Desktop/test.RDS\")"
                   #   )
                   # ))
                   # golem::print_dev("Session loaded!")
                   golem::print_dev(eval(parse(
                     text = paste0(file_paths_name,
                                   "$",
                                   tibble_object_name)
                   )))
                   # golem::print_dev(eval(parse(text = paste0(
                   #   file_paths_name
                   # ))))
                   if(sum(!is.na(eval(parse(text = paste0(file_paths_name,
                                                      "$",
                                                      tibble_object_name,
                                                      "$picpaths")))))>0){
                     gargoyle::trigger("fileSelection")
                   }
                   if(sum(!is.na(eval(parse(text = paste0(file_paths_name,
                                                      "$",
                                                      tibble_object_name,
                                                      "$expepath")))))>0){
                     gargoyle::trigger("expeFileSelection")
                   }
                   if(sum(!is.na(eval(parse(text = paste0(file_paths_name,
                                                      "$",
                                                      tibble_object_name,
                                                      "$picpaths")))))>0){
                     gargoyle::trigger("resultsDirSelection")
                   }
                   if(sum(!is.na(eval(parse(text = paste0(file_paths_name,
                                                      "$",
                                                      tibble_object_name,
                                                      "$objectclasstablepath")))))>0){
                     gargoyle::trigger("postProcessingDone")
                   }
                   # golem::print_dev(eval(parse(
                   #   text = paste0(file_paths_name,
                   #                 "$",
                   #                 tibble_object_name)
                   # )))
                   # golem::print_dev(eval(parse(text = paste0(
                   #   file_paths_name
                   # ))))
                   # golem::print_dev("Session Really loaded!")
                 }







               })

})



}

## To be copied in the UI
# mod_save_load_R6_object_ui("save_load_R6_object_1")

## To be copied in the server
# mod_save_load_R6_object_server("save_load_R6_object_1")
