#' create_show_final_result_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_create_show_final_result_graph_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(actionButton(ns("finalGraphButton"),label="Create final graph"),
    plotOutput(ns("finalGraph")),width=12)
  )
}

#' create_show_final_result_graph Server Functions
#'
#' @noRd
mod_create_show_final_result_graph_server <- function(id,
                                                      file_paths,
                                                      file_paths_name =deparse(substitute(file_paths)),
                                                      variable_containing_output_dir_path,
                                                      final_graph_ready_trigger_name,
                                                      format_name){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    object_data <- reactiveValues(graph=NULL)

    observeEvent(input$finalGraphButton,
                 {
                   result_dir <- eval(parse(text = paste0(file_paths_name, "$", variable_containing_output_dir_path)))

                   x <- create_final_data_graph(readxl::read_excel(normalizePath(paste0(result_dir,"/final_results.xlsx")),
                                                                   sheet="resultats-bruts",
                                                                   na=c("","NA")),format_name)
                   object_data$graph <- x
                   ggplot2::ggsave(normalizePath(paste0(result_dir,"/final_graph.png")),x,width=10)

                   gargoyle::trigger(final_graph_ready_trigger_name)
                 })
    output$finalGraph <- renderPlot({
      gargoyle::watch(final_graph_ready_trigger_name)
      object_data$graph})
  })
}

## To be copied in the UI
# mod_create_show_final_result_graph_ui("create_show_final_result_graph_1")

## To be copied in the server
# mod_create_show_final_result_graph_server("create_show_final_result_graph_1")
