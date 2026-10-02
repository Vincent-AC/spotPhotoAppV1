#' create_show_final_result_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_create_show_final_result_table_ui <- function(id) {
  ns <- NS(id)
  tagList(box(
    actionButton(ns("finalTableButton"), label = "Create final table"),
    tagList(DT::dataTableOutput(ns("finaltable"))),
    width = 12
  ))
}

#' create_show_final_result_table Server Functions
#'
#' @noRd
mod_create_show_final_result_table_server <- function(id,
                                                      file_paths,
                                                      file_paths_name =
                                                        deparse(substitute(file_paths)),
                                                      variable_containing_table_filelist_csv,
                                                      variable_containing_output_dir_path,
                                                      final_table_ready_trigger_name,
                                                      format_name,
                                                      tibble_object_name) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    object_data <- reactiveValues(data = NULL)

    observeEvent(input$finalTableButton,
                 {
                   results_csv_paths <- eval(parse(
                     text = paste0(
                       file_paths_name,
                       "$",
                       tibble_object_name,
                       "$",
                       variable_containing_table_filelist_csv
                     )
                   ))
                   result_dir <-
                     eval(parse(text = paste0(
                       file_paths_name, "$",
                       variable_containing_output_dir_path
                     )))
                   x <-
                     create_final_result_table(results_csv_paths, format_name)
                   object_data$data <- x



                   wb1 <-
                     XLConnect::loadWorkbook(filename = normalizePath(paste0(result_dir, "/final_results.xlsx")), create = T)
                   XLConnect::createSheet(wb1, "resultats-bruts")
                   XLConnect::writeWorksheet(
                     wb1,
                     data = x,
                     sheet = "resultats-bruts",
                     startRow = 1,
                     startCol = 1,
                     header = TRUE
                   )
                   XLConnect::saveWorkbook(wb1)

                   f <- function(x, y) {
                     if (is.na(y)) {
                       x
                     } else{
                       x + y  + 14
                     }
                   }

                   summary_data <- x %>%
                     dplyr::group_by(strain, ATB, rep) %>%
                     dplyr::summarise(n_conc = length(unique(conc)),
                                      n_time = length(unique(time))) %>%
                     dplyr::mutate(start_line_info = 1,
                                   start_line_table = 3) %>%
                     dplyr::ungroup() %>%
                     dplyr::mutate(lag_conc = dplyr::lag(n_conc)) %>%
                     dplyr::mutate(
                       start_line_info = purrr::accumulate(.x = lag_conc, f, .init = 1)[-1] %>% unlist(),
                       start_line_table = purrr::accumulate(.x = lag_conc, f, .init =
                                                              3)[-1] %>% unlist()
                     )
                   golem::print_dev(summary_data)
                   XLConnect::createSheet(wb1, "resultats-formates")
                   XLConnect::saveWorkbook(wb1)
                   result_dir <-
                     eval(parse(
                       text = paste0(
                         file_paths_name,
                         "$",
                         variable_containing_output_dir_path
                       )
                     ))
                   purrr::pwalk(
                     list(
                       summary_data$strain,
                       summary_data$ATB,
                       summary_data$rep
                     ),
                     ~ spsComps::shinyCatch(
                       create_formatted_result_excel_sheet(
                         x,
                         summary_data,
                         wb1,
                         "resultats-formates",
                         ..1,
                         ..2,
                         ..3,
                         result_dir
                       )
                     )
                   )

                   rJava::.jgc()

                   gargoyle::trigger(final_table_ready_trigger_name)
                 })

    output$finaltable = DT::renderDT({
      gargoyle::watch(final_table_ready_trigger_name)
      strain_col_index = which(colnames(isolate(object_data$data)) == "strain")

      req(isolate(object_data$data))

      DT::datatable(
        isolate(isolate(object_data$data)),
        rownames = FALSE,
        selection = 'none',
        extensions = c('RowGroup', "KeyTable"),
        options = list(
          keys = T,
          rowGroup = list(dataSrc = strain_col_index - 1),
          scrollX = TRUE
        )
      ) %>%
        DT::formatSignif(columns = c("CFU.mL"), digits = 3)

    })

  })
}

## To be copied in the UI
# mod_create_show_final_result_table_ui("create_show_final_result_table_1")

## To be copied in the server
# mod_create_show_final_result_table_server("create_show_final_result_table_1")
