#' editable_bacteria_density_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_editable_bacteria_density_table_ui <- function(id) {
  ns <- NS(id)
  tagList(DT::dataTableOutput(ns("mytable")))
}

#' editable_bacteria_density_table Server Functions
#'
#' @noRd
mod_editable_bacteria_density_table_server <- function(id,
                                                       file_paths,
                                                       file_paths_name =
                                                         deparse(substitute(file_paths)),
                                                       variable_containing_table_filelist_csv,
                                                       variable_containing_csv_index,
                                                       gargoyle_csv_available_trigger_name,
                                                       gargoyle_image_nav_trigger_name,
                                                       tibble_object_name) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
object_data <- reactiveValues(data=NULL)

    columns2show <-  c(
      "grid_coordinate",
      "sum_countable",
      "bact_density_wo_uncountable",
      "bloq",
      "relevant_count",
      "conc"
    )

    enable_keyboard_edits <- c(
      "table.on('key', function(e, datatable, key, cell, originalEvent){",
      "  var targetName = originalEvent.target.localName;",
      "  if(key == 13 && targetName == 'body'){",
      "    $(cell.node()).trigger('dblclick.dt');",
      "  }",
      "});",
      "table.on('keydown', function(e){",
      "  if(e.target.localName == 'input' && [9,13,37,38,39,40].indexOf(e.keyCode) > -1){",
      "    $(e.target).trigger('blur');",
      "  }",
      "});",
      "table.on('key-focus', function(e, datatable, cell, originalEvent){",
      "  var targetName = originalEvent.target.localName;",
      "  var type = originalEvent.type;",
      "  if(type == 'keydown' && targetName == 'input'){",
      "    if([9,37,38,39,40].indexOf(originalEvent.keyCode) > -1){",
      "      $(cell.node()).trigger('dblclick.dt');",
      "    }",
      "  }",
      "});"
    )

    gargoyle::on(gargoyle_csv_available_trigger_name,
                 {
                   eval(parse(
                     text = paste0(
                       "x<-",
                       file_paths_name,
                       "$",
                       tibble_object_name,
                       "$",
                       variable_containing_table_filelist_csv,
                       "[",
                       file_paths_name,
                       "$",
                       variable_containing_csv_index,
                       "]"
                     )
                   ))
                   golem::print_dev(paste(x, "lapin"))
                   req(x)
                   eval(parse(text="object_data$data <- readr::read_csv(x)"))
                   golem::print_dev(paste(object_data$data, "lapin2"))
                 })

    gargoyle::on(gargoyle_image_nav_trigger_name,
                 {
                   eval(parse(
                     text = paste0(
                       "x<-",
                       file_paths_name,
                       "$",
                       tibble_object_name,
                       "$",
                       variable_containing_table_filelist_csv,
                       "[",
                       file_paths_name,
                       "$",
                       variable_containing_csv_index,
                       "]"
                     )
                   ))
                   golem::print_dev(paste(x, "pouet"))
                   req(x)
                   eval(parse(text="object_data$data <- readr::read_csv(x)"))
                   golem::print_dev(paste(object_data$data, "pouet2"))
                 })

    output$mytable = DT::renderDT({
      gargoyle::watch(gargoyle_csv_available_trigger_name)
      gargoyle::watch(gargoyle_image_nav_trigger_name)

      req(isolate(object_data$data))

      columns2hide = which(!(colnames(isolate(object_data$data)) %in% columns2show))
      column_letter_col_index = which(colnames(isolate(object_data$data)) == "column_letter")
      column_display_names <-
        c(
          'cell' = 'grid_coordinate',
          'BLOQ' = 'bloq',
          'CFU' = 'sum_countable',
          'dilution' = 'dilution',
          'CFU/mL' = 'bact_density_wo_uncountable',
          'ATB' = 'ATB',
          'conc(mg/L)' = 'conc',
          'time' = 'time',
          'strain' = 'strain',
          'modified' = 'modified_by_user',
          'column_letter' = 'column_letter',
          'line_number' = 'line_number',
          '1-bact' = '1-bact',
          '2-bact' = '2-bact',
          '3-bact' = '3-bact',
          'not-countable' = 'not-countable',
          'all_countable' = 'all_countable',
          'relevant' = 'relevant_count'
        )

      DT::datatable(
        isolate(isolate(object_data$data)),
        rownames = FALSE,
        editable = T,
        callback = DT::JS(enable_keyboard_edits),
        selection = 'none',
        extensions = c('RowGroup',"KeyTable"),
        options = list(
          keys=T,
          columnDefs = list(list(
            visible = FALSE, targets = columns2hide - 1
          )),
          rowGroup = list(dataSrc = column_letter_col_index - 1),
          iDisplayLength = 8,
          scrollX = TRUE
        ),
        colnames = column_display_names
      ) %>%
        DT::formatStyle('relevant',
                        target = "row",
                        backgroundColor = DT::styleEqual(1, 'green')) %>%
        DT::formatSignif(columns = c("CFU/mL"),digits=3)

    })

mytable_dt_proxy <- DT::dataTableProxy("mytable")

    observeEvent(input$mytable_cell_edit, {

      info = input$mytable_cell_edit
      utils::str(info)
      object_data$data <<-
        DT::editData(
          isolate(object_data$data),
          info,
          mytable_dt_proxy,
          rownames = FALSE,
          resetPaging = F
          )
      #now we recalculate bacterial density
      recompute_density_df <- info
      recompute_density_df$col <- grep("bact_density_wo_uncountable", names(object_data$data))-1
      recompute_density_df$value <- object_data$data[info$row,"sum_countable"] * 1/(object_data$data[info$row,"dilution"]) * 10^3 %>% as.numeric()
      utils::str(recompute_density_df)
      object_data$data <<-
        DT::editData(
          isolate(object_data$data),
          recompute_density_df,
          mytable_dt_proxy,
          rownames = FALSE,
          resetPaging = F
        )
      #here add code to mark the column as edited
      mark_as_modified_df <- data.frame(
        row = info$row,
        col = grep("modified_by_user", names(object_data$data))-1,
        value = 1
      )
      utils::str(mark_as_modified_df)
      object_data$data <<-
        DT::editData(
          isolate(object_data$data),
          mark_as_modified_df,
          rownames = FALSE,
          resetPaging = F
        )
      #here we update BLOQ column
      edit_bloq_df <- info
      edit_bloq_df$col <- grep("bloq", names(object_data$data))-1
      edit_bloq_df$value <- as.numeric(object_data$data[info$row,"sum_countable"]==0)
      utils::str(edit_bloq_df)
      object_data$data <<-
        DT::editData(
          isolate(object_data$data),
          edit_bloq_df,
          mytable_dt_proxy,
          rownames = FALSE,
          resetPaging = F
        )

      eval(parse(
        text = paste0(
          "x<-",
          file_paths_name,
          "$",
          tibble_object_name,
          "$",
          variable_containing_table_filelist_csv,
          "[",
          file_paths_name,
          "$",
          variable_containing_csv_index,
          "]"
        )
      ))
      readr::write_csv(isolate(object_data$data), x)
    })
  })
}

## To be copied in the UI
# mod_editable_bacteria_density_table_ui("editable_bacteria_density_table_1")

## To be copied in the server
# mod_editable_bacteria_density_table_server("editable_bacteria_density_table_1")
