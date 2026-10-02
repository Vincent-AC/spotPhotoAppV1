#' create_formatted_result_excel_sheet
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
create_formatted_result_excel_sheet <-   function(data,
                                                  summary_data,
                                                  workbook_object,
                                                  sheet_name,
                                                  selected_strain,
                                                  selected_ATB,
                                                  selected_replicate,
                                                  result_dir) {
  selected_data <-
    dplyr::filter(data,
           strain == selected_strain &
             ATB == selected_ATB & rep == selected_replicate) %>%
    dplyr::mutate(cfu=log10(CFU.mL))

  table_start_row <-
    dplyr::filter(
      summary_data,
      strain == selected_strain &
        ATB == selected_ATB & rep == selected_replicate
    ) %>%
    dplyr::pull(start_line_table)

  info_start_row <-
    dplyr::filter(
      summary_data,
      strain == selected_strain &
        ATB == selected_ATB & rep == selected_replicate
    ) %>%
    dplyr::pull(start_line_info)

  n_unique_timepoints <- dplyr::filter(
    summary_data,
    strain == selected_strain &
      ATB == selected_ATB & rep == selected_replicate
  ) %>%
    dplyr::pull(n_time)

  cells_to_merge <-
    paste0("B", info_start_row + 1,":", LETTERS[n_unique_timepoints+1], info_start_row + 1)

  unique_conc <- unique(selected_data$conc)
  unique_time <- sort(as.numeric(unique(selected_data$time)))

  blank_data <- matrix(nrow = length(unique_conc),
                       ncol = length(unique_time) + 1) %>%
    tibble::as_tibble()

  colnames(blank_data) = c("conc", unique_time)

  pull_cfu_value <- function(data, selected_time, selected_conc) {
    pulled_value <- data %>% dplyr::filter(time == as.numeric(selected_time) &
                                      conc == selected_conc) %>% dplyr::pull(cfu)
    if (length(pulled_value) == 0) {
      return(NA)
    } else{
      return(pulled_value)
    }
  }
  conc_unit <- "mg/L"

  plot <-
    ggplot2::ggplot(
      selected_data %>% dplyr::mutate(cfu = dplyr::case_when(bloq == 1 ~ 0,
                                                      TRUE ~
                                                        cfu)),
      ggplot2::aes(
        x = time,
        y = cfu,
        col = as.factor(conc),
        shape = as.factor(bloq),
        group = as.factor(conc)
      )
    ) +
    ggplot2::geom_point(size=2) +
    ggplot2::geom_line(linewidth=1) +
    ggplot2::geom_hline(yintercept=3,linetype="dashed") +
    ggplot2::scale_x_continuous("Temps (h)",breaks=seq(0,30,2)) +
    ggplot2::scale_y_continuous("log10(CFU/mL)", breaks = seq(-2, 12, 1)) +
    ggplot2::scale_color_discrete(paste0("Concentration (", conc_unit,")")) +
    ggplot2::scale_shape_discrete("BLOQ") +
    ggplot2::coord_cartesian(ylim = c(-1, 11), xlim = c(0, 30)) +
    ggplot2::geom_hline(yintercept=3,linetype="dashed")+
    ggplot2::ggtitle(paste(
      selected_strain,
      selected_ATB,
      "Réplicat:",
      selected_replicate
    ),subtitle = "BLQ à 1 CFU/mL")+
    ggplot2::theme_bw()

  ggplot2::ggsave(normalizePath(
    paste0(
      result_dir,
      "/",
      selected_strain,
      "_",
      selected_ATB,
      "_",
      selected_replicate,
      "_graph.png"
    )
  ), plot, width = 22.25,height=10.13,units="cm")

  completed_data <- blank_data %>%
    dplyr::mutate(conc = unique_conc) %>%
    dplyr::mutate(across(
      !ends_with("conc"),
      ~ purrr::map2_dbl(
        as.numeric(dplyr::cur_column()),
        conc,
        ~ pull_cfu_value(selected_data, ..1, ..2)
      )
    ))




  formatted_data <- completed_data %>%
    dplyr::mutate(conc = paste(conc, conc_unit))

  colnames(formatted_data)[1] <- "Conc. AB"
  formatted_data


  spsComps::shinyCatch(XLConnect::mergeCells(workbook_object, sheet = sheet_name, reference = cells_to_merge))
  XLConnect::writeWorksheet(workbook_object,
                 data="Temps (h)",
                 sheet=sheet_name,
                 startRow = info_start_row+1,
                 startCol=2,
                 header = FALSE)
  XLConnect::saveWorkbook(workbook_object)
  XLConnect::writeWorksheet(
    workbook_object,
    data = "Strain:",
    sheet = sheet_name,
    startRow = info_start_row,
    startCol = 2,
    header = FALSE
  )
  XLConnect::writeWorksheet(
    workbook_object,
    data = selected_strain,
    sheet = sheet_name,
    startRow = info_start_row,
    startCol = 3,
    header = FALSE
  )
  XLConnect::writeWorksheet(
    workbook_object,
    data = "ATB:",
    sheet = sheet_name,
    startRow = info_start_row,
    startCol = 4,
    header = FALSE
  )
  XLConnect::writeWorksheet(
    workbook_object,
    data = selected_ATB,
    sheet = sheet_name,
    startRow = info_start_row,
    startCol = 5,
    header = FALSE
  )
  XLConnect::writeWorksheet(
    workbook_object,
    data = "Replicate:",
    sheet = sheet_name,
    startRow = info_start_row,
    startCol = 6,
    header = FALSE
  )
  XLConnect::writeWorksheet(
    workbook_object,
    data = selected_replicate,
    sheet = sheet_name,
    startRow = info_start_row,
    startCol = 7,
    header = FALSE
  )
  cell_names <- paste0("Strain_",
                       selected_strain,
                       "_",
                       selected_ATB,
                       "_",
                       selected_replicate) %>%
    stringr::str_replace_all("[^[:alnum:]]","_")

  XLConnect::createName(
    workbook_object,
    name = cell_names,
    formula = paste0("'",sheet_name,"'", "!$", LETTERS[9], "$", info_start_row),TRUE
  )
  XLConnect::addImage(
    workbook_object,
    normalizePath(
      paste0(
        result_dir,
        "/",
        selected_strain,
        "_",
        selected_ATB,
        "_",
        selected_replicate,
        "_graph.png"
      )
    ),
    name=cell_names,
    TRUE
  )
  XLConnect::writeWorksheet(
    workbook_object,
    data = formatted_data,
    sheet = sheet_name,
    startRow = table_start_row,
    startCol = 1,
    header = TRUE
  )
  XLConnect::saveWorkbook(workbook_object)
}
