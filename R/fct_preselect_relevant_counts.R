#' @return The return value, if any, from executing the function.
#' @title Identify the most relevant count for each column
#' @description Based on a set of predetermined rules propose a set of the most relevant measurement for each grid column
#' @param bact_density_with_grid_coordinates_and_exp_conditions tibble output of compute_bact_density()
#' @return a tibble
#' @details Bacterial counts outside the grid are filtered out then a column is added, it is named relevant count and is
#' =1 for the most relevant count of the column and 0 for other counts. The rules are as follows :
#' * If the whole column was empty of objects the least diluted sample is flagged as relevant
#' * If the whole column contained not-countable objects the most diluted sample is flagged as relevant
#' * Else the lowest diluted sample with the next dilution being empty is flagged as relevant
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#'
#' @noRd
preselect_relevant_counts <-
  function(bact_density_with_grid_coordinates_and_exp_conditions) {
    bact_density_with_grid_coordinates_and_exp_conditions %>%
      dplyr::arrange(grid_coordinate) %>%
      # Extract column letter and line number for grid coordinates
      tidyr::separate(
        grid_coordinate,
        1,
        into = c("column_letter", "line_number"),
        remove = F
      ) %>%
      #Remove all counts outside the grid
      dplyr::filter(!(column_letter %in% c("N", "G") |
                 line_number %in% c(0, 9))) %>%
      dplyr::group_by(column_letter) %>%
      dplyr::mutate(bloq = dplyr::case_when(is.na(bact_density_with_uncountable) ~ 1,
                              TRUE ~ 0),
             line_number = as.numeric(line_number),
             next_dil_empty = as.numeric(dplyr::lead(bloq)==1),
             next_dil_empty_and_positive_count = as.numeric(next_dil_empty == 1 & bact_density_wo_uncountable > 1),
             relevant_count=dplyr::case_when(sum(is.na(bact_density_wo_uncountable)) == dplyr::n() & dilution == max(dilution) ~ 1,
                                      sum(all_countable, na.rm = T) == 0 & dilution == min(dilution, na.rm = T) & (sum(next_dil_empty_and_positive_count,na.rm=T)==0|is.na(sum(next_dil_empty_and_positive_count,na.rm=T)))~ 1,
                                      dilution == max(dilution[next_dil_empty_and_positive_count%in%T])~1,
                                      # dilution == max(dilution[all_countable%in%T])~1, the least diluted sample with no not-countable object is flagged as relevant
                                      TRUE~0)) %>%
      dplyr::mutate(relevant_count==dplyr::case_when(sum(relevant_count)>1&dilution==max(dilution[relevant_count %in%1])~1,
                                                     sum(relevant_count)>1&dilution!=max(dilution[relevant_count %in%1])~0,
                                                     TRUE~relevant_count))
  }
