#' @title Add experimental conditions to each grid coordinate
#' @description Add experimental conditions corresponding to each grid coordinate to the count data
#' @param bact_count_table_with_coordinates tibble output of count_objects_for_each_grid_coordinate()
#' @param exp_conditions_table_with_coordiantes table containing a grid-coordinate column and 1 column for each experimental variable of interest (e.g. time, concentration)
#' @return tibble
#' @details none
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#'
#' @noRd
associate_grid_coordinates_with_exp_conditions <-
  function(bact_count_table_with_coordinates,
           exp_conditions_table_with_coordiantes) {
    dplyr::full_join(bact_count_table_with_coordinates,exp_conditions_table_with_coordiantes)
  }
