#' @title Convert object types to bacterial counts
#' @description Starting from ilastik object table with grid coordinates, convert number of each type of objects into bacterial counts
#' @param ilastik_object_table_with_coordinates tibble output of attribute_coordinates_to_ilastik_objects()
#' @return a tibble with the number of objects of each type for each grid coordinate and total counts.
#' @details The resulting tibble has the following added columns :
#' * 1-bact : number of objects identified as 1-bacteria by ilastik
#' * 2-bact : number of objects identified as 2-bacteria by ilastik
#' * 3-bact : number of objects identified as 3-bacteria by ilastik
#' * not-countable : number of objects identified as not-countable by ilastik
#' * all_countable : if TRUE = no 'not-countable' object in this grid coordinate else FALSE
#' * sum_countable : number of countable bacteria
#' * sum_countable_and_not_countable : number of countable bacteria + not-countable objects
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#'
#' @noRd
count_objects_for_each_grid_coordinate <- function(ilastik_object_table_with_coordinates){
  mandatory_cols <- c(`1-bact`=as.numeric(0),`2-bact`=as.numeric(0),`3-bact`=as.numeric(0),`not-countable`=as.numeric(0))
  ilastik_object_table_with_coordinates %>%
    dplyr::select(grid_coordinate,`Predicted Class`) %>%
    table() %>%
    tibble::as_tibble () %>%
    tidyr::pivot_wider(names_from = `Predicted Class`,
                values_from = n) %>%
    tibble::add_column(!!!mandatory_cols[setdiff(names(mandatory_cols), names(.))]) %>% #if one class is missing create column nonetheless
    dplyr::mutate(all_countable = `not-countable`==0,
           sum_countable = `1-bact`+`2-bact`*2+`3-bact`*3)
}
