#' @title Attribute grid coordinates to each object identified by ilastik
#' @description Objects were identified by ilastik object classification and stored in a table, the poisition of the center of each
#' object was used to determine the square of the grid which the object is part of.
#' @param ilastik_object_table path to .csv table produced by ilastik object classification
#' @param y_grid_start y-position of the start of the spotting grid in pixels, Default: 80
#' @param y_grid_step_size distance in pixel between two horizontal borders of the spotting grid, Default: 100
#' @param x_grid_start x-position of the start of the spotting grid in pixels, Default: 160
#' @param x_grid_step_size distance in pixel between two vertical borders of the spotting grid, Default: 100
#' @return a tibble
#' @details When an object is outside the grid horizontally, its column_letter will be "NA" if outside on the left or "G" if
#' outside on the right. When an object is outside of the grid vertically its line_number will be 0 if outside on the top
#' or 9 if outside at the bottom. These coordinates will be used to link the experimental conditions with the colony counts
#' at a later stage.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @noRd
attribute_coordinates_to_ilastik_objects <-
  function(ilastik_object_table,
           y_grid_start =
             80,
           #px y-position of the start of the grid
           y_grid_step_size =
             100,
           #px y-size of the grid
           x_grid_start =
             160,
           #px x-position of the start of the grid
           x_grid_step_size = 100 #px x-size of the grid
  )
  {
    raw_object_results <- readr::read_csv(ilastik_object_table)
    attribute_column_letter <- function(x_coordinate,
                                        x_grid_start,
                                        x_grid_step_size) {
      purrr::map(x_coordinate,   ~c(NA,LETTERS)[findInterval(.x,
                                                      seq(x_grid_start, by = x_grid_step_size, length.out = 7)
      )+1]) %>% unlist()
    }
    attribute_line_number <- function(y_coordinate,
                                      y_grid_start,
                                      y_grid_step_size) {
      purrr::map(y_coordinate,  ~ findInterval(.x,
                                        seq(
                                          y_grid_start, by = y_grid_step_size, length.out =
                                            9
                                        ))) %>% unlist()
    }

    dplyr::mutate(raw_object_results,
           column_letter = attribute_column_letter(`Center of the object_0`,x_grid_start,
                                                   x_grid_step_size),
           line_number = attribute_line_number(`Center of the object_1`,y_grid_start,
                                               y_grid_step_size),
           grid_coordinate = paste0(column_letter,line_number))
  }
