#' @title Compute bacterial densities
#' @description Compute bacterial densities with and without uncountable objects
#' @param bact_count_with_grid_coordinates_and_exp_conditions tibble output of associate_grid_coordinates_with_exp_conditions()
#' @param spot_volume volume deposited on each spot of (diluted) sample, Default: 1 * 10^-6
#' @return a tibble
#' @details none
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#'
#' @noRd
compute_bact_density <- function(bact_count_with_grid_coordinates_and_exp_conditions,
                                 spot_volume = 1*10^-6){
  bact_count_with_grid_coordinates_and_exp_conditions %>%
    dplyr::mutate(bact_density_wo_uncountable = sum_countable * 1/dilution * 1/spot_volume * 10^-3,
           bact_density_with_uncountable = (sum_countable+`not-countable`) * 1/dilution * 1/spot_volume * 10^-3)
}
