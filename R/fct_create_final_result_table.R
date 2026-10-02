#' create_final_result_table
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

create_final_result_table <- function(list_of_csv_paths,
                                      format_name){

  if(format_name == "format_1"){
    purrr::map(list_of_csv_paths,~readr::read_csv(.x)) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(relevant_count==1) %>%
      dplyr::select(strain,time,ATB,conc,dilution,sum_countable,bact_density_wo_uncountable,bloq,rep) %>%
      dplyr::rename(CFU=sum_countable,
                    CFU.mL = bact_density_wo_uncountable) %>%
      dplyr::arrange(strain,ATB,conc,rep,time)

  } else if(format_name == "format_2"){
    purrr::map(list_of_csv_paths,~readr::read_csv(.x)) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(relevant_count==1) %>%
      dplyr::select(strain,time,ATB,conc,dilution,sum_countable,bact_density_wo_uncountable,bloq,rep) %>%
      dplyr::rename(CFU=sum_countable,
                    CFU.mL = bact_density_wo_uncountable)%>%
      dplyr::arrange(strain,ATB,conc,rep,time)

  }

}
