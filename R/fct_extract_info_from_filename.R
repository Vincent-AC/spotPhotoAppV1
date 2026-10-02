#' extract_info_from_filename
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

extract_info_from_filename <- function(format_name,
                                       file_name)
{
  if(format_name == "format_1"){
    stringr::str_split(file_name,"_",simplify=TRUE) %>%
      tibble::as_tibble() %>%
      dplyr::rename(time=1,
             strain=2,
             ATB=3) %>%
      dplyr::mutate(filename = file_name,
             time = gsub("T","",time) %>% as.numeric(),
             rep=1,
             strain = as.character(strain))

  } else if(format_name == "format_2"){
    stringr::str_split(file_name,"_",simplify=TRUE) %>%
      tibble::as_tibble() %>%
      dplyr::rename(time=1,
                    genus=2,
                    species=3,
                    strain=4,
                    ATB=5,
                    rep=6) %>%
      dplyr::mutate(filename = file_name,
                    time = gsub("T","",time) %>% as.numeric(),
                    strain = paste0(genus,species,strain))

  }
}
