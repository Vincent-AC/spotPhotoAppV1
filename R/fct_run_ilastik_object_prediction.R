#' @title Run ilastik object classification
#' @description Takes input pixel classification/autocontext and matching raw image files and runs a pre-trained ilastik object classification  model in headless batch-mode.
#' @param ilastik_exec_path path to ilastik.exe
#' @param ilastik_project_path path to the ilastik autocontext model file (.ilp)
#' @param ilastik_output_table_format character value for ilastik --table_filename option, see details.
#' @param ilastik_output_format character value for ilastik --output_format option, see details.
#' @param ilastik_output_filename_format character value for ilastik --output_filename_format option, see details.
#' @param ilastik_export_source character value for ilastik --export_source option, see details.
#' @param ilastik_input_axes character value for ilastik --input_axes option, see details. Default: "tzyxc"
#' @param raw_input_file_paths vector of paths to image files on which pixel classification/autocontext was applied
#' @param pixel_prediction_file_paths vector of paths to hdf5 pixel classification/autocontext input files
#' @return no R output, 2 ilastik output files including a .csv table of identified objects
#' @details To understand what ilastik options are please refer to the ilastik headless mode documentation (https://www.ilastik.org/documentation/basics/headless.html)
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @noRd

run_ilastik_object_prediction <- function(ilastik_exec_path,
                                          ilastik_project_path,
                                          ilastik_output_table_format,
                                          ilastik_output_format,
                                          ilastik_output_filename_format,
                                          ilastik_export_source,
                                          raw_input_file_paths,
                                          ilastik_input_axes="tzyxc",
                                          pixel_prediction_file_paths
){
#cut task 25 by 25 photos to make sure that the command does not exceed windows' cmd character input limit

split_file_paths <- function(x,n) split(x, ceiling(seq_along(x)/n))

list_raw_file_paths_separated <- split_file_paths(raw_input_file_paths,25)
list_file_paths_separated <- split_file_paths(pixel_prediction_file_paths,25)

run_ilastik_for_one_chunk <- function(chunk_raw_file_paths,chunk_pixel_pred_file_paths){
  cmd <- paste(shQuote(ilastik_exec_path),
                shQuote("--headless"),
                shQuote(paste0("--project=",ilastik_project_path)),
                shQuote(paste0("--table_filename=",ilastik_output_table_format)),
                shQuote(paste0("--output_format=",ilastik_output_format)),
                shQuote(paste0("--output_filename_format=",ilastik_output_filename_format)),
                shQuote(paste0("--export_source=",ilastik_export_source)),
                shQuote(paste0("--input_axes=",ilastik_input_axes)),
                shQuote("--raw_data"),paste(chunk_raw_file_paths, collapse = " "),
                shQuote("--prediction_maps"),paste(chunk_pixel_pred_file_paths, collapse = " "))

  golem::print_dev(cmd)

  system(cmd)
  system("sleep 1")}

purrr::walk2(list_raw_file_paths_separated,list_file_paths_separated,run_ilastik_for_one_chunk)
}
