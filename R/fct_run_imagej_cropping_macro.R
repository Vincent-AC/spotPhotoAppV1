#' @title Run ImageJ cropping macro
#' @description This function runs an ImageJ macro to crop agar plate pictures and enhance their contrast. It saves pictures in png and hdf5 formats.
#' @param imagej_exec_path path to ImageJ-win64.exe
#' @param macro_path path to the ImageJ macro file
#' @param input_file_paths vector of paths to input bmp files
#' @param output_dir path to directory where cropped pictures
#' @param output_file_suffix suffix to append to the initial filename
#' @return no R output, 1 .png and .h5 file per input bmp
#' @details Can be tweaked to work with other input file format if you modify the imageJ macro file accordingly.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#'
#' @noRd
run_imagej_cropping_macro <- function(imagej_exec_path,
                                      macro_path,
                                      input_file_paths,
                                      output_dir,
                                      output_file_suffix){

  #cut task 25 by 25 photos to make sure that the command does not exceed windows' cmd character input limit
  input_files_string_together <- paste0("'",paste(input_file_paths ,collapse="#"),"'")

  split_file_paths <- function(x,n) split(x, ceiling(seq_along(x)/n))

  list_file_paths_separated <- split_file_paths(input_files_string_together,4)

  run_imagej_for_one_chunk <- function(chunk_file_paths){
    cmd <- paste0(imagej_exec_path," --ij2 --headless --console --run ",macro_path,
                  " filenames=",chunk_file_paths,",",
                  "listseparator='#',",
                  "outputdir='",output_dir,"',",
                  "outputfilesuffix='",output_file_suffix,"'")

    golem::print_dev(cmd)

    system(cmd)
    system("sleep 1")}

  purrr::walk(list_file_paths_separated,run_imagej_for_one_chunk)

}
