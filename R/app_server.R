#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import gargoyle
#' @noRd
app_server <- function(input, output, session) {
  # Create new object to share data across modules using the R6 Class
  file_paths <- FilePaths$new()
  gargoyle::init("fijiExeSelection")
  gargoyle::init("fijiMacroSelection")
  gargoyle::init("ilastikExeSelection")
  gargoyle::init("fileSelection")
  gargoyle::init("expeFileSelection")
  gargoyle::init("resultsDirSelection")
  gargoyle::init("imageCroppingDone")
  gargoyle::init("rawImageIndex")
  gargoyle::init("rawImageNav")
  gargoyle::init("croppedImageNav")
  gargoyle::init("ilastikAutocontextFileSelection")
  gargoyle::init("ilastikObjectClassFileSelection")
  gargoyle::init("postProcessingDone")
  gargoyle::init("finalTableReady")
  gargoyle::init("finaGraphReady")

  volumes <- shinyFiles::getVolumes()

  mod_save_load_R6_object_server(
    "save_load_R6_object_1",
    file_paths,
    tibble_object_name = "fileTable",
    root = volumes
  )
  mod_input_files_buttons_server(
    "input_files_buttons_1",
    file_paths,
    variable_containing_input_file_format = "inputFileFormat",
    tibble_object_name = "fileTable"
  )
  mod_software_settings_server("software_settings_1", file_paths)
  mod_image_carousel_server(
    "image_carousel_1",
    file_paths,
    variable_containing_img_paths = "picpaths",
    variable_containing_img_index = "rawImageIndex",
    gargoyle_image_available_trigger_name = "fileSelection",
    gargoyle_image_nav_trigger_name = "rawImageNav",
    width = "1280px",
    height = "1024px",
    tibble_object_name = "fileTable"
  )
  mod_dir_select_button_server(
    "dir_select_button_1",
    file_paths,
    variable_to_store_filelist = "resultspath",
    gargoyle_trigger_name = "resultsDirSelection",
    root = volumes
  )
  mod_imagej_crop_server(
    "imagej_crop_1",
    file_paths,
    variable_containing_imagej_path = "fijipath",
    variable_containing_imagej_macro_path =
      "fijimacropath",
    variable_containing_raw_img_paths = "picpaths",
    variable_containing_output_dir_path =
      "resultspath",
    variable_to_store_output_filelist_png =
      "croppedfilespathPNG",
    variable_to_store_output_filelist_h5 =
      "croppedfilespathH5",
    variable_containing_img_index = "processedImageIndex",
    variable_containing_start_time = "processingStartTime",
    output_dir_suffix = "_cropped",
    gargoyle_trigger_name = "imageCroppingDone",
    gargoyle_image_nav_trigger_name = "croppedImageNav",
    width = "945px",
    height = "942px",
    tibble_object_name = "fileTable"
  )
  mod_run_ilastik_and_show_results_server(
    "run_ilastik_and_show_results_1",
    file_paths,
    format_name = file_paths$inputFileFormat,
    variable_containing_raw_img_paths =
      "picpaths",
    variable_containing_output_dir_path =
      "resultspath",
    variable_containing_cropped_filelist_png =
      "croppedfilespathPNG",
    variable_containing_ilastik_exec_path =
      "ilastikpath",
    variable_containing_ilastik_autocontext_project_path =
      "autocontextpath",
    ilastik_autocontext_output_format =
      "compressed hdf5",
    ilastik_autocontext_output_filename_format =
      "{dataset_dir}\\{nickname}_pixel.h5",
    ilastik_autocontext_export_source =
      "Probabilities Stage 2",
    variable_containing_ilastik_autocontext_input_file_paths =
      "croppedfilespathH5",
    variable_containing_ilastik_object_class_project_path =
      "objectclasspath",
    ilastik_object_class_output_table_format =
      "{dataset_dir}\\{nickname}_objects.csv",
    ilastik_object_class_output_format =
      "png",
    ilastik_object_class_output_filename_format =
      "{dataset_dir}\\{nickname}_objects.h5",
    ilastik_object_class_export_source =
      "Object Predictions",
    variable_containing_ilastik_pixel_prediction_file_paths =
      "pixelclassresultpath",
    variable_containing_ilastik_object_table_file_paths =
      "objectclasstablepath",
    variable_containing_expe_file_paths =
      "expepath",
    variable_to_store_table_filelist_csv =
      "outputtablecsvpath",
    variable_to_store_output_filelist_png =
      "outputggplotpath",
    variable_containing_img_index =
      "processedImageIndex",
    variable_containing_start_time =
      "processingStartTime",
    variable_containing_end_time =
      "processingEndTime",
    gargoyle_crop_trigger_name = "imageCroppingDone",
    gargoyle_trigger_name = "postProcessingDone",
    gargoyle_image_nav_trigger_name =
      "croppedImageNav",
    width = "945px",
    height = "942px",
    tibble_object_name = "fileTable"
  )
  mod_editable_bacteria_density_table_server(
    "editable_bacteria_density_table_1",
    file_paths,
    variable_containing_table_filelist_csv =
      "outputtablecsvpath",
    variable_containing_csv_index =
      "processedImageIndex",
    gargoyle_csv_available_trigger_name =
      "postProcessingDone",
    gargoyle_image_nav_trigger_name =
      "croppedImageNav",
    tibble_object_name = "fileTable"
  )
  mod_create_show_final_result_table_server(
    "create_show_final_result_table_1",
    file_paths,
    variable_containing_table_filelist_csv =
      "outputtablecsvpath",
    variable_containing_output_dir_path =
      "resultspath",
    final_table_ready_trigger_name =
      "finalTableReady",
    format_name = file_paths$inputFileFormat,
    tibble_object_name = "fileTable"
  )
  mod_create_show_final_result_graph_server(
    "create_show_final_result_graph_1",
    file_paths,
    variable_containing_output_dir_path =
      "resultspath",
    final_graph_ready_trigger_name =
      "finaGraphReady",
    format_name = file_paths$inputFileFormat
  )
}
