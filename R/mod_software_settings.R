#' software_settings UI Function
#'
#' @description Module in which path to ilastik and fiji-ImageJ will be given. Also will be specified the picture files
#' and files specifying experimental conditions.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_software_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Software settings"),
    mod_file_select_button_ui(ns("fiji_select_button"),
                              label = "Fiji-ImageJ executable location",
                              title = "Please select ImageJ-win64.exe"),
    mod_file_select_button_ui(ns("fiji_macro_select_button"),
                              label = "Fiji-ImageJ macro location",
                              title = "Please select fiji macro"),
    mod_file_select_button_ui(ns("ilastik_select_button"),
                              label = "Ilastik executable location",
                              title = "Please select ilastik.exe"),
 mod_file_select_button_ui(ns("ilastik_autocontext_file_select_button"),
                              label = "Select ilastik file for autocontext model",
                              title = "Please select the .ilp file containing trained autocontext model"),
    mod_file_select_button_ui(ns("ilastik_object_class_file_select_button"),
                              label = "Select ilastik file for object classification model",
                              title = "Please select the .ilp file containing trained object classification model")
    )

}

#' software_settings Server Functions
#'
#' @noRd
mod_software_settings_server <- function(id,file_paths) {
  moduleServer(id, function(input, output, session, fileRoot = NULL) {
    ns <- session$ns
    volumes <- shinyFiles::getVolumes()
    mod_file_select_button_server("fiji_select_button",
                                  file_paths=file_paths,
                                  variable_to_store_filelist="fijipath",
                                  gargoyle_trigger_name="fijiExeSelection",
                                  root = volumes)
    mod_file_select_button_server("fiji_macro_select_button",
                                  file_paths=file_paths,
                                  variable_to_store_filelist="fijimacropath",
                                  gargoyle_trigger_name="fijiMacroSelection",
                                  root = volumes)
    mod_file_select_button_server("ilastik_select_button",
                                  file_paths=file_paths,
                                  variable_to_store_filelist="ilastikpath",
                                  gargoyle_trigger_name="ilastikExeSelection",
                                  root = volumes)
    mod_file_select_button_server("ilastik_autocontext_file_select_button",
                                  file_paths=file_paths,
                                  variable_to_store_filelist="autocontextpath",
                                  gargoyle_trigger_name="ilastikAutocontextFileSelection",
                                  root = volumes)
    mod_file_select_button_server("ilastik_object_class_file_select_button",
                                  file_paths=file_paths,
                                  variable_to_store_filelist="objectclasspath",
                                  gargoyle_trigger_name="ilastikObjectClassFileSelection",
                                  root = volumes)

  })
  }

## To be copied in the UI
# mod_software_settings_ui("software_settings_1")

## To be copied in the server
# mod_software_settings_server("software_settings_1")
