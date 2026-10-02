#' run_ilastik_and_show_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_run_ilastik_and_show_results_ui <- function(id, width, height) {
  ns <- NS(id)
  tagList(
    div(style = "display:inline-block", uiOutput(ns('tickmark'))),
    mod_image_carousel_ui(
      ns("cropped_images_carousel"),
      width = width,
      height = height
    )
  )
}

#' run_ilastik_and_show_results Server Functions
#'
#' @noRd
mod_run_ilastik_and_show_results_server <- function(id,
                                                    file_paths,
                                                    file_paths_name = deparse(substitute(file_paths)),
                                                    format_name,
                                                    variable_containing_raw_img_paths,
                                                    variable_containing_output_dir_path,
                                                    variable_containing_cropped_filelist_png,
                                                    variable_containing_ilastik_exec_path,
                                                    variable_containing_ilastik_autocontext_project_path,
                                                    ilastik_autocontext_output_format =
                                                      "compressed hdf5",
                                                    ilastik_autocontext_output_filename_format =
                                                      "{dataset_dir}\\{nickname}_pixel.h5",
                                                    ilastik_autocontext_export_source =
                                                      "Probabilities Stage 2",
                                                    variable_containing_ilastik_autocontext_input_file_paths,
                                                    variable_containing_ilastik_object_class_project_path,
                                                    ilastik_object_class_output_table_format =
                                                      "{dataset_dir}\\{nickname}_objects.csv",
                                                    ilastik_object_class_output_format =
                                                      "png",
                                                    ilastik_object_class_output_filename_format =
                                                      "{dataset_dir}\\{nickname}_objects.h5",
                                                    ilastik_object_class_export_source =
                                                      "Object Predictions",
                                                    variable_containing_ilastik_pixel_prediction_file_paths,
                                                    variable_containing_ilastik_object_table_file_paths,
                                                    variable_containing_expe_file_paths,
                                                    variable_to_store_table_filelist_csv,
                                                    variable_to_store_output_filelist_png,
                                                    variable_containing_img_index,
                                                    variable_containing_start_time,
                                                    variable_containing_end_time,
                                                    gargoyle_crop_trigger_name,
                                                    gargoyle_trigger_name,
                                                    gargoyle_image_nav_trigger_name,
                                                    width,
                                                    height,
                                                    tibble_object_name) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    gargoyle::on(gargoyle_crop_trigger_name,
                 {
                   run_ilastik_autocontext(
                     ilastik_exec_path = eval(parse(
                       text = paste0(
                         file_paths_name,
                         "$",
                         variable_containing_ilastik_exec_path
                       )
                     )),
                     ilastik_project_path = eval(parse(
                       text = paste0(
                         file_paths_name,
                         "$",
                         variable_containing_ilastik_autocontext_project_path
                       )
                     )),
                     ilastik_output_format = ilastik_autocontext_output_format,
                     ilastik_output_filename_format = ilastik_autocontext_output_filename_format,
                     ilastik_export_source = ilastik_autocontext_export_source,
                     input_file_paths = eval(parse(
                       text = paste0(
                         file_paths_name,
                         "$",
                         tibble_object_name,
                         "$",
                         variable_containing_ilastik_autocontext_input_file_paths
                       )
                     ))
                   )
                   # eval(parse(
                   #   text = paste0(
                   #     file_paths_name,
                   #     "$",
                   #     variable_containing_ilastik_pixel_prediction_file_paths,
                   #     "<-stringr::str_sort(list.files(full.names=T,path=",
                   #     file_paths_name,
                   #     "$",
                   #     variable_containing_output_dir_path,
                   #     ",pattern=\"_cropped_pixel.h5\"),numeric=T)"
                   #   )
                   # ))
                   eval(parse(
                     text = paste0(
                       file_paths_name,
                       "$",
                       tibble_object_name,
                       "$",
                       variable_containing_ilastik_pixel_prediction_file_paths,
                       "<-stringr::str_sort(list.files(full.names=T,path=",
                       file_paths_name,
                       "$",
                       variable_containing_output_dir_path,
                       ",pattern=paste0(\"_cropped_pixel.h5\")),numeric=T)"
                     )
                   ))
                   golem::print_dev(eval(parse(
                     text = paste0(file_paths_name,
                                   "$",
                                   tibble_object_name)
                   )))


                   run_ilastik_object_prediction(
                     ilastik_exec_path = eval(parse(
                       text = paste0(
                         file_paths_name,
                         "$",
                         variable_containing_ilastik_exec_path
                       )
                     )),
                     ilastik_project_path = eval(parse(
                       text = paste0(
                         file_paths_name,
                         "$",
                         variable_containing_ilastik_object_class_project_path
                       )
                     )),
                     ilastik_output_table_format = ilastik_object_class_output_table_format,
                     ilastik_output_format = ilastik_object_class_output_format,
                     ilastik_output_filename_format = ilastik_object_class_output_filename_format,
                     ilastik_export_source = ilastik_object_class_export_source,
                     raw_input_file_paths = eval(parse(
                       text = paste0(
                         file_paths_name,
                         "$",
                         tibble_object_name,
                         "$",
                         variable_containing_ilastik_autocontext_input_file_paths
                       )
                     )),
                     pixel_prediction_file_paths = eval(parse(
                       text = paste0(
                         file_paths_name,
                         "$",
                         tibble_object_name,
                         "$",
                         variable_containing_ilastik_pixel_prediction_file_paths
                       )
                     ))
                   )
                   # eval(parse(
                   #   text = paste0(
                   #     file_paths_name,
                   #     "$",
                   #     variable_containing_ilastik_object_table_file_paths,
                   #     "<-stringr::str_sort(list.files(full.names=T,path=",
                   #     file_paths_name,
                   #     "$",
                   #     variable_containing_output_dir_path,
                   #     ",pattern=\"_cropped_objects_table.csv\"),numeric=T)"
                   #   )
                   # ))

                   eval(parse(
                     text = paste0(
                       file_paths_name,
                       "$",
                       tibble_object_name,
                       "$",
                       variable_containing_ilastik_object_table_file_paths,
                       "<-stringr::str_sort(list.files(full.names=T,path=",
                       file_paths_name,
                       "$",
                       variable_containing_output_dir_path,
                       ",pattern=paste0(\"_cropped_objects_table.csv\")),numeric=T)"
                     )
                   ))
                   golem::print_dev(eval(parse(
                     text = paste0(file_paths_name,
                                   "$",
                                   tibble_object_name)
                   )))

                   process_ilastik_results <-
                     function(ilastik_object_table,
                              cropped_photo_path,
                              expe_xlsx_path,
                              input_pics_paths,
                              format_name) {
                       file_name <- tools::file_path_sans_ext(basename(input_pics_paths))

                       final_plot <-
                         graph_cropped_with_grid_overlayed(
                           cropped_photo_path,
                           y_grid_start = 80,
                           y_grid_step_size = 100,
                           x_grid_start = 160,
                           x_grid_step_size = 100
                         )
                       ggplot2::ggsave(
                         paste0(
                           tools::file_path_sans_ext(cropped_photo_path),
                           "_final_plot.png"
                         ),
                         final_plot,
                         width = 7,
                         height = 5
                       )
                       mic_table <-
                         readxl::read_excel(expe_xlsx_path, sheet = "CMI") %>%
                         dplyr::mutate(Souche = as.character(Souche)) %>%
                         dplyr::rename(strain = Souche,
                                       MIC = CMI)

                       exp_conditions_table_with_coordiantes <-
                         expand.grid(LETTERS[1:6], 1:8) %>%
                         tidyr::unite("grid_coordinate", c("Var1", "Var2"), sep = "") %>%
                         dplyr::arrange(grid_coordinate) %>%
                         dplyr::mutate(dilution = rep(10 ^ -seq(0, 7, 1), 6),
                                       xMIC = sort(rep(c(
                                         0, 2 ^ seq(-2, 2, 1)
                                       ), 8), decreasing = T)) %>%
                         tibble::as_tibble()


                       info_from_filename_df <-
                         extract_info_from_filename(format_name = format_name,
                                                    file_name = file_name)

                       attribute_coordinates_to_ilastik_objects(
                         ilastik_object_table,
                         y_grid_start = 80,
                         y_grid_step_size = 100,
                         x_grid_start = 160,
                         x_grid_step_size = 100
                       ) %>%
                         count_objects_for_each_grid_coordinate %>%
                         associate_grid_coordinates_with_exp_conditions(exp_conditions_table_with_coordiantes) %>%
                         compute_bact_density(spot_volume = 1 * 10 ^ -6) %>%
                         preselect_relevant_counts %>%
                         dplyr::mutate(filename = file_name,
                                       modified_by_user = 0) %>%
                         dplyr::full_join(info_from_filename_df) %>%
                         dplyr::left_join(mic_table) %>%
                         dplyr::mutate(conc = xMIC * MIC) %>%
                         readr::write_csv(paste0(
                           tools::file_path_sans_ext(cropped_photo_path),
                           "_final_table.csv"
                         ))

                     }
                   purrr::pwalk(
                     list(
                       eval(parse(
                         text = paste0(
                           file_paths_name,
                           "$",
                           tibble_object_name,
                           "$",
                           variable_containing_ilastik_object_table_file_paths
                         )
                       )),
                       eval(parse(
                         text = paste0(
                           file_paths_name,
                           "$",
                           tibble_object_name,
                           "$",
                           variable_containing_cropped_filelist_png
                         )
                       )),
                       eval(parse(
                         text = paste0(
                           file_paths_name,
                           "$",
                           tibble_object_name,
                           "$",
                           variable_containing_expe_file_paths
                         )
                       )),
                       eval(parse(
                         text = paste0(
                           file_paths_name,
                           "$",
                           tibble_object_name,
                           "$",
                           variable_containing_raw_img_paths
                         )
                       )),
                       format_name
                     ),
                     ~ spsComps::shinyCatch(process_ilastik_results(..1, ..2, ..3, ..4, ..5))
                   )
                   eval(parse(
                     text = paste0(
                       file_paths_name,
                       "$",
                       variable_containing_end_time,
                       "<-Sys.time()"
                     )
                   ))

                   eval(parse(
                     text = paste0(
                       "processing_time <- hms::round_hms(hms::as_hms(difftime(",
                       file_paths_name,
                       "$",
                       variable_containing_end_time,
                       ",",
                       file_paths_name,
                       "$",
                       variable_containing_start_time,
                       ")),secs=1)"
                     )
                   ))
                   print(paste("Processing duration:", processing_time))
                   gargoyle::trigger(gargoyle_trigger_name)


                 })
    output$tickmark <- renderUI({
      eval(parse(
        text = paste0(
          "processing_time <- hms::round_hms(hms::as_hms(difftime(",
          file_paths_name,
          "$",
          variable_containing_end_time,
          ",",
          file_paths_name,
          "$",
          variable_containing_start_time,
          ")),secs=1)"
        )
      ))
      gargoyle::watch(gargoyle_trigger_name)
      if (eval(parse(
        text = paste0(
          "is.null(",
          file_paths_name,
          "$",
          variable_containing_end_time,
          ")"
        )
      ))) {
        print(NULL)
      } else {
        p(icon("check"),
          paste("Processing done ! Duration:", processing_time))
      }
    })
    gargoyle::on(gargoyle_trigger_name, {
      shinyjs::enable(selector = ".cropButtonEnable")
      shinyjs::removeClass(selector = ".cropButtonAnimate", class = "loading")
      shinyjs::removeClass(selector = ".cropButtonAnimate", class = "dots")
      # eval(parse(
      #   text = paste0(
      #     file_paths_name,
      #     "$",
      #     tibble_object_name,
      #     "$",
      #     variable_to_store_output_filelist_png,
      #     "<-stringr::str_sort(list.files(full.names=T,path=",
      #     file_paths_name,
      #     "$",
      #     variable_containing_output_dir_path,
      #     ",pattern=\"_final_plot.png\"),numeric=T)"
      #   )
      # ))
      # golem::print_dev(eval(parse(
      #   text = paste0(
      #     file_paths_name,
      #     "$",
      #     tibble_object_name,
      #     "$",
      #     variable_to_store_output_filelist_png
      #   )
      # )))
      eval(parse(
        text = paste0(
          file_paths_name,
          "$",
          tibble_object_name,
          "$",
          variable_to_store_output_filelist_png,
          "<-stringr::str_sort(list.files(full.names=T,path=",
          file_paths_name,
          "$",
          variable_containing_output_dir_path,
          ",pattern=paste0(\"_final_plot.png\")),numeric=T)"
        )
      ))
      golem::print_dev(eval(parse(
        text = paste0(file_paths_name,
                      "$",
                      tibble_object_name)
      )))
      # eval(parse(
      #   text = paste0(
      #     file_paths_name,
      #     "$",
      #     variable_to_store_table_filelist_csv,
      #     "<-stringr::str_sort(list.files(full.names=T,path=",
      #     file_paths_name,
      #     "$",
      #     variable_containing_output_dir_path,
      #     ",pattern=\"_final_table.csv\"),numeric=T)"
      #   )
      # ))
      # golem::print_dev(eval(parse(
      #   text = paste0(
      #     file_paths_name,
      #     "$",
      #     variable_to_store_table_filelist_csv
      #   )
      # )))
      eval(parse(
        text = paste0(
          file_paths_name,
          "$",
          tibble_object_name,
          "$",
          variable_to_store_table_filelist_csv,
          "<-stringr::str_sort(list.files(full.names=T,path=",
          file_paths_name,
          "$",
          variable_containing_output_dir_path,
          ",pattern=paste0(\"_final_table.csv\")),numeric=T)"
        )
      ))
      golem::print_dev(eval(parse(
        text = paste0(file_paths_name,
                      "$",
                      tibble_object_name)
      )))
    })

    mod_image_carousel_server(
      "cropped_images_carousel",
      file_paths,
      variable_containing_img_paths = variable_to_store_output_filelist_png,
      variable_containing_img_index = variable_containing_img_index,
      gargoyle_image_available_trigger_name = gargoyle_trigger_name,
      gargoyle_image_nav_trigger_name = gargoyle_image_nav_trigger_name,
      width = width,
      height = height,
      tibble_object_name = tibble_object_name
    )
  })
}

## To be copied in the UI
# mod_run_ilastik_and_show_results_ui("run_ilastik_and_show_results_1")

## To be copied in the server
# mod_run_ilastik_and_show_results_server("run_ilastik_and_show_results_1")
