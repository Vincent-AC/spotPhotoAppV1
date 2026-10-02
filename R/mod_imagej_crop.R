#' imagej-crop-and-show UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_imagej_crop_ui <- function(id,width,height){
  ns <- NS(id)
  tagList(
    div(style="display:inline-block",actionButton(ns("cropButton"), span("Crop and classify images", id = "cropButtonAnimate", class = "cropButtonAnimate"),class="cropButtonEnable"))
  )
}

#' imagej-crop-and-show Server Functions
#'
#' @noRd
mod_imagej_crop_server <- function(id,file_paths,
                                            file_paths_name=deparse(substitute(file_paths)),
                                            variable_containing_imagej_path,
                                            variable_containing_imagej_macro_path,
                                            variable_containing_raw_img_paths,
                                            variable_containing_output_dir_path,
                                            variable_to_store_output_filelist_png,
                                            variable_to_store_output_filelist_h5,
                                            variable_containing_img_index,
                                            variable_containing_start_time,
                                            output_dir_suffix = "_cropped",
                                            gargoyle_trigger_name,
                                            gargoyle_image_nav_trigger_name,
                                            width,
                                            height,
                                            tibble_object_name){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    x <- environment()
    observeEvent(input$cropButton,
                 { shinyjs::addClass(selector = ".cropButtonAnimate", class = "loading")
                   shinyjs::addClass(selector = ".cropButtonAnimate", class = "dots")
                   shinyjs::disable(selector=".cropButtonEnable")
                   eval(parse(text=paste0(file_paths_name,"$",variable_containing_start_time,"<-Sys.time()")))
                   run_imagej_cropping_macro(
                     imagej_exec_path = eval(parse(text = paste0(file_paths_name, "$", variable_containing_imagej_path))),
                     macro_path=eval(parse(text = paste0(file_paths_name, "$", variable_containing_imagej_macro_path))),
                     input_file_paths=eval(parse(text = paste0(file_paths_name, "$",tibble_object_name,"$", variable_containing_raw_img_paths))),
                     output_dir=eval(parse(text = paste0(file_paths_name, "$", variable_containing_output_dir_path))),
                     output_file_suffix=output_dir_suffix
                   )
                   gargoyle::trigger(gargoyle_trigger_name)
                   x$messageCroppingDone <-  "Cropping done !"
                 })
    gargoyle::on(gargoyle_trigger_name,{
      # eval(parse(text=paste0(file_paths_name,"$",variable_to_store_output_filelist_png,"<-stringr::str_sort(list.files(full.names=T,path=",file_paths_name, "$", variable_containing_output_dir_path,",pattern=paste0(output_dir_suffix,\".png\")),numeric=T)")))
      # golem::print_dev(eval(parse(text=paste0(file_paths_name,"$",variable_to_store_output_filelist_png))))

      eval(parse(
        text = paste0(
          file_paths_name,
          "$",
          tibble_object_name,
          "$",
          variable_to_store_output_filelist_png,
          "<-stringr::str_sort(list.files(full.names=T,path=",file_paths_name, "$", variable_containing_output_dir_path,",pattern=paste0(output_dir_suffix,\".png\")),numeric=T)"
        )
      ))
      # eval(parse(text=paste0(file_paths_name,"$",variable_to_store_output_filelist_h5,"<-stringr::str_sort(list.files(full.names=T,path=",file_paths_name, "$", variable_containing_output_dir_path,",pattern=paste0(output_dir_suffix,\".h5\")),numeric=T)")))
      # golem::print_dev(eval(parse(text=paste0(file_paths_name,"$",variable_to_store_output_filelist_h5))))
      eval(parse(
        text = paste0(
          file_paths_name,
          "$",
          tibble_object_name,
          "$",
          variable_to_store_output_filelist_h5,
          "<-stringr::str_sort(list.files(full.names=T,path=",file_paths_name, "$", variable_containing_output_dir_path,",pattern=paste0(output_dir_suffix,\".h5\")),numeric=T)"
        )
      ))
      golem::print_dev(eval(parse(
        text = paste0(file_paths_name,
                      "$",
                      tibble_object_name)
      )))

    })
  })
}

## To be copied in the UI
# mod_imagej_crop_ui("imagej_crop_1")

## To be copied in the server
# mod_imagej_crop_server("imagej_crop_1")
