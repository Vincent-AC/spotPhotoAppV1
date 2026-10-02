#' image_carousel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_image_carousel_ui <- function(id,width,height) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, align="center", actionButton(ns("previous"), "Previous"), actionButton(ns("next"), "Next")),
    ),

    # fluidRow(column(12,align="center",textOutput(ns("pictureName"),h4))),
    fluidRow(column(12,align="center",uiOutput(ns("pictureSelector")))),
    fluidRow(column(12,align="center",imageOutput(ns("image"), width = width, height = height)))
  )
}

#' image_carousel Server Functions
#'
#' @noRd
mod_image_carousel_server <-
  function(id,
           file_paths,
           file_paths_name = deparse(substitute(file_paths)),
           gargoyle_image_available_trigger_name,
           gargoyle_image_nav_trigger_name,
           variable_containing_img_paths,
           variable_containing_img_index,
           width,
           height,
           tibble_object_name) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      observeEvent(input[["previous"]], {
        eval(parse(text=paste0(file_paths_name,"$",variable_containing_img_index,
                               "<-max(",
                               file_paths_name,"$",variable_containing_img_index,
                               "-1,1)")))
        updateSelectInput(session,inputId="picSelector",
                          choices = eval(parse(text=paste0("as.list(basename(",          file_paths_name,
                                                           "$",
                                                           tibble_object_name,
                                                           "$",
                                                           variable_containing_img_paths,
                                                           "))"))),
                          selected=eval(parse(text=paste0("as.list(basename(",          file_paths_name,
                                                          "$",
                                                          tibble_object_name,
                                                          "$",
                                                          variable_containing_img_paths,
                                                          "))[",
                                                          file_paths_name,"$",variable_containing_img_index,
                                                          "]"))))
        gargoyle::trigger(gargoyle_image_nav_trigger_name)
        golem::print_dev(eval(parse(text=paste0(file_paths_name,"$",variable_containing_img_index))))
      })

      observeEvent(input[["next"]], {
        eval(parse(text=paste0(file_paths_name,"$",variable_containing_img_index,
                               "<-min(",
                               file_paths_name,"$",variable_containing_img_index,
                               "+1,",
                               "length(",
                               file_paths_name,"$",
                               tibble_object_name,
                               "$",variable_containing_img_paths,
                               "))")))
        updateSelectInput(session,inputId="picSelector",
                          choices = eval(parse(text=paste0("as.list(basename(",          file_paths_name,
                                                           "$",
                                                           tibble_object_name,
                                                           "$",
                                                           variable_containing_img_paths,
                                                           "))"))),
                          selected=eval(parse(text=paste0("as.list(basename(",          file_paths_name,
                                                          "$",
                                                          tibble_object_name,
                                                          "$",
                                                          variable_containing_img_paths,
                                                          "))[",
                                                          file_paths_name,"$",variable_containing_img_index,
                                                          "]"))))
        gargoyle::trigger(gargoyle_image_nav_trigger_name)
        golem::print_dev(eval(parse(text=paste0(file_paths_name,"$",variable_containing_img_index))))
        })

      observeEvent(input$picSelector, {
        req(input$picSelector)
        eval(parse(text=paste0(file_paths_name,"$",variable_containing_img_index,
                               "<- which(as.list(basename(",
                               file_paths_name,
                               "$",
                               tibble_object_name,
                               "$",
                               variable_containing_img_paths,
                               ")) %in% input$picSelector)")))
        gargoyle::trigger(gargoyle_image_nav_trigger_name)
        golem::print_dev(eval(parse(text=paste0(file_paths_name,"$",variable_containing_img_index))))},
        ignoreInit = TRUE)

      output$pictureSelector <- renderUI({
        gargoyle::watch(gargoyle_image_available_trigger_name)
        selectInput(ns("picSelector"),
                    "Current picture : ",
                    choices = eval(parse(text=paste0("as.list(basename(",file_paths_name,
                                                     "$",
                                                     tibble_object_name,
                                                     "$",
                                                     variable_containing_img_paths,
                                                     "))"))),
                    selected=eval(parse(text=paste0(file_paths_name,"$",variable_containing_img_index))))
      })
      # output$pictureName <- renderText({
      #   gargoyle::watch(gargoyle_image_available_trigger_name)
      #   gargoyle::watch(gargoyle_image_nav_trigger_name)
      #   paste0("Current picture : ",eval(parse(text=paste0("basename(",file_paths_name,"$",variable_containing_img_paths,"[",file_paths_name,"$",variable_containing_img_index,"])"))))
      # })

      output$image <- renderImage({
        gargoyle::watch(gargoyle_image_available_trigger_name)
        gargoyle::watch(gargoyle_image_nav_trigger_name)
        eval(parse(text=paste0("x<-",file_paths_name,"$",tibble_object_name,"$",variable_containing_img_paths,"[",file_paths_name,"$",variable_containing_img_index,"]")))
        golem::print_dev(x)
        req(x)
        list(
          src = x,
          alt = "alternate text",
          width = width, #"427px",
          height = height # "341px"
        )
      }, deleteFile = FALSE)

    })
  }

## To be copied in the UI
# mod_image_carousel_ui("image_carousel_1")

## To be copied in the server
# mod_image_carousel_server("image_carousel_1")
