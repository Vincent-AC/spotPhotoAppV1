#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    shinyjs::useShinyjs(),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      dashboardHeader(title = "CFU Spot Reader"),
      dashboardSidebar(sidebarMenu(
        menuItem("Input", tabName = "input", icon = icon("dashboard")),
        menuItem(
          "Image processing",
          tabName = "processing",
          icon = icon("th")
        ),
        menuItem("Final results", tabName = "finalResults", icon = icon("th")),
        menuItem(
          "Software settings",
          tabName = "softwareSettings",
          icon = icon("dashboard")
        )
      )),
      dashboardBody(
        tags$head(
          tags$style(
            type = "text/css",
            '
            .loading {
                display: inline-block;
                overflow: hidden;
                height: 1.3em;
                margin-top: -0.3em;
                line-height: 1.5em;
                vertical-align: text-bottom;
                box-sizing: border-box;
            }
            .loading.dots::after {
                text-rendering: geometricPrecision;
                content: "⠋\\A⠙\\A⠹\\A⠸\\A⠼\\A⠴\\A⠦\\A⠧\\A⠇\\A⠏";
                animation: spin10 1s steps(10) infinite;
                animation-duration: 1s;
                animation-timing-function: steps(10);
                animation-delay: 0s;
                animation-iteration-count: infinite;
                animation-direction: normal;
                animation-fill-mode: none;
                animation-play-state: running;
                animation-name: spin10;
            }
            .loading::after {
                display: inline-table;
                white-space: pre;
                text-align: left;
            }
            @keyframes spin10 { to { transform: translateY(-15.0em); } }
            '
          )
        ),
        tabItems(
          # First tab content
          tabItem(
            tabName = "input",
            mod_save_load_R6_object_ui("save_load_R6_object_1"),
            mod_input_files_buttons_ui("input_files_buttons_1"),
            fluidRow(
              box(h3("Imported pictures"),
                mod_image_carousel_ui("image_carousel_1",
                                    width = "75%",
                                    height = "75%"),width=12)
            )
          ),

          # Second tab content
          tabItem(
            tabName = "processing",
            fluidRow(
              box(
              mod_dir_select_button_ui("dir_select_button_1",
                                       label = "Select folder where results shall be saved",
                                       title = "Select folder where results shall be saved"),width=12)
            ),
            fluidRow(
              box(
                mod_imagej_crop_ui("imagej_crop_1",
                                   width = "945px",
                                   height = "942px"),
                mod_run_ilastik_and_show_results_ui(
                  "run_ilastik_and_show_results_1",
                  width =
                    "945px",
                  height =
                    "942px"
                ),
                width = 8
              ),
              box(
                mod_editable_bacteria_density_table_ui("editable_bacteria_density_table_1"),
                width = 4
              )
            ),
          ),
          tabItem(
            tabName = "finalResults",
            fluidRow(
              mod_create_show_final_result_table_ui("create_show_final_result_table_1")
            ),
            fluidRow(
              mod_create_show_final_result_graph_ui("create_show_final_result_graph_1")
            )
          ),
          tabItem(tabName = "softwareSettings",
                  fluidRow(
                    mod_software_settings_ui("software_settings_1")
                  ))
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www",
                    app_sys("app/www"))

  tags$head(favicon(),
            bundle_resources(path = app_sys("app/www"),
                             app_title = "spotPhotosApp"))
            # Add here other external resources
            # for example, you can add shinyalert::useShinyalert())
}
