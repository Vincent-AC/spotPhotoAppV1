#' @title Overlay ilastik object classification results on original picture
#' @description Reads the .csv produced by ilastik object classification and overlays on top of the original picture.
#' Also shows the spotting grid.
#' @param ilastik_object_table path to .csv table produced by ilastik object classification
#' @param cropped_photo_path path to .png picture
#' @param y_grid_start y-position of the start of the spotting grid in pixels, Default: 80
#' @param y_grid_step_size distance in pixel between two horizontal borders of the spotting grid, Default: 100
#' @param x_grid_start x-position of the start of the spotting grid in pixels, Default: 160
#' @param x_grid_step_size distance in pixel between two vertical borders of the spotting grid, Default: 100
#' @return ggplot figure
#' @details The spotting grid is a 6 * 8 grid.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[png]{readPNG}}
#'
#' @noRd
#' @importFrom png readPNG
graph_ilastik_object_csv <- function(ilastik_object_table,
                                     cropped_photo_path,
                                     y_grid_start=80, #px y-position of the start of the grid
                                     y_grid_step_size=100, #px y-size of the grid
                                     x_grid_start=160, #px x-position of the start of the grid
                                     x_grid_step_size = 100 #px x-size of the grid
){
  ##### Read files #####
  raw_object_results <- readr::read_csv(ilastik_object_table)
  cropped_photo <- png::readPNG(cropped_photo_path)
  ##### Plot ######
  ggplot2::ggplot(raw_object_results,
                  ggplot2::aes(x=`Center of the object_0`,
             y=`Center of the object_1`,
             shape=`Predicted Class`,
             color=`Predicted Class`))+
    ggplot2::annotation_raster(cropped_photo,xmin=0,xmax=945,ymin=0,ymax=-942)+
    ggplot2::scale_x_continuous(limits=c(0,945))+
    ggplot2::scale_y_reverse(limits=c(942,0))+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start,xend=x_grid_start+6*x_grid_step_size,yend=-y_grid_start,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start-1*y_grid_step_size,xend=x_grid_start+6*x_grid_step_size,yend=-y_grid_start-1*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start-2*y_grid_step_size,xend=x_grid_start+6*x_grid_step_size,yend=-y_grid_start-2*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start-3*y_grid_step_size,xend=x_grid_start+6*x_grid_step_size,yend=-y_grid_start-3*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start-4*y_grid_step_size,xend=x_grid_start+6*x_grid_step_size,yend=-y_grid_start-4*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start-5*y_grid_step_size,xend=x_grid_start+6*x_grid_step_size,yend=-y_grid_start-5*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start-6*y_grid_step_size,xend=x_grid_start+6*x_grid_step_size,yend=-y_grid_start-6*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start-7*y_grid_step_size,xend=x_grid_start+6*x_grid_step_size,yend=-y_grid_start-7*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start-8*y_grid_step_size,xend=x_grid_start+6*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start,xend=x_grid_start,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+1*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+1*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+2*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+2*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+3*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+3*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+4*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+4*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+5*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+5*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+6*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+6*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_text(label="A",x=(x_grid_start+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="B",x=(x_grid_start+1*x_grid_step_size+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="C",x=(x_grid_start+2*x_grid_step_size+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="D",x=(x_grid_start+3*x_grid_step_size+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="E",x=(x_grid_start+4*x_grid_step_size+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="F",x=(x_grid_start+5*x_grid_step_size+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="1",x=x_grid_start,y=-(y_grid_start+y_grid_step_size/2),color="red",hjust=2,vjust=0.5,size=5)+
    ggplot2::geom_text(label="2",x=x_grid_start,y=-(y_grid_start+1*y_grid_step_size+y_grid_step_size/2),color="red",hjust=2,vjust=0.5,size=5)+
    ggplot2::geom_text(label="3",x=x_grid_start,y=-(y_grid_start+2*y_grid_step_size+y_grid_step_size/2),color="red",hjust=2,vjust=0.5,size=5)+
    ggplot2::geom_text(label="4",x=x_grid_start,y=-(y_grid_start+3*y_grid_step_size+y_grid_step_size/2),color="red",hjust=2,vjust=0.5,size=5)+
    ggplot2::geom_text(label="5",x=x_grid_start,y=-(y_grid_start+4*y_grid_step_size+y_grid_step_size/2),color="red",hjust=2,vjust=0.5,size=5)+
    ggplot2::geom_text(label="6",x=x_grid_start,y=-(y_grid_start+5*y_grid_step_size+y_grid_step_size/2),color="red",hjust=2,vjust=0.5,size=5)+
    ggplot2::geom_text(label="7",x=x_grid_start,y=-(y_grid_start+6*y_grid_step_size+y_grid_step_size/2),color="red",hjust=2,vjust=0.5,size=5)+
    ggplot2::geom_text(label="8",x=x_grid_start,y=-(y_grid_start+7*y_grid_step_size+y_grid_step_size/2),color="red",hjust=2,vjust=0.5,size=5)+
    ggplot2::geom_point(alpha=0.5)+
    ggplot2::theme_void()+
    ggplot2::theme(legend.position="bottom")}

