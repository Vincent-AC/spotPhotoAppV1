graph_cropped_with_grid_overlayed <- function(cropped_photo_path,
                                              y_grid_start=80, #px y-position of the start of the grid
                                              y_grid_step_size=100, #px y-size of the grid
                                              x_grid_start=160, #px x-position of the start of the grid
                                              x_grid_step_size = 100 #px x-size of the grid
){
  ##### Read files #####
  cropped_photo <- png::readPNG(cropped_photo_path)
  df <-  data.frame(x=945,y=942)
  ##### Plot ######
  ggplot2::ggplot(df,ggplot2::aes(x=x,y=y))+
    ggplot2::annotation_raster(cropped_photo,xmin=0,xmax=945,ymin=0,ymax=-942)+
    ggplot2::scale_x_continuous(limits=c(0,945))+
    ggplot2::scale_y_reverse(limits=c(942,0))+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start,xend=x_grid_start+12*x_grid_step_size,yend=-y_grid_start,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start-1*y_grid_step_size,xend=x_grid_start+12*x_grid_step_size,yend=-y_grid_start-1*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start-2*y_grid_step_size,xend=x_grid_start+12*x_grid_step_size,yend=-y_grid_start-2*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start-3*y_grid_step_size,xend=x_grid_start+12*x_grid_step_size,yend=-y_grid_start-3*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start-4*y_grid_step_size,xend=x_grid_start+12*x_grid_step_size,yend=-y_grid_start-4*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start-5*y_grid_step_size,xend=x_grid_start+12*x_grid_step_size,yend=-y_grid_start-5*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start-6*y_grid_step_size,xend=x_grid_start+12*x_grid_step_size,yend=-y_grid_start-6*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start-7*y_grid_step_size,xend=x_grid_start+12*x_grid_step_size,yend=-y_grid_start-7*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start-8*y_grid_step_size,xend=x_grid_start+12*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start,y=-y_grid_start,xend=x_grid_start,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+1*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+1*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+2*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+2*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+3*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+3*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+4*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+4*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+5*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+5*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+6*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+6*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+7*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+7*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+8*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+8*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+9*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+9*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+10*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+10*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+11*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+11*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_segment(x=x_grid_start+12*x_grid_step_size,y=-y_grid_start,xend=x_grid_start+12*x_grid_step_size,yend=-y_grid_start-8*y_grid_step_size,color="red",lwd=1)+
    ggplot2::geom_text(label="A",x=(x_grid_start+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="B",x=(x_grid_start+1*x_grid_step_size+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="C",x=(x_grid_start+2*x_grid_step_size+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="D",x=(x_grid_start+3*x_grid_step_size+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="E",x=(x_grid_start+4*x_grid_step_size+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="F",x=(x_grid_start+5*x_grid_step_size+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="G",x=(x_grid_start+6*x_grid_step_size+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="H",x=(x_grid_start+7*x_grid_step_size+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="I",x=(x_grid_start+8*x_grid_step_size+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="J",x=(x_grid_start+9*x_grid_step_size+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="K",x=(x_grid_start+10*x_grid_step_size+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="L",x=(x_grid_start+11*x_grid_step_size+x_grid_step_size/2),y=-y_grid_start,color="red",hjust=0.5,vjust=-1,size=5)+
    ggplot2::geom_text(label="1",x=x_grid_start,y=-(y_grid_start+y_grid_step_size/2),color="red",hjust=2,vjust=0.5,size=5)+
    ggplot2::geom_text(label="2",x=x_grid_start,y=-(y_grid_start+1*y_grid_step_size+y_grid_step_size/2),color="red",hjust=2,vjust=0.5,size=5)+
    ggplot2::geom_text(label="3",x=x_grid_start,y=-(y_grid_start+2*y_grid_step_size+y_grid_step_size/2),color="red",hjust=2,vjust=0.5,size=5)+
    ggplot2::geom_text(label="4",x=x_grid_start,y=-(y_grid_start+3*y_grid_step_size+y_grid_step_size/2),color="red",hjust=2,vjust=0.5,size=5)+
    ggplot2::geom_text(label="5",x=x_grid_start,y=-(y_grid_start+4*y_grid_step_size+y_grid_step_size/2),color="red",hjust=2,vjust=0.5,size=5)+
    ggplot2::geom_text(label="6",x=x_grid_start,y=-(y_grid_start+5*y_grid_step_size+y_grid_step_size/2),color="red",hjust=2,vjust=0.5,size=5)+
    ggplot2::geom_text(label="7",x=x_grid_start,y=-(y_grid_start+6*y_grid_step_size+y_grid_step_size/2),color="red",hjust=2,vjust=0.5,size=5)+
    ggplot2::geom_text(label="8",x=x_grid_start,y=-(y_grid_start+7*y_grid_step_size+y_grid_step_size/2),color="red",hjust=2,vjust=0.5,size=5)+
    ggplot2::theme_void()}


graph_cropped_with_grid_overlayed("dev/adapt-app-for-viktoriia/sample-pic-cropped.png",
                                  y_grid_start=0, #px y-position of the start of the grid
                                  y_grid_step_size=116.5, #px y-size of the grid
                                  x_grid_start=0, #px x-position of the start of the grid
                                  x_grid_step_size = 78.9) #px x-size of the grid)
