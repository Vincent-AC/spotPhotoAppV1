#' create_final_data_graph
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
create_final_data_graph <- function(final_data_table,
                                    format_name){
  spot_volume_mL <- (1*10^-3) #1 mcL
  final_data_loq_modified <- final_data_table %>%
    dplyr::mutate(loq = 1/(spot_volume_mL*dilution),
           CFU.mL = dplyr::case_when(bloq==1~loq,
                              TRUE~CFU.mL))

  if(format_name == "format_1"){
    ggplot2::ggplot(final_data_loq_modified,
                    ggplot2::aes(x=time,y=CFU.mL,color=as.factor(conc),shape=as.factor(bloq)))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::facet_wrap(~strain+ATB,ncol=3)+
      ggplot2::scale_y_log10("log10(CFU/mL)")+
      ggplot2::scale_x_continuous("Time (h)")+
      ggplot2::theme_bw()

  } else if(format_name == "format_2"){
    ggplot2::ggplot(final_data_loq_modified,
                    ggplot2::aes(x=time,y=CFU.mL,color=as.factor(conc),shape=as.factor(bloq)))+
      ggplot2::geom_point()+
      ggplot2::geom_point()+
      ggplot2::facet_wrap(~strain+ATB+rep,ncol=3)+
      ggplot2::scale_y_log10("log10(CFU/mL)")+
      ggplot2::scale_x_continuous("Time (h)")+
      ggplot2::theme_bw()

  }
}
