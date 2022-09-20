#' LMD Plot
#'
#' @description
#' Method for plotting Product Functions (PFs) and Residue
#'
#' @param lmd_obj LMD object created from LMD function
#' @param max_pf Number of PFs to Plot
#' @param show_residue Whether to plot residue or not
#' @param pricolor_plot color of plots
#' @param line_size_plot Size of line in ggplot
#'
#' @return ggplot plot for Product Functions (PFs) and Residue
#' @author Shubhra Prakash, \email{shubhraprakash279@@gmail.com}
#' @keywords LMD PF Residue
#' @import ggplot2
#' @import patchwork
#' @examples
#' x=1:100
#' y = (2 / 3 )* sin(x * 30) + (2 / 3) * sin(x * 17.5) + (4 / 5) *cos(x * 2)
#' plot_lmd(lmd(y))
#' @export plot_lmd

plot_lmd <- function(lmd_obj,max_pf=length(lmd_obj[["pf"]]),show_residue=TRUE,pricolor_plot="midnightblue",line_size_plot=1) {
  # requireNamespace(ggplot2)
  # requireNamespace(patchwork)

stopifnot(max_pf<=length(lmd_obj[["pf"]]) && (0<max_pf))

uniplot <- function(series, yname,priColor=pricolor_plot,line_size=line_size_plot){

  x=1:(length(series))
    ggplot2::ggplot() +
    # ggplot2::geom_point(aes(x = x ,y=series),size=point_size,fill=priColor, color = priColor,shape=21)+
    ggplot2::geom_line(aes(x = x,y=series),colour = priColor,size=line_size) +
    ggplot2::ylab(yname) +ggplot2::labs(yname) +ggplot2::theme_bw() +
    ggplot2::theme(panel.border=ggplot2::element_rect(size=0.1),legend.position = c(0.8, 0.8), panel.grid.major.x=ggplot2::element_blank())

  }

plot_list=lapply((1:max_pf), function(x){
  yname=paste0("PF",x)
  series=lmd_obj[["pf"]][[x]]
  p=uniplot(series, yname)
  return(p)

})




if(show_residue){
  series=lmd_obj[["residue"]]
  yname="Residue"
  residue_plot=uniplot(series, yname)
  plot_list[["residue"]]=residue_plot
}

final_plot=wrap_plots(plot_list,ncol=1)

return(suppressMessages(final_plot))

}

