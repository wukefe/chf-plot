# install.packages("ggplot2");
library(ggplot2)

# sec_axis not works on R version 3.4.1
#          but works on R version 3.5.3
#
# R is upgraded with a newer package from
#   https://mirror.its.sfu.ca/mirror/CRAN/

data_norm <- function(data) {
  data[1]/data  #return
}

data_ratio <- function(data) {
  max(data)/min(data)
}

# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
# https://stackoverflow.com/questions/42919470/wrong-x-axis-order-when-using-geom-bar-with-character-x-object
plot_data_bar2 <- function(mname, xname, xdata, yname, ydata, id){
  #print(xdata)
  #print(ydata)
  df=data.frame(thread=xdata,  value=ydata)
  #print(df)
  if(FALSE){
  pp <- ggplot(data=df, aes(x=factor(thread, levels=thread), y=value)) + 
    geom_bar(stat = "identity", fill = "steelblue", width=0.7) +
    geom_text(aes(label=value), vjust=-1, color="black") +
    #geom_line(aes(y=value), color="red")+
    theme_minimal()+
    ggtitle(mname)
  }
  if(TRUE){
    n=8
    ratio=data_norm(ydata)
    range_label=ceiling(seq(0, max(ratio), max(ratio)/n))
    range_value=ceiling(max(ydata)/max(ratio)*range_label)
    ratio2=max(ydata)/max(ratio)*ratio
  pp <- ggplot(data=df, aes(factor(thread, levels=thread), value)) +
    geom_col(fill = "steelblue", width=0.5) +
    geom_line(aes(factor(thread, levels=thread), ratio2), color="red",  group = 1) +
    geom_point(aes(factor(thread, levels=thread), ratio2), size=3, shape=21, fill="white") +
    #geom_text(aes(label=ratio2), color="black") + 
    scale_y_continuous(
      "Execution Time (ms)",
      sec.axis = dup_axis(name = "Speedup",  breaks = range_value, labels = range_label)
    ) + 
    labs(x=xname) +
    ggtitle(mname)
    pp <- pp + 
      theme(axis.text.y.right = element_text(color = "red"))
  }
  print(pp)
  pp
}

plot_data_bar <- function(mname, xname, xdata, yname, ydata, id){
  barplot(ydata, main=mname, xlab=xname, ylab=yname,
          names.arg=xdata,
          ylim = c(0,max(ydata)))
}

# if scale, add "cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5" into plot
plot_data_line <- function(mname, xname, xdata, yname, ydata, id){
  # xaxt: not draw x-axis
  plot(ydata, type="o", col="blue", xlab=xname, ylab="", xaxt='n', ylim=c(0,ceiling(max(ydata))))
  title(main=mname, font.main=2)
  title(ylab=yname, line=2.5)  # line: distance between ylab and yaxis
  axis(1,at=seq(1, length(xdata)), labels=xdata, cex.axis=1)
}

# src: https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/postscript.html
plot_data_multiple_init <- function(file_dim, file_name){
  scale = 2.2
  file_height = file_dim[1]*scale
  file_width  = file_dim[2]*(scale+0.1)
  setEPS()
  postscript(file=paste("./",file_name,".eps", sep=""),
             onefile=FALSE, height=file_height, width=file_width, bg="white")
  # mar: c(bottom, left, top, right)
  par(mfrow=file_dim, mar = c(2,3.5,2,1), ps=11)  # font-size 12pt
}

# ---------------------------- plot_data_all ----------------------------

plot_data_all <- function (input_file, plot_type="line", plot_norm=FALSE, output_file) {
  data <- read.csv(input_file)
  #print(data[[1]])
  #print(nrow(data))
  #print(as.numeric(data[2,]))
  plot_data_multiple_init(c(4,6), output_file)
  for (id in 1:nrow(data)){
  #for (id in 1:22){
    col_caption="Number of threads"
    row_caption="Execution time (ms)"
    col_names=colnames(data)[2:ncol(data)]
    row_value=as.numeric(data[id,])[2:ncol(data)]
    if(plot_norm){
      row_value = data_norm(row_value)
    }
    main_name=as.vector(data[[1]])[id]
    if (plot_type == "bar") {
      plot_data_line(main_name, col_caption, col_names, row_caption, row_value, id)
    }
    else if (plot_type == "line") {
      plot_data_line(main_name, col_caption, col_names, row_caption, row_value, id)
    }
  }
  dev.off()
}

# sf1
#plot_data_all("./hyper-sf1.csv", output_file = "plot-sf1")
#plot_data_all("./hyper-sf1.csv", plot_norm=TRUE, output_file = "plot-sf1-norm")

# sf10
#plot_data_all("./hyper-sf10.csv", output_file = "plot-sf10")
#plot_data_all("./hyper-sf10.csv", plot_norm=TRUE, output_file = "plot-sf10-norm")

# ---------------------------- plot_data_single ----------------------------
plot_data_partial <- function (input_file, 
                               plot_type="line",
                               plot_norm=FALSE,
                               output_file,
                               plot_id) {
  data <- read.csv(input_file)
  output_file <- paste(output_file, "-q", plot_id, ".eps", sep="")
  #plot_data_multiple_init(c(1,1), output_file)
  id <- plot_id
  col_caption = "Number of threads"
  row_caption = "Execution time (ms)"
  col_names = colnames(data)[2:ncol(data)]
  row_value = as.numeric(data[id, ])[2:ncol(data)]
  if (plot_norm) {
    row_value = data_norm(row_value)
  }
  main_name = as.vector(data[[1]])[id]
  if (plot_type == "bar") {
    # plot_data_bar(main_name, col_caption, col_names, row_caption, row_value, id)
    g = plot_data_bar2(main_name, col_caption, col_names, row_caption, row_value, id)
  }
  else if (plot_type == "line") {
    g = plot_data_line(main_name, col_caption, col_names, row_caption, row_value, id)
  }
  #dev.off()  # or graphics.off()
  ggsave(g, file=output_file, device="eps", width=4, height=3) # save to a file
}

plot_data_partial("./hyper-sf1.csv", plot_type = "bar", output_file = "plot-sf1", plot_id = 3)

