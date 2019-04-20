library(ggplot2)

plot_data_bar2 <- function(xname, xdata, yname, ydata){
  print(xdata)
  print(ydata)
  data=data.frame(name=xdata,  value=ydata)
  ggplot(data, aes(x=xname, y=value)) + geom_bar(stat = "identity")
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

data_norm <- function(data) {
  data[1]/data  #return
}

plot_data <- function (input_file, plot_type="line", plot_norm=FALSE, output_file) {
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
plot_data("./hyper-sf1.csv", output_file = "plot-sf1")
#plot_data("./hyper-sf1.csv", plot_norm=TRUE, output_file = "plot-sf1-norm")

# sf10
#plot_data("./hyper-sf10.csv", output_file = "plot-sf10")
#plot_data("./hyper-sf10.csv", plot_norm=TRUE, output_file = "plot-sf10-norm")
