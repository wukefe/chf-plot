library(ggplot2)

# set working directory
setwd("~/Documents/GitHub/chf-plot/r/profile-list//")

# function declaration
plot_data_bar <- function(data){
  n <- length(data)
  df=data.frame(cell_id=1:n, cell_len=data)
  pp <- ggplot(data=df, aes(x=cell_id, y=cell_len)) + 
    geom_bar(stat = "identity", fill = "steelblue", width=0.5) +
    geom_text(aes(label=cell_len), vjust=-1, color="black") +
    theme_minimal()+
    labs(x="Cell id ", y="Length of cell") + 
    ggtitle("List Profiling Result")
  print(pp)
}

# paste your code data below
data <- c(1478493,38854,3004998,1478870)

# call function
plot_data_bar(data)


