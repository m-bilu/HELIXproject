library(glmnet)
library(ggplot2)
library(grid)
library(gtable)
full_data<-read.csv('Data/life_data.csv')
library(SmartEDA)
##General Overview of the data
library("GGally")
library(tidyverse)
library(DataExplorer)
full_data %>%
  create_report(
    output_file = 'test',
    output_dir = "/Users/kazirahman/Library/CloudStorage/OneDrive-UniversityofWaterloo/3A/Stat331/Final Project",
    y = "hs_correct_raven",
    report_title = "TITLE"
  )





ggplot(full_data, aes(x=h_cereal_preg_Ter, y=hs_correct_raven, shape=h_cereal_preg_Ter, color=h_cereal_preg_Ter)) +
  geom_point()

###scatterplot
col_names <- colnames(full_data[3:7])
col_names <- col_names[-1]

plot_list <- list()

for (i in col_names){
  plot <- ggplot(full_data, aes_string(x=full_data$hs_correct_raven, y=i)) +
    geom_point()
  plot_list[[i]] <- plot
}
plot_grob <-arrangeGrob(grobs=plot_list)
pdf("testUSgraph.pdf")
grid.arrange(plot_grob)
dev.off()


##interaction plot
preg<-read.csv('Data/preg_data.csv')

loc_preg<-c(names(preg))
#data for pregnancy variables under lifestyle family
preg_data<-full_data[loc_preg]


pd_inter <- aov(data = preg_data, 
                hs_correct_raven~h_fastfood_preg_Ter*h_fish_preg_Ter)
summary(pd_inter)coefficients[,3] 

interaction.plot(x.factor = preg_data$h_fish_preg_Ter, #x-axis variable
                 trace.factor = preg_data$h_fastfood_preg_Ter, #variable for lines
                 response = preg_data$hs_correct_raven, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "raven score",
                 xlab = "cat",
                 col = c("pink", "blue", "green",'black'),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Dairy")


