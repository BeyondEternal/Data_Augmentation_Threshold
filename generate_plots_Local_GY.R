rm(list = ls())

library(dplyr)
library(ggplot2)

#setwd("~/data_science/argentina")

source("utils.r")
file_path <- "Results_ALL.xlsx"

# Define the name of the columns in the sheets that refers to the traits
traits_columns <- c("summary")

# Read the names of all sheets in the Excel file
sheet <- readxl::excel_sheets(file_path)
# Read a sheet from the Excel file and rename the columns in the expected
# format by the BBA library
results1<- readxl::read_excel(file_path, sheet = sheet)
results1
colnames(results1)
digits=4
results11 <-results1 %>%
  group_by(Dataset, Trait, Method) %>%
  summarise(
    NRMSE = mean(NRMSE),
    MAAPE =mean(MAAPE),
    NRMSE_80 = mean(NRMSE_80),
    MAAPE_80 =mean(MAAPE_80),
    )
#%>%
 # ungroup() %>%
#  mutate(Dataset = dataset, Method =Method) %>%
#  relocate(Dataset = dataset, Method =Method, .before = 1) 

results11
colnames(results11)
write.csv(results11,file="Results_summary_LOEO_CV_V2.csv")

Data_set=unique(results11$Dataset)
for (data in Data_set){
#  Trait_t=response
 results=results11[results11$Dataset==data, ]
  results=droplevels(results)
  
metrics <- c( "NRMSE",    "MAAPE",   "NRMSE_80",   "MAAPE_80")
#metrics <- c(metrics, paste0(metrics, "a"))
metrics 

plots_dir<- paste(data,"plots_New",sep="_")
# delete all the previous plots
#unlink(plots_dir, recursive = true)
# create the directory where the plots are goind to be stored in
dir.create(plots_dir)
for (metric in metrics) {
  #metric=metrics[2]
  cat(metric, "\n")
  plot <- ggplot(
    results,
    aes_string(x = "Trait", y = metric, fill = "Method")
  ) +
    geom_bar(stat = "identity", position = "dodge") 
    # facet_wrap(~Model) +
    # geom_errorbar(
    #   aes_string(
    #     ymin = sprintf("%s - %s_SE/sqrt(1)", metric, metric),
    #     ymax = sprintf("%s +  %s_SE/sqrt(1)", metric, metric)
    #   ),
    #   width = 0.2,
   #   position = position_dodge(0.9),
    #  colour = "black"
    #)
  plot <- white_theme(plot)
  Plot <- plot + theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14, face = "bold")
  ) +
    theme_bw() +
    theme(text = element_text(size = 18))
  Plot <- vertical_x(Plot, angle = 90)
  
  
  save_plot(Plot, file = file.path(plots_dir, paste0(metric, ".png")))
  
}
}
