library(dplyr)
library(openxlsx)

excels <- c(
  "Accuracy_A_Conventional_V3.csv",
  "Accuracy_A_Optimal_Threshold.csv",
  "Accuracy_A_Val_V3.csv",
  "Accuracy_Data_Augmentation_OT.csv"
)
ot <- c("Conventional", "Conventional_TH", "Augmented_TH", "Augmented")
i <- 1
df <- data.frame()
for (data in excels) {
  new_data <- read.csv(data)
  columns <- nrow(new_data)
  columns <- seq_along(1:columns)
  traits <- unique(new_data[3])
  combined_data <- new_data %>%
    group_by(Trait, Model=ot[i]) %>%
    summarise(
      Accuracy = mean(Accuracy, na.rm = T),
      Kappa =mean(Kappa, na.rm = T),
      Sensitivity = mean(Sensitivity, na.rm = T),
      Specificity =mean(Specificity, na.rm = T),
      F1 = mean(F1, na.rm = T)
    )%>%
    ungroup()
  combined_data <- cbind(1:nrow(combined_data),combined_data)
  colnames(combined_data)[1] <- ""
  
  df <- rbind(df,combined_data)
  # Imprimir el nuevo dataframe
  filename <- paste("mean_All.xlsx")
  i <- i + 1
}
  write.xlsx(df, filename, rowNames = FALSE)
