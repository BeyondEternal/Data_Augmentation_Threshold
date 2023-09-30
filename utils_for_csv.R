library(dplyr)

excels <- c(
  "Accuracy_A_Conventional_V3.csv",
  "Accuracy_A_Optimal_Threshold.csv",
  "Accuracy_A_Val_V3.csv",
  "Accuracy_Data_Augmentation_OT.csv"
)

for (data in excels) {
  new_data <- read.csv(data)
  columns <- nrow(new_data)
  columns <- seq_along(1:columns)
  combined_data <- new_data %>%
    group_by(Data = 1:nrow(new_data),Trait, Model) %>%
    summarise(
      Accuracy = mean(Accuracy, na.rm = T),
      Kappa =mean(Kappa, na.rm = T),
      Sensitivity = mean(Sensitivity, na.rm = T),
      Specificity =mean(Specificity, na.rm = T),
      F1 = mean(F1, na.rm = T)
    )%>%
    ungroup()
  
  # Imprimir el nuevo dataframe
  filename <- paste("ALL_data.csv")
  write.csv(combined_data, filename, row.names = FALSE)
}
