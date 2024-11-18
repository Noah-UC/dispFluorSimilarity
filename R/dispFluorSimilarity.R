
dispFluorSimilarity <- function(panel) {

  #installing required packages

  usethis::use_package("ggplot2")
  usethis::use_package("dplyr")

  #loading database

  cytek_data = read.csv("../spectra-11_17_2024, 4_44_54 PM.csv", sep = "," , header = T,  row.names = 1, check.names = FALSE, )


  similarity_df =
    cytek_data %>%
    filter(
      row.names(
        cytek_data) %in% panel) %>%
    select(panel)

  similarity_matrix = as.matrix(
      similarity_df
  )

  similarity_matrix[upper.tri(similarity_matrix)] =
    t(
      similarity_matrix)[upper.tri(similarity_matrix)]

  storage.mode(similarity_matrix) <- "numeric"



  similarity_long = as.data.frame(
    as.table(
      similarity_matrix
      )
    )

  p <- ggplot(data = similarity_long,
              aes(x = Var1, y = Var2, fill = Freq)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red", name = "Value") +
    geom_text(aes(label = Freq), color = "black", size = 5) +
    scale_y_discrete(limits = rev(levels(as.factor(similarity_long$Var2)))) +
    labs(x = "Fluor A", y = "Fluor B", fill = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(p)

}
