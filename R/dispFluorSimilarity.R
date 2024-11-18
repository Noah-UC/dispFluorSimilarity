#' Displays a heatmap of similarity indices for fluorochromes in the panel
#'
#' @param panel list of fluors as characters strings
#' @import dplyr
#' @import ggplot2
#' @export
dispFluorSimilarity <- function(panel) {

  #code to make sure at least two character strings are passed in a list

    if (length(panel) < 2) {
    stop("Panel should have at least two Fluors.")
  }

  if (all(panel %in% rownames(cytek_data))==FALSE) {
    stop("Fluor not in database. Check spellings on spectrum.cytekbio.com.")
  }

  #pick out similarity indices from database

  similarity_df =
    cytek_data %>%
    filter(
      row.names(
        cytek_data) %in% panel) %>%
    select(all_of(panel))

  similarity_matrix = as.matrix(                #making it into a matrix
      similarity_df
  )


  #Making a symmetrical Matrix

  similarity_matrix[upper.tri(similarity_matrix)] =
    t(
      similarity_matrix)[upper.tri(similarity_matrix)]

  storage.mode(similarity_matrix) <- "numeric"

  #Converting to tidy_long for ggplot2

  similarity_long = as.data.frame(
    as.table(
      similarity_matrix
      )
    )

  #generating the plot

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
