library("ggplot2")
library("plyr")

source("./adorned_with_cuts.R")


get_decade_totals_from_estc <- function(estc_data, decades) {
  yearly_titles_summary <- count(estc_data, "publication_decade")
  titles_per_decade_base <- yearly_titles_summary[which(yearly_titles_summary$publication_decade %in% decades),]
  return(titles_per_decade_base$freq)
}


get_data_decade_subset <- function(decade, dataset_estc_orig, dataset_estc_processed, dataset_ecco_dump) {
  dataset_estc_processed_subset <-
    subset(dataset_estc_processed, publication_year >= decade & publication_year <= decade + 9)
  dataset_estc_orig_subset <-
    dataset_estc_orig[which(dataset_estc_orig$control_number %in% dataset_estc_processed_subset$control_number),]
  dataset_ecco_dump_subset <- 
    subset(dataset_ecco_dump, substr(pubDate, 1, 4) >= decade & substr(pubDate, 1, 4) <= decade + 9)
  return(list(estc_processed = dataset_estc_processed_subset, estc_orig = dataset_estc_orig_subset, ecco = dataset_ecco_dump_subset))
}


get_subset_cuts <- function(subset_list) {
  adorned_with_cuts <- get_adorned_with_cuts(subset_list$estc_orig,
                                             subset_list$estc_processed,
                                             subset_list$ecco,
                                             low_year = 1470)  
  return(adorned_with_cuts)
}


get_ecco_hits_per_decade <- function(adorned_results, fields = c(1, 2, 3, 6)) {
  hits_ecco_decade <- sum(as.numeric(append(adorned_results$new_estc[fields], adorned_results$new_outside[fields])))
  return(hits_ecco_decade)
}


get_abs_plot <- function(hits_per_decade_df) {
  ggplot(hits_per_decade_df, aes(decade, hits)) +
    geom_bar(stat = "identity") +
    # ggtitle("Adorned with cuts in ESTC") +
    labs(x = "", y = "titles") +
    theme_grey() +
    theme(axis.text.x = element_text(size = 9, angle = -90),
          axis.title.x = element_blank())
}


get_rel_plot <- function(hits_per_decade_df) {
  ggplot(hits_per_decade_df, aes(decade, relative)) +
    geom_bar(stat = "identity") +
    # ggtitle("Adorned with cuts in ESTC") +
    labs(x = "", y = "frequency of titles") +
    theme_grey() +
    theme(axis.text.x = element_text(size = 9, angle = -90),
          axis.title.x = element_blank())
}


get_plotdata_df <- function(df.orig, df.preprocessed, df.ecco, decades, fields = c(1, 2, 3, 6)) {
  hits_per_decade <- vector()
  row_names <- list()
  
  i <- 1
  for (decade in decades) {
    adorned <- get_subset_cuts(get_data_decade_subset(decade, df.orig, df.preprocessed, df.ecco))
    hits <- get_ecco_hits_per_decade(adorned, fields)
    hits_per_decade[i] <- hits
    row_names <- append(row_names, paste0(decade, "-", (decade + 9)))
    i <- i + 1
  }
  
  hits_per_decade_df <- data.frame(decade = unlist(row_names), hits = hits_per_decade)
  hits_per_decade_df$all_titles_per_decade <- get_decade_totals_from_estc(df.preprocessed, decades)
  hits_per_decade_df$relative <- hits_per_decade_df$hits / hits_per_decade_df$all_titles_per_decade
  return(hits_per_decade_df)
}

df.orig <- readRDS("./estc_orig.Rds")
df.preprocessed <- readRDS("./estc_df.Rds")
df.ecco <- readRDS("./df.ecco.Rds")
# estc_titles_per_decade <- readRDS("./titles_per_decade_ESTC.Rds")
decades <- seq(1470, 1799, by = 10)
# adorned_with_cuts <- get_adorned_with_cuts(df.orig, df.preprocessed, df.ecco)
fields <- c(1, 2, 3)

hits_per_decade_df <- get_plotdata_df(df.orig, df.preprocessed, df.ecco, decades, fields)

abs_plot <- get_abs_plot(hits_per_decade_df)
rel_plot <- get_rel_plot(hits_per_decade_df)
abs_plot
rel_plot

