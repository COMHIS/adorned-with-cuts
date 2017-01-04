
library("reshape2")

get_adorned_with_cuts <- function(df.orig, df.preprocessed, df.ecco, ignore_terms=NULL, low_year = 1700) {
  
  
  # Remove anything before closing parentheses
  estcid_in_estc <- gsub("^.*[)](.*)", "\\1", df.orig$system_control_number)
  
  # Remove zeros after the letter
  estcid_in_ecco <- df.ecco$ESTCID
  estcid_in_ecco <- gsub("([A-Z])([0]+)(.*)", "\\1\\3", estcid_in_ecco)
  
  ecco_ins <- which(estcid_in_ecco %in% estcid_in_estc)
  
  # Get two corpora, one to test against ECCO, one to have those which ECCO doesn't contain
  ins <- (which(estcid_in_estc %in% estcid_in_ecco))
  df.estc_in_ecco <- df.orig[ins,]
  # Limit to 1700-1800
  not_in_ecco_indices <- setdiff(1:nrow(df.orig),ins)
  century18th_indices <- which((df.preprocessed$publication_year_from >= low_year & df.preprocessed$publication_year_from <= 1799))
  # Check also the old ones
  old_indices <- which(df.preprocessed$publication_year_from < low_year)
  df.old_estc <- df.orig[old_indices,]
  #df.estc_not_in_ecco <- df.orig[intersect(not_in_ecco_indices, century18th_indices),]
  df.outside <- df.orig[intersect(not_in_ecco_indices, century18th_indices),]
  # Filter the df.preprocessed too
  df.preprocessed_in_ecco <- df.preprocessed[ins,]
  df.preprocessed.outside <- df.preprocessed[intersect(not_in_ecco_indices, century18th_indices),]
  df.preprocessed.old <- df.preprocessed[old_indices,]
  
  # Wipe out ECCO rows that don't have a match in ESTC
  df.ecco_in_estc <- df.ecco[ecco_ins,]
  
  # Get title, subtitle and edition statement into one variable to ease up the queries
  title_only_in_ecco <- paste0(df.estc_in_ecco$title, " ::: ", df.estc_in_ecco$title_remainder)
  title_only_outside <- paste0(df.outside$title, " ::: ", df.outside$title_remainder)
  title_only_old <- paste0(df.old_estc$title, "::: ", df.old_estc$title_remainder)
  
  title_edition_in_ecco <- paste0(df.estc_in_ecco$title, " ::: ", df.estc_in_ecco$title_remainder, ". ", df.estc_in_ecco$edition_statement)
  title_edition_outside <- paste0(df.outside$title, " ::: ", df.outside$title_remainder, ". ", df.outside$edition_statement)
  title_edition_old <- paste0(df.old_estc$title, "::: ", df.old_estc$title_remainder, ". ", df.old_estc$edition_statement)
  
  vols_in_ecco <- df.preprocessed_in_ecco$volcount
  vols_in_ecco[which(is.na(vols_in_ecco))] <- 1
  vols_outside <- df.preprocessed.outside$volcount
  vols_outside[which(is.na(vols_outside))] <- 1
  vols_old <- df.preprocessed.old$volcount
  vols_old[which(is.na(vols_old))] <- 1
  
  cuts_ecco <- rep(FALSE, nrow(df.ecco_in_estc))
  cuts_estc <- rep(FALSE, nrow(df.estc_in_ecco))
  cuts_outside <- rep(FALSE, nrow(df.outside))
  cuts_old <- rep(FALSE, nrow(df.old_estc))
  
  ignore_ecco <- rep(FALSE, nrow(df.ecco_in_estc))
  ignore_estc <- rep(FALSE, nrow(df.estc_in_ecco))
  ignore_outside <- rep(FALSE, nrow(df.outside))
  ignore_old <- rep(FALSE, nrow(df.old_estc))
  
  # These figures do NOT include ECCO Part2 !!!
  search_terms <- c("with cuts", " adorn['e]d with", "with ([^ ]+ )+cuts", "emblem",
                    "with engrav")
  
  for (ignore_term in ignore_terms) {
    ignore_ecco[grep(ignore_term, df.ecco_in_estc$fullTitle)] <- TRUE
    ignore_estc[grep(ignore_term, title_edition_in_ecco)] <- TRUE
    ignore_outside[grep(ignore_term, title_edition_outside)] <- TRUE
    ignore_old[grep(ignore_term, title_edition_old)] <- TRUE
  }
  
  i <- length(search_terms)
  
  compiled_results <- data.frame(searchterm=character(i), 
                                 ecco=integer(i), 
                                 ecco_vol=integer(i),
                                 estc=integer(i), 
                                 estc_vol=integer(i), 
                                 outside=integer(i), 
                                 outside_vol=integer(i),
                                 old=integer(i), 
                                 old_vol=integer(i),
                                 new_ecco=integer(i), 
                                 new_estc=integer(i), 
                                 new_outside=integer(i),
                                 new_old=integer(i),
                                 title_only_in_ecco=integer(i),
                                 title_only_not_in_ecco=integer(i),
                                 title_only_old=integer(i),
                                 stringsAsFactors = FALSE)
  
  for (j in 1:length(search_terms)) {
    searchterm <- search_terms[j]
    
    ret_ecco <- grep(searchterm, df.ecco_in_estc$fullTitle, ignore.case = TRUE)
    ret_estc_in_ecco <- grep(searchterm, title_edition_in_ecco, ignore.case = TRUE)
    ret_outside <- grep(searchterm, title_edition_outside, ignore.case = TRUE)
    ret_old <- grep(searchterm, title_edition_old, ignore.case = TRUE)
    
    ret_ecco <- intersect(which(ignore_ecco==FALSE), ret_ecco)
    ret_estc_in_ecco <- intersect(which(ignore_estc==FALSE), ret_estc_in_ecco)
    ret_outside <- intersect(which(ignore_estc==FALSE), ret_outside)
    ret_old <- intersect(which(ignore_old==FALSE), ret_old)
    
    ret_title_estc_in_ecco <- grep(searchterm, title_only_in_ecco, ignore.case = TRUE)
    ret_title_outside <- grep(searchterm, title_only_outside, ignore.case = TRUE)
    ret_title_old <- grep(searchterm, title_only_old, ignore.case = TRUE)
    
    new_rets_ecco <- intersect(which(cuts_ecco==FALSE), ret_ecco)
    new_rets_estc <- intersect(which(cuts_estc==FALSE), ret_estc_in_ecco)
    new_rets_outside <- intersect(which(cuts_outside==FALSE), ret_outside)
    new_rets_old <- intersect(which(cuts_old==FALSE), ret_old)
    
    cuts_ecco[ret_ecco] <- TRUE
    cuts_estc[ret_estc_in_ecco] <- TRUE
    cuts_old[ret_old] <- TRUE
    
    unique_ecco <- length(unique(df.ecco_in_estc$ESTCID[ret_ecco]))
    ret_vols_in_ecco <- sum(vols_in_ecco[ret_estc_in_ecco], na.rm=FALSE)
    ret_vols_outside <- sum(vols_outside[ret_outside], na.rm=FALSE)
    ret_vols_old <- sum(vols_old[ret_old], na.rm=FALSE)
    
    compiled_results[j,] <- c(searchterm, 
                              unique_ecco, 
                              length(ret_ecco), 
                              length(ret_estc_in_ecco),
                              ret_vols_in_ecco, 
                              length(ret_outside),
                              ret_vols_outside,
                              length(ret_old),
                              ret_vols_old,
                              length(new_rets_ecco),
                              length(new_rets_estc),
                              length(new_rets_outside),
                              length(new_rets_old),
                              length(ret_title_estc_in_ecco),
                              length(ret_title_outside),
                              length(ret_title_old))
  }
  
  more_estc <- grep("plate", df.estc_in_ecco$physical_extent, ignore.case = TRUE)
  new_more_estc <- intersect(which(cuts_estc==FALSE), more_estc)
  cuts_estc[more_estc] <- TRUE
  vol_more_estc <- sum(vols_in_ecco[more_estc], na.rm=FALSE)
  
  more_outside <- grep("plate", df.outside$physical_extent, ignore.case = TRUE)
  new_more_outside <- intersect(which(cuts_outside==FALSE), more_outside)
  cuts_outside[more_outside] <- TRUE
  vol_more_outside <- sum(vols_outside[more_outside], na.rm=FALSE)
  
  more_old <- grep("plate", df.old_estc$physical_extent, ignore.case = TRUE)
  new_more_old <- intersect(which(cuts_old==FALSE), more_old)
  cuts_old[more_old] <- TRUE
  vol_more_old <- sum(vols_old[more_old], na.rm=FALSE)
  
  compiled_results2 <- data.frame("  PLATE  ",
                                  NA,
                                  NA,
                                  length(more_estc), 
                                  vol_more_estc,
                                  length(more_outside),
                                  vol_more_outside, 
                                  length(more_old),
                                  vol_more_old, 
                                  NA,
                                  length(new_more_estc),
                                  length(new_more_outside),
                                  length(new_more_old),
                                  NA,
                                  NA,
                                  NA
  )
  
  names(compiled_results2) <- names(compiled_results)
  compiled_results <- rbind.data.frame(compiled_results, compiled_results2, deparse.level = 0)
  
  more_estc <- grep("table", df.estc_in_ecco$physical_extent, ignore.case = TRUE)
  new_more_estc <- intersect(which(cuts_estc==FALSE), more_estc)
  cuts_estc[more_estc] <- TRUE
  vol_more_estc <- sum(vols_in_ecco[more_estc], na.rm=FALSE)
  
  more_outside <- grep("table", df.outside$physical_extent, ignore.case = TRUE)
  new_more_outside <- intersect(which(cuts_outside==FALSE), more_outside)
  cuts_outside[more_outside] <- TRUE
  vol_more_outside <- sum(vols_outside[more_outside], na.rm=FALSE)
  
  more_old <- grep("plate", df.old_estc$physical_extent, ignore.case = TRUE)
  new_more_old <- intersect(which(cuts_old==FALSE), more_old)
  cuts_old[more_old] <- TRUE
  vol_more_old <- sum(vols_old[more_old], na.rm=FALSE)
  
  compiled_results2 <- data.frame("  TABLE  ",
                                  NA,
                                  NA,
                                  length(more_estc), 
                                  vol_more_estc,
                                  length(more_outside),
                                  vol_more_outside, 
                                  length(more_old),
                                  vol_more_old, 
                                  NA,
                                  length(new_more_estc),
                                  length(new_more_outside),
                                  length(new_more_old),
                                  NA,
                                  NA,
                                  NA
  )
  
  
  
  names(compiled_results2) <- names(compiled_results)
  compiled_results <- rbind.data.frame(compiled_results, compiled_results2, deparse.level = 0)
  
  
  return (compiled_results)
  
} 

get_adorned_indices <- function(df.orig, include_plates=TRUE) {

  title_edition <- paste0(df.orig$title, " ::: ", df.orig$title_remainder, ". " , df.orig$edition_statement)
  ret_inds <- rep(FALSE, nrow(df.orig))

  searchterms <- c("with cuts", " adorned with", "with ([^ ]+ )+cuts", "emblem", "with engrav")

  for (searchterm in searchterms) {
    ret_inds[grep(searchterm, title_edition, ignore.case = TRUE)] <- TRUE
  }


  if (include_plates) {
    for (searchterm in c("plates", "tables")) {
      ret_inds[grep(searchterm, df.orig$physical_extent, ignore.case = TRUE)] <- TRUE
    }
  }

  return(which(ret_inds))
}

# adorned_indices <- get_adorned_indices(df.orig, include_plates = TRUE)
# adorned_indices_without_plates <- get_adorned_indices(df.orig, include_plates = FALSE)
# pubs <- df.preprocessed$publisher[adorned_indices]
# pubs_without_plates <- df.preprocessed$publisher[adorned_indices_without_plates]
# write_xtable(pubs, "adorned_publishers_with_plates.csv")
# write_xtable(pubs_without_plates, "adorned_publishers_without_plates.csv")
# 
# all_stats <- get_adorned_with_cuts(df.orig=df.orig,
#                                    df.preprocessed=df.preprocessed,
#                                    df.ecco=df.ecco)
# 
# all_stats_sans_copper <- get_adorned_with_cuts(df.orig=df.orig,
#                                                df.preprocessed=df.preprocessed,
#                                                df.ecco=df.ecco)
# 
# names.bu <- names(all_stats)
# all_stats$ESTC_TOTAL <- unname(unlist(transform(rowSums(
#   cbind(as.integer(all_stats$new_estc),as.integer(all_stats$new_outside))))))
# 
# all_stats_sans_copper$ESTC_TOTAL <- unname(unlist(transform(rowSums(
#   cbind(as.integer(all_stats_sans_copper$new_estc),as.integer(all_stats_sans_copper$new_outside))))))
# 
# 
# names(all_stats) <- c("Search term", "Unique ECCO", "Volumes in ECCO",
#                       "Unique ESTC in ECCO", "Volumes in ESTC", "ESTC not in ECCO", "Volumes not in ECCO",
#                       "ESTC before 1700", "ESTC volumes before 1700",
#                       "ECCO", "ESTC in ECCO", "ESTC not in ECCO", "ESTC before 1700",
#                       "ESTC in ECCO (from title fields)", "ESTC not in ECCO (from title fields)", "ESTC before 1700 (from title fields)",
#                       "ESTC 18th century")
# names(all_stats_sans_copper) <- names(all_stats)
# 
# 
# 
# pic1 <- get_stack_plot(all_stats[1:5,c("Search term", "ESTC before 1700", "ESTC 18th century")],
#                        id.vars="Search term",
#                        variable.name="corpus", value.name="count")
# pic1_without_copper <- get_stack_plot(all_stats_sans_copper[1:5,c("Search term", "ESTC before 1700", "ESTC 18th century")],
#                                       id.vars="Search term",
#                                       variable.name="corpus", value.name="count")
# pic2 <- get_stack_plot(all_stats[1:7,c("Search term", "ESTC before 1700", "ESTC 18th century")],
#                        id.vars="Search term",
#                        variable.name="corpus", value.name="count")
# pic2_without_copper <- get_stack_plot(all_stats_sans_copper[1:7,c("Search term", "ESTC before 1700", "ESTC 18th century")],
#                                       id.vars="Search term",
#                                       variable.name="corpus", value.name="count")
# pic3 <- get_stack_plot(all_stats[1:5, c("Search term", "ESTC in ECCO", "ESTC not in ECCO")],
#                        id.vars="Search term",
#                        variable.name="corpus", value.name="count")
# pic4 <- get_stack_plot(all_stats[1:5, c("Search term", "ESTC in ECCO (from title fields)", "ESTC not in ECCO (from title fields)")],
#                        id.vars="Search term",
#                        variable.name="corpus", value.name="count")



get_stack_plot <- function(stats, id.vars, variable.name, value.name) {
  
  pic <- prepare_df_for_plotting(stats,
                                 id.vars=id.vars,
                                 variable.name=variable.name, 
                                 value.name=value.name)
  lev <- unique(pic[[1]])
  pic[[1]] <- factor(x=pic[[1]], levels = lev)
  #return (pic)
  ret_plot <-  (plot_adorned_with_cuts_stack(pic))
  ret_plot
}


prepare_df_for_plotting <- function(df.temp, id.vars, variable.name, value.name) {
  ret_df <- melt(data=df.temp, id.vars=id.vars, variable.name=variable.name, value.name=value.name)
  ret_df$count <- as.integer(ret_df$count)
  ret_df
}


plot_adorned_with_cuts_dodge <- function(with_cuts) {
  p <- ggplot(with_cuts, aes(x=with_cuts[,names(with_cuts[1])], 
                             y=with_cuts[,names(with_cuts[3])], 
                             fill=with_cuts[,names(with_cuts[2])]))
  p <- p + geom_bar(stat="identity", position= position_dodge(), )
  #p <- p + coord_flip()
  #v <- 10 ^ (1:max(na.omit(round(log10(max(with_cuts$count))))))
  #v <- max(with_cuts$count)
  #p <- p + scale_y_discrete(breaks = v, labels = v)
  
  p <- p + xlab("Term occurrences (n)") + ylab("")
  p <- p + ggtitle("Total hits")
  return(p)
}

plot_adorned_with_cuts_stack <- function(with_cuts) {
  with_cuts <- with_cuts[order(with_cuts[[1]], decreasing = FALSE),]
  
  p <- ggplot(with_cuts[order(with_cuts[[1]], decreasing = FALSE),], 
              aes(x=with_cuts[,names(with_cuts[2])], 
                  y=with_cuts[,names(with_cuts[3])], 
                  fill=factor(with_cuts[,names(with_cuts[1])], levels=rev(levels(with_cuts[[1]])))))
  p <- p + geom_bar(stat="identity", position= position_stack(), )
  #p <- p + coord_flip()
  #v <- 10 ^ (1:max(na.omit(round(log10(max(with_cuts$count))))))
  #v <- max(with_cuts$count)
  #p <- p + scale_y_discrete(breaks = v, labels = v)
  p <- p + theme(legend.title = element_blank())
  p <- p + xlab("Term occurrences (n)") + ylab("")
  p <- p + ggtitle("Total hits")
  return(p)
}