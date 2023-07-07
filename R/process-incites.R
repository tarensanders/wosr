process_incites <- function(incites_df, as_raw = FALSE, version = 1) {
  if (!is.data.frame(incites_df)) {
    return(NULL)
  }
  if (as_raw) {
    return(incites_df)
  }
  colnames(incites_df) <- tolower(colnames(incites_df))

  if (version == 1) {
    cols_v1 <- list(
      "string" = c("isi_loc", "article_type"),
      "numeric" = c(
        "tot_cites", "journal_expected_citations", "journal_act_exp_citations",
        "impact_factor", "avg_expected_rate", "percentile", "nci"
      ),
      "binary" = c(
        "esi_most_cited_article", "hot_paper", "is_international_collab",
        "is_institution_collab", "is_industry_collab", "oa_flag"
      )
    )
    cols_list <- cols_v1
    cols <- Reduce(c, cols_v1)
  }

  if (version == 2) {
    # Unpack the OPEN_ACCESS column
    incites_df[, "open_access"] <- incites_df$open_access$OA_FLAG
    cols_v2 <- list(
      "string" = c("accession_number", "document_type"),
      "numeric" = c(
        "times_cited", "impact_factor", "journal_expected_citations", "jnci",
        "harmean_cat_exp_citation", "avg_cnci"
      ),
      "binary" = c(
        "is_international_collab", "open_access", "is_industry_collab",
        "is_institution_collab", "esi_highly_cited_paper", "esi_hot_paper"
      )
    )

    if ("percentile" %in% colnames(incites_df)) {
      cols_v2$string <- c(cols_v2$string, "percentile")
    }
    cols_list <- cols_v2
    cols <- Reduce(c, cols_v2)
  }

  bad_cols <- cols[!cols %in% colnames(incites_df)]
  if (length(bad_cols) != 0) {
    stop(
      "API isn't serving these columns anymore: ",
      paste0(bad_cols, collapse = ", ")
    )
  }

  incites_df <- incites_df[, cols]
  colnames(incites_df)[1] <- "ut"
  incites_df$ut <- paste0("WOS:", incites_df$ut)
  incites_df[, c(cols_list$numeric, cols_list$binary)] <-
    apply(incites_df[, c(cols_list$numeric, cols_list$binary)],
      MARGIN = 2, FUN = as.numeric
    )
  incites_df[, cols_list$binary] <- apply(
    incites_df[, cols_list$binary],
    MARGIN = 2, FUN = function(x) x == 1
  )
  incites_df
}
