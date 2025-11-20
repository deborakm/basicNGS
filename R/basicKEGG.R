#' basicKEGG: KEGG enrichment analysis using gprofiler2
#'
#' @description
#' Performs KEGG pathway enrichment using gprofiler2::gost(),
#' computes gene ratio, filters results, generates a dotplot (optional),
#' and returns the ggplot object.
#'
#' @param genes Character vector of gene symbols.
#' @param organism Organism code (default = "hsapiens").
#' @param sources pathways source to query. Default is "KEGG".
#' @param pval_cutoff P-value cutoff (default = 0.05).
#' @param top Number of KEGG terms to keep (default = 50).
#' @param save_plot Logical; save PNG plot? (default = FALSE).
#' @param plot_path Path to save PNG plot.
#' @param save_xlsx Logical; save results table as XLSX? (default = FALSE).
#' @param xlsx_path Path for XLSX file.
#'
#' @return A ggplot object representing the KEGG dotplot.
#' @export
#'
basicKEGG <- function(
    genes,
    organism = "hsapiens",
    sources = 'KEGG',
    pval_cutoff = 0.05,
    top = 50,
    save_plot = FALSE,
    plot_path = "KEGG_dotplot.png",
    save_xlsx = FALSE,
    xlsx_path = "KEGG_results.xlsx"
) {

  # --- Checks ------------------------------------------------------------------
  if (missing(genes)) stop("Voce deve fornecer um vetor de genes.")

  if (!is.vector(genes) || !is.character(genes)) {
    stop("'genes' deve ser um vetor de caracteres.")
  }

  genes <- unique(na.omit(genes))

  if (length(genes) < 5) {
    stop("Forneca pelo menos 5 genes para uma análise robusta.")
  }

  # --- Enrichment ---------------------------------------------------------------
  gost_results <- gprofiler2::gost(
    query = genes,
    organism = organism,
    sources = "KEGG",
    evcodes = TRUE
  )

  if (is.null(gost_results)) {
    stop("Nenhum resultado encontrado para KEGG.")
  }

  df <- gost_results$result

  # --- Filter -------------------------------------------------------------------
  df <- df |>
    dplyr::filter(p_value < pval_cutoff) |>
    dplyr::arrange(p_value) |>
    dplyr::slice_head(n = top) |>
    dplyr::mutate(
      gene_ratio = intersection_size / query_size
    )

  # --- Build plot ----------------------------------------------------------------
  plot_KEGG <- ggplot2::ggplot(df, ggplot2::aes(
    x = reorder(term_name, gene_ratio),
    y = gene_ratio,
    color = p_value,
    size = intersection_size
  )) +
    ggplot2::geom_point() +
    viridis::scale_color_viridis(option = "magma") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "",
      y = "Gene ratio",
      color = "p-value",
      size = "Intersection size",
      title = "KEGG Pathway Enrichment"
    ) +
    ggplot2::theme_minimal()

  # --- Optional: Save PNG --------------------------------------------------------
  if (save_plot) {
    grDevices::png(plot_path, width = 8, height = 8, units = "in", res = 1200)
    print(plot_KEGG)
    grDevices::dev.off()
  }

  # --- Optional: Save XLSX -------------------------------------------------------
  if (save_xlsx) {
    df_clean <- df |>
      dplyr::select(-dplyr::where(is.list))

    openxlsx::write.xlsx(df_clean, xlsx_path)
  }

  # --- Return only the plot ------------------------------------------------------
  return(plot_KEGG)
}
