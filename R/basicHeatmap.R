#' Heatmap genérico com ComplexHeatmap
#'
#' @param data Data frame ou tibble contendo apenas valores numericos.
#' @param groups Vetor opcional com grupo de cada coluna (ex.: condicao, tratamento).
#' @param annotations Lista opcional de vetores para anotacoes adicionais (ex.: Age, Sex).
#' @param annotation_colors Lista de listas com cores para cada anotacao.
#' @param palette Paleta viridis usada para o heatmap (ex.: "magma", "viridis").
#' @param scale_data Se TRUE, escala cada linha (z-score).
#' @param cluster_rows Se TRUE, clusteriza linhas.
#' @param cluster_columns Se TRUE, clusteriza colunas.
#' @param title Titulo do heatmap.
#' @param output Arquivo PNG opcional para salvar o grafico.
#'
#' @return Um objeto Heatmap.
#' @export
basicHeatmap <- function(
    data,
    groups = NULL,
    annotations = NULL,
    annotation_colors = NULL,
    palette = "magma",
    scale_data = FALSE,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    title = "",
    output = NULL
){

  # ----------------------------
  # 1. Verificacoes
  # ----------------------------
  if (!is.data.frame(data)) {
    stop("'data' deve ser um data frame ou tibble.")
  }
  if (!all(sapply(data, is.numeric))) {
    stop("Todas as colunas de 'data' devem ser numericas.")
  }

  mat <- as.matrix(data)

  # Escalar linhas (opcional)
  if (scale_data) {
    mat <- t(scale(t(mat)))
  }

  # ----------------------------
  # 2. Anotacao superior (groups)
  # ----------------------------
  top_ann <- NULL

  if (!is.null(groups)) {
    if (length(groups) != ncol(mat)) {
      stop("O vetor 'groups' deve ter o mesmo numero de elementos que as colunas.")
    }

    top_ann <- HeatmapAnnotation(
      Group = groups,
      col = list(Group = if (!is.null(annotation_colors$Group))
        annotation_colors$Group else NULL),
      annotation_name_gp = gpar(fontsize = 8),
      annotation_legend_param = list(
        title_gp = gpar(fontsize = 9),
        labels_gp = gpar(fontsize = 8)
      )
    )
  }

  # ----------------------------
  # 3. Outras anotacoes (Age, Sex…)
  # ----------------------------
  bottom_ann <- NULL

  if (!is.null(annotations)) {
    ann_df <- data.frame(annotations)

    bottom_ann <- HeatmapAnnotation(
      df = ann_df,
      col = annotation_colors,
      annotation_name_gp = gpar(fontsize = 8),
      border = FALSE
    )
  }

  # ----------------------------
  # 4. Escolha da paleta
  # ----------------------------
  pal_fun <- switch(
    palette,
    magma = viridis::magma(100, direction = -1),
    viridis = viridis::viridis(100),
    plasma = viridis::plasma(100),
    inferno = viridis::inferno(100),
    cividis = viridis::cividis(100),
    viridis::magma(100)   # default
  )

  # ----------------------------
  # 5. Criar Heatmap
  # ----------------------------
  ht <- Heatmap(
    mat,
    name = "Value",
    col = pal_fun,
    cluster_rows = cluster_rows,
    cluster_columns = cluster_columns,
    show_row_names = FALSE,
    show_column_names = TRUE,
    clustering_method_rows = "ward.D2",
    clustering_method_columns = "ward.D2",
    column_names_gp = gpar(fontsize = 9),
    top_annotation = top_ann,
    bottom_annotation = bottom_ann,
    heatmap_legend_param = list(
      title_gp = gpar(fontsize = 9),
      labels_gp = gpar(fontsize = 8)
    )
  )

  # ----------------------------
  # 6. Salvar (opcional)
  # ----------------------------
  if (!is.null(output)) {
    png(output, width = 8, height = 7, units = "in", res = 1200)
    draw(ht, heatmap_legend_side = "right", annotation_legend_side = "right")
    grid.text(title, x = unit(0.5, "npc"), y = unit(1, "npc") - unit(4, "mm"),
              gp = gpar(fontsize = 13, fontface = "bold"))
    dev.off()
  }

  return(ht)
}


