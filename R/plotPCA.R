#' Plota o grafico de PCA (PC1 x PC2)
#'
#' @param pca_result Objeto resultante de prcomp().
#' @param grupos Um vetor com fatores de agrupamento (ex.: condicoes).
#'               Deve ter o mesmo numero de linhas que pca_result$x.
#' @param rotulos Vetor com nomes/samples para mostrar no grafico.
#'
#' @return Um grafico ggplot2.
#' @export


plotPCA <- function(pca_result, grupos = NULL, rotulos = NULL) {

  # ------------------------------------------
  # 1. Criar dataframe com as coordenadas da PCA
  # ------------------------------------------
  pca_df <- as.data.frame(pca_result$x[,1:2])
  colnames(pca_df) <- c("PC1", "PC2")

  # Se foram passados grupos, acrescentar
  if (!is.null(grupos)) {
    if (length(grupos) != nrow(pca_df)) {
      stop("O vetor 'grupos' deve ter o mesmo número de elementos que as amostras.")
    }
    pca_df$Grupo <- as.factor(grupos)
  }

  # Se foram passados rotulos, acrescentar
  if (!is.null(rotulos)) {
    if (length(rotulos) != nrow(pca_df)) {
      stop("O vetor 'rotulos' deve ter o mesmo número de elementos que as amostras.")
    }
    pca_df$Sample <- rotulos
  } else {
    pca_df$Sample <- rownames(pca_df)
  }

  # ------------------------------------------
  # 2. Variancia explicada
  # ------------------------------------------
  autovalores <- pca_result$sdev^2
  porcentagem <- autovalores / sum(autovalores) * 100

  rotulo_x <- paste0("PC1 (", round(porcentagem[1], 1), "%)")
  rotulo_y <- paste0("PC2 (", round(porcentagem[2], 1), "%)")

  # ------------------------------------------
  # 3. Plot
  # ------------------------------------------
  g <- ggplot2::ggplot(
    pca_df,
    ggplot2::aes(
      x = PC1,
      y = PC2,
      label = Sample,
      color = if (!is.null(grupos)) Grupo else NULL
    )
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(vjust = -0.8, show.legend = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Principal Component Analysis",
      x = rotulo_x,
      y = rotulo_y,
      color = NULL
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.text = ggplot2::element_text(size = 10)
    )

  return(g)
}


