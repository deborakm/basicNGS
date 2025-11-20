#' Executa Analise de Componentes Principais (PCA)
#'
#' Esta funcao realiza uma PCA utilizando `prcomp`, com opcoes para
#' centralizacao e escalonamento das variaveis.
#'
#' @param data Um data.frame ou tibble contendo apenas colunas numericas.
#' @param scale Logical. Se TRUE, padroniza as variaveis (desvio padrao = 1).
#' @param center Logical. Se TRUE, centraliza as variaveis na media.
#' @param remove_na Logical. Se TRUE, remove linhas contendo NA antes da PCA.
#'
#' @return Um objeto da classe `prcomp` contendo os resultados da PCA.
#' @export

basicPCA <- function(data, scale = TRUE, center = TRUE) {

  # Verifica a classe do objeto data
  if (!is.data.frame(data)) {
    stop("O objeto 'data' deve ser um data.frame ou tibble.")
  }

  # Verifica se todas as colunas são numericas
  if (!all(sapply(data, is.numeric))) {
    stop("Todas as colunas de 'data' devem ser numericas.")
  }

  # Remover NAs
  if (remove_na) {
    n_before <- nrow(data) #conta quantas linhas tem antes de remover NAs
    data <- na.omit(data)
    n_after <- nrow(data) #conta quantas linhas tem depois de remover NAs

    if (n_after < n_before) {
      message("Foram removidas ", n_before - n_after, " linhas contendo NA.")
    }

    if (n_after == 0) {
      stop("Apos remover NAs, não sobrou nenhuma linha para analise.")
    }
  }

  # Rodar PCA
  pca_result <- prcomp(data, scale. = scale, center = center)

  return(pca_result)
}


