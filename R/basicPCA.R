#' Executa Análise de Componentes Principais (PCA)
#'
#' Esta função realiza uma PCA utilizando `prcomp`, com opções para
#' centralização e escalonamento das variáveis.
#'
#' @param dados Um data.frame ou tibble contendo apenas colunas numéricas.
#' @param scale Logical. Se TRUE, padroniza as variáveis (desvio padrão = 1).
#' @param center Logical. Se TRUE, centraliza as variáveis na média.
#' @param remove_na Logical. Se TRUE, remove linhas contendo NA antes da PCA.
#'
#' @return Um objeto da classe `prcomp` contendo os resultados da PCA.
#' @export

basicPCA <- function(dados, scale = TRUE, center = TRUE) {

  # Verifica a classe do objeto dados
  if (!is.data.frame(dados)) {
    stop("O objeto 'dados' deve ser um data.frame ou tibble.")
  }

  # Verifica se todas as colunas são numéricas
  if (!all(sapply(dados, is.numeric))) {
    stop("Todas as colunas de 'dados' devem ser numéricas.")
  }

  # Remover NAs
  if (remove_na) {
    n_before <- nrow(dados) #conta quantas linhas tem antes de remover NAs
    dados <- na.omit(dados)
    n_after <- nrow(dados) #conta quantas linhas tem depois de remover NAs

    if (n_after < n_before) {
      message("Foram removidas ", n_before - n_after, " linhas contendo NA.")
    }

    if (n_after == 0) {
      stop("Após remover NAs, não sobrou nenhuma linha para análise.")
    }
  }

  # Rodar PCA
  pca_result <- prcomp(dados, scale. = scale, center = center)

  return(pca_result)
}


