#' @title 36an oinarritutako zenbakikuntzatik 10erakora
#' @description Funtzinoiak 36an oinarritutako  \code{x} zenbakia sistema hamartarrera dakar.
#' @param x Karaktere motakoa. 36an oinarritutako zenbakia (zenbaki eta letraz osatua izan liteke)
#' @details Funtzino hau behar da \code{f_idat} funtzino barruan.
#' @examples
#' f_36_10era('11')
#' f_36_10era('ab2')
#' @export
# barruko erabilerarako
# 36an oinarritutako sistematik gure sistema hamartarrera ekarteko
f_36_10era <- function(x) strtoi(x, base = 36)
