#' @title EPOC formatutik POSIXera ekarri
#' @description Funtzinoiak zerbitzari aldeko EPOC formatuko datua \code{x}
#' eredu irakurgarrira dakar
#' @param x Zenbakizkoa motakoa. 1970-01-01tik ona igarotako denbora, milisegunduetan eta
#' @details Funtzino hau behar da \code{f_idat} funtzino barruan.
#' @examples
#' f_epoc_ekarri(1600352447019)
#' @export
# Barneko erabilerarako
# epoc formatuko datak gure erara ekarteko
# [zati 1000 be egin bihar da, baina ez dakit zergaitik]
f_epoc_ekarri <- function(x){
    as.POSIXct(x/1000, origin="1970-01-01")
}
