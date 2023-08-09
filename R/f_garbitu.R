#' Analisietan oztopo egin ohi duen informazio lerroak ezabatzeko zenbait aukera
#'
#' @param objektua IDAT erabilita sortutako zerrenda edo data framea
#' @param copypaste Luzera batetik gorako `revs` ezabatzen ditu. Luzera `n`k zehazten du
#' @param n Ezabatuko den informazioaren gutxieneko karaktere kopurua. Balio lehenetsia `7` da
#' @param hutsa Identifikatu gabeko `author`ik agertzen ez diren lerroak ezabatzen ditu `TRUE`k.
#' Hori zerbitzarian aurretik egon ohi den informazioa izaten da.
#' @param ni Paketearen egileak erabili ohi duen "author" ezabatzen du
#' @param nirea Analisia egiten ari denaren edo beste baten erabiltzaileak idatzitakoa
#' ezabatzen du. Balio lehenetsia paketearen egilearen `author` balioa da
#'
#' @return Etherpaden idatzitako dokumentu baten \code{objektua}ari kentzen dizkio
#' informazio nahasiko lerroak, \code{copypaste} egindakoa delako, zein `author` identifikazio
#' \code{hutsa} edo \code{ni} nazelako
#'
#'
#' @examples
#' require(dplyr)
#' # o_idat <- f_IDAT(kokapena, fitxategia)
#'
#' f_garbitu(o_idat)$df_aldaketak
#'
#' @export

f_garbitu <- function(objektua,
                      copypaste = FALSE, n = 7,
                      hutsa = TRUE,
                      ni = TRUE, nirea = "a.WqkJLdiHlrEQ40Fl") {
  if (is.data.frame(objektua)) {
    df <- objektua
  } else if (is.list(objektua)) {
    df <- objektua$df_aldaketak
  }
  if (copypaste == TRUE) {
    df <- df |>
      filter(bank_luzera <= n)
  }
  if (hutsa == TRUE) {
    df <- df |>
      filter(author != "")
  }
  if (ni == TRUE) {
    df <- df |>
      filter(author != nirea)
  }
  if (is.data.frame(objektua)) {
    objektua <- df
    return(objektua)
  }
  if (is.list(objektua)) {
    objektua$df_aldaketak <- df
    return(objektua)
  }
}
