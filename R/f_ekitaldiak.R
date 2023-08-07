#' Idazteko orduan egin diren ekitaldiak identifikatzen ditu
#'
#' @param objektua IDAT erabilita sortutako zerrenda edo data framea
#' @param muga Ekitaldi bakoitza zehazteko kontuan izateko gehienezko muga, milisegunduetan
#'
#' @return Etherpaden idatzitako dokumentu baten \code{objektua}aren \code{muga} baten araberako ekitaldiak
#'
#' @importFrom forcats fct_lump_min fct_na_level_to_value
#'
#' @examples
#' require(dplyr)
#' # o_idat <- f_IDAT(fitxategia, kokapena)
#' f_ekitaldiak(o_idat)$df_aldaketak |> select(ekitaldia)
#'
#' @export

f_ekitaldiak <- function(objektua, muga = 1000){
    message('\nArgitzekoa:\nPausu bitik beherakoak ez dira hartzen "ekitalditzat". \nEkitaldirik laburrenak, beraz, 3 pausutik gorakoak dira.\n')
    if(is.data.frame(objektua)){
        df <- objektua
    } else if(is.list(objektua)){
        df <- objektua$df_aldaketak
    }

    df <- df |>
        group_by(author) |>
        arrange(author, revs)
    burst <- 1
    ekitaldia <- numeric(length = 0L)
    zalantza <- df$author[1]
    for(i in 1:nrow(df)){
        if(zalantza != df$author[i]){
            zalantza <- df$author[i]
            burst <- burst + 1
            next
        } # Kasun baten batu di fitxategi asko landuta
        if(is.na(df$aldi[i])) next
        if(df$aldi[i]<muga){
            ekitaldia[i] <- burst
            zalantza <- df$author[i]
        }else{
            burst <- burst + 1
            ekitaldia[i] <- burst
            zalantza <- df$author[i]
        }
    }

    df <- df |>
        ungroup() |>
        bind_cols(tibble(ekitaldia = factor(ekitaldia))) |>
        mutate(ekitaldia = fct_lump_min(ekitaldia,
                                        min = 2,
                                        other_level = 'kendu'),
               ekitaldia = fct_na_level_to_value(ekitaldia, 'kendu')) |>
        arrange(pad, revs)

    if(is.data.frame(objektua)){
        objektua <- df
    } else if(is.list(objektua)){
        objektua$df_aldaketak <- df
    }

    return(objektua)
}
