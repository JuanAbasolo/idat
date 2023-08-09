#' Idazteko orduan egin diren ekitaldiak identifikatzen ditu
#'
#' @param objektua IDAT erabilita sortutako zerrenda edo data framea
#' @param egileak karaktere bektore bat, authorrek dituen mailen luzerakoa. Egileak
#' author mailak baino gutxiago badira, izenak errepika daitekez, mailak egokitzeko.
#' @param authorka TRUE balioak mailak author horren arabera kalkulatzen ditu.
#' @param nameka TRUE balioak mailak name horren arabera kalkulatzen ditu.
#' @param anonimu TRUE balioak author edo name-ko balioak E-n zenbakiz ordezkatzen ditu, n zenbaki bat dela.
#'
#' @return IDAT objektu batean egileak batzeko edo anonimatzeko bideak
#' \code{objektua}aren \code{egileak} eman dakizkio, edo \code{authorka} antolatu,
#' zein \code{nameka}. Azken aukera bietan anonimo egin daitekez \code{anonimu} aldagaian TRUE emanda.

#' @importFrom dplyr left_join
#' @importFrom tidyr unnest_wider pivot_wider
#' @importFrom forcats fct_anon
#'
#' @examples
#' require(dplyr)
#' # o_idat <- f_IDAT(kokapena, fitxategia)
#' f_IDentifikau(o_idat, anonimu = TRUE)$df_aldaketak |>
#'   select(egileak) |>
#'   head(10)
#'
#' @export

f_IDentifikau <- function(objektua,
                          egileak = NULL,
                          authorka = TRUE,
                          nameka = FALSE,
                          anonimu = FALSE) {
  # Ea objektua zelakoa dan
  if (is.data.frame(objektua)) {
    df <- objektua
  } else if (is.list(objektua)) {
    df <- objektua$df_aldaketak
    # df_egileak <- objektua$df_egileak
  }


  ## Ixenak emonda
  if (exists(x = "egileak", mode = "character")) {
    message('\nOharra:\tEgileak ematen zaizkionean oinarritzat "author" hartzen da\n"author" bakoitzari egile bat esleitzen zaio\n')
    if (length(egileak) != length(levels(factor(df$author)))) {
      stop(paste(
        "\nERROREA:", length(egileak), "izen emanda",
        length(levels(factor(df$author))), "ordezkatzeko.\n EGOKITU\n"
      ))
    }
    ## Informazinu, ordezkuk zelan hartzen dituzan
    print(data.frame(author = levels(df$author), egileak) |>
      knitr::kable(caption = "Honela ordezkatu dira"))

    ## Ordezkatu
    df <- df |>
      mutate(egileak = author)
    levels(df$egileak) <- egileak

    if (is.data.frame(objektua)) {
      objektua <- df
    } else if (is.list(objektua)) {
      objektua$df_aldaketak <- df
    }
    return(objektua)
  }

  # Aurretik zerbitzarian emoniko ixenen arabera
  if (nameka == TRUE) {
    if (anonimu == TRUE) {
      warning("\nAukera honek izenik gabeko lerroak (NA) ezabatzen ditu.")
      df <- df |>
        filter(is.na(name) == FALSE) |>
        mutate(egileak = fct_anon(name, prefix = "E-")) |>
        select(-name)
      if (is.data.frame(objektua)) {
        objektua <- df
      } else if (is.list(objektua)) {
        objektua$df_aldaketak <- df
      }
      return(objektua)
    }

    df <- df |>
      mutate(egileak = name) |>
      select(-name)
    if (is.data.frame(objektua)) {
      objektua <- df
    } else if (is.list(objektua)) {
      objektua$df_aldaketak <- df
    }
    return(objektua)
  }

  if (authorka == TRUE) {
    if (anonimu == TRUE) {
      df <- df |>
        mutate(egileak = fct_anon(author, prefix = "E-"))
      if (is.data.frame(objektua)) {
        objektua <- df
      } else if (is.list(objektua)) {
        objektua$df_aldaketak <- df
      }
      return(objektua)
    }

    df <- df |>
      mutate(egileak = author)
    if (is.data.frame(objektua)) {
      objektua <- df
    } else if (is.list(objektua)) {
      objektua$df_aldaketak <- df
    }
    return(objektua)
  }
}
