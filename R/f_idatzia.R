#' Idazteko orduan sortu den testuaren kalkuluetarako aldagaiak sortzen ditu
#'
#' @param objektua IDAT erabilita sortutako zerrenda edo data framea
#' @param lerru Aldaketak zein lerrotan ematen diren eta aurreko lerrotik zein distantzia duen
#' @param etenak Etenen luzera zein izan den.#'
#' @param dm Etentzat hartzeko denbora muga, milisegunduetan. Balio lehenetsia 3 segundu dira, 3000.
#' @param ezabatuak Idazketa prozesuan ezabatutakoak zenbat karakterekoak izan diren, pausuka.
#' @param copypaste Hamar karaktere baino gehiagoko pausuak nabarmentzen ditu.
#' @param lerroko_jauziak Lerroan / paragrafoan bertan egindako lekutze aldaketak karaktereka zenbatzen ditu eta norabidea seinalatzen du.
#'
#' @return Etherpaden idatzitako dokumentu baten \code{objektua}aren prozesuan sortutako
#' testuaren berreraiketa, hainbat elementu nabarmenduta.
#'
#' @importFrom dplyr mutate lag rowwise select arrange ungroup group_by
#' @importFrom stringr str_replace_all
#'
#' @examples
#' require(dplyr)
#' require(stringr)
#' # o_idat <- f_IDAT(fitxategia, kokapena)
#' f_idatzia(o_idat)$df_aldaketak |>
#'   filter(egileak == levels(egileak)[2]) |>
#'   select(idatzia) |>
#'   unlist() |>
#'   cat(sep = "")
#'
#' @export

f_idatzia <- function(objektua,
                      lerru = TRUE,
                      etenak = TRUE, dm = 3000,
                      ezabatuak = TRUE,
                      copypaste = TRUE,
                      lerroko_jauziak = TRUE) {
  if (is.data.frame(objektua)) {
    df <- objektua
  } else if (is.list(objektua)) {
    df <- objektua$df_aldaketak
    df_egileak <- objektua$df_egileak
  }

  if (!"egileak" %in% names(df)) {
    df <- df |>
      mutate(egileak = author)
  }
  hasieri <- names(df)

  if (lerru) {
    if (any(is.na(df$kok_aurreko_kar_kop)) | any(is.na(lag(df$kok_aurreko_kar_kop)))) {
      galduk_jauziak <- sum(is.na(df$kok_aurreko_kar_kop))
    }
    df <- df |>
      rowwise() |>
      mutate(
        lerru = ifelse(lerro_aldaketa == 0,
          "",
          ifelse(lerro_aldaketa < 0,
            paste0(c(
              "\n\n\U25CF", kok_lerru, ".Ln:",
              abs(lerro_aldaketa), "\U021D1",
              "::"
            ), collapse = ""),
            paste0(c(
              "\n\n\U25CF", kok_lerru, "Ln:",
              abs(lerro_aldaketa), "\U021D3",
              "::"
            ), collapse = "")
          )
        ),
        lerru = if (is.na(lerru)) {
          "\n\n?\U25CFLn:?::"
          # print(warnErrList("Batzuk NA ixan dozak"))
        } else {
          lerru
        }
      )
  } else {
    df <- df |>
      rowwise() |>
      mutate(
        lerru = ifelse(lerro_aldaketa == 0,
          "",
          "\n\n"
        ),
        lerru = if (is.na(lerru)) {
          "\n\n"
          # print(warnErrList("Batzuk NA ixan dozak"))
        } else {
          lerru
        }
      )
  }
  if (etenak) {
    if (any(is.na(df$aldi))) {
      galduk_etenak <- sum(is.na(df$aldi))
    }
    df <- df |>
      rowwise() |>
      mutate(
        etenak = ifelse(aldi > dm,
          paste0("<", ifelse(round(aldi / 1000, 1) <= 60,
            round(aldi / 1000, 1),
            paste0(
              round(aldi / 1000, 1) %/% 60, "m.",
              round(round(aldi / 1000, 1) %% 60)
            )
          ), "seg>", ""),
          ""
        ),
        etenak = if (is.na(etenak)) {
          ""
          # print(warnErrList("Batzuk NA ixan dozak"))
        } else {
          etenak
        }
      )
  }
  if (ezabatuak) {
    df <- df |>
      rowwise() |>
      mutate(
        ezabatuak =
          ifelse(azktex == "<" & newLen > 0,
            paste0("[\U02190", as.character(newLen), "]",
              collapse = ""
            ),
            ""
          )
      )
  }
  if (copypaste) {
    df <- df |>
      rowwise() |>
      mutate(
        charBank = ifelse(is.na(charBank),
          NA,
          ifelse(bank_luzera > 10,
            paste0("\U25A0", "C+P\U021D2::",
              str_replace_all(charBank, "\n", "\U021A9"),
              "::\U021D0", "C+P\U25A0\n",
              collapse = ""
            ),
            charBank
          )
        )
      )
  }
  if (lerroko_jauziak) {
    if (any(is.na(df$kok_aurreko_kar_kop)) | any(is.na(lag(df$kok_aurreko_kar_kop)))) {
      galduk_jauziak <- sum(is.na(df$kok_aurreko_kar_kop))
    }
    df <- df |>
      group_by(author) |>
      arrange(author, revs) |>
      mutate(
        gehi = kok_lerrun_nun + ifelse(azktex == ">", newLen, -(newLen)),
        jauzi = kok_lerrun_nun - lag(gehi)
      ) |>
      ungroup() |>
      arrange(revs) |>
      mutate(
        jauziB = ifelse(jauzi != 0,
          jauzi + ifelse(lag(azktex) == ">",
            -lag(newLen),
            lag(newLen)
          ),
          NA
        ),
        ## Hurrengo honetan 5 ipinten da 5 karaktereko tolerantzia eukiteko.
        ## 0 tolerantzia litzateke 1 ipinita.
        seg = ifelse(abs(jauzi) < 5,
          TRUE,
          ifelse(jauziB == 0, TRUE, FALSE)
        ),
        jauzia = ifelse(seg == TRUE, "", paste0(
          "[", abs(jauzi),
          ifelse(jauzi < 0, "\U021B6]", "\U021B7]")
        )),
        jauzia = ifelse(is.na(jauzia), "", jauzia)
      ) |>
      select(-seg, -jauzi, -jauziB, -gehi)
  }
  amaieran <- names(df)
  # print(c(setdiff(amaieran, hasieri), 'charBank'))

  # Abixuak
  if (lerru | lerroko_jauziak) {
    mezu <- paste0(
      "\nLerro arteko jauzietan eta",
      " lerro barrukoetan ",
      galduk_jauziak,
      " datu galdu egon dira; beraz, horrenbeste marka falta daitekez\n\n"
    )
    warning(mezu)
  }
  if (etenak) {
    mezu <- paste0(
      "Etenenen marketan ere, ",
      galduk_etenak,
      " datu falta dira; beraz, horrenbeste eten markatzeke egon daitekez\n"
    )
    warning(mezu)
  }

  if (!lerroko_jauziak) {
    df$idatzia <- do.call(
      paste,
      c(df[c(setdiff(amaieran, hasieri), "charBank")],
        sep = ""
      )
    )

    if (is.list(objektua)) {
      objektua$df_aldaketak <- df
    }
    if (is.data.frame(objektua)) {
      objektua <- df
    }
    return(objektua)
  }
  df <- df |>
    #     rowwise() |>
    mutate(idatzia = do.call(
      paste,
      c(df[c(setdiff(amaieran, hasieri), "charBank")],
        sep = ""
      )
    ))

  if (is.data.frame(objektua)) {
    objektua <- df
  } else if (is.list(objektua)) {
    objektua$df_aldaketak <- df
  }
  return(objektua)
} # Hamen amaitzen da funtzinoia
