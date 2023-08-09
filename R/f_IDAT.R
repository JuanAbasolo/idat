#' Etherpad dokumentu batean idatzitakoa aztertzeko oinarrizko data framea sortzen du
#'
#' @param kokapena Etherpad fitxategia non dagoen; e.b. "/home/juan/dokumentuak/"
#' @param fitxategia Etherpad fitxategiaren izen osoa, luzapena eta guzti. "adibidea.etherpad"
#'
#' @return Etherpaden idatzitako dokumentuaren analisirako, \code{kokapena}ren eta \code{izena} emanda.
#'
#' @importFrom stringr str_detect str_remove str_remove_all str_extract str_match str_count
#' @importFrom dplyr tibble mutate rename relocate rowwise mutate_if filter last_col lag bind_cols across
#' @importFrom tidyr unnest_wider pivot_wider
#' @importFrom rlang .data
#' @importFrom tibble rownames_to_column
#'
#' @examples
#' # kokapena <- 'extdata/'
#' # fitxategia <- 'txepetx-2-lo1uvq90o.etherpad'
#' # o_idat <- f_IDAT(kokapena, fitxategia)
#' o_idat
#'
#' @export

f_IDAT <- function(kokapena = kokapena, fitxategia = fitxategia) {
  # # epoc formatuko datak gure erara ekarteko
  # # [zati 1000 be egin bihar da, baina ez dakit zergaitik]
  # f_epoc_ekarri <- function(x){
  #     as.POSIXct(x/1000, origin="1970-01-01")
  # }
  #
  # f_36_10era <- function(x) strtoi(x, base = 36)

  # Hasikera
  parsetuta <- jsonlite::read_json(paste0(kokapena, fitxategia))
  izenak <- names(parsetuta)

  banatzeko <- izenak
  banatzeko[str_detect(banatzeko, pattern = "globalAuthor:")] <- "egilea"
  banatzeko[str_detect(banatzeko, pattern = ":revs:")] <- "prozesua"
  banatzeko[str_detect(banatzeko, pattern = ":chat:")] <- "txata"
  banatzeko[str_detect(banatzeko, pattern = "pad:")] <- "nagusia"

  # Elementuak
  pad <- names(parsetuta)[1] |>
    str_remove("\\w*:")
  testua <- parsetuta[[1]]$atext$text
  ni <- "a.WqkJLdiHlrEQ40Fl"

  # Pool-en data framea
  l <- parsetuta[[izenak[banatzeko == "nagusia"]]]$pool$numToAttrib

  df_pool <- l |>
    unlist() |>
    matrix(nrow = length(l), ncol = 2, byrow = T) |>
    data.frame(row.names = names(l), stringsAsFactors = FALSE) |>
    rownames_to_column() |>
    mutate(pad = pad) |>
    relocate(pad, .before = 1) |>
    rename(
      numToAttrib = rowname,
      var = X1,
      attrib = X2
    ) |>
    mutate(
      pad = factor(pad),
      var = factor(var),
      attrib = factor(attrib)
    ) |>
    tibble()

  # Egileen data framea

  l <- parsetuta[izenak[banatzeko == "egilea"]]

  df_egileak <- tibble(l) |>
    unnest_wider(l) |>
    mutate(
      colorId = unlist(colorId),
      author = names(l) |> str_remove("globalAuthor:") # ,
      # desizenak = paste0(1:length(l), '_trm'),
      # izenordainak = LETTERS[as.numeric(as.factor(name))]
    ) |>
    mutate(noz = f_epoc_ekarri(timestamp)) |>
    mutate_if(is.character, as.factor) |>
    relocate(pad = padIDs, .before = 1)

  ## Aldaketen data framea:
  l <- parsetuta[izenak[banatzeko == "prozesua"]]

  dtk <- l |>
    unlist() |>
    attributes() |>
    unlist() |>
    unname()
  pad <- dtk[1] |>
    str_extract(":([\\d\\w-]*):") |>
    str_remove_all(":")
  revs <- dtk |>
    str_extract(":revs:\\d+\\.") |>
    str_extract("\\d+") |>
    as.numeric()
  zer <- dtk |>
    str_extract("\\w+$")
  balioak <- l |>
    unlist() |>
    unlist() |>
    unname()

  # Instantzia batzuetan sortzen dira elementu bakan batzuk ez dabena
  # ondo ixten korritzen. Horreek garbitzeko sortzen da `kontuanizatekoak`aldagaia.
  # Esate baterako:
  #       2022/09 HD-LHko lanak korritzeko bihar da
  #       Aztertu bihar da zer dizen:
  #       "pad:1ga-01-1tl-tmfj94v:revs:300.meta.pool.numToAttrib.42"
  # Beste urtenbide posible bat:
  #   filter(str_detect(zer, '\\d{1,}', negate = TRUE)) |> gehitzia

  kontuanizatekoak <- c("author", "attribs", "changeset", "text", "timestamp")

  #
  df_aldaketak <- data.frame(revs,
    zer,
    balioak,
    stringsAsFactors = FALSE
  ) |>
    # filter(str_detect(zer, '\\d{1,}', negate = TRUE)) |>
    filter(zer %in% kontuanizatekoak) |>
    pivot_wider(
      names_from = "zer",
      values_from = "balioak",
      values_fill = NA
    ) |>
    mutate(
      noz = f_epoc_ekarri(as.numeric(timestamp)),
      pad = pad,
      timestamp = as.numeric(timestamp),
      author = as.factor(author),
      pad = factor(pad)
    )

  # Changeset dekodetzeko
  eredua_changeseterako <- "Z:([:alnum:]+)(<|>)([:alnum:]+)(.*)\\$([[:print:]\\n]*)"

  df_aldaketak <- df_aldaketak$changeset |>
    str_match(eredua_changeseterako) |>
    as.data.frame() |>
    select(V2, V3, V4, V5, V6) |>
    bind_cols(df_aldaketak) |>
    relocate(1:5, .after = last_col()) |>
    rename(
      oldLen = V2,
      azktex = V3,
      newLen = V4,
      opt = V5,
      charBank = V6
    ) |>
    mutate(across(c(oldLen, newLen), f_36_10era),
      bank_luzera = str_count(charBank)
    ) |>
    tibble() |>
    relocate(pad, .before = 1) |>
    left_join(df_egileak[, c("author", "name")], by = "author")

  ## opt dekodetu (a) zatia
  df_aldaketak <- df_aldaketak$opt |>
    str_match("([[:alnum:]\\+\\|=]*).*([\\+-][:alnum:]+)$") |>
    data.frame() |>
    select(-1) |>
    rename(
      landu = 1,
      amaieran = 2
    ) |>
    bind_cols(df_aldaketak) |>
    relocate(1:2, .after = last_col())

  # Azken antolaketa
  df_aldaketak <- df_aldaketak$landu |>
    str_match("^\\|*([:alnum:]*)=([:alnum:]*)=*([:alnum:]*)") |>
    as.matrix() |>
    data.frame() |>
    select(X2, X3, X4) |>
    rename(
      kok_lerru = X2,
      kok_aurreko_kar_kop = X3,
      kok_lerrun_nun = X4
    ) |>
    sapply(function(x) strtoi(x, base = 36)) |>
    data.frame() |>
    mutate(
      kok_lerru = ifelse(is.na(kok_lerru), 0, kok_lerru),
      # kok_lerrun_nun = ifelse(is.na(kok_lerrun_nun), 0, kok_lerrun_nun)) |> # Honek arazoak emoten ditu formatu aldaketa hutsian dabixenin
      kok_lerrun_nun = ifelse(is.na(kok_lerrun_nun), kok_aurreko_kar_kop, kok_lerrun_nun)
    ) |> # Honek arazoak emoten ditu formatu aldaketa hutsian dabixenin

    bind_cols(df_aldaketak) |>
    relocate(1:3, .after = last_col())

  df_aldaketak <- df_aldaketak |>
    rowwise() |>
    mutate(kok_erlatiboa = kok_aurreko_kar_kop / (oldLen + bank_luzera)) |>
    ungroup()

  df_aldaketak <- df_aldaketak |>
    select(-landu, -amaieran) |>
    group_by(author) |> # HAU BIHAR DA?
    mutate(
      aldi = timestamp - lag(timestamp),
      lerro_aldaketa = kok_lerru - lag(kok_lerru)
    ) |>
    ungroup() |>
    relocate(
      pad, revs, changeset, author, name,
      timestamp, opt, oldLen, newLen,
      charBank, text, attribs,
      noz, aldi,
      azktex, bank_luzera,
      kok_lerru, kok_aurreko_kar_kop, kok_lerrun_nun, kok_erlatiboa,
      lerro_aldaketa
    )

  # Amaiera eta urtera
  return(list(
    pad = pad,
    testua = testua,
    df_aldaketak = df_aldaketak,
    df_egileak = df_egileak,
    df_pool = df_pool,
    ni = ni
  ))
}
