# =====================================================================
# Suhdannetilanne - koko putki
# Lukee series.json, hakee sarjat (StatFin, Eurostat, EKP, FRED),
# laskee jokaiselle trendipoikkeaman (taso) ja sen muutoksen,
# kokonaisindikaattorin setin sisalla ja kirjoittaa clock.json.
# Aja: Rscript pipeline.R [series.json] [clock.json]
# Riippuvuudet: httr, jsonlite, stats (base).
# =====================================================================

suppressPackageStartupMessages({ library(httr); library(jsonlite) })
if (!l10n_info()$`UTF-8`)
  stop("Aja UTF-8-lokaalissa, esim. LANG=C.UTF-8 Rscript pipeline.R (muuten ä/ö rikkoutuvat)")

args     <- commandArgs(trailingOnly = TRUE)
CFG_PATH <- if (length(args) >= 1) args[1] else "series.json"
OUT_PATH <- if (length(args) >= 2) args[2] else "clock.json"

Z_FROM   <- 2010          # standardointi-ikkunan ja julkaisun alkuvuosi
LAMBDA   <- c(M = 129600, Q = 1600)  # trendi
LAMBDA_S <- c(M = 14,     Q = 1)     # kohinasuodin: poistaa alle ~12 kk syklit
TAIL_N   <- c(M = 6, Q = 4)
EXT_H    <- c(M = 12, Q = 4)  # ennustejatke ennen suodatusta (paatepisteongelma, Kaiser ja Maravall 1999)
TILE_N   <- 130           # markkinaruutujen paivahavainnot; sivu nayttaa niista viimeiset 3 kk
CACHE_DIR <- "cache"      # edelliset onnistuneet haut sarjoittain
STALE    <- character(0)  # sarjat, joiden data tuli valimuistista
STALE_OLD <- character(0) # ... ja valimuisti on yli STALE_DAYS vanha: vasta silloin tarkistushuomio (tilapaiset katkot eivat kaada ajoa)
STALE_DAYS <- 3
VALID_TXT <- ""
CHECKS   <- character(0)    # ajon tarkistusten huomiot (ks. lopun tarkistukset)

STATFIN  <- "https://pxdata.stat.fi/PXWeb/api/v1/fi/StatFin/"
EUROSTAT <- "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/"
ECB      <- "https://data-api.ecb.europa.eu/service/data/"
FRED     <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id="
BOF      <- "https://api.boffsaopendata.fi/v4/observations/"

# ---------------------------------------------------------------------
# Jaksot: "YYYY-MM" tai "YYYY-Qn" <-> kokonaislukuindeksi
# ---------------------------------------------------------------------
per_idx <- function(p, freq) {
  y <- as.integer(substr(p, 1, 4))
  if (freq == "M") y * 12 + as.integer(substr(p, 6, 7)) - 1
  else             y * 4  + as.integer(substr(p, 7, 7)) - 1
}
idx_per <- function(i, freq) {
  if (freq == "M") sprintf("%04d-%02d", i %/% 12, i %% 12 + 1)
  else             sprintf("%04d-Q%d",  i %/% 4,  i %% 4 + 1)
}

get_text <- function(url) {
  px <- grepl("pxdata.stat.fi", url, fixed = TRUE)   # Tilastokeskus rajoittaa kyselytiheytta (429): odota rajoitusikkuna yli
  r <- RETRY("GET", url, timeout(40), times = if (px) 5 else 3, pause_base = if (px) 5 else 2, pause_min = if (px) 11 else 1, quiet = TRUE)
  stop_for_status(r)
  content(r, "text", encoding = "UTF-8")
}

# ---------------------------------------------------------------------
# Lahdeadapterit: kaikki palauttavat data.frame(date, value)
# ---------------------------------------------------------------------
is_time_px <- function(vals) any(grepl("^\\d{4}(M\\d{2}|Q\\d)$", vals))   # kk tai nelj.

# StatFin rajoittaa kyselyja (noin 30 / 10 s): taulujen metatiedot muistiin ja pieni tauko kyselyjen valiin
.px_meta <- new.env()
fetch_statfin <- function(s) {
  base <- if (is.null(s$db)) STATFIN else sub("StatFin/$", paste0(s$db, "/"), STATFIN)   # db: esim. StatFin_Passiivi (arkistotaulut)
  if (!is.null(s$px_base)) base <- s$px_base                                             # muu PxWeb-palvelu (esim. Verohallinto)
  if (is.null(.px_meta[[s$table]])) {
    Sys.sleep(0.8); .px_meta[[s$table]] <- fromJSON(get_text(paste0(base, s$table)), simplifyVector = FALSE)$variables }
  vars <- .px_meta[[s$table]]
  Sys.sleep(0.8)
  # muuttujakoodeissa on luokituksen paivays (esim. alue_23_20260101), joka vaihtuu luokituksen paivittyessa:
  # jos tarkkaa koodia ei loydy, kaytetaan valintaa, jonka koodi alkaa samalla tunnuksella
  stem <- function(x) sub("_\\d+_\\d{8}$", "", x)
  pick <- s$pick
  for (k in setdiff(names(pick), vapply(vars, function(v) v$code, ""))) {
    hit <- Filter(function(v) stem(v$code) == stem(k), vars)
    if (length(hit) != 1) stop("StatFin ", s$table, ": muuttujaa '", k, "' ei loydy")
    CHECKS <<- c(CHECKS, sprintf("%s: muuttuja %s -> %s (paivita series.json)", s$id, k, hit[[1]]$code))
    message("  HUOM ", s$table, ": ", k, " -> ", hit[[1]]$code); pick[[hit[[1]]$code]] <- pick[[k]]
  }
  q <- lapply(vars, function(v) {
    vals <- unlist(v$values)
    if (is_time_px(vals))
      return(list(code = v$code, selection = list(filter = "all", values = list("*"))))
    chosen <- if (!is.null(pick[[v$code]])) pick[[v$code]] else {
      message("  OLETUS ", v$code, " = ", vals[1]); vals[1] }
    if (!(chosen %in% vals)) stop("StatFin ", s$table, ": arvoa '", chosen, "' ei ole muuttujassa ", v$code)
    list(code = v$code, selection = list(filter = "item", values = list(chosen)))
  })
  body <- toJSON(list(query = q, response = list(format = "json-stat2")), auto_unbox = TRUE)
  r <- RETRY("POST", paste0(base, s$table), body = body, content_type_json(),
             timeout(40), times = 5, pause_base = 5, pause_min = 11, quiet = TRUE)   # 429: odota rajoitusikkuna yli
  stop_for_status(r)
  js <- fromJSON(content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  tcode <- names(js$dimension)[vapply(js$dimension,
             function(d) is_time_px(names(d$category$index)), logical(1))][1]
  idx <- unlist(js$dimension[[tcode]]$category$index)
  ord <- names(idx)[order(idx)]
  v <- js$value; v[vapply(v, is.null, logical(1))] <- NA
  data.frame(date = sub("Q", "-Q", sub("M", "-", ord)), value = as.numeric(unlist(v))[seq_along(ord)])
}

fetch_eurostat <- function(s, geo = "FI") {
  if (!is.null(s$geo)) geo <- s$geo                  # vertailumaat (esim. SE, DE, EA)
  pick <- c(s$pick, geo = geo)
  qs <- paste(sprintf("%s=%s", names(pick), unlist(pick)), collapse = "&")
  js <- fromJSON(get_text(paste0(EUROSTAT, s$ds, "?format=JSON&lang=EN&", qs)),
                 simplifyVector = FALSE)
  extra <- setdiff(names(js$dimension), c("time", names(pick)))
  for (d in extra)
    if (length(js$dimension[[d]]$category$index) > 1)
      stop("Eurostat ", s$ds, ": dimensio '", d, "' valitsematta")
  tidx <- unlist(js$dimension$time$category$index)
  codes <- names(tidx)[order(tidx)]
  vals <- vapply(tidx[codes], function(p) {
    x <- js$value[[as.character(p)]]; if (is.null(x)) NA_real_ else as.numeric(x)
  }, numeric(1))
  data.frame(date = codes, value = unname(vals))
}

fetch_ecb <- function(s) {
  d <- read.csv(text = get_text(sprintf("%s%s/%s?format=csvdata%s", ECB, s$flow, s$key,
                                        if (is.null(s$start)) "" else paste0("&startPeriod=", s$start))))
  d <- data.frame(date = d$TIME_PERIOD, value = as.numeric(d$OBS_VALUE))
  if (all(grepl("^\\d{4}-W\\d{2}$", d$date))) {      # viikkodata (esim. Eurojarjestelman tase): viikon perjantai paivamaaraksi
    y <- as.integer(substr(d$date, 1, 4)); w <- as.integer(substr(d$date, 7, 8))
    j4 <- as.Date(sprintf("%d-01-04", y)); mon <- j4 - (as.integer(format(j4, "%u")) - 1) + 7 * (w - 1)
    d$date <- format(mon + 4, "%Y-%m-%d") }
  if (!isTRUE(s$daily) && all(nchar(d$date) == 10)) {   # paivadata -> kuukauden viimeinen havainto
    d <- d[order(d$date), ]; m <- substr(d$date, 1, 7)
    d <- data.frame(date = unique(m), value = tapply(d$value, m, function(v) tail(v, 1))[unique(m)])
  }
  d
}

# Maailmanpankin raaka-ainehinnat (Pink Sheet, CC BY 4.0): kuukausitiedosto, jonka osoite vaihtuu kuukausittain,
# joten linkki haetaan Commodity Markets -sivulta. Tiedosto ladataan kerran ajoa kohden.
WB_PAGE <- "https://www.worldbank.org/en/research/commodity-markets"
.wb <- new.env()
wb_sheet <- function(sheet) {
  if (is.null(.wb$file)) {
    html <- get_text(WB_PAGE)
    url <- regmatches(html, regexpr("https://thedocs\\.worldbank\\.org[^\"']*CMO-Historical-Data-Monthly\\.xlsx", html))
    if (!length(url)) stop("Maailmanpankki: kuukausitiedoston linkkia ei loydy sivulta")
    f <- tempfile(fileext = ".xlsx"); r <- RETRY("GET", url, write_disk(f, overwrite = TRUE), timeout(120), times = 3, quiet = TRUE)
    stop_for_status(r); .wb$file <- f; message("  Maailmanpankki: ", basename(dirname(dirname(url))))
  }
  if (is.null(.wb[[sheet]])) .wb[[sheet]] <- as.data.frame(readxl::read_excel(.wb$file, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
  .wb[[sheet]]
}
fetch_wb <- function(s) {
  x <- wb_sheet(if (is.null(s$sheet)) "Monthly Prices" else s$sheet)
  clean <- function(v) gsub("\\s+", " ", trimws(gsub("\\*", "", v)))
  hdr <- x[1:12, , drop = FALSE]
  col <- which(vapply(seq_along(hdr), function(j) any(clean(hdr[[j]]) == clean(s$series), na.rm = TRUE), logical(1)))
  if (length(col) != 1) stop("Maailmanpankki: sarakkeita '", s$series, "' loytyi ", length(col))
  rows <- grepl("^\\d{4}M\\d{2}$", x[[1]])
  data.frame(date = sub("M", "-", x[[1]][rows]), value = suppressWarnings(as.numeric(x[[col]][rows])))
}

# Excel-tiedosto verkosta (esim. Caldara ym. kauppapoliittisen epavarmuuden indeksi): valilehti, paivays- ja arvosarake
.xlsx_memo <- new.env()                          # sama tiedosto useammalle sarjalle (esim. GPR: maailma ja Suomi) haetaan kerran
fetch_xlsx <- function(s) {
  if (is.null(.xlsx_memo[[s$url]])) {
    f <- tempfile(fileext = paste0(".", tools::file_ext(s$url))); r <- RETRY("GET", s$url, write_disk(f, overwrite = TRUE), timeout(120), times = 3, quiet = TRUE)
    stop_for_status(r); .xlsx_memo[[s$url]] <- f }
  x <- as.data.frame(readxl::read_excel(.xlsx_memo[[s$url]], sheet = s$sheet, guess_max = 10000))
  d <- as.Date(x[[s$date_col]]); v <- suppressWarnings(as.numeric(x[[s$value_col]]))
  ok <- !is.na(d) & !is.na(v)
  data.frame(date = format(d[ok], "%Y-%m"), value = v[ok])
}

fetch_fred <- function(s) {
  d <- read.csv(text = get_text(paste0(FRED, s$fred_id, if (isTRUE(s$fred_avg)) "&fq=Monthly&fam=avg" else "")))   # fred_avg: paivasarjan kuukausikeskiarvo
  d <- data.frame(date = substr(d[[1]], 1, if (isTRUE(s$daily)) 10 else 7), value = suppressWarnings(as.numeric(d[[2]])))
  if (isTRUE(s$fred_avg)) d <- d[!is.na(d$value), ]   # kesken oleva kuukausi tyhja
  d
}

fetch_bof <- function(s) {                       # Suomen Pankki, avoin data v4
  js <- fromJSON(get_text(sprintf("%s%s?seriesName=%s&pageSize=10000", BOF, s$dataset, s$key)),
                 simplifyVector = FALSE)
  obs <- js$items[[1]]$observations
  pc  <- vapply(obs, function(o) o$periodCode, character(1))           # esim. "1997M9"
  data.frame(date  = sprintf("%s-%02d", substr(pc, 1, 4), as.integer(sub("^\\d{4}M", "", pc))),
             value = vapply(obs, function(o) if (is.null(o$value)) NA_real_ else as.numeric(o$value), numeric(1)))
}

# HICP:n vuosimuutoksen kontribuutiot (%-yks.) Eurostatin indekseista ja painoista.
# Ketjutettu indeksi: pi = (A - 1) * B + (B - 1), A = I_t / I_Dec(y-1), B = I_Dec(y-1) / I_t-12;
# erat painoilla w_y ja w_y-1. Tasmaa Tilastokeskuksen julkaisemiin vaikutuksiin (YKHI) ~0,002 %-yks.
.hicp_memo <- new.env()
hicp_get <- function(ds, pick) {
  key <- paste(ds, unlist(pick), collapse = "_")
  if (is.null(.hicp_memo[[key]])) .hicp_memo[[key]] <- fetch_eurostat(list(ds = ds, pick = pick))
  .hicp_memo[[key]]
}
fetch_hicp_ctrb <- function(s) {
  idx <- function(code) { d <- hicp_get("prc_hicp_minr", list(coicop18 = code, unit = "I25")); setNames(d$value, d$date) }
  wts <- function(code) { d <- hicp_get("prc_hicp_iw", list(coicop18 = code, statinfo = "IW")); setNames(d$value, d$date) }
  it <- idx("TOTAL"); dates <- names(it)
  y <- as.integer(substr(dates, 1, 4)); m <- as.integer(substr(dates, 6, 7))
  ym <- function(y, m) sprintf("%04d-%02d", y, m)
  t12 <- ym(y - 1, m); D <- ym(y - 1, 12); D2 <- ym(y - 2, 12)
  if (s$coicop18 == "TOTAL") return(data.frame(date = dates, value = unname(100 * (it[dates] / it[t12] - 1))))
  ix <- idx(s$coicop18)[dates]; names(ix) <- dates
  wt <- wts("TOTAL"); wx <- wts(s$coicop18); w <- wx / wt[names(wx)]
  w1 <- w[as.character(y)]; w0 <- w[as.character(y - 1)]
  B <- it[D] / it[t12]; den <- it[t12] / it[D2]
  v <- 100 * (w1 * (ix[dates] / ix[D] - 1) * B + w0 * (ix[D] - ix[t12]) / ix[D2] / den)
  data.frame(date = dates, value = unname(v))
}

# BKT:n kysyntaerien kontribuutiot vuosikasvuun (%-yks.) Tilastokeskuksen neljannesvuositilinpidosta.
# Vuosiketjutus: erat edellisen vuoden hinnoin = viitevuoden volyymi * edellisen vuoden implisiittinen hinta.
# c_i = 100 * (X_i,t - X_i,t-4) * P_i,y-1 / (Y_t-4 * P_Y,y-1); tasmaa Eurostatin kontribuutioihin ~0,01 %-yks.
.ntp_memo <- new.env()
fetch_ntp_ctrb <- function(s) {
  get <- function(i, cc) { key <- paste(i, cc)
    if (is.null(.ntp_memo[[key]])) .ntp_memo[[key]] <- fetch_statfin(list(table = "ntp/132h.px",
      pick = list(taloustoimi_1_20180101 = i, contentscode = cc)))
    d <- .ntp_memo[[key]]; setNames(d$value, d$date) }
  pbar <- function(i) { c1 <- get(i, "tyopcp"); v1 <- get(i, "tyopvv2015")[names(c1)]; yy <- substr(names(c1), 1, 4)
    p <- tapply(c1, yy, sum) / tapply(v1, yy, sum); p[tapply(!is.na(c1), yy, sum) < 4] <- NA; p }
  tot <- if (is.null(s$total)) "B1GMH" else s$total   # kokonaiserä: BKT tai kysyntäerä (esim. vienti), jonka kasvua osat selittävät
  Y <- get(tot, "tyopvv2015"); dates <- names(Y); y <- as.integer(substr(dates, 1, 4))
  t0 <- sprintf("%d%s", y - 1, substr(dates, 5, 7)); py <- as.character(y - 1)
  if (isTRUE(s$contrib_total)) return(data.frame(date = dates, value = unname(100 * (Y[dates] / Y[t0] - 1))))
  PY <- pbar(tot); v <- 0
  for (k in seq_along(s$items)) {
    X <- get(s$items[[k]], "tyopvv2015"); P <- pbar(s$items[[k]]); sg <- if (is.null(s$signs)) 1 else s$signs[[k]]
    v <- v + sg * 100 * (X[dates] - X[t0]) * P[py] / (Y[t0] * PY[py])
  }
  data.frame(date = dates, value = unname(v))
}

# kotitalouksien kaytettavissa olevan reaalitulon (S14, B6N) kasvu ja osatekijoiden kasvuvaikutukset, %-yks.:
# neljan neljanneksen summat, vuosimuutos. Palkansaajakorvaukset jaetaan palkansaajien tehtyihin tyotunteihin ja
# keskiansioon tyotuntia kohden; hinta = yksityisen kulutuksen deflaattori (reaali- ja nimellismuutoksen erotus).
.hhi <- new.env()
fetch_hhi_ctrb <- function(s) {
  if (is.null(.hhi$res)) {
    sk <- function(i) { d <- fetch_statfin(list(table = "ntp/15a6.px", pick = list(sektoriluokitus_7_20230101 = "S14", taloustoimi_1_20180101 = i, contentscode = "ntp-TASM")))
      setNames(d$value, d$date) }
    nt <- function(t, i, cc, extra = list()) { d <- fetch_statfin(list(table = t, pick = c(list(taloustoimi_1_20180101 = i), extra, list(contentscode = cc))))
      setNames(d$value, d$date) }
    X <- list(Y = sk("B6N"), W = sk("D1R"), B = sk("D62R"), T1 = sk("D5K"), T2 = sk("D61K"),
              cp = nt("ntp/132h.px", "P3KS14_S15", "tasmcp"), vv = nt("ntp/132h.px", "P3KS14_S15", "tasmvv2015"),
              H = nt("ntp/11tj.px", "E22", "ntp-TASM", list(toimiala_79_20180101 = "SSS")))
    dates <- sort(Reduce(intersect, lapply(X, names)))
    S4 <- lapply(X, function(x) as.numeric(stats::filter(x[dates], rep(1, 4), sides = 1)))
    L <- function(x) c(rep(NA, 4), head(x, -4))
    Y <- S4$Y; T <- S4$T1 + S4$T2; ly <- L(Y)
    c_w <- 100 * (S4$W - L(S4$W)) / ly; c_b <- 100 * (S4$B - L(S4$B)) / ly; c_t <- -100 * (T - L(T)) / ly
    g <- 100 * (Y / ly - 1); c_o <- g - c_w - c_b - c_t
    c_h <- 100 * L(S4$W) / ly * (S4$H / L(S4$H) - 1)
    P <- S4$cp / S4$vv; pi <- P / L(P) - 1; real <- 100 * ((1 + g / 100) / (1 + pi) - 1)
    .hhi$res <- data.frame(date = dates, real = real, whour = c_w - c_h, hours = c_h, benef = c_b, tax = c_t, other = c_o, price = real - g)
  }
  d <- .hhi$res; data.frame(date = d$date, value = d[[s$part]])
}

# BKT:n nimellinen vuosikasvu jaettuna volyymiin ja hintaan (deflaattori), tyopaivakorjattu; hinta = nimellinen - volyymi
.gnp <- new.env()
fetch_gdp_np <- function(s) {
  if (is.null(.gnp$res)) {
    g <- function(cc) { d <- fetch_statfin(list(table = "ntp/132h.px", pick = list(taloustoimi_1_20180101 = "B1GMH", contentscode = cc))); setNames(d$value, d$date) }
    cp <- g("tyopcp"); vv <- g("tyopvv2015"); dates <- sort(intersect(names(cp), names(vv)))
    L <- function(x) c(rep(NA, 4), head(x, -4)); a <- cp[dates]; b <- vv[dates]
    nom <- 100 * (a / L(a) - 1); vol <- 100 * (b / L(b) - 1)
    .gnp$res <- data.frame(date = dates, nom = unname(nom), vol = unname(vol), price = unname(nom - vol))
  }
  d <- .gnp$res; data.frame(date = d$date, value = d[[s$part]])
}

# saumaus: sama sarja useasta taulusta (esim. perusvuoden vaihtuessa), osat jarjestyksessa, myohempi osa voittaa
# osan from/to rajaa jakson ("YYYY-MM"); tarkoitettu tasosarjoille kuten lukumaarille, ei indekseille
# kuluttajahintaindeksin (KHI) vuosimuutoksen vaikutukset (%-yks.) hyodykeryhmittain: perusvuosi 2015 (arkisto, -2025M12)
# ja 2025 (2026M01-) saumattuna; ryhma = hyodykkeiden vaikutusten summa etumerkein (terms), TOT = kokonaisindeksi
.khi_memo <- new.env()
fetch_khi_ctrb <- function(s) {
  get1 <- function(code) {
    if (is.null(.khi_memo[[code]])) {
      old <- fetch_statfin(list(id = paste0("khi_old_", code), db = "StatFin_Passiivi", table = "khi/statfinpas_khi_pxt_11xb_2025m12.px",
                                pick = list(`Hyödyke` = if (code == "TOT") "0" else code, Tiedot = "vuosivaikutus")))
      new <- fetch_statfin(list(id = paste0("khi_new_", code), table = "khi/15b5.px",
                                pick = list(coicop_46_20231201 = if (code == "TOT") "SSS" else code, contentscode = "vv_khi")))
      old <- old[old$date <= "2025-12" & !is.na(old$value), ]; new <- new[new$date >= "2026-01" & !is.na(new$value), ]
      .khi_memo[[code]] <- setNames(c(old$value, new$value), c(old$date, new$date)) }
    .khi_memo[[code]] }
  X <- lapply(names(s$terms), get1); dates <- Reduce(intersect, lapply(X, names))
  v <- Reduce(`+`, Map(function(x, w) w * x[dates], X, unlist(s$terms)))
  data.frame(date = dates, value = unname(v))[order(dates), ]
}

fetch_splice <- function(s) {
  parts <- lapply(seq_along(s$parts), function(k) { p <- modifyList(s$parts[[k]], list(id = paste0(s$id, "_osa", k)))
    x <- fetch_statfin(p)
    if (!is.null(p$from)) x <- x[x$date >= p$from, ]
    if (!is.null(p$to)) x <- x[x$date <= p$to, ]
    x[!is.na(x$value), ] })
  # chain: indeksit eri perusvuosilla ketjutetaan paallekkaisen jakson keskiarvolla (aiemmat osat uuteen tasoon)
  d <- Reduce(function(a, b) { if (isTRUE(s$chain)) { o <- intersect(a$date, b$date)
      if (!length(o)) stop(s$id, ": ketjutettavilla osilla ei paallekkaista jaksoa")
      a$value <- a$value * mean(b$value[b$date %in% o]) / mean(a$value[a$date %in% o]) }
    rbind(a[!(a$date %in% b$date), ], b) }, parts)
  d[order(d$date), ]
}

# STOXX-indeksit (esim. VSTOXX): paivatiedosto "pp.kk.vvvv;tunnus;arvo" -> kuukausikeskiarvo (vain paattyneet kuukaudet)
fetch_stoxx <- function(s) {
  x <- read.csv(text = get_text(s$url), sep = ";", colClasses = "character")
  d <- as.Date(x[[1]], "%d.%m.%Y"); v <- suppressWarnings(as.numeric(x[[3]])); ok <- !is.na(d) & !is.na(v)
  m <- format(d[ok], "%Y-%m"); a <- tapply(v[ok], m, mean); a <- a[names(a) < format(Sys.Date(), "%Y-%m")]
  data.frame(date = names(a), value = as.numeric(a))
}

# Valtiokonttorin avoin rajapinta (valtionvelka): JSON-lista, kentta 'field', suodatus 'filter' (esim. serial)
VK_API <- "https://api.tutkihallintoa.fi/central-government-debt/v1/"
.vk_memo <- new.env()
fetch_vk <- function(s) {
  if (is.null(.vk_memo[[s$path]])) .vk_memo[[s$path]] <- fromJSON(get_text(paste0(VK_API, s$path)), simplifyVector = FALSE)
  x <- .vk_memo[[s$path]]
  for (k in names(s$filter)) x <- Filter(function(r) identical(r[[k]], s$filter[[k]]), x)
  d <- data.frame(date = vapply(x, function(r) substr(r$date, 1, 7), ""), value = vapply(x, function(r) as.numeric(r[[s$field]]), 0))
  d[order(d$date), ]
}

# Fintraffic LAM (Digitraffic): raskaan liikenteen kuukausisummat asemittain. Taso ketjutetaan kuukausimuutoksista
# samoilta asemilta (asemien lisaykset eivat vaikuta), summat jaetaan painotetulla kalenteripaivien maaralla
# (ma-to 1,0, pe 0,93, la 0,26, su ja arkipyhat 0,23; estimoitu Uudenmaan paivadatasta 2025). Kuluva kuukausi pois.
LAM_API <- "https://tie.digitraffic.fi/api/tms/v1/history"
easter_date <- function(y) {
  a <- y %% 19; b <- y %/% 100; c <- y %% 100; d <- b %/% 4; e <- b %% 4; f <- (b + 8) %/% 25; g <- (b - f + 1) %/% 3
  h <- (19 * a + b - d - g + 15) %% 30; i <- c %/% 4; k <- c %% 4; l <- (32 + 2 * e + 2 * i - h - k) %% 7; m <- (a + 11 * h + 22 * l) %/% 451
  as.Date(sprintf("%d-%02d-%02d", y, (h + l - 7 * m + 114) %/% 31, ((h + l - 7 * m + 114) %% 31) + 1))
}
lam_days <- function(ym) {
  y <- as.integer(substr(ym, 1, 4)); d0 <- as.Date(paste0(ym, "-01")); d <- seq(d0, seq(d0, by = "month", length.out = 2)[2] - 1, by = "day")
  e <- easter_date(y); mid <- as.Date(sprintf("%d-06-%02d", y, 19:25)); mid <- mid[as.integer(format(mid, "%u")) == 5]
  hol <- c(as.Date(sprintf("%d-%s", y, c("01-01", "01-06", "05-01", "12-06", "12-24", "12-25", "12-26"))), e - 2, e + 1, e + 39, mid, mid + 1)
  w <- c(1.01, 1.022, 1.025, 1.015, 0.927, 0.262, 0.226)[as.integer(format(d, "%u"))]
  sum(ifelse(d %in% hol, 0.226, w))
}
fetch_lam <- function(s) {
  last <- format(seq(as.Date(format(Sys.Date(), "%Y-%m-01")), by = "-1 month", length.out = 2)[2], "%Y-%m")
  x <- do.call(rbind, lapply(s$sets, function(p) {
    u <- paste0(LAM_API, "?api=liikennemaara&tyyppi=kk&pvm=", s$from, "&loppu=", last, "&lam_type=option2&pistejoukko=", p,
                "&ryhma=", URLencode(s$group_code, reserved = TRUE), "&suunta=&sisallytakaistat=0")
    r <- RETRY("GET", u, timeout(600), add_headers(`Digitraffic-User` = "suhdannetilanne.fi"), times = 3, pause_base = 5, quiet = TRUE)
    stop_for_status(r)
    y <- read.csv(text = content(r, "text", encoding = "UTF-8"), sep = ";", colClasses = "character", check.names = FALSE)
    data.frame(st = y[[1]], date = paste0(y$vuosi, "-", y$kuukausi), v = suppressWarnings(as.numeric(y[[ncol(y)]])))
  }))
  x <- x[!is.na(x$v) & x$v > 0 & x$date <= last, ]; x <- x[!duplicated(x[c("st", "date")]), ]
  ms <- sort(unique(x$date)); x$a <- x$v / vapply(ms, lam_days, 0)[match(x$date, ms)]
  w <- lapply(split(x, x$date), function(z) setNames(z$a, z$st))
  lev <- numeric(length(ms)); lev[1] <- 100
  for (i in seq_along(ms)[-1]) {
    a <- w[[ms[i]]]; b <- w[[ms[i - 1]]]; k <- intersect(names(a), names(b))
    k <- k[abs(log(a[k] / b[k])) < 0.4]            # mittauskatkot pois
    lev[i] <- lev[i - 1] * sum(a[k]) / sum(b[k])
  }
  data.frame(date = ms, value = lev)
}

# Euroopan komission AMECO (Redisstat): vuosisarja, esim. NAWRU (rakenteellinen tyottomyys), mukana ennustevuodet
AMECO_API <- "https://webgate.ec.europa.eu/ecfin/redisstat/api/dissemination/statistics/1.0/data/"
fetch_ameco <- function(s) {
  x <- fromJSON(get_text(paste0(AMECO_API, s$ds, "?lang=EN&ref_area=", if (is.null(s$geo)) "FIN" else s$geo, "&item=", s$item)), simplifyVector = FALSE)
  tp <- names(x$dimension$TIME_PERIOD$category$index); ix <- unlist(x$dimension$TIME_PERIOD$category$index)
  v <- vapply(seq_along(tp), function(j) { z <- x$value[[as.character(ix[j])]]; if (is.null(z)) NA_real_ else as.numeric(z) }, 0)
  d <- data.frame(date = tp, value = v); d <- d[!is.na(d$value), ]; d[order(d$date), ]
}
# vertailutaso trendin tilalle (bench): vuosiarvot vuoden puolivaliin ja lineaarinen interpolointi kuukausille
bench_path <- function(s, dates) {
  b <- fetch_series(c(s$bench, list(id = paste0(s$id, "__bench"))))
  approx(as.numeric(b$date) + 0.5, b$value, xout = as.integer(substr(dates, 1, 4)) + (as.integer(substr(dates, 6, 7)) - 0.5) / 12, rule = 2)$y
}

# summa useasta sarjasta (esim. sosiaaliturvarahastot = tyoelakelaitokset + muut; nettovelka = velat - varat)
fetch_sum <- function(s) {
  ds <- lapply(seq_along(s$parts), function(j) { d <- fetch_series(c(s$parts[[j]], list(id = paste0(s$id, "_p", j))))
    if (isTRUE(s$parts[[j]]$neg)) d$value <- -d$value; d })                    # neg: vahennettava osa (esim. varat nettovelassa)
  Reduce(function(a, b) { m <- merge(a, b, by = "date"); data.frame(date = m$date, value = m$value.x + m$value.y) }, ds)
}

# julkisyhteisojen velkasuhteen muutoksen hajotelma (4 nelj. liukuvat summat, muutos vuodentakaisesta, %-yks.):
# d = D / Y, dd = d_t - d_t-4 = -(B9 + INT) / Y + INT / Y - g / (1 + g) * d_t-4 + muut
# D = EDP-velka (sulautettu), Y = nimellinen BKT, B9 = rahoitusjaama, INT = korkomenot (pl. julkisyhteisojen valiset)
.dd_memo <- new.env()
fetch_jt_decomp <- function(s) {
  if (is.null(.dd_memo$x)) {
    g <- function(id, tb, pick) fetch_series(list(id = id, source = "statfin", table = tb, pick = pick))
    jt <- function(code) list(taloustoimi_1_20180101 = code, sektoriluokitus_7_20230101 = "S13", contentscode = "jtume-TASM")
    D <- g("jt_dd_velka", "jyev/11yv.px", list(sektoriluokitus_7_20230101 = "S13_C", varojenluokitus_5_20180101 = "F2TF4", contentscode = "jyev-K"))
    Y <- g("jt_dd_bkt", "ntp/132h.px", list(taloustoimi_1_20180101 = "B1GMH", contentscode = "tasmcp"))
    B <- g("jt_dd_b9", "jtume/11zf.px", jt("B9")); I1 <- g("jt_dd_d41", "jtume/11zf.px", jt("D41K")); I2 <- g("jt_dd_d41s13", "jtume/11zf.px", jt("D411KS13"))
    q <- sort(Reduce(intersect, list(D$date, Y$date, B$date, I1$date, I2$date)))
    v <- function(d) d$value[match(q, d$date)]; r4 <- function(x) as.numeric(stats::filter(x, rep(1, 4), sides = 1)); L4 <- function(x) c(rep(NA, 4), head(x, -4))
    Dv <- v(D); Y4 <- r4(v(Y)); B4 <- r4(v(B)); I4 <- r4(v(I1) - v(I2))
    d <- 100 * Dv / Y4; gr <- Y4 / L4(Y4) - 1
    x <- data.frame(date = q, ratio = d, dd = d - L4(d), pb = -100 * (B4 + I4) / Y4, int = 100 * I4 / Y4, growth = -gr / (1 + gr) * L4(d), irate = 100 * I4 / L4(Dv))
    x$sfa <- x$dd - x$pb - x$int - x$growth   # virta-varantokorjaus
    .dd_memo$x <- x }
  x <- .dd_memo$x; data.frame(date = x$date, value = x[[s$part]])
}

# julkisten kulutusmenojen (P3, S13, kayvin hinnoin) vuosimuutoksen hajotelma menolajeittain, 4 nelj. liukuvat summat, %-yks.:
# P3 = palkansaajakorvaukset + valituotekaytto + kiinteän paaoman kuluminen + ostopalvelut (D632) - myyntitulot (P11 + P131 + P12) + muut (tuotantoverot ym.)
.jk_memo <- new.env()
fetch_jk_decomp <- function(s) {
  if (is.null(.jk_memo$x)) {
    g <- function(code) fetch_series(list(id = paste0("jk_", code), source = "statfin", table = "jtume/11zf.px",
                                          pick = list(taloustoimi_1_20180101 = code, sektoriluokitus_7_20230101 = "S13", contentscode = "jtume-TASM")))
    codes <- c("P3K", "D1K", "P2K", "P51CR", "D632K", "P11R", "P131R", "P12R"); X <- lapply(codes, g); names(X) <- codes
    q <- sort(Reduce(intersect, lapply(X, function(d) d$date)))
    r4 <- function(x) as.numeric(stats::filter(x, rep(1, 4), sides = 1)); L4 <- function(x) c(rep(NA, 4), head(x, -4))
    v <- lapply(X, function(d) r4(d$value[match(q, d$date)]))
    T0 <- L4(v$P3K); ct <- function(x) 100 * (x - L4(x)) / T0
    x <- data.frame(date = q, tot = 100 * (v$P3K / T0 - 1), d1 = ct(v$D1K), p2 = ct(v$P2K), p51c = ct(v$P51CR), d632 = ct(v$D632K),
                    sales = -ct(v$P11R + v$P131R + v$P12R))
    x$muu <- x$tot - x$d1 - x$p2 - x$p51c - x$d632 - x$sales
    .jk_memo$x <- x }
  x <- .jk_memo$x; data.frame(date = x$date, value = x[[s$part]])
}

fetch_series <- function(s) {
  f <- switch(s$source, statfin = fetch_statfin, splice = fetch_splice, eurostat = fetch_eurostat, hicp_ctrb = fetch_hicp_ctrb, ntp_ctrb = fetch_ntp_ctrb, hhi_ctrb = fetch_hhi_ctrb, khi_ctrb = fetch_khi_ctrb, xlsx = fetch_xlsx, stoxx = fetch_stoxx, lam = fetch_lam, ameco = fetch_ameco, vk = fetch_vk, sum = fetch_sum, jt_decomp = fetch_jt_decomp, jk_decomp = fetch_jk_decomp, gdp_np = fetch_gdp_np,
              ecb = fetch_ecb, fred = fetch_fred, bof = fetch_bof, wb = fetch_wb, stop("Tuntematon lahde ", s$source))
  # valimuisti: onnistunut haku tallennetaan, epaonnistuessa kaytetaan edellista hakua
  cache <- file.path(CACHE_DIR, paste0(s$id, ".csv"))
  d <- tryCatch({
    x <- f(s); dir.create(CACHE_DIR, showWarnings = FALSE)
    write.csv(x, cache, row.names = FALSE); writeLines(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste0(cache, ".t")); x
  }, error = function(e) {
    if (!file.exists(cache)) stop("Haku epaonnistui eika valimuistia ole: ", s$id, " (", conditionMessage(e), ")")
    # valimuistin ika haun aikaleimasta (tiedoston mtime voi muuttua valimuistin palautuksessa)
    t0 <- if (file.exists(paste0(cache, ".t"))) as.POSIXct(readLines(paste0(cache, ".t"))[1]) else file.mtime(cache)
    message("  VAROITUS ", s$id, ": lahde ei vastannut, kaytetaan valimuistia (", format(t0, "%d.%m.%Y"), "): ", conditionMessage(e))
    STALE <<- c(STALE, s$id)
    if (difftime(Sys.time(), t0, units = "days") > STALE_DAYS) STALE_OLD <<- c(STALE_OLD, s$id)
    read.csv(cache, colClasses = c("character", "numeric"))
  })
  if (isTRUE(s$ytd12)) {                           # kumulatiivinen vuoden alusta -> 12 kk liukuva summa: x_m + x_joulu(v-1) - x_m(v-1)
    d <- d[order(d$date), ]; y <- substr(d$date, 1, 4); mo <- substr(d$date, 6, 7)
    dec <- d$value[match(paste0(as.integer(y) - 1, "-12"), d$date)]; prev <- d$value[match(paste0(as.integer(y) - 1, "-", mo), d$date)]
    d$value <- ifelse(mo == "12", d$value, d$value + dec - prev) }
  if (isTRUE(s$sahm)) {                            # Sahmin saanto: 3 kk keskiarvo miinus sen minimi edeltavalta 12 kk:lta (%-yks.)
    d <- d[order(d$date), ]; m3 <- as.numeric(stats::filter(d$value, rep(1 / 3, 3), sides = 1))
    mn <- vapply(seq_along(m3), function(i) if (i <= 12) NA_real_ else suppressWarnings(min(m3[(i - 12):(i - 1)], na.rm = TRUE)), 0)
    d$value <- m3 - ifelse(is.finite(mn), mn, NA) }
  if (isTRUE(s$to_quarter) || identical(s$to_quarter, "sum") || identical(s$to_quarter, "last")) {   # kuukausisarja -> neljannes (keskiarvo tai "sum"), vain taydet neljannekset
    q <- sprintf("%s-Q%d", substr(d$date, 1, 4), (as.integer(substr(d$date, 6, 7)) - 1) %/% 3 + 1)
    agg <- if (identical(s$to_quarter, "sum")) sum else if (identical(s$to_quarter, "last")) function(v) v[3] else mean
    a <- tapply(d$value, q, function(v) if (sum(!is.na(v)) == 3) agg(v) else NA)
    d <- data.frame(date = names(a), value = as.numeric(a))
  }
  if (isTRUE(s$q_interp)) {                       # neljannesvuosikysely kuukausittaiseksi: arvo kyselykuukaudelle
    d <- d[!is.na(d$value), ]                       # (neljanneksen 1. kk), valikuukaudet lineaarisesti
    q <- as.integer(substr(d$date, nchar(d$date), nchar(d$date)))
    mi <- as.integer(substr(d$date, 1, 4)) * 12 + 3 * (q - 1)
    full <- min(mi):max(mi)
    d <- data.frame(date = sprintf("%d-%02d", full %/% 12, full %% 12 + 1), value = approx(mi, d$value, full)$y)
  }
  if (!is.null(s$shift_q)) {                       # neljannesten siirto (esim. luotonantokysely: kierros koskee edellista neljannesta)
    qi <- as.integer(substr(d$date, 1, 4)) * 4 + as.integer(substr(d$date, 7, 7)) - 1 + s$shift_q
    d$date <- sprintf("%d-Q%d", qi %/% 4, qi %% 4 + 1) }
  if (!is.null(s$ma)) {                            # liukuva keskiarvo (jakson omassa frekvenssissa, ennen kuukausinakymaan siirtoa)
    d <- d[order(d$date), ]; d$value <- as.numeric(stats::filter(d$value, rep(1 / s$ma, s$ma), sides = 1)) }
  if (isTRUE(s$q_to_m)) {                         # neljannesvuosisarja kuukausinakymaan: neljanneksen viimeinen kuukausi
    d$date <- sprintf("%s-%02d", substr(d$date, 1, 4), 3 * as.integer(substr(d$date, 7, 7)))
  }
  if (!is.null(s$roll)) {                          # liukuva summa (esim. 12 kk konkurssit), vain taydet ikkunat
    d <- d[order(d$date), ]; d$value <- as.numeric(stats::filter(d$value, rep(1, s$roll), sides = 1))
  }
  if (isTRUE(s$stl)) {                             # kausitasoitus stl:lla (alkuperainen sarja, ei julkaistua kausitasoitettua)
    d <- d[order(d$date), ]; ok <- which(!is.na(d$value)); d <- d[min(ok):max(ok), ]
    f <- if (grepl("-Q", d$date[1])) 4 else 12
    st <- c(as.integer(substr(d$date[1], 1, 4)), if (f == 4) as.integer(substr(d$date[1], 7, 7)) else as.integer(substr(d$date[1], 6, 7)))
    x <- approx(seq_along(d$value), d$value, seq_along(d$value))$y
    fit <- stl(ts(x, start = st, frequency = f), s.window = "periodic", robust = TRUE)
    d$value <- x - as.numeric(fit$time.series[, "seasonal"]) }
  if (!is.null(s$div)) {                           # suhde toiseen sarjaan: div_mult * x / y (esim. % BKT:stä, deflaattori, tuottavuus)
    dv <- if (!is.null(s$div$source)) list(s$div) else s$div   # useampi jakaja peräkkäin (esim. vaihtosuhde)
    for (j in seq_along(dv)) {
      e <- fetch_series(c(dv[[j]], list(id = paste0(s$id, "_div", j))))
      m <- merge(d, e, by = "date"); d <- data.frame(date = m$date, value = m$value.x / m$value.y) }
    d$value <- d$value * (if (is.null(s$div_mult)) 100 else s$div_mult) }
  if (!is.null(s$vac_emp)) {                       # avoimien tyopaikkojen aste: V / (V + tyolliset) * 100
    e <- fetch_series(c(s$vac_emp, list(id = paste0(s$id, "_emp"))))
    m <- merge(d, e, by = "date")
    d <- data.frame(date = m$date, value = 100 * m$value.x / (m$value.x + m$value.y)) }
  if (!is.null(s$per)) {                           # suhde toiseen sarjaan (esim. avoimet tyopaikat / tyottomat)
    e <- fetch_series(c(s$per, list(id = paste0(s$id, "_per"))))
    m <- merge(d, e, by = "date")
    d <- data.frame(date = m$date, value = m$value.x / m$value.y) }
  if (!is.null(s$mult)) d$value <- d$value * s$mult  # yksikonmuunnos (esim. milj. -> mrd. euroa)
  if (!is.null(s$pct_lag)) {                        # muutos %, edellisesta vuodesta (lag 4 nelj. / 12 kk)
    d <- d[order(d$date), ]; n <- nrow(d); L <- s$pct_lag
    d$value <- c(rep(NA, L), 100 * (d$value[(L + 1):n] / d$value[1:(n - L)] - 1)) }
  if (!is.null(s$rebase)) {                         # perusvuosi = 100 (vuoden havaintojen keskiarvo)
    b <- mean(d$value[substr(d$date, 1, 4) == as.character(s$rebase)], na.rm = TRUE)
    if (!is.finite(b)) stop(s$id, ": perusvuodelta ", s$rebase, " ei havaintoja")
    d$value <- 100 * d$value / b }
  if (!is.null(s$deflate)) {                       # nimellinen sarja -> reaalinen (hintaindeksilla)
    p <- fetch_series(c(s$deflate, list(id = paste0(s$id, "_defl"))))
    m <- merge(d, p, by = "date")
    d <- data.frame(date = m$date, value = m$value.x / m$value.y * 100)
  }
  d <- d[order(d$date), ]
  if (isTRUE(s$daily)) return(d[!is.na(d$value), ])   # paivadata: pyhapaivat pois, ei interpolointia
  # pois alku- ja loppupaan puuttuvat, sisalla interpoloidaan
  ok <- which(!is.na(d$value))
  d <- d[min(ok):max(ok), ]
  if (anyNA(d$value)) {
    message("  HUOM ", s$id, ": ", sum(is.na(d$value)), " puuttuvaa interpoloitu")
    d$value <- approx(seq_along(d$value), d$value, seq_along(d$value))$y
  }
  d
}

# ---------------------------------------------------------------------
# Esikasittely: trend (valmis), none, sa_trend (stl)
# ---------------------------------------------------------------------
preprocess <- function(d, handling, freq) {
  if (handling %in% c("trend", "none")) return(d)
  if (handling == "sa") {                          # kausitasoitus: stl:n kausikomponentti pois, satunnaisvaihtelu jaa
    f <- if (freq == "M") 12 else 4
    st <- c(as.integer(substr(d$date[1], 1, 4)), per_idx(d$date[1], freq) %% f + 1)
    fit <- stl(ts(d$value, start = st, frequency = f), s.window = "periodic", robust = TRUE)
    d$value <- d$value - as.numeric(fit$time.series[, "seasonal"]); return(d)
  }
  if (handling == "sa_trend") {
    f <- if (freq == "M") 12 else 4
    st <- c(as.integer(substr(d$date[1], 1, 4)), per_idx(d$date[1], freq) %% f + 1)
    fit <- stl(ts(d$value, start = st, frequency = f), s.window = "periodic", robust = TRUE)
    d$value <- as.numeric(fit$time.series[, "trend"]); return(d)
  }
  stop("Tuntematon handling ", handling)
}

# ---------------------------------------------------------------------
# Kello: taso = standardoitu HP-poikkeama, muutos = tason muutos skaalattuna
# ---------------------------------------------------------------------
hp_trend <- function(y, lambda) {
  n <- length(y); D <- diff(diag(n), differences = 2)
  as.numeric(solve(diag(n) + lambda * crossprod(D), y))
}

# jatketaan sarjaa AR-ennusteella: volyymit erotuksista (kasvuvauhti jatkuu), muut tasoista (palautuu keskiarvoon)
ext_ar <- function(y, h, freq, kind) {
  om <- if (freq == "M") 12 else 4
  if (kind == "volume") { f <- ar(diff(y), order.max = om, aic = TRUE, method = "yule-walker")
    return(c(y, y[length(y)] + cumsum(as.numeric(predict(f, n.ahead = h)$pred)))) }
  f <- ar(y, order.max = om, aic = TRUE, method = "yule-walker")
  c(y, as.numeric(predict(f, n.ahead = h)$pred))
}

clock_series <- function(d, s, freq) {
  y <- if (s$kind == "volume") 100 * log(d$value) else d$value
  n <- length(y); ye <- ext_ar(y, EXT_H[[freq]], freq, s$kind)
  lam <- if (is.null(s$lambda)) LAMBDA[[freq]] else s$lambda          # tyomarkkina-asteille jaykempi trendi
  sm <- if (isTRUE(s$nosmooth)) y else hp_trend(ye, LAMBDA_S[[freq]])[1:n]   # kohinasuodin (ei valmiille trendisarjoille)
  # volume/balance: poikkeama pitkan aikavalin trendista (HP-kaistanpaasto)
  # level (korot, stressi, epavarmuus): taso suhteessa omaan historiaan, ei trendia
  tr  <- if (s$kind == "level") rep(NA_real_, n) else if (!is.null(s$bench)) bench_path(s, d$date) else hp_trend(ye, lam)[1:n]   # bench: esim. NAWRU
  gap <- if (s$kind == "level") sm else sm - tr
  keep <- as.integer(substr(d$date, 1, 4)) >= Z_FROM
  lvl <- (gap - mean(gap[keep])) / sd(gap[keep])
  if (isTRUE(s$invert)) lvl <- -lvl
  # muutos: volyymeilla poikkeaman muutos; saldoluvuilla ja asteilla (ei kasvutrendia) tasoitetun sarjan oma muutos,
  # jotta trendin liike ei nay muutoksena (paikallaan pysyva aste = nollamuutos)
  kl  <- if (is.null(s$dlag)) 1 else s$dlag                     # dlag: muutos k kuukauden keskiarvona (pyoristetyt trendisarjat)
  dl  <- if (s$kind == "balance") (if (isTRUE(s$invert)) -1 else 1) * c(rep(NA, kl), diff(sm, lag = kl)) / kl else c(NA, diff(lvl))
  chg <- dl / sd(dl[keep], na.rm = TRUE)          # ei keskistysta: etumerkki sailyy
  # trendi sarjan omassa yksikossa (volyymit log-muodosta takaisin), naytetaan julkaistun arvon kuvassa
  trend <- if (s$kind == "volume") exp(tr / 100) else tr
  data.frame(date = d$date, level = lvl, change = chg, raw = d$value, trend = trend)
}

# ---------------------------------------------------------------------
# Aja
# ---------------------------------------------------------------------
cfg  <- fromJSON(CFG_PATH, simplifyVector = FALSE)
r3   <- function(x) ifelse(is.na(x), NA, round(x, 3))

sets_out <- list()
# ---------------------------------------------------------------------
# Tilannetaulukko: viimeisin arvo ja muutos 1 pv, 1 vk ja vuoden alusta
# chg: "kp" (korot, korkopisteina), "pct" (kurssit ja hinnat, %), "abs" (indeksit)
# kuukausisarjoille vain muutos vuoden alusta (edellisen vuoden joulukuusta)
# ---------------------------------------------------------------------
TABLE_START <- format(Sys.Date() - 430, "%Y-%m-%d")    # paivasarjoista haetaan vain runsas vuosi
# valinnaiset sarjat (optional): jos lahde ei vastaa eika valimuistia ole, sarja jatetaan pois (kuvio piirretaan ilman sita)
for (k in rev(seq_along(cfg$series))) { s <- cfg$series[[k]]
  if (isTRUE(s$optional) && !isFALSE(s$enabled) && !file.exists(file.path(CACHE_DIR, paste0(s$id, ".csv")))) {
    ok <- tryCatch({ fetch_series(s); TRUE }, error = function(e) { message("  VAROITUS ", s$id, " ohitetaan: ", conditionMessage(e)); FALSE })
    if (!ok) { cfg$series[[k]] <- NULL; CHECKS <- c(CHECKS, paste("valinnainen sarja puuttuu:", s$id)) } } }
ser_cfg <- function(id) { x <- Filter(function(s) identical(s$id, id), cfg$series); if (!length(x)) stop("Sarjaa ei loydy: ", id); x[[1]] }
.tbl_memo <- new.env()
get_any <- function(id, raws) {
  if (!is.null(raws[[id]])) return(raws[[id]])
  if (!is.null(.tbl_memo[[id]])) return(.tbl_memo[[id]])
  .tbl_memo[[id]] <- get_any_(id, raws); .tbl_memo[[id]]
}
get_any_ <- function(id, raws) {
  s <- ser_cfg(id)
  if (identical(s$source, "spread")) { a <- get_any(s$a, raws); b <- get_any(s$b, raws); m <- merge(a, b, by = "date")
    return(data.frame(date = m$date, value = m$value.x - m$value.y)) }
  message("  taulukko ", id); fetch_series(s)
}
snap_row <- function(id, raws) {
  s <- ser_cfg(id); d <- get_any(id, raws); d <- d[!is.na(d$value), ]; d <- d[order(d$date), ]
  n <- nrow(d); last <- d$value[n]; ld <- d$date[n]; daily <- nchar(ld) == 10
  chg <- if (is.null(s$chg)) "abs" else s$chg
  dif <- function(ref) if (!length(ref) || is.na(ref)) NULL else switch(chg, kp = 100 * (last - ref), pct = 100 * (last / ref - 1), last - ref)
  at_or_before <- function(k) if (length(k)) d$value[max(k)] else NA
  ytd_ref <- at_or_before(which(substr(d$date, 1, 4) < substr(ld, 1, 4)))
  d1 <- if (daily && n > 1) dif(d$value[n - 1]) else NULL
  w1 <- if (daily) dif(at_or_before(which(as.Date(d$date) <= as.Date(ld) - 7))) else NULL
  m1 <- if (!daily && n > 1) dif(d$value[n - 1]) else NULL                       # kuukausisarjat: edellinen kuukausi
  y1 <- if (!daily) dif(d$value[match(sprintf("%d-%s", as.integer(substr(ld, 1, 4)) - 1, substr(ld, 6, 7)), d$date)]) else NULL
  if (daily && as.Date(ld) < Sys.Date() - 10) CHECKS <<- c(CHECKS, sprintf("%s: taulukon viimeisin havainto %s", id, ld))
  list(label = if (is.null(s$short)) s$label else s$short, date = ld, last = round(last, 4), chg = chg,
       unit = s$unit, dec = s$dec, d1 = d1, w1 = w1, m1 = m1, y1 = y1, ytd = dif(ytd_ref))
}
# tuottokayra: viimeisin, kuukausi sitten ja edellisen vuoden lopussa
curve_data <- function(c) {
  m <- lapply(c$maturities, function(k) { s <- list(id = paste0("kayra_", k), source = "ecb", flow = c$flow, key = sprintf(c$key, k),
    daily = TRUE, start = TABLE_START); message("  kayra ", k); x <- fetch_series(s); setNames(x, c("date", k)) })
  w <- Reduce(function(a, b) merge(a, b, by = "date"), m); w <- w[order(w$date), ]; w <- w[complete.cases(w), ]
  ld <- w$date[nrow(w)]
  pick <- function(k) if (length(k)) w[max(k), ] else NULL
  ref <- list(list(label = "Viimeisin", row = w[nrow(w), ]),
              list(label = "Kuukausi sitten", row = pick(which(as.Date(w$date) <= seq(as.Date(ld), by = "-1 month", length.out = 2)[2]))),
              list(label = "Vuoden lopussa", row = pick(which(substr(w$date, 1, 4) < substr(ld, 1, 4)))))
  list(maturities = unlist(c$maturities), labels = unlist(c$labels),
       lines = lapply(Filter(function(r) !is.null(r$row), ref), function(r)
         list(label = r$label, date = r$row$date, values = round(unname(unlist(r$row[unlist(c$maturities)])), 4))))
}

for (set in cfg$sets) {
  freq <- set$freq
  members <- Filter(function(s) identical(s$set, set$id) && !isFALSE(s$enabled) && !isTRUE(s$tile) && !isTRUE(s$contrib) && is.null(s$decomp) && !isTRUE(s$validation) && !isTRUE(s$chart_only) && !isTRUE(s$table_only), cfg$series)
  # markkinaruudut: paivittaiset sarjat, viimeiset TILE_N havaintoa
  tiles <- lapply(Filter(function(s) identical(s$set, set$id) && isTRUE(s$tile) && !isFALSE(s$enabled), cfg$series), function(s) {
    message("  ruutu ", s$id); d <- tail(fetch_series(s), TILE_N + 1)
    list(id = s$id, label = s$label, unit = s$unit, dec = s$dec, abs = isTRUE(s$abs), dates = d$date, values = round(d$value, 4))
  })
  message("Setti ", set$id, " (", length(members), " sarjaa)")

  # 1) haetaan lahdesarjat, 2) johdetut erotukset (esim. korkokayra = 10 v - 3 kk)
  raws <- list()
  for (s in members) if (s$source != "spread") { message("  ", s$id); raws[[s$id]] <- fetch_series(s) }
  for (s in members) if (s$source == "spread") {
    m <- merge(raws[[s$a]], raws[[s$b]], by = "date")
    raws[[s$id]] <- data.frame(date = m$date, value = m$value.x - m$value.y)
  }
  # kuvionakyma (esim. rahoitusmarkkinat): sarjat alkuperaisina arvoina, ei kelloa
  if (identical(set$view, "charts")) {
    from_y  <- if (!is.null(set$from)) set$from else Z_FROM
    last_i  <- max(vapply(raws, function(r) max(per_idx(r$date, freq)), numeric(1)))
    start_i <- per_idx(if (freq == "M") sprintf("%d-01", from_y) else sprintf("%d-Q1", from_y), freq)
    periods <- idx_per(start_i:last_i, freq)
    # tilannetaulukot (type = "table") ja tuottokayra (type = "curve") lasketaan valmiiksi
    charts_out <- lapply(set$charts, function(c) {
      if (identical(c$type, "table")) c$rows <- lapply(c$rows, function(r) c(r, snap_row(r$id, raws)))
      if (identical(c$type, "curve")) c$curve <- curve_data(c)
      c })
    # taulukkorivien historia kuukausisarjoina (paivadata: kuukauden viimeinen havainto) rivin valintaa varten
    tbl_ids <- unique(unlist(lapply(Filter(function(c) identical(c$type, "table"), set$charts), function(c) vapply(c$rows, function(r) r$id, ""))))
    hist <- lapply(tbl_ids, function(id) {
      d <- get_any(id, raws); d <- d[!is.na(d$value), ]
      if (all(nchar(d$date) == 10)) { d <- d[order(d$date), ]; m <- substr(d$date, 1, 7)
        d <- data.frame(date = unique(m), value = tapply(d$value, m, function(v) tail(v, 1))[unique(m)]) }
      list(id = paste0("h_", id), label = ser_cfg(id)$label, last = d$date[nrow(d)], values = r3(d$value[match(periods, d$date)])) })
    sets_out[[length(sets_out) + 1]] <- list(
      id = set$id, label = set$label, freq = freq, default = isTRUE(set$default), view = "charts",
      from = from_y, periods = periods, charts = charts_out, colors = set$colors, tiles = unname(tiles), summary = set$summary, kpis = set$kpis,
      series = c(unname(lapply(members, function(s) { r <- raws[[s$id]]
        list(id = s$id, label = s$label, last = r$date[nrow(r)], values = r3(r$value[match(periods, r$date)])) })), hist))
    next
  }
  # kasvunakyma (kansantalous): q/q- ja v/v-muutokset prosentteina, ei kelloa
  if (identical(set$view, "growth")) {
    from_y  <- if (!is.null(set$from)) set$from else 2015
    ser <- list(); rows <- list(); lag <- if (freq == "Q") 4 else 12
    for (s in members) {
      r <- preprocess(raws[[s$id]], s$handling, freq); v <- r$value; n <- length(v)
      raws[[paste0(s$id, "_qq")]] <- data.frame(date = r$date, value = 100 * (v / c(NA, v[-n]) - 1))
      # v/v kuten virallisissa tilastoissa: tyopaivakorjatusta (yy_s_adj) tai alkuperaisesta (yy_raw) sarjasta
      ry <- if (!is.null(s$yy_s_adj)) { s2 <- s; s2$id <- paste0(s$id, "_", tolower(s$yy_s_adj)); s2$pick$s_adj <- s$yy_s_adj; fetch_series(s2) }
            else if (!is.null(s$yy_pick)) { s2 <- s; s2$id <- paste0(s$id, "_yyp"); s2$pick <- modifyList(s$pick, s$yy_pick); fetch_series(s2) }
            else if (isTRUE(s$yy_raw)) raws[[s$id]] else r
      vy <- ry$value; ny <- length(vy)
      raws[[paste0(s$id, "_yy")]] <- data.frame(date = ry$date, value = 100 * (vy / c(rep(NA, lag), vy[1:(ny - lag)]) - 1))
      ser[[length(ser) + 1]] <- list(id = paste0(s$id, "_qq"), label = paste(s$label, "q/q"))
      ser[[length(ser) + 1]] <- list(id = paste0(s$id, "_yy"), label = paste(s$label, "v/v"))
      # in_table = false: sarja vain kuvioihin (esim. reaaliansiot ansiotason kuviossa), ei taulukkoon eika kelloon
      if (!isFALSE(s$in_table))
        rows[[length(rows) + 1]] <- list(id = s$id, label = s$label, group = s$group, neutral = isTRUE(s$neutral), invert = isTRUE(s$invert), sub = isTRUE(s$sub), gap_after = isTRUE(s$gap_after))
    }
    # kontribuutioryhmat (esim. BKT:n kysyntaerat, inflaation hyodykeryhmat), %-yks.;
    # jaannos = kokonaismuutos - erien summa, jos kokonaissarjalla on residual-nimi
    cmem <- Filter(function(s) identical(s$set, set$id) && isTRUE(s$contrib) && !isFALSE(s$enabled), cfg$series)
    grp_of <- function(s) if (is.null(s$contrib_group)) "bkt" else s$contrib_group
    for (s in cmem) { message("  ", s$id); raws[[s$id]] <- fetch_series(s); ser[[length(ser) + 1]] <- list(id = s$id, label = s$label) }
    # chart_only: tasosarjat rivikohtaisiin kuvioihin (esim. asuntojen hinnat ja kaupat alueittain), ei taulukkoon eika kelloon
    xmem <- Filter(function(s) identical(s$set, set$id) && isTRUE(s$chart_only) && !isFALSE(s$enabled), cfg$series)
    for (s in xmem) { message("  ", s$id); raws[[s$id]] <- fetch_series(s); ser[[length(ser) + 1]] <- list(id = s$id, label = s$label) }
    for (g in unique(vapply(cmem, grp_of, ""))) {
      gm <- Filter(function(s) grp_of(s) == g, cmem)
      totS <- Filter(function(s) isTRUE(s$contrib_total), gm)[[1]]
      if (is.null(totS$residual)) next
      parts <- vapply(Filter(function(s) !isTRUE(s$contrib_total), gm), function(s) s$id, "")
      m <- Reduce(function(a, b) merge(a, b, by = "date"), lapply(c(totS$id, parts), function(id) setNames(raws[[id]], c("date", id))))
      rid <- if (g == "bkt") "k_muu" else paste0("k_muu_", g)
      raws[[rid]] <- data.frame(date = m$date, value = m[[totS$id]] - rowSums(m[parts]))
      ser[[length(ser) + 1]] <- list(id = rid, label = totS$residual)
    }
    # tyottomien maaran vuosimuutoksen hajotelma (tyovoimatutkimus), osuudet vaestosta:
    # U = P*a - P*e (a = (E+U)/P tyovoimaosuus, e = E/P tyollisyysaste)
    # => dU = dP*U0/P0 + P1*da - P1*de, summautuu tarkasti julkaistuilla (pyoristetyilla) luvuilla
    dmem <- Filter(function(s) identical(s$set, set$id) && !is.null(s$decomp) && !isFALSE(s$enabled), cfg$series)
    if (length(dmem)) {
      dd <- lapply(dmem, function(s) { message("  ", s$id); setNames(fetch_series(s), c("date", s$decomp)) })
      m <- Reduce(function(a, b) merge(a, b, by = "date"), dd); m <- m[order(m$date), ]
      j <- match(idx_per(per_idx(m$date, freq) - lag, freq), m$date)
      P0 <- m$P[j]; P1 <- m$P
      dec <- list(tu_d      = m$U - m$U[j],
                  tu_vaesto = (P1 - P0) * m$U[j] / P0,
                  tu_osall  = P1 * ((m$E + m$U) / P1 - (m$E[j] + m$U[j]) / P0),
                  tu_tyoll  = -P1 * (m$E / P1 - m$E[j] / P0))
      labs <- c(tu_d = "Työttömien määrän muutos", tu_vaesto = "Väestönkasvu",
                tu_osall = "Työvoimaosuus", tu_tyoll = "Työllisyysaste")
      for (k in names(dec)) {
        raws[[k]] <- data.frame(date = m$date, value = dec[[k]])
        ser[[length(ser) + 1]] <- list(id = k, label = labs[[k]])
      }
    }
    last_i  <- max(vapply(members, function(s) max(per_idx(raws[[s$id]]$date, freq)), numeric(1)))
    start_i <- per_idx(if (freq == "M") sprintf("%d-01", from_y) else sprintf("%d-Q1", from_y), freq)
    periods <- idx_per(start_i:last_i, freq)
    fc <- set$forecast                             # BKT-ennusteet (series.json, kasin): toteutunut vuosikasvu alkuperaisesta sarjasta
    if (!is.null(fc)) {
      d <- fetch_series(list(id = "bkt_vuosi", source = "statfin", table = "ntp/132h.px",
                             pick = list(taloustoimi_1_20180101 = "B1GMH", contentscode = "tasmvv2015")))
      ys <- tapply(d$value, substr(d$date, 1, 4), function(v) if (sum(!is.na(v)) == 4) sum(v) else NA)
      ys <- ys[!is.na(ys)]; g <- 100 * (ys[-1] / ys[-length(ys)] - 1)
      g <- g[as.integer(names(g)) >= fc$from]
      fc$actual <- list(years = as.integer(names(g)), values = round(unname(as.numeric(g)), 2))
      nd <- max(as.Date(vapply(fc$rows, function(r) r$date, "")))
      if (as.numeric(Sys.Date() - nd) > 60) CHECKS <- c(CHECKS, sprintf("BKT-ennusteet: uusin ennuste %s, päivitä series.json", format(nd, "%d.%m.%Y")))
    }
    sets_out[[length(sets_out) + 1]] <- list(
      id = set$id, label = set$label, freq = freq, default = isTRUE(set$default), view = "charts",
      from = from_y, periods = periods, charts = set$charts, colors = set$colors, forecast = fc,
      growth = unname(rows), tabs = set$tabs, kpis = set$kpis,   # kasvunakyma: taulukko ja kuviot (ei karttaa), alavalilehdet
      series = unname(lapply(ser, function(x) { r <- raws[[x$id]]
        list(id = x$id, label = x$label, last = r$date[max(which(!is.na(r$value)))], values = r3(r$value[match(periods, r$date)])) })))
    next
  }
  clock_members <- Filter(function(s) !isFALSE(s$in_clock), members)
  res <- lapply(clock_members, function(s)
    clock_series(preprocess(raws[[s$id]], s$handling, freq), s, freq))

  last_all  <- vapply(res, function(r) max(per_idx(r$date, freq)), numeric(1))
  start_i   <- per_idx(if (freq == "M") sprintf("%d-01", Z_FROM) else sprintf("%d-Q1", Z_FROM), freq)
  periods_i <- start_i:max(last_all)
  periods   <- idx_per(periods_i, freq)

  align <- function(r, col) { v <- r[[col]][match(periods, r$date)]; r3(v) }

  inds <- Map(function(s, r) {
    lp <- r$date[nrow(r)]; rw <- raws[[s$id]]            # julkaistu arvo ennen esikasittelya
    list(id = s$id, label = s$label, list = if (is.null(s$list)) s$label else s$list,   # list: lyhyt nimi ryhman sisalla
         map = s$kname,                                                          # map: kartan lyhyt nimi useaa valittaessa
         group = s$group, kind = s$kind, unit = s$unit, raw_desc = s$raw_desc,
         invert = isTRUE(s$invert), in_composite = !isFALSE(s$in_composite), trend_label = s$trend_label, trend_note = s$trend_note,
         last = lp, last_raw = round(r$raw[nrow(r)], 2),
         level = align(r, "level"), change = align(r, "change"), raw = r3((if (is.null(s$raw_scale)) 1 else s$raw_scale) * rw$value[match(periods, rw$date)]),
         trend = r3((if (is.null(s$raw_scale)) 1 else s$raw_scale) * r$trend[match(periods, r$date)]),
         # kausitasoitettu taso omassa yksikossa (STL), jos lahde on kausitasoittamaton
         sa = if (s$handling %in% c("sa", "sa_trend")) { a <- preprocess(rw, "sa", freq)
           r3((if (is.null(s$raw_scale)) 1 else s$raw_scale) * a$value[match(periods, a$date)]) } else NULL)
  }, clock_members, res)

  # kokonaisindikaattori: keskiarvo, vain jaksot joissa kaikki mukana olevat sarjat
  comp_ids <- which(vapply(inds, function(x) x$in_composite, logical(1)))
  L <- sapply(inds[comp_ids], function(x) unlist(lapply(x$level, function(v) if (is.null(v)) NA else v)))
  comp_lvl <- rowMeans(L)                            # NA jos yksikin puuttuu
  dl <- c(NA, diff(comp_lvl))
  comp_chg <- dl / sd(dl, na.rm = TRUE)
  comp_last <- periods[max(which(!is.na(comp_lvl)))]

  # indikaattorikohtaiset osatekijakuviot, laskutapa set$contribs[[ryhma]]$calc:
  #   direct   : sarja sellaisenaan * scale (luottamusindikaattorin kysymykset)
  #   additive : summautuvat tasot (esim. kerrosala), osa = 100 * dX / T_t-12
  #   weighted : indeksit ja painot (wpick), osa = w_t-12 * dI / T_t-12; jaannos = kokonaismuutos - osat
  cmem <- Filter(function(s) identical(s$set, set$id) && isTRUE(s$contrib) && !isFALSE(s$enabled), cfg$series)
  cser <- list()
  add_c <- function(id, label, d) { ok <- which(!is.na(d$value))
    cser[[length(cser) + 1]] <<- list(id = id, label = label, last = d$date[max(ok)], values = r3(d$value[match(periods, d$date)])) }
  lagd <- function(dates) idx_per(per_idx(dates, freq) - (if (freq == "M") 12 else 4), freq)
  mav  <- function(v, k) if (k <= 1) v else as.numeric(stats::filter(v, rep(1 / k, k), sides = 1))
  for (g in unique(vapply(cmem, function(s) s$contrib_group, ""))) {
    cc <- set$contribs[[g]]; calc <- if (is.null(cc$calc)) "direct" else cc$calc
    gm <- Filter(function(s) identical(s$contrib_group, g), cmem)
    R <- lapply(gm, function(s) { message("  ", s$id); fetch_series(s) }); names(R) <- vapply(gm, function(s) s$id, "")
    if (calc == "direct") {
      for (s in gm) { d <- R[[s$id]]; d$value <- (if (is.null(s$scale)) 1 else s$scale) * d$value; add_c(s$id, s$label, d) }
      next
    }
    tot <- Filter(function(s) isTRUE(s$contrib_total), gm)[[1]]; parts <- Filter(function(s) !isTRUE(s$contrib_total), gm)
    dates <- R[[tot$id]]$date; j <- match(lagd(dates), dates)
    T1 <- R[[tot$id]]$value; T0 <- T1[j]
    val <- function(id, d = R[[id]]) d$value[match(dates, d$date)]
    if (calc == "level") {                         # tasot pinottuina (esim. konkurssit 12 kk summana), kokonaisen vuosimuutos erikseen
      add_c(tot$id, tot$label, data.frame(date = dates, value = T1))
      add_c(paste0(tot$id, "_yy"), paste(tot$label, "v/v"), data.frame(date = dates, value = 100 * (T1 / T0 - 1)))
      S <- 0
      for (s in parts) { X1 <- val(s$id); S <- S + X1; add_c(s$id, s$label, data.frame(date = dates, value = X1)) }
      if (!is.null(cc$residual)) add_c(paste0(tot$id, "_muu"), cc$residual, data.frame(date = dates, value = T1 - S))
      next
    }
    pv <- lapply(parts, function(s) { X1 <- val(s$id); X0 <- X1[j]
      if (calc == "additive") return(100 * (X1 - X0) / T0)
      w <- fetch_series(modifyList(s, list(id = paste0(s$id, "_w"), pick = modifyList(s$pick, list(contentscode = s$wpick)))))
      val(NULL, w)[j] * (X1 - X0) / T0 })
    line <- 100 * (T1 / T0 - 1); k <- if (is.null(cc$ma)) 1 else cc$ma
    add_c(tot$id, tot$label, data.frame(date = dates, value = mav(line, k)))
    for (i in seq_along(parts)) add_c(parts[[i]]$id, parts[[i]]$label, data.frame(date = dates, value = mav(pv[[i]], k)))
    if (!is.null(cc$residual)) add_c(paste0(tot$id, "_muu"), cc$residual, data.frame(date = dates, value = mav(line - Reduce(`+`, pv), k)))
  }

  # vertailu BKT:hen: BKT:n trendipoikkeama (sama menetelma, neljannesvuosittain) ja kokonaisindikaattorin
  # neljanneskeskiarvo; sarjat kuukausiakselilla neljanneksen viimeiselle kuukaudelle
  vmem <- Filter(function(s) identical(s$set, set$id) && isTRUE(s$validation), cfg$series)
  contribs <- set$contribs
  if (length(vmem) && freq == "M") {
    vs <- vmem[[1]]; message("  ", vs$id)
    g <- clock_series(fetch_series(vs), modifyList(vs, list(kind = "volume", invert = FALSE)), "Q")
    qq <- sprintf("%s-Q%d", substr(periods, 1, 4), (as.integer(substr(periods, 6, 7)) - 1) %/% 3 + 1)
    cq <- tapply(comp_lvl, qq, function(v) if (sum(!is.na(v)) == 3) mean(v) else NA)
    qm <- function(q) sprintf("%s-%02d", substr(q, 1, 4), 3 * as.integer(substr(q, 7, 7)))
    both <- intersect(names(cq)[!is.na(cq)], g$date)
    r_all <- cor(cq[both], g$level[match(both, g$date)])
    ex <- both[!substr(both, 1, 4) %in% c("2020", "2021")]
    r_ex <- cor(cq[ex], g$level[match(ex, g$date)])
    cser[[length(cser) + 1]] <- list(id = "val_comp", label = "Kokonaisindikaattori (neljänneksen ka.)", last = qm(tail(both, 1)),
      values = r3(ifelse(as.integer(substr(periods, 6, 7)) %% 3 == 0, unname(cq[qq]), NA)))
    cser[[length(cser) + 1]] <- list(id = "val_gdp", label = "BKT:n trendipoikkeama", last = qm(tail(g$date, 1)),
      values = r3(g$level[match(periods, qm(g$date))]))
    VALID_TXT <<- sprintf(   # vertailu BKT:hen menetelmakuvaukseen (kuvio poistettu sivulta)
      " Kokonaisindikaattorin neljännesvuosikeskiarvon korrelaatio BKT:n trendipoikkeamaan %s–%s on %s (ilman vuosia 2020–21 %s).",
      substr(both[1], 1, 4), substr(tail(both, 1), 1, 4),
      sub(".", ",", sprintf("%.2f", r_all), fixed = TRUE), sub(".", ",", sprintf("%.2f", r_ex), fixed = TRUE))
    message(sprintf("  Kokonaisindikaattori vs BKT: r = %.2f (ilman 2020-21 %.2f)", r_all, r_ex))
  }

  sets_out[[length(sets_out) + 1]] <- list(
    id = set$id, label = set$label, freq = freq, default = isTRUE(set$default),
    tail = TAIL_N[[freq]], groups = set$groups, lead = set$lead, periods = periods,
    composite = list(last = comp_last, level = r3(comp_lvl), change = r3(comp_chg)),
    indicators = unname(inds), series = unname(cser), contribs = contribs)
}

# ---------------------------------------------------------------------
# Lahdeluettelo generoidaan kaytossa olevista sarjoista: tuntematon koodi -> tarkistushuomio
# ---------------------------------------------------------------------
SRC_NAMES <- list(
  statfin  = list(.n = "Tilastokeskus", ntp = "kansantalouden tilinpito", jtume = "julkisyhteisöjen tulot ja menot", jyev = "julkisyhteisöjen velka", rtp = "rahoitustilinpito",
                  tyti = "työvoimatutkimus", atp = "avoimet työpaikat", tyonv = "työnvälitystilasto", ati = "ansiotasoindeksi",
                  ktps = "palkkasummakuvaajat", khi = "kuluttajahintaindeksi", kbar = "kuluttajabarometri",
                  ashi = "osakeasuntojen hinnat", asvu = "asuntojen vuokrat", raku = "rakennus- ja asuntotuotanto",
                  rlv = "rakentamisen liikevaihtokuvaaja", ttvi = "teollisuustuotanto", teul = "teollisuuden uudet tilaukset",
                  ktkk = "tuotannon suhdannekuvaaja", kony = "konkurssit",
                  plv = "palvelualojen liikevaihtokuvaaja", merek = "ajoneuvojen ensirekisteröinnit"),
  eurostat = list(.n = "Eurostat", ei_bsin_m_r2 = "Euroopan komission luottamuskyselyt", ei_bsbu_m_r2 = "Euroopan komission luottamuskyselyt",
                  ei_bsrt_m_r2 = "Euroopan komission luottamuskyselyt", ei_bsse_m_r2 = "Euroopan komission luottamuskyselyt",
                  ei_bsin_q_r2 = "kapasiteetin käyttöaste", sts_trtu_m = "vähittäiskaupan liikevaihto", sts_setu_m = "palvelujen liikevaihto",
                  prc_hicp_minr = "YKHI", prc_hicp_iw = "YKHI-painot", namq_10_gdp = "BKT vertailumaissa", namq_10_lp_ulc = "yksikkötyökustannukset",
                  bop_c6_q = "maksutase", ext_st_27_2020msbec = "tavarakauppa", gov_10q_ggnfa = "julkisyhteisöjen verotulot"),
  bof      = list(.n = "Suomen Pankki", .all = "luotonanto ja lainojen korot"),
  ecb      = list(.n = "EKP", FM = "markkinakorot", EST = "€STR", YC = "tuottokäyrä", IRS = "valtionlainojen korot", EXR = "valuuttakurssit",
                  CISS = "rahoitusstressi", CLIFS = "rahoitusstressi", BLS = "pankkien luotonantokysely", SPF = "ammattiennustajien kysely",
                  ILM = "Eurojärjestelmän viikkotase", BSI = "rahan määrä ja luotonanto"),
  wb       = list(.n = "Maailmanpankki", .all = "raaka-aineiden hinnat, Pink Sheet"),
  xlsx     = list(.n = "Caldara, Iacoviello ym.", tpu = "kauppapoliittisen epävarmuuden indeksi", gpr = "geopoliittisen riskin indeksi"),
  stoxx    = list(.n = "STOXX", .all = "VSTOXX-volatiliteetti-indeksi"),
  vero     = list(.n = "Verohallinto", Verotulojen_kehitys = "verokertymät", Tulorekisteri = "ennakonpidätyksen alaiset ansiotulot (tulorekisteri)"),
  vk       = list(.n = "Valtiokonttori", .all = "valtionvelka"),
  lam      = list(.n = "Fintraffic", .all = "LAM-liikennemäärät, Digitraffic"),
  ameco    = list(.n = "Euroopan komissio", .all = "AMECO, rakenteellinen työttömyys (NAWRU)"),
  fred     = list(.n = "FRED", EUEPUINDXM = "talouspoliittisen epävarmuuden indeksi", GS10 = "Yhdysvaltain korot © OECD",
                  FEDFUNDS = "Yhdysvaltain korot © OECD", IRLTLT01JPM156N = "Japanin korot © OECD", IRSTCI01JPM156N = "Japanin korot © OECD",
                  DCOILBRENTEU = "Brent ja WTI © EIA", DCOILWTICO = "Brent ja WTI © EIA", VIXCLS = "VIX-volatiliteetti-indeksi © Cboe"))
src_used <- list()
src_add <- function(p, code) {
  nm <- SRC_NAMES[[p]]; it <- if (!is.null(nm$.all)) nm$.all else nm[[code]]
  if (is.null(it)) { CHECKS <<- c(CHECKS, sprintf("lähdeluettelo: nimi puuttuu (%s %s)", p, code)); return() }
  src_used[[p]] <<- unique(c(src_used[[p]], it)) }
src_walk <- function(x) {
  if (!is.list(x)) return()
  sc <- x$source
  if (!is.null(sc)) {
    if (sc == "statfin" && !is.null(x$px_base)) src_add("vero", sub("/.*", "", x$table))
    else if (sc == "statfin" || (sc == "splice" && !is.null(x$table))) src_add("statfin", sub("/.*", "", x$table))
    else if (sc == "jt_decomp") { src_add("statfin", "jtume"); src_add("statfin", "jyev"); src_add("statfin", "ntp") }
    else if (sc == "jk_decomp") src_add("statfin", "jtume")
    else if (sc == "eurostat") src_add("eurostat", x$ds)
    else if (sc == "ecb") src_add("ecb", x$flow)
    else if (sc == "fred") src_add("fred", x$fred_id)
    else if (sc %in% c("bof", "wb", "stoxx", "vk", "lam", "ameco")) src_add(sc, "")
    else if (sc == "ntp_ctrb") src_add("statfin", "ntp")
    else if (sc == "hhi_ctrb") src_add("statfin", "ntp")
    else if (sc == "khi_ctrb") src_add("statfin", "khi")
    else if (sc == "xlsx") src_add("xlsx", x$src_key)
    else if (sc == "gdp_np") src_add("statfin", "ntp")
    else if (sc == "hicp_ctrb") { src_add("eurostat", "prc_hicp_minr"); src_add("eurostat", "prc_hicp_iw") }
  } else if (!is.null(x$table)) src_add("statfin", sub("/.*", "", x$table))   # splice-osat
  for (k in setdiff(names(x), "pick")) if (is.list(x[[k]])) src_walk(x[[k]])
  if (is.null(names(x))) for (v in x) src_walk(v)
}
for (s in Filter(function(s) !isFALSE(s$enabled), cfg$series)) src_walk(s)
sources_out <- lapply(intersect(names(SRC_NAMES), names(src_used)), function(p)
  list(name = SRC_NAMES[[p]]$.n, items = I(intersect(unique(unlist(SRC_NAMES[[p]][names(SRC_NAMES[[p]]) != ".n"])), src_used[[p]]))))   # jarjestys SRC_NAMES:n mukaan

out <- list(
  meta = list(
    generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    stale = STALE,
    method = paste0("Suhdannetilanne: kunkin sarjan taso on kohinasuodatetun sarjan (HP-suodin, λ = ", LAMBDA_S[["M"]],
                    " kuukausisarjoille, ", LAMBDA_S[["Q"]], " neljännessarjoille) poikkeama pitkän aikavälin HP-trendistä (λ = ",
                    format(LAMBDA[["M"]], big.mark = " "), " kuukausisarjoille, ", format(LAMBDA[["Q"]], big.mark = " "),
                    " neljännessarjoille), volyymeille log-muodossa, standardoitu vuodesta ", Z_FROM, " alkaen. ",
                    "Ennen suodatusta sarjoja jatketaan vuoden AR-ennusteella päätepisteongelman lieventämiseksi (Kaiser ja Maravall 1999). ",
                    "Muutos on tason muutos edellisestä jaksosta jaettuna sen keskihajonnalla. Sarjoissa, joilla ei ole kasvutrendiä (saldoluvut, ",
                    "käyttöaste, työllisyys- ja työttömyysaste), muutos lasketaan kohinasuodatetun sarjan omasta muutoksesta, jotta trendin ",
                    "liike ei näy muutoksena. Käännetyissä sarjoissa ",
                    "(työttömyysaste, lomautetut, konkurssit) etumerkki on vaihdettu, joten ylhäällä = parempi. Majoitus- ja ravitsemuspalvelujen ",
                    "myynti on deflatoitu alan kuluttajahintaindeksillä, koska Eurostat ei julkaise Suomelle volyymisarjaa."),
    growth = paste0("Reaalitalous: q/q kausitasoitetusta sarjasta, v/v työpäiväkorjatusta (tilinpito) tai alkuperäisestä sarjasta ",
                    "(inflaatio, asuntohinnat, ansiot, työttömät), kuten virallisissa tilastoissa. Taulukon värin voimakkuus on muutos ",
                    "suhteessa sarjan keskihajontaan vuodesta 2015 ilman vuosia 2020–2021. Kontribuutiot on laskettu edellisen vuoden ",
                    "hinnoin, jolloin osat summautuvat kasvuun; erotus näkyy viivoitettuna jäännöksenä. Rahoitusjäämät ja vaihtotase ovat ",
                    "neljän neljänneksen summia suhteessa saman jakson BKT:hen. Kuukausisarjat näytetään neljänneksen keskiarvoina. ",
                    "Tilastokeskuksen vuokraindeksi on ketjutettu perusvuoden vaihdon (2025) yli päällekkäisen vuoden keskiarvolla. ",
                    "Avoimien työpaikkojen aste on avoimet työpaikat / (avoimet + työlliset), työn tuottavuus BKT:n volyymi tehtyä työtuntia kohden ",
                    "ja vaihtosuhde vientihintojen ja tuontihintojen suhde. Asuntojen hinnat suhteutetaan kotitalouksien käytettävissä oleviin tuloihin ",
                    "asukasta kohden ja vapaarahoitteisiin vuokriin. Kotitalouksien reaalitulon osatekijöissä palkansaajakorvaukset on jaettu ",
                    "palkansaajien tehtyihin työtunteihin ja keskiansioon työtuntia kohden; hinta on yksityisen kulutuksen deflaattori. ",
                    "BKT:n hintaosuus on nimellisen ja volyymin vuosimuutoksen erotus. Sarjat, joille ei julkaista ",
                    "kausitasoitettua versiota, on kausitasoitettu STL-menetelmällä."),
    charts = paste0("Rahoitusmarkkinat: taulukoissa päivä- tai kuukausiarvot, kuvioissa kuukausiarvot ",
                    "(kuukausikeskiarvo tai kuun viimeinen havainto, ks. kuvion huomautus). ",
                    "Riskilisä on Suomen ja Saksan 10 vuoden korkojen ero."),
    caveat = paste0("Trendisuotimen loppupisteet tarkentuvat uuden datan myötä, ",
                    "joten viimeisimmät pisteet voivat siirtyä."),
    composite = paste0("Kokonaisindikaattori: sarjojen tasojen keskiarvo jaksoilta, joilla kaikki sarjat ovat saatavilla. BKT ei ole mukana keskiarvossa. Käyttöaste, konkurssit ja tavaravienti näkyvät kartalla, mutta eivät ole mukana keskiarvossa; tavaravienti julkaistaan muita myöhemmin.", VALID_TXT),
    sources = sources_out),
  sets = sets_out)

# ---------------------------------------------------------------------
# Tarkistukset: vanhentuneet sarjat ja valimuistin varaan jaaneet haut.
# STRICT=1 (esim. GitHub Actions) -> virhekoodi, jos huomioita on.
# ---------------------------------------------------------------------
for (st in sets_out) {
  lag_max <- if (st$freq == "M") 4 else 2
  end_i <- per_idx(st$periods[length(st$periods)], st$freq)
  lasts <- c(lapply(st$indicators, function(x) list(id = x$id, last = x$last)),
             lapply(st$series, function(x) list(id = x$id, last = x$last)))
  qm <- vapply(Filter(function(s) isTRUE(s$q_to_m), cfg$series), function(s) s$id, "")   # neljannessarja kuukausinakymassa: julkaisuviive pidempi
  for (x in lasts) if (!is.null(x$last) && length(x$last) && end_i - per_idx(x$last, st$freq) > lag_max + if (x$id %in% qm) 3 else 0)
    CHECKS <- c(CHECKS, sprintf("%s/%s: viimeisin havainto %s", st$id, x$id, x$last))
}
if (length(STALE)) message("Valimuistista (tilapainen katko): ", paste(STALE, collapse = ", "))
if (length(STALE_OLD)) CHECKS <- c(CHECKS, paste0("valimuisti yli ", STALE_DAYS, " pv vanha: ", paste(STALE_OLD, collapse = ", ")))
CHECKS <- unique(CHECKS)
if (length(CHECKS)) message("TARKISTUS: ", length(CHECKS), " huomiota\n  ", paste(CHECKS, collapse = "\n  ")) else message("TARKISTUS: ok")
out$meta$checks <- CHECKS

writeLines(toJSON(out, auto_unbox = TRUE, na = "null", null = "null", digits = NA), OUT_PATH)
message("Kirjoitettu ", OUT_PATH)

# Paikallinen esikatselu: data upotettuna sivupohjaan, avautuu tuplaklikkaamalla
TEMPLATE <- "clock_template.html"
if (file.exists(TEMPLATE)) {
  json <- gsub("</", "<\\/", paste(readLines(OUT_PATH, encoding = "UTF-8"), collapse = "\n"), fixed = TRUE)
  tpl  <- paste(readLines(TEMPLATE, encoding = "UTF-8"), collapse = "\n")
  writeLines(sub("__DATA__", json, tpl, fixed = TRUE), "suhdannetilanne.html", useBytes = TRUE)
  message("Kirjoitettu suhdannetilanne.html (avaa selaimessa)")
}
if (length(CHECKS) && identical(Sys.getenv("STRICT"), "1")) {
  message("STRICT: lopetetaan virhekoodilla tarkistushuomioiden vuoksi"); quit(status = 1)
}
