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
TILE_N   <- 130           # markkinaruutujen paivahavainnot; sivu nayttaa niista viimeiset 3 kk
CACHE_DIR <- "cache"      # edelliset onnistuneet haut sarjoittain
STALE    <- character(0)  # sarjat, joiden data tuli valimuistista
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
  r <- RETRY("GET", url, timeout(40), times = 3, pause_base = 2, quiet = TRUE)
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

fetch_fred <- function(s) {
  d <- read.csv(text = get_text(paste0(FRED, s$fred_id)))
  data.frame(date = substr(d[[1]], 1, if (isTRUE(s$daily)) 10 else 7), value = suppressWarnings(as.numeric(d[[2]])))
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

# saumaus: sama sarja useasta taulusta (esim. perusvuoden vaihtuessa), osat jarjestyksessa, myohempi osa voittaa
# osan from/to rajaa jakson ("YYYY-MM"); tarkoitettu tasosarjoille kuten lukumaarille, ei indekseille
fetch_splice <- function(s) {
  parts <- lapply(seq_along(s$parts), function(k) { p <- modifyList(s$parts[[k]], list(id = paste0(s$id, "_osa", k)))
    x <- fetch_statfin(p)
    if (!is.null(p$from)) x <- x[x$date >= p$from, ]
    if (!is.null(p$to)) x <- x[x$date <= p$to, ]
    x[!is.na(x$value), ] })
  d <- Reduce(function(a, b) rbind(a[!(a$date %in% b$date), ], b), parts)
  d[order(d$date), ]
}

fetch_series <- function(s) {
  f <- switch(s$source, statfin = fetch_statfin, splice = fetch_splice, eurostat = fetch_eurostat, hicp_ctrb = fetch_hicp_ctrb, ntp_ctrb = fetch_ntp_ctrb,
              ecb = fetch_ecb, fred = fetch_fred, bof = fetch_bof, wb = fetch_wb, stop("Tuntematon lahde ", s$source))
  # valimuisti: onnistunut haku tallennetaan, epaonnistuessa kaytetaan edellista hakua
  cache <- file.path(CACHE_DIR, paste0(s$id, ".csv"))
  d <- tryCatch({
    x <- f(s); dir.create(CACHE_DIR, showWarnings = FALSE)
    write.csv(x, cache, row.names = FALSE); x
  }, error = function(e) {
    if (!file.exists(cache)) stop("Haku epaonnistui eika valimuistia ole: ", s$id, " (", conditionMessage(e), ")")
    message("  VAROITUS ", s$id, ": lahde ei vastannut, kaytetaan valimuistia (", format(file.mtime(cache), "%d.%m.%Y"), ")")
    STALE <<- c(STALE, s$id)
    read.csv(cache, colClasses = c("character", "numeric"))
  })
  if (isTRUE(s$to_quarter) || identical(s$to_quarter, "sum")) {   # kuukausisarja -> neljannes (keskiarvo tai "sum"), vain taydet neljannekset
    q <- sprintf("%s-Q%d", substr(d$date, 1, 4), (as.integer(substr(d$date, 6, 7)) - 1) %/% 3 + 1)
    agg <- if (identical(s$to_quarter, "sum")) sum else mean
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
  if (isTRUE(s$q_to_m)) {                         # neljannesvuosisarja kuukausinakymaan: neljanneksen viimeinen kuukausi
    d$date <- sprintf("%s-%02d", substr(d$date, 1, 4), 3 * as.integer(substr(d$date, 7, 7)))
  }
  if (!is.null(s$roll)) {                          # liukuva summa (esim. 12 kk konkurssit), vain taydet ikkunat
    d <- d[order(d$date), ]; d$value <- as.numeric(stats::filter(d$value, rep(1, s$roll), sides = 1))
  }
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

clock_series <- function(d, s, freq) {
  y <- if (s$kind == "volume") 100 * log(d$value) else d$value
  sm <- hp_trend(y, LAMBDA_S[[freq]])                                  # kohinasuodin
  # volume/balance: poikkeama pitkan aikavalin trendista (HP-kaistanpaasto)
  # level (korot, stressi, epavarmuus): taso suhteessa omaan historiaan, ei trendia
  tr  <- if (s$kind == "level") rep(NA_real_, length(y)) else hp_trend(y, LAMBDA[[freq]])
  gap <- if (s$kind == "level") sm else sm - tr
  keep <- as.integer(substr(d$date, 1, 4)) >= Z_FROM
  lvl <- (gap - mean(gap[keep])) / sd(gap[keep])
  if (isTRUE(s$invert)) lvl <- -lvl
  dl  <- c(NA, diff(lvl))
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
      from = from_y, periods = periods, charts = charts_out, colors = set$colors, tiles = unname(tiles), summary = set$summary,
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
    # kasvukello: taso = v/v-kasvu standardoituna (ero omaan keskiarvoon), muutos = kasvun kiihtyminen
    inds <- lapply(Filter(function(s) !isFALSE(s$in_table) && !isFALSE(s$in_clock), members), function(s) {
      r <- raws[[paste0(s$id, "_yy")]]; r <- r[!is.na(r$value), ]
      keep <- as.integer(substr(r$date, 1, 4)) >= Z_FROM
      lvl <- (if (isTRUE(s$invert)) -1 else 1) * (r$value - mean(r$value[keep])) / sd(r$value[keep])
      dl <- c(NA, diff(lvl)); chg <- dl / sd(dl[keep], na.rm = TRUE)
      list(id = s$id, label = s$label, group = s$group, kind = "growth", invert = isTRUE(s$invert),
           in_composite = !isFALSE(s$in_composite), last = r$date[nrow(r)], last_raw = round(tail(r$value, 1), 2),
           level = r3(lvl[match(periods, r$date)]), change = r3(chg[match(periods, r$date)]))
    })
    L <- sapply(Filter(function(x) x$in_composite, inds), function(x) unlist(lapply(x$level, function(v) if (is.null(v)) NA else v)))
    comp_lvl <- rowMeans(L); dlc <- c(NA, diff(comp_lvl)); comp_chg <- dlc / sd(dlc, na.rm = TRUE)
    sets_out[[length(sets_out) + 1]] <- list(
      id = set$id, label = set$label, freq = freq, default = isTRUE(set$default), view = "charts",
      from = from_y, periods = periods, charts = set$charts, colors = set$colors, groups = set$groups, lead = set$lead,
      tail = TAIL_N[[freq]], indicators = unname(inds),
      composite = list(last = periods[max(which(!is.na(comp_lvl)))], level = r3(comp_lvl), change = r3(comp_chg)),
      growth = unname(rows),
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
    list(id = s$id, label = s$label, group = s$group, kind = s$kind, unit = s$unit, raw_desc = s$raw_desc,
         invert = isTRUE(s$invert), in_composite = !isFALSE(s$in_composite),
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
    if (!is.null(contribs$comp)) contribs$comp$note <- paste0(contribs$comp$note, sprintf(
      " Korrelaatio %s–%s: %s (ilman 2020–21: %s).", substr(both[1], 1, 4), substr(tail(both, 1), 1, 4),
      sub(".", ",", sprintf("%.2f", r_all), fixed = TRUE), sub(".", ",", sprintf("%.2f", r_ex), fixed = TRUE)))
    message(sprintf("  Kokonaisindikaattori vs BKT: r = %.2f (ilman 2020-21 %.2f)", r_all, r_ex))
  }

  sets_out[[length(sets_out) + 1]] <- list(
    id = set$id, label = set$label, freq = freq, default = isTRUE(set$default),
    tail = TAIL_N[[freq]], groups = set$groups, lead = set$lead, periods = periods,
    composite = list(last = comp_last, level = r3(comp_lvl), change = r3(comp_chg)),
    indicators = unname(inds), series = unname(cser), contribs = contribs)
}

out <- list(
  meta = list(
    generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    stale = STALE,
    method = paste0("Taso: kohinasuodatetun sarjan (HP lambda ", LAMBDA_S[["M"]], " kk / ",
                    LAMBDA_S[["Q"]], " nelj.) poikkeama pitkän aikavälin HP-trendista (lambda ",
                    LAMBDA[["M"]], " / ", LAMBDA[["Q"]], "), volyymeille log-muodossa, standardoitu ",
                    Z_FROM, "-> ikkunalla. Muutos: tason muutos edellisestä jaksosta ",
                    "jaettuna sen keskihajonnalla. Käännetyt sarjat (työttömyysaste) ",
                    "etumerkki vaihdettu, ylhäällä = parempi. Majoitus- ja ravitsemuspalveluiden liikevaihto ",
                    "on deflatoitu alan kuluttajahintaindeksillä (HICP CP11), koska Eurostat ei julkaise Suomelle volyymisarjaa. ",
                    "Reaalitalous: q/q kausitasoitetusta sarjasta, v/v työpäiväkorjatusta (tilinpito) tai ",
                    "alkuperäisestä sarjasta (inflaatio, asuntohinnat, ansiot, työttömät), kuten virallisissa tilastoissa."),
    charts = paste0("Rahoitusmarkkinat: sarjat alkuperäisinä kuukausiarvoina. Vertailu historiaan kertoo, ",
                    "kuinka suuri osa kuukausista vuodesta 2000 alkaen on ollut nykyistä matalampia. ",
                    "Riskilisä ja korkokäyrä ovat 10 vuoden korkojen ja 3 kk Euriborin erotuksia."),
    caveat = paste0("Trendisuotimen loppupisteet tarkentuvat uuden datan myötä, ",
                    "joten viimeisimmät pisteet voivat siirtyä."),
    composite = "Kokonaisindikaattori: sarjojen tasojen keskiarvo jaksoilta, joilla kaikki sarjat ovat saatavilla. BKT ei ole mukana keskiarvossa."),
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
  for (x in lasts) if (!is.null(x$last) && length(x$last) && end_i - per_idx(x$last, st$freq) > lag_max)
    CHECKS <- c(CHECKS, sprintf("%s/%s: viimeisin havainto %s", st$id, x$id, x$last))
}
if (length(STALE)) CHECKS <- c(CHECKS, paste("valimuistista:", paste(STALE, collapse = ", ")))
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
