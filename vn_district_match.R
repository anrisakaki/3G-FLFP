
suppressMessages({library(data.table); library(stringi)})

LFS_DIR <- "Raw Data/LFS"
MCL_DIR <- "Raw Data/MCL"
SEP     <- " ¦ "   # sentinel marking hard punctuation; blocks cross-clause matches

## ---- 1. Vietnamese normalisation (keeps diacritics) -----------------
.oa <- c("oà"="òa","oá"="óa","oả"="ỏa","oã"="õa","oạ"="ọa",
         "oè"="òe","oé"="óe","oẻ"="ỏe","oẽ"="õe","oẹ"="ọe")
.uy <- c("uỳ"="ùy","uý"="úy","uỷ"="ủy","uỹ"="ũy","uỵ"="ụy")

#' Canonicalise Vietnamese text: NFC, lowercase, unify tone-mark placement.
vn_canon <- function(x, hard_sep = TRUE){
  x <- stri_trans_nfc(x)
  x <- stri_replace_all_regex(x, "(?<=\\p{Ll})(?=\\p{Lu})", " ")
  x <- stri_trans_tolower(x)
  for (k in names(.oa)) x <- stri_replace_all_regex(x, paste0(k, "(?![\\p{L}])"), .oa[[k]])
  for (k in names(.uy)) x <- stri_replace_all_regex(x, paste0("(?<!q)", k, "(?![\\p{L}])"), .uy[[k]])
  if (hard_sep) x <- stri_replace_all_regex(x, "[,.;:|/\\\\()\\[\\]{}\"'`\\n\\r\\t*#•+–—-]+", SEP)
  x <- stri_replace_all_regex(x, "[^\\p{L}\\p{N}¦]+", " ")
  stri_trim_both(stri_replace_all_regex(x, " +", " "))
}
.esc <- function(x) stri_replace_all_regex(x, "([\\^$.|?*+()\\[\\]{}\\\\])", "\\\\$1")
.alt <- function(keys){
  keys <- keys[order(-stri_length(keys))]
  paste0("(?<![\\p{L}\\p{N}])(?:", paste(.esc(keys), collapse = "|"), ")(?![\\p{L}\\p{N}])")
}

## ---- 2. gazetteer ---------------------------------------------------
build_gazetteer <- function(lfs_dir = LFS_DIR){
  g <- fread(file.path(lfs_dir, "lfs_dist_11.csv"), encoding = "UTF-8")
  g[, `:=`(distname = trimws(distname), provname = trimws(provname))]
  g <- unique(g[, .(tinh, huyen, provname, distname)])
  g12 <- fread(file.path(lfs_dir, "lfs_dist_12.csv"), encoding = "UTF-8")
  g12[, `:=`(distname = trimws(distname), provname = trimws(provname))]
  g12 <- unique(g12[, .(tinh, huyen, provname, distname)])
  g <- rbind(g, g12[!g, on = .(tinh, huyen)])
  g[, dtype := stri_trans_tolower(stri_extract_first_regex(distname, "(?i)^(Quận|Huyện|Thị xã|Thành phố)"))]
  g[, dname := vn_canon(stri_replace_first_regex(distname, "(?i)^(Quận|Huyện|Thị xã|Thành phố)\\s+", ""), FALSE)]
  g[, pname := vn_canon(provname, FALSE)]
  g <- g[dname != ""][, .SD[1], by = .(tinh, huyen)]
  stopifnot(!anyNA(g$dtype))     # every distname must carry a recognised prefix

  par <- function(nm) g[dname == nm, .(tinh, huyen, provname)][1]
  sup <- rbindlist(list(
    data.table(dname = c("bắc từ liêm","nam từ liêm"), dtype = "quận",  par("từ liêm"),  split_of = "từ liêm"),
    data.table(dname = "lâm bình",                     dtype = "huyện", par("nà hang"),  split_of = "nà hang"),
    data.table(dname = "vân hồ",                       dtype = "huyện", par("mộc châu"), split_of = "mộc châu")
  ), fill = TRUE)
  sup[, `:=`(distname = dname, pname = vn_canon(provname, FALSE))]
  stopifnot(!anyNA(sup$huyen))  
  g[, split_of := NA_character_]
  rbind(g, sup, fill = TRUE)
}

build_provinces <- function(g){
  p <- unique(g[, .(tinh, pname)])
  extra <- rbindlist(list(
    data.table(tinh = 79L, pname = c("tp hcm","tphcm","hcm","sài gòn","saigon","hồ chí minh")),
    data.table(tinh =  1L, pname = c("hà nội","hanoi")),
    data.table(tinh = 48L, pname = c("đà nẵng","danang")),
    data.table(tinh = 46L, pname = c("huế","thừa thiên huế"))
  ))
  unique(rbind(p, extra))[stri_length(pname) >= 3]
}

## Wards that belong to exactly one district nationally, so a stated ward pins
## the district. UNIQUENESS is judged on the COMPLETE ward list (GADM level-3,
## vnmap3, 11k wards) 
build_wards <- function(g, v3 = NULL,
    gadm_path = "Raw Data/VNShapefile/gadm41_VNM_shp/gadm41_VNM_3.shp"){
  if (is.null(v3)) v3 <- sf::st_drop_geometry(sf::st_read(gadm_path, quiet = TRUE))
  v3 <- as.data.table(v3)[, .(NAME_1, NAME_2, NAME_3)]
  cn <- function(x) vn_canon(stri_replace_all_regex(
          stri_replace_all_regex(x, "[-–—]", " "), "\\s+", " "), FALSE)
  v3[, wname := cn(stri_replace_first_regex(NAME_3, "^(Phường|Xã|Thị trấn)\\s+", ""))]
  v3 <- v3[wname != "" & !stri_detect_regex(wname, "^[0-9]+$") & stri_length(wname) >= 6]
  u <- v3[, .(nd = uniqueN(paste(NAME_1, NAME_2))), by = wname][nd == 1L]
  v3 <- v3[wname %in% u$wname]
  v3[, pc := cn(stri_replace_first_regex(NAME_1, "^(Tp|TP|Thành phố|Tỉnh)\\s+", ""))]
  pl <- unique(g[, .(tinh, pc = vn_canon(stri_replace_first_regex(provname,
          "^(Tp|TP|Thành phố|Tỉnh)\\s+", ""), FALSE))])
  v3 <- merge(v3, pl, by = "pc")
  gn <- unique(g[, .(tinh, huyen, dc = vn_canon(stri_replace_first_regex(distname,
          "^(Quận|Huyện|Thị xã|Thành phố)\\s+", ""), FALSE))])
  gn <- unique(gn, by = c("tinh", "dc"))
  v3[, dc := cn(stri_replace_first_regex(NAME_2, "^(Quận|Huyện|Thị xã|Thành phố)\\s+", ""))]
  v3 <- merge(v3, gn, by = c("tinh", "dc"))
  unique(v3[, .(tinh, huyen, wname)])
}

## ---- 3. matcher -----------------------------------------------------
## `q`/`h`/`tp`/`tx` are allowed as prefixes only immediately before a known district name 
RX_PREFIX <- "(?<![\\p{L}])(?:quận|huyện|thị xã|thành phố|tp|tx|q|h)\\s*¦?\\s*$"
## accent-folded prefix marker, for the tone-less / English fallback below
RX_PREFIX_A <- "(?<![\\p{L}])(?:quan|huyen|thi xa|thanh pho|tp|tx|q|h)\\s*¦?\\s*$"
RX_PFXTYPE <- "(?<![\\p{L}\\p{N}])(quận|huyện|thị xã|thành phố|tp|tx|q|h)\\s*¦?\\s*$"
PFX_TYPE   <- c("q"="quận","quận"="quận","h"="huyện","huyện"="huyện",
                "tx"="thị xã","thị xã"="thị xã","tp"="thành phố","thành phố"="thành phố")
RX_SUFMARK <- "^\\s*¦?\\s*(?:district|dist)(?![\\p{L}])"
RX_STREET <- "\\d+\\s*[a-z]?\\s*¦?\\s*$"
RX_WARDPRE <- "(?<![\\p{L}])(?:phường|xã|p)\\s*¦?\\s*$"
RX_NUM    <- "(?<![\\p{L}\\p{N}])(?:quận|district|(?:q|dist)\\s*¦?)\\s*0?(1[0-2]|[1-9])(?![0-9])"
RX_FIRMSPAN <- paste0("(?:công\\s+ty|cty|tập\\s+đoàn|doanh\\s+nghiệp|xí\\s+nghiệp)",
  "(?:(?!¦| tuyển| cần | tại | ở | thông\\s+báo| địa\\s+chỉ| chuyên| có | đang| hiện| là | được",
  "| chi\\s+nhánh| kcn| khu\\s+công\\s+nghiệp| số\\s*\\d| đường| phố).){0,60}")
RISKY_DEFAULT <- c("lý nhân",     
                   "thống nhất")  

##   Posts with a province but no district get one `province_only` row.
match_districts <- function(text, page, g, prov, risky = RISKY_DEFAULT,
                            adj_chars = 40L, province_only = TRUE, wards = NULL){
  named <- g[!stri_detect_regex(dname, "^[0-9]{1,2}$")]
  numd  <- g[stri_detect_regex(dname, "^[0-9]{1,2}$")]   # Quận 1-12, HCMC only
  rx_d  <- .alt(unique(named$dname)); rx_p <- .alt(unique(prov$pname))
  rx_w  <- if (!is.null(wards)) paste0(
    "(?<![\\p{L}])(?:phường|xã|thị trấn|p|tt)\\s*¦?\\s*(",
    paste(.esc(unique(wards$wname)[order(-stri_length(unique(wards$wname)))]), collapse = "|"),
    ")(?![\\p{L}\\p{N}])") else NULL
  afold   <- function(x) stri_trans_general(x, "Latin-ASCII")
  namasc  <- unique(named[, .(tinh, huyen, da = afold(dname))])[stri_count_fixed(da, " ") >= 1L]
  rx_dasc <- if (nrow(namasc)) .alt(unique(namasc$da)) else ""
  ## district-abbreviation dictionary: the uppercase initialism of a multi-word
  ## district name ("Hai Bà Trưng" -> HBT), ASCII-folded. 
  .initof <- function(nm){ tk <- stri_split_regex(nm, " ")[[1]]; tk <- tk[nzchar(tk)]
    if (!length(tk)) "" else paste(toupper(afold(stri_sub(tk, 1, 1))), collapse = "") }
  abbrdt <- unique(named[, .(tinh, huyen, dname)])
  abbrdt[, ab := vapply(dname, .initof, character(1))]
  abbrdt <- abbrdt[stri_length(ab) == 3L]
  abbrdt[, nprov := uniqueN(huyen), by = .(tinh, ab)]
  abbrdt <- unique(abbrdt[nprov == 1L], by = c("tinh", "ab"))

  tc <- vn_canon(text); pc <- vn_canon(page)
  dm <- stri_locate_all_regex(tc, rx_d, omit_no_match = TRUE)
  pm <- stri_locate_all_regex(tc, rx_p, omit_no_match = TRUE)
  pg <- stri_extract_all_regex(pc, rx_p, omit_no_match = TRUE)
  nh_cap <- stri_match_all_regex(tc, RX_NUM, omit_no_match = TRUE)
  nh_loc <- stri_locate_all_regex(tc, RX_NUM, omit_no_match = TRUE)
  wm <- if (!is.null(rx_w)) lapply(stri_match_all_regex(tc, rx_w, omit_no_match = TRUE),
               function(x) if (length(x)) unique(x[, 2]) else character()) else NULL
  ntok <- setNames(stri_count_fixed(named$dname, " ") + 1L, named$dname)

  res <- vector("list", length(text))
  for (i in seq_along(text)){
    ## keep each numbered district once
    nums <- if (length(nh_cap[[i]])) nh_cap[[i]][, 2] else character()
    npos <- if (length(nh_loc[[i]])) nh_loc[[i]][, 1] else integer()
    keepn <- !duplicated(nums); nums <- nums[keepn]; npos <- npos[keepn]
    hasnum <- length(nums) > 0L
    nd <- nrow(dm[[i]]); np <- nrow(pm[[i]])
    pa <- if (np) pm[[i]][,1] else numeric(); pb <- if (np) pm[[i]][,2] else numeric()
    ptinh <- if (np) prov[match(stri_sub(tc[i], pa, pb), pname)]$tinh else integer()
    ppage <- if (length(pg[[i]])) unique(prov[pname %in% pg[[i]]]$tinh) else integer()

    ## firm-name spans in this post; hits inside them are company names
    fsp <- stri_locate_all_regex(tc[i], RX_FIRMSPAN, omit_no_match = TRUE)[[1]]
    in_firm <- function(p) nrow(fsp) > 0L && any(p >= fsp[,1] & p <= fsp[,2])

    rows <- list()
    if (hasnum){
      keepn2 <- !vapply(as.integer(npos), in_firm, logical(1))
      nums <- nums[keepn2]; npos <- npos[keepn2]; hasnum <- length(nums) > 0L
    }
    if (hasnum) rows[[1]] <- data.table(matched = nums, tinh = 79L,
        huyen = numd$huyen[match(nums, numd$dname)], evidence = "numbered", conf = "high",
        pos = as.integer(npos))

    if (nd) for (j in seq_len(nd)){
      a <- dm[[i]][j,1]; b <- dm[[i]][j,2]
      if (in_firm(a)) next                      
      hit  <- stri_sub(tc[i], a, b)
      cand <- named[dname == hit]
      pre  <- stri_sub(tc[i], max(1L, a - 16L), a - 1L)
      pfx  <- stri_detect_regex(pre, RX_PREFIX) ||
              stri_detect_regex(stri_sub(tc[i], b + 1L, b + 14L), RX_SUFMARK)
      st   <- stri_detect_regex(pre, RX_STREET)
      ## explicitly marked as a ward, and no district prefix -> not a district
      if (!pfx && stri_detect_regex(pre, RX_WARDPRE)) next

      pstr <- if (np) stri_sub(tc[i], pa, pb) else character()
      keep <- if (np) (pb < a | pa > b) & (pstr != hit) else logical(0)
      near <- if (any(keep)) ptinh[keep][abs(pa[keep] - a) <= adj_chars] else integer()
      ptxt <- if (any(keep)) unique(ptinh[keep]) else integer()
      conflict <- length(ptxt) > 0L && !any(cand$tinh %in% ptxt)
      ## ambiguous name whose stated admin TYPE picks a unique district
      ct <- if (pfx && nrow(cand) > 1L){
        pt <- stri_match_last_regex(pre, RX_PFXTYPE)[, 2]
        if (!is.na(pt)) cand[dtype == PFX_TYPE[[pt]]] else cand[0]
      } else cand[0]

      if (any(cand$tinh %in% near))       { ev <- "prov_adjacent"; pick <- cand[tinh %in% near][1] }
      else if (any(cand$tinh %in% ptxt))  { ev <- "prov_in_text";  pick <- cand[tinh %in% ptxt][1] }
      else if (pfx && nrow(cand) == 1L)   { ev <- if (conflict) "prefix_conflict" else "prefix_only"
                                            pick <- cand[1] }
      else if (any(cand$tinh %in% ppage)) { ev <- "prov_in_page";  pick <- cand[tinh %in% ppage][1] }
      else if (nrow(ct) == 1L)            { ev <- "prefix_typed";  pick <- ct[1] }
      else {
        ok <- nrow(cand) == 1L && ntok[[hit]] >= 2L && !(hit %in% risky) &&
              !st && length(ptxt) == 0L && !(hit %in% prov$pname)
        if (!ok) next
        ev <- "bare"; pick <- cand[1]
      }
      cf <- switch(ev, prov_adjacent = "high", numbered = "high",
                   prov_in_text = if (nrow(cand) > 1L) "medium" else "high",
                   prefix_only = "medium", prov_in_page = "medium",
                   prefix_typed = if (conflict) "low" else "medium",
                   prefix_conflict = "low", bare = "low")
      rows[[length(rows) + 1L]] <- data.table(matched = hit, tinh = pick$tinh,
                                              huyen = pick$huyen, evidence = ev, conf = cf,
                                              pos = as.integer(a))
    }
    
    ## ---- recall fallbacks, only when the TEXT yielded no district ----
    if (!length(rows) && !is.null(wm) && length(wm[[i]])){
      ww <- wards[wname %in% wm[[i]]]
      for (r in seq_len(nrow(ww)))
        rows[[length(rows) + 1L]] <- data.table(matched = ww$wname[r], tinh = ww$tinh[r],
            huyen = ww$huyen[r], evidence = "ward", conf = "medium", pos = NA_integer_)
    }

    if (!length(rows) && nzchar(rx_dasc)){
      ta <- afold(tc[i])
      am <- stri_locate_all_regex(ta, rx_dasc, omit_no_match = TRUE)[[1]]
      if (nrow(am)) for (j in seq_len(nrow(am))){
        aa <- am[j, 1]; bb <- am[j, 2]
        if (in_firm(aa)) next
        hitA   <- stri_sub(ta, aa, bb)
        marked <- stri_detect_regex(stri_sub(ta, max(1L, aa - 16L), aa - 1L), RX_PREFIX_A) ||
                  stri_detect_regex(stri_sub(ta, bb + 1L, bb + 14L), RX_SUFMARK)
        if (!marked) next
        candA  <- namasc[da == hitA]
        pscope <- unique(c(ptinh, ppage))
        inp    <- if (length(pscope)) candA[tinh %in% pscope] else candA[0]
        pick <- if (nrow(inp)) inp[1]
                else if (!length(pscope) && uniqueN(candA[, .(tinh, huyen)]) == 1L) candA[1]
                else NULL
        if (is.null(pick) || !nrow(pick)) next
        rows[[length(rows) + 1L]] <- data.table(matched = hitA, tinh = pick$tinh,
            huyen = pick$huyen, evidence = "ascii_marked", conf = "low", pos = as.integer(aa))
      }
    }
    ## (c) district-abbreviation fallback: an uppercase 3-letter initialism sitting
    ##     in an address list ("... , HBT , Hà Nội" / "Q.HBT"), when the province is
    ##     known and the abbreviation is unique within it. 
    if (!length(rows) && nrow(abbrdt)){
      pscope <- unique(c(ptinh, ppage))
      if (length(pscope)){
        om <- stri_locate_all_regex(text[i], "(?<![\\p{L}])[A-ZĐ]{3}(?![\\p{L}])",
                                    omit_no_match = TRUE)[[1]]
        if (nrow(om)) for (j in seq_len(nrow(om))){
          aa <- om[j, 1]; bb <- om[j, 2]
          tok <- toupper(afold(stri_sub(text[i], aa, bb)))
          cand <- abbrdt[ab == tok & tinh %in% pscope]
          if (nrow(cand) != 1L) next
          before <- stri_sub(text[i], max(1L, aa - 4L), aa - 1L)
          after  <- stri_sub(text[i], bb + 1L, bb + 4L)
          ctx <- stri_detect_regex(before, "[,(/-]\\s*$") ||
                 stri_detect_regex(before, "(?i)(?:quận|huyện|q|h|tp|tx)\\.?\\s*$") ||
                 stri_detect_regex(after,  "^\\s*[,)./-]")
          if (!ctx) next
          rows[[length(rows) + 1L]] <- data.table(matched = stri_trans_tolower(stri_sub(text[i], aa, bb)),
              tinh = cand$tinh, huyen = cand$huyen, evidence = "abbrev", conf = "low", pos = NA_integer_)
        }
      }
    }
    if (length(rows)) {
      r <- rbindlist(rows)[order(pos, na.last = TRUE)]
      r <- r[order(evidence == "prefix_typed")][!duplicated(huyen)][order(pos, na.last = TRUE)]
      res[[i]] <- r[, post := i]
    }
    else if (province_only && uniqueN(ptinh) == 1L)
      res[[i]] <- data.table(matched = NA_character_, tinh = ptinh[1], huyen = NA_integer_,
                             evidence = "province_only", conf = "province",
                             pos = NA_integer_, post = i)
  }
  out <- rbindlist(res)
  if (!nrow(out)) return(out)
  ## label from the PARENT row only: post-2011 splits share their parent's key
  g2 <- unique(g[is.na(split_of), .(tinh, huyen, provname, distname)])
  out[g2, on = .(tinh, huyen), `:=`(provname = i.provname, distname = i.distname)]
  out[, split_of := g[match(out$matched, dname)]$split_of]
  out[]
}

derive_risky <- function(m, min_n = 10L, min_support = 0.30){
  s <- m[evidence != "province_only", .(n = .N, sup = mean(evidence != "bare")), by = matched]
  s[n >= min_n & sup < min_support]$matched
}

mcl_district_panel <- function(mcl_dir = MCL_DIR, filter_rx = "tuyển dụng"){
  g <- build_gazetteer(); prov <- build_provinces(g); wards <- build_wards(g)
  files <- list.files(mcl_dir, pattern = "\\.csv$", full.names = TRUE)
  d <- rbindlist(lapply(files, function(f)
        fread(f, encoding = "UTF-8",
              select = c("id","creation_time","text","surface.name","post_owner.name"),
              colClasses = c(creation_time = "character"))), fill = TRUE)
  d <- d[stri_detect_regex(text, filter_rx, case_insensitive = TRUE)]
  d[, page := paste(surface.name, post_owner.name)]

  m1    <- match_districts(d$text, d$page, g, prov, risky = character(), wards = wards)
  risky <- union(RISKY_DEFAULT, derive_risky(m1))
  message("risky names dropped from the bare tier: ", paste(risky, collapse = ", "))
  m <- match_districts(d$text, d$page, g, prov, risky = risky, wards = wards)

  m[, `:=`(id = d$id[post], creation_time = d$creation_time[post])]
  m[, year := lubridate::year(lubridate::ymd_hms(creation_time))]
  m[]
}
