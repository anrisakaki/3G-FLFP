library(data.table)
library(tidyverse)
library(stringi)
library(sf)
library(readxl)

load("Clean data/dist_3G.Rda")
load("Clean data/district_controls_09.Rda")

cd_files <- list.files("Raw Data/Consistent District",
                       pattern = "__31_12.*\\.xls$", full.names = TRUE)
consist_dist <- rbindlist(lapply(cd_files, function(f) {
  d <- as.data.table(read_excel(f, sheet = 1, col_types = "text"))
  setnames(d, 1:8, c("distcode", "distname", "eng", "level",
                     "decree", "note", "provcode", "provname"))
  d[, year := as.integer(stri_extract_last_regex(basename(f), "20\\d{2}"))]
  d[]
}))
consist_dist <- consist_dist[!is.na(distcode) & !is.na(provcode)]   
consist_dist <- unique(consist_dist, by = c("year", "provcode", "distcode"))  
consist_dist[, `:=`(provname = trimws(provname), distname = trimws(distname))]
setcolorder(consist_dist, c("year", "provcode", "provname", "distcode", "distname", "level"))

.cd_strip <- function(x, pref) trimws(stri_replace_first_regex(x, paste0("^(?i)(", pref, ")\\s+"), ""))
consist_dist[, `:=`(dname = .cd_strip(distname, "Quận|Huyện|Thị xã|Thành phố"),
                    pname = .cd_strip(provname, "Tỉnh|Thành phố"))]
.cd_clash <- consist_dist[, .(nc = uniqueN(distcode)), by = .(year, pname, dname)][
               nc > 1, unique(paste(pname, dname))]
consist_dist[paste(pname, dname) %in% .cd_clash, dname := paste0(dname, " (", level, ")")]

rm(.cd_strip, .cd_clash)

mcl_files <- list.files("Raw Data/MCL", pattern = "\\.csv$", full.names = T)
datetime_cols <- c("creation_time", "modified_time", "statistics.views_date_last_refreshed")

vnmap3 <- st_read("Raw Data/VNShapefile/gadm41_VNM_shp/gadm41_VNM_3.shp")
## OSM street directory (road x GADM-ward spatial join) — the slowest step. It is
## cached to Clean data/street_directory.Rds; DELETE that file to rebuild it from
## the shapefiles (e.g. after refreshing the OSM extracts).
if (file.exists("Clean data/street_directory.Rds")) {
  street_dir <- as.data.table(readRDS("Clean data/street_directory.Rds"))
} else {
  road_shps <- list.files("Raw Data/VNShapefile", pattern = "^gis_osm_roads_free_1\\.shp$",
                          recursive = T, full.names = T)
  street_dir <- rbindlist(lapply(road_shps, function(f)
    st_read(f, quiet = TRUE) %>%
      filter(!is.na(name)) %>%
      dplyr::select(name, fclass, ref) %>%
      st_join(vnmap3) %>%
      st_drop_geometry() %>%
      dplyr::select(name, fclass, ref, NAME_1, NAME_2, NAME_3)
  ), fill = TRUE)
  street_dir <- unique(street_dir[!is.na(name),
    .(street = name, fclass, ref, province = NAME_1, district = NAME_2, ward = NAME_3)])
  setorder(street_dir, province, district, ward, street)
  fwrite(street_dir,  "Clean data/street_directory.csv", bom = TRUE)
  saveRDS(street_dir, "Clean data/street_directory.Rds")
}

roads <- unique(street_dir[, .(name = street, NAME_1 = province, NAME_2 = district, NAME_3 = ward)])

mcl <- rbindlist(
  lapply(mcl_files, function(f) fread(f, colClasses = setNames(rep("character", length(datetime_cols)), datetime_cols))),
  fill = T
) %>%
  as.data.frame()
mcl$creation_time <- lubridate::ymd_hms(mcl$creation_time)
mcl$year <- lubridate::year(mcl$creation_time)

mcl <- mcl %>%
  select(-c(activities, is_branded_content, lang, match_type, mcl_url, modified_time, multimedia,
            post_owner.id, post_owner.type, post_owner.username,
            surface.id, statistics.views_date_last_refreshed, surface.type, shared_post_id)) 

mcl_main <- mcl %>%
#  filter(str_detect(text, regex("tuyển dụng|tuyendung|cần tìm", ignore_case = T)),
#         !is.na(text), text != "") %>%
  mutate(page = paste(surface.name, post_owner.name)) %>%
  arrange(creation_time) %>%                     
  distinct(text, .keep_all = T)               

JOBAD_ROLE <- paste0("(nhân\\s+viên|nv|thợ|ctv|cộng\\s+tác\\s+viên|kế\\s+toán|phục\\s+vụ|bảo\\s+vệ|",
  "lái\\s+xe|tài\\s+xế|pg|pb|công\\s+nhân|lao\\s+động|sinh\\s+viên|giáo\\s+viên|gia\\s+sư|",
  "thu\\s+ngân|lễ\\s+tân|đầu\\s+bếp|phụ\\s+bếp|pha\\s+chế|tạp\\s+vụ|giúp\\s+việc|shipper|",
  "k[ỹĩ]\\s+sư|k[ỹĩ]\\s+thuật(\\s+viên)?|ktv|bán\\s+hàng|kinh\\s+doanh|sales?|marketing|",
  "văn\\s+phòng|phiên\\s+dịch|biên\\s+dịch|trợ\\s+lý|thủ\\s+kho|giao\\s+hàng|",
  "lập\\s+trình(\\s+viên)?|tester|developer|designer|",
  "quản\\s+lý|trưởng|chuyên\\s+viên|thực\\s+tập|nam|nữ|người|bạn|vị\\s+trí)")
JOBAD_MAIN <- c(
  "mô\\s+tả\\s+(chi\\s+tiết\\s+)?công\\s+việc|nội\\s+dung\\s+công\\s+việc|công\\s+việc\\s*[:：]",
  "yêu\\s+cầu\\s*[:：]|yêu\\s+cầu\\s+(công\\s+việc|ứng\\s+viên|chung|tuyển)",
  "quyền\\s+lợi|đãi\\s+ngộ|phúc\\s+lợi",
  paste0("mức\\s+lương|lương\\s*[:：]|lương\\s+(cứng|cơ\\s+bản|căn\\s+bản|từ\\s*\\d|thỏa|thoả|cb|khởi\\s+điểm)|",
         "lương\\s+\\d|thu\\s+nhập\\s*[:：\\d]"),
  "số\\s+lượng\\s*[:：]?\\s*\\d|số\\s+lượng\\s+(tuyển|cần)",
  "vị\\s+trí\\s*[:：]|vị\\s+trí\\s+(tuyển|công\\s+việc|cần)|chức\\s+danh|chức\\s+vụ",
  "nộp\\s+hồ|gửi\\s+hồ\\s+sơ|hồ\\s+sơ\\s+(bao\\s+gồm|gồm|xin\\s+việc)|ứng\\s+tuyển|hạn\\s+(chót|nộp)|deadline",
  "thời\\s+gian\\s+làm\\s+việc|giờ\\s+làm|ca\\s+(sáng|chiều|tối|xoay|gãy|đêm)|theo\\s+ca|full\\s*time|part\\s*time",
  "(nơi|địa\\s+điểm|địa\\s+chỉ)\\s+làm\\s+việc|nơi\\s+làm|làm\\s+việc\\s+tại",
  paste0("(liên\\s+hệ|hotline|zalo|sđt|đt|đth)\\s*[:：]?[^\\n]{0,20}\\d{4,}|",
         "(?<!\\d)0\\d{2,4}[ .]?\\d{2,4}[ .]?\\d{3,5}(?![\\d])"),
  paste0("cần\\s+tuyển|tuyển\\s+gấp|báo\\s+tuyển|tin\\s+tuyển|",
         "(đang\\s+|nhu\\s+cầu\\s+)?tuyển(\\s+dụng)?\\s*[:：]?[^\\p{L}\\n]{0,8}(gấp\\s+)?",
         "(các\\s+|nhiều\\s+|một\\s+|\\d+\\s*[./)]?\\s*)?", JOBAD_ROLE, "|",
         "tuyển(\\s+dụng)?\\s*[:：]?\\s*\\n\\s*\\d+\\s*[./)]\\s*(các\\s+|nhiều\\s+)?", JOBAD_ROLE, "|",
         "cần\\s+(gấp\\s+)?(\\d+\\s+)?", JOBAD_ROLE))
JOBAD_PERI <- c(
  "địa\\s+chỉ\\s*[:：]",
  "\\d+\\s*(tr|triệu|k)\\s*/?\\s*(tháng|ca|ngày|giờ|h)(?![\\p{L}])",
  "(?<![\\p{L}])(ib|inbox)(?![\\p{L}])")
JOBAD_NEG <- c(
  "bí\\s+quyết|mẹo|cách\\s+(viết|trả\\s+lời|deal|gây\\s+ấn)|làm\\s+thế\\s+nào\\s+để|kinh\\s+nghiệm\\s+(phỏng\\s+vấn|xin\\s+việc)|câu\\s+hỏi\\s+phỏng\\s+vấn",
  "nhà\\s+tuyển\\s+dụng",
  "khai\\s+giảng|học\\s+phí|khoá\\s+học|khóa\\s+học|lớp\\s+học|chiêu\\s+sinh|tuyển\\s+sinh|học\\s+bổng",
  "phóng\\s+viên|theo\\s+báo|trả\\s+lời\\s+(báo|phỏng\\s+vấn\\s+của)|cho\\s+biết|khẳng\\s+định",
  "cuộc\\s+thi|vòng\\s+(loại|chung\\s+kết|sơ\\s+khảo)|hội\\s+thảo|workshop|talkshow|minigame|giveaway|give\\s+away",
  "truyện\\s+cười|siêu\\s+hài\\s+hước|thư\\s+giãn\\s+cuối\\s+tuần|mẩu\\s+chuyện\\s+(vui|cười)")
# job_page: poster is a dedicated job/recruitment page (name of page or owner).
JOBAD_PAGE <- paste0("việc\\s*làm|viec\\s*lam|tìm\\s*việc|tim\\s*viec|",
                     "tuyển\\s*dụng|tuyen\\s*dung|(?<![a-z])job|(?<![a-z])hr(?![a-z])|",
                     "vietnamworks|career|topcv|mywork")
JOBAD_STRONG <- paste0("báo\\s+tuyển|tin\\s+tuyển|",
                       "vị\\s+trí\\s+tuyển\\s+dụng|cần\\s+tuyển|tuyển\\s+gấp|",
                       "(đang\\s+|nhu\\s+cầu\\s+)?tuyển(\\s+dụng)?\\s*[:：]?[^\\p{L}\\n]{0,8}(gấp\\s+)?",
                       "(các\\s+|nhiều\\s+|một\\s+|\\d+\\s*[./)]?\\s*)?", JOBAD_ROLE, "|",
                       "tuyển(\\s+dụng)?\\s*[:：]?\\s*\\n\\s*\\d+\\s*[./)]\\s*(các\\s+|nhiều\\s+)?", JOBAD_ROLE)
JOBAD_NEG2 <- c(
  "khuyến\\s+mãi|giảm\\s+giá|ưu\\s+đãi|voucher|sale\\s+off",
  "tuyển\\s+(chồng|vợ|người\\s+yêu|bạn\\s+(trai|gái)(?![\\p{L}]))",
  "(muốn|cần|nhu\\s+cầu)\\s+(gửi|đăng)\\s+tin|đăng\\s+tin\\s+tuyển\\s+dụng\\s+vui\\s+lòng",
  "lừa\\s+đảo|cảnh\\s+báo|giả\\s+mạo|bóc\\s+phốt|đa\\s+cấp")
JOBAD_HASH <- paste0("#\\s*jobs?(?![\\p{L}])|",
                     "#\\s*(tin[\\s_]*)?tuyển[\\s_]*dụng|#\\s*(tin[\\s_]*)?tuyen[\\s_]*dung|",
                     "#\\s*việc[\\s_]*làm|#\\s*viec[\\s_]*lam")
.ja <- stri_trans_tolower(stri_trans_nfc(mcl_main$text))
.pgnm <- stri_trans_tolower(stri_trans_nfc(paste(mcl_main$surface.name, mcl_main$post_owner.name,
  fifelse(is.na(mcl_main$surface.username), "", mcl_main$surface.username))))
mcl_main$job_page <- as.integer(stri_detect_regex(.pgnm, JOBAD_PAGE))
.mn <- Reduce(`+`, lapply(JOBAD_MAIN, function(p) stri_detect_regex(.ja, p)))
.pn <- .mn + Reduce(`+`, lapply(JOBAD_PERI, function(p) stri_detect_regex(.ja, p)))
.nn <- Reduce(`+`, lapply(JOBAD_NEG, function(p) stri_detect_regex(.ja, p)))
.n2 <- Reduce(`+`, lapply(JOBAD_NEG2, function(p) stri_detect_regex(.ja, p)))
.stp <- stri_locate_first_regex(.ja, JOBAD_STRONG)[, 1]
.hh <- stri_detect_regex(.ja, JOBAD_HASH)
mcl_main$is_job_ad <- as.integer(
  (.pn >= 2 & .mn >= 1 & !(.nn >= 2 & .pn <= 3)) |
  (mcl_main$job_page == 1L & .mn >= 1 & .nn == 0 & .n2 == 0) |
  (!is.na(.stp) & .stp <= 250 & .nn == 0 & .n2 == 0) |
  (.hh & .nn == 0 & .n2 == 0) |
  stri_detect_regex(.pgnm, "canthoinfo") | stri_detect_fixed(.ja, "canthoinfo"))
rm(.ja, .pgnm, .mn, .pn, .nn, .n2, .stp, .hh)

mcl_main <- mcl_main %>% filter(is_job_ad == 1)

source("vn_district_match.R")

library(parallel)
match_districts_par <- function(text, page, g, prov, risky, wards,
                                ncores = max(1L, detectCores() - 1L)) {
  n <- length(text)
  if (ncores <= 1L || n < 5000L)
    return(match_districts(text, page, g, prov, risky = risky, wards = wards))
  grp <- splitIndices(n, ncores)                         # contiguous index blocks
  cl  <- makeCluster(ncores)
  on.exit(stopCluster(cl), add = TRUE)
  clusterCall(cl, function(d) { setwd(d); library(data.table); library(stringi)
                                source("vn_district_match.R"); TRUE }, getwd())
  parts <- clusterMap(cl,
    function(ii, t, p, g, prov, risky, wards) {
      m <- match_districts(t, p, g, prov, risky = risky, wards = wards)
      if (nrow(m)) m[, post := ii[post]]                 
      m
    },
    grp, lapply(grp, function(ii) text[ii]), lapply(grp, function(ii) page[ii]),
    MoreArgs = list(g = g, prov = prov, risky = risky, wards = wards))
  rbindlist(parts)[order(post)]
}

.cn <- function(x) vn_canon(x, FALSE)
.ds <- function(x) stri_replace_first_regex(trimws(x), "^(?i)(Quận|Huyện|Thị xã|Thành phố)\\s+", "")
.ps <- function(x) stri_replace_first_regex(trimws(x), "^(?i)(Tỉnh|Thành phố|Tp\\.?)\\s+", "")
.l11 <- fread("Raw Data/LFS/lfs_dist_11.csv", encoding = "UTF-8")[,
          .(tinh = as.integer(tinh), huyen = as.integer(huyen), provname, distname)]
.l11[, `:=`(pk = .cn(.ps(provname)), dk = .cn(.ds(distname)),
            dtype = .cn(stri_extract_first_regex(distname, "(?i)^(Quận|Huyện|Thị xã|Thành phố)")))]
.provL <- unique(.l11[, .(tinh, provname, pk)])[, .SD[1], by = tinh]      # one label per tinh
.provX <- unique(.l11[, .(pk, tinh)])
.l11d  <- unique(.l11[, .(tinh, dk, huyen)])[, .SD[1], by = .(tinh, dk)]
.look  <- function(t, d){ r <- .l11d[tinh == t & dk == d, huyen]; if (length(r)) r[1] else NA_integer_ }

.ch <- fread(text = "prov|child|parent
Hà Nội|Bắc Từ Liêm|Từ Liêm
Hà Nội|Nam Từ Liêm|Từ Liêm
Tuyên Quang|Lâm Bình|Nà Hang
Tuyên Quang|Na Hang|Nà Hang
Điện Biên|Nậm Pồ|Mường Nhé
Lai Châu|Nậm Nhùn|Mường Tè
Sơn La|Vân Hồ|Mộc Châu
Nghệ An|Hoàng Mai|Quỳnh Lưu
Quảng Bình|Ba Đồn|Quảng Trạch
Ninh Thuận|Thuận Nam|Ninh Phước
Kon Tum|Ia H' Drai|Sa Thầy
Gia Lai|Chư Pưh|Chư Sê
Bình Phước|Bù Gia Mập|Phước Long
Bình Phước|Phú Riềng|Phước Long
Bình Phước|Hớn Quản|Bình Long
Bình Dương|Bàu Bàng|Bến Cát
Bình Dương|Bắc Tân Uyên|Tân Uyên
Long An|Kiến Tường|Mộc Hóa
Bến Tre|Mỏ Cày Bắc|Mỏ Cày Nam
Kiên Giang|Giang Thành|Kiên Lương
Sóc Trăng|Trần Đề|Long Phú
Quảng Ninh|Quảng Yên|Yên Hưng
Bình Phước|Đồng Phú|Đồng Phù
Bình Định|Quy Nhơn|Qui Nhơn", sep = "|")
.ch[, `:=`(pk = .cn(prov), ck = .cn(child), rk = .cn(parent))]
.par <- function(p, d){ r <- .ch[pk == p & ck == d, rk]; if (length(r)) r[1] else NA_character_ }

.G <- unique(consist_dist[, .(pk = .cn(.ps(provname)), dk = .cn(.ds(distname)),
                              distname = .ds(distname), lvl = stri_trans_tolower(trimws(level)))])
.G <- merge(.G, .provX, by = "pk", all.x = TRUE)
.res <- as.integer(mapply(.look, .G$tinh, .G$dk))                        
for (.i in which(is.na(.res) & !is.na(.G$tinh))) {                       
  .pr <- .par(.G$pk[.i], .G$dk[.i]); if (!is.na(.pr)) .res[.i] <- .look(.G$tinh[.i], .pr) }
.G[, huyen := .res]
.prim <- unique(.l11[, .(tinh, huyen, distname = .ds(distname), dname = dk, split_of = NA_character_, dtype)])
.ali  <- unique(.G[!is.na(huyen), .(tinh, huyen, distname, dname = dk, split_of = "gso", dtype = lvl)])
gaz  <- merge(rbind(.prim, .ali), .provL[, .(tinh, provname, pname = pk)], by = "tinh", all.x = TRUE)
gaz  <- unique(gaz[dname != ""])[, .SD[1], by = .(tinh, dname)]          # primary listed first -> wins
setcolorder(gaz, c("tinh", "huyen", "provname", "distname", "dname", "pname", "dtype", "split_of"))
stopifnot(nrow(gaz[stri_detect_regex(dname, "^[0-9]{1,2}$")]) == 12L,    # HCMC numbered quận
          all(gaz[stri_detect_regex(dname, "^[0-9]{1,2}$"), tinh] == 79L),
          nrow(unique(gaz[, .(tinh, huyen)])[!unique(.l11[, .(tinh, huyen)]), on = c("tinh","huyen")]) == 0L)
prov  <- build_provinces(gaz)
wards <- build_wards(gaz, sf::st_drop_geometry(vnmap3))
rm(.cn, .ds, .ps, .l11, .provL, .provX, .l11d, .look, .ch, .par, .G, .res, .prim, .ali)

mcl_main <- mcl_main %>% mutate(page = paste(surface.name, post_owner.name))

pass1 <- match_districts_par(mcl_main$text, mcl_main$page, gaz, prov, risky = character(), wards = wards)
risky <- union(RISKY_DEFAULT, derive_risky(pass1))
message("risky names dropped from the bare tier: ", paste(risky, collapse = ", "))

# risky names identified: lý nhân, thống nhất, đức thọ, yên bình,
# tân sơn, minh long, an phú, kế sách, vĩnh hưng, tân trụ, tân hưng
# these match as districts only when supported by a district prefix
# (Quận/Huyện/TP/TX/Q/H) OR a province cue (province in the post text or page);
# a context-free mention is ignored

mcl_dist <- match_districts_par(mcl_main$text, mcl_main$page, gaz, prov, risky = risky, wards = wards)
mcl_dist[, `:=`(id   = mcl_main$id[post],
                year = mcl_main$year[post])]

prov_lab <- unique(gaz[is.na(split_of), .(tinh, provname)])
mcl_dist[is.na(provname), provname := prov_lab$provname[match(tinh, prov_lab$tinh)]]

print(mcl_dist[, .N, by = .(evidence, conf)][order(-N)])

fwrite(mcl_dist, "mcl_district_panel.csv")

NAMED_IN_TEXT <- c("prov_adjacent", "prov_in_text", "numbered",
                   "prefix_only", "prov_in_page", "prefix_typed",
                   "prefix_conflict", "bare", "ward", "ascii_marked", "abbrev")
CONF_RANK <- c(low = 1L, medium = 2L, high = 3L)

dist_rows <- mcl_dist[evidence %in% NAMED_IN_TEXT]
dist_rows[, conf_rank := CONF_RANK[conf]]

pbest <- mcl_dist[, .(prov_tinh = if (uniqueN(tinh) == 1L) tinh[1] else NA_integer_), by = post]

long_d <- dist_rows[order(post, pos, na.last = TRUE),
                    .SD[1], by = .(post, huyen),
                    .SDcols = c("tinh", "distname", "evidence", "conf", "pos")]
setnames(long_d, c("distname", "evidence", "conf"),
                 c("district", "dist_evidence", "dist_conf"))

no_d <- data.table(post = setdiff(seq_len(nrow(mcl_main)), unique(long_d$post)))
no_d[pbest, on = "post", tinh := i.prov_tinh]      
no_d[, `:=`(huyen = NA_integer_, district = NA_character_,
            dist_evidence = NA_character_, dist_conf = NA_character_, pos = NA_integer_)]

mcl_long <- rbind(long_d, no_d, use.names = TRUE)[order(post, pos, na.last = TRUE)]
mcl_long[, n_districts := sum(!is.na(huyen)), by = post]
mcl_long[, province := prov_lab$provname[match(tinh, prov_lab$tinh)]]

n_posts  <- nrow(mcl_main)
mcl_main <- mcl_main %>% select(-starts_with("V"))
mcl_main <- as.data.table(cbind(mcl_main[mcl_long$post, ], mcl_long[, !"post"]))
mcl_main[, post_row := seq_len(.N), by = id]

STAT_COLS <- grep("^statistics\\.", names(mcl_main), value = TRUE)
dup_rows  <- which(mcl_main$post_row > 1L)
for (cl in STAT_COLS) {
  v <- mcl_main[[cl]]
  set(mcl_main, i = dup_rows, j = cl, value = if (is.character(v)) NA_character_ else NA)
}

setcolorder(mcl_main, c("id", "post_row", "n_districts", "year",
                        "tinh", "huyen", "district", "province",
                        "dist_evidence", "dist_conf"))

WORK  <- "(nơi|địa\\s+điểm|địa\\s+chỉ|khu\\s+vực)\\s+làm\\s+việc|làm\\s+việc\\s+tại|làm\\s+tại"
APPLY <- "nộp\\s+hồ\\s+sơ|gửi\\s+hồ\\s+sơ|nhận\\s+hồ\\s+sơ|phỏng\\s+vấn\\s+(trực\\s+tiếp\\s+)?tại|liên\\s+hệ"
HQ    <- "trụ\\s+sở|văn\\s+phòng\\s+(chính|công\\s+ty|đại\\s+diện)"
ids_with_d <- unique(mcl_main[!is.na(huyen) & !is.na(pos)]$id)
pp <- mcl_main[post_row==1L & id %in% ids_with_d, .(id, text)]
tc <- vn_canon(pp$text)
wl <- stri_locate_all_regex(tc, WORK,  omit_no_match=TRUE)
al <- stri_locate_all_regex(tc, APPLY, omit_no_match=TRUE)
hl <- stri_locate_all_regex(tc, HQ,    omit_no_match=TRUE)
names(wl) <- names(al) <- names(hl) <- as.character(pp$id)
classify_cue <- function(i, p, win=150L){
  best <- ""; bd <- win + 1L
  for (nm in c("work","apply","hq")){
    cu <- switch(nm, work=wl[[i]], apply=al[[i]], hq=hl[[i]])
    if (!is.null(cu) && nrow(cu)){
      dd <- p - cu[,2]; dd <- dd[dd > 0 & dd <= win]
      if (length(dd) && min(dd) < bd){ bd <- min(dd); best <- nm }
    }
  }
  best
}
mcl_main[, loc_cue := ""]
.i <- which(!is.na(mcl_main$huyen) & !is.na(mcl_main$pos))
mcl_main[.i, loc_cue := mapply(classify_cue, as.character(id), pos)]

## (2) re-rank: work-cue district first; stats follow to the new row 1
.st <- mcl_main[post_row==1L, c("id", STAT_COLS), with=FALSE]
mcl_main[, rk := fifelse(loc_cue=="work" & !is.na(huyen), 0L, 1L)]
setorderv(mcl_main, c("id","rk","pos"), na.last=TRUE)
mcl_main[, post_row := seq_len(.N), by=id][, rk := NULL]
for (cl in STAT_COLS) set(mcl_main, j=cl, value=NA)
mcl_main[post_row==1L, (STAT_COLS) := .st[.SD, on="id", mget(paste0("i.", STAT_COLS))]]

## (3) KCN gazetteer — authoritative SEZ / industrial-zone list from
## Vietnam_SEZ_Metadata.xlsx 
.d1 <- mcl_main[post_row==1L, .(id, text, n_districts, tinh)]
.af <- function(x) stri_trans_tolower(stri_trim_both(stri_trans_general(x, "Latin-ASCII")))
.sez <- as.data.table(readxl::read_excel("Raw Data/VNShapefile/Vietnam_SEZ_Metadata.xlsx",
                                         sheet = "SEZ metadata", col_types = "text"))
setnames(.sez, 1:6, c("code","kname","kprov","kdist","kaddr","ktype"))
.sez <- .sez[!is.na(kname) & kname != "SEZ name"]
.sez[, kc := .af(stri_replace_first_regex(kname,
  "(?i)^(Industrial (Zone|Park)|Special Economic Zone|(Coastal|Cross-border|Border) Economic Zone|Economic Zone)\\s+", ""))]
.gzf <- unique(gaz[, .(tinh, huyen,
          pf = .af(stri_replace_first_regex(provname, "(?i)^(Tỉnh|Thành phố|Tp\\.?)\\s+", "")),
          df = .af(dname))])[, .SD[1], by = .(pf, df)]
.sez[, pf := .af(kprov)]
.sez[, dfcol := .af(stri_replace_first_regex(kdist, "(?i)^(TP|TX|Q|H|Thanh pho|Thi xa|Quan|Huyen)[\\.\\s]+", ""))]
.sez[, rid := .I]
## district(s) named in the Vietnamese Address field are more granular/reliable than
## the District column; use them, falling back to the column only where the address
## names none. A multi-district address yields several rows, which the province-gated
## matcher below then treats as ambiguous (and leaves unfilled).
.dmk <- "(?i)(?:huyện|thị\\s*xã|quận|thành\\s*phố|tp|tx)\\.?\\s+([\\p{L}][\\p{L} ]+?)(?=\\s*[,.;()]|\\s+tỉnh|\\s+t\\.|$)"
.sez[, adl := lapply(stri_match_all_regex(kaddr, .dmk, omit_no_match = TRUE),
                     function(m) if (nrow(m)) unique(.af(trimws(m[, 2]))) else character())]
.szl <- .sez[, .(df = if (length(adl[[1]])) adl[[1]] else dfcol), by = .(rid, pf, kc)]
kcn_gaz <- unique(merge(.szl[df != ""], .gzf, by = c("pf","df"))[kc != "" & stri_length(kc) >= 4L, .(kcn = kc, tinh, huyen)])
## VSIP is written bare in ads ("KCN VSIP") but stored per-site in the xlsx
## ("VSIP Quảng Ngãi"); register a bare "vsip" for each province with exactly ONE
## VSIP-family district, so a bare mention resolves province-gated (ambiguous
## multi-VSIP provinces like Bình Dương are left as-is).
.vsip <- unique(kcn_gaz[stri_detect_fixed(kcn, "vsip"), .(tinh, huyen)])
.vsip <- .vsip[, if (uniqueN(huyen) == 1L) .(kcn = "vsip", huyen = huyen[1]), by = tinh]
kcn_gaz <- unique(rbind(kcn_gaz, .vsip[, .(kcn, tinh, huyen)]))
.lab <- unique(gaz[is.na(split_of), .(tinh, huyen, district = distname, province = provname)])[, .SD[1], by = .(tinh, huyen)]
fwrite(merge(kcn_gaz, .lab, by = c("tinh","huyen"))[order(province, district, kcn)],
       "Clean data/kcn_gazetteer.csv", bom = TRUE)

.rxk <- paste0("(?<![\\p{L}])(?:kcn|khu\\s+cong\\s+nghiep|kcx|khu\\s+che\\s+xuat|vsip)\\s+(",
               paste(.esc(unique(kcn_gaz$kcn)[order(-stri_length(unique(kcn_gaz$kcn)))]), collapse = "|"),
               ")(?![\\p{L}])")
.km <- stri_match_all_regex(.af(vn_canon(.d1$text)), .rxk, omit_no_match = TRUE)
.d1[, khit := lapply(.km, function(m) if (length(m)) unique(m[, 2]) else character())]

mcl_main[, `:=`(dist_pred=NA_character_, huyen_pred=NA_integer_, pred_source=NA_character_)]
.kl <- .d1[n_districts==0 & lengths(khit) > 0, .(kcn = unlist(khit)), by = .(id, ptinh = tinh)]
.kl <- merge(.kl, kcn_gaz, by = "kcn", allow.cartesian = TRUE)
.kf <- .kl[, {
  inp <- if (!is.na(ptinh[1])) .SD[tinh == ptinh[1]] else .SD[0L]
  if (nrow(inp) && uniqueN(inp$huyen) == 1L)   .(tinh = inp$tinh[1], huyen = inp$huyen[1])
  else if (uniqueN(paste(tinh, huyen)) == 1L)  .(tinh = tinh[1],     huyen = huyen[1])
  else                                         .(tinh = NA_integer_, huyen = NA_integer_)
}, by = id][!is.na(huyen)]
if (nrow(.kf)){
  .fill <- merge(.kf, .lab, by = c("tinh","huyen"))
  mcl_main[.fill, on = "id", `:=`(tinh = i.tinh, province = i.province,
           dist_pred = i.district, huyen_pred = i.huyen, pred_source = "kcn")]
}
message("loc_cue work rows: ", sum(mcl_main$loc_cue=="work"),
        " | KCN gazetteer: ", uniqueN(kcn_gaz$kcn), " parks | posts filled via KCN: ", nrow(.kf))

RX_ST <- paste0(
  "(?<![\\p{L}\\d/])(?!(?:19|20)\\d\\d(?![\\d]))\\d{1,4}[a-z]?(?:\\s*/\\s*\\d+[a-z]?){0,2}\\s+",
  "(?:đường\\s+|phố\\s+|đại\\s+lộ\\s+)?",
  "([\\p{L}]+(?:\\s+[\\p{L}]+){1,3})",
  "(?=\\s*(?:¦|$|phường|quận|huyện|tx|tp|khu)|\\s*\\d)")
.name_of <- function(v){
  v <- stri_replace_first_regex(v, "^\\d{1,4}[a-z]?(\\s*/\\s*\\d+[a-z]?){0,2}\\s+", "")
  v <- stri_replace_first_regex(v, "^(đường|phố|đại\\s+lộ)\\s+", "")
  stri_trim_both(v) }
ST_STOP1 <- c("tầng","phòng","lô","km","số","ngõ","hẻm","toà","tòa","đt","tel","kcn","kdc","ấp","thôn","xã")
ST_STOPTOK <- c("triệu","nghìn","vnđ","vnd","usd","tr","k","đ","sáng","trưa","chiều","tối","đêm",
             "tháng","ngày","giờ","tuần","năm","tuổi","phút","tiếng","người","bạn","vị","trí",
             "ca","chi","nhánh","đến","từ","và","hoặc","làm","việc","lương","thưởng","doanh",
             "lượng","nhân","viên","gần","nhất","trở","lên","cái","chiếc","suất","buổi","cách","giỏi")
.d2 <- mcl_main[post_row==1L, .(id, text, n_districts, tinh)]
.tc2 <- vn_canon(.d2$text)
.stx <- stri_extract_all_regex(.tc2, RX_ST, omit_no_match=TRUE)
.d2[, streets := lapply(.stx, function(v){
      x <- unique(.name_of(v))
      x <- x[!stri_extract_first_regex(x, "^[\\p{L}]+") %in% ST_STOP1 & stri_count_fixed(x," ") >= 1]
      bad <- vapply(stri_split_fixed(x, " "), function(tk) any(tk %in% ST_STOPTOK), logical(1))
      x[!bad] })]
.d2[, n_street := lengths(streets)]
## explicitly-marked streets ("đường/phố/đại lộ X", no house number needed) — catches
## "Số 566, đường Núi Thành" (a separator before đường) and streets whose name is also
## a district name (resolved below against .rgz_m, which keeps them). "thành phố" excluded.
RX_STM <- "(?<![\\p{L}])(?<!thành )(?:đường|phố|đại\\s+lộ)\\s+([\\p{L}]+(?:\\s+[\\p{L}]+){0,3})(?=\\s*(?:¦|$|phường|quận|huyện|tx|tp|khu)|\\s*\\d)"
.smx <- stri_match_all_regex(.tc2, RX_STM, omit_no_match=TRUE)
.d2[, mstreets := lapply(.smx, function(m){ if (!nrow(m)) return(character())
      x <- unique(stri_trim_both(m[,2]))
      x <- x[!stri_extract_first_regex(x, "^[\\p{L}]+") %in% ST_STOP1 & stri_count_fixed(x," ") >= 1]
      bad <- vapply(stri_split_fixed(x, " "), function(tk) any(tk %in% ST_STOPTOK), logical(1))
      x[!bad] })]
.d2[, n_mstreet := lengths(mstreets)]

.dn <- unique(mcl_main[!is.na(huyen), .(tinh,
        dname = vn_canon(stri_replace_first_regex(district,
          "^(Quận|Huyện|Thị\\s+xã|Thành\\s+phố)\\s+", ""), hard_sep=FALSE))])
.pn <- unique(mcl_main[!is.na(tinh) & !is.na(province), .(tinh, dname = vn_canon(province, hard_sep=FALSE))])
.tg2 <- .d2[n_street>0 & n_districts==0 & !is.na(tinh), .(id, tinh, streets)][
          , .(street = unlist(streets)), by=.(id, tinh)]
.lab2 <- unique(mcl_main[!is.na(huyen), .(tinh, huyen, district, province)])

.cn2 <- function(x){ x <- stri_replace_all_regex(x, "[-–—]", " ")
  x <- stri_replace_all_regex(x, "\\s+", " "); vn_canon(stri_trim_both(x), hard_sep=FALSE) }
.rg <- as.data.table(roads)[!is.na(name)]
.rg[, pc := .cn2(stri_replace_first_regex(NAME_1, "^(Tp|TP|Thành\\s+phố|Tỉnh)\\s+", ""))]
.pl <- unique(gaz[, .(tinh, provname)])
.pl[, pc := .cn2(stri_replace_first_regex(provname, "^(Tp|TP|Thành\\s+phố|Tỉnh)\\s+", ""))]
.rg <- merge(.rg, .pl[, .(pc, tinh)], by="pc")
.rg[, dc := .cn2(stri_replace_first_regex(NAME_2, "^(Quận|Huyện|Thị\\s+xã|Thành\\s+phố)\\s+", ""))]
.gn <- unique(gaz[, .(tinh, huyen,
        dc = .cn2(stri_replace_first_regex(distname, "^(Quận|Huyện|Thị\\s+xã|Thành\\s+phố)\\s+", "")))])
.gn <- unique(.gn, by=c("tinh","dc"))
.rg <- merge(.rg, .gn, by=c("tinh","dc"))
.rg <- .rg[!stri_detect_regex(tolower(name),
        "^(quốc\\s*l[ộô]|tỉnh\\s*l[ộô]|hương\\s*l[ộô]|cao\\s*tốc|xa\\s*lộ|ql|tl|đt|ct|ah)\\s*\\d")]
.rg[, rc := .cn2(stri_replace_first_regex(name, "^(?i)(đường|duong|phố|pho|đại\\s+lộ)\\s+", ""))]
.rg <- .rg[stri_detect_regex(rc, "^[\\p{L}]+( [\\p{L}]+){1,3}$")]
.rgz <- unique(.rg[, .(rc, tinh, huyen)])
.rgz <- .rgz[, if (.N == 1L) .SD, by=.(rc, tinh)]                 # unique within province
.rgz_m <- .rgz[!rc %in% .pn$dname]               # MARKED streets keep district-named ones (đường Núi Thành)
.rgz   <- .rgz[!rc %in% c(.dn$dname, .pn$dname)] # UNMARKED: no district/province names
## street->district comes from OSM alone (unique within province); no teacher-post
## veto here — a firm's named job-district need not be the street's district.
.tg3 <- .tg2                                # all street posts eligible
.tgm <- .d2[n_mstreet>0 & n_districts==0 & !is.na(tinh), .(street = unlist(mstreets)), by=.(id, tinh)]
.fl3 <- unique(rbind(
          merge(.tg3, .rgz,   by.x=c("street","tinh"), by.y=c("rc","tinh"))[, .(id, tinh, huyen)],
          merge(.tgm, .rgz_m, by.x=c("street","tinh"), by.y=c("rc","tinh"))[, .(id, tinh, huyen)]))
.fl3 <- .fl3[, .(huyen = if (uniqueN(huyen)==1L) huyen[1] else NA_integer_), by=.(id, tinh)][!is.na(huyen)]
.fl3 <- .fl3[!id %in% mcl_main[!is.na(pred_source), id]]   # kcn preds keep priority
if (nrow(.fl3)){
  .fl3 <- merge(.fl3, .lab2, by=c("tinh","huyen"))
  mcl_main[.fl3, on="id", `:=`(dist_pred=i.district, huyen_pred=i.huyen,
           pred_source="osr")]
}
message("road gazetteer: ", nrow(.rgz), " roads",
        " | posts filled via road_gaz: ", nrow(.fl3))

.rw <- as.data.table(roads)[!is.na(name) & !is.na(NAME_3)]
.rw[, pc := .cn2(stri_replace_first_regex(NAME_1, "^(Tp|TP|Thành\\s+phố|Tỉnh)\\s+", ""))]
.rw <- merge(.rw, .pl[, .(pc, tinh)], by="pc")
.rw[, dc := .cn2(stri_replace_first_regex(NAME_2, "^(Quận|Huyện|Thị\\s+xã|Thành\\s+phố)\\s+", ""))]
.rw <- merge(.rw, .gn, by=c("tinh","dc"))
.rw <- .rw[!stri_detect_regex(tolower(name),
        "^(quốc\\s*l[ộô]|tỉnh\\s*l[ộô]|hương\\s*l[ộô]|cao\\s*tốc|xa\\s*lộ|ql|tl|đt|ct|ah)\\s*\\d")]
.rw[, rc := .cn2(stri_replace_first_regex(name, "^(?i)(đường|duong|phố|pho|đại\\s+lộ)\\s+", ""))]
.rw <- .rw[stri_detect_regex(rc, "^[\\p{L}]+( [\\p{L}]+){1,3}$")]
.rw[, wc := .cn2(stri_replace_first_regex(NAME_3, "^(Phường|Xã|Thị\\s+trấn)\\s+", ""))]
.rw[, wc := stri_replace_first_regex(wc, "^0+(?=[0-9])", "")]
.pw0 <- unique(.rw[wc != "" & !rc %in% c(.dn$dname, .pn$dname), .(rc, wc, tinh, huyen)])
.pwp <- .pw0[, if (.N == 1L) .SD, by=.(rc, wc, tinh)]              # unique within province
.pwn <- .pw0[, if (uniqueN(paste(tinh, huyen)) == 1L) .SD[1], by=.(rc, wc)]  # nationally unique
.v3 <- as.data.table(sf::st_drop_geometry(vnmap3))[, .(NAME_1, NAME_2, NAME_3)]
.v3[, pc := .cn2(stri_replace_first_regex(NAME_1, "^(Tp|TP|Thành\\s+phố|Tỉnh)\\s+", ""))]
.v3 <- merge(.v3, .pl[, .(pc, tinh)], by="pc")
.v3[, dc := .cn2(stri_replace_first_regex(NAME_2, "^(Quận|Huyện|Thị\\s+xã|Thành\\s+phố)\\s+", ""))]
.v3 <- merge(.v3, .gn, by=c("tinh","dc"))
.v3[, wc := .cn2(stri_replace_first_regex(NAME_3, "^(Phường|Xã|Thị\\s+trấn)\\s+", ""))]
.wg <- unique(.v3[!stri_detect_regex(wc, "^[0-9]+$") & stri_length(wc) >= 6, .(wc, tinh, huyen)])
.wg <- .wg[, if (.N == 1L) .SD, by=.(wc, tinh)]
## ward mentions in posts (marker required; numeric wards with phường/p only)
.wnm <- setdiff(unique(c(.pw0$wc, .wg$wc)), as.character(0:99))
.wnm <- .wnm[stri_length(.wnm) >= 4]
.rxwn <- paste0("(?<![\\p{L}])(?:phường|xã|thị\\s+trấn|p|tt)\\s*¦?\\s*(",
                paste(.esc(.wnm[order(-stri_length(.wnm))]), collapse="|"), ")(?![\\p{L}\\p{N}])")
.rxwu <- "(?<![\\p{L}\\p{N}])(?:phường|p)\\s*¦?\\s*0?([1-9][0-9]?)(?![0-9])"
.wn <- stri_match_all_regex(.tc2, .rxwn, omit_no_match=TRUE)
.wu <- stri_match_all_regex(.tc2, .rxwu, omit_no_match=TRUE)
.d2[, wardm := mapply(function(a, b) unique(c(if (length(a)) a[,2], if (length(b)) b[,2])),
                      .wn, .wu, SIMPLIFY=FALSE)]

.el <- .d2[n_districts==0 & !id %in% mcl_main[!is.na(pred_source), id]]
.f4 <- .el[!is.na(tinh) & n_street>0 & lengths(wardm)>0,
           CJ(rc=unlist(streets), wc=unlist(wardm), unique=TRUE), by=.(id, tinh)]
.f4 <- merge(.f4, .pwp, by=c("rc","wc","tinh"))
.f4 <- .f4[, .(huyen = if (uniqueN(huyen)==1L) huyen[1] else NA_integer_), by=.(id,tinh)][!is.na(huyen)]
.f5 <- .el[is.na(tinh) & n_street>0 & lengths(wardm)>0,
           CJ(rc=unlist(streets), wc=unlist(wardm), unique=TRUE), by=id]
.f5 <- merge(.f5, .pwn, by=c("rc","wc"))
.f5 <- .f5[, .(dt=uniqueN(paste(tinh,huyen)), tinh=tinh[1], huyen=huyen[1]), by=id][dt==1L][, dt := NULL]
.f6 <- .el[!is.na(tinh) & lengths(wardm)>0, .(wc=unlist(wardm)), by=.(id,tinh)]
.f6 <- merge(.f6, .wg, by=c("wc","tinh"))
.f6 <- .f6[, .(huyen = if (uniqueN(huyen)==1L) huyen[1] else NA_integer_), by=.(id,tinh)][!is.na(huyen)]
.f6 <- .f6[!id %in% .f4$id]
if (nrow(.f4)){ .f4 <- merge(.f4, .lab2, by=c("tinh","huyen"))
  mcl_main[.f4, on="id", `:=`(dist_pred=i.district, huyen_pred=i.huyen, pred_source="osrw")] }
if (nrow(.f5)){ .f5 <- merge(.f5, .lab2, by=c("tinh","huyen"))
  mcl_main[.f5, on="id", `:=`(tinh=i.tinh, province=i.province,
           dist_pred=i.district, huyen_pred=i.huyen, pred_source="osrw")] }
if (nrow(.f6)){ .f6 <- merge(.f6, .lab2, by=c("tinh","huyen"))
  mcl_main[.f6, on="id", `:=`(dist_pred=i.district, huyen_pred=i.huyen, pred_source="ward3")] }
message("road+ward pairs: ", nrow(.pwp), " prov-keyed / ", nrow(.pwn), " national",
        " | ward3 keys: ", nrow(.wg),
        " | fills: osrw ", nrow(.f4), "+", nrow(.f5), " | ward3 ", nrow(.f6))
rm(.st,.d1,.af,.sez,.gzf,.dmk,.szl,.vsip,kcn_gaz,.rxk,.km,.kl,.kf,.lab,.i, wl, al, hl, tc, pp,
   .d2,.tc2,.stx,.smx,.dn,.pn,.tg2,.tgm,.lab2,.name_of,
   .cn2,.rg,.pl,.gn,.rgz,.rgz_m,.tg3,.fl3,
   .rw,.pw0,.pwp,.pwn,.v3,.wg,.wnm,.rxwn,.rxwu,.wn,.wu,
   .el,.f4,.f5,.f6)

## female-labour cue. EVERY bare-"nữ" ending carries (?![\p{L}]) so the very
## common word "nữa" (moreover/also) never matches. Covers nữ after OR before a
## role noun, "bạn nữ", "là nữ", "title: nữ", "(nữ)", and "nữ" + requirement.
NB <- "(?![\\p{L}])"
RX_FEMALE <- paste0("(?<![\\p{L}])(",
  "tuyển\\s+(dụng\\s+)?(\\d+\\s+)?nữ",NB,"|cần\\s+(tuyển\\s+)?(\\d+\\s+)?nữ",NB,"|nhận\\s+nữ",NB,"|",
  "\\d+\\s+(bạn\\s+)?nữ",NB,"|(các\\s+)?bạn\\s+nữ",NB,"|",                          # "12 nữ", "bạn nữ"
  "nữ\\s+(nhân\\s+viên|nv|lao\\s+động|công\\s+nhân|ứng\\s+viên|cử\\s+nhân|sinh\\s+viên|ctv|nhân\\s+sự|part|full|pg|pb)|",  # nữ + role
  "(nhân\\s+viên|lao\\s+động|công\\s+nhân|nhân\\s+công|lễ\\s+tân|kế\\s+toán|phục\\s+vụ|thu\\s+ngân|ứng\\s+viên|giúp\\s+việc|tạp\\s+vụ|bán\\s+hàng|thư\\s+ký|nhân\\s+sự|pg|pb)\\s*[:：/\\-]?\\s*nữ",NB,"|",  # role (:/-) nữ
  "ưu\\s+tiên\\s+(ứng\\s+viên\\s+|các\\s+bạn\\s+|)?(là\\s+)?nữ",NB,"|chỉ\\s+(tuyển\\s+|nhận\\s+)?nữ",NB,"|(dự\\s+tuyển\\s+|ứng\\s+viên\\s+)?là\\s+nữ",NB,"|",
  "giới\\s+tính\\s*[:：]?\\s*nữ",NB,"|yêu\\s+cầu\\s*[:：]?\\s*nữ",NB,"|nữ\\s+giới|\\(\\s*nữ\\s*\\)|",  # (nữ)
  ## "nữ" + punctuation run (spaces allowed between marks: "+ Nữ. + Tốt nghiệp")
  ## + a requirement continuation word
  "nữ\\s*([.,:;()/+\\-]\\s*)+(tuổi|từ|độ\\s*tuổi|cao|ngoại\\s*hình|tốt|trình|có|biết|giao|nhanh|chăm|độc|ưu|yêu|kinh|áo|trên|\\d)|",
  "nữ\\s+(tuổi|từ\\s*\\d|cao\\s*(từ\\s*)?\\d|tốt\\s+nghiệp|\\d)",
  ")")

## both-gender marker: nam & nữ joined by punctuation, spaces, OR a small number
RX_BOTH <- paste0("(?<![\\p{L}])(",
  "nam\\s*[,/.\\-\\d\\s]{0,4}\\s*(và\\s+|hoặc\\s+|hay\\s+)?nữ|",
  "nữ\\s*[,/.\\-\\d\\s]{0,4}\\s*(và\\s+|hoặc\\s+|hay\\s+)?nam",
  ")(?![\\p{L}])")
RX_MALE <- paste0("(?<![\\p{L}])(",
  "tuyển\\s+(dụng\\s+)?nam(?![\\p{L}])|nam\\s*[:：]?\\s*(cao|>\\s*\\d|≥\\s*\\d)|nam\\s*[:：]\\s*\\d|",
  "nam\\s+từ\\s+\\d|nam\\s+\\d+\\s*(-|đến|tuổi)|",
  "\\d+\\s+nam(?![\\p{L}])|(nhân\\s+viên|nv|lễ\\s+tân|phục\\s+vụ|bảo\\s+vệ|công\\s+nhân|lao\\s+động)\\s+nam(?![\\p{L}])|",
  "giới\\s+tính\\s*[:：]?\\s*nam(?![\\p{L}])|nam\\s+giới|ưu\\s+tiên\\s+nam(?![\\p{L}])",
  ")")

## formal: 1 if the job shows any marker of FORMAL employment. Exhaustive union
## of the canonical formal-sector signals (ILO-style: social insurance OR written
## contract, plus permanent status):
##  - social/health/unemployment insurance: BHXH/BHYT/BHTN/BHLĐ, bảo hiểm (paid)
##  - permanent/official status: biên chế, (trở thành) nhân viên chính thức
##  - union / labour-law coverage: công đoàn, theo (bộ) luật lao động
##  - severance: trợ cấp thôi việc / thất nghiệp
RX_FORMAL <- paste0(
  "(?<![\\p{L}])bh\\s*[/.,\\-]?\\s*(xh|yt|tn)(?![\\p{L}])",
  "|bảo\\s+hiểm\\s+(xã\\s+hội|y\\s+tế|thất\\s+nghiệp|lao\\s+động|sức\\s+khỏe)",
  "|bảo\\s+hiểm\\s*[:：]?\\s*(yt|xh|tn)(?![\\p{L}])",
  "|(đóng|tham\\s+gia|hưởng|hỗ\\s+trợ|đảm\\s+bảo|chế\\s+độ|phúc\\s+lợi|quyền\\s+lợi|các\\s+loại|mua|trích|nộp)",
    "\\s*[:：]?\\s+([^.!?\\n]{0,30}\\s+)?bảo\\s+hiểm",
  "|[+]\\s*bảo\\s+hiểm",                                    
  "|bảo\\s+hiểm\\s+(đầy\\s+đủ|theo\\s+quy\\s+định|theo\\s+luật|bắt\\s+buộc)",
  "|bảo\\s+hiểm\\s*[:：]\\s*(y\\s+tế|xã\\s+hội|sức\\s+khỏe)",
  "|(theo\\s+)?(bộ\\s+)?luật\\s+lao\\s+động",
  "|trợ\\s+cấp\\s+(thôi\\s+việc|thất\\s+nghiệp)",
  ## bare "BH" needs a benefit cue; small gap covers "hưởng các khoản BH"
  "|(đóng|hưởng|tham\\s+gia|chế\\s+độ|được|có|nộp|trích)\\s*[:：]?\\s+([^.!?\\n]{0,12}\\s+)?bh(?![\\p{L}])",
  "|(?<![\\p{L}])bh\\s+(đầy\\s+đủ|y\\s+tế|xã\\s+hội|sức\\s+khỏe|theo\\s+quy\\s+định)")

## taxid: 1 if the post names a REGISTERED legal entity, which by law has an
## enterprise code = tax code (mã số thuế). Exhaustive set of registration
## signals: TNHH (LLC), công ty cổ phần / CP / hợp danh (JSC / partnership),
## doanh nghiệp tư nhân, tập đoàn, tổng công ty, English forms (Co.,Ltd / JSC /
## Corp), hợp tác xã / HTX (cooperative), xí nghiệp, văn phòng đại diện, foreign-
## invested (100% vốn / FDI / vốn nước ngoài), an explicit business licence (GPKD
## / ĐKKD / giấy phép kinh doanh) or tax code, and nhà máy (factory). ~29%.
## Marker of a FORMAL firm (distinct from `formal`, the job's terms). 
RX_TAXID <- paste0(
  "(?<![\\p{L}])tnhh(?![\\p{L}])",
  "|công\\s+ty\\s+(cổ\\s+phần|cp(?![\\p{L}])|hợp\\s+danh)",
  "|cổ\\s+phần|doanh\\s+nghiệp\\s+tư\\s+nhân|tổng\\s+công\\s+ty",
  "|mã\\s+số\\s+thuế|(?<![\\p{L}])mst(?![\\p{L}])|mã\\s+số\\s+doanh\\s+nghiệp",
  "|(?<![\\p{L}])(co\\.?\\s*,?\\s*ltd|ltd|jsc|j\\.s\\.c|corp|corporation)(?![\\p{L}])",
  "|hợp\\s+tác\\s+xã|(?<![\\p{L}])htx(?![\\p{L}])|xí\\s+nghiệp",
  "|văn\\s+phòng\\s+đại\\s+diện",
  "|100\\s*%\\s*vốn|(?<![\\p{L}])fdi(?![\\p{L}])|vốn\\s+(nước\\s+ngoài|đầu\\s+tư\\s+nước)",
  "|giấy\\s+phép\\s+kinh\\s+doanh|đăng\\s+ký\\s+kinh\\s+doanh|(?<![\\p{L}])(đkkd|gpkd)(?![\\p{L}])",
  "|(?<!các )(?<!những )(?<!ở )(?<!tại )(?<!trong )(?<!nhiều )(?<!mọi )(?<!từ )(?<!như )(?<!một )(?<!cho )(?<!\\d )nhà\\s+máy",
  "|(?<!các )(?<!những )(?<!ở )(?<!tại )(?<!trong )(?<!nhiều )(?<!mọi )(?<!từ )(?<!như )(?<!một )(?<!cho )(?<!\\d )tập\\s+đoàn")

.tl <- stri_trans_tolower(stri_trans_nfc(mcl_main$text))
## hiring_female (WIDE): the ad demands female labour -- a woman could/would be
## hired. Includes female-specific ads, mixed nam/nữ ads, and gender-neutral
## ("không phân biệt") ads. Only male-ONLY ads and no-gender-mention ads are 0.
mcl_main[, hiring_female := as.integer(
  stri_detect_regex(.tl, RX_FEMALE) |
  stri_detect_regex(.tl, RX_BOTH) |
  stri_detect_regex(.tl, "không\\s+phân\\s+biệt\\s+(giới\\s+tính|nam)"))]
## hiring_female_only (NARROW): female labour ONLY is demanded -- a female cue,
## no both-gender marker, no male requirement, not gender-neutral.
mcl_main[, hiring_female_only := as.integer(
  stri_detect_regex(.tl, RX_FEMALE) &
  !stri_detect_regex(.tl, RX_BOTH) &
  !stri_detect_regex(.tl, RX_MALE) &
  !stri_detect_regex(.tl, "không\\s+phân\\s+biệt\\s+(giới\\s+tính|nam)"))]
mcl_main[, formal := as.integer(stri_detect_regex(.tl, RX_FORMAL))]
mcl_main[, taxid  := as.integer(stri_detect_regex(.tl, RX_TAXID))]

## contract: 1 if the post mentions a WRITTEN EMPLOYMENT contract (HĐLĐ / ký hợp
## đồng / a typed labour contract). Distinct from `formal` (insurance / labour
## law). Kept to employment-contract forms; non-employment contracts (đại lý /
## mua bán / cộng tác viên / kinh tế) are excluded so it measures the worker's
## contract, not the firm's deals.
RX_CONTRACT <- paste0(
  "hợp\\s+đồng\\s+lao\\s+động",
  "|(?<![\\p{L}])hđlđ(?![\\p{L}])",
  "|hợp\\s+đồng\\s+(chính\\s+thức|dài\\s+hạn|thử\\s+việc|thời\\s+vụ|có\\s+thời\\s+hạn|",
    "(không|vô)\\s+(xác\\s+định\\s+)?thời\\s+hạn|\\d+\\s*(tháng|năm))",
  "|ký\\s+(kết\\s+)?(hợp\\s+đồng|hđ(?![\\p{L}]))",
    "(?!\\s+(đại\\s+lý|mua\\s+bán|cộng\\s+tác|nguyên\\s+tắc|kinh\\s+tế|thuê|nhượng))",
  "|được\\s+ký\\s+(chính\\s+thức|hợp\\s+đồng)")
mcl_main[, contract := as.integer(stri_detect_regex(.tl, RX_CONTRACT))]

## ---- sector (agri / manu / service) --------------------------------------
RX_AGRI <- paste0("nông\\s+nghiệp|trồng\\s+trọt|chăn\\s+nuôi|nuôi\\s+trồng|",
  "nuôi\\s+(tôm|cá|heo|lợn|gà|vịt|bò|ong)|nông\\s+trại|trang\\s+trại|làm\\s+vườn|",
  "lâm\\s+nghiệp|nông\\s+dân|cây\\s+(giống|trồng)|phân\\s+bón|thú\\s+y|",
  "thuốc\\s+bảo\\s+vệ\\s+thực\\s+vật|nuôi\\s+trồng\\s+thủy\\s+sản|giống\\s+(cây|thủy\\s+sản)|",
  "trồng(?!\\s+răng)|nuôi(?!\\s*(ăn|ở|cơm|con|cái|dạy|dưỡng))|(?<![\\p{L}])bón(?![\\p{L}])|phần\\s+chăn")
RX_MANU <- paste0("công\\s+nhân(?!\\s+xây)|nhà\\s+máy|nhà\\s+xưởng|(?<![\\p{L}])xưởng|",
  "khu\\s+công\\s+nghiệp|(?<![\\p{L}])kcn(?![\\p{L}])|khu\\s+chế\\s+xuất|dây\\s+chuyền|",
  "sản\\s+xuất|gia\\s+công|lắp\\s+ráp|chế\\s+biến|linh\\s+kiện|điện\\s+tử(?!\\s+viễn)|",
  "may\\s+(mặc|công\\s+nghiệp)|thợ\\s+may|dệt(?![\\p{L}])|nhuộm|giày\\s+(da|dép)|da\\s+giày|",
  "cơ\\s+khí|thợ\\s+(hàn|tiện|phay|cnc)|hàn\\s+xì|bao\\s+bì|in\\s+ấn|nhựa(?![\\p{L}])|",
  "thép(?![\\p{L}])|đúc(?![\\p{L}])|luyện\\s+kim|hóa\\s+chất|đóng\\s+gói|",
  "xe\\s+đưa|khu\\s+công|sam\\s*sung")
RX_CONS <- paste0("xây\\s+dựng|công\\s+trình|thi\\s+công|nhà\\s+thầu|thầu\\s+(xây|thi\\s+công)|",
  "giàn\\s+giáo|cốt\\s+thép|đổ\\s+bê\\s+tông|phụ\\s+hồ|thợ\\s+(hồ|xây|sơn\\s+nước|điện\\s+nước)|",
  "san\\s+lấp|cầu\\s+đường|công\\s+nhân\\s+xây")
RX_SERV <- paste0("bán\\s+hàng|phục\\s+vụ|nhà\\s+hàng|quán|cafe|cà\\s+phê|khách\\s+sạn|resort|",
  "lễ\\s+tân|thu\\s+ngân|phụ\\s+bếp|đầu\\s+bếp|pha\\s+chế|bartender|buồng\\s+phòng|",
  "kế\\s+toán|văn\\s+phòng|marketing|kinh\\s+doanh|tư\\s+vấn|chăm\\s+sóc\\s+khách|telesale|",
  "giao\\s+hàng|ship|lái\\s+xe|tài\\s+xế|bảo\\s+vệ|tạp\\s+vụ|giúp\\s+việc|thủ\\s+kho|thư\\s+ký|",
  "spa|salon|thẩm\\s+mỹ|(?<![\\p{L}])nail|cắt\\s+tóc|gội\\s+đầu|massage|trang\\s+điểm|",
  "gia\\s+sư|giáo\\s+viên|mầm\\s+non|trung\\s+tâm\\s+(anh\\s+ngữ|ngoại\\s+ngữ|đào\\s+tạo|tiếng)|",
  "lập\\s+trình|thiết\\s+kế|ngân\\s+hàng|bảo\\s+hiểm|bất\\s+động\\s+sản|môi\\s+giới|",
  "du\\s+lịch|lữ\\s+hành|siêu\\s+thị|cửa\\s+hàng|showroom|(?<![\\p{L}])shop|",
  "vận\\s+tải|logistics|chuyển\\s+phát|viễn\\s+thông|phòng\\s+khám|nha\\s+khoa|dược|nhà\\s+thuốc|",
  "(?<![\\p{L}])pg(?![\\p{L}])|(?<![\\p{L}])bar(?![\\p{L}])|karaoke|(?<![\\p{L}])gym|fitness|dịch\\s+vụ|",
  "chạy\\s+bàn|típ|coffee")
## RETAIL markers force service even against a product-manu word: a shoe SHOP is
## service though "giày dép" is a manu marker; a shoe FACTORY keeps "nhà máy"->manu.
RX_RETAIL <- paste0("cửa\\s+hàng|cửa\\s+hiệu|(?<![\\p{L}])shop(?![\\p{L}])|showroom|",
                    "siêu\\s+thị|đại\\s+lý|chuỗi\\s+(cửa\\s+hàng|shop|bán\\s+lẻ)|bán\\s+lẻ")
## assign in REVERSE priority (last write wins): service < manu < retail(service) < cons(NA) < agri
mcl_main[, sector := NA_character_]
mcl_main[stri_detect_regex(.tl, RX_SERV),   sector := "service"]
mcl_main[stri_detect_regex(.tl, RX_MANU),   sector := "manu"]
mcl_main[stri_detect_regex(.tl, RX_RETAIL), sector := "service"]     # retail shop -> service (overrides product-manu)
mcl_main[stri_detect_regex(.tl, RX_CONS),   sector := NA_character_] # construction -> NA (not manu)
mcl_main[stri_detect_regex(.tl, RX_AGRI),   sector := "agri"]

## ---- fdi + is_abroad ------------------------------------------------------
## fdi: the hiring firm is FOREIGN-INVESTED (in Vietnam). Explicit capital
## phrases (100% vốn [not nhà nước/trong nước], vốn đầu tư nước ngoài, FDI,
## liên doanh) OR nationality-company descriptors ("công ty Nhật Bản")
RX_ABROAD <- paste0("xuất\\s+khẩu\\s+lao\\s+động|(?<![\\p{L}])xkld(?![\\p{L}])|thực\\s+tập\\s+sinh|",
  "đơn\\s+hàng\\s+(nhật|đài|hàn)|làm\\s+việc\\s+tại\\s+(nhật|đài\\s+loan|hàn\\s+quốc|nước\\s+ngoài)|",
  "sang\\s+(nhật|hàn|đài)|đi\\s+(nhật|hàn\\s+quốc|đài\\s+loan)(?![\\p{L}])")
RX_FDI_CAP <- paste0("100\\s*%\\s*vốn(?!\\s*(nhà\\s+nước|việt\\s+nam|trong\\s+nước))|",
  "vốn\\s+(đầu\\s+tư\\s+)?nước\\s+ngoài|(?<![\\p{L}])fdi(?![\\p{L}])|",
  "vốn\\s+(nhật|hàn(\\s+quốc)?|đài\\s+loan|trung\\s+quốc|singapore|mỹ(?!\\s*(phẩm|nghệ)))|",
  "liên\\s+doanh|doanh\\s+nghiệp\\s+nước\\s+ngoài|",
  "công\\s+ty\\s+có\\s+vốn\\s+(đầu\\s+tư\\s+)?(nước\\s+ngoài|nhật|hàn|đài|ngoại)")
RX_FDI_NAT <- paste0("(công\\s+ty|tập\\s+đoàn|doanh\\s+nghiệp)\\s+",
  "(nhật(\\s+bản)?|hàn\\s+quốc|đài\\s+loan|trung\\s+quốc|mỹ(?!\\s*(phẩm|nghệ))|singapore|",
  "malaysia|thái\\s+lan|châu\\s+âu|đa\\s+quốc\\s+gia|nước\\s+ngoài)(?![\\p{L}])")
.abr <- stri_detect_regex(.tl, RX_ABROAD)
mcl_main[, fdi := as.integer(stri_detect_regex(.tl, RX_FDI_CAP) |
                             (stri_detect_regex(.tl, RX_FDI_NAT) & !.abr))]
rm(.abr)

RX_MST <- paste0(
  "(?:mã\\s*số\\s*thuế|mã\\s*số\\s*doanh\\s*nghiệp|mã\\s*số\\s*dn(?![\\p{L}])|(?<![\\p{L}])mst(?![\\p{L}])|",
  "số\\s*đkkd|(?<![\\p{L}])đkkd(?![\\p{L}])|(?<![\\p{L}])gpkd(?![\\p{L}])|",
  "giấy\\s+(?:phép|chứng\\s+nhận)\\s+(?:đăng\\s+ký\\s+)?kinh\\s+doanh(?:\\s+số)?)",
  "[^0-9\\n]{0,15}",
  "(?<!\\d)((?:\\d[ .]?){9}\\d)(?:\\s*[-–]\\s*(\\d{3}))?(?!\\d)")
.m <- stri_match_first_regex(.tl, RX_MST)
mcl_main[, mst := { x <- stri_replace_all_regex(.m[,2], "[^0-9]", "")
                    x[!is.na(x) & nchar(x) != 10] <- NA_character_; x }]
mcl_main[, mst_branch := fifelse(is.na(mst), NA_character_, .m[,3])]
rm(.m)
rm(.tl)

FIRM_LEGAL <- "(?i:công\\s+ty|cty|c\\.ty|tập\\s+đoàn|doanh\\s+nghiệp|xí\\s+nghiệp)"

FIRM_MOD   <- paste0("(?i:",
  "tnhh|mtv|một\\s+thành\\s+viên|cổ\\s+phần|cp|hợp\\s+danh|",
  "tmdv|tm|dv|sx|sxtm|xnk|xkld|",                                        # abbreviations
  "thương\\s+mại|dịch\\s+vụ|sản\\s+xuất|xuất\\s+nhập\\s+khẩu|đầu\\s+tư|phát\\s+triển|",
  "công\\s+nghệ|công\\s+nghiệp|truyền\\s+thông|giáo\\s+dục|đào\\s+tạo|tư\\s+vấn|",
  "xây\\s+dựng|du\\s+lịch|vận\\s+tải|bất\\s+động\\s+sản|tài\\s+chính|logistics|",
  "kỹ\\s+thuật|cơ\\s+khí|điện\\s+tử|điện\\s+lạnh|cơ\\s+điện|kiến\\s+trúc|thiết\\s+kế|",
  "nội\\s+thất|may\\s+mặc|dệt\\s+may|thực\\s+phẩm|dược(\\s+phẩm)?|y\\s+tế|mỹ\\s+phẩm|",
  "nông\\s+nghiệp|thủy\\s+sản|hóa\\s+chất|vật\\s+liệu(\\s+xây\\s+dựng)?|thiết\\s+bị|máy\\s+móc|",
  "ô\\s*tô|in\\s+ấn|quảng\\s+cáo|sự\\s+kiện|giải\\s+trí|kinh\\s+doanh|phân\\s+phối|",
  "nhân\\s+lực|vệ\\s+sinh|bảo\\s+vệ|năng\\s+lượng|môi\\s+trường|viễn\\s+thông|",
  "phần\\s+mềm|giải\\s+pháp|quốc\\s+tế|toàn\\s+cầu|vàng\\s+bạc|trang\\s+sức|",
  "nhà\\s+hàng|khách\\s+sạn|bán\\s+lẻ|siêu\\s+thị|xi\\s+măng|thép|nhựa|gỗ",
  ")")
FIRM_STOP  <- paste0("(?i:tuyển|cần\\s|thông\\s+báo|chuyên|xin|kính|hân\\s+hạnh|thành\\s+lập|",
                     "hiện|đang|có\\s+nhu|với\\s|là\\s|được\\s|hoạt\\s+động|trân\\s+trọng|mời|",
                     "thông\\s+tin|địa\\s+chỉ|chúng\\s+tôi|chúng\\s+mình|nuôi|hỗ\\s+trợ|sẽ\\s|",
                     "\\bcó\\b|\\bmình\\b)")

RX_FIRM <- paste0(FIRM_LEGAL, "\\s+(?:", FIRM_MOD, "(?:\\s*[&/\\-]\\s*|\\s+và\\s+|\\s+))*",
                  "(?=[\\p{Lu}\\d])[^\\n]{1,55}?",
                  "(?=\\s*(?:", FIRM_STOP, "|[.,;:!?•*()\\[\\]/|]|\\n|$))")
mcl_main[, firm_name := stri_replace_all_regex(
  stri_trim_both(stri_match_first_regex(text, RX_FIRM)[, 1]), "\\s+", " ")]
## NA out (a) legal-form-only matches and (b) pure nationality/type descriptors
## that are not real names ("công ty Nhật Bản", "công ty đa quốc gia").
mcl_main[is.na(firm_name) | firm_name == "" |
         stri_detect_regex(firm_name,
           "(?i)^(công\\s+ty|cty|c\\.ty|tập\\s+đoàn|doanh\\s+nghiệp|xí\\s+nghiệp)\\s*(tnhh|cổ\\s+phần|cp|mtv)?\\s*$") |
         stri_detect_regex(firm_name,
           "(?i)^(công\\s+ty|cty|tập\\s+đoàn|doanh\\s+nghiệp)\\s+(nhật(\\s+bản)?|hàn\\s+quốc|trung\\s+quốc|đài\\s+loan|nước\\s+ngoài|đa\\s+quốc\\s+gia|fdi|nhỏ|vừa|sẽ|mới|này|đó|trên|của|yêu)\\s*$"),
         firm_name := NA_character_]

setcolorder(mcl_main, intersect(
  c("id", "year", "creation_time", "content_type",
    "surface.name", "surface.username", "post_owner.name", "page",
    "tinh", "huyen", "district", "province",
    "dist_evidence", "dist_conf", "loc_cue", "pos", "n_districts",
    "dist_pred", "huyen_pred", "pred_source", "post_row", "text"),
  names(mcl_main)))

save(mcl_main, file = "Clean data/mcl_main.Rda")

# Sum stats

mcl_sum <- mcl_main %>% 
  filter(is_job_ad == 1 & dist_conf == "high") %>%
  rename(socinsur = formal) %>% 
  mutate(formal = ifelse(socinsur == 1 | contract == 1, 1, 0)) %>% 
  group_by(year, tinh, huyen) %>% 
  summarise(
    job_ads = sum(is_job_ad == 1, na.rm = T),
    agri = sum(sector == "agri", na.rm = T),
    manu = sum(sector == "manu", na.rm = T),
    service = sum(sector == "service", na.rm = T),
    taxid = sum(taxid == 1, na.rm = T),
    taxid = sum(socinsur == 1, na.rm = T),
    formal = sum(formal == 1, na.rm = T),
    fdi = sum(fdi == 1, na.rm = T),
    hiring_female = sum(hiring_female == 1, na.rm = T),
    hiring_female_only = sum(hiring_female_only == 1, na.rm = T)
  ) %>% 
  filter(!is.na(huyen)) %>% 
  full_join(dist_3G) %>% 
  mutate(
    job_ads = ifelse(is.na(job_ads), 0, job_ads),
    agri = ifelse(is.na(agri), 0, agri),
    manu = ifelse(is.na(manu), 0, manu),
    fdi = ifelse(is.na(fdi), 0, fdi),
    formal = ifelse(is.na(formal), 0, formal)
  )

mcl_jobad_agg <- mcl_main %>%
  filter(is_job_ad == 1 & post_row == 1) %>%
  group_by(year) %>%
  summarise(n = n())

ytop <- ceiling(max(mcl_jobad_agg$n) / 20000) * 20000  

ggplot(mcl_jobad_agg, aes(x = year, y = n)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = mcl_jobad_agg$year) +
  scale_y_continuous(limits = c(0, ytop), breaks = seq(0, ytop, 20000),
                     labels = scales::label_comma()) +
  labs(x = "Year", y = "Number of job ads posted on Facebook pages",
       title = "") +
  theme_classic()
ggsave("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Descriptive Stats/tuyen_dung_by_year.pdf", width = 8, height = 5)
