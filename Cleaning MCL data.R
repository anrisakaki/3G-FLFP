library(data.table)
library(tidyverse)
library(stringi)   

mcl_files <- list.files("Raw Data/MCL", pattern = "\\.csv$", full.names = T)
datetime_cols <- c("creation_time", "modified_time", "statistics.views_date_last_refreshed")

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
  filter(str_detect(text, regex("tuyển dụng", ignore_case = T)))

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
  "nộp\\s+hồ\\s+sơ|gửi\\s+hồ\\s+sơ|hồ\\s+sơ\\s+(bao\\s+gồm|gồm|xin\\s+việc)|ứng\\s+tuyển|hạn\\s+(chót|nộp)|deadline",
  "thời\\s+gian\\s+làm\\s+việc|giờ\\s+làm|ca\\s+(sáng|chiều|tối|xoay|gãy|đêm)|theo\\s+ca|full\\s*time|part\\s*time",
  "(nơi|địa\\s+điểm|địa\\s+chỉ)\\s+làm\\s+việc|làm\\s+việc\\s+tại",
  paste0("(liên\\s+hệ|hotline|zalo|sđt|đt|đth)\\s*[:：]?[^\\n]{0,20}\\d{4,}|",
         "(?<!\\d)0\\d{2,4}[ .]?\\d{2,4}[ .]?\\d{3,5}(?![\\d])"),
  paste0("cần\\s+tuyển|tuyển\\s+gấp|thông\\s+báo\\s+tuyển\\s+dụng|tin\\s+tuyển\\s+dụng|",
         "(đang\\s+|nhu\\s+cầu\\s+)?tuyển(\\s+dụng)?\\s*[:：]?[^\\p{L}\\n]{0,8}(gấp\\s+)?",
         "(các\\s+|nhiều\\s+|một\\s+|\\d+\\s*[./)]?\\s*)?", JOBAD_ROLE, "|",
         "tuyển(\\s+dụng)?\\s*[:：]?\\s*\\n\\s*\\d+\\s*[./)]\\s*(các\\s+|nhiều\\s+)?", JOBAD_ROLE, "|",
         "cần\\s+(gấp\\s+)?(\\d+\\s+)?", JOBAD_ROLE))
JOBAD_PERI <- c(
  "địa\\s+chỉ\\s*[:：]",
  "\\d+\\s*(tr|triệu|k)\\s*/?\\s*(tháng|ca|ngày|giờ|h)(?![\\p{L}])",
  "(?<![\\p{L}])(ib|inbox)(?![\\p{L}])")
JOBAD_NEG <- c(
  "bí\\s+quyết|mẹo\\s|cách\\s+(viết|trả\\s+lời|deal|gây\\s+ấn)|làm\\s+thế\\s+nào\\s+để|kinh\\s+nghiệm\\s+(phỏng\\s+vấn|xin\\s+việc)|câu\\s+hỏi\\s+phỏng\\s+vấn",
  "nhà\\s+tuyển\\s+dụng",
  "khai\\s+giảng|học\\s+phí|khoá\\s+học|khóa\\s+học|lớp\\s+học|chiêu\\s+sinh|tuyển\\s+sinh|học\\s+bổng",
  "phóng\\s+viên|theo\\s+báo|trả\\s+lời\\s+(báo|phỏng\\s+vấn\\s+của)|cho\\s+biết|khẳng\\s+định",
  "cuộc\\s+thi|vòng\\s+(loại|chung\\s+kết|sơ\\s+khảo)|hội\\s+thảo|workshop|talkshow|minigame|giveaway|give\\s+away")
# job_page: poster is a dedicated job/recruitment page (name of page or owner).
JOBAD_PAGE <- paste0("việc\\s*làm|viec\\s*lam|tìm\\s*việc|tim\\s*viec|",
                     "tuyển\\s*dụng|tuyen\\s*dung|(?<![a-z])job|(?<![a-z])hr(?![a-z])|",
                     "vietnamworks|career|topcv|mywork")
JOBAD_STRONG <- paste0("thông\\s+báo\\s+tuyển\\s+dụng|tin\\s+tuyển\\s+dụng|",
                       "vị\\s+trí\\s+tuyển\\s+dụng|cần\\s+tuyển|tuyển\\s+gấp|",
                       "(đang\\s+|nhu\\s+cầu\\s+)?tuyển(\\s+dụng)?\\s*[:：]?[^\\p{L}\\n]{0,8}(gấp\\s+)?",
                       "(các\\s+|nhiều\\s+|một\\s+|\\d+\\s*[./)]?\\s*)?", JOBAD_ROLE, "|",
                       "tuyển(\\s+dụng)?\\s*[:：]?\\s*\\n\\s*\\d+\\s*[./)]\\s*(các\\s+|nhiều\\s+)?", JOBAD_ROLE)
JOBAD_NEG2 <- c(
  "khuyến\\s+mãi|giảm\\s+giá|ưu\\s+đãi|voucher|sale\\s+off",
  "tuyển\\s+(chồng|vợ|người\\s+yêu|bạn\\s+(trai|gái)(?![\\p{L}]))",
  "(muốn|cần|nhu\\s+cầu)\\s+(gửi|đăng)\\s+tin|đăng\\s+tin\\s+tuyển\\s+dụng\\s+vui\\s+lòng",
  "lừa\\s+đảo|cảnh\\s+báo|giả\\s+mạo|bóc\\s+phốt|đa\\s+cấp")
.ja <- stri_trans_tolower(stri_trans_nfc(mcl_main$text))
.pgnm <- stri_trans_tolower(stri_trans_nfc(paste(mcl_main$surface.name, mcl_main$post_owner.name,
  fifelse(is.na(mcl_main$surface.username), "", mcl_main$surface.username))))
mcl_main$job_page <- as.integer(stri_detect_regex(.pgnm, JOBAD_PAGE))
.mn <- Reduce(`+`, lapply(JOBAD_MAIN, function(p) stri_detect_regex(.ja, p)))
.pn <- .mn + Reduce(`+`, lapply(JOBAD_PERI, function(p) stri_detect_regex(.ja, p)))
.nn <- Reduce(`+`, lapply(JOBAD_NEG, function(p) stri_detect_regex(.ja, p)))
.n2 <- Reduce(`+`, lapply(JOBAD_NEG2, function(p) stri_detect_regex(.ja, p)))
.stp <- stri_locate_first_regex(.ja, JOBAD_STRONG)[, 1]
mcl_main$is_job_ad <- as.integer(
  (.pn >= 2 & .mn >= 1 & !(.nn >= 2 & .pn <= 3)) |
  (mcl_main$job_page == 1L & .mn >= 1 & .nn == 0 & .n2 == 0) |
  (!is.na(.stp) & .stp <= 250 & .nn == 0 & .n2 == 0))
rm(.ja, .pgnm, .mn, .pn, .nn, .n2, .stp)
message("is_job_ad == 1: ", sum(mcl_main$is_job_ad),
        sprintf(" of %d posts (%.1f%%)", nrow(mcl_main), 100*mean(mcl_main$is_job_ad)))

source("vn_district_match.R")

gaz   <- build_gazetteer()
prov  <- build_provinces(gaz)
wards <- build_wards()   

mcl_main <- mcl_main %>% mutate(page = paste(surface.name, post_owner.name))

pass1 <- match_districts(mcl_main$text, mcl_main$page, gaz, prov, risky = character(), wards = wards)
risky <- union(RISKY_DEFAULT, derive_risky(pass1))
message("risky names dropped from the bare tier: ", paste(risky, collapse = ", "))

mcl_dist <- match_districts(mcl_main$text, mcl_main$page, gaz, prov, risky = risky, wards = wards)
mcl_dist[, `:=`(id   = mcl_main$id[post],
                year = mcl_main$year[post])]

prov_lab <- unique(gaz[is.na(split_of), .(tinh, provname)])
mcl_dist[is.na(provname), provname := prov_lab$provname[match(tinh, prov_lab$tinh)]]

print(mcl_dist[, .N, by = .(evidence, conf)][order(-N)])

fwrite(mcl_dist, "mcl_district_panel.csv")

NAMED_IN_TEXT <- c("prov_adjacent", "prov_in_text", "numbered",
                   "prefix_only", "prov_in_page", "prefix_conflict", "bare")
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
no_d[pbest, on = "post", tinh := i.prov_tinh]      # province may still be known
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

stopifnot(uniqueN(mcl_main$id) == n_posts,
          !anyDuplicated(mcl_main[, .(id, huyen)]),
          nrow(mcl_main[post_row == 1L]) == n_posts,
          nrow(mcl_main[!is.na(huyen) & is.na(tinh)]) == 0)

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

## (3) KCN crosswalk (learned from posts that name BOTH an industrial park and a district)
.d1 <- mcl_main[post_row==1L, .(id, text, n_districts)]
.tk <- vn_canon(.d1$text)
.kn <- stri_extract_first_regex(.tk,
  "(?<![\\p{L}])(kcn|khu\\s+công\\s+nghiệp|kcx|khu\\s+chế\\s+xuất)\\s+[\\p{L}0-9][\\p{L}0-9 ]{2,25}")
.kn <- stri_replace_first_regex(.kn, "^(kcn|khu\\s+công\\s+nghiệp|kcx|khu\\s+chế\\s+xuất)\\s+", "")
.kn <- stri_replace_first_regex(.kn, "\\s+(gần|tại|thuộc|và|có|đang|tuyển|cần|với|là|ở|đc|địa)\\b.*$", "")
.kn <- stri_trim_both(stri_replace_all_regex(.kn, "\\s+", " "))
.kn[nchar(.kn) < 3] <- NA
.d1[, kcn := .kn]
.hi <- mcl_main[!is.na(huyen) & dist_conf=="high",
                .(nh=uniqueN(huyen), tinh=tinh[1], huyen=huyen[1]), by=id]
.tr <- merge(.d1[!is.na(kcn), .(id, kcn)], .hi[nh==1L, .(id, tinh, huyen)], by="id")
.xw <- .tr[, .N, by=.(kcn, tinh, huyen)][order(-N)]
.xw[, tot := sum(N), by=kcn]
.xw <- .xw[, .SD[1], by=kcn][N >= 5 & N/tot >= 0.65]
.xw <- .xw[!stri_detect_regex(kcn, "^vsip")]                       # multi-province chain
.lab <- unique(mcl_main[!is.na(huyen), .(tinh, huyen, district, province)])
.ad  <- .lab[district == "Huyện An Dương" & province == "Hải Phòng"]
if (nrow(.ad) == 1L) .xw[stri_detect_regex(kcn, "^nomura"), `:=`(tinh=.ad$tinh, huyen=.ad$huyen)]
.tgt <- merge(.d1[!is.na(kcn) & n_districts==0, .(id, kcn)], .xw[, .(kcn, tinh, huyen)], by="kcn")
if (nrow(.tgt)){
  .fill <- merge(.tgt, .lab, by=c("tinh","huyen"))
  mcl_main[.fill, on="id", `:=`(tinh=i.tinh, huyen=i.huyen, district=i.district,
           province=i.province, dist_evidence="kcn", dist_conf="medium",
           n_districts=1L, loc_cue="kcn")]
}
message("loc_cue work rows: ", sum(mcl_main$loc_cue=="work"),
        " | KCN parks learned: ", nrow(.xw), " | posts filled via KCN: ", nrow(.tgt))

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
.hi2 <- mcl_main[!is.na(huyen) & dist_conf=="high", .(nh=uniqueN(huyen), tinh=tinh[1], huyen=huyen[1]), by=id]
.pr  <- merge(.d2[n_street>0, .(id, streets)], .hi2[nh==1, .(id, tinh, huyen)], by="id")[
          , .(street = unlist(streets)), by=.(id, tinh, huyen)]
.sxw <- .pr[, .N, by=.(street, tinh, huyen)]
.sxw[, tot := sum(N), by=.(street, tinh)]
.sxw <- .sxw[order(-N)][, .SD[1], by=.(street, tinh)][N >= 5 & N/tot >= 0.70]
.dn <- unique(mcl_main[!is.na(huyen), .(tinh,
        dname = vn_canon(stri_replace_first_regex(district,
          "^(Quận|Huyện|Thị\\s+xã|Thành\\s+phố)\\s+", ""), hard_sep=FALSE))])
.pn <- unique(mcl_main[!is.na(tinh) & !is.na(province), .(tinh, dname = vn_canon(province, hard_sep=FALSE))])
.sxw <- .sxw[!rbind(.dn, .pn), on = c(street="dname", tinh="tinh")]
.tg2 <- .d2[n_street>0 & n_districts==0 & !is.na(tinh), .(id, tinh, streets)][
          , .(street = unlist(streets)), by=.(id, tinh)]
.fl2 <- merge(.tg2, .sxw[, .(street, tinh, huyen)], by=c("street","tinh"))
.cf2 <- .fl2[, uniqueN(huyen), by=id]
.fl2 <- .fl2[id %in% .cf2[V1==1]$id][, .SD[1], by=id]
.lab2 <- unique(mcl_main[!is.na(huyen), .(tinh, huyen, district, province)])
if (nrow(.fl2)){
  .fl2 <- merge(.fl2, .lab2, by=c("tinh","huyen"))
  mcl_main[.fl2, on="id", `:=`(tinh=i.tinh, huyen=i.huyen, district=i.district,
           province=i.province, dist_evidence="street", dist_conf="medium",
           n_districts=1L, loc_cue="street")]
}
message("street crosswalk: ", nrow(.sxw), " entries | posts filled via street: ", nrow(.fl2))
rm(.st,.d1,.tk,.kn,.hi,.tr,.xw,.lab,.ad,.tgt,.i, wl, al, hl, tc, pp,
   .d2,.tc2,.stx,.hi2,.pr,.sxw,.dn,.pn,.tg2,.fl2,.cf2,.lab2,.name_of)
stopifnot(uniqueN(mcl_main$id) == n_posts,
          nrow(mcl_main[post_row == 1L]) == n_posts,
          !anyDuplicated(mcl_main[, .(id, huyen)]))

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

## ---- sector (agri / manu / service) --------------------------------------
## Argmax over occupation/industry keyword-family COUNTS. 
RX_AGRI <- paste0("nông\\s+nghiệp|trồng\\s+trọt|chăn\\s+nuôi|nuôi\\s+trồng|thu\\s+hoạch|",
  "nông\\s+trại|trang\\s+trại|làm\\s+vườn|thủy\\s+sản|thú\\s+y|",
  "cây\\s+(giống|trồng)|phân\\s+bón|thuốc\\s+bảo\\s+vệ\\s+thực\\s+vật|nông\\s+dân|lâm\\s+nghiệp")
RX_MANU <- paste0("công\\s+nhân(?!\\s+xây)|nhà\\s+máy|xưởng|sản\\s+xuất|dây\\s+chuyền|lắp\\s+ráp|",
  "linh\\s+kiện|điện\\s+tử(?!\\s+viễn)|may\\s+(mặc|công\\s+nghiệp)|thợ\\s+may|dệt|",
  "giày\\s+(da|dép)|bao\\s+bì|in\\s+ấn|cơ\\s+khí|thợ\\s+(hàn|tiện|phay)|gia\\s+công|",
  "chế\\s+biến|đóng\\s+gói|kcn|khu\\s+công\\s+nghiệp|khu\\s+chế\\s+xuất")
RX_CONS <- paste0("xây\\s+dựng|công\\s+trình|thợ\\s+(hồ|xây|điện\\s+nước|sơn\\s+nước)|phụ\\s+hồ|",
  "giàn\\s+giáo|cốt\\s+thép|đổ\\s+bê\\s+tông|công\\s+nhân\\s+xây")
RX_SERV <- paste0("bán\\s+hàng|phục\\s+vụ|nhà\\s+hàng|quán|cafe|cà\\s+phê|khách\\s+sạn|lễ\\s+tân|",
  "thu\\s+ngân|kế\\s+toán|văn\\s+phòng|marketing|kinh\\s+doanh|tư\\s+vấn|chăm\\s+sóc\\s+khách|",
  "giao\\s+hàng|ship|lái\\s+xe|tài\\s+xế|bảo\\s+vệ|tạp\\s+vụ|giúp\\s+việc|spa|salon|làm\\s+tóc|",
  "gia\\s+sư|giáo\\s+viên|lập\\s+trình|thiết\\s+kế|ngân\\s+hàng|bất\\s+động\\s+sản|du\\s+lịch|",
  "pha\\s+chế|bartender|trang\\s+điểm|(?<![\\p{L}])pg(?![\\p{L}])|telesale|kho\\b|thư\\s+ký|",
  "nhân\\s+viên\\s+(văn\\s+phòng|kinh\\s+doanh|thị\\s+trường)|siêu\\s+thị")
.sa <- stri_count_regex(.tl, RX_AGRI); .sm <- stri_count_regex(.tl, RX_MANU)
.sc <- stri_count_regex(.tl, RX_CONS); .ss <- stri_count_regex(.tl, RX_SERV)
mcl_main[, sector := NA_character_]
mcl_main[.sm > .ss & .sm > .sa & .sm > .sc, sector := "manu"]
mcl_main[.ss > .sm & .ss > .sa & .ss > .sc, sector := "service"]
mcl_main[.sa >= 1 & .sa >= .sm & .sa >= .ss & .sa >= .sc, sector := "agri"]
rm(.sa, .sm, .sc, .ss)

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

## final column order: id + post metadata first, then the district block
setcolorder(mcl_main, intersect(
  c("id", "year", "creation_time", "content_type",
    "surface.name", "surface.username", "post_owner.name", "page",
    "tinh", "huyen", "district", "province",
    "dist_evidence", "dist_conf", "loc_cue", "pos", "n_districts", "post_row"),
  names(mcl_main)))

save(mcl_main, file = "Clean data/mcl_main.Rda")

# Sum stats

td_agg <- mcl_main %>%
  filter(is_job_ad == 1 & post_row == 1) %>%
  group_by(year) %>%
  summarise(n = n())

ytop <- ceiling(max(td_agg$n) / 20000) * 20000  

ggplot(td_agg, aes(x = year, y = n)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = td_agg$year) +
  scale_y_continuous(limits = c(0, ytop), breaks = seq(0, ytop, 20000),
                     labels = scales::label_comma()) +
  labs(x = "Year", y = "Number of job ads posted on Facebook pages",
       title = "") +
  theme_classic()
ggsave("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Descriptive Stats/tuyen_dung_by_year.pdf", width = 8, height = 5)
