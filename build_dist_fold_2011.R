suppressMessages({library(data.table); library(stringi)})
setwd("C:/Users/Anri Sakakibara/Dropbox/3G - FLFP/3G-FLFP/Code")
source("vn_district_match.R")   

dc <- function(x) vn_canon(stri_replace_first_regex(trimws(x),
        "^(Quận|Huyện|Thị xã|Thành phố)\\s+", ""), FALSE)

l11 <- fread("Raw Data/LFS/lfs_dist_11.csv", encoding = "UTF-8")
l11[, `:=`(tinh = as.integer(tinh), huyen = as.integer(huyen), k = dc(distname))]

lfs11code <- function(prov_tinh, name) {
  r <- l11[tinh == prov_tinh & k == dc(name), huyen]; if (length(r)) r[1] else NA_integer_
}

CW <- fread("Raw Data/Consistent District/consistent_district_wide.csv",
            colClasses = "character", encoding = "UTF-8")
CW[, dclean := trimws(stri_replace_first_regex(dname, "\\s*\\([^)]*\\)$", ""))]
CW[, own11 := fifelse(!is.na(distcode11) & distcode11 != "", distcode11, distcode10)]
codeName <- unique(CW[own11 != "" & !is.na(own11),
                      .(gp = as.integer(provcode11), gc = as.integer(own11), pn = dc(dclean))])
CW[, cons_p := as.integer(fifelse(provcode != "", provcode,
                fifelse(provcode11 != "", provcode11, provcode10)))]
CW[, cons_d := as.integer(fifelse(distcode != "", distcode,
                fifelse(distcode11 != "", distcode11, distcode10)))]
CW <- merge(CW, codeName, by.x = c("cons_p","cons_d"), by.y = c("gp","gc"), all.x = TRUE)
CW[is.na(pn), pn := dc(dclean)]
CW[, par11 := mapply(lfs11code, cons_p, pn)]
P <- rbindlist(lapply(10:17, function(yy)
  CW[!is.na(par11) & get(paste0("distcode", yy)) != "" & !is.na(get(paste0("distcode", yy))),
     .(year   = 2000L + yy,
       tinh   = as.integer(get(paste0("provcode", yy))),
       huyen  = as.integer(get(paste0("distcode", yy))),
       tinh11 = cons_p, huyen11 = par11)]))
P <- unique(P[tinh != tinh11 | huyen != huyen11])

mk <- function(t, h, pt, pname) data.table(tinh = t, huyen = h,
        tinh11 = pt, huyen11 = lfs11code(pt, pname))
E <- rbindlist(list(
  mk(8, 71, 8, "Nà Hang"),      mk(44, 458, 44, "Quảng Trạch"),
  mk(58, 589, 58, "Ninh Phước"),mk(70, 688, 70, "Phước Long"),
  mk(70, 698, 70, "Phước Long"),mk(70, 690, 70, "Bình Long"),
  mk(83, 838, 83, "Mỏ Cày Nam"),mk(91, 914, 91, "Kiên Lương"),
  mk(94, 951, 94, "Long Phú"),  mk(64, 639, 64, "Chư Sê")))
stopifnot(nrow(E[is.na(huyen11)]) == 0)          # every parent must resolve
E <- rbindlist(lapply(2010:2017, function(y) copy(E)[, year := y]))

folds <- unique(rbind(P, E[, .(year, tinh, huyen, tinh11, huyen11)]))
saveRDS(folds, "Clean data/dist_fold_2011.Rds")
cat("dist_fold_2011.Rds written:", nrow(folds), "rows\n")
