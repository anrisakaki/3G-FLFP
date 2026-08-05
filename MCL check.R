library(data.table)
library(tidyverse)

load("Clean data/mcl_main.Rda")
setDT(mcl_main)

stopifnot("is_job_ad" %in% names(mcl_main))

OUT_DIR <- "Clean data/MCL Check"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

ID_COLS <- c("id", "post_row", "year", "page", "post_owner.name", "surface.name", "text")
prep <- function(dt) {                            # tidy text + blank check columns
  dt[, text := str_replace_all(text, "[\r\n]+", " ")]
  dt[, `:=`(check_correct = "", true_district = "", notes = "")]
  dt[]
}

## ---- (1) job ads with a district NAMED IN THE POST -----------------------
INF_COLS <- c(ID_COLS, "tinh", "huyen", "district", "province",
              "dist_evidence", "dist_conf", "n_districts")
stopifnot(all(INF_COLS %in% names(mcl_main)))
inferred <- prep(mcl_main[is_job_ad == 1L & !is.na(huyen), ..INF_COLS])

N_FILES <- 10L                                    
pid <- unique(inferred$id)
grp <- cut(seq_along(pid), N_FILES, labels = FALSE)[match(inferred$id, pid)]
stopifnot(!anyNA(grp))
for (k in seq_len(N_FILES)) {
  p <- file.path(OUT_DIR, sprintf("jobads_named_%02d_of_%02d.csv", k, N_FILES))
  fwrite(inferred[grp == k], p, bom = TRUE)       # bom = TRUE so Excel reads UTF-8
  message(sprintf("%s : %s rows, %s posts, %.1f MB", basename(p),
                  format(sum(grp == k), big.mark = ","),
                  format(uniqueN(inferred$id[grp == k]), big.mark = ","),
                  file.size(p) / 1e6))
}
stopifnot(sum(table(grp)) == nrow(inferred), uniqueN(paste(grp)) == N_FILES)
message("inferred-from-post: ", format(nrow(inferred), big.mark = ","),
        " district rows across ", format(uniqueN(inferred$id), big.mark = ","), " job ads")

## multi-district digests, for a separate look
many <- inferred[n_districts > 9][order(id, huyen)]
fwrite(many, file.path(OUT_DIR, "jobads_named_over9_districts.csv"), bom = TRUE)
message("over-9-districts: ", format(uniqueN(many$id), big.mark = ","), " posts, ",
        format(nrow(many), big.mark = ","), " rows")

## ---- (2) job ads with a PREDICTED district (kcn/osr/osrw/ward3) -----------
PRED_COLS <- c("id", "year", "page", "post_owner.name", "surface.name", "text",
               "tinh", "province", "huyen_pred", "dist_pred", "pred_source")
stopifnot(all(PRED_COLS %in% names(mcl_main)))
predicted <- prep(mcl_main[is_job_ad == 1L & !is.na(huyen_pred), ..PRED_COLS])
stopifnot(!anyDuplicated(predicted$id))           # one predicted district per post
fwrite(predicted, file.path(OUT_DIR, "jobads_predicted.csv"), bom = TRUE)
message("predicted: ", format(nrow(predicted), big.mark = ","), " job ads (",
        paste(names(table(predicted$pred_source)), table(predicted$pred_source),
              sep = "=", collapse = ", "), ")")

## ---- (3) job ads with NO district but a KNOWN PROVINCE --------------------
## (neither inferred nor predicted). Both-missing posts are excluded: with no
## province to narrow it down, the district can't be filled in by hand.
MISS_DIR <- file.path(OUT_DIR, "Missing dist")
if (!dir.exists(MISS_DIR)) dir.create(MISS_DIR, recursive = TRUE)
MISS_COLS <- c("id", "year", "page", "post_owner.name", "surface.name", "text",
               "tinh", "province")
stopifnot(all(MISS_COLS %in% names(mcl_main)))
missing <- prep(mcl_main[is_job_ad == 1L & is.na(huyen) & is.na(huyen_pred) &
                         !is.na(tinh), ..MISS_COLS])
stopifnot(!anyDuplicated(missing$id), !anyNA(missing$tinh))  # one row/post, province set

mpid <- unique(missing$id)                          # chunk (large) for Excel, posts kept whole
mgrp <- cut(seq_along(mpid), N_FILES, labels = FALSE)[match(missing$id, mpid)]
stopifnot(!anyNA(mgrp))
for (k in seq_len(N_FILES)) {
  p <- file.path(MISS_DIR, sprintf("jobads_missingdist_%02d_of_%02d.csv", k, N_FILES))
  fwrite(missing[mgrp == k], p, bom = TRUE)
  message(sprintf("%s : %s posts, %.1f MB", basename(p),
                  format(sum(mgrp == k), big.mark = ","), file.size(p) / 1e6))
}
message("missing-district (province known) job ads: ", format(nrow(missing), big.mark = ","))


# 

mcl_main <- mcl_main %>%
  filter(
    id != "1307053131464245"

  )

read_post <- function(x) {
  r <- if (is.numeric(x)) mcl_main[x] else mcl_main[id == as.character(x)]
  r <- r[1]                                   # a post repeats across its district rows
  cat("id       :", r$id,
      "\nyear     :", r$year,
      "\npage     :", r$page,
      "\ndistrict :", paste(na.omit(mcl_main[id == r$id]$district), collapse = " | "),
      "\nformal :", r$formal,
      "\ntaxid :", r$taxid,
      "\n---------------------------------------------\n")
  writeLines(r$text)                          
}

read_post("1000121039476235")   
