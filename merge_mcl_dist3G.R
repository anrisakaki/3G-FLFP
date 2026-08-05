## ============================================================================
## merge_mcl_dist3G.R  --  attach the dist_3G treatment panel to the MCL job ads
## ============================================================================
## MCL now identifies districts by RAW GADM names (see "Cleaning MCL data.R":
## district/province = GADM NAME_2/NAME_1, no LFS codes). dist_3G is keyed on
## GADM's FOLDED ID_2 (Bắc/Nam Từ Liêm -> Từ Liêm, etc.) and carries no names.
## This helper folds MCL's names the SAME way the OCI build does, maps them to
## ID_2 via the saved crosswalk, and merges on (ID_2, year).
##
## PREREQUISITES
##   1. Regenerate mcl_main by re-running "Cleaning MCL data.R" (GADM identity).
##   2. Run "Cleaning OCI and CB data - district level.R" once so it writes
##      Clean data/vnmap2_dist.Rds (the ID_2 <-> folded-name crosswalk).
##
## USAGE
##   source("merge_mcl_dist3G.R")
##   m <- merge_mcl_dist3G()                       # uses the saved defaults
##   # or pass objects already in memory:
##   m <- merge_mcl_dist3G(mcl_main, dist_3G, vnmap2_dist)
## ----------------------------------------------------------------------------
suppressMessages({library(data.table); library(stringr)})

## Fold raw GADM (province, district) into the (NAME_1, NAME_2) keys dist_3G
## uses -- transforms identical to the OCI vnmap2 / vnmap2_dist construction.
gadm_fold_key <- function(province, district){
  d <- str_trim(str_remove(district,
         "^(Quận|Huyện|Thành phố|Thị xã|TP|Thị Xã|Thành Phố|TP\\.)\\s*"))
  d <- fifelse(d %in% c("Bắc Từ Liêm","Nam Từ Liêm"), "Từ Liêm",
       fifelse(d == "Mỏ Cày Bắc",  "Mỏ Cày Nam",
       fifelse(d == "Bắc Tân Uyên", "Tân Uyên", d)))
  d <- fifelse(d == "Hoà An" & province == "Cao Bằng", "Cao Bằng", d)
  d <- fifelse(d == "Đồng Phú", "Đồng Phù", d)
  p <- fifelse(province == 'Bà Rịa - Vũng Tàu', 'Bà Rịa-Vũng Tàu',
       fifelse(province == 'Khánh Hòa',        'Khánh Hoà',
       fifelse(province == 'Thanh Hóa',        'Thanh Hoá',
       fifelse(province == 'Thừa Thiên Huế',   'Thừa Thiên-Huế',
       fifelse(province == 'Hồ Chí Minh',      'Tp Hồ Chí Minh', province)))))
  list(NAME_1 = p, NAME_2 = d)
}

#' @param use_pred  if TRUE, fall back to the inferred district (dist_pred /
#'                  pred_source) when no district is named in the text.
#' @return  mcl_main (one row per post x district) with ID_2 and every dist_3G
#'          column attached; ID_2 is NA where the district is unknown or falls
#'          outside dist_3G's frame (post-2011 splits, ~32 GADM units).
merge_mcl_dist3G <- function(mcl_main = NULL, dist_3G = NULL, vnmap2_dist = NULL,
                             use_pred = TRUE, verbose = TRUE){
  if (is.null(mcl_main))    { e <- new.env(); load("Clean data/mcl_main.Rda", e); mcl_main <- e$mcl_main }
  if (is.null(dist_3G))     { e <- new.env(); load("Clean data/dist_3G.Rda",  e); dist_3G  <- e$dist_3G }
  if (is.null(vnmap2_dist)) vnmap2_dist <- readRDS("Clean data/vnmap2_dist.Rds")
  mcl_main    <- as.data.table(copy(mcl_main))
  dist_3G     <- as.data.table(dist_3G)
  vnmap2_dist <- unique(as.data.table(vnmap2_dist)[, .(NAME_1, NAME_2, ID_2)])
  stopifnot(all(c("district","province","year") %in% names(mcl_main)),
            !anyDuplicated(vnmap2_dist[, .(NAME_1, NAME_2)]))   # crosswalk must be 1:1

  ## district to place the post at: text-named, else inferred
  mcl_main[, dist_final := if (use_pred && "dist_pred" %in% names(mcl_main))
                             fifelse(!is.na(district), district, dist_pred) else district]
  k <- gadm_fold_key(mcl_main$province, mcl_main$dist_final)
  mcl_main[, `:=`(NAME_1 = k$NAME_1, NAME_2 = k$NAME_2)]
  mcl_main[vnmap2_dist, on = c("NAME_1","NAME_2"), ID_2 := i.ID_2]   # name -> ID_2

  out <- merge(mcl_main, dist_3G, by = c("ID_2","year"), all.x = TRUE)

  if (verbose){
    nd  <- mcl_main[!is.na(dist_final), .N]
    nid <- mcl_main[!is.na(ID_2), .N]
    message(sprintf("districted rows: %s | mapped to ID_2: %s (%.1f%%) | in dist_3G: %s",
                    format(nd, big.mark=","), format(nid, big.mark=","), 100*nid/nd,
                    format(out[!is.na(mean_3G_OCI), .N], big.mark=",")))
    miss <- mcl_main[!is.na(dist_final) & is.na(ID_2), .N, by=.(province, dist_final)][order(-N)]
    if (nrow(miss)) message("districts with no dist_3G ID_2 (outside the panel frame): ",
                            nrow(miss), " -- e.g. ",
                            paste(head(miss[, paste0(dist_final,"/",province)], 6), collapse=", "))
  }
  out[]
}
