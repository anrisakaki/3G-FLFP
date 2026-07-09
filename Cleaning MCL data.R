library(data.table)
library(tidyverse)

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

## ---- reshape mcl_main long: one row per (post, district) ----------------
## A post naming Quận 2 and Quận 7 becomes two rows sharing one id. Posts with
## no named district keep exactly one row (huyen NA), so no post is dropped.
## Rows are ordered by `pos`, the offset of the district's first mention, so
## post_row == 1 is the FIRST district the post names -- not the smallest code.
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

stopifnot(uniqueN(mcl_main$id) == n_posts,               # no post lost
          !anyDuplicated(mcl_main[, .(id, huyen)]),      # no district twice per post
          nrow(mcl_main[post_row == 1L]) == n_posts,     # exactly one anchor row each
          nrow(mcl_main[!is.na(huyen) & is.na(tinh)]) == 0)

save(mcl_main, file = "Clean data/mcl_main.Rda")

td_agg <- mcl_main %>%
  filter(post_row == 1) %>%
  group_by(year) %>%
  summarise(n = n())

ytop <- ceiling(max(td_agg$n) / 20000) * 20000  

ggplot(td_agg, aes(x = year, y = n)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = td_agg$year) +
  scale_y_continuous(limits = c(0, ytop), breaks = seq(0, ytop, 20000),
                     labels = scales::label_comma()) +
  labs(x = "Year", y = "Number of posts",
       title = "") +
  theme_classic()
ggsave("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Descriptive Stats/tuyen_dung_by_year.pdf", width = 8, height = 5)
