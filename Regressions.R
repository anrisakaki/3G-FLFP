load("Clean data/vhlss_all_dist.Rda")
load("Clean data/vhlss1014_panel.Rda")

dict = c("as.factor(female)" = "Female",
         "coverage_share" = "3G Coverage share",
         "age" = "Age",
         "coverage" = "3G Coverage",
         "dist_coverage" = "3G Coverage",
         "yrschool" = "Years school",
         "nchild" = "No. of children",
         "work" = "Work",
         "informal" = "Informal",
         "nonagri_informal2" = "Informal (Agriculture)",
         "manu_informal2" = "Informal (Manufacturing)",
         "service_informal2" = "Informal (Service)",
         "as.factor(female)1" = "Female")

# DiD

vhlss1014 <- vhlss1014 %>% 
  mutate(time_to_treat = ifelse(is.na(first_treated) | first_treated > 2014, -1000, time_to_treat),
         first.treat.csdid = ifelse(is.na(first_treated) | first_treated > 2014, 0, first_treated),
         first_treated2 = ifelse(time_to_treat == -1000, 10000, first_treated))

####################################################
# INDV-LEVEL - EXPLOITING DISTRICT LEVEL VARIATION #
####################################################

etable(list(
  feols(work ~ dist_coverage | ivid + year, subset(vhlss1014, age > 19 & age < 65), weights = ~hhwt, vcov = ~dist),
  feols(informal ~ dist_coverage | ivid + year, subset(vhlss1014, age > 19 & age < 65), weights = ~hhwt, vcov = ~dist),
  feols(agri_formal ~ dist_coverage | ivid + year, subset(vhlss1014, age > 19 & age < 65), weights = ~hhwt, vcov = ~dist),
  feols(manu_formal ~ dist_coverage | ivid + year, subset(vhlss1014, age > 19 & age < 65), weights = ~hhwt, vcov = ~dist),
  feols(service_formal ~ dist_coverage | ivid + year, subset(vhlss1014, age > 19 & age < 65), weights = ~hhwt, vcov = ~dist)), 
  tex = T, dict = dict)

##################
# DISTRICT-LEVEL #
##################

# TWFE

etable(list(
  feols(work ~ coverage | year + dist, vhlss_dist_did, vcov = ~dist),
  feols(informal ~ coverage | year + dist, vhlss_dist_did, vcov = ~dist),
  feols(agri_formal ~ coverage | year + dist, vhlss_dist_did, vcov = ~dist),
  feols(manu_formal ~ coverage | year + dist, vhlss_dist_did, vcov = ~dist),
  feols(service_formal ~ coverage | year + dist, vhlss_dist_did, vcov = ~dist)
), tex = T)

# EVENT STUDY

## Work

csdid_work_dist <- aggte(att_gt(yname = "work",
                                    gname = "first.treat.csdid",
                                    idname = "dist",
                                    tname = "year",
                                    control_group = "notyettreated",
                                    data = vhlss_dist_did), type = "dynamic")

csdid_work_df <- data.frame(
  event_time = csdid_work_dist$egt,
  att = csdid_work_dist$att.egt,
  se = csdid_work_dist$se.egt,
  model = "C&S"
)

twfe_work <- feols(work ~ i(time_to_treat, ref = c(-2, -1000)) | 
                         dist + year, vhlss_dist_did, vcov = ~dist)
twfe_work_df <- broom::tidy(twfe_work, conf.int = TRUE)
twfe_work_df$event_time <- as.numeric(gsub("time_to_treat::", "", twfe_work_df$term))
twfe_work_df <- twfe_work_df[, c("event_time", "estimate", "std.error")]
twfe_work_df$model <- "TWFE"
colnames(twfe_work_df) <- c("event_time", "att", "se", "model")

work_did <- rbind(csdid_work_df, twfe_work_df)

## Informality 

csdid_informal_dist <- aggte(att_gt(yname = "informal",
                         gname = "first.treat.csdid",
                         idname = "dist",
                         tname = "year",
                         control_group = "notyettreated",
                         data = vhlss_dist_did), type = "dynamic")

csdid_informal_df <- data.frame(
  event_time = csdid_informal_dist$egt,
  att = csdid_informal_dist$att.egt,
  se = csdid_informal_dist$se.egt,
  model = "C&S"
)

twfe_informal <- feols(informal ~ i(time_to_treat, ref = c(-2, -1000)) | 
        dist + year, vhlss_dist_did, vcov = ~dist)
twfe_informal_df <- broom::tidy(twfe_informal, conf.int = TRUE)
twfe_informal_df$event_time <- as.numeric(gsub("time_to_treat::", "", twfe_informal_df$term))
twfe_informal_df <- twfe_informal_df[, c("event_time", "estimate", "std.error")]
twfe_informal_df$model <- "TWFE"
colnames(twfe_informal_df) <- c("event_time", "att", "se", "model")

informal_did <- rbind(csdid_informal_df, twfe_informal_df)

## Agri informal 

csdid_agri_dist <- aggte(att_gt(yname = "agri_informal",
                                    gname = "first.treat.csdid",
                                    idname = "dist",
                                    tname = "year",
                                    control_group = "notyettreated",
                                    data = vhlss_dist_did), type = "dynamic")

csdid_agri_df <- data.frame(
  event_time = csdid_agri_dist$egt,
  att = csdid_agri_dist$att.egt,
  se = csdid_agri_dist$se.egt,
  model = "C&S"
)

twfe_agri <- feols(agri_informal ~ i(time_to_treat, ref = c(-2, -1000)) | 
                         dist + year, vhlss_dist_did, vcov = ~dist)
twfe_agri_df <- broom::tidy(twfe_agri, conf.int = TRUE)
twfe_agri_df$event_time <- as.numeric(gsub("time_to_treat::", "", twfe_agri_df$term))
twfe_agri_df <- twfe_agri_df[, c("event_time", "estimate", "std.error")]
twfe_agri_df$model <- "TWFE"
colnames(twfe_agri_df) <- c("event_time", "att", "se", "model")

agri_did <- rbind(csdid_agri_df, twfe_agri_df)

## Manu formal

csdid_manu_dist <- aggte(att_gt(yname = "manu_formal",
                                   gname = "first.treat.csdid",
                                   idname = "dist",
                                   tname = "year",
                                   control_group = "notyettreated",
                                   data = vhlss_dist_did), type = "dynamic")
twfe_manu <- feols(manu_formal ~ i(time_to_treat, ref = c(-2, -1000)) | 
                        dist + year, vhlss_dist_did, vcov = ~dist)

csdid_manu_df <- data.frame(
  event_time = csdid_manu_dist$egt,
  att = csdid_manu_dist$att.egt,
  se = csdid_manu_dist$se.egt,
  model = "C&S"
)

twfe_manu_df <- broom::tidy(twfe_manu, conf.int = TRUE)
twfe_manu_df$event_time <- as.numeric(gsub("time_to_treat::", "", twfe_manu_df$term))
twfe_manu_df <- twfe_manu_df[, c("event_time", "estimate", "std.error")]
twfe_manu_df$model <- "TWFE"
colnames(twfe_manu_df) <- c("event_time", "att", "se", "model")

manu_did <- rbind(csdid_manu_df, twfe_manu_df)

## Service formal

csdid_service_dist <- aggte(att_gt(yname = "service_formal",
                                    gname = "first.treat.csdid",
                                    idname = "dist",
                                    tname = "year",
                                    control_group = "notyettreated",
                                    data = vhlss_dist_did), type = "dynamic")
twfe_service <- feols(service_formal ~ i(time_to_treat, ref = c(-2, -1000)) | 
                         dist + year, vhlss_dist_did, vcov = ~dist)

csdid_service_df <- data.frame(
  event_time = csdid_service_dist$egt,
  att = csdid_service_dist$att.egt,
  se = csdid_service_dist$se.egt,
  model = "C&S"
)

twfe_service_df <- broom::tidy(twfe_service, conf.int = TRUE)
twfe_service_df$event_time <- as.numeric(gsub("time_to_treat::", "", twfe_service_df$term))
twfe_service_df <- twfe_service_df[, c("event_time", "estimate", "std.error")]
twfe_service_df$model <- "TWFE"
colnames(twfe_service_df) <- c("event_time", "att", "se", "model")

service_did <- rbind(csdid_service_df, twfe_service_df)

# Female vs male 

##################
# PROVINCE-LEVEL #
##################

vhlss_all_did <- vhlss_all %>%
  mutate(time_to_treated = ifelse(first_treated > 2012, -1000, time_to_treated),
         first_treated = ifelse(first_treated > 2012, 10000, first_treated))

# VHLSS

etable(list(
  feols(work ~ coverage_share + age + age^2 + as.factor(female) + yrschool + nchild | 
          tinh + year, subset(vhlss_all, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~tinh),
  feols(informal ~ coverage_share + age + age^2 + as.factor(female) + yrschool + nchild | 
               tinh + year, subset(vhlss_all, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~tinh),
  feols(agri_informal2 ~ coverage_share + age + age^2 + as.factor(female) + yrschool + nchild | 
          tinh + year, subset(vhlss_all, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~tinh),
  feols(manu_informal2 ~ coverage_share + age + age^2 + as.factor(female) + yrschool + nchild | 
          tinh + year, subset(vhlss_all, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~tinh),
  feols(service_informal2 ~ coverage_share + age + age^2 + as.factor(female) + yrschool + nchild | 
          tinh + year, subset(vhlss_all, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~tinh)), tex = T, dict = dict)

etable(list(
  feols(work ~ coverage + age + age^2 + as.factor(female) + yrschool + nchild | 
          tinh + year, subset(vhlss_all, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~tinh),
  feols(informal ~ coverage + age + age^2 + as.factor(female) + yrschool + nchild | 
          tinh + year, subset(vhlss_all, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~tinh),
  feols(agri_informal2 ~ coverage + age + age^2 + as.factor(female) + yrschool + nchild | 
          tinh + year, subset(vhlss_all, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~tinh),
  feols(manu_informal2 ~ coverage + age + age^2 + as.factor(female) + yrschool + nchild | 
          tinh + year, subset(vhlss_all, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~tinh),
  feols(service_informal2 ~ coverage + age + age^2 + as.factor(female) + yrschool + nchild | 
          tinh + year, subset(vhlss_all, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~tinh)), tex = T, dict = dict)

# LFS

etable(list(
  feols(work ~ coverage_share + age + age^2 + as.factor(female) + educattain + nchild | 
          tinh + year, subset(lfs_all, age > 19 & age < 65), weights = ~wt, vcov = ~tinh),
  feols(informal ~ coverage_share + age + age^2 + as.factor(female) + educattain + nchild | 
          tinh + year, lfs_all, weights = ~wt, vcov = ~tinh),
  feols(agri_informal ~ coverage_share + age + age^2 + as.factor(female) + educattain + nchild | 
          tinh + year, lfs_all, weights = ~wt, vcov = ~tinh),
  feols(manu_informal ~ coverage_share + age + age^2 + as.factor(female) + educattain + nchild | 
          tinh + year, lfs_all, weights = ~wt, vcov = ~tinh),
  feols(service_informal ~ coverage_share + age + age^2 + as.factor(female) + educattain + nchild | 
          tinh + year, lfs_all, weights = ~wt, vcov = ~tinh)), dict = dict)

etable(list(
  feols(work ~ coverage + age + age^2 + as.factor(female) + educattain + nchild | 
          tinh + year, subset(lfs_all, age > 19 & age < 65), weights = ~wt, vcov = ~tinh),
  feols(informal ~ coverage + age + age^2 + as.factor(female) + educattain + nchild | 
          tinh + year, lfs_all, weights = ~wt, vcov = ~tinh),
  feols(agri_informal ~ coverage + age + age^2 + as.factor(female) + educattain + nchild | 
          tinh + year, lfs_all, weights = ~wt, vcov = ~tinh),
  feols(manu_informal ~ coverage + age + age^2 + as.factor(female) + educattain + nchild | 
          tinh + year, lfs_all, weights = ~wt, vcov = ~tinh),
  feols(service_informal ~ coverage + age + age^2 + as.factor(female) + educattain + nchild | 
          tinh + year, lfs_all, weights = ~wt, vcov = ~tinh)), dict = dict)

