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

##################
# DISTRICT-LEVEL #
##################

etable(list(
  feols(work ~ dist_coverage_share + age + age^2 + as.factor(female) + yrschool + nchild | 
          dist + year, subset(vhlss_all_dist, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~dist),
  feols(informal ~ dist_coverage_share + age + age^2 + as.factor(female) + yrschool + nchild | 
          dist + year, subset(vhlss_all_dist, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~dist),
  feols(agri_informal2 ~ dist_coverage_share + age + age^2 + as.factor(female) + yrschool + nchild | 
          dist + year, subset(vhlss_all_dist, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~dist),
  feols(manu_informal2 ~ dist_coverage_share + age + age^2 + as.factor(female) + yrschool + nchild | 
          dist + year, subset(vhlss_all_dist, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~dist),
  feols(service_informal2 ~ dist_coverage_share + age + age^2 + as.factor(female) + yrschool + nchild | 
          dist + year, subset(vhlss_all_dist, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~dist)), tex = T, dict = dict)

etable(list(
  feols(work ~ dist_coverage + age + age^2 + as.factor(female) + yrschool + nchild | 
          dist + year, subset(vhlss_all_dist, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~dist),
  feols(informal ~ dist_coverage + age + age^2 + as.factor(female) + yrschool + nchild | 
          dist + year, subset(vhlss_all_dist, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~dist),
  feols(agri_informal2 ~ dist_coverage + age + age^2 + as.factor(female) + yrschool + nchild | 
          dist + year, subset(vhlss_all_dist, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~dist),
  feols(manu_informal2 ~ dist_coverage + age + age^2 + as.factor(female) + yrschool + nchild | 
          dist + year, subset(vhlss_all_dist, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~dist),
  feols(service_informal2 ~ dist_coverage + age + age^2 + as.factor(female) + yrschool + nchild | 
          dist + year, subset(vhlss_all_dist, age > 19 & age < 65 & year > 2008), weights = ~hhwt, vcov = ~dist)), tex = T, dict = dict)

# DiD

vhlss_all_dist_did <- vhlss_all_dist %>% 
  mutate(time_to_treat = ifelse(is.na(first_treated), -1000, time_to_treat),
         first.treat.csdid = ifelse(is.na(first_treated), 0, first_treated),
         first_treated2 = ifelse(time_to_treat == -1000, 10000, first_treated))

vhlss_dist_did <- vhlss_all_dist_did %>% 
  filter(age > 19 & age < 65) %>% 
  group_by(year, dist, time_to_treat, first_treated2, first.treat.csdid) %>% 
  summarise(
    work = weighted.mean(work, hhwt, na.rm = T),
    informal = weighted.mean(informal, hhwt, na.rm = T),
    agri = weighted.mean(agri, hhwt, na.rm = T),
    manu = weighted.mean(manu, hhwt, na.rm = T),
    service = weighted.mean(service, hhwt, na.rm = T),
    service_informal = weighted.mean(service_informal2, na.rm = T)
  )

csdid_informal_dist <- aggte(att_gt(yname = "informal",
                         gname = "first.treat.csdid",
                         idname = "dist",
                         tname = "year",
                         control_group = "notyettreated",
                         data = vhlss_dist_did), type = "dynamic")
ggdid(csdid_informal_dist, title = "(cs)did")

csdid_informal_df <- as.data.frame(csdid_informal_dist$egt)
csdid_informal_df$att <- csdid_informal_dist$att.egt
csdid_informal_df$se <- csdid_informal_dist$se.egt
csdid_informal_df$model <- "C&S"
colnames(csdid_informal_df)[1] <- "event_time"

twfe_informal <- feols(informal ~ i(time_to_treat, ref = c(-2, -1000)) | 
        dist + year, vhlss_dist_did, vcov = ~dist)
twfe_informal_df <- broom::tidy(twfe_informal, conf.int = TRUE)
twfe_informal_df$event_time <- as.numeric(gsub("time_to_treat::", "", twfe_informal_df$term))
twfe_informal_df <- twfe_informal_df[, c("event_time", "estimate", "std.error")]
twfe_informal_df$model <- "TWFE"
colnames(twfe_informal_df) <- c("event_time", "att", "se", "model")

informal_did <- rbind(csdid_informal_df, twfe_informal_df)
ggplot(informal_did, aes(x = event_time, y = att, color = model)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Share of workers in informal sector",
       x = "Time to treatment",
       y = "ATT",
       color = "Model") +
  theme_minimal()

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

