lfs_sum_dist_f_ddd <- lfs_sum_dist_f_ddd %>% 
  group_by(agegr, ID_2) %>% 
  mutate(dist_agegr = cur_group_id())

summary(ddd(yname = "work",
              tname = "year",
              idname = "dist_agegr",
              gname = "year_med_OCI", 
              pname = "agegr",
              xformla = ~lnexport_all,
              data = lfs_sum_dist_f_ddd,
              control_group = "notyettreated", 
              base_period = "universal",
              est_method = "dr"))


ggdid(aggte(att_gt(
  yname = "hhbus",
  gname = "year_med_OCI",
  idname = "ID_2",
  tname = "year",
  xformla = ~1,
  data = lfs_sum_dist_f,
  est_method = "reg",
  control_group = "notyettreated",
), type = "dynamic"))

ggdid(aggte(att_gt(
  yname = "hhbus",
  gname = "year_mean_OCI",
  idname = "ID_2",
  tname = "year",
  xformla = ~lnexport_all,
  data = lfs_sum_dist_f,
  est_method = "reg",
  control_group = "nevertreated"
), type = "dynamic"))

