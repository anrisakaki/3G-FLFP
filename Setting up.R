library(tidyverse)
library(haven)
library(sf)
library(fixest)
library(extrafont)
library(lubridate)
library(sjPlot)
library(sfheaders)
library(broom)
library(patchwork)
library(srvyr)
library(survey)
library(lubridate)
library(units)
library(purrr)
library(did)
library(terra)
library(exactextractr)

rm(list=ls())

opencell <- read.csv("Raw data/Vietnam_Cell_tower.csv")

vnmap1 <- read_sf("Raw data/VNShapefile/gadm36_VNM_1.shp")
vnmap2 <- read_sf("Raw data/VNShapefile/gadm36_VNM_2.shp")

dist9919 <- read_dta("Raw data/Consistent 2019 to 1999 wards with 1999 districts.dta")

wards_rural <- read.csv("Raw Data/rural_urban_wards.csv")

# Collins Bartholomew 
cb11 <- rast("Raw Data/Collins Bartholomew/hc904nr7037/MCE_Global3G_2011.tif")
cb12 <- rast("Raw Data/Collins Bartholomew/wx910xj1289/MCE_Global3G_2012.tif")
cb13 <- rast("Raw Data/Collins Bartholomew/dg771jv6579/MCE_Global3G_2013.tif")
cb14 <- rast("Raw Data/Collins Bartholomew/pd038fr9690/MCE_Global3G_2014.tif")
cb15 <- rast("Raw Data/Collins Bartholomew/pd038fr9690/MCE_Global3G_2015.tif")
cb16 <- rast("Raw Data/Collins Bartholomew/ky819sb7704/MCE_Global3G_2016.tif")
cb17 <- rast("Raw Data/Collins Bartholomew/by170qy6709/MCE_Global3G_2017.tif")

# Population data
pop_files <- list.files("Raw Data/Population Data", pattern = "\\.tif$", full.names = TRUE)
pop_years <- as.integer(sub(".*vnm_ppp_(\\d{4})\\.tif$", "\\1", pop_files))
pop_list <- lapply(pop_files, terra::rast)
names(pop_list) <- paste0("pop", substr(pop_years, 3, 4))

# Export data
export_ctrl <- read_dta(file = "Clean data/province level/export_province_year_isic4.dta")

# VHLSS
## 2004
ho1_04 <- read_dta(file = "Raw data/VHLSS/2004/ho1.dta")
m123a_04 <- read_dta(file = "Raw data/VHLSS/2004/m1_2_3a.dta")
m4a_04 <- read_dta(file = "Raw data/VHLSS/2004/m4a.dta")
m4c1_04 <- read_dta(file = "Raw data/VHLSS/2004/m4c1.dta")
m6b_04 <- read_dta(file = "Raw data/VHLSS/2004/m6b.dta")
wt04 <- read_dta(file = "Raw data/VHLSS/2004/hhinc04.dta")

## 2006 
m1a_06 <- read_dta(file = "Raw data/VHLSS/2006/muc1a.dta")
m1b_06 <- read_dta(file = "Raw data/VHLSS/2006/muc1b.dta")
m2a_06 <- read_dta(file = "Raw data/VHLSS/2006/muc2a.dta")
m4a_06 <- read_dta(file = "Raw data/VHLSS/2006/muc4a.dta")
m4c_06 <- read_dta(file = "Raw data/VHLSS/2006/muc4c.dta")
m6b_06 <- read_dta(file = "Raw data/VHLSS/2006/muc6b.dta")
wt06 <- read_dta(file = "Raw data/VHLSS/2006/hhinc06.dta")

## 2008
ho1_08 <- read_dta(file = "Raw data/VHLSS/2008/Hhold/ho.dta")
m123a_08 <- read_dta(file = "Raw data/VHLSS/2008/Hhold/muc123a.dta")
m4a_08 <- read_dta(file = "Raw data/VHLSS/2008/Hhold/muc4a.dta")
m4c1_08 <- read_dta(file = "Raw data/VHLSS/2008/Hhold/muc4c1.dta")
m4am_08 <- read_dta(file = "Raw data/VHLSS/2008/Hhold/muc4am.dta")
m6b_08 <- read_dta(file = "Raw data/VHLSS/2008/Hhold/muc6b.dta")
wt08 <- read_dta(file = "Raw data/VHLSS/2008/Hhold/weight08new4.dta")

## 2010
ho1_10 <- read_dta(file = "Raw data/VHLSS/2010/ho11.dta")
m1a_10 <- read_dta(file = "Raw data/VHLSS/2010/muc1a.dta")
m2a1_10 <- read_dta(file = "Raw data/VHLSS/2010/muc2a1.dta")
m4a1_10 <- read_dta(file = "Raw data/VHLSS/2010/muc4a1.dta")
m4a2_10 <- read_dta(file = "Raw data/VHLSS/2010/muc4a2.dta")
m4a3_10 <- read_dta(file = "Raw data/VHLSS/2010/muc4a3.dta")
m4a4_10 <- read_dta(file = "Raw data/VHLSS/2010/muc4a4.dta")
m4c1_10 <- read_dta(file = "Raw data/VHLSS/2010/muc4c1.dta")
m6b_10 <- read_dta(file = "Raw data/VHLSS/2010/muc6b.dta")
wt10 <- read_dta(file = "Raw data/VHLSS/2010/wt10.dta")

## 2012
ho1_12 <- read_dta(file = "Raw data/VHLSS/2012/ho11.dta")
m1a_12 <- read_dta(file = "Raw data/VHLSS/2012/Muc1A.dta")
m1c_12 <- read_dta(file = "Raw data/VHLSS/2012/Muc1C.dta")
m2a_12 <- read_dta(file = "Raw data/VHLSS/2012/Muc2A1.dta")
m4c1_12 <- read_dta(file = "Raw data/VHLSS/2012/Muc4C1.dta")
m6b_12 <- read_dta(file = "Raw data/VHLSS/2012/Muc6B.dta")
wt12 <- read_dta(file = "Raw data/VHLSS/2012/wt2012new.dta")

## 2014
ho1_14 <- read_dta(file = "Raw data/VHLSS/2014/Ho1.dta")
m1a_14 <- read_dta(file = "Raw data/VHLSS/2014/Muc1A.dta")
m1c_14 <- read_dta(file = "Raw data/VHLSS/2014/Muc1C.dta")
m2a_14 <- read_dta(file = "Raw data/VHLSS/2014/Muc2A.dta")
m4a_14 <- read_dta(file = "Raw data/VHLSS/2014/Muc4A.dta")
m4c1_14 <- read_dta(file = "Raw data/VHLSS/2014/Muc4C1.dta")
m6b_14 <- read_dta(file = "Raw data/VHLSS/2014/Muc6B.dta")
wt14 <- read_dta(file = "Raw data/VHLSS/2014/wt2014.dta")

## 2016
ho1_16 <- read_dta(file = "Raw data/VHLSS/2016/Household/Ho1.dta")
m1a_16 <- read_dta(file = "Raw data/VHLSS/2016/Household/Muc1A.dta")
m2ab_16 <- read_dta(file = "Raw data/VHLSS/2016/Household/Muc2AB.dta")
m4a_16 <- read_dta(file = "Raw data/VHLSS/2016/Household/Muc4A.dta")
m4c1_16 <- read_dta(file = "Raw data/VHLSS/2016/Household/Muc4C1.dta")
m6b_16 <- read_dta(file = "Raw data/VHLSS/2016/Household/Muc6B.dta")
wt16 <- read_dta(file = "Raw data/VHLSS/2016/Household/wt16.dta")

## 2018
ho1_18 <- read_dta(file = "Raw data/VHLSS/2018/2 - Data/1 - Households/Ho1.dta")
m1a_18 <- read_dta(file = "Raw data/VHLSS/2018/2 - Data/1 - Households/MUC1A.dta")
m2v_18 <- read_dta(file = "Raw data/VHLSS/2018/2 - Data/1 - Households/MUC2V.dta")
m4a_18 <- read_dta(file = "Raw data/VHLSS/2018/2 - Data/1 - Households/MUC4A.dta")
m4c1_18 <- read_dta(file = "Raw data/VHLSS/2018/2 - Data/1 - Households/MUC4C1.dta")
m6b_18 <- read_dta(file = "Raw data/VHLSS/2018/2 - Data/1 - Households/MUC6B.dta")
wt18 <- read_dta(file = "Raw data/VHLSS/2018/2 - Data/1 - Households/wt18.dta")

## 2020 
ho1_20 <- read_dta(file = "Raw Data/VHLSS/2020/VHLSS 2020_Household Eng full/Data VHLSS2020_HH_Eng/HO1.dta")
m1a_20 <- read_dta(file = "Raw data/VHLSS/2020/VHLSS 2020_Household Eng full/Data VHLSS2020_HH_Eng/MUC1A.dta")
m2v_20 <- read_dta(file = "Raw data/VHLSS/2020/VHLSS 2020_Household Eng full/Data VHLSS2020_HH_Eng/MUC2V.dta")
m4a_20 <- read_dta(file = "Raw data/VHLSS/2020/VHLSS 2020_Household Eng full/Data VHLSS2020_HH_Eng/MUC4A.dta")
m4c1_20 <- read_dta(file = "Raw data/VHLSS/2020/VHLSS 2020_Household Eng full/Data VHLSS2020_HH_Eng/MUC4C1.dta")
m6b_20 <- read_dta(file = "Raw data/VHLSS/2020/VHLSS 2020_Household Eng full/Data VHLSS2020_HH_Eng/MUC6B.dta")
wt20 <- read_dta(file = "Raw data/VHLSS/2020/VHLSS 2020_Household Eng full/Data VHLSS2020_HH_Eng/wt2020.dta")

# Panel

panel1018 <- read_dta(file = "Clean data/panel_final_10-18.dta")

# LFS 
lfs10_distid <- read.csv("Raw Data/LFS/lfs_dist_10.csv")
lfs11_distid <- read.csv("Raw Data/LFS/lfs_dist_11.csv")
lfs12_distid <- read.csv("Raw Data/LFS/lfs_dist_12.csv")
lfs13_distid <- read.csv("Raw Data/LFS/lfs_dist_13.csv")
lfs14_distid <- read.csv("Raw Data/LFS/lfs_dist_14.csv")

lfs10 <- read_sav("Raw data/LFS/Micr_LFS_2010-2014/LFS_2010_final_DCTDT_GUI.sav")
lfs11 <- read_sav("Raw data/LFS/Micr_LFS_2010-2014/LFS_2011_final_DCTDT_GUI.sav")
lfs12 <- read_sav("Raw data/LFS/Micr_LFS_2010-2014/LFS-2012-add_var_weight_goc.sav")
lfs12.a <- read_sav("Raw data/LFS/Micr_LFS_2010-2014/LFS_2012_final_DCTDT_GUI.sav")
lfs13 <- read_sav("Raw data/LFS/Micr_LFS_2010-2014/LFS_2013_final_DCTDT_GUI.sav")
lfs14 <- read_sav("Raw data/LFS/Micr_LFS_2010-2014/LFS_2014_final_DCTDT_GUI.sav")
lfs15 <- read_dta(file = "Raw data/LFS/LFS_2015_final_full.dta")
lfs16 <- read_dta(file = "Raw data/LFS/LFS_2016_final_full.dta")
lfs17 <- read_dta(file = "Raw data/LFS/LFS_2017_final_full.dta")
lfs18 <- read_dta(file = "Raw data/LFS/LFS_2018_final_full.dta")
lfs19 <- read_dta(file = "Raw data/LFS/LFS_2019_final_full.dta")
lfs20 <- read_dta(file = "Raw data/LFS/LFS_2020_final_full.dta")

# VES 
ec <- list.files("VES/DN", pattern = "^dn.*\\.dta$", full.names = TRUE)

ec_list <- lapply(ec, read_dta)

ec_gender <- list.files("VES/Gender data", pattern = "^dn.*\\.dta$", full.names = TRUE)
ecgender_list <- lapply(ec_gender, read_dta)
