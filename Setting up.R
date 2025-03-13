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

rm(list=ls())

opencell <- read.csv("Vietnam_Cell_tower.csv")

vnmap1 <- read_sf("VNShapefile/gadm36_VNM_1.shp")
vnmap2 <- read_sf("VNShapefile/gadm36_VNM_2.shp")

# VHLSS 
## 2008
ho1_08 <- read_dta(file = "VHLSS/2008/Hhold/ho.dta")
m123a_08 <- read_dta(file = "VHLSS/2008/Hhold/muc123a.dta")
m4a_08 <- read_dta(file = "VHLSS/2008/Hhold/muc4a.dta")
m4am_08 <- read_dta(file = "VHLSS/2008/Hhold/muc4am.dta")
wt08 <- read_dta(file = "VHLSS/2008/Hhold/weight08new4.dta")

## 2010
ho1_10 <- read_dta(file = "VHLSS/2010/ho11.dta")
m1a_10 <- read_dta(file = "VHLSS/2010/muc1a.dta")
m2a1_10 <- read_dta(file = "VHLSS/2010/muc2a1.dta")
m4a1_10 <- read_dta(file = "VHLSS/2010/muc4a1.dta")
m4a2_10 <- read_dta(file = "VHLSS/2010/muc4a2.dta")
m4a3_10 <- read_dta(file = "VHLSS/2010/muc4a3.dta")
m4a4_10 <- read_dta(file = "VHLSS/2010/muc4a4.dta")
wt10 <- read_dta(file = "VHLSS/2010/wt10.dta")

## 2012
ho1_12 <- read_dta(file = "VHLSS/2012/ho11.dta")
m1a_12 <- read_dta(file = "VHLSS/2012/Muc1A.dta")
m2a_12 <- read_dta(file = "VHLSS/2012/Muc2A1.dta")
wt12 <- read_dta(file = "VHLSS/2012/wt2012new.dta")

## 2014
ho1_14 <- read_dta(file = "VHLSS/2014/Ho1.dta")
m1a_14 <- read_dta(file = "VHLSS/2014/Muc1A.dta")
m2a_14 <- read_dta(file = "VHLSS/2014/Muc2A.dta")
m4a_14 <- read_dta(file = "VHLSS/2014/Muc4A.dta")
wt14 <- read_dta(file = "VHLSS/2014/wt2014.dta")

## 2016
ho1_16 <- read_dta(file = "VHLSS/2016/Household/Ho1.dta")
m1a_16 <- read_dta(file = "VHLSS/2016/Household/Muc1A.dta")
m2ab_16 <- read_dta(file = "VHLSS/2016/Household/Muc2AB.dta")
m4a_16 <- read_dta(file = "VHLSS/2016/Household/Muc4A.dta")
wt16 <- read_dta(file = "VHLSS/2016/Household/wt16.dta")

## 2018
ho1_18 <- read_dta(file = "VHLSS/2018/2 - Data/1 - Households/Ho1.dta")
m1a_18 <- read_dta(file = "VHLSS/2018/2 - Data/1 - Households/MUC1A.dta")
m2v_18 <- read_dta(file = "VHLSS/2018/2 - Data/1 - Households/MUC2V.dta")
m4a_18 <- read_dta(file = "VHLSS/2018/2 - Data/1 - Households/MUC4A.dta")
wt18 <- read_dta(file = "VHLSS/2018/2 - Data/1 - Households/wt18.dta")
