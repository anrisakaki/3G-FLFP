library(tidyverse)
library(haven)
library(sf)
library(fixest)
library(extrafont)
library(lubridate)
library(sjPlot)
library(sfheaders)
library(mgrs)
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
m123_08 <- read_dta(file = "VHLSS/2008/Hhold/muc123a.dta")
m4a_08 <- read_dta(file = "VHLSS/2008/Hhold/muc4a.dta")

## 2010
m1a_10 <- read_dta(file = "VHLSS/2010/muc1a.dta")
