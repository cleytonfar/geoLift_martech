library(GeoLift)
library(data.table)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# 1. data preparation ----

## reading:
creds = fread("data/creds_autocred.csv")

## checking:
creds %>% glimpse

## cleaning city column:

### copying to municipality:
creds[, municipality := city]
### cleaning:
creds[, municipality := iconv(municipality, to="ASCII//TRANSLIT")][]
creds[, municipality := str_to_lower(municipality)]
creds[, municipality := str_replace_all(municipality, "[:space:]{2,}", " ")][]
creds[str_detect(city, "[()]"), municipality := str_extract_all(municipality, "\\((.+)\\)", simplify = T)][]
creds[, municipality := str_remove_all(municipality, "[\\(\\)]")][]

### cleaning state column:
creds[, state := iconv(state, to="ASCII//TRANSLIT")]
creds[, state := str_to_lower(state)]
creds[, state := case_when(state == "sao paulo" ~ "sp",
                           state == "rio de janeiro" ~ "rj",
                           state == "bh" ~ "mg",
                           state == "para" ~ "pa",
                           state == "bahia" ~ "ba",
                           .default = state)][]

### grouping:
creds2 = creds[, .(creds = sum(creds)), .(date, municipality, state)]
creds2

