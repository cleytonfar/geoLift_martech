# 0. Attaching packages ----
library(GeoLift)
library(data.table)
library(gridExtra)
library(ggplot2)
library(patchwork)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)
library(geobr)
library(tsibble)

# 1. Data preparation ----

## reading:
creds = fread("data/creds_autocred.csv")

## checking:
creds %>% glimpse

# convert to date:
creds[, date := ymd(date)]

## cleaning city column:

### copying to municipality:
creds[, municipality := city]
### cleaning:

creds[, municipality := iconv(municipality, to = "ASCII//TRANSLIT")][]
creds[, municipality := str_to_lower(municipality)]
creds[, municipality := str_squish(municipality)][]
creds[str_detect(city, "[()]"), municipality := str_extract_all(municipality, "\\((.+)\\)", simplify = T)][]
creds[, municipality := str_remove_all(municipality, "[\\(\\)]")][]

### cleaning state column:
creds[, state := iconv(state, to = "ASCII//TRANSLIT")]
creds[, state := str_to_lower(state)]
creds[, state := case_when(state == "sao paulo" ~ "sp",
                           state == "rio de janeiro" ~ "rj",
                           state == "bh" ~ "mg",
                           state == "para" ~ "pa",
                           state == "bahia" ~ "ba",
                           .default = state)][]

# hard-coded cleaning:
creds[municipality == "santana do livramento", municipality := "sant'ana do livramento"]
creds[municipality == "lagoa do itaenga", municipality := "lagoa de itaenga"]
creds[state == "go" & municipality == "bom jesus" , municipality := "bom jesus de goias"]
creds[state == "pa" & municipality == "santa isabel do para" , municipality := "santa izabel do para"]
creds[state == "rn" & municipality == "arez" , municipality := "ares"]
creds[state == "ma" & municipality == "pindare mirim" , municipality := "pindare-mirim"]

# create dataframe of all BR cities with their indexing to higher aggregation
# units from geobr
df_br_map <- lookup_muni(name_muni = "all")
setDT(df_br_map)

# cleaning names:
df_br_map[, municipality := clean_name(name_muni)]
df_br_map[, state := clean_name(abbrev_state)]
df_br_map[, microrregiao := clean_name(name_micro)][]
df_br_map[, mesorregiao := clean_name(name_meso)][]

## hard-coded cleaning:
df_br_map[microrregiao == "itapecerica da serra" & municipality == "embu", municipality := "embu das artes"]
df_br_map[code_muni == 3530805, municipality := "mogi mirim"]
df_br_map[code_muni == 2306306, municipality := "itapaje"]
df_br_map[code_muni == 1506500, municipality := "santa izabel do para"]
df_br_map[code_muni == 2515401, municipality := "sao vicente do serido"]
df_br_map[code_muni == 2401305, municipality := "campo grande"]
df_br_map[code_muni == 2410306, municipality := "serra caiada"]

# merging:
creds2 = merge(
    creds,
    df_br_map[, .(municipality, code_muni, state, code_state, microrregiao, code_micro, mesorregiao, code_meso)],
    by = c("state", "municipality"),
    all.x = T
)

# % de cidades covered:
creds2[!is.na(mesorregiao), n_distinct(state, municipality)] / df_br_map[, n_distinct(code_muni)]

# cities not covered:
creds2[is.na(mesorregiao), .N, .(state, municipality)][order(-N)]

### grouping:
creds_mesorregiao = creds2[!is.na(mesorregiao), .(creds = sum(creds)), .(date, code_meso, mesorregiao)]
setorder(creds_mesorregiao, date)

# 2. Organizing data for GeoLift ----
creds_mesorregiao = creds_mesorregiao[date >= '2021-01-01']

# guaranteeing the whole period for all regions:
# GeoLift requires a balanced panel for all regions; 
# otherwise, it is going to throw an error.
creds_mesorregiao = merge(
    creds_mesorregiao[, .(date = seq(min(creds_mesorregiao$date), today(), by = '1 day')), .(code_meso, mesorregiao)],
    creds_mesorregiao,
    by = c("date", "code_meso", "mesorregiao"),
    all.x = T
)
# fillna with 0:
creds_mesorregiao[is.na(creds), creds := 0]

# convert to geolift format:
GeoTestData_PreTest <- GeoDataRead(
    data = creds_mesorregiao[, .(date, mesorregiao, creds)],
    date_id = "date",
    location_id = "mesorregiao",
    Y_id = "creds",
    X = c(), #empty list as we have no covariates
    format = "yyyy-mm-dd",
    summary = TRUE
)
setDT(GeoTestData_PreTest )

### Plotting 
GeoPlot(
    GeoTestData_PreTest,
    Y_id = "Y",
    time_id = "time",
    location_id = "location"
)

# 3. Power Analysis ----
MarketSelections <- GeoLiftMarketSelection(
    data = GeoTestData_PreTest,
    treatment_periods = c(10, 15, 20),
    N = c(2, 3, 4, 5),
    Y_id = "Y",
    location_id = "location",
    time_id = "time",
    effect_size = seq(0, 0.2, 0.05),
    lookback_window = 1, 
    #include_markets = c("chicago"),
    #exclude_markets = c("honolulu"),
    cpic = 450,
    budget = 25000,
    alpha = 0.1,
    Correlations = TRUE,
    fixed_effects = TRUE,
    side_of_test = "two_sided"
)


# saving:
list(
    data = creds_mesorregiao,
    GeoTestData_PreTest = GeoTestData_PreTest,
    MarketSelections = MarketSelections
     ) %>% 
    saveRDS("output/geolift_20240516.rds")

# 4. Get the weights

# We can get the weights
weights <- GetWeights(
    Y_id = "Y",
    location_id = "location",
    time_id = "time",
    data = GeoTestData_PreTest,
    locations = c("centro maranhense", "leste alagoano"),
    pretreatment_end_time = 90,
    fixed_effects = TRUE
)

weights %>%
    arrange(weight) %>% 
    mutate(weight = weight %>% round(3))

