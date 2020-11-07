# Johns-Hopkins format

library(dplyr)
library(glue)
require(lubridate)
require(stringr)

x = readr::read_csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
x$data <- lubridate::as_date(x$data)
x$codice_regione <- as.numeric(x$codice_regione)

level_tst <- levels(factor(x$denominazione_regione[which(x$codice_regione == 4)]))
x$codice_regione[which(x$denominazione_regione == level_tst[2])] = -1

x <- x %>%
  group_by(codice_regione) %>%
  arrange(data) %>%
  mutate(denominazione_regione = first(denominazione_regione)) %>%
  ungroup()

x <- x %>%
  select(
    data,
    denominazione_regione,
    stato,
    lat,
    long,
    totale_casi,
    dimessi_guariti,
    deceduti
  ) %>%
  rename(
    `Province/State` = "denominazione_regione",
    Lat = "lat",
    Long = "long"
  ) %>%
  mutate(
    data = str_trim(glue("{month(data)}/{day(data)}/{substr(year(data), 3, 4)}"))
  ) %>%
  rename(`Country/Region` = stato) %>%
  mutate(`Country/Region` = "Italia")

x_confirmed <- x %>%
  select(-dimessi_guariti, -deceduti) %>%
  mutate(Lat = first(Lat), Long = first(Long)) %>%
  ungroup() %>%
  group_by(`Province/State`, `Country/Region`) %>%
  tidyr::pivot_wider(id_cols = c(`Province/State`, `Country/Region`,  "Lat", "Long"), names_from = data, values_from = totale_casi) %>% mutate_all(funs(tidyr::replace_na(.,0))) %>%
  bind_rows(readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
  filter(`Country/Region` != "Italy") %>%
  group_by(`Province/State`, `Country/Region`) %>%
  slice_head() %>%
  .[,c(1:4, 4 + order(as.Date(names(.)[-c(1:4)],format="%m/%d/%Y")))]


readr::write_csv(x_confirmed, "docs/time_series_19-covid-Confirmed_Italy.csv")

x_deaths <- x %>%
  select(-dimessi_guariti, -totale_casi) %>%
  tidyr::pivot_wider(id_cols = c(`Province/State`, `Country/Region`,  "Lat", "Long"), names_from = data, values_from = deceduti) %>% mutate_all(funs(tidyr::replace_na(.,0))) %>%
  bind_rows(readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>%
  filter(`Country/Region` != "Italy") %>% 
  group_by(`Province/State`, `Country/Region`, Lat, Long) %>%
  slice_head() %>%
  .[,c(1:4, 4 + order(as.Date(names(.)[-c(1:4)],format="%m/%d/%Y")))]

readr::write_csv(x_deaths, "docs/time_series_19-covid-Deaths_Italy.csv")

x_recovered <- x %>%
  select(-deceduti, -totale_casi) %>%
  tidyr::pivot_wider(id_cols = c(`Province/State`, `Country/Region`,  "Lat", "Long"), names_from = data, values_from = dimessi_guariti) %>% mutate_all(funs(tidyr::replace_na(.,0)))

readr::write_csv(x_recovered, "docs/time_series_19-covid-Recovered_Italy.csv")
