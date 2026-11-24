library(RColorBrewer)
library(dplyr)
map <- read_map("410101")
data <- read_excel("chen.xlsx") |>
  select(all, male_inci, female_inci, male_mort, female_mort) |>
  mutate(adcode = as.integer(all)) |>
  mutate(
    adcode = case_when(
      adcode == 410123 ~ 410181,
      adcode == 410224 ~ 410212,
      adcode == 410322 ~ 410306,
      adcode == 410307 ~ 410311,
      adcode == 410524 ~ 410526,
      adcode == 410780 ~ 410783,
      adcode == 411222 ~ 411203,
      adcode == 411626 ~ 411603,
      adcode == 411680 ~ 411628,
      TRUE ~ adcode
    )
  )
map2 <- map |>
  left_join(data, by = c("adcode"))


p1 <- ggplot(map2) +
  geom_sf(aes(fill = desc(male_inci))) +
  scale_fill_gradient(
    low = "black",
    high = "white",
    na.value = "grey",
    name = "Crude Rate"
  ) +
  facet_wrap(~aaa) +
  theme_void()
ggtitle("Male, incidence")

p2 <- ggplot(map2) +
  geom_sf() +
  theme_void() +
  geom_sf(aes(fill = desc(female_inci))) +
  scale_fill_gradient(
    low = "darkred",
    high = "white",
    na.value = "grey",
    name = "Crude Rate"
  ) +
  ggtitle("Female, incidence")

p3 <- ggplot(map2) +
  geom_sf() +
  theme_void() +
  geom_sf(aes(fill = desc(male_mort))) +
  scale_fill_gradient(
    low = "darkblue",
    high = "white",
    na.value = "grey",
    name = "Crude Rate"
  ) +
  ggtitle("Male, Mortality")
p4 <- ggplot(map2) +
  geom_sf() +
  theme_void() +
  geom_sf(aes(fill = desc(female_mort))) +
  scale_fill_gradient(
    low = "darkblue",
    high = "white",
    na.value = "grey",
    name = "Crude Rate"
  ) +
  ggtitle("Female, Mortality")

library(patchwork)
(p1 + p2) /
  (p3 + p4) +
  plot_annotation(
    title = "Geographical distribution of esophageal cancer incidence and mortality in 2020 in Henan Province, China",
    caption = "Data were sourced from Henan Cancer Registration Report (2023)",
    tag_levels = "A"
  )

data3 <- data2 |>
  mutate(areacode = as.integer(areacode)) |>
  mutate(
    areacode = case_when(
      areacode == 410123 ~ 410181,
      areacode == 410224 ~ 410212,
      areacode == 410322 ~ 410306,
      areacode == 410307 ~ 410311,
      areacode == 410524 ~ 410526,
      areacode == 410780 ~ 410783,
      areacode == 411222 ~ 411203,
      areacode == 411626 ~ 411603,
      areacode == 411680 ~ 411628,
      TRUE ~ areacode
    )
  )
