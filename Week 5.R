library(tidyverse)

#1
parrotfish <- read_csv("data/parrotfish.csv")

glimpse(parrotfish)

#2
parrotfish_long <- parrotfish %>%
  pivot_longer(
    cols = -c(YEAR, REGION, STRAT, PROT, PRIMARY_SAMPLE_UNIT),
    names_to = "SPECIES_CD",
    values_to = "density"
  )

parrotfish_long

#3

parrotfish_mean <- parrotfish_long %>%
  group_by(STRAT, PRIMARY_SAMPLE_UNIT) %>%
  summarise(total_density = sum(density))

parrotfish_mean <- parrotfish_mean %>%
  group_by(STRAT) %>%
  summarise(mean_density = mean(total_density))

parrotfish_mean



#4

taxonomic <- read_csv("data/taxonomic.csv")

taxonomic

parrotfish_tax <- parrotfish_long %>%
  left_join(taxonomic, by = "SPECIES_CD")



parrotfish_wide <- parrotfish_tax %>%
  select(
    YEAR,
    REGION,
    STRAT,
    PROT,
    PRIMARY_SAMPLE_UNIT,
    SCINAME,
    density
  ) %>%
  pivot_wider(
    names_from = SCINAME,
    values_from = density
  )

parrotfish_wide



#5 

parrotfish_sep <- parrotfish_long %>%
  separate(
    STRAT,
    into = c("HABITAT", "DEPTHCAT"),
    sep = "_",
    remove = FALSE
  )





#6
parrotfish_ID <- parrotfish_long %>%
  unite(
    ID,
    YEAR,
    PRIMARY_SAMPLE_UNIT,
    sep = "_",
    remove = FALSE
  )

parrotfish_ID

?separate


#7

parrotfish_richness <- parrotfish_sep %>%
  group_by(PRIMARY_SAMPLE_UNIT, DEPTHCAT) %>%
  summarise(
    richness = sum(density > 0)
  )

parrotfish_richness


#not shallow 