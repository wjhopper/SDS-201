library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-10-19/readme.md

# id: Year and type of plant
# place: Place/ranking
# weight_lbs: Weight in pounds
# grower_name: Name of grower
# city: City
# state_prov: State/Province
# country: Country
# gpc_site: GPC site (great pumpkin commonwealth)
# seed_mother: Seed mother
# pollinator_father: Father
# ott: "Over the top" inches (can be used to estimate weight, see https://www.backyardgardener.com/garden-forum-education/pumpkins/how-to-weigh-a-pumpkin/)
# est_weight: Estimated weight in lbs
# pct_chart: Percent on chart (percent change in of weight vs. estimated weight, I think)
# variety: Variety of pumpkin

pumpkins_raw <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')

# Some weird metadata about number of entries and number damaged in some rows,
# probably artifact from scraping html tables

pumpkins <- dplyr::filter(pumpkins_raw, !grepl("\\t", grower_name))

## Coerce some columns to numeric, since they were considered characters due to 
## that weird metadata being included as rows
## NB: some squashes don't place in a competition, if it was just a friendly 
## exhibition ("EXH") or if the squash was damaged (DMG)


pumpkins <- pumpkins %>%
  separate(id, into = c("year", "type")) %>%
  mutate(across(c(year, est_weight, weight_lbs, ott, place), parse_number)) %>%
  mutate(type = recode(.x = type,
                       F = "Field Pumpkin", P = "Giant Pumpkin",
                       S = "Giant Squash", W = "Giant Watermelon", 
                       L = "Long Gourd", T = "Tomato")
         )

## Long gourds measured by length in inches, not weight in pounds!
## So I guess we'll remove it

pumpkins <- filter(pumpkins, type != "Long Gourd")

ggplot(pumpkins, aes(x=factor(year), y=weight_lbs, color=type)) + 
  geom_boxplot()

## Some estimated weights are 0, which I'll take to mean "no weight estimated"
pumpkins <- mutate(pumpkins,
                   est_weight = replace(est_weight, est_weight==0, NA)
                   )

ggplot(pumpkins, mapping = aes(x=est_weight, y=weight_lbs, color=type)) +
  geom_point()

write.csv(pumpkins, file = "pumpkins.csv", row.names = FALSE)
