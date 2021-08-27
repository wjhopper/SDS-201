library(dplyr)
library(tidyr)
library(ggplot2)
library(skimr)

fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')

fishing_cleaned <- mutate(fishing,
                          species = recode(species,
                                           `Amercian Eel`="American Eel",
                                           Bullheads="Bullhead",
                                           `Channel catfish` = "Channel Catfish",
                                           Crappies = "Crappie", 
                                           `Freshwater Drum` = "Drum",
                                           `Pacific salmon` = "Pacfic Salmon",
                                           `White bass` = "White Bass"
                                           )
                          ) %>%
  filter(!grepl(" and ", species)) %>%
  filter(region != "U.S. Total") %>%
  group_by(year, lake, species) %>%
  summarize(values=sum(values, na.rm = TRUE), .groups = "drop")

candidates <- fishing_cleaned %>%
  filter(!is.na(values)) %>%
  count(species, lake) %>%
  filter(n > 20) %>%
  arrange(species, lake) %>%
  group_by(species) %>%
  filter(n() > 1)
  
wide_values <- inner_join(fishing_cleaned,
                          distinct(candidates, species),
                          by="species") %>%
  spread(species, values)

values_correlations <- cor(wide_values[, 4:ncol(wide_values)],
                           use = "pairwise.complete.obs"
                           )

values_correlations[upper.tri(values_correlations)] <- NA

values_correlations <- values_correlations %>%
  as_tibble(rownames = "species1") %>%
  gather("species2", "cor", -species1) %>%
  filter(!is.na(cor)) %>%
  filter(cor < 1 & cor > .3)

# ggplot(data = values_correlations, aes(species1, species2, fill = cor))+
#   geom_tile(color = "white")+
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                        midpoint = 0, limit = c(-1,1), space = "Lab", 
#                        name="Pearson\nCorrelation") +
#   theme_minimal()+ 
#   theme(axis.text.x = element_text(angle = 90, vjust = 1, 
#                                    size = 12, hjust = 1))+
#   coord_fixed()


# for (i in 1:nrow(values_correlations)) {
#   x <- values_correlations$species1[i]
#   y <- values_correlations$species2[i]
#   
#     p <- ggplot(wide_values,
#            aes_(x=as.name(x), y=as.name(y), color=as.name('lake'))
#     ) +
#       geom_point() +
#       geom_smooth(method = lm, se=FALSE)
#     print(p)
# }
# 
# ggplot(wide_totals,
#        aes(x=`Round Whitefish`, y=`Gizzard Shad`, color = lake)
#        ) +
#   geom_point() +
#   geom_smooth(method = lm, se=FALSE)

great_lakes_fishing <- select(wide_values, Year=year, Lake=lake,
                              `Rainbow Smelt`, `Suckers`,
                              `Lake Trout`, `Walleye`, `Channel Catfish`,
                              `Burbot`, `Carp`, `Yellow Perch`
                              )

write.csv(great_lakes_fishing, "fishing.csv", row.names = FALSE)
