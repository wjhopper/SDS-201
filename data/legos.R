## Anna D. Peterson & Laura Ziegler (2021) Building a Multiple Linear Regression Model With LEGO Brick Data, Journal of Statistics and Data Science Education, 29:3, 297-303, DOI: 10.1080/26939169.2021.1946450 

## lego.population.csv retrieved from supplemental materials at https://www.tandfonline.com/doi/full/10.1080/26939169.2021.1946450

# Data are scraped from scraped from brickset.com, except Pages
# Variables are:
# - Item_Number: Item number of the set
# - Set_Name: Name of the set
# - Theme: Theme of the set (e.g., City, Duplo, Friends)
# - Pieces: Number of pieces in the set
# - Price: LEGO recommended price of the set
# - Amazon_Price: Amazon price of the set
# - Year: The year the set was produced
# - Ages: LEGO set age recommendation
# - Pages: Number of pages in the set instructions manual (scraped from lego.brickinstructions.com)
# - Minifigures: Number of minifigures (LEGO people) in the set.  LEGO sets with no
#               minifigures have been coded as NA. NA’s also represent missing data.
#               This is due to how brickset.com reports their data.
# - Packaging: Type of packaging (Blister pack, Box, Box with handle, Foil pack, Plastic
#             box, Polybag, Shrink-wrapped, Other)
# - Weight: Weight of the LEGO set in kg and lbs

library(dplyr)
library(tidyr)

legos_raw <- read.csv("~/lego.population.csv", fileEncoding = "windows-1252")

# Remove extended ascii characters like ® from theme column
legos <- mutate(legos_raw, Theme = iconv(Theme, from="windows-1252", to="ascii", sub=""))

# Remove leading $ from prices
legos <- mutate(legos, 
                across(c(Price, Amazon_Price), ~as.numeric(sub("$", "", .x, fixed = TRUE))),
                )
## Removes Ages_ prefix from Age column
legos <- mutate(legos, Ages = sub("^Ages_", "", Ages))

## remove weight in pounds, and convert kilograms to grams
legos <- mutate(legos, Weight = 1000 * as.numeric(sub("Kg.*$", "", Weight)))

## Assume missing minifigures value means 0
legos <- mutate(legos, Minifigures = replace(Minifigures, is.na(Minifigures), 0))

legos <- select(legos, Item_Number, Set_Name, Year, Theme, Ages, Pieces, Unique_Pieces,
                Price, Amazon_Price, Pages, Minifigures, Packaging, Weight, Size, Availability
                )

write.csv(legos, "legos.csv", row.names = FALSE, fileEncoding = "UTF-8")
