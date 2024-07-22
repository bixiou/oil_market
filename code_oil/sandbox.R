##### Data loading #####
DF <- read.csv("../data/Rystad/DF_Year_Cleaned.csv") # production, cost, discovery by asset. Built by Léo Jean. 
# Crude Oil can be disaggregated by oil type (Bitumen, Synthetic, Extra Heavy, Heavy, Regular, Light, Sour)
reserves_year <- read.csv("../data/Rystad/Reserves_year.csv") # reserve by asset for each year
asset <- read.csv("../data/Rystad/Asset_Time_independent.csv") # reserve by asset (with categories instead of figures for reserve size)
companies <- read.csv("../data/Rystad/Company_Info.csv") # few info, e.g. only reserve category (not figure)
history <- read.csv2("../data/Rystad/Historical_participations.csv") # historical ownership by asset (in % for each company)


View(DF)
View(reserves)
View(asset)
View(companies)
View(history)
