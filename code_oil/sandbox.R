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

# Pourquoi certaines asset ont des dates d'award/discovery dans le futur ?
# > Ce que Rystad prédit, Léo les utilise pas, ça semble tiré du chapeau.
# Pourquoi asset_time_independent donne seulement des catégories de réserves plutôt que des chiffres ? Est-ce qu'on peut reconstituer les chiffres à partir de (la date la plus récente de) reserves_year ?
# > Oui.
# Quel logiciel tu utilises ?
# > R
# As-tu déjà un code qui calcule les réserves par pays ?
# > Prendre reserve, merge avec time_independent, summarize sum et tu l'as