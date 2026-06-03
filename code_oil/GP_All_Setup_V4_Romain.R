##### Setup - Packages #####
ptm_Global <- proc.time()

my_packages <- list("ggrepel","shades","parallel","haven","plyr","dplyr","tidyr","data.table","QuantPsyc","MASS","plotly","sf","olsrr","stats","patchwork","xtable","FinCal","zoo","lubridate","stringr","scales","Hmisc","gdata","gridExtra","rJava","glmulti","geosphere","knitr","stargazer","huxtable","slam","purrr","Matrix","rio","gurobi")
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]    # Extract not installed packages
if(length(not_installed)) install.packages(unlist(not_installed))               

library(shades)
library(ggrepel)
library(haven)
library(plyr)
library(knitr)
library(stringr)
library(tidyverse)
library(data.table)
#library(geosphere)
#library(stargazer)
#library(scales)
#library(splitstackshape)
library(Hmisc)
library(gdata)
library(gridExtra)
library(ggpubr)
#library(rJava)
#library(glmulti)
library(QuantPsyc)
#library(olsrr)
library(xtable)
library(FinCal)
library(zoo)
library(lubridate)
library(stats)
library(patchwork)
library(dplyr)
library(tidyr)
library(MASS)
library(plotly)
#library(sf)
#library(plm)
library(parallel)
#library(miceadds)
#library(fuzzyjoin)
#library(scatterplot3d)
#library(scales)
library(purrr)
library(Matrix)
library(slam)
library(rio)
library(gurobi)

rm(not_installed,my_packages)

Crude_Oil_Definition <- c("Bitumen","Synthetic crude","Extra Heavy Oil","Heavy Oil 15-19","Heavy Oil 20-22","Sour (> 3%)","Regular","Light")
Oil_Definition <- c("Bitumen","Synthetic crude","Extra Heavy Oil","Heavy Oil 15-19","Heavy Oil 20-22","Sour (> 3%)","Regular","Light","NGL","Condensate")
Gas_Definition <- c("Gas","Gas in Pipelines","GTL")

##### Input - Data import #####
# df_year_full <- fread("Data/df_year_clean.csv")
# df_year_full[is.na(df_year_full)] <- 0
# df_year_full <- df_year_full[rowSums(df_year_full[,16:41]!=0)>0]
# df_year_full[,Startup := min(Year[Prod_Tot > 0]),by=`RE ID`][,`Start-up Year` := ifelse(!is.na(Startup) & Startup != `Start-up Year`,Startup,`Start-up Year`)]

prod <- fread("Data/Rystad_16_05_2025/Production.csv",encoding="Latin-1")
prod <- prod[`Oil and Gas Detail`!="Sum"][grepl("ther liquids|Refinery gains",Asset)==F]
prod[,`Production (Million bbl)` := as.numeric(gsub(",",".",`Production (Million bbl)`))]
prod[,`Production (Million bbl)` := round(`Production (Million bbl)`,6)]
prod <- prod[`Production (Million bbl)`!=0]
Selling_Prices_Prod <- prod[,c("RE ID","Year","Oil and Gas Detail","Production (Million bbl)")]
prod[,Crude_Oil_Mbbl := sum(`Production (Million bbl)`[`Oil and Gas Detail` %in% Crude_Oil_Definition]),by=.(`RE ID`,Year)]
prod[,Condensate_Mbbl := sum(`Production (Million bbl)`[`Oil and Gas Detail` %in% "Condensate"]),by=.(`RE ID`,Year)]
prod[,NGL_Mbbl := sum(`Production (Million bbl)`[`Oil and Gas Detail` %in% "NGL"]),by=.(`RE ID`,Year)]
prod[,Prod_Oil := Crude_Oil_Mbbl + Condensate_Mbbl + NGL_Mbbl]
prod[,Gas_Mboe := sum(`Production (Million bbl)`[`Oil and Gas Detail` %in% c("Gas","Gas in Pipelines")]),by=.(`RE ID`,Year)]
prod[,GTL_Mbbl := sum(`Production (Million bbl)`[`Oil and Gas Detail` %in% "GTL"]),by=.(`RE ID`,Year)]
prod[,Prod_Gas := Gas_Mboe + GTL_Mbbl]
prod[,Prod_Other := sum(`Production (Million bbl)`[!(`Oil and Gas Detail` %in% c(Gas_Definition,Oil_Definition))]),by=.(`RE ID`,Year)]
prod[,Prod_Tot := Prod_Oil + Prod_Gas + Prod_Other]
prod <- prod[!duplicated(paste(`RE ID`,Year))]

eco <- rbind(fread("Data/Rystad_16_05_2025/Eco_North_America.csv",encoding = "Latin-1"),
             fread("Data/Rystad_16_05_2025/Eco_Rest.csv",encoding = "Latin-1"))
for (j in names(eco)[!(names(eco) %in% c("RE ID","Asset","Oil and Gas Detail"))]) set(eco, j = j, value = round(as.numeric(gsub(",",".",eco[[j]])),6))
eco[is.na(eco)] <- 0
names(eco)[names(eco)=="Sum"] <- "Gross Revenues"
eco <- eco[substr(`RE ID`,3,3)!="X"]
Selling_Prices <- eco[,c("RE ID","Year","Oil and Gas Detail","Gross Revenues")]
eco <- eco[,-c("Asset","Oil and Gas Detail")][,lapply(.SD,sum),by=.(`RE ID`,Year)]
#eco[,Other_costs := `Abandonment cost` + `Exploration Well Capex` + ]
eco <- rename(eco,Abandonment_cost = `Abandonment cost`,
              Free_cash_flow = `Free Cash Flow`,
              Royalty_effects = `Royalty effects`,
              Income_tax = `Income Tax`,
              Well_capex = `Well Capex`,
              Facility_capex = `Facility Capex`,
              Exploration_well_capex = `Exploration Well Capex`,
              GG_seismic_capex = `G&G and Seismic capex`,
              SGA_opex = `SG&A Opex`,
              Transportation_opex = `Transportation Opex`,
              Production_opex =  `Production Opex`,
              Gross_revenues = `Gross Revenues`,
              Government_profit = `Government Profit Oil`,
              Bonuses = `Lease Capex`,
              Taxes_opex = `Taxes Opex`)
eco <- eco[!(Abandonment_cost==0 & Free_cash_flow==0 & Royalty_effects==0 & Income_tax==0 & Well_capex==0 & Facility_capex==0 & Exploration_well_capex==0 & GG_seismic_capex==0 & SGA_opex==0 & Transportation_opex==0 & Production_opex==0 & Gross_revenues==0 & Government_profit==0 & Bonuses==0 & Taxes_opex==0)]
eco[,Exploration_capex := Exploration_well_capex + GG_seismic_capex]

asset_lvl <- fread("Data/Rystad_16_05_2025/Asset_Info.csv",encoding="Latin-1")[!duplicated(`RE ID`)]
asset_lvl[,Offshore := `On-Offshore`=="Offshore"]
asset_lvl[,Continent := case_when(
  grepl("Africa",Region) ~ "Africa",
  grepl("North America|Arctic Ocean",Region) ~ "North America",
  grepl("South America|Central America|Caribbean",Region) ~ "South America",
  grepl("Europe",Region) ~ "Europe",
  grepl("Australasia|Melanesia",Region) ~ "Australia",
  grepl("Asia|Bohai",Region) ~ "Asia",
  grepl("Middle East",Region) ~ "Middle East",
  grepl("Russia",Region) ~ "Russia",
)]

df_year_full <- merge(merge(eco,
                             prod[,-c("Asset","Oil and Gas Detail","Data Type","Data Confidence","Data Source","Production (Million bbl)")],by=c("RE ID","Year"),all=T),
                       asset_lvl[,c("RE ID","Asset","Country","Province","Area","Location","Project","Fiscal Regime Group","Tax Model Name","Award Year","Discovery Year","Approval Year","Start-up Year","Offshore")],
                       by = "RE ID",all.x=T)
df_year_full[is.na(`Discovery Year`),`Discovery Year` := pmin(`Start-up Year`,`Approval Year`,2100,na.rm=T)][is.na(`Approval Year`),`Approval Year` := pmin(`Start-up Year`,2100,na.rm=T)][is.na(`Start-up Year`),`Start-up Year` := 2100]
df_year_full[is.na(df_year_full)] <- 0
# df_year_oil <- fread("Data/df_year_clean_oil_cost_only.csv")
# df_year_oil[is.na(df_year_oil)] <- 0
# df_year_oil <- df_year_oil[rowSums(df_year_oil[,16:41]!=0)>0]
# df_year_oil[,Startup := min(Year[Prod_Tot > 0]),by=`RE ID`][,`Start-up Year` := ifelse(!is.na(Startup) & Startup != `Start-up Year`,Startup,`Start-up Year`)][,Startup := NULL]

# Outages data
outages <- fread("Data/Rystad_16_05_2025/Outages.csv",encoding = "Latin-1")
outages <- outages[!(`Outage Detail` %in% c("Production","Outage Disregarded","Sum"))][`Outage Detail` %in% c("NGL","Regular","Condensate"),`Outage Detail` := "Other Unplanned"]
#outages <- dcast(outages,id.vars = c("RE ID","Asset","Year","Outage Detail"),measure.vars = "Oil and Gas Detail")
outages[`RE ID`=="LYF0506" & Year==2016 & `Oil and Gas Detail`=="Gas" & `Outage Detail`!="Production",`Production (Million bbl)` := `Production (Million bbl)`/1e4] 
outages <- rbind(outages,
                 Selling_Prices_Prod[Year %in% unique(outages$Year)][,c("Asset","Outage Detail") := list("","Production")][,names(outages),with=F])
# outages[,Regulatory_Outage_2017_2019 := sum(`Production (Million bbl)`[`Outage Detail`=="Regulatory Order" & Year %in% 2017:2019 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2017:2019 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_Outage_2017_2019),Regulatory_Outage_2017_2019:=0]
# outages[,Regulatory_OutageExt_2017_2019 := sum(`Production (Million bbl)`[`Outage Detail`!="Production" & Year %in% 2017:2019 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2017:2019 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_OutageExt_2017_2019),Regulatory_OutageExt_2017_2019:=0]
outages[,Regulatory_Outage_2019 := sum(`Production (Million bbl)`[`Outage Detail`=="Regulatory Order" & Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_Outage_2019),Regulatory_Outage_2019:=0]
outages[,Regulatory_OutageExt_2019 := sum(`Production (Million bbl)`[`Outage Detail`!="Production" & Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_OutageExt_2019),Regulatory_OutageExt_2019:=0]
outages[,Regulatory_OutageInvoluntary_2019 := sum(`Production (Million bbl)`[!(`Outage Detail` %in% c("Production","Regulatory Order")) & Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_OutageInvoluntary_2019),Regulatory_OutageInvoluntary_2019:=0]
# outages[,Regulatory_Outage_2023_2024 := sum(`Production (Million bbl)`[`Outage Detail`=="Regulatory Order" & Year %in% 2023:2024 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2023:2024 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_Outage_2023_2024),Regulatory_Outage_2023_2024:=0]
# outages[,Regulatory_OutageExt_2023_2024 := sum(`Production (Million bbl)`[`Outage Detail`!="Production" & Year %in% 2023:2024 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2023:2024 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_OutageExt_2023_2024),Regulatory_OutageExt_2023_2024:=0]
# outages[,Regulatory_Outage_2024 := sum(`Production (Million bbl)`[`Outage Detail`=="Regulatory Order" & Year %in% 2024 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2024 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_Outage_2024),Regulatory_Outage_2024:=0]
# outages[,Regulatory_OutageExt_2024 := sum(`Production (Million bbl)`[`Outage Detail`!="Production" & Year %in% 2024 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2024 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_OutageExt_2024),Regulatory_OutageExt_2024:=0]
# outages[,Regulatory_Outage_2022_2024 := sum(`Production (Million bbl)`[`Outage Detail`=="Regulatory Order" & Year %in% 2022:2024 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2022:2024 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_Outage_2022_2024),Regulatory_Outage_2022_2024:=0]
# outages[,Regulatory_OutageExt_2022_2024 := sum(`Production (Million bbl)`[`Outage Detail`!="Production" & Year %in% 2022:2024 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2022:2024 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_OutageExt_2022_2024),Regulatory_OutageExt_2022_2024:=0]
# outages[,Regulatory_Outage_2020_2024 := sum(`Production (Million bbl)`[`Outage Detail`=="Regulatory Order" & Year %in% 2020:2024 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2020:2024 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_Outage_2020_2024),Regulatory_Outage_2020_2024:=0]
# outages[,Regulatory_OutageExt_2020_2024 := sum(`Production (Million bbl)`[`Outage Detail`!="Production" & Year %in% 2020:2024 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2020:2024 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_OutageExt_2020_2024),Regulatory_OutageExt_2020_2024:=0]
outages[,Regulatory_Outage_2015_2024 := sum(`Production (Million bbl)`[`Outage Detail`=="Regulatory Order" & Year %in% 2015:2024 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2015:2024 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_Outage_2015_2024),Regulatory_Outage_2015_2024:=0]
outages[,Regulatory_OutageExt_2015_2024 := sum(`Production (Million bbl)`[`Outage Detail`!="Production" & Year %in% 2015:2024 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2015:2024 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_OutageExt_2015_2024),Regulatory_OutageExt_2015_2024:=0]
# outages[,Prod_Cap_2017_2019 := sum(`Production (Million bbl)`[Year %in% 2017:2019]),by=`RE ID`]
# outages[,Prod_Cap_2023_2024 := sum(`Production (Million bbl)`[Year %in% 2023:2024]),by=`RE ID`]
outages[,Real_Prod_Cap := sum(`Production (Million bbl)`[`Oil and Gas Detail` %in% Oil_Definition],na.rm=T),by = .(`RE ID`,Year)]
outages[,Real_Prod_Cap_2019 := sum(`Production (Million bbl)`[`Oil and Gas Detail` %in% Oil_Definition & Year==2019],na.rm=T),by = .(`RE ID`)]

outages <- merge(outages,asset_lvl[,c("RE ID","OPEC NON-OPEC","OPEC Plus","Country")],by="RE ID")
outages[`OPEC Plus`=="Non-OPEC+" | grepl("VE|LY|IR|NS|SD",`RE ID`),Regulatory_OutageBaselineA_2019 := sum(`Production (Million bbl)`[!(`Outage Detail` %in% c("Production")) & Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_OutageBaselineA_2019),Regulatory_OutageBaselineA_2019:=0]
outages[!(`OPEC Plus`=="Non-OPEC+" | grepl("VE|LY|IR|NS|SD",`RE ID`)),Regulatory_OutageBaselineA_2019 := 0]
outages[,Regulatory_OutageBaselineB_2019 := Regulatory_OutageBaselineA_2019]
outages[!(`OPEC Plus`=="Non-OPEC+" | grepl("VE|LY|IR|NS|SD",`RE ID`)),Regulatory_OutageBaselineB_2019 := sum(`Production (Million bbl)`[!(`Outage Detail` %in% c("Production","Regulatory Order")) & Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_OutageBaselineB_2019),Regulatory_OutageBaselineB_2019:=0]

outages[!(`OPEC Plus`=="Non-OPEC+" | grepl("VE|LY|IR|NS|SD",`RE ID`)),Regulatory_OutageStrategic_2019 := sum(`Production (Million bbl)`[`Outage Detail`!="Production" & Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_OutageStrategic_2019),Regulatory_OutageStrategic_2019:=0]
outages[`OPEC Plus`=="Non-OPEC+" | grepl("VE|LY|IR|NS|SD",`RE ID`),Regulatory_OutageStrategic_2019 := 0]

outages[,Regulatory_Outage_2019Corrected := sum(`Production (Million bbl)`[`Outage Detail`=="Regulatory Order" & Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_Outage_2019Corrected),Regulatory_Outage_2019Corrected:=0]
outages[,Regulatory_OutageExt_2019Corrected := sum(`Production (Million bbl)`[`Outage Detail`!="Production" & Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition]) / sum(`Production (Million bbl)`[Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][is.na(Regulatory_OutageExt_2019Corrected),Regulatory_OutageExt_2019Corrected:=0]

Avg_Share_OPECCore <- outages[Country %in% c("Saudi Arabia","Kuwait","UAE") & Year==2019][!duplicated(`RE ID`)][, weighted.mean(Regulatory_OutageExt_2019Corrected,Real_Prod_Cap)]
outages[Country %in% c("Lybia","Venezuela","Iran","Neutral Zone","South Sudan"), 
        Regulatory_OutageExt_2019Corrected := Regulatory_OutageExt_2019Corrected * Avg_Share_OPECCore / weighted.mean(Regulatory_OutageExt_2019Corrected[!duplicated(`RE ID`)],Real_Prod_Cap_2019[!duplicated(`RE ID`)]),by=Country]

# temp <- outages[Year==2019][!duplicated(`RE ID`)] %>% group_by(Country) %>% summarise(OPEC = unique(`OPEC Plus`),Prod_Cap = round(sum(Real_Prod_Cap)/365,3),
#                                                                                    AvgExt = 100*weighted.mean(Regulatory_OutageExt_2019Corrected,Real_Prod_Cap),
#                                                                                    Avg = 100*weighted.mean(Regulatory_Outage_2019Corrected,Real_Prod_Cap))

for (j in names(outages)[grepl("Regulatory_Outage",names(outages))]) set(outages, j = j, value = pmin(1,pmax(outages[[j]],0)))

outages[`Outage Detail`=="Production" & grepl("Feed",`Oil and Gas Detail`)==F & `Production (Million bbl)` <0, `Production (Million bbl)` := 0]

#outages <- fread("Data/Rystad_16_05_2025/Outages_Monthly.csv",encoding = "Latin-1")

# Deflator cost data
deflator <- fread("Data/Rystad_16_05_2025/Deflator.csv")
deflator <- deflator[,Deflate_factor := `Economics (MUSD real)` / `Economics (MUSD)` ][,c("Year","Deflate_factor")]





ownership <- fread("Data/Rystad_16_05_2025/Ownership.csv",encoding="Latin-1")
ownership <- rename(ownership,value = `Historical Participation`)[`Historical Company`!="Sum"][value > 0 & `Consolidated Equity Affiliates` == "Consolidated"]
ownership <- ownership[,c("RE ID","Year","Historical Company","value")]
ownership <- ownership[Year >= 2000 & !is.na(value)]
#ownership <- setDT(ownership)[CJ(`RE ID` = `RE ID`, Year = c(1992:2100), unique=T),on=.(`RE ID`,Year)]
ownership <- ownership[order(`RE ID`,Year,`Historical Company`)]
ownership[,Last_Year := max(Year),by=`RE ID`][,First_Year := min(Year),by=`RE ID`]

for(year in 2000:max(ownership$Year)){
  ownership <- rbind(ownership,
                     ownership[First_Year > year][Year==First_Year][,Year := year],
                     ownership[Last_Year < year][Year==Last_Year][,Year := year])
}
for(year in (max(ownership$Year)+1):2100){
  ownership <- rbind(ownership,ownership[Year==2026][,Year:=year])
}


Company_Info <- fread("Data/Rystad_16_05_2025/Company_Info.csv")

# ownership <- merge(ownership,
#                    fread("Data/Company_Info.csv"),by.x = "Historical Company",by.y = "Company", all.x = T)




CI <- fread("Output/CI/CI_Full.csv")[!is.na(CI_Final_EBA)]
CI_AR6_20y <- copy(CI)
CI_AR6_20y <- CI_AR6_20y[,c("CI_Final_EBA","CI_midstream","CI_downstream") := list(CI_Final_EBA_20y,CI_midstream_AR6_20y,CI_downstream_AR6_20y)]
#CI_AR4_20y <- CI[,c("CI_Final_EBA","CI_midstream","CI_downstream") := list(CI_Final_EBA_20y,CI_midstream_AR6_20y,CI_downstream_AR6_20y)]

#plot(density(log(CI$FOR+1),weights = CI$Res_Oil_1900/sum(CI$Res_Oil_1900)),xlim=c(0,log(10200)))

reserves <- rbind(fread("Data/Rystad_16_05_2025/Reserves_North_America.csv",encoding="Latin-1")[Year %in% c(1900,1970,2000:2080)],
                  fread("Data/Rystad_16_05_2025/Reserves_Rest.csv",encoding="Latin-1")[Year %in% c(1900,1970,2000:2080)])

for (j in c("Year","Technical recoverable resources (Million bbl)","Resources (Million bbl)","1P Reserves (Million bbl)","2P Reserves (Million bbl)","Discovered Resources (Million bbl)")) set(reserves, j = j, value = round(as.numeric(gsub(",",".",reserves[[j]])),6))
reserves <- reserves[!is.na(Year)]
reserves[is.na(reserves)] <- 0
set(reserves,"Oil and Gas Detail",i=which(reserves$`Oil and Gas Detail` %in% Oil_Definition),value="Oil")
set(reserves,"Oil and Gas Detail",i=which(reserves$`Oil and Gas Detail` %in% c("Gas","LNG","Feed gas to LNG","Feed gas to Pipeline","Gas in Pipelines","GTL")),value="Gas")
reserves <- reserves[,c("Oil_Gas","Resources","Reserves_1P","Reserves_2P") := list(`Oil and Gas Detail`,`Resources (Million bbl)`,`1P Reserves (Million bbl)`,`2P Reserves (Million bbl)`),][,c("RE ID","Year","Oil_Gas","Resources","Reserves_1P","Reserves_2P")]
reserves <- reserves[,lapply(.SD,sum),by=.(`RE ID`,Year,Oil_Gas)]
reserves <- dcast(reserves,`RE ID` + Year ~ Oil_Gas,value.var = c("Resources","Reserves_1P","Reserves_2P"))
reserves[is.na(reserves)] <- 0

prices <- fread("Data/Rystad_16_05_2025/Prices.csv")

Selling_Prices <- merge(Selling_Prices,Selling_Prices_Prod,by = c("RE ID","Year","Oil and Gas Detail"),all = T)
Selling_Prices[is.na(Selling_Prices)] <- 0
Selling_Prices <- Selling_Prices[`RE ID` %in% asset_lvl$`RE ID`][`Oil and Gas Detail` %in% Oil_Definition]
Selling_Prices[,`Price (USD/bbl)` := `Gross Revenues` / `Production (Million bbl)`]
Selling_Prices <- Selling_Prices[`Production (Million bbl)` >= 1e-5 & `Gross Revenues` >= 1e-5]
Selling_Prices <- merge(Selling_Prices,prices[,c("Year","Brent oil price (USD/bbl)","WTI Cushing oil price (USD/bbl)")],by="Year")
Selling_Prices[,Discount_Price := weighted.mean(`Price (USD/bbl)`,`Production (Million bbl)`) / `Brent oil price (USD/bbl)`,by=.(`RE ID`,Year)]
Selling_Prices <- Selling_Prices[,c("RE ID","Year","Oil and Gas Detail","Price (USD/bbl)","Brent oil price (USD/bbl)","WTI Cushing oil price (USD/bbl)")]

Selling_Prices <- merge(Selling_Prices,deflator,by="Year")
Selling_Prices[,Brent_Price_Deflated := `Brent oil price (USD/bbl)` * Deflate_factor]


# df_year_oil[, Decline_Rate := max(ifelse(shift(cumsum(Prod_Oil),fill=0)/sum(Prod_Oil) <= 0.8, 
#                                          Prod_Oil / (sum(Prod_Oil) - shift(cumsum(Prod_Oil),fill=0)),
#                                          0)), by = "RE ID"][,Decline_Rate := unique(Decline_Rate[!is.na(Decline_Rate)]),by="RE ID"][Decline_Rate==0,Decline_Rate := 1]
df_year_full[, Decline_Rate := max(ifelse(shift(cumsum(Prod_Oil),fill=0)/sum(Prod_Oil) <= 0.8, 
                                          Prod_Oil / (sum(Prod_Oil) - shift(cumsum(Prod_Oil),fill=0)),
                                          0)), by = "RE ID"][,Decline_Rate := unique(Decline_Rate[!is.na(Decline_Rate)]),by="RE ID"][Decline_Rate==0,Decline_Rate := 1]

# Country_Share_World_Demand <- fread("Data/Statistical Review of World Energy Narrow File.csv")
# Country_Share_World_Demand <- Country_Share_World_Demand[Year==2022 & Var=="oilcons_kbd"]
# Country_Share_World_Demand <- Country_Share_World_Demand[,World_Demand := Value[Country=="Total World"]][!is.na(ISO3166_numeric)]
# #Country_Share_World_Demand <- merge(Country_Share_World_Demand[,Match := substr(ISO3166_alpha3,1,2)],
# #                                    asset_lvl[!duplicated(Country),Match := substr(`RE ID`,1,2)][,c("Match","Country")],by="Match",all.x=T)
# Country_Share_World_Demand[grepl("China",Country),Value := sum(Value)]
# Country_Share_World_Demand[Country=="Russian Federation",Country := "Russia"][Country=="Trinidad & Tobago",Country := "Trinidad and Tobago"][Country=="Turkiye",Country := "Turkey"][Country=="United Arab Emirates",Country := "UAE"][Country=="US", Country := "United States"]


Country_Share_World_Demand <- fread("Data/INT-Export-01-24-2025_17-43-29.csv") #https://www.eia.gov/international/data/world/petroleum-and-other-liquids/annual-refined-petroleum-products-consumption?pd=5&p=0000001&u=0&f=A&v=mapbubble&a=-&i=none&vo=value&t=C&g=00000000000000000000000000000000000000000000000001&l=249-ruvvvvvfvtvnvv1urvvvvfvvvvvvfvvvou20evvvvvvvvvnvvvvs&s=94694400000&e=1672531200000&
Country_Share_World_Demand <- data.table(Country = as.character(unlist(Country_Share_World_Demand[3:nrow(Country_Share_World_Demand),2])),
                                         Demand = as.numeric(unlist(Country_Share_World_Demand[3:nrow(Country_Share_World_Demand),52])))
Country_Share_World_Demand <- Country_Share_World_Demand[!is.na(Demand)]
Country_Share_World_Demand[,Country := str_trim(Country)]
Country_Share_World_Demand[Country=="Congo-Kinshasa",Country := "Democratic Republic of Congo"][Country=="Congo-Brazzaville",Country := "Congo"][Country=="Czechia",Country := "Czech Republic"][Country=="Gambia, The",Country := "Gambia"][Country=="Burma",Country := "Myanmar"][Country=="Turkiye",Country := "Turkey"][Country=="United Arab Emirates",Country := "UAE"]

Country_Share_World_Demand <- Country_Share_World_Demand[,Country_Share_Demand := Demand / Demand[Country=="World"]][,c("Country","Country_Share_Demand")]

# 
# outages <- fread("Data/Rystad_2025/Outages_Complete.csv",encoding="Latin-1")
# outages <- melt(outages,id.vars = c("RE ID","Asset","Year","Outage Detail"),variable.name = "Oil and Gas Category")
# outages[`RE ID`=="LYF0506" & Year==2016 & `Oil and Gas Category`=="Gas" & `Outage Detail`!="Production",value := value/1e4] 
# outages <- outages[`Outage Detail`!="Outage Disregarded"]

# outages[,Regulatory_Outage_2017_2019 := sum(value[`Outage Detail`=="Regulatory Order" & Year %in% 2017:2019 & `Oil and Gas Category` %in% c("Crude Oil","NGL","Condensate")]) / sum(value[Year %in% 2017:2019 & `Oil and Gas Category` %in% c("Crude Oil","NGL","Condensate")]),by=`RE ID`][is.na(Regulatory_Outage_2017_2019),Regulatory_Outage_2017_2019:=0]
# outages[,Regulatory_Outage_2023_2024 := sum(value[`Outage Detail`=="Regulatory Order" & Year %in% 2023:2024 & `Oil and Gas Category` %in% c("Crude Oil","NGL","Condensate")]) / sum(value[Year %in% 2023:2024 & `Oil and Gas Category` %in% c("Crude Oil","NGL","Condensate")]),by=`RE ID`][is.na(Regulatory_Outage_2023_2024),Regulatory_Outage_2023_2024:=0]
# outages[,Prod_Cap_2017_2019 := sum(value[Year %in% 2017:2019]),by=`RE ID`]
# outages[,Prod_Cap_2023_2024 := sum(value[Year %in% 2023:2024]),by=`RE ID`]
# outages[,Real_Prod_Cap := sum(value[`Oil and Gas Category` %in% c("Crude Oil","NGL","Condensate")],na.rm=T),by = .(`RE ID`,Year)]

# Average outage (by outage detail):
# 2023-2024 (rest polluted by COVID)
# or 2017-2019
# Only regulatory order
# ==> 2 world config
# What to do: 1. change Plateau prod (taking into account REAL max, including outages)
# 2. Have for each asset prod_cap reduction in percentage
# 3. Test with relaxing prod cap in 2027 / 2032 / 2040
# 4. Check revenues for all countries with >10% of capacities (OPEC and OPEC + others)
# ==> for regulatory: UAE, Algeria, Saudi Arabia, Iraq, Kuwait, Oman / Russia

# temp <- df_year_full
# DT_Opti <- merge(DT_Opti,asset_lvl[,c("RE ID","Country")], by.x="ID_Unit",by.y="RE ID")
# DT_Opti <- rbind(DT_Opti,DT_Opti[Country=="Neutral Zone"])
# DT_Opti[Country=="Neutral Zone",Prod_Cap_Initial := .5 * Prod_Cap_Initial]
# DT_Opti[duplicated(ID_Unit),Country := "Kuwait"][Country=="Neutral Zone",Country := "Saudi Arabia"]
# outages <- merge(outages[,CN := substr(`RE ID`,1,2)],temp,by=c("CN","Year"),all.x=T)
# outages[,CN := substr(`RE ID`,1,2)]

temp <- as.data.table(outages[Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition][,CN := substr(`RE ID`,1,2)] %>% group_by(`Outage Detail`,CN) %>% summarise(Prod = round(sum(`Production (Million bbl)`,na.rm=T)/((max(Year)-min(Year)+1)),3)))
#temp <- rbind(temp,rename(prod[Year %in% 2017:2019,c("RE ID","Prod_Oil")][,CN := substr(`RE ID`,1,2)][,c("CN","Prod_Oil")][,lapply(.SD,sum),by="CN"],Prod = Prod_Oil)[,`Outage Detail`:="Production"][,c("CN","Outage Detail","Prod")])
temp[,Prod := Prod/365]
temp[,Share := round(100 * Prod / sum(Prod),3),by=.(CN)]
#temp[,`Outage Detail` := ifelse(!(`Outage Detail` %in% c("Regulatory Order","Production")),"Involuntary",`Outage Detail`)]
#temp <- temp[CN %in% temp[`Outage Detail`=="Production"][Prod>=0.1,CN]]
temp[,OPEC := ifelse(CN %in% asset_lvl[`OPEC NON-OPEC`=="OPEC",unique(substr(`RE ID`,1,2))],"OPEC","Non-OPEC")]
temp[,OPECPlus := ifelse(CN %in% asset_lvl[`OPEC Plus`=="OPEC+",unique(substr(`RE ID`,1,2))],"OPEC+","Non-OPEC+")]
temp[OPEC=="OPEC",OPECPlus := "OPEC"]
temp[grepl("IR|NS|VE|LY",CN),OPECPlus := "Excluded"]
temp[CN=="AE"]
#View(temp[`Outage Detail`=="Regulatory Order" ])

# temp2 <- copy(temp)[,Prod := ifelse(`Outage Detail`=="Regulatory Order",sum(Prod[`Outage Detail`!="Production"]),Prod),by="CN"][`Outage Detail` %in% c("Production","Regulatory Order")]
# temp2[,Share := round(100 * Prod / sum(Prod),3),by=CN]
#View(temp2[`Outage Detail`=="Regulatory Order" & OPEC==T])
# temp <- fread("Output/GP_All/Results/Asset_UMD_Baseline_V2/Misc/DT_Opti.csv")
# outages <- merge(outages,temp[!duplicated(ID_Unit),c("ID_Unit","Extrac_cost")],by.x="RE ID",by.y="ID_Unit",all.x=T)
# 
# temp <- outages[Year==2024 & Country=="Saudi Arabia" & !is.na(Extrac_cost)]
# wtd.quantile(temp[!is.na(Extrac_cost),Extrac_cost],temp[!is.na(Extrac_cost),as.numeric(`Outage Detail`=="Production") * value],seq(0,1,0.05))
# wtd.quantile(temp[!is.na(Extrac_cost),Extrac_cost],temp[!is.na(Extrac_cost),as.numeric(`Outage Detail`=="Regulatory Order") * value],seq(0,1,0.05))
# temp[,Share_Cap := sum(value[`Outage Detail`=="Regulatory Order"])/sum(value),by=`RE ID`]
# plot(temp[,Extrac_cost],temp[,Share_Cap],xlim = c(0,min(60,max(temp$Extrac_cost))))

# Outages RO by extraction cost
# Only for assets in which it is easy

# temp <- merge(outages[,Prod_Cap_2019 := sum(`Production (Million bbl)`[Year %in% 2019 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][!duplicated(`RE ID`)][Country %in% c("Saudi Arabia","Kuwait","UAE"),c("RE ID","Prod_Cap_2019","Regulatory_Outage_2019","Regulatory_OutageExt_2019")],
#               fread("Output/GP_All/Results/Asset_UMD_CHR/Misc/DT_Opti.csv")[!duplicated(ID_Unit),c("ID_Unit","Extrac_cost")],
#               by.x="RE ID",by.y="ID_Unit")[Prod_Cap_2019 > 0]
# 
# ggplot(data=temp,aes(x = Extrac_cost,y = log(Prod_Cap_2019+1))) + geom_point(aes(col = 100*Regulatory_Outage_2019)) + scale_color_gradient(low="lightblue", high="red") + 
#   theme(legend.position = c(0.87, 0.8) ) + labs(col = "Outage (%)",y = "Log Production Capacity",x = "Extraction cost ($)") + xlim(min(temp$Extrac_cost),min(50,max(temp$Extrac_cost)))
# 
# ggplot(data=temp,aes(x = Extrac_cost,y = log(Prod_Cap_2019+1))) + geom_point(aes(col = 100*Regulatory_OutageExt_2019)) + scale_color_gradient(low="lightblue", high="red") + 
#   theme(legend.position = c(0.84, 0.8) ) + labs(col = "Outage All (%)",y = "Log Production Capacity",x = "Extraction cost ($)")  + xlim(min(temp$Extrac_cost),min(50,max(temp$Extrac_cost)))
# 
# temp <- merge(outages[,Prod_Cap_2019 := sum(`Production (Million bbl)`[Year %in% 2015:2024 & `Oil and Gas Detail` %in% Oil_Definition]),by=`RE ID`][!duplicated(`RE ID`)][Country %in% c("Saudi Arabia","Kuwait","UAE"),c("RE ID","Prod_Cap_2019","Regulatory_Outage_2015_2024","Regulatory_OutageExt_2015_2024")],
#               fread("Output/GP_All/Results/Asset_UMD_CHR/Misc/DT_Opti.csv")[!duplicated(ID_Unit),c("ID_Unit","Extrac_cost")],
#               by.x="RE ID",by.y="ID_Unit")[Prod_Cap_2019 > 0]
# 
# ggplot(data=temp,aes(x = Extrac_cost,y = log(Prod_Cap_2019+1))) + geom_point(aes(col = 100*Regulatory_Outage_2015_2024)) + scale_color_gradient(low="lightblue", high="red") + 
#   theme(legend.position = c(0.87, 0.8) ) + labs(col = "Outage (%)",y = "Log Production Capacity",x = "Extraction cost ($)") + xlim(min(temp$Extrac_cost),min(50,max(temp$Extrac_cost))) 
# 
# ggplot(data=temp,aes(x = Extrac_cost,y = log(Prod_Cap_2019+1))) + geom_point(aes(col = 100*Regulatory_OutageExt_2015_2024)) + scale_color_gradient(low="lightblue", high="red") + 
#   theme(legend.position = c(0.84, 0.8) ) + labs(col = "Outage All (%)",y = "Log Production Capacity",x = "Extraction cost ($)")  + xlim(min(temp$Extrac_cost),min(50,max(temp$Extrac_cost)))

