##### Setup #####
#.rs.restartR()
rm(list = ls())
wdpath <- "C:/Users/le.jean/Dropbox/NEW_GP_ALL/Code_LeoJ"
setwd(wdpath)
#Cleaning up attached objects
for(x in search()[grepl("GlobalEnv|package|tools:rstudio|Autoloads",search())==F]) detach(x, character.only = T)

source("GP_All_Setup_V4.R")

source("GP_All_Functions_V4.R")

##### Data Selection #####
# 1. Variable name, 2. Operator for selection, 3. Values (in list if several)
data_selection <- list(list("`Discovery Year`","<=",2024),
                       list("`Start-up Year`","<=",2080), #In order to have assets with eco data (withdraw < 10 assets with not enough prod year, representing <0.003% of reserves)
                       list("GOR","<=","Inf"))
data_selection <- as.data.table(matrix(do.call(rbind, data_selection),ncol=3))
names(data_selection) <- c("Variable","Operator","Value") 

##### Unit Parameters #####
Unit_Parameters <- list(list(Version = "LB_UMD_CHR", #Name of this version of parameters
                             Unit = "License-Block", #Asset, License-Block, Project
                             Asset_Remaining_Development_Cost = "As of Year_Start_World", #"As of Year_Start_World" or "Full"
                             Year_Start_World = 2025, #Start of optimization
                             Year_End_Future = 2100, #End of optimization
                             Reserve_Definition = "Resources_Oil", #To be precised in functions if 2P (in which year ? 2025 + Prod 1992-2021 ?)
                             Reserves_Min = 1e-3, #Min amount of reserves to be included in the optimization
                             Lag_After_Discovery = 0, #Lag before development after discovery of the reserves
                             Gas_Cost = "Excluded", #Included, Excluded, Capex_Included, Opex_Included --> Share of oil among products (in boe)
                             Cost_Definition = "Average",#Average, Breakeven
                             Costs_Included = list("Facility_capex","Well_capex","SGA_opex","Production_opex","Transportation_opex"),
                             Capital_Depreciation = F,
                             Bins = F, # Are there costs bins or not ?
                             Bins_Ratio = NA, #list of ratios between costs
                             Bins_Threshold = NA, #list of thresholds from one bin to another (0->1)
                             Development_Cost = "Included", #Included, Separate
                             Limit_Development_Year = 2, #Upper bound (if 1 then only startup year)
                             Lag_After_Development = 1, #Lag before extraction after investment is made, can be equal to "Asset_Specific" --> O-10Mbbl = 1y, 10-250-Mbbl = 2y, 250-5000Mbbl = 3y, >5Gbbl = 4y
                             Production_Capacity_OPEC = "Hybrid_Outage", #Plateau, Decline, Hybrid (both), Infinite, Observed
                             Production_Capacity_Fringe = "Hybrid_Outage", #Plateau, Decline, Hybrid (both), Infinite, Observed
                             Decline_Rate_Max_Depletion = 0.8, # Depletion cutoff under which max decline rate is computed
                             CI_Lifecycle = list("Upstream","Midstream","Downstream"), #Emissions included in theta
                             CI_AR = 6,
                             CI_GWP = 100,
                             CI_Upstream_Method = "EBA",
                             CI_Upstream_Scenario = "Baseline"))

Names_Parameters <- names(Unit_Parameters[[1]])
Unit_Parameters <- as.data.table(matrix(do.call(rbind, Unit_Parameters),ncol=26))
names(Unit_Parameters) <- Names_Parameters

temp <- as.data.table(do.call(expand.grid,list(ag = c("LB","Asset","Project"),
                                               ci = c("U","UM","UMD","UMDu","D"),
                                               ci_scenar = c("CIBase","DRE_91","DRE_100","Fla_0","Fla_50","20y"),
                                               other = c("OtherNone","FutureCosts","ReducedRes10","InfRes","CapDec","CapPla","CapPla_InfRes","Depreciation"))))[,Version := paste(ag,ci,ci_scenar,"CHR",other,sep="_")]
temp[grepl("CIBase",Version),Version := gsub("CIBase_","",Version)]
temp[grepl("OtherNone",Version),Version := gsub("_OtherNone","",Version)]

Unit_Parameters <- map_dfr(seq_len(nrow(temp)), ~Unit_Parameters)

Unit_Parameters$Version <- temp$Version
Unit_Parameters <- merge(Unit_Parameters,temp,by="Version")

Unit_Parameters[ag!="LB",Unit := as.character(unlist(ag))]
Unit_Parameters[,c("CI_Lifecycle","CI_GWP","CI_Upstream_Scenario") := list(
  ifelse(ci == "U",list(list("Upstream")),ifelse(ci == "UM",list(list("Upstream","Midstream")),ifelse(ci == "UMD",list(list("Upstream","Midstream","Downstream")),ifelse(ci == "UMDu",list(list("Upstream","Midstream","DownstreamUniform")),list(list("Downstream")))))),
  ifelse(ci_scenar=="20y",20,100),
  ifelse(!(ci_scenar %in% c("CIBase","20y")),as.character(ci_scenar),CI_Upstream_Scenario)
)]
Unit_Parameters[other=="FutureCosts",Cost_Definition := "Average_Future"]
Unit_Parameters[other=="ReducedRes10",Reserve_Definition := "Resources_Oil_Minus10pct"]
Unit_Parameters[grepl("InfRes",other),Reserve_Definition := "Resources_Oil_Infinite"]
Unit_Parameters[other=="CapDec",c("Production_Capacity_OPEC","Production_Capacity_Fringe") := list("Decline","Decline")]
Unit_Parameters[grepl("CapPla",other),c("Production_Capacity_OPEC","Production_Capacity_Fringe") := list("Plateau","Plateau")]
Unit_Parameters[other=="Depreciation",Capital_Depreciation := T]
Unit_Parameters[,c("ag","ci","ci_scenar","other") := NULL]

Unit_Parameters <- rbind(Unit_Parameters,
                         Unit_Parameters[Version=="Asset_UMD_CHR"][,c("Version","Year_Start_World") := list("Validity_2020_Asset_CHR",2020)],
                         Unit_Parameters[Version=="Asset_UMD_Baseline"][,c("Version","Year_Start_World") := list("Validity_2020_Asset_Baseline",2020)],
                         Unit_Parameters[Version=="Asset_UMD_CHR"][,c("Version","Year_Start_World") := list("Validity_2019_Asset_CHR",2019)],
                         Unit_Parameters[Version=="Asset_UMD_Baseline"][,c("Version","Year_Start_World") := list("Validity_2019_Asset_Baseline",2019)],
                         Unit_Parameters[Version=="Asset_UMD_CHR"][,c("Version","Year_Start_World") := list("Validity_2015_Asset_CHR",2015)],
                         Unit_Parameters[Version=="Asset_UMD_Baseline"][,c("Version","Year_Start_World") := list("Validity_2015_Asset_Baseline",2015)])

Unit_Parameters <- rbind(Unit_Parameters,
                         copy(Unit_Parameters)[Version=="Asset_UMD_CHR"][,c("Version","Cost_Definition") := list("Asset_UMD_CHR_Breakeven","Breakeven")])

# Unit_Parameters <- rbind(Unit_Parameters,
#                          Unit_Parameters[Version=="Asset_UMDu_CHR"][,c("Version","Year_Start_World") := list("Validity_2020_Asset_CHR",2020)],
#                          Unit_Parameters[Version=="Asset_UMDu_Baseline"][,c("Version","Year_Start_World") := list("Validity_2020_Asset_Baseline",2020)],
#                          Unit_Parameters[Version=="Asset_UMDu_CHR"][,c("Version","Year_Start_World") := list("Validity_2015_Asset_CHR",2015)],
#                          Unit_Parameters[Version=="Asset_UMDu_Baseline"][,c("Version","Year_Start_World") := list("Validity_2015_Asset_Baseline",2015)])

#Now all Baseline (endogenous dvpt)
Unit_Parameters <- rbind(Unit_Parameters,
                         copy(Unit_Parameters)[,c("Version","Development_Cost") := list(gsub("CHR","Baseline",Version),"Separate")])
# #Now with 2100 as last year of opti
# Unit_Parameters <- rbind(Unit_Parameters,
#                          copy(Unit_Parameters)[,c("Version","Year_End_Future") := list(paste0("End2100_",Version),2100)])

#Unit_Parameters[,Version := paste0(Version,"_V2")]

rm(temp)

# Spare_Capacities_Obs <- data.table(Country = c("Saudi Arabia","UAE","Iraq","Kuwait","Algeria","Nigeria"),
#                                    `4Full` = c(1.338,1.319,1.148,1.16,1.11,1.069))[,c("1Below","2Baseline","3Half","4Full","5Above") := list(1-(`4Full`%%1)/2,1,1+(`4Full`%%1)/2,1+(`4Full`%%1)*2)]
# Spare_Capacities_Obs <- data.table(Country = c("Saudi Arabia","UAE","Iraq","Kuwait","Algeria","Nigeria"),
#                                    `2Baseline` = c(.744,.767,.878,.862,.91,0.929))[,c("1Below","3Half","4Full","5Above") := list(`2Baseline` - (1-`2Baseline`)/2,`2Baseline` + (1-`2Baseline`)/2,1,1 + (1-`2Baseline`)/2)]



##### Scenario parameters #####
Values_Uniform <- c(25.7,50.6,72.1,93.2)
Values_NPP <- c(2,14.5,34.9,54.4)

Scenario_Parameters <- data.table(Scenario = c(paste("SCC",seq(0,300,25),sep="_"),paste("CB",seq(200,650,50),sep="_"),paste("Uniform",Values_Uniform,sep="_"),paste("NPP",Values_NPP,sep="_"),paste("NPI",seq(0.25,1,0.25),sep="_"),paste("ITU",c(0.1,0.25,0.5,1,2,3,4),sep="_"),paste("ITCI",seq(0,1000,50),sep="_")),
                                  Policy = c(rep("SCC",13),rep("CB",10),rep("Uniform",length(Values_Uniform)),rep("NPP",length(Values_NPP)),rep("NPI",4),rep("ITU",7),rep("ITCI",21)), #1. SCC or Carbon budget, No_Polluting_Prod, No_Polluting_Investment
                                  Policy_Values = c(seq(0,300,25),seq(200,650,50),Values_Uniform,Values_NPP,seq(0.25,1,0.25),c(0.1,0.25,0.5,1,2,3,4),seq(0,1000,50)),# 2. list of values to test
                                  rho = 0.03, #Actualization rate
                                  Distortions_Opti = "None", # "None","","Royalty","PriceDiscount", "Royalty_CTD"; if !="None", Taxes_Opex always included
                                  Incomplete_Development = T, #if True, authorizes incomplete dvpt, allowing proportional max production (plateau) i.e. if 30% of dvpt costs are paid, only 30% of plateau prod can be attained as max prod
                                  Progressive_Development = T, #Is development allowed to be carried out in multiple years ?
                                  Investment_Capped = "No", #No, Country, Majors_NOCs, Company, add number otherwise 1Mds$ (min observed investment over the period to be bound, in millions $)
                                  Production_Capped = "No", #No, Country, Total, OPEC_Country, OPEC_Total, Fringe_Country, Fringe_Total, Unit_Past, or any other name (provide Table named Prod_Capped_Name with Country * Year)
                                  Development_OPEC = "Optimal", #Development 1992-2021, 1st is for OPEC, 2nd for Non-OPEC
                                  Development_Fringe = "Optimal", #Development 1992-2021, 1st is for OPEC, 2nd for Non-OPEC,
                                  End_Year_Investment = Inf, #Year after which no investment allowed,
                                  Cost_Clean_Backstop = 163.2, #Extraction cost of the clean backstop (in the future can become a function)
                                  Demand = "Elastic", #Elastic, Inelastic, Inelastic_Past
                                  Demand_Source = NA, #Inelastic: EIA, etc (source demand --> Not implemented yet, lack data)
                                  Demand_Elasticity = "Annual", # Value or "Annual" (and add dataset named Annual_Elasticities with Year and Elast)
                                  Demand_Ref = "Observed", # Observed (observed demand and price), Annual (provide Annual_Ref), Annual_X (X year used as ref) or two values Quantity_ref and Price_ref
                                  Demand_Aggregation = "World",
                                  Market_Share_Fixed = "None") #OPEC, Country, Country_OPEC, Name of dataset with Country * Year and Market Share for which constant market share enforced

Scenario_Parameters <- Scenario_Parameters[grepl("ITCI",Scenario)==F & Policy != "CB"]

Scenario_Parameters <- rbind(Scenario_Parameters,
                             copy(Scenario_Parameters)[,c("Scenario","Demand_Elasticity") := list(paste(Policy,"Elast_2",Policy_Values,sep="_"),"Annual_2")],
                             copy(Scenario_Parameters)[,c("Scenario","Incomplete_Development") := list(paste(Policy,"Complete_Dvpt",Policy_Values,sep="_"),F)],
                             copy(Scenario_Parameters)[,c("Scenario","Progressive_Development") := list(paste(Policy,"Unique_Dvpt",Policy_Values,sep="_"),F)],
                             copy(Scenario_Parameters)[,c("Scenario","Incomplete_Development","Progressive_Development") := list(paste(Policy,"Binary_Dvpt",Policy_Values,sep="_"),F,F)],
                             copy(Scenario_Parameters)[,c("Scenario","End_Year_Investment") := list(paste(Policy,"NoNewInvestment",Policy_Values,sep="_"),2025)],
                             copy(Scenario_Parameters)[,c("Scenario","Production_Capped") := list(paste(Policy,"Shale",Policy_Values,sep="_"),"Shale")],
                             copy(Scenario_Parameters)[,c("Scenario","Production_Capped") := list(paste(Policy,"Shale_Startup",Policy_Values,sep="_"),"Shale_Startup")],
                             copy(Scenario_Parameters)[,c("Scenario","Production_Capped") := list(paste(Policy,"Shale_Approval",Policy_Values,sep="_"),"Shale_Approval")],
                             copy(Scenario_Parameters)[,c("Scenario","Production_Capped","Market_Share_Fixed") := list(paste(Policy,"Shale_OPEC_MS",Policy_Values,sep="_"),"Shale","OPEC")],
                             copy(Scenario_Parameters)[,c("Scenario","Production_Capped","Market_Share_Fixed") := list(paste(Policy,"Shale_Startup_OPEC_MS",Policy_Values,sep="_"),"Shale_Startup","OPEC")],
                             copy(Scenario_Parameters)[,c("Scenario","Production_Capped","Market_Share_Fixed") := list(paste(Policy,"Shale_Approval_OPEC_MS",Policy_Values,sep="_"),"Shale_Approval","OPEC")],
                             copy(Scenario_Parameters)[,c("Scenario","Production_Capped","Demand_Elasticity") := list(paste(Policy,"Shale_Startup_Elast2",Policy_Values,sep="_"),"Shale_Startup","Annual_2")],
                             copy(Scenario_Parameters)[,c("Scenario","Production_Capped","Demand_Elasticity") := list(paste(Policy,"Shale_Startup_Elast3",Policy_Values,sep="_"),"Shale_Startup","Annual_3")],
                             copy(Scenario_Parameters)[,c("Scenario","Production_Capped","Demand_Elasticity") := list(paste(Policy,"Shale_Startup_ElastCaldara",Policy_Values,sep="_"),"Shale_Startup",-0.14)],
                             copy(Scenario_Parameters)[,c("Scenario","Production_Capped","Demand_Elasticity") := list(paste(Policy,"Shale_Startup_ElastKilian",Policy_Values,sep="_"),"Shale_Startup",-0.26)],
                             copy(Scenario_Parameters)[,c("Scenario","Production_Capped","Demand_Ref") := list(paste(Policy,"Shale_Startup_Demand2015",Policy_Values,sep="_"),"Shale_Startup","Annual_2015")],
                             copy(Scenario_Parameters)[,c("Scenario","Production_Capped","Cost_Clean_Backstop") := list(paste(Policy,"Shale_Startup_CBP120",Policy_Values,sep="_"),"Shale_Startup",168.5*1.2)],
                             copy(Scenario_Parameters)[,c("Scenario","Production_Capped","Cost_Clean_Backstop") := list(paste(Policy,"Shale_Startup_CBP80",Policy_Values,sep="_"),"Shale_Startup",168.5*.8)])

Scenario_Parameters <- rbind(Scenario_Parameters,
                             copy(Scenario_Parameters)[grepl("Shale_Startup",Production_Capped)][,c("Scenario","Production_Capped") := list(gsub("Shale_Startup","Shale_Startup_Outage2019Y_2050",Scenario),gsub("Shale_Startup","Shale_Startup_Outage2019Y_2050",Production_Capped))],
                             copy(Scenario_Parameters)[grepl("Shale_Startup",Production_Capped)][,c("Scenario","Production_Capped") := list(gsub("Shale_Startup","Shale_Startup_Outage2019Y",Scenario),gsub("Shale_Startup","Shale_Startup_Outage2019Y",Production_Capped))])

Scenario_Parameters <- rbind(Scenario_Parameters,
                             copy(Scenario_Parameters)[grepl("2019Y",Scenario)][,c("Scenario","Production_Capped") := list(gsub("2019Y","Half2019Y",Scenario),gsub("2019Y","Half2019Y",Production_Capped))],
                             copy(Scenario_Parameters)[grepl("2019Y",Scenario)][,c("Scenario","Production_Capped") := list(gsub("2019Y","Over2019Y",Scenario),gsub("2019Y","Over2019Y",Production_Capped))])

Scenario_Parameters <- rbind(Scenario_Parameters,
                             Scenario_Parameters[grepl("Outage",Scenario)][,c("Scenario","Production_Capped") := list(gsub("Outage","OutageExt",Scenario),gsub("Outage","OutageExt",Production_Capped))])

# Scenario_Parameters <- rbind(Scenario_Parameters,
#                              copy(Scenario_Parameters[Scenario == "SCC_0"])[,c("Scenario","Demand","Demand_Ref","Cost_Clean_Backstop") := list("SCC_0_Rystad_Demand","Inelastic","Annual",250)])
# Scenario_Parameters <- rbind(Scenario_Parameters,
#                              copy(Scenario_Parameters[Scenario == "SCC_0"])[,c("Scenario","Demand","Demand_Ref","Cost_Clean_Backstop") := list("SCC_0_Rystad_Demand_Elastic","Elastic","Annual",250)],
#                              copy(Scenario_Parameters[Scenario == "SCC_0"])[,c("Scenario","Demand","Demand_Ref","Cost_Clean_Backstop") := list("SCC_0_2019_Demand_Elastic","Elastic","Observed",250)])

Annual_Ref <- merge(df_year_full[order(Year) & `Discovery Year` <= 2024 & `Start-up Year` <= 2080][Year %in% 1945:2100,.SD[,sum(Prod_Oil)],by=Year],
                    Selling_Prices[order(Year) & !duplicated(Year)][Year %in% 1945:2100, c("Year","Brent_Price_Deflated")], by ="Year")
#Annual_Ref[Year==2024,c("V1","Brent_Price_Deflated") := list(outages[Year==2024 & `Outage Detail`=="Production" & `Oil and Gas Category` %in% Oil_Definition,sum(value,na.rm=T)],80.5 * deflator[Year==2024,Deflate_factor])]
names(Annual_Ref)[2:3] <- c("Q_ref","P_ref")

Scenario_Parameters <- rbind(Scenario_Parameters,
                             copy(Scenario_Parameters)[Scenario=="SCC_0"][,c("Scenario","Production_Capped") := list("SCC_Validity_Shale_Startup_0","Shale_Startup")],
                             copy(Scenario_Parameters)[Scenario=="SCC_0"][,c("Scenario","Demand_Ref","Production_Capped") := list("SCC_Validity_Demand2019_Shale_Startup_0","Annual_2019","Shale_Startup")],
                             copy(Scenario_Parameters)[Scenario=="SCC_0"][,c("Scenario","Production_Capped") := list("SCC_Validity_Shale_Startup_OutageInvoluntaryPast_0","Shale_Startup_OutageInvoluntaryPast")],
                             copy(Scenario_Parameters)[Scenario=="SCC_0"][,c("Scenario","Demand_Ref","Production_Capped") := list("SCC_Validity_Demand2019_Shale_Startup_OutageInvoluntaryPast_0","Annual_2019","Shale_Startup_OutageInvoluntaryPast")],
                             copy(Scenario_Parameters)[Scenario=="SCC_0"][,c("Scenario","Production_Capped") := list("SCC_Validity_Shale_Startup_MarketPowerAll_0","Shale_Startup_OutageExt2019Y_2050")],
                             copy(Scenario_Parameters)[Scenario=="SCC_0"][,c("Scenario","Demand_Ref","Production_Capped") := list("SCC_Validity_Demand2019_Shale_Startup_MarketPowerAll_0","Annual_2019","Shale_Startup_OutageExt2019Y_2050")],
                             copy(Scenario_Parameters)[Scenario=="SCC_0"][,c("Scenario","Production_Capped") := list("SCC_Validity_Shale_Startup_OutageInvoluntaryPast_MarketPowerAll_0","Shale_Startup_OutageInvoluntaryPast_OutageExt2019Y_2050")],
                             copy(Scenario_Parameters)[Scenario=="SCC_0"][,c("Scenario","Demand_Ref","Production_Capped") := list("SCC_Validity_Demand2019_Shale_Startup_OutageInvoluntaryPast_MarketPowerAll_0","Annual_2019","Shale_Startup_OutageInvoluntaryPast_OutageExt2019Y_2050")])
Scenario_Parameters <- rbind(Scenario_Parameters,
                             copy(Scenario_Parameters)[grepl("Validity",Scenario)][,c("Scenario","Demand") := list(gsub("Validity","Validity_InelasticPast",Scenario),"Inelastic_Past")])

Scenario_Parameters <- rbind(Scenario_Parameters,
                             copy(Scenario_Parameters)[,c("Scenario","rho") := list(paste0(sub("(.*)_.*", "\\1", Scenario),"_DiscountHalf_",Policy_Values), 0.015)],
                             copy(Scenario_Parameters)[,c("Scenario","rho") := list(paste0(sub("(.*)_.*", "\\1", Scenario),"_Discount6_",Policy_Values), 0.06)],
                             copy(Scenario_Parameters)[,c("Scenario","rho") := list(paste0(sub("(.*)_.*", "\\1", Scenario),"_Discount9_",Policy_Values), 0.09)])

Scenario_Parameters <- rbind(Scenario_Parameters,
                             copy(Scenario_Parameters)[,c("Scenario","Distortions_Opti") := list(paste0(sub("(.*)_.*", "\\1", Scenario),"_Royalty_",Policy_Values), "Royalty")],
                             copy(Scenario_Parameters)[,c("Scenario","Distortions_Opti") := list(paste0(sub("(.*)_.*", "\\1", Scenario),"_Royalty_CTD_",Policy_Values), "Royalty_CTD")],
                             copy(Scenario_Parameters)[,c("Scenario","Distortions_Opti") := list(paste0(sub("(.*)_.*", "\\1", Scenario),"_PriceDiscount_",Policy_Values), "PriceDiscount")],
                             copy(Scenario_Parameters)[,c("Scenario","Distortions_Opti") := list(paste0(sub("(.*)_.*", "\\1", Scenario),"_Royalty_PriceDiscount_",Policy_Values), "Royalty_PriceDiscount")],
                             copy(Scenario_Parameters)[,c("Scenario","Distortions_Opti") := list(paste0(sub("(.*)_.*", "\\1", Scenario),"_Royalty_Profits_Tax_",Policy_Values), "Royalty_Profits_Tax")],
                             copy(Scenario_Parameters)[,c("Scenario","Distortions_Opti") := list(paste0(sub("(.*)_.*", "\\1", Scenario),"_Royalty_CTD_Profits_Tax_",Policy_Values), "Royalty_CTD_Profits_Tax")])


# Scenario_Parameters <- rbind(Scenario_Parameters,
#                              copy(Scenario_Parameters[Scenario == "SCC_Shale_Startup_200"])[,c("Scenario","Cost_Clean_Backstop") := list("Test_168.5",168.5)],
#                              copy(Scenario_Parameters[Scenario == "SCC_Shale_Startup_200"])[,c("Scenario","Cost_Clean_Backstop") := list("Test_169",169)],
#                              copy(Scenario_Parameters[Scenario == "SCC_Shale_Startup_200"])[,c("Scenario","Cost_Clean_Backstop") := list("Test_167",167)],
#                              copy(Scenario_Parameters[Scenario == "SCC_Shale_Startup_200"])[,c("Scenario","Cost_Clean_Backstop") := list("Test_168",168)])


#Scenario_Parameters <- as.data.table(map_dfr(seq_len(270), ~Scenario_Parameters))

Scenario_Parameters <- rbind(Scenario_Parameters,
                             copy(Scenario_Parameters)[grepl("2019Y",Scenario) & grepl("Strategic|Baseline",Scenario)==F][,c("Scenario","Production_Capped") := list(gsub("2019Y","2019YCorrected",Scenario),gsub("2019","2019Corrected",Production_Capped))],
                             copy(Scenario_Parameters)[grepl("Outage2019Y_2050",Scenario)][,Scenario := gsub("Outage2019Y_2050","MarketPowerRO",Scenario)],
                             copy(Scenario_Parameters)[grepl("OutageExt2019Y_2050",Scenario)][,Scenario := gsub("OutageExt2019Y_2050","MarketPowerAll",Scenario)])
Scenario_Parameters[grepl("OutageExt2019YCorrected_2050",Scenario),Scenario := gsub("OutageExt2019YCorrected_2050","MarketPowerAllCor",Scenario)]
# Scenario_Parameters <- rbind(Scenario_Parameters,
#                              copy(Scenario_Parameters)[Scenario=="SCC_Shale_Startup_PreCovid_0"])

#Setting up annual elasticities (if demand elasticity is "Annual")
# Annual_Elasticity_Setup <- data.table(Year = unlist(Unit_Parameters[1]$Year_Start_World):unlist(Unit_Parameters[1]$Year_End_Future),
#                                       Elasticity = -seq(0.1,0.1 + (unlist(Unit_Parameters[1]$Year_End_Future) - unlist(Unit_Parameters[1]$Year_Start_World))*0.005, 0.005))
Annual_Elasticity_Setup <- data.table(Year = 1901:2100,
                                      Elasticity = .1)[125:174,Elasticity := c(seq(.1,.28,.02),seq(.3,.49,.01),seq(.5,.595,.005))][174:200,Elasticity:=0.6][,Elasticity := -Elasticity]
Annual_Elasticity_Setup[,Elasticity_2 := Elasticity][Year >= 2025,Elasticity_2 := -pmin(0.1 + (.I-1)*.01,0.6)]
Annual_Elasticity_Setup[,Elasticity_3 := Elasticity][Year >= 2025,Elasticity_3 := -pmin(0.1 + (.I-1)*.02,0.6)]

##### Model parameters (Gurobi) #####

Model_Parameters <- list(list(Gurobi_Method = "Barrier",
                              Model_Method = 2,
                              Crossover = -1,
                              PWL_Points = 20000,
                              IntFeas = 1e-3,
                              MIP_Gap = 1e-4,
                              Threads_count = detectCores() - 2,
                              Nb_Nodes_Max = 10000,
                              MIP_Focus = "Default")) #Default, Bound, Solution, Optimality
Names_Parameters <- names(Model_Parameters[[1]])
Model_Parameters <- as.data.table(matrix(do.call(rbind, Model_Parameters),nrow=1))
names(Model_Parameters) <- Names_Parameters
Model_Parameters <- map_dfr(seq_len(3), ~Model_Parameters)
Model_Parameters[2,c("Gurobi_Method","Model_Method") :=  list("Automatic",-1)]
Model_Parameters[3,c("Gurobi_Method","Model_Method") :=  list("Simplex",1)]
# DC for MIP (deterministic concurrent)
# Barrier or Simplex for LP

##### Scenarios #####
##### To_Run = indicates all configurations for which we want the optimisation results
## Automatically checks if the set of parameters has already been run before, change name if necessary, create/update folders automatically

To_Run <- rbind(data.table(Unit = "Asset_UMD_CHR",
                           Scenario = paste0("SCC_",seq(0,250,50))),
                data.table(Unit = "Asset_UMD_CHR_CapPla",
                           Scenario = paste0("SCC_",seq(0,250,50))),
                data.table(Unit = "Asset_UMD_CHR_CapPla_InfRes",
                           Scenario = paste0("SCC_",seq(0,250,50))),
                data.table(Unit = "Asset_UMD_CHR_CapDec",
                           Scenario = paste0("SCC_",seq(0,200,50))),
                data.table(Unit = "Asset_UMD_CHR_CapDec_InfRes",
                           Scenario = paste0("SCC_",seq(0,200,50))),
                data.table(Unit = "Asset_UMD_CHR_CapPla",
                           Scenario = paste0("SCC_Startup_",seq(0,200,50))),
                data.table(Unit = "Asset_UMD_CHR_CapPla_InfRes",
                           Scenario = paste0("SCC_Startup_",seq(0,200,50))))
 
##### Running the optimisations ########

To_Run[,Model := "Barrier"]#[Unit=="LB_UMAverage_Baseline" & Scenario=="SCC_100",Model := "Simplex"]

To_Run[,Status := ifelse(file.exists(paste0("Output/GP_All/Results/",Unit,"/",Scenario,"_Results.csv")),"Done","Missing")]
To_Run[,Status_Raw := ifelse(file.exists(paste0("Output/GP_All/Results/",Unit,"/Results_Raw/",Scenario,"_Raw.csv")),"Done","Missing")]

#Scenario_Parameters <- Scenario_Parameters[Scenario %in% To_Run$Scenario]
Compute(To_Run = To_Run)


#Compute(To_Run = To_Run[Unit=="Asset_UMD_CHR" & grepl("MarketPower",Scenario)==F][grepl("SCC",Scenario) | grepl("Royalty",Scenario)==F]) #282:313 28:36,82:116  [c(37)][grepl("Binary",Scenario)==F]

To_Run[,Policy := sub("_.*", "\\1", Scenario)]
To_Run[,Scenario_Tweak := sub("^[^_]*_","",sub("(.*)_.*", "\\1", Scenario))][Scenario_Tweak==Policy,Scenario_Tweak:="Baseline"]
To_Run[,Complete := ifelse(sum(Status == "Missing")>0,F,T), by = .(Policy,Scenario_Tweak,Unit)]
To_Run[,Tax := as.numeric(sub(".*_(.*)", "\\1", Scenario))]
To_Run[,Status := ifelse(file.exists(paste0("Output/GP_All/Results/",Unit,"/",Scenario,"_Results.csv")),"Done","Missing")]#[On_HDD == T, Status := "Done"]
# c("SCC_Royalty_0","SCC_Inelastic_Past_0","SCC_Inelastic_Past_Royalty_0","SCC_Royalty_PriceDiscount_0")
# SCC_Fixed_Past_Demand_InvFix_Company_ProdCap_OPEC_Country_0 ==> > 21,600s
### Passé:
# somme des couts actualisés pas loin (objectif)

# for(unit in unique(To_Run$Unit)){
#   temp <- fread(paste0("Output/GP_All/Results/",unit,"/Scenarios.csv"))
#   #temp[,Scenario:=gsub("CT_Deducted","CTD",Scenario)][,Distortions_Opti:=gsub("CT_Deducted","CTD",Distortions_Opti)]
#   temp <- temp[grepl("NPP",Scenario)==F]
#   fwrite(temp,paste0("Output/GP_All/Results/",unit,"/Scenarios.csv"))
# }

# Si très loin --> Contrainte: prod OPEC constraint ? Scale OPEC / Scale Country

### Futur
# Résultats --> Profits variation avec taxe
# Arabie Saoudite agit de manière stratégique: qu'est ce qui se passe si elle augmente 
# Regarder % des réserves totales extraites au plateau dans pays hors OPEP --> Apply this to Saudian assets 
# On peut s'attendre à ce que l'AS réagisse à une taxe dans le futur --> Augm prod pour réduire prix

