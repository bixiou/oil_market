##### Functions - Useful #####
### Useful functions
Add_Constraint <- function(model,i,j,x,rhs,dim_i,sense,constrnames){
  objective_length <- ifelse(is.null(model$obj),length(model$multiobj[[1]]$objn),length(model$obj))
  model$A <- rbind(model$A,
                   sparseMatrix(i = i, j = j, x = x, dims = c(dim_i,objective_length)))
  model$constrnames <- c(model$constrnames,constrnames)
  model$sense <- c(model$sense,rep(sense, length(constrnames)))
  model$rhs <- c(model$rhs,rhs)
  return(model)
}

Add_Variable <- function(model,newvar,newvarnames,vtype){
  model$obj <- c(model$obj,newvar)
  
  if(vtype == "B"){
    model$vtype <- c(model$vtype,rep(vtype,length(newvar)))
    model$ub <- c(model$ub,rep(1,length(newvar)))
    model$lb <- c(model$lb,rep(0,length(newvar)))
  }else{
    model$vtype <- c(model$vtype,rep(vtype,length(newvar)))
    model$ub <- c(model$ub,rep(+Inf,length(newvar)))
    model$lb <- c(model$lb,rep(-Inf,length(newvar)))
  }
  model$varnames <- c(model$varnames,newvarnames)
  if(!is.null(model$A)) model$A <- cbind(model$A,Matrix(data=0,ncol=length(newvar),nrow=nrow(model$A),sparse = T))
  #model$A <- cbind(model$A,sparseMatrix(i=1,j=1,x=0,dims = c(nrow(model$A),length(newvar))))
  return(model)
}

Remove_Constraint <- function(model,constrnames){
  index_to_remove <- which(model$constrnames %in% constrnames)
  model$A <- model$A[-index_to_remove,]
  model$constrnames <- model$constrnames[-index_to_remove]
  model$sense <- model$sense[-index_to_remove]
  model$rhs <- model$rhs[-index_to_remove]
  return(model)
}

Remove_Variable <- function(model,varnames,SOS,SOS_Constraints){
  index_to_remove <- which(model$varnames %in% varnames)
  reindex <- data.table(Index_before = which(!(model$varnames %in% varnames)))[,Index_after := .I]
  model$obj <- model$obj[-index_to_remove]
  model$vtype <- model$vtype[-index_to_remove]
  model$ub <- model$ub[-index_to_remove]
  model$lb <- model$lb[-index_to_remove]
  model$varnames <- model$varnames[-index_to_remove]
  model$A <- model$A[,-index_to_remove]
  
  # if(length(model$sos)>0){
  #   for(i in 1:length(model$sos)){
  #     model$sos[[i]]$weight <- model$sos[[i]]$weight[-which(model$sos[[i]]$index %in% index_to_remove)]
  #     model$sos[[i]]$index <- model$sos[[i]]$index[-which(model$sos[[i]]$index %in% index_to_remove)]
  #     model$sos[[i]]$index <- reindex[Index_before %in% model$sos[[i]]$index,Index_after]
  #     if(length(model$sos[[i]]$index)==0) SOS_to_remove <- c(SOS_to_remove,i)
  #   }
  # }
  if(SOS == T){
    SOS <- SOS_Constraints[!(SOS_Index %in% index_to_remove)]
    SOS <- merge(SOS,reindex,by.x = "SOS_Index", by.y = "Index_before")[,SOS_Index := Index_after]
    
    model$sos <- list()
    SOS[,Index_Asset := as.numeric(factor(ID_Unit,levels = unique(ID_Unit)))]
    for(i in unique(SOS$Index_Asset)){
      model$sos[[i]] <- list(index = SOS[Index_Asset==i,SOS_Index],
                             weight = rep(1,length(SOS[Index_Asset==i,SOS_Index])),
                             type = 1)
    }
  }
  
  
  if(length(model$pwlobj)>0){
    for(i in 1:length(model$pwlobj)){
      model$pwlobj[[i]]$var <- reindex[Index_before==model$pwlobj[[i]]$var,Index_after]
    }
  }
  return(model)
}

Delete_Results <- function(Unit_Version,Scenarios){
  Scenario <- fread(paste0("Output/GP_All/Results/",Unit_Version,"/Scenarios.csv"))
  Scenario <- Scenario[!(Scenario %in% Scenarios)]
  fwrite(Scenario,paste0("Output/GP_All/Results/",Unit_Version,"/Scenarios.csv"))
  for(scenario in Scenarios){
    file.remove(paste0("Output/GP_All/Results/",Unit_Version,"/",scenario,"_Results.csv"))
    file.remove(paste0("Output/GP_All/Results/",Unit_Version,"/Results_Raw/",scenario,"_Raw.csv"))
  }
}

##### Function - Data preparation #####
Data_Preparation <- function(df_full,Unit_Param,Data_Selec){
  #attach(Unit_Param,warn.conflicts = F)
  Year_Start_Opti <- unlist(Unit_Param$Year_Start_World)
  
  ##### Optimization setup #####
  
  ### Oil costs or oil + gas costs
  Cost_Cols <- c("Abandonment_cost","Bonuses","Exploration_well_capex","GG_seismic_capex","Facility_capex","Government_profit","Income_tax","Production_opex","Royalty_effects","SGA_opex","Taxes_opex","Transportation_opex","Well_capex","Free_cash_flow")
  if(Unit_Param$Gas_Cost=="Included"){
    df_year <- copy(df_full)
    rm(df_full)
  } 
  if(Unit_Param$Gas_Cost == "Excluded"){
    df_year <- copy(df_full)
    rm(df_full)
    df_year[,(Cost_Cols) := lapply(.SD,function(d) d * sum(Prod_Oil)/sum(Prod_Tot)), .SDcols = Cost_Cols, by="RE ID"]
  }
  
  ### Identifying shale assets with corresponding shale play XXX
  df_year <- merge(df_year,asset_lvl[,c("RE ID","Shale Plays","Unconventional Detail")])
  df_year[,Shale := grepl("Tight|Shale|shale",`Unconventional Detail`)][Shale==1,Shale_Play := ifelse(`Shale Plays`=="Not shale play",Asset,`Shale Plays`)]
  df_year[,c("Shale Plays","Unconventional Detail") := NULL]
  
  ### Deflating cost data ####
  df_year <- merge(df_year,deflator,by="Year")
  Cost_Cols <- c("Abandonment_cost","Bonuses","Exploration_well_capex","GG_seismic_capex","Facility_capex","Government_profit","Income_tax","Production_opex","Royalty_effects","SGA_opex","Taxes_opex","Transportation_opex","Well_capex","Free_cash_flow")
  df_year[, (Cost_Cols) := lapply(.SD, function(d) d * get('Deflate_factor')), .SDcols = Cost_Cols]
  
  ### Keeping only selected assets
  List_ID_Selection <- asset_lvl$`RE ID`
  for(numrow in 1:nrow(Data_Selec)){
    if(gsub("`","",Data_Selec[numrow]$Variable) %in% names(asset_lvl)){
      List_ID_Selection <- List_ID_Selection[List_ID_Selection %in% asset_lvl[eval(parse(text=paste(Data_Selec[numrow]$Variable,Data_Selec[numrow]$Operator,Data_Selec[numrow]$Value)))]$`RE ID`]
    } else print(paste(Data_Selec[numrow]$Variable," - Variable not found"))
  }
  
  df_year_comp <- df_year[`RE ID` %in% List_ID_Selection][,.SD[sum(Prod_Oil[Year >= Year_Start_Opti]) > 0],by="RE ID"] #Keep df_year intact to be reused in results
  
  ### Computing Reserves #####
  reserves[,Resources_Oil_Minus10pct := Resources_Oil * 0.9]

  # if(grepl("bis_|ter_|quater_",Version)){
  #     if(grepl("bis_",Version)){
  #       if(Year_Start_Opti==2032){
  #         reserves <- merge(reserves,fread("Output/GP_All/Results/Asset_UMD_CHR_CapSpare4Full_V2/SCC_Shale_Startup_2Baseline_2032_Discount4.5_Royalty_CTD_0_Results.csv")[,c("ID_Unit","Year","Prod")],by.x=c("RE ID","Year"),by.y=c("ID_Unit","Year"),all.x=T)[is.na(Prod),Prod:=0]
  #       }else if(Year_Start_Opti==2050){
  #         reserves <- merge(reserves,fread("Output/GP_All/Results/Asset_UMD_CHR_CapSpare4Full_V2/SCC_Shale_Startup_2Baseline_2050_Discount4.5_Royalty_CTD_0_Results.csv")[,c("ID_Unit","Year","Prod")],by.x=c("RE ID","Year"),by.y=c("ID_Unit","Year"),all.x=T)[is.na(Prod),Prod:=0]
  #       }else if(Year_Start_Opti==2027){
  #         reserves <- merge(reserves,fread("Output/GP_All/Results/Asset_UMD_CHR_CapSpare4Full_V2/SCC_Shale_Startup_2Baseline_2027_Discount4.5_Royalty_CTD_0_Results.csv")[,c("ID_Unit","Year","Prod")],by.x=c("RE ID","Year"),by.y=c("ID_Unit","Year"),all.x=T)[is.na(Prod),Prod:=0]
  #       }else if(Year_Start_Opti==2035){
  #         reserves <- merge(reserves,fread("Output/GP_All/Results/Asset_UMD_CHR_CapSpare4Full_V2/SCC_Shale_Startup_2Baseline_2035_Discount4.5_Royalty_CTD_0_Results.csv")[,c("ID_Unit","Year","Prod")],by.x=c("RE ID","Year"),by.y=c("ID_Unit","Year"),all.x=T)[is.na(Prod),Prod:=0]
  #       }else if(Year_Start_Opti==2075){
  #         reserves <- merge(reserves,fread("Output/GP_All/Results/Asset_UMD_CHR_CapSpare4Full_V2/SCC_Shale_Startup_2Baseline_2075_Discount4.5_Royalty_CTD_0_Results.csv")[,c("ID_Unit","Year","Prod")],by.x=c("RE ID","Year"),by.y=c("ID_Unit","Year"),all.x=T)[is.na(Prod),Prod:=0]
  #       }
  #     }else if(grepl("ter_",Version)){
  #       if(Year_Start_Opti==2032){
  #         reserves <- merge(reserves,fread("Output/GP_All/Results/Asset_UMD_CHR_CapSpare4Full_V2/SCC_Shale_Startup_ElastCaldara_2Baseline_2032_Discount4.5_Royalty_CTD_0_Results.csv")[,c("ID_Unit","Year","Prod")],by.x=c("RE ID","Year"),by.y=c("ID_Unit","Year"),all.x=T)[is.na(Prod),Prod:=0]
  #       }else if(Year_Start_Opti==2050){
  #         reserves <- merge(reserves,fread("Output/GP_All/Results/Asset_UMD_CHR_CapSpare4Full_V2/SCC_Shale_Startup_ElastCaldara_2Baseline_2050_Discount4.5_Royalty_CTD_0_Results.csv")[,c("ID_Unit","Year","Prod")],by.x=c("RE ID","Year"),by.y=c("ID_Unit","Year"),all.x=T)[is.na(Prod),Prod:=0]
  #       }else if(Year_Start_Opti==2027){
  #         reserves <- merge(reserves,fread("Output/GP_All/Results/Asset_UMD_CHR_CapSpare4Full_V2/SCC_Shale_Startup_ElastCaldara_2Baseline_2027_Discount4.5_Royalty_CTD_0_Results.csv")[,c("ID_Unit","Year","Prod")],by.x=c("RE ID","Year"),by.y=c("ID_Unit","Year"),all.x=T)[is.na(Prod),Prod:=0]
  #       }else if(Year_Start_Opti==2035){
  #         reserves <- merge(reserves,fread("Output/GP_All/Results/Asset_UMD_CHR_CapSpare4Full_V2/SCC_Shale_Startup_ElastCaldara_2Baseline_2035_Discount4.5_Royalty_CTD_0_Results.csv")[,c("ID_Unit","Year","Prod")],by.x=c("RE ID","Year"),by.y=c("ID_Unit","Year"),all.x=T)[is.na(Prod),Prod:=0]
  #       }else if(Year_Start_Opti==2075){
  #         reserves <- merge(reserves,fread("Output/GP_All/Results/Asset_UMD_CHR_CapSpare4Full_V2/SCC_Shale_Startup_ElastCaldara_2Baseline_2075_Discount4.5_Royalty_CTD_0_Results.csv")[,c("ID_Unit","Year","Prod")],by.x=c("RE ID","Year"),by.y=c("ID_Unit","Year"),all.x=T)[is.na(Prod),Prod:=0]
  #       }
  #     }else if(grepl("quater_",Version)){
  #       if(Year_Start_Opti==2032){
  #         reserves <- merge(reserves,fread("Output/GP_All/Results/Asset_UMD_CHR_CapSpare4Full_V2/SCC_Shale_Startup_2Baseline_2032_ElastKilian_Discount4.5_Royalty_CTD_0_Results.csv")[,c("ID_Unit","Year","Prod")],by.x=c("RE ID","Year"),by.y=c("ID_Unit","Year"),all.x=T)[is.na(Prod),Prod:=0]
  #       }else if(Year_Start_Opti==2050){
  #         reserves <- merge(reserves,fread("Output/GP_All/Results/Asset_UMD_CHR_CapSpare4Full_V2/SCC_Shale_Startup_2Baseline_2050_ElastKilian_Discount4.5_Royalty_CTD_0_Results.csv")[,c("ID_Unit","Year","Prod")],by.x=c("RE ID","Year"),by.y=c("ID_Unit","Year"),all.x=T)[is.na(Prod),Prod:=0]
  #       }else if(Year_Start_Opti==2027){
  #         reserves <- merge(reserves,fread("Output/GP_All/Results/Asset_UMD_CHR_CapSpare4Full_V2/SCC_Shale_Startup_2Baseline_2027_ElastKilian_Discount4.5_Royalty_CTD_0_Results.csv")[,c("ID_Unit","Year","Prod")],by.x=c("RE ID","Year"),by.y=c("ID_Unit","Year"),all.x=T)[is.na(Prod),Prod:=0]
  #       }else if(Year_Start_Opti==2035){
  #         reserves <- merge(reserves,fread("Output/GP_All/Results/Asset_UMD_CHR_CapSpare4Full_V2/SCC_Shale_Startup_2Baseline_2035_ElastKilian_Discount4.5_Royalty_CTD_0_Results.csv")[,c("ID_Unit","Year","Prod")],by.x=c("RE ID","Year"),by.y=c("ID_Unit","Year"),all.x=T)[is.na(Prod),Prod:=0]
  #       }else if(Year_Start_Opti==2075){
  #         reserves <- merge(reserves,fread("Output/GP_All/Results/Asset_UMD_CHR_CapSpare4Full_V2/SCC_Shale_Startup_2Baseline_2075_ElastKilian_Discount4.5_Royalty_CTD_0_Results.csv")[,c("ID_Unit","Year","Prod")],by.x=c("RE ID","Year"),by.y=c("ID_Unit","Year"),all.x=T)[is.na(Prod),Prod:=0]
  #       }
  #     }
  #     reserves[,Res_Minus_2022Baseline := Resources_Oil[Year==2022] - shift(cumsum(Prod),fill=0),by="RE ID"]
  #     reserves[,Prod := NULL]
  # }
  df_year_comp <- merge(df_year_comp,reserves[Year==Year_Start_Opti,.SD,.SDcols=unlist(c("RE ID",Unit_Param$Reserve_Definition))],by="RE ID",all.x=T)
  names(df_year_comp)[ncol(df_year_comp)] <- "Reserves"
  df_year_comp[is.na(Reserves),Reserves := 0]
  df_year_comp[!(`RE ID` %in% unique(df_year_comp[Year >= Year_Start_Opti,`RE ID`])),Reserves := 0]

  ### Computing decline rates ####
  df_year_comp[, Decline_Rate := max(ifelse(shift(cumsum(Prod_Oil),fill=0)/sum(Prod_Oil) <= unlist(Unit_Param$Decline_Rate_Max_Depletion), 
                                            Prod_Oil / (sum(Prod_Oil) - shift(cumsum(Prod_Oil),fill=0)),
                                            0)), by = "RE ID"][,Decline_Rate := unique(Decline_Rate[!is.na(Decline_Rate)]),by="RE ID"][Decline_Rate==0,Decline_Rate := 1]
 
  if(grepl("Hybrid_|Plateau_|Observed_",Unit_Param$Production_Capacity_OPEC) & grepl("_OPEC_",Unit_Param$Production_Capacity_OPEC)) df_year_comp[`RE ID` %in% asset_lvl[`OPEC NON-OPEC`=="OPEC"]$`RE ID`, Decline_Rate :=  Decline_Rate * 0.01 * as.numeric(sub('.+_(.+)', '\\1', Unit_Param$Production_Capacity_OPEC))]
  if(grepl("Hybrid_|Plateau_|Observed_",Unit_Param$Production_Capacity_OPEC) & grepl("_SA_",Unit_Param$Production_Capacity_OPEC)) df_year_comp[Country=="Saudi Arabia", Decline_Rate :=  Decline_Rate * 0.01 * as.numeric(sub('.+_(.+)', '\\1', Unit_Param$Production_Capacity_OPEC))]
  if(grepl("Hybrid_|Plateau_|Observed_",Unit_Param$Production_Capacity_OPEC) & grepl("_Spare_",Unit_Param$Production_Capacity_OPEC)){
    #df_year_comp[, Spare_Cap_Def :=  sub('.+_(.+)', '\\1', Production_Capacity_OPEC)]
    df_year_comp <- merge(df_year_comp,Spare_Capacities_Obs[,c("Country",sub('.+_(.+)', '\\1', Unit_Param$Production_Capacity_OPEC)),with=F],by="Country",all.x=T)
    df_year_comp[!is.na(get(sub('.+_(.+)', '\\1', Unit_Param$Production_Capacity_OPEC))), Decline_Rate :=  Decline_Rate * get(sub('.+_(.+)', '\\1', Unit_Param$Production_Capacity_OPEC))]
    df_year_comp <- df_year_comp[,-c(sub('.+_(.+)', '\\1', Unit_Param$Production_Capacity_OPEC)),with=F]
  } 
  if(grepl("Hybrid_|Plateau_|Observed_",Unit_Param$Production_Capacity_Fringe) & grepl("Spare",Unit_Param$Production_Capacity_Fringe)) df_year_comp[`RE ID` %in% asset_lvl[`OPEC NON-OPEC`!="OPEC"]$`RE ID`, Decline_Rate :=  Decline_Rate * 0.01 * as.numeric(sub('.+_(.+)', '\\1', Unit_Param$Production_Capacity_Fringe))]
  # df_year_comp[,Decline := ifelse(shift(cumsum(Prod_Oil),fill=0)/sum(Prod_Oil) <= 0.9, 
  #                                 Prod_Oil / (sum(Prod_Oil) - shift(cumsum(Prod_Oil),fill=0)),
  #                                 0), by = "RE ID"]
  # df_year_comp[order(-Decline),Rank := 1:.N,by=`RE ID`][,Decline_Rate_2 := mean(Decline[Rank <= round(pmax(sum(Prod_Oil>0)/10,1))]),by = `RE ID`][,Rank := NULL]
  
  ### Buildup Years ##### 
  # Maximum 5 years
  df_year_comp[, Buildup_Years := min(4,min(Year[Prod_Oil >= 0.8 * max(Prod_Oil)]) - `Start-up Year`),by="RE ID"][Shale==T,Buildup_Years:=0]
  
  ### Computing costs #####
  df_year_comp$Extrac_cost <- rowSums(df_year_comp[,.SD,.SDcols=unlist(Unit_Param$Costs_Included)])
  
  rho <- 0.03
  
  # Average prod cost over the future
  if(Unit_Param$Cost_Definition=="Average_Future"){
    df_year_comp[, Extrac_cost := sum(Extrac_cost[Year >= Year_Start_Opti]) / sum(Prod_Oil[Year >= Year_Start_Opti]) ,by="RE ID"][is.na(Extrac_cost),Extrac_cost := 1000]
  }else if(Unit_Param$Cost_Definition=="Breakeven"){
    df_year_comp[, Extrac_cost := sum(Extrac_cost * exp(-unlist(rho)*(Year-Year_Start_Opti))) / sum(Prod_Oil * exp(-unlist(rho)*(Year-Year_Start_Opti))), by = `RE ID`]
  }else{
    df_year_comp[, Extrac_cost := sum(Extrac_cost) / sum(Prod_Oil) ,by="RE ID"]
  }

  # 1. For Development cost separated
  if(Unit_Param$Development_Cost=="Separate"){
    
    # Adapting Extrac cost (removing dvpt costs)
    df_year_comp[,Development_Cost_Year := Facility_capex * (Year < `Start-up Year` + unlist(Unit_Param$Limit_Development_Year)) + Well_capex * (Year < `Start-up Year` + unlist(Unit_Param$Limit_Development_Year))]
    
    df_year_comp[Shale == T, Development_Cost_Year := 0]
    
    df_year_comp[Development_Cost_Year != 0,Development_Cost_Year := Development_Cost_Year + (Development_Cost_Year / (Development_Cost_Year + SGA_opex + Transportation_opex + Production_opex)) * (ifelse("Royalty_effects" %in% unlist(Unit_Param$Costs_Included),Royalty_effects,0) + ifelse("Government_profit" %in% unlist(Unit_Param$Costs_Included),Government_profit,0) + ifelse("Income_tax" %in% unlist(Unit_Param$Costs_Included),Income_tax,0))]
    
    df_year_comp[, Development_Cost := sum(Development_Cost_Year),by="RE ID"]
    
    df_year_comp[,Production_Last_Year := max(Year[Prod_Oil>0]), by = "RE ID"]
    
    # Computing depreciation rate (avg)
    df_year_comp[,Delta_K := sum(Facility_capex[Year >= (`Start-up Year`+ unlist(Unit_Param$Limit_Development_Year))] + Well_capex[Year >= (`Start-up Year`+ unlist(Unit_Param$Limit_Development_Year))])/Development_Cost / (Production_Last_Year - (`Start-up Year`+ unlist(Unit_Param$Limit_Development_Year))),by="RE ID"]
    df_year_comp[Shale==T, Delta_K := 0]
    df_year_comp[is.na(Delta_K) | Delta_K > 0.1,Delta_K := 0.1]
    df_year_comp[Delta_K < 0, Delta_K := 0]
    if(Unit_Param$Capital_Depreciation == F) df_year_comp[,Delta_K := 0]
    
    #If depreciation activated, removing delta_k from extrac costs (K ma)
    if(Unit_Param$Cost_Definition=="Average"){
      df_year_comp[, Extrac_cost := Extrac_cost - sum(Development_Cost_Year) * (1 + Delta_K * (Production_Last_Year - `Start-up Year` - unlist(Unit_Param$Limit_Development_Year))) /sum(Prod_Oil), by = `RE ID`]
    }else if(Unit_Param$Cost_Definition=="Breakeven"){
      df_year_comp[, Extrac_cost := Extrac_cost - sum(Development_Cost_Year * exp(-unlist(rho)*(Year-Year_Start_Opti)))/sum(Prod_Oil * exp(-unlist(rho)*(Year-Year_Start_Opti))), by = `RE ID`]
    }

    
    if(Unit_Param$Asset_Remaining_Development_Cost == "As of Year_Start_World"){
      df_year_comp[, Development_Level_Start_Opti := sum(Development_Cost_Year * (Year < pmin(`Start-up Year` + unlist(Unit_Param$Limit_Development_Year),Year_Start_Opti))) / Development_Cost,by="RE ID"]
    }else{
      df_year_comp[, Development_Level_Start_Opti := ifelse(pmax(`Start-up Year` - unlist(Unit_Param$Lag_After_Development),`Discovery Year`) < Year_Start_Opti,1,0)]
    }
    df_year_comp[is.na(Development_Level_Start_Opti),Development_Level_Start_Opti := ifelse(`Start-up Year` < Year_Start_Opti | Shale==1,1,0)]
    
  }else{df_year_comp[,c("Development_Cost_Year","Development_Cost","Development_Level_Start_Opti","Delta_K") := list(0,0,1,0),]}
  
  
  temp_obs <- df_year_comp[,Prod_Obs:=Prod_Oil][,.SD,.SDcols = c("RE ID","Year","Reserves","Start-up Year","Prod_Obs","Development_Cost_Year","Abandonment_cost","Bonuses","Exploration_capex","Facility_capex","Government_profit","Income_tax","Production_opex","Royalty_effects","SGA_opex","Taxes_opex","Transportation_opex","Well_capex","Delta_K")]
  
  ### CIs ######
  
  df_year_comp <- df_year_comp[,-c("Crude_Oil_Mbbl","Condensate_Mbbl","NGL_Mbbl","Gas_Mboe","GTL_Mbbl","Free_cash_flow","Deflate_factor")]
  
  ### Adding carbon intensities (take care of scenarios DRE or FOR by means btw CI_Final_EBA_DRE_91/CI_Final_EBA_DRE_100 and CI_Final_EBA/CI_Final_EBA_Fla_0)
  if(Unit_Param$CI_GWP == 20) CI_Merge <- CI_AR6_20y else CI_Merge <- CI
  
  CI_Merge <- merge(CI_Merge,df_year_comp[!duplicated(`RE ID`),c("RE ID","Reserves")],by.x="REID",by.y="RE ID")
  
  CI_Merge[,theta := ifelse("Upstream" %in% unlist(Unit_Param$CI_Lifecycle),
                            ifelse(Unit_Param$CI_Upstream_Method=="EBA",
                                   ifelse(Unit_Param$CI_Upstream_Scenario=="Baseline",CI_Final_EBA,get(paste0("CI_Final_EBA_",Unit_Param$CI_Upstream_Scenario))),
                            ifelse(CI_Upstream_Method=="CPD",CI_Final_CPD,0)),0) +
       ifelse("Midstream" %in% unlist(Unit_Param$CI_Lifecycle),CI_midstream,0) +
       ifelse("Downstream" %in% unlist(Unit_Param$CI_Lifecycle),CI_downstream,0) +
       ifelse("DownstreamUniform" %in% unlist(Unit_Param$CI_Lifecycle),weighted.mean(CI_Merge$CI_downstream,CI_Merge$Reserves),0) +
       ifelse("UMAverage" %in% unlist(Unit_Param$CI_Lifecycle),CI_Final_EBA+CI_midstream,0) +
       ifelse("UMDAverage" %in% unlist(Unit_Param$CI_Lifecycle),CI_Final_EBA+CI_midstream+CI_downstream,0),by="REID"] #in kg/bbl
  CI_Merge[,theta_Share_Downstream := Vectorize(ifelse)("Downstream" %in% unlist(Unit_Param$CI_Lifecycle),CI_downstream / theta,0)]
  df_year_comp <- merge(df_year_comp,CI_Merge[,c("REID","theta","theta_Share_Downstream")],by.x="RE ID",by.y="REID",all.x=T)
  
  df_year_comp <- df_year_comp[!is.na(theta)] #XXX A changer / Verif avec code CI
  
  ### Tax averages #####
  Selling_Prices_Temp <- merge(Selling_Prices,df_year_comp[,c("RE ID","Year","Prod_Oil")], by = c("RE ID","Year"))
  Selling_Prices_Temp[,Discount_Price := weighted.mean(`Price (USD/bbl)` / `Brent oil price (USD/bbl)`, Prod_Oil * as.numeric(Year >= Year_Start_Opti)), by = "RE ID"]
  
  ### Compute tax averages XXX --> First Tax_Bases (gross prod for tax_opex, Gross revenue for royalties, Revenue after royalties and expenses for Income Tax and gvt profit)
  Tax_Averages <- merge(merge(
    df_year_comp[Year >= Year_Start_Opti & Year <= unlist(Unit_Param$Year_End_Future)],
    Selling_Prices_Temp[!duplicated(paste(`RE ID`,Year)),c("RE ID","Year","Discount_Price")], by=c("RE ID","Year"),all.x=T),
    Selling_Prices_Temp[!duplicated(Year),c("Year","Brent_Price_Deflated")],by="Year")[is.na(Discount_Price), Discount_Price := 1]
  Tax_Averages[,Tax_Opex_Avg := sum(Taxes_opex)/sum(Prod_Oil), by = "RE ID"]
  Tax_Averages[,Royalty_Avg := sum(Royalty_effects) / sum(Prod_Oil * Brent_Price_Deflated * Discount_Price), by = "RE ID"][,Royalty_Avg := pmin(pmax(0,Royalty_Avg),0.8)]
  Tax_Averages[,Gvt_Profit_Avg := sum(Government_profit) / sum((Prod_Oil * Brent_Price_Deflated * Discount_Price  - (Production_opex + Transportation_opex + SGA_opex + Facility_capex + Well_capex + Royalty_effects + Taxes_opex))), by = "RE ID"][Gvt_Profit_Avg<0,Gvt_Profit_Avg:=0][Gvt_Profit_Avg>0.95,Gvt_Profit_Avg:=0.95] 
  Tax_Averages[,Income_Tax_Avg := sum(Income_tax) / sum((Prod_Oil * Brent_Price_Deflated * Discount_Price  - (Production_opex + Transportation_opex + Exploration_capex + SGA_opex + Facility_capex + Well_capex + Royalty_effects + Taxes_opex + Government_profit))), by = "RE ID"][Income_Tax_Avg<0,Income_Tax_Avg:=0][Income_Tax_Avg>0.95,Income_Tax_Avg:=0.95]
  Tax_Averages <- Tax_Averages[!duplicated(`RE ID`),c("RE ID","Reserves","Tax_Opex_Avg","Royalty_Avg","Gvt_Profit_Avg","Income_Tax_Avg","Discount_Price")]
  Tax_Averages[is.na(Tax_Opex_Avg),Tax_Opex_Avg:=0][is.na(Royalty_Avg),Royalty_Avg:=0][is.na(Gvt_Profit_Avg),Gvt_Profit_Avg:=0][is.na(Income_Tax_Avg),Income_Tax_Avg:=0][is.na(Discount_Price),Discount_Price:=1] # 1 tiny issue with MXF0124 when start year is 2019; line just to be sure
  
  ### Aggregation #####
  # Cleaning issues with asset names (assigning number at the end of the name to differentiate)
  df_year_comp[Asset %in% df_year_comp[!duplicated(`RE ID`)][duplicated(Asset)]$Asset,
               Asset := paste(Asset,as.numeric(factor(`RE ID`)),sep =" - "),
               by = Asset]
  
  if(!(Unit_Param$Unit %in% names(df_year_comp))){
    df_year_comp <- merge(df_year_comp,asset_lvl[,.SD,.SDcols = c("RE ID",unlist(Unit))],by="RE ID")
  }
  df_year_comp[,Unit := df_year_comp[,.SD,.SDcols = unlist(Unit_Param$Unit)]]
  
  temp_obs <- merge(temp_obs,df_year_comp[!duplicated(`RE ID`),c("RE ID","Unit")], by="RE ID")
  
  df_year_comp <- merge(df_year_comp,asset_lvl[,c("RE ID","OPEC NON-OPEC")],by = "RE ID")[,OPEC := ifelse(`OPEC NON-OPEC`=="OPEC",T,F)][,`OPEC NON-OPEC` := NULL]
  
  DT_Opti <- merge(merge(merge(df_year_comp[,lapply(.SD,sum),by = .(Unit,Year), .SDcols = c("Prod_Oil","Prod_Gas","Prod_Tot","Abandonment_cost","Bonuses","Exploration_capex","Facility_capex","Government_profit","Income_tax","Production_opex","Royalty_effects","SGA_opex","Taxes_opex","Transportation_opex","Well_capex")],
                               df_year_comp[!duplicated(`RE ID`)][,lapply(.SD,min),by = Unit, .SDcols = c("Award Year","Discovery Year","Approval Year","Start-up Year")],by = "Unit"),
                         df_year_comp[order(`Start-up Year`)][,lapply(.SD,first),by = Unit, .SDcols = c("RE ID","Asset","OPEC","Country","Province","Area","Location","Project","Fiscal Regime Group","Tax Model Name","Offshore","Shale","Shale_Play")],by = c("Unit")),
                   df_year_comp[!duplicated(`RE ID`)][,lapply(.SD,sum),by = .(Unit), .SDcols = c("Reserves","Development_Cost")],by = c("Unit"))
  
  DT_Opti <- merge(DT_Opti,df_year_comp[!duplicated(`RE ID`)][,c("Decline_Rate","Buildup_Years","Delta_K","Extrac_cost","Development_Level_Start_Opti","theta","theta_Share_Downstream") := 
                                          list(weighted.mean(Decline_Rate,Reserves),
                                               weighted.mean(Buildup_Years,Reserves),
                                               weighted.mean(Delta_K,Development_Cost+1e-8),
                                               weighted.mean(Extrac_cost,Reserves),
                                               weighted.mean(Development_Level_Start_Opti,Development_Cost),
                                               weighted.mean(theta,Reserves),
                                               weighted.mean(theta_Share_Downstream,Reserves)),
                                        by = Unit][!duplicated(Unit),c("Unit","Decline_Rate","Buildup_Years","Delta_K","Extrac_cost","Development_Level_Start_Opti","theta","theta_Share_Downstream")],
                   by = "Unit")
  DT_Opti[is.na(Development_Level_Start_Opti),Development_Level_Start_Opti := ifelse(`Start-up Year` < Year_Start_Opti,1,0)]
  
  DT_Opti[,Unit_Extrac_cost := Extrac_cost]
  if("UMAverage" %in% unlist(Unit_Param$CI_Lifecycle) | "UMDAverage" %in% unlist(Unit_Param$CI_Lifecycle)) DT_Opti[,theta := weighted.mean(theta[!duplicated(`RE ID`)],Reserves[!duplicated(`RE ID`)])]

  DT_Opti[,c("ID_Unit","ID_Duplicate","Bin_Cost","Bin_Reserves") := list(`RE ID`,paste(`RE ID`,1,sep="_"),1,Reserves),]
  DT_Opti <- DT_Opti[Reserves >= unlist(Unit_Param$Reserves_Min)]
  
  if(!(Unit_Param$Unit %in% names(Tax_Averages))){
    Tax_Averages <- merge(Tax_Averages,asset_lvl[,.SD,.SDcols = c("RE ID",unlist(Unit_Param$Unit))],by="RE ID")
  }
  Tax_Averages$Unit <- Tax_Averages[,.SD,.SDcols = unlist(Unit_Param$Unit)]
  
  Tax_Averages[,c("Tax_Opex_Avg","Royalty_Avg","Gvt_Profit_Avg","Income_Tax_Avg") := 
                 list(weighted.mean(Tax_Opex_Avg,Reserves),
                      weighted.mean(Royalty_Avg,Reserves),
                      weighted.mean(Gvt_Profit_Avg,Reserves),
                      weighted.mean(Income_Tax_Avg,Reserves)), by = Unit]
  Tax_Averages <- merge(Tax_Averages,DT_Opti[!duplicated(ID_Unit),c("Unit","ID_Unit")],by="Unit")
  
  fwrite(Tax_Averages[!duplicated(Unit),c("ID_Unit","Unit","Tax_Opex_Avg","Royalty_Avg","Gvt_Profit_Avg","Income_Tax_Avg","Discount_Price")],paste0("Output/GP_All/Results/",Unit_Param$Version,"/Misc/Tax_Averages.csv"))
  
  ### 2. For cost Unit_Param$Bins #####
  
  if(Unit_Param$Bins==T){
    # Computing the cost ratio in bin1 so that overall cost over all asset reserve is equal
    ratio_cost_bin1 <- 1 / (sum(shift(unlist(Unit_Param$Bins_Ratio),fill=1) * unlist(Unit_Param$Bins_Threshold)) +
                              unlist(Unit_Param$Bins_Ratio)[length(unlist(Unit_Param$Bins_Ratio))] -
                              sum(unlist(Unit_Param$Bins_Threshold) * unlist(Unit_Param$Bins_Ratio)))
    # Duplicating assets per Unit_Param$Bins and modifying vars "Extrac_cost" and "Unit_Param$Bins_Reserves
    DT_Opti[Bin_Cost == 1, Extrac_cost := Extrac_cost * ratio_cost_bin1 ,]
    DT_Opti[Bin_Cost == 1, Bin_Reserves := Reserves * unlist(Unit_Param$Bins_Threshold)[1] - ifelse(Unit_Param$Development_Cost=="Separate",1e-6,0),]
    for(bin in 2:(length(unlist(Unit_Param$Bins_Ratio))+1)){
      DT_Opti <- rbind(DT_Opti,DT_Opti[Bin_Cost == 1][,ID_Duplicate := paste(ID_Unit,bin,sep="_"),][,Bin_Cost := bin,][,Extrac_cost := Extrac_cost * unlist(Unit_Param$Bins_Ratio)[bin - 1],])
      DT_Opti[Bin_Cost == bin, Bin_Reserves := Reserves * (ifelse(is.na(unlist(Unit_Param$Bins_Threshold)[bin]),1,unlist(Unit_Param$Bins_Threshold)[bin]) - unlist(Unit_Param$Bins_Threshold)[bin-1]),]
    }
  }
  
  ### In case of multiple bins and Prod_Past = "Observed", attributing past production to each bin
  DT_Opti[,Prod_bin := Prod_Oil,]
  setkeyv(DT_Opti, c("Bin_Cost","ID_Unit", "Year"))
  if(Unit_Param$Bins == T){
    for(bin in min(DT_Opti$Bin_Cost):max(DT_Opti$Bin_Cost)){
      DT_Opti[Bin_Cost <= bin, Prod_bin := ifelse(Bin_Cost < bin,Prod_bin,Prod_bin - sum(Prod_bin * (Bin_Cost < bin),na.rm=T)),by=.(ID_Unit,Year)]
      DT_Opti[Bin_Cost == bin, Prod_bin := (ifelse(cumsum(Prod_bin)<Bin_Reserves,cumsum(Prod_bin),Bin_Reserves) - shift(cumsum(Prod_bin),fill=0,type="lag")) * (shift(cumsum(Prod_bin),fill=0) <= Bin_Reserves),by="ID_Duplicate"]
    }
  }
  DT_Opti[abs(Prod_bin) < 1e-12,Prod_bin := 0,]
  
  ### Completing the dataset #####
  
  DT_Opti <- setDT(DT_Opti)[CJ(ID_Duplicate = ID_Duplicate, Year = c(1900:2100), unique=T),on=.(ID_Duplicate,Year)]
  
  DT_Opti[ , c("Unit","RE ID","Asset","OPEC","Country","Province","Area","Location","Project","Fiscal Regime Group","Tax Model Name","Award Year","Discovery Year","Approval Year","Start-up Year","Offshore","Shale","Shale_Play","Reserves","Extrac_cost","Unit_Extrac_cost","ID_Unit","Bin_Cost","Bin_Reserves","Development_Cost","Delta_K","theta","theta_Share_Downstream","Development_Level_Start_Opti","Buildup_Years","Decline_Rate") := 
             lapply(.SD, function(y) unique(y[!is.na(y)])),
           by ="ID_Duplicate",
           .SDcols = c("Unit","RE ID","Asset","OPEC","Country","Province","Area","Location","Project","Fiscal Regime Group","Tax Model Name","Award Year","Discovery Year","Approval Year","Start-up Year","Offshore","Shale","Shale_Play","Reserves","Extrac_cost","Unit_Extrac_cost","ID_Unit","Bin_Cost","Bin_Reserves","Development_Cost","Delta_K","theta","theta_Share_Downstream","Development_Level_Start_Opti","Buildup_Years","Decline_Rate")]
  
  DT_Opti[ , c("Prod_Oil","Prod_Gas","Prod_Tot","Prod_bin","Abandonment_cost","Bonuses","Exploration_capex","Facility_capex","Government_profit","Income_tax","Production_opex","Royalty_effects","SGA_opex","Taxes_opex","Transportation_opex","Well_capex") :=
             lapply(.SD, function(y) ifelse(is.na(y),0,y)),
           by ="ID_Duplicate",
           .SDcols = c("Prod_Oil","Prod_Gas","Prod_Tot","Prod_bin","Abandonment_cost","Bonuses","Exploration_capex","Facility_capex","Government_profit","Income_tax","Production_opex","Royalty_effects","SGA_opex","Taxes_opex","Transportation_opex","Well_capex")]
  
  #DT_Opti[,Bin_Reserves_Start_Opti := sum(Prod_bin[Year >= Year_Start_Opti]),by="ID_Duplicate"]#[ID_Unit=="CBS0000",Bin_Reserves_Start_Opti := Inf]
  #DT_Opti[,Reserves_Start_Opti := sum(Prod_bin[Year >= Year_Start_Opti]),by="ID_Unit"]#[ID_Unit=="CBS0000",Reserves_Start_Opti := Inf]
  
  ### Computing max prod per year (plateau)
  #DT_Opti[,Prod_Plateau := max(Prod_Oil,na.rm=T),by="ID_Unit"]
  #DT_Opti[order(-Prod_Oil),Rank := 1:.N,by=ID_Unit][,Prod_Plateau_2 := mean(Prod_Oil[Rank <= 3]),by = ID_Unit]
  
  ### Plateau Capacity ####
  DT_Opti[order(-Prod_Oil),Rank := 1:.N,by=ID_Unit][,Prod_Plateau := mean(Prod_Oil[Rank <= round(pmax(sum(Prod_Oil>0)/10,1))]),by = ID_Unit][,Rank := NULL]
  
  if(grepl("Plateau_|Hybrid_",Unit_Param$Production_Capacity_OPEC) & grepl("_OPEC_",Unit_Param$Production_Capacity_OPEC)){
    DT_Opti[OPEC==T,Prod_Plateau := Prod_Plateau * 0.01 * as.numeric(sub('.+_(.+)', '\\1', Unit_Param$Production_Capacity_OPEC))]
  }
  if(grepl("Plateau_|Hybrid_",Unit_Param$Production_Capacity_OPEC) & grepl("_SA_",Unit_Param$Production_Capacity_OPEC)){
    DT_Opti[Country=="Saudi Arabia",Prod_Plateau := Prod_Plateau * 0.01 * as.numeric(sub('.+_(.+)', '\\1', Unit_Param$Production_Capacity_OPEC))]
  }
  if(grepl("Hybrid_|Plateau_",Unit_Param$Production_Capacity_OPEC) & grepl("_Spare_",Unit_Param$Production_Capacity_OPEC)){
    DT_Opti <- merge(DT_Opti,Spare_Capacities_Obs[,c("Country",sub('.+_(.+)', '\\1', Unit_Param$Production_Capacity_OPEC)),with=F],by="Country",all.x=T)
    DT_Opti[!is.na(get(sub('.+_(.+)', '\\1', Unit_Param$Production_Capacity_OPEC))), Prod_Plateau :=  Prod_Plateau * get(sub('.+_(.+)', '\\1', Unit_Param$Production_Capacity_OPEC))]
    DT_Opti <- DT_Opti[,-c(sub('.+_(.+)', '\\1', Unit_Param$Production_Capacity_OPEC)),with=F]
  }  
  
  if(grepl("Plateau_|Hybrid_",Unit_Param$Production_Capacity_Fringe) & !is.na(as.numeric(sub('.+_(.+)', '\\1', Unit_Param$Production_Capacity_Fringe)))){
    DT_Opti[OPEC==F,Prod_Plateau := Prod_Plateau * 0.01 * as.numeric(sub('.+_(.+)', '\\1', Unit_Param$Production_Capacity_Fringe))]
  }
  
  # if((grepl("Plateau_|Hybrid_",Unit_Param$Production_Capacity_OPEC) & grepl("_Outage",Unit_Param$Production_Capacity_OPEC)) | (grepl("Plateau_|Hybrid_",Unit_Param$Production_Capacity_Fringe) & grepl("_Outage",Unit_Param$Production_Capacity_Fringe))){
  #   DT_Opti <- merge(DT_Opti,outages[!duplicated(paste(`RE ID`,Year)),c("RE ID","Year","Real_Prod_Cap")],by.x = c("ID_Unit","Year"),by.y = c("RE ID","Year"),all.x=T)
  #   DT_Opti[is.na(Real_Prod_Cap),Real_Prod_Cap:=0]
  #   DT_Opti[,Real_Prod_Cap := pmax(Real_Prod_Cap,Prod_Oil)]
  #   
  #   if(grepl("Plateau_|Hybrid_",Unit_Param$Production_Capacity_OPEC) & grepl("_Outage",Unit_Param$Production_Capacity_OPEC)) DT_Opti[order(-Real_Prod_Cap),Rank := 1:.N,by=ID_Unit][OPEC==T,Prod_Plateau := mean(Real_Prod_Cap[Rank <= round(pmax(sum(Real_Prod_Cap>0)/10,1))]),by = ID_Unit][,Rank := NULL]
  #   if(grepl("Plateau_|Hybrid_",Unit_Param$Production_Capacity_Fringe) & grepl("_Outage",Unit_Param$Production_Capacity_Fringe)) DT_Opti[order(-Real_Prod_Cap),Rank := 1:.N,by=ID_Unit][OPEC==F,Prod_Plateau := mean(Real_Prod_Cap[Rank <= round(pmax(sum(Real_Prod_Cap>0)/10,1))]),by = ID_Unit][,Rank := NULL]
  #   DT_Opti[,Real_Prod_Cap := NULL]
  # }
  #### Baseline plateau is here now (Real_Prod_Cap instead of Prod_Oil)
  if((grepl("Plateau_|Hybrid_",Unit_Param$Production_Capacity_OPEC) & grepl("_Outage",Unit_Param$Production_Capacity_OPEC)) | (grepl("Plateau_|Hybrid_",Unit_Param$Production_Capacity_Fringe) & grepl("_Outage",Unit_Param$Production_Capacity_Fringe))){
    DT_Opti <- merge(DT_Opti,outages[!duplicated(paste(`RE ID`,Year)),c("RE ID","Year","Real_Prod_Cap")],by.x = c("ID_Unit","Year"),by.y = c("RE ID","Year"),all.x=T)
    DT_Opti[is.na(Real_Prod_Cap),Real_Prod_Cap:=0]
    DT_Opti[,Real_Prod_Cap := pmax(Real_Prod_Cap,Prod_Oil)]
    
    if(grepl("Plateau_|Hybrid_",Unit_Param$Production_Capacity_OPEC) & grepl("_Outage",Unit_Param$Production_Capacity_OPEC)) DT_Opti[order(-Real_Prod_Cap),Rank := 1:.N,by=ID_Unit][OPEC==T,Prod_Plateau := mean(Real_Prod_Cap[Rank <= round(pmax(sum(Real_Prod_Cap>0)/10,1))]),by = ID_Unit][,Rank := NULL] # in the order before -Real_Prod_Cap, Year < 2025,
    if(grepl("Plateau_|Hybrid_",Unit_Param$Production_Capacity_Fringe) & grepl("_Outage",Unit_Param$Production_Capacity_Fringe)) DT_Opti[order(-Real_Prod_Cap),Rank := 1:.N,by=ID_Unit][OPEC==F,Prod_Plateau := mean(Real_Prod_Cap[Rank <= round(pmax(sum(Real_Prod_Cap>0)/10,1))]),by = ID_Unit][,Rank := NULL]
    DT_Opti[,Real_Prod_Cap := NULL]
  }
  
  ### Development lag #####
  if(Unit_Param$Development_Cost=="Separate"){
    if(Unit_Param$Lag_After_Development=="Asset_Specific") DT_Opti[,Lag_After_Development_Asset := as.numeric(cut(Reserves,breaks=c(0,10,250,5000,Inf),labels=seq(1,4,1))),] else DT_Opti[,Lag_After_Development_Asset := as.numeric(unlist(Unit_Param$Lag_After_Development)),]
    DT_Opti[Shale==F,Lag_After_Development_Asset := ifelse(sum(Facility_capex[Year >= `Start-up Year` - as.numeric(unlist(Unit_Param$Lag_After_Development)) + 1 & Year <= `Start-up Year` + unlist(Unit_Param$Limit_Development_Year) - 1] + Well_capex[Year >= `Start-up Year` - as.numeric(unlist(Unit_Param$Lag_After_Development)) + 1 & Year <= `Start-up Year` + unlist(Unit_Param$Limit_Development_Year) - 1])/Development_Cost > 0.5,0,Lag_After_Development_Asset), by = ID_Unit]
    DT_Opti[Development_Cost==0,Lag_After_Development_Asset:=0]
  }else{
    DT_Opti[,Lag_After_Development_Asset := 0,]
  }
  
  # Dvlpt level at start --> Allows for which prod ? (related to buildup)
  DT_Opti[Year <= Year_Start_Opti-1, Buildup_Value_Past := min(sum((Facility_capex + Well_capex)/Development_Cost * pmin((Year_Start_Opti - Year - 1 + Lag_After_Development_Asset)/(Buildup_Years+1),1)),1),by="RE ID"]
  DT_Opti[,Buildup_Value_Past := unique(Buildup_Value_Past[!is.na(Buildup_Value_Past)]),by="ID_Unit"]
  DT_Opti[Buildup_Value_Past < 1,Buildup_Value_Past := ifelse(Buildup_Value_Past * Prod_Plateau < Prod_Oil[Year == Year_Start_Opti-1],Prod_Oil[Year == Year_Start_Opti-1]/Prod_Plateau,Buildup_Value_Past),by=ID_Unit]
  
  ### Year selection - To reduce size of dataset
  
  DT_Opti <- DT_Opti[Year >= Year_Start_Opti & Year <= unlist(Unit_Param$Year_End_Future)]
  # Complete dataset with all missing years > Discovery Year + gap
  
  DT_Opti <- DT_Opti[Year >= ifelse(`Discovery Year` + unlist(Unit_Param$Lag_After_Discovery) > `Start-up Year`,`Start-up Year`,`Discovery Year` + unlist(Unit_Param$Lag_After_Discovery))]#[!(ID_Unit %in% DT_Opti[Bin_Cost==1 & (Extrac_cost + ifelse(`Start-up Year` > Year_Start_World,Dvpt_cost_per_bbl,0) > 100)]$ID_Unit)]
  setkeyv(DT_Opti,c("Bin_Cost","Year","ID_Unit")) #Main ordering of the table
  
  ### Creating variable "Prod_Impossible" #####
  
  # To gain some time (reducing nb of variables) - Identifying asset*years in which production is impossible (early production in bins > 1 f.i)
  DT_Opti[,Year_Min_Start := max(Year_Start_Opti,`Discovery Year` + unlist(Unit_Param$Lag_After_Discovery) + Lag_After_Development_Asset),by="RE ID"]
  DT_Opti[,Prod_Max_Path:=0]
  if(Unit_Param$Production_Capacity_OPEC=="Plateau") DT_Opti[OPEC==T & Year >= Year_Min_Start,Prod_Max_Path := Prod_Plateau,] else
    if(Unit_Param$Production_Capacity_OPEC=="Decline") for(year in min(DT_Opti$Year):max(DT_Opti$Year)) DT_Opti[OPEC==T & Year <= year & Year >= Year_Min_Start,Prod_Max_Path := ifelse(Year<year,Prod_Max_Path,(Reserves - cumsum(Prod_Max_Path)) * Decline_Rate),by=.(`RE ID`,Bin_Cost)] else
      if(Unit_Param$Production_Capacity_OPEC=="Hybrid") for(year in min(DT_Opti$Year):max(DT_Opti$Year)) DT_Opti[OPEC==T & Year <= year & Year >= Year_Min_Start,Prod_Max_Path := ifelse(Year<year,Prod_Max_Path,ifelse((Reserves - cumsum(Prod_Max_Path)) * Decline_Rate > Prod_Plateau,Prod_Plateau,(Reserves - cumsum(Prod_Max_Path)) * Decline_Rate)),by=.(`RE ID`,Bin_Cost)]
  for(bin in min(DT_Opti$Bin_Cost):max(DT_Opti$Bin_Cost)){
    DT_Opti[OPEC==T & Bin_Cost <= bin, Prod_Max_Path := ifelse(Bin_Cost < bin,Prod_Max_Path,Prod_Max_Path - sum(Prod_Max_Path * (Bin_Cost < bin),na.rm=T)),by=.(ID_Unit,Year)]
    DT_Opti[OPEC==T & Bin_Cost == bin, Prod_Max_Path := (ifelse(cumsum(Prod_Max_Path)<Bin_Reserves,cumsum(Prod_Max_Path),Bin_Reserves) - shift(cumsum(Prod_Max_Path),fill=0,type="lag")) * (shift(cumsum(Prod_Max_Path),fill=0) <= Bin_Reserves),by="ID_Duplicate"]
  }
  DT_Opti[OPEC==T & abs(Prod_Max_Path) < 1e-10,Prod_Max_Path := 0,]
  DT_Opti[OPEC==T,Prod_Impossible := cumsum(Prod_Max_Path)==0,by=.(`RE ID`,Bin_Cost)]
  if(!(Unit_Param$Production_Capacity_OPEC %in% c("Plateau","Decline","Hybrid"))) DT_Opti[OPEC==T & Year >= Year_Min_Start, Prod_Impossible := F]
  
  if(Unit_Param$Production_Capacity_Fringe=="Plateau") DT_Opti[OPEC==F & Year >= Year_Min_Start,Prod_Max_Path := Prod_Plateau,] else
    if(Unit_Param$Production_Capacity_Fringe=="Decline") for(year in min(DT_Opti$Year):max(DT_Opti$Year)) DT_Opti[OPEC==F & Year <= year & Year >= Year_Min_Start,Prod_Max_Path := ifelse(Year<year,Prod_Max_Path,(Reserves - cumsum(Prod_Max_Path)) * Decline_Rate),by=.(`RE ID`,Bin_Cost)] else
      if(Unit_Param$Production_Capacity_Fringe=="Hybrid") for(year in min(DT_Opti$Year):max(DT_Opti$Year)) DT_Opti[OPEC==F & Year <= year & Year >= Year_Min_Start,Prod_Max_Path := ifelse(Year<year,Prod_Max_Path,ifelse((Reserves - cumsum(Prod_Max_Path)) * Decline_Rate > Prod_Plateau,Prod_Plateau,(Reserves - cumsum(Prod_Max_Path)) * Decline_Rate)),by=.(`RE ID`,Bin_Cost)]
  for(bin in min(DT_Opti$Bin_Cost):max(DT_Opti$Bin_Cost)){
    DT_Opti[OPEC==F & Bin_Cost <= bin, Prod_Max_Path := ifelse(Bin_Cost < bin,Prod_Max_Path,Prod_Max_Path - sum(Prod_Max_Path * (Bin_Cost < bin),na.rm=T)),by=.(ID_Unit,Year)]
    DT_Opti[OPEC==F & Bin_Cost == bin, Prod_Max_Path := (ifelse(cumsum(Prod_Max_Path)<Bin_Reserves,cumsum(Prod_Max_Path),Bin_Reserves) - shift(cumsum(Prod_Max_Path),fill=0,type="lag")) * (shift(cumsum(Prod_Max_Path),fill=0) <= Bin_Reserves),by="ID_Duplicate"]
  }
  DT_Opti[OPEC==F & abs(Prod_Max_Path) < 1e-10,Prod_Max_Path := 0,]
  DT_Opti[OPEC==F,Prod_Impossible := cumsum(Prod_Max_Path)==0,by=.(`RE ID`,Bin_Cost)]
  if(!(Unit_Param$Production_Capacity_Fringe %in% c("Plateau","Decline","Hybrid"))) DT_Opti[OPEC==F & Year >= Year_Min_Start, Prod_Impossible := F]
  #ID_No_Res_Constraint <- unique(DT_Opti[,sum(Prod_Max_Path),by=ID_Duplicate])
  
  
  ### Adding the clean backstop (RE ID == "CBS0000") #####
  
  DT_Opti <- rbind(DT_Opti,data.table(Unit = "Clean backstop",`RE ID`="CBS0000",ID_Unit="CBS0000",ID_Duplicate="CBS0000_1",Asset="Clean backstop", OPEC=F,
                                      Country = "Clean backstop", Province = "Clean backstop",Area = "Clean backstop",Location = "Clean backstop",Project = "Clean backstop", `Fiscal Regime Group` = "Clean backstop", `Tax Model Name` = "Clean backstop",
                                      Year = Year_Start_Opti:unlist(Unit_Param$Year_End_Future), Reserves=Inf, Bin_Reserves = Inf, Development_Level_Start_Opti = 1, Prod_Plateau = Inf, Prod_Impossible = F),fill=T)
  DT_Opti[ID_Unit=="CBS0000"][is.na(DT_Opti[ID_Unit=="CBS0000"])] <- 0
  
  DT_Opti[,i_Asset := as.numeric(factor(ID_Unit,levels = unique(ID_Unit))),]
  DT_Opti[,i_Duplicate := as.numeric(factor(ID_Duplicate,levels = unique(ID_Duplicate))),]
  DT_Opti[,i_Asset_Year := as.numeric(factor(paste(ID_Unit,Year),levels = unique(paste(ID_Unit,Year)))),]
  DT_Opti[,i_Duplicate_Year := as.numeric(factor(paste(ID_Duplicate,Year),levels = unique(paste(ID_Duplicate,Year)))),]
  DT_Opti[,i_Year := as.numeric(factor(Year,levels = unique(Year))),]
  DT_Opti[,j_Matrix := .I,]
  
  ### Outputs #####
  # Table with ownership data
  own <- merge(ownership[Year %in% min(DT_Opti$Year):max(DT_Opti$Year)],
               temp_obs[!duplicated(`RE ID`),c("RE ID","Unit","Reserves")],by = "RE ID",all.x=T)[Reserves != 0 & !is.na(Reserves)]
  
  own <- own[,Share := sum(value * Reserves),by=.(Unit,`Historical Company`,Year)][,Share := Share / sum(value * Reserves),by=.(Unit,Year)][!duplicated(paste(Unit,Year,`Historical Company`)),-c("value","Reserves")]
  own <- merge(own[Unit %in% unique(DT_Opti$Unit)],DT_Opti[!duplicated(ID_Unit),c("Unit","ID_Unit")],by="Unit")
  fwrite(own[,c("ID_Unit","Unit","Historical Company","Year","Share")],paste0("Output/GP_All/Results/",Unit_Param$Version,"/Misc/Ownership.csv"))
  
  #Table with observed values
  obs <- merge(temp_obs,DT_Opti[!duplicated(ID_Unit),c("Unit","ID_Unit","Lag_After_Development_Asset","Development_Cost")],by="Unit",all.x=T)
  obs[,Development_Obs_One_Year := as.numeric(Year==min(`Start-up Year` - Lag_After_Development_Asset)),by=Unit]
  if(Unit_Param$Development_Cost=="Separate") obs[,Development_Obs := Development_Cost_Year * (Year < `Start-up Year` + unlist(Unit_Param$Limit_Development_Year)) / Development_Cost,][is.na(Development_Obs),Development_Obs := ifelse(Development_Obs_One_Year==1,1,0)] else obs[,Development_Obs := 0]
  
  obs <- merge(obs[!duplicated(paste(Unit,Year)),c("ID_Unit","Unit","Year","Development_Obs_One_Year","Development_Cost")],
               obs[,lapply(.SD,sum),by = .(Unit,Year), .SDcols = c("Prod_Obs","Development_Obs","Abandonment_cost","Bonuses","Exploration_capex","Facility_capex","Government_profit","Income_tax","Production_opex","Royalty_effects","SGA_opex","Taxes_opex","Transportation_opex","Well_capex")], by = c("Unit","Year"))
  obs <- merge(obs,DT_Opti[!duplicated(ID_Unit),c("Unit","theta")],by="Unit")
  #obs <- merge(obs,temp[,c("Unit","Year","Extrac_cost_Obs")],by=c("Unit","Year"),all.x=T)
  obs$Extrac_cost_Obs <- (rowSums(obs[,.SD,.SDcols=unlist(Unit_Param$Costs_Included)]) - obs[,Development_Obs * Development_Cost]) / obs$Prod_Obs
  if(Unit_Param$Capital_Depreciation==T) obs$Extrac_cost_Obs <- (rowSums(obs[,.SD,.SDcols=unlist(Unit_Param$Costs_Included)[grepl("Facility|Well",unlist(Unit_Param$Costs_Included))==F]])) / obs$Prod_Obs
  obs[,c("Taxes_Opex_Obs","Royalty_Obs","Gvt_Profit_Obs","Income_Tax_Obs") := list(Taxes_opex,Royalty_effects,Government_profit,Income_tax)]
  obs <- obs[Prod_Obs!=0 | Development_Obs_One_Year!=0 | Development_Obs!=0 | Extrac_cost_Obs!=0 | Taxes_opex != 0| Royalty_effects!=0 | Government_profit != 0 | Income_tax!=0 & Year >= Year_Start_Opti & Year <= unlist(Unit_Param$Year_End_Future),c("ID_Unit","Unit","Year","Development_Cost","theta","Prod_Obs","Development_Obs","Development_Obs_One_Year","Extrac_cost_Obs","Taxes_Opex_Obs","Royalty_Obs","Gvt_Profit_Obs","Income_Tax_Obs")]
  fwrite(obs,paste0("Output/GP_All/Results/",Unit_Param$Version,"/Misc/Observed.csv"))
  
  DT_Opti <- merge(DT_Opti,obs[,c("ID_Unit","Year","Development_Obs")],by = c("ID_Unit","Year"),all.x=T)[is.na(Development_Obs),Development_Obs := 0]
  
  fwrite(DT_Opti[,c("Unit","Year","OPEC","Shale","Shale_Play","Discovery Year","Start-up Year","Reserves","Development_Cost","Delta_K","Decline_Rate","Buildup_Years","Buildup_Value_Past","Extrac_cost","Development_Level_Start_Opti","theta","theta_Share_Downstream","Unit_Extrac_cost","ID_Unit","ID_Duplicate","Bin_Cost","Bin_Reserves","Prod_bin","Development_Obs","Prod_Plateau","Lag_After_Development_Asset","Prod_Impossible","i_Asset","i_Duplicate","i_Asset_Year","i_Duplicate_Year","i_Year","j_Matrix")],
         paste0("Output/GP_All/Results/",Unit_Param$Version,"/Misc/DT_Opti.csv"))
  
  #detach(Unit_Param)
  
  return(DT_Opti)
}

##### Function - Model preparation #####
Model_Preparation <- function(Unit_Param,Scenario_Param,Model_Param,Subset){
  
  ##### Preparation #####
  DT_Opti <- fread(paste0("Output/GP_All/Results/",unlist(Unit_Param$Version),"/Misc/DT_Opti.csv"))[order(j_Matrix)]
  DT_Opti[,OPEC_Plus := ID_Unit %in% asset_lvl[`OPEC Plus`=="OPEC+"]$`RE ID`]
  if(!is.na(Subset)){
    DT_Opti <- DT_Opti[ID_Unit %in% unlist(Subset)]
    DT_Opti[,i_Asset := as.numeric(factor(ID_Unit,levels = unique(ID_Unit))),]
    DT_Opti[,i_Duplicate := as.numeric(factor(ID_Duplicate,levels = unique(ID_Duplicate))),]
    DT_Opti[,i_Asset_Year := as.numeric(factor(paste(ID_Unit,Year),levels = unique(paste(ID_Unit,Year)))),]
    DT_Opti[,i_Duplicate_Year := as.numeric(factor(paste(ID_Duplicate,Year),levels = unique(paste(ID_Duplicate,Year)))),]
    DT_Opti[,i_Year := as.numeric(factor(Year,levels = unique(Year))),]
    DT_Opti[,j_Matrix := .I,]
  } 
  #DT_Opti[,Prod_Impossible := F]
  DT_Opti[Development_Cost > 0 & Development_Cost < 1e-5,Development_Cost := 1e-5]
  
  attach(Scenario_Param,warn.conflicts = F)
  attach(Unit_Param,warn.conflicts = F)
  Year_Start_Opti <- min(DT_Opti$Year)
  Year_End_Future <- max(DT_Opti$Year)
  Demand_Year_Future <- 2019
  
  #Annual_Elasticity <- Annual_Elasticity_Setup[Year >= Year_Start_Opti & Year <= unlist(Year_End_Future)]
  #Annual_Elasticity[,Elasticity := Annual_Elasticity_Setup$Elasticity[1:nrow(Annual_Elasticity)]]
  Annual_Elasticity <- copy(Annual_Elasticity_Setup[Year >= 1900])
  
  #Creating dataset with assets that need to be dvlped in order to produce
  #DT_Opti_Dvpt <- DT_Opti[ID_Unit %in% DT_Opti[Development_Cost_Start_Opti > 0]$ID_Unit]
  if(Unit_Param$Development_Cost=="Separate"){
    DT_Opti_Dvpt <- DT_Opti[Development_Level_Start_Opti < ifelse(Unit_Param$Capital_Depreciation == T,Inf,1) | Buildup_Value_Past < 1]
    DT_Opti_Dvpt <- DT_Opti_Dvpt[,j_Matrix_Prod := Reduce(paste,as.character(j_Matrix)),by=i_Asset_Year][,Nb_bins := max(Bin_Cost),][Bin_Cost==1] #for multiple bins
    DT_Opti_Dvpt[,j_Matrix_Dvpt := .I + max(DT_Opti$j_Matrix)]
    DT_Opti_Dvpt[,i_Asset_Dvpt := as.numeric(factor(ID_Unit,levels = unique(ID_Unit)))]
    DT_Opti_Dvpt[,j_Matrix_Prior_Dvpt := max(j_Matrix_Dvpt) + as.numeric(factor(ID_Unit, levels = unique(ID_Unit)))]
  }else{
    DT_Opti_Dvpt <- DT_Opti[0]
  }
  #Assigning to clean backstop its cost
  DT_Opti[ID_Unit=="CBS0000", Extrac_cost := unlist(Scenario_Param$Cost_Clean_Backstop)]
  
  # Taking into account tax included
  if("Government_profit" %in% unlist(Unit_Param$Costs_Included) | "Income_tax" %in% unlist(Unit_Param$Costs_Included)) DT_Opti[Extrac_cost < 1e-6,Extrac_cost := 1e-6] #This will be removed, not used anymore
  
  # Tax / Price discount distortions: increase extrac_cost in the optimization c_it / (1 - Royalty rate)
  if(unlist(Distortions_Opti)!="None"){
    Tax_Avg <- fread(paste0("Output/GP_All/Results/",unlist(Unit_Param$Version),"/Misc/Tax_Averages.csv"))[Discount_Price <= 0.1,Discount_Price := 0.1]
    DT_Opti <- merge(DT_Opti,Tax_Avg[,-"Unit"], by = "ID_Unit",all.x=T)[ID_Unit=="CBS0000",c("Tax_Opex_Avg","Royalty_Avg","Gvt_Profit_Avg","Income_Tax_Avg","Discount_Price") := list(0,0,0,0,1)]
    
    DT_Opti[, Extrac_cost := Extrac_cost + Tax_Opex_Avg] #Always included if Distortions_Opti != "None"
    if(grepl("PriceDiscount",unlist(Distortions_Opti))) DT_Opti[, Extrac_cost_Multiplier := 1 / Discount_Price]
    if(grepl("Royalty",unlist(Distortions_Opti))) DT_Opti[, Extrac_cost_Multiplier := 1 / (1 - Royalty_Avg)]
    
    DT_Opti_Dvpt <- merge(DT_Opti_Dvpt,Tax_Avg[,-"Unit"], by = "ID_Unit",all.x=T)[ID_Unit=="CBS0000",c("Tax_Opex_Avg","Royalty_Avg","Gvt_Profit_Avg","Income_Tax_Avg","Discount_Price") := list(0,0,0,0,1)]
    if(grepl("Profits_Tax",unlist(Distortions_Opti))) DT_Opti_Dvpt[, Dvpt_cost_Multiplier := 1 / (1 - Gvt_Profit_Avg) / (1 - Income_Tax_Avg)]
    
    DT_Opti[,c("Tax_Opex_Avg","Royalty_Avg","Gvt_Profit_Avg","Income_Tax_Avg","Discount_Price") := NULL]
    DT_Opti_Dvpt[,c("Tax_Opex_Avg","Royalty_Avg","Gvt_Profit_Avg","Income_Tax_Avg","Discount_Price") := NULL]
    DT_Opti <- DT_Opti[order(i_Asset_Year)]
    DT_Opti_Dvpt <- DT_Opti_Dvpt[order(i_Asset_Year)]
  }
  
  model <- list()
  model$varnames <- DT_Opti[, paste("Prod", ID_Duplicate, Year, sep = "_")]
  model$lb <- rep(0,nrow(DT_Opti))
  model$ub <- rep(Inf,nrow(DT_Opti))
  model$modelsense <- "min"
  model$obj <- vector("list", 1)
  #model$obj <- DT_Opti[,exp(-unlist(rho) * (Year - unlist(Year_Start_Future))) * Extrac_cost] + 1e-3 * DT_Opti$theta * unlist(SCC)
  
  ##### Objective - Cost #####
  
  # Objective 1 - cost component
  model$obj <- DT_Opti[,exp(-unlist(rho) * (Year - Year_Start_Opti)) * Extrac_cost]
  
  # Objective 1 - Multiplying if royalty rate included --> Applies to gross revenues deducting carbon tax
  if(grepl("Royalty_CTD",unlist(Distortions_Opti))) model$obj <- model$obj * DT_Opti$Extrac_cost_Multiplier
  
  # Objective 1 - theta component
  if(Policy=="SCC") model$obj <- model$obj + 1e-3 * DT_Opti$theta * Policy_Values[[1]]
  if(Policy=="Uniform") model$obj[which(DT_Opti$ID_Unit!="CBS0000")] <- model$obj[which(DT_Opti$ID_Unit!="CBS0000")] + Policy_Values[[1]]
  
  # Objective 1 - Multiplying if royalty rate included (or price discount) --> Applies to gross revenues (before carbon tax is collected)
  if(grepl("Royalty|PriceDiscount",unlist(Distortions_Opti)) & grepl("CTD",unlist(Distortions_Opti))==F) model$obj <- model$obj * DT_Opti$Extrac_cost_Multiplier
  if(grepl("Royalty|PriceDiscount",unlist(Distortions_Opti))) DT_Opti[,Extrac_cost_Multiplier := NULL]
  
  # Objective 2 - Environmental among optima
  #model$multiobj[[2]]$objn <- DT_Opti$theta
  #model$multiobj[[1]]$priority <- 1
  #model$multiobj[[2]]$priority <- 0
  
  model$vtype <- rep("C",length(model$obj))
  
  ### Constraints: 
  
  ##### Constraints - Carbon budget #####
  if(Policy=="Carbon budget"){
    model <- Add_Constraint(model = model,
                            i = rep(1,nrow(DT_Opti)),
                            j = DT_Opti$j_Matrix,
                            x = DT_Opti$theta * 1e-3,
                            rhs = Policy_Values[[1]] * 1000,
                            dim_i = 1,
                            sense = "<=",
                            constrnames = "___Carbon.Budget")
  }
  
  ##### Constraints - Reserves #####
  if(Reserve_Definition != "Resources_Oil_Infinite"){
    model <- Add_Constraint(model = model,
                            i = DT_Opti$i_Duplicate,
                            j = DT_Opti$j_Matrix,
                            x = 1,
                            rhs = DT_Opti[!duplicated(i_Duplicate)]$Bin_Reserves,
                            dim_i = max(DT_Opti$i_Duplicate),
                            sense = "<=",
                            constrnames = paste(DT_Opti[!duplicated(i_Duplicate)]$ID_Duplicate,"","Reserve",sep="_"))
  }
  
  ##### Constraints - Prod capacity - Plateau #####
  if(grepl("Hybrid|Plateau|Observed",Production_Capacity_OPEC)){
    DT_Opti[,Prod_Plateau_Constraint := Prod_Plateau]
    DT_Opti[Prod_Plateau_Constraint < 0,Prod_Plateau_Constraint := 0] # Don't know why, sometime small values around -1e-6
    
    if(sum(grepl("Regulatory_Outage",names(DT_Opti)))==0) DT_Opti[,Regulatory_Outage := 0]
    names(DT_Opti)[grepl("Regulatory_Outage",names(DT_Opti))] <- "Regulatory_Outage"
    
    model$ub[DT_Opti[OPEC==T]$j_Matrix] <- DT_Opti[OPEC==T]$Prod_Plateau_Constraint * 1.5
    model <- Add_Constraint(model = model,
                            i = DT_Opti[OPEC==T]$i_Asset_Year,
                            j = DT_Opti[OPEC==T]$j_Matrix,
                            x = 1,
                            rhs = DT_Opti[Bin_Cost==1 | ID_Unit=="CBS0000"]$Prod_Plateau_Constraint,
                            dim_i = max(DT_Opti$i_Asset_Year),
                            sense = "<=",
                            constrnames = paste(DT_Opti[Bin_Cost==1 | ID_Unit=="CBS0000"]$ID_Unit,1,DT_Opti[Bin_Cost==1 | ID_Unit=="CBS0000"]$Year,"Prod.Cap.Plateau",sep="_"))
  }
  if(grepl("Hybrid|Plateau|Observed",Production_Capacity_Fringe)){

    model$ub[DT_Opti[OPEC==F]$j_Matrix] <- DT_Opti[OPEC==F]$Prod_Plateau_Constraint * 1.5
    model <- Add_Constraint(model = model,
                            i = DT_Opti[OPEC==F]$i_Asset_Year,
                            j = DT_Opti[OPEC==F]$j_Matrix,
                            x = 1,
                            rhs = DT_Opti[Bin_Cost==1 | ID_Unit=="CBS0000"]$Prod_Plateau_Constraint,
                            dim_i = max(DT_Opti$i_Asset_Year),
                            sense = "<=",
                            constrnames = paste(DT_Opti[Bin_Cost==1 | ID_Unit=="CBS0000"]$ID_Unit,1,DT_Opti[Bin_Cost==1 | ID_Unit=="CBS0000"]$Year,"Prod.Cap.Plateau",sep="_"))
  }
  DT_Opti[,Prod_Plateau_Constraint := NULL]
  
  ##### Constraints - Prod Capacity - Decline #####
  #DT_Opti[ID_Unit == "CBS0000",Decline_Rate := 1]
  if(grepl("Hybrid|Decline|Observed",Production_Capacity_OPEC)){
    DT_Past_Prod <- DT_Opti[Bin_Cost==1 & OPEC==T]
    DT_Past_Prod[,Past_Prod_Index := Reduce(paste,j_Matrix,accumulate = T),by=ID_Unit][,Past_Prod_Index := shift(Past_Prod_Index,fill=""),by=ID_Unit]#[,Past_Prod_Index := str_remove(Past_Prod_Index, as.character(j_Matrix)),]
    #DT_Past_Prod[,Past_Prod_Index := unique(Past_Prod_Index)[1],by=i_Asset_Year]
    DT_Past_Prod[,Past_Prod_Count := str_count(Past_Prod_Index," ") + 1,][Past_Prod_Index=="",Past_Prod_Count:=0,]
    
    j <- as.numeric(unlist(strsplit(DT_Past_Prod[,Past_Prod_Index]," ",fixed = T)))
    i <- rep(DT_Past_Prod[,i_Asset_Year],DT_Past_Prod[,Past_Prod_Count])
    x <- rep(1,length(i))
    j <- c(j,DT_Past_Prod$j_Matrix)
    i <- c(i,DT_Past_Prod$i_Asset_Year)
    
    x <- c(x,DT_Past_Prod[,1/Decline_Rate])
    

    if(Bins == T){
      for(bin in 2:max(DT_Opti$Bin_Cost)) j <- c(j,j[1:length(i)] + nrow(DT_Past_Prod) * (bin-1))
      i <- rep(i,max(DT_Opti$Bin_Cost))
      x <- rep(x,max(DT_Opti$Bin_Cost))
    }
    
    rhs <- DT_Opti[Bin_Cost==1 | ID_Unit=="CBS0000"][order(i_Asset_Year),Reserves]
    
    model <- Add_Constraint(model = model,
                            i = i,
                            j = j,
                            x = x,
                            rhs = rhs,
                            dim_i = nrow(DT_Opti[Bin_Cost==1 | ID_Unit=="CBS0000"]),
                            sense = "<=",
                            constrnames = paste(DT_Opti[Bin_Cost==1 | ID_Unit=="CBS0000"]$ID_Unit,1,DT_Opti[Bin_Cost==1 | ID_Unit=="CBS0000"]$Year,"Prod.Cap.Decline","TEMP_OPEC",sep="_"))
    rm(i,j,x,rhs)
  }
  if(grepl("Hybrid|Decline|Observed",Production_Capacity_Fringe)){
    DT_Past_Prod <- DT_Opti[Bin_Cost==1 & OPEC==F]
    DT_Past_Prod[,Past_Prod_Index := Reduce(paste,j_Matrix,accumulate = T),by=ID_Unit][,Past_Prod_Index := shift(Past_Prod_Index,fill=""),by=ID_Unit]#[,Past_Prod_Index := str_remove(Past_Prod_Index, as.character(j_Matrix)),]
    #DT_Past_Prod[,Past_Prod_Index := unique(Past_Prod_Index)[1],by=i_Asset_Year]
    DT_Past_Prod[,Past_Prod_Count := str_count(Past_Prod_Index," ") + 1,][Past_Prod_Index=="",Past_Prod_Count:=0,]
    
    j <- as.numeric(unlist(strsplit(DT_Past_Prod[,Past_Prod_Index]," ",fixed = T)))
    i <- rep(DT_Past_Prod[,i_Asset_Year],DT_Past_Prod[,Past_Prod_Count])
    x <- rep(1,length(i))
    j <- c(j,DT_Past_Prod$j_Matrix)
    i <- c(i,DT_Past_Prod$i_Asset_Year)
    
    if(grepl("Outage",Production_Capped) & grepl("OutageInvoluntaryPast",Production_Capped)==F){
      DT_Past_Prod[Regulatory_Outage==1,c("Regulatory_Outage","Reserves") := list(0,0)]
      x <- c(x,DT_Past_Prod[,1/(Decline_Rate * (1 - as.numeric(Year < Year_Limit) * Regulatory_Outage))])
    }else{
      x <- c(x,DT_Past_Prod[,1/Decline_Rate])
    }
    
    if(Bins == T){
      for(bin in 2:max(DT_Opti$Bin_Cost)) j <- c(j,j[1:length(i)] + nrow(DT_Past_Prod) * (bin-1))
      i <- rep(i,max(DT_Opti$Bin_Cost))
      x <- rep(x,max(DT_Opti$Bin_Cost))
    }
    
    rhs <- DT_Opti[Bin_Cost==1 | ID_Unit=="CBS0000"][order(i_Asset_Year),Reserves]
    
    model <- Add_Constraint(model = model,
                            i = i,
                            j = j,
                            x = x,
                            rhs = rhs,
                            dim_i = nrow(DT_Opti[Bin_Cost==1  | ID_Unit=="CBS0000"]),
                            sense = "<=",
                            constrnames = paste(DT_Opti[Bin_Cost==1 | ID_Unit=="CBS0000"]$ID_Unit,1,DT_Opti[Bin_Cost==1 | ID_Unit=="CBS0000"]$Year,"Prod.Cap.Decline","TEMP_FRINGE",sep="_"))
    rm(i,j,x,rhs)
  }
  
  Empty_rows_index <- model$constrnames[which(rowSums(model$A!=0)==0)]
  if(length(Empty_rows_index) > 1) model <- Remove_Constraint(model=model,constrnames = Empty_rows_index)
  model$constrnames <- gsub("_TEMP_OPEC|_TEMP_FRINGE","",model$constrnames)
  
  ##### Constraints - Prod Capacity - Observed path #####
  if(Production_Capacity_OPEC == "Observed") model$ub[DT_Opti[OPEC==T & Year <= Year_Start_Opti]$j_Matrix] <- pmin(DT_Opti[OPEC==T & Year <= Year_Start_Opti]$Prod_bin,model$ub[DT_Opti[OPEC==T & Year <= Year_Start_Opti]$j_Matrix])
  if(Production_Capacity_Fringe == "Observed") model$ub[DT_Opti[OPEC==F & Year <= Year_Start_Opti & DT_Opti$ID_Unit!="CBS0000"]$j_Matrix] <- pmin(DT_Opti[OPEC==F & Year <= Year_Start_Opti & ID_Unit!="CBS0000"]$Prod_bin,model$ub[DT_Opti[OPEC==F & Year <= Year_Start_Opti & DT_Opti$ID_Unit!="CBS0000"]$j_Matrix])
  
  ##### Constraints - Development prior to production start #####
  if(Development_Cost=="Separate"){
    
    #Uniform tax on investment
    if(Policy=="ITU") DT_Opti_Dvpt[,Development_Cost := Development_Cost * (1 + unlist(Policy_Values[[1]]))]
    #CI based tax on investment (tax * quantity of tCO2eq in the reservoir)
    if(Policy=="ITCI") DT_Opti_Dvpt[,Development_Cost := Development_Cost + Reserves * theta * 1e-3 * unlist(Policy_Values[[1]])]
    
    model <- Add_Variable(model = model,
                          newvar = DT_Opti_Dvpt[,exp(-unlist(rho) * (Year - Year_Start_Opti)) * Development_Cost], 
                          newvarnames = paste("Development",DT_Opti_Dvpt$ID_Unit,"1",DT_Opti_Dvpt$Year,sep="_"),
                          vtype = "C")
    model$lb[which(grepl("Development",model$varnames))] <- 0
    model$ub[which(grepl("Development",model$varnames))] <- 1
    
    if(grepl("Profits_Tax",unlist(Distortions_Opti))){
      model$obj[which(grepl("Development",model$varnames))] <- model$obj[which(grepl("Development",model$varnames))] * DT_Opti_Dvpt[,Dvpt_cost_Multiplier]
    }
    
    model <- Add_Variable(model = model,
                          newvar = rep(0,uniqueN(DT_Opti_Dvpt$j_Matrix_Prior_Dvpt)),
                          newvarnames = paste("Development",DT_Opti_Dvpt[!duplicated(j_Matrix_Prior_Dvpt)]$ID_Unit,"1","Past",sep="_"),
                          vtype = "C")
    model$lb[which(grepl("Development",model$varnames) & grepl("Past",model$varnames))] <- DT_Opti_Dvpt[!duplicated(j_Matrix_Prior_Dvpt)][,Development_Level_Start_Opti]
    model$ub[which(grepl("Development",model$varnames) & grepl("Past",model$varnames))] <- DT_Opti_Dvpt[!duplicated(j_Matrix_Prior_Dvpt)][,Development_Level_Start_Opti]
    
    if(Incomplete_Development == F){
      model <- Add_Variable(model = model,
                            newvar = rep(0,uniqueN(DT_Opti_Dvpt$ID_Unit)),
                            newvarnames = paste("Binary.Sum.Dvpt",DT_Opti_Dvpt[!duplicated(ID_Unit)]$ID_Unit,"1","1",sep="_"),
                            vtype = "B")
      model$lb[which(grepl("Binary.Sum.Dvpt",model$varnames))] <- 0
      model$ub[which(grepl("Binary.Sum.Dvpt",model$varnames))] <- 1
      
      model <- Add_Constraint(model = model,
                              i = c(DT_Opti_Dvpt$i_Asset_Dvpt,unique(DT_Opti_Dvpt$i_Asset_Dvpt),unique(DT_Opti_Dvpt$i_Asset_Dvpt)),
                              j = c(DT_Opti_Dvpt$j_Matrix_Dvpt,which(grepl("Development",model$varnames) & grepl("Past",model$varnames)),which(grepl("Binary.Sum.Dvpt",model$varnames))),
                              x = c(rep(1,nrow(DT_Opti_Dvpt) + uniqueN(DT_Opti_Dvpt$j_Matrix_Prior_Dvpt)),rep(-1,uniqueN(DT_Opti_Dvpt$ID_Unit))),
                              rhs = rep(0,uniqueN(DT_Opti_Dvpt$ID_Unit)),
                              dim_i = uniqueN(DT_Opti_Dvpt$ID_Unit),
                              sense = "=",
                              constrnames = paste(DT_Opti_Dvpt[!duplicated(ID_Unit)]$ID_Unit,1,"","Sum.Development",sep="_"))
    }
    
    if(Progressive_Development == F){
      SOS_Constraints <- DT_Opti_Dvpt[,c("ID_Unit","j_Matrix_Dvpt")]
      SOS_Constraints[,SOS_Index := j_Matrix_Dvpt]
    }
    
    #Selecting each combination of i_Asset_Year, and withdrawing assets which development is assured (happened before optim period)
    #DT_Opti_Dvpt[,Development_Lag := min(`Start-up Year` - Year_Start_Opti,Lag_After_Development_Asset),by=ID_Unit]
    DT_Opti_Dvpt[,Past_Dvpt_Index := Reduce(paste,j_Matrix_Dvpt,accumulate = T),by=ID_Unit]
    for(dvpt_lag in 0:max(DT_Opti_Dvpt$Lag_After_Development_Asset)) DT_Opti_Dvpt[Lag_After_Development_Asset==dvpt_lag, Past_Dvpt_Index := shift(Past_Dvpt_Index,n = dvpt_lag,fill=""),by=ID_Unit]
    DT_Opti_Dvpt[,Past_Dvpt_Index := gsub(" NA","",Past_Dvpt_Index)]
    DT_Opti_Dvpt[,Past_Dvpt_Index := ifelse(Past_Dvpt_Index=="",as.character(j_Matrix_Prior_Dvpt),paste(j_Matrix_Prior_Dvpt,Past_Dvpt_Index,sep = " "))]
    
    DT_Opti_Dvpt[,Past_Dvpt_Count := str_count(Past_Dvpt_Index," ") + 1,]
    
    # Constructing x (with coefficients for last years of development)
    DT_Opti_Dvpt[,Value := as.character(round(pmin(1:(.N)/(Buildup_Years+1),1),3)),by=ID_Unit]
    
    DT_Opti_Dvpt[Buildup_Value_Past < 1/(Buildup_Years+1),Buildup_Value_Past := 1/(Buildup_Years+1)][is.na(Buildup_Value_Past),Buildup_Value_Past := 0]
    DT_Opti_Dvpt[,Past_Dvpt_Coef := as.character(round(pmin(Buildup_Value_Past + (Year - Year_Start_Opti + 1 - Lag_After_Development_Asset)/(Buildup_Years+1),1),3)),]
    DT_Opti_Dvpt[Lag_After_Development_Asset==0, Past_Dvpt_Coef := paste(Past_Dvpt_Coef,Value)]
    
    for(t in 1:(max(DT_Opti_Dvpt$Year) - Year_Start_Opti)){
      DT_Opti_Dvpt[,Value := shift(Value,fill=""), by = "ID_Unit"]
      DT_Opti_Dvpt[, Past_Dvpt_Coef := paste(Past_Dvpt_Coef,Value)]
    }
    DT_Opti_Dvpt[,Past_Dvpt_Coef := gsub("\\s+", " ",str_trim(Past_Dvpt_Coef))]

    j <- as.numeric(unlist(strsplit(DT_Opti_Dvpt[,Past_Dvpt_Index]," ",fixed = T)))
    i <- rep(DT_Opti_Dvpt[,i_Asset_Year],DT_Opti_Dvpt[,Past_Dvpt_Count])
    if(Unit_Param$Capital_Depreciation==T){
      DT_Opti_Dvpt[,Min_Year_Dvpt_Asset := min(Year),by = "ID_Unit"]
      DT_Opti_Dvpt[is.na(Delta_K),Delta_K := 0]
      DT_Opti_Dvpt[,Past_Dvpt_Delta := paste((1-Delta_K) ^ seq(Year-Min_Year_Dvpt_Asset,Lag_After_Development_Asset-1,-1),collapse = " "),by = .(j_Matrix_Dvpt)]
      x <- as.numeric(unlist(strsplit(DT_Opti_Dvpt[,Past_Dvpt_Delta]," ",fixed = T)))
    }else{
      x <- as.numeric(unlist(strsplit(DT_Opti_Dvpt[,Past_Dvpt_Coef]," ",fixed = T)))
    }

    
    j <- c(j,as.numeric(unlist(strsplit(DT_Opti_Dvpt[,j_Matrix_Prod]," ",fixed = T))))
    i <- c(i,rep(DT_Opti_Dvpt[,i_Asset_Year],DT_Opti_Dvpt[,Nb_bins]))
    x <- c(x,rep(-1/DT_Opti_Dvpt[,Prod_Plateau],DT_Opti_Dvpt[,Nb_bins]))
    
    rhs <- rep(0,nrow(DT_Opti[Bin_Cost==1]))
    
    model <- Add_Constraint(model = model,
                            i = i,
                            j = j,
                            x = x,
                            rhs = rhs,
                            dim_i = nrow(DT_Opti[Bin_Cost==1]),
                            sense = ">=",
                            constrnames = paste(DT_Opti[Bin_Cost==1]$ID_Unit,1,DT_Opti[Bin_Cost==1]$Year,"Development",sep="_"))
    
    if(Progressive_Development==T & Incomplete_Development==T & Capital_Depreciation == F){
      # Limiting dvpt to 1
      model <- Add_Constraint(model = model,
                              i = c(DT_Opti_Dvpt$i_Asset_Dvpt,DT_Opti_Dvpt[!duplicated(j_Matrix_Prior_Dvpt)]$i_Asset_Dvpt),
                              j = c(DT_Opti_Dvpt$j_Matrix_Dvpt,DT_Opti_Dvpt[!duplicated(j_Matrix_Prior_Dvpt)]$j_Matrix_Prior_Dvpt),
                              x = 1,
                              rhs = rep(1,max(DT_Opti_Dvpt$i_Asset_Dvpt)),
                              dim_i = max(DT_Opti_Dvpt$i_Asset_Dvpt),
                              sense = "<=",
                              constrnames = paste(DT_Opti_Dvpt[!duplicated(i_Asset_Dvpt)]$ID_Unit,1,"","Development.Opti",sep="_"))
    }
    
  }
  
  DT_Opti_Dvpt[Development_Obs<0,Development_Obs:=0][Development_Obs>1,Development_Obs:=1]#[,Development_Obs := Development_Obs-1e-10]
  ### Fixed dvpt if Development_OPEC == "Observed"
  if(Development_OPEC=="Observed"){
    model$lb[DT_Opti_Dvpt[OPEC==T & Year %in% Year_Start_Opti:2021]$j_Matrix_Dvpt] <- DT_Opti_Dvpt[OPEC==T & Year %in% Year_Start_Opti:2021]$Development_Obs
    model$ub[DT_Opti_Dvpt[OPEC==T & Year %in% Year_Start_Opti:2021]$j_Matrix_Dvpt] <- DT_Opti_Dvpt[OPEC==T & Year %in% Year_Start_Opti:2021]$Development_Obs
  }
  if(Development_Fringe=="Observed"){
    model$lb[DT_Opti_Dvpt[OPEC==F & Year %in% Year_Start_Opti:2021 & ID_Unit!="CBS0000"]$j_Matrix_Dvpt] <- DT_Opti_Dvpt[OPEC==F & Year %in% Year_Start_Opti:2021 & ID_Unit!="CBS0000"]$Development_Obs
    model$ub[DT_Opti_Dvpt[OPEC==F & Year %in% Year_Start_Opti:2021 & ID_Unit!="CBS0000"]$j_Matrix_Dvpt] <- DT_Opti_Dvpt[OPEC==F & Year %in% Year_Start_Opti:2021 & ID_Unit!="CBS0000"]$Development_Obs
  }
  
  ##### Policy constraint - Production forbidden in polluting assets #####
  if(Scenario_Param$Policy == "NPP"){
    Scenario_Tweak <- sub("^[^_]*_","",sub("(.*)_.*", "\\1", Scenario_Param$Scenario))
    if(Scenario_Tweak==Scenario_Param$Policy) Scenario_Tweak <- "" else Scenario_Tweak <- paste0(Scenario_Tweak,"_")
    scc0 <- fread(paste0("Output/GP_All/Results/",unlist(Unit_Param$Version),"/SCC_",Scenario_Tweak,"0_Results.csv"))[Prod > 0 & ID_Unit!="CBS0000"]
    cutoff <- wtd.quantile(scc0$theta,scc0$Prod,
                           1 - 0.01 * unlist(Policy_Values[[1]]))
    model$ub[which(DT_Opti[,theta > cutoff])] <- 0
    model$lb[which(DT_Opti[,theta > cutoff])] <- 0
  }
  ##### Policy constraint - Investments banned #####
  
  model$lb[which(grepl("Development",model$varnames) & grepl("Past",model$varnames)==F)][DT_Opti_Dvpt$Year >= End_Year_Investment] <- 0
  model$ub[which(grepl("Development",model$varnames) & grepl("Past",model$varnames)==F)][DT_Opti_Dvpt$Year >= End_Year_Investment] <- 0
  
  if(Scenario_Param$Policy == "NPI"){
    Scenario_Tweak <- sub("^[^_]*_","",sub("(.*)_.*", "\\1", Scenario_Param$Scenario))
    if(Scenario_Tweak==Scenario_Param$Policy) Scenario_Tweak <- "" else Scenario_Tweak <- paste0(Scenario_Tweak,"_")
    scc0 <- fread(paste0("Output/GP_All/Results/",unlist(Unit_Param$Version),"/SCC_",Scenario_Tweak,"0_Results.csv"))[Prod > 0 & ID_Unit!="CBS0000"]
    cutoff <- wtd.quantile(scc0$theta,scc0$Prod,
                           1 - unlist(Policy_Values[[1]]))
    model$ub[DT_Opti_Dvpt[theta > cutoff]$j_Matrix_Dvpt] <- 0
    model$lb[DT_Opti_Dvpt[theta > cutoff]$j_Matrix_Dvpt] <- 0
  }
  
  ##### Demand - Constraints/PWL #####
  if(Demand=="Inelastic"){
    if(is.numeric(unlist(Demand_Ref))){
      Annual_Elasticity[, c("Q_ref","P_ref") := list(unlist(Demand_Ref)[1],unlist(Demand_Ref)[2])]
    }else if(Demand_Ref=="Annual"){
      Annual_Elasticity <- merge(Annual_Elasticity[,c("Year","Elasticity","Elasticity_2","Elasticity_3")],Annual_Ref,by="Year")
    }else if(Demand_Ref=="Observed"){
      Annual_Elasticity[,Q_ref := df_year_full[order(Year)][Year %in% Annual_Elasticity$Year,.SD[,sum(Prod_Oil)],by=Year]$V1]
      Annual_Elasticity[,Q_ref := ifelse(Year >= Year_Start_Opti,sum(df_year_full[Year==Demand_Year_Future]$Prod_Oil),Q_ref)]
      
    }
    DT_Opti <- merge(DT_Opti,Annual_Elasticity[,c("Year","Q_ref")],by="Year")
    model <- Add_Constraint(model = model,
                            i = DT_Opti$i_Year,
                            j = DT_Opti$j_Matrix,
                            x = 1,
                            rhs = DT_Opti[!duplicated(i_Year)]$Q_ref,
                            dim_i = length(unique(DT_Opti$Year)),
                            sense = ">=",
                            constrnames = paste("","",DT_Opti[!duplicated(Year)]$Year,"Demand",sep="_"))
    #Adapt rhs by adding inelastic demand sources (EIA, Declining linearly until 2060, etc)
  }
  if(Demand=="Elastic"){
    model <- Add_Variable(model = model,
                          newvar = rep(0,length(unique(DT_Opti$Year))),
                          newvarnames = paste("Demand","","",DT_Opti[!duplicated(Year)]$Year,sep="_"),
                          vtype = "C")
    
    model <- Add_Constraint(model = model,
                            i = c(DT_Opti$i_Year, unique(DT_Opti$i_Year)),
                            j = c(DT_Opti$j_Matrix, which(grepl("Demand",model$varnames))),
                            x = c(rep(1,nrow(DT_Opti)),rep(-1,length(unique(DT_Opti$Year)))),
                            rhs = rep(0,length(unique(DT_Opti$Year))),
                            dim_i = length(unique(DT_Opti$Year)),
                            sense = "=",
                            constrnames = paste("","",DT_Opti[!duplicated(Year)]$Year,"Demand",sep="_"))
    
    model$pwlobj <- list()
    
    #To build the demand curves: need for each year elasticity, Quantity_ref and Price_ref
    # Elasticities
    if(!is.na(as.numeric(unlist(Demand_Elasticity)))){
      Annual_Elasticity[, Elasticity := as.numeric(unlist(Demand_Elasticity))]
    }else if(Demand_Elasticity=="Annual_2"){
      Annual_Elasticity[,Elasticity := Elasticity_2]
    }else if(Demand_Elasticity=="Annual_3"){
      Annual_Elasticity[,Elasticity := Elasticity_3]
    }
    # P_Ref and Q_Ref
    if(grepl("/",Demand_Ref)){
      Annual_Elasticity[, c("Q_ref","P_ref") := list(as.numeric(str_split(Demand_Ref,"/")[[1]][1]),as.numeric(str_split(Demand_Ref,"/")[[1]][2]))]
    }else if(Demand_Ref=="Annual"){
      Annual_Elasticity <- merge(Annual_Elasticity,Annual_Ref,by="Year")
    }else if(Demand_Ref!="Annual" & grepl("Annual",Demand_Ref)){
      Annual_Ref <- data.table(Year = 1900:2100,
                               Q_ref = sum(df_year_full[Year==as.numeric(sub('.+_(.+)', '\\1', Demand_Ref))]$Prod_Oil),
                               P_ref = unique(Selling_Prices[Year==as.numeric(sub('.+_(.+)', '\\1', Demand_Ref))]$Brent_Price_Deflated)) 
      Annual_Elasticity <- merge(Annual_Elasticity,Annual_Ref,by="Year")
    }else if(Demand_Ref=="Observed"){
      Annual_Elasticity[,Q_ref := df_year_full[order(Year)][Year %in% Annual_Elasticity$Year,.SD[,sum(Prod_Oil)],by=Year]$V1]
      Annual_Elasticity[,P_ref := Selling_Prices[order(Year)][!duplicated(Year) & Year %in% Annual_Elasticity$Year & order(Year),Brent_Price_Deflated]]
      Annual_Elasticity[,c("Q_ref","P_ref") := list(ifelse(Year > Demand_Year_Future,sum(df_year_full[Year==Demand_Year_Future]$Prod_Oil),Q_ref),
                                                    ifelse(Year > Demand_Year_Future,unique(Selling_Prices[Year==Demand_Year_Future]$Brent_Price_Deflated),P_ref))]
    }
    Annual_Elasticity <- Annual_Elasticity[Year >= Year_Start_Opti]
    pwl_nb_points <- unlist(Model_Param[,PWL_Points])
    for(year in Annual_Elasticity$Year){
      # To maximize total surplus (area below demand function)
      min_bound <- .95 * Annual_Elasticity[Year==year,Q_ref] * (Annual_Elasticity[Year==year,P_ref] / unlist(Cost_Clean_Backstop))^(-Annual_Elasticity[Year==year,Elasticity])
      max_bound <- min(Annual_Elasticity[Year==year,Q_ref] * (Annual_Elasticity[Year==year,P_ref] / min(DT_Opti[Extrac_cost!=0]$Extrac_cost))^(-Annual_Elasticity[Year==year,Elasticity]),Annual_Elasticity[Year==year,Q_ref * 2])
      vector_x <- seq(from = min_bound, to = max_bound, length.out = pwl_nb_points)
      vector_y <- - Annual_Elasticity[Year==year,Elasticity] / (1 + Annual_Elasticity[Year==year,Elasticity]) *
        vector_x ^ ((1 + Annual_Elasticity[Year==year,Elasticity]) / Annual_Elasticity[Year==year,Elasticity]) *
        Annual_Elasticity[Year==year,Q_ref] ^ -(1 / Annual_Elasticity[Year==year,Elasticity]) *
        Annual_Elasticity[Year==year,P_ref] *
        exp(- unlist(rho) * (year - Year_Start_Opti)) # Discount for future utility
      
      #To maximize producers profits (area below price* x quantity)
      # min_bound <- Annual_Elasticity[Year==year,Q_ref] * (Annual_Elasticity[Year==year,P_ref] / unlist(Cost_Clean_Backstop))^(-Annual_Elasticity[Year==year,Elasticity])
      # max_bound <- min(Annual_Elasticity[Year==year,Q_ref] * (Annual_Elasticity[Year==year,P_ref] / min(DT_Opti[Extrac_cost!=0]$Extrac_cost))^(-Annual_Elasticity[Year==year,Elasticity]),Annual_Elasticity[Year==year,Q_ref * 2])
      # vector_x <- seq(from = min_bound, to = max_bound, length.out = pwl_nb_points)
      # vector_y_2 <- - vector_x ^ (1 + 1/Annual_Elasticity[Year==year,Elasticity]) *
      #   Annual_Elasticity[Year==year,Q_ref] ^ (-1 / Annual_Elasticity[Year==year,Elasticity]) *
      #   Annual_Elasticity[Year==year,P_ref] *
      #   exp(- unlist(rho) * (year - Year_Start_Opti)) # Discount for future utility

      model$pwlobj[[year - Year_Start_Opti + 1]] <- list(var = which(model$varnames==paste("Demand","","",year,sep="_")),
                                                         x = vector_x,
                                                         y = vector_y)
      model$lb[which(model$varnames==paste("Demand","","",year,sep="_"))] <- min_bound
      model$ub[which(model$varnames==paste("Demand","","",year,sep="_"))] <- max_bound
    }
  }
  if(Demand=="Inelastic_Past"){
    model <- Add_Variable(model = model,
                          newvar = rep(0,length(unique(DT_Opti$Year))),
                          newvarnames = paste("Demand","","",DT_Opti[!duplicated(Year)]$Year,sep="_"),
                          vtype = "C")
    
    model <- Add_Constraint(model = model,
                            i = c(DT_Opti$i_Year, unique(DT_Opti$i_Year)),
                            j = c(DT_Opti$j_Matrix, which(grepl("Demand",model$varnames))),
                            x = c(rep(1,nrow(DT_Opti)),rep(-1,length(unique(DT_Opti$Year)))),
                            rhs = rep(0,length(unique(DT_Opti$Year))),
                            dim_i = length(unique(DT_Opti$Year)),
                            sense = "=",
                            constrnames = paste("","",DT_Opti[!duplicated(Year)]$Year,"Demand",sep="_"))
    
    model$pwlobj <- list()
    
    #To build the demand curves: need for each year elasticity, Quantity_ref and Price_ref
    # Elasticities
    if(!is.na(as.numeric(unlist(Demand_Elasticity)))){
      Annual_Elasticity[, Elasticity := as.numeric(unlist(Demand_Elasticity))]
    }else if(Demand_Elasticity=="Annual_2"){
      Annual_Elasticity[,Elasticity := Elasticity_2]
    }
    # P_Ref and Q_Ref
    if(is.numeric(unlist(Demand_Ref))){
      Annual_Elasticity[, c("Q_ref","P_ref") := list(unlist(Demand_Ref)[1],unlist(Demand_Ref)[2])]
    }else if(Demand_Ref=="Annual"){
      Annual_Elasticity <- merge(Annual_Elasticity,Annual_Ref,by="Year")
    }else if(Demand_Ref!="Annual" & grepl("Annual",Demand_Ref)){
      Annual_Ref <- data.table(Year = 1900:2100,
                               Q_ref = sum(df_year_full[Year==as.numeric(sub('.+_(.+)', '\\1', Demand_Ref))]$Prod_Oil),
                               P_ref = unique(Selling_Prices[Year==as.numeric(sub('.+_(.+)', '\\1', Demand_Ref))]$Brent_Price_Deflated)) 
      Annual_Elasticity <- merge(Annual_Elasticity,Annual_Ref,by="Year")
    }else if(Demand_Ref=="Observed"){
      Annual_Elasticity[,Q_ref := df_year_full[order(Year)][Year %in% Annual_Elasticity$Year,.SD[,sum(Prod_Oil)],by=Year]$V1]
      Annual_Elasticity[,P_ref := Selling_Prices[order(Year)][!duplicated(Year) & Year %in% Annual_Elasticity$Year & order(Year),Brent_Price_Deflated]]
      Annual_Elasticity[,c("Q_ref","P_ref") := list(ifelse(Year > Demand_Year_Future,sum(df_year_full[Year==Demand_Year_Future]$Prod_Oil),Q_ref),
                                                    ifelse(Year > Demand_Year_Future,unique(Selling_Prices[Year==Demand_Year_Future]$Brent_Price_Deflated),P_ref))]
    }
    Annual_Elasticity <- Annual_Elasticity[Year >= Year_Start_Opti & Year <= Year_End_Future]
    pwl_nb_points <- unlist(Model_Param[,PWL_Points])
    for(year in Annual_Elasticity$Year){
      # To maximize total surplus (area below demand function)
      min_bound <- .95 * Annual_Elasticity[Year==year,Q_ref] * (Annual_Elasticity[Year==year,P_ref] / unlist(Cost_Clean_Backstop))^(-Annual_Elasticity[Year==year,Elasticity])
      max_bound <- min(Annual_Elasticity[Year==year,Q_ref] * (Annual_Elasticity[Year==year,P_ref] / min(DT_Opti[Extrac_cost!=0]$Extrac_cost))^(-Annual_Elasticity[Year==year,Elasticity]),Annual_Elasticity[Year==year,Q_ref * 2])
      vector_x <- seq(from = min_bound, to = max_bound, length.out = pwl_nb_points)
      vector_y <- - Annual_Elasticity[Year==year,Elasticity] / (1 + Annual_Elasticity[Year==year,Elasticity]) *
        vector_x ^ ((1 + Annual_Elasticity[Year==year,Elasticity]) / Annual_Elasticity[Year==year,Elasticity]) *
        Annual_Elasticity[Year==year,Q_ref] ^ -(1 / Annual_Elasticity[Year==year,Elasticity]) *
        Annual_Elasticity[Year==year,P_ref] *
        exp(- unlist(rho) * (year - Year_Start_Opti)) # Discount for future utility
      
      #To maximize producers profits (area below price* x quantity)
      # min_bound <- Annual_Elasticity[Year==year,Q_ref] * (Annual_Elasticity[Year==year,P_ref] / unlist(Cost_Clean_Backstop))^(-Annual_Elasticity[Year==year,Elasticity])
      # max_bound <- min(Annual_Elasticity[Year==year,Q_ref] * (Annual_Elasticity[Year==year,P_ref] / min(DT_Opti[Extrac_cost!=0]$Extrac_cost))^(-Annual_Elasticity[Year==year,Elasticity]),Annual_Elasticity[Year==year,Q_ref * 2])
      # vector_x <- seq(from = min_bound, to = max_bound, length.out = pwl_nb_points)
      # vector_y_2 <- - vector_x ^ (1 + 1/Annual_Elasticity[Year==year,Elasticity]) *
      #   Annual_Elasticity[Year==year,Q_ref] ^ (-1 / Annual_Elasticity[Year==year,Elasticity]) *
      #   Annual_Elasticity[Year==year,P_ref] *
      #   exp(- unlist(rho) * (year - Year_Start_Opti)) # Discount for future utility
      
      model$pwlobj[[year - Year_Start_Opti + 1]] <- list(var = which(model$varnames==paste("Demand","","",year,sep="_")),
                                                         x = vector_x,
                                                         y = vector_y)
      model$lb[which(model$varnames==paste("Demand","","",year,sep="_"))] <- min_bound
      model$ub[which(model$varnames==paste("Demand","","",year,sep="_"))] <- max_bound
      if(year <= 2024){
        model$lb[which(model$varnames==paste("Demand","","",year,sep="_"))] <- Annual_Elasticity[Year==year,Q_ref]
        model$ub[which(model$varnames==paste("Demand","","",year,sep="_"))] <- Annual_Elasticity[Year==year,Q_ref]
      }
    }
  }
  if(Demand=="Fixed_Past"){
    model <- Add_Variable(model = model,
                          newvar = rep(0,length(unique(DT_Opti$Year))),
                          newvarnames = paste("Demand","","",DT_Opti[!duplicated(Year)]$Year,sep="_"),
                          vtype = "C")
    
    model <- Add_Constraint(model = model,
                            i = c(DT_Opti$i_Year, unique(DT_Opti$i_Year)),
                            j = c(DT_Opti$j_Matrix, which(grepl("Demand",model$varnames))),
                            x = c(rep(1,nrow(DT_Opti)),rep(-1,length(unique(DT_Opti$Year)))),
                            rhs = rep(0,length(unique(DT_Opti$Year))),
                            dim_i = length(unique(DT_Opti$Year)),
                            sense = "=",
                            constrnames = paste("","",DT_Opti[!duplicated(Year)]$Year,"Demand",sep="_"))
    
    model$pwlobj <- list()
    
    #To build the demand curves: need for each year elasticity, Quantity_ref and Price_ref
    # Elasticities
    if(!is.na(as.numeric(unlist(Demand_Elasticity)))){
      Annual_Elasticity[, Elasticity := as.numeric(unlist(Demand_Elasticity))]
    }else if(Demand_Elasticity=="Annual_2"){
      Annual_Elasticity[,Elasticity := Elasticity_2]
    }
    # P_Ref and Q_Ref
    if(is.numeric(unlist(Demand_Ref))){
      Annual_Elasticity[, c("Q_ref","P_ref") := list(unlist(Demand_Ref)[1],unlist(Demand_Ref)[2])]
    }else if(Demand_Ref=="Annual"){
      Annual_Elasticity <- merge(Annual_Elasticity,Annual_Ref,by="Year")
    }else if(Demand_Ref!="Annual" & grepl("Annual",Demand_Ref)){
      Annual_Ref <- data.table(Year = 1900:2100,
                               Q_ref = sum(df_year_full[Year==as.numeric(sub('.+_(.+)', '\\1', Demand_Ref))]$Prod_Oil),
                               P_ref = unique(Selling_Prices[Year==as.numeric(sub('.+_(.+)', '\\1', Demand_Ref))]$Brent_Price_Deflated)) 
      Annual_Elasticity <- merge(Annual_Elasticity,Annual_Ref,by="Year")
    }else if(Demand_Ref=="Observed"){
      Annual_Elasticity[,Q_ref := df_year_full[order(Year)][Year %in% Annual_Elasticity$Year,.SD[,sum(Prod_Oil)],by=Year]$V1]
      Annual_Elasticity[,P_ref := Selling_Prices[order(Year)][!duplicated(Year) & Year %in% Annual_Elasticity$Year & order(Year),Brent_Price_Deflated]]
      Annual_Elasticity[,c("Q_ref","P_ref") := list(ifelse(Year > Demand_Year_Future,sum(df_year_full[Year==Demand_Year_Future]$Prod_Oil),Q_ref),
                                                    ifelse(Year > Demand_Year_Future,unique(Selling_Prices[Year==Demand_Year_Future]$Brent_Price_Deflated),P_ref))]
    }
    pwl_nb_points <- unlist(Model_Param[,PWL_Points])
    for(year in Annual_Elasticity$Year){
      # To maximize total surplus (area below demand function)
      min_bound <- .95 * Annual_Elasticity[Year==year,Q_ref] * (Annual_Elasticity[Year==year,P_ref] / unlist(Cost_Clean_Backstop))^(-Annual_Elasticity[Year==year,Elasticity])
      max_bound <- min(Annual_Elasticity[Year==year,Q_ref] * (Annual_Elasticity[Year==year,P_ref] / min(DT_Opti[Extrac_cost!=0]$Extrac_cost))^(-Annual_Elasticity[Year==year,Elasticity]),Annual_Elasticity[Year==year,Q_ref * 2])
      vector_x <- seq(from = min_bound, to = max_bound, length.out = pwl_nb_points)
      vector_y <- - Annual_Elasticity[Year==year,Elasticity] / (1 + Annual_Elasticity[Year==year,Elasticity]) *
        vector_x ^ ((1 + Annual_Elasticity[Year==year,Elasticity]) / Annual_Elasticity[Year==year,Elasticity]) *
        Annual_Elasticity[Year==year,Q_ref] ^ -(1 / Annual_Elasticity[Year==year,Elasticity]) *
        Annual_Elasticity[Year==year,P_ref] *
        exp(- unlist(rho) * (year - Year_Start_Opti)) # Discount for future utility
      
      #To maximize producers profits (area below price* x quantity)
      # min_bound <- Annual_Elasticity[Year==year,Q_ref] * (Annual_Elasticity[Year==year,P_ref] / unlist(Cost_Clean_Backstop))^(-Annual_Elasticity[Year==year,Elasticity])
      # max_bound <- min(Annual_Elasticity[Year==year,Q_ref] * (Annual_Elasticity[Year==year,P_ref] / min(DT_Opti[Extrac_cost!=0]$Extrac_cost))^(-Annual_Elasticity[Year==year,Elasticity]),Annual_Elasticity[Year==year,Q_ref * 2])
      # vector_x <- seq(from = min_bound, to = max_bound, length.out = pwl_nb_points)
      # vector_y_2 <- - vector_x ^ (1 + 1/Annual_Elasticity[Year==year,Elasticity]) *
      #   Annual_Elasticity[Year==year,Q_ref] ^ (-1 / Annual_Elasticity[Year==year,Elasticity]) *
      #   Annual_Elasticity[Year==year,P_ref] *
      #   exp(- unlist(rho) * (year - Year_Start_Opti)) # Discount for future utility
      
      model$pwlobj[[year - Year_Start_Opti + 1]] <- list(var = which(model$varnames==paste("Demand","","",year,sep="_")),
                                                         x = vector_x,
                                                         y = vector_y)
      model$lb[which(model$varnames==paste("Demand","","",year,sep="_"))] <- ifelse(year <= Year_Start_Opti-1,Annual_Elasticity[Year==year,Q_ref],min_bound)
      model$ub[which(model$varnames==paste("Demand","","",year,sep="_"))] <- ifelse(year <= Year_Start_Opti-1,Annual_Elasticity[Year==year,Q_ref],max_bound)
    }
  }
  
  ##### Constraints - Productions Capped #####
  if(Production_Capped != "No"){
    if(grepl("War|Saudi|Country|Total|OPEC",Production_Capped)){
      Prod_Cons <- data.table(Country="",Year=0)
      if(grepl("War",Production_Capped)) Prod_Cons <- rbind(Prod_Cons,data.table(Country = c(rep("Iraq",19),rep("Iran",27),"Kuwait",rep("Azerbaijan",14),rep("Syria",11),rep("Libya",11)),
                                                                                 Year = c(2003:2024,1995:2024,1992,1992:2005,2011:2024,2011:2024)))
      if(grepl("Saudi",Production_Capped)) Prod_Cons <- rbind(Prod_Cons,data.table(Country = "Saudi Arabia",Year = 1992:2024))
      if(grepl("OPEC",Production_Capped)) Prod_Cons <- rbind(Prod_Cons,data.table(Country = c(rep(unique(asset_lvl[`OPEC NON-OPEC`=="OPEC"]$Country),each=30),rep("Qatar",27)),
                                                                                  Year = c(rep(1992:2024,length(unique(asset_lvl[`OPEC NON-OPEC`=="OPEC"]$Country))),1992:2018)))
      if(Production_Capped == "Country") Prod_Cons <- rbind(Prod_Cons <- data.table(Country = c(rep(unique(asset_lvl$Country),each=30)),
                                                                                Year = c(rep(1992:2024,length(unique(asset_lvl$Country))))))
      Prod_Cons <- Prod_Cons[-1]
      if("Country" %in% names(DT_Opti)) DT_Opti[,Country:=NULL]
      DT_Opti <- merge(DT_Opti,asset_lvl[,c("RE ID","Country")],by.x = "ID_Unit", by.y = "RE ID", all.x=T)[ID_Unit=="CBS0000",Country := "Clean backstop"]
      
      Prod_Cons <- DT_Opti[paste(Country,Year) %in% Prod_Cons[,paste(Country,Year)],c("Country","Year","j_Matrix","Prod_bin")]
      
      if(grepl("Total",Production_Capped)){
        Prod_Cons[,i_Prod_Cons := as.numeric(factor(Year,levels = unique(Year)))]
        Prod_Cons[,Country := Production_Capped]
      }else{
        Prod_Cons[,i_Prod_Cons := as.numeric(factor(paste(Country,Year),levels = unique(paste(Country,Year))))]
      }
      Prod_Cons[,Total_Prod := sum(Prod_bin),by=i_Prod_Cons]
      
      model <- Add_Constraint(model = model,
                              i = Prod_Cons$i_Prod_Cons,
                              j = Prod_Cons$j_Matrix,
                              x = 1,
                              rhs = Prod_Cons[!duplicated(i_Prod_Cons)]$Total_Prod,
                              dim_i = max(Prod_Cons$i_Prod_Cons),
                              sense = "<=",
                              constrnames = paste(Prod_Cons[!duplicated(i_Prod_Cons)]$Country,"",Prod_Cons[!duplicated(i_Prod_Cons)]$Year,"History",sep="_"))
      
    }
    
    if(grepl("Startup",Production_Capped)){
      DT_Opti[,To_Cap := ID_Unit != "CBS0000"][To_Cap==T & Year < `Start-up Year`,Prod_Plateau := 0,by=ID_Unit]
      model$ub[DT_Opti[To_Cap == T & Year < `Start-up Year`]$j_Matrix] <- 0
      DT_Opti[,c("To_Cap") := NULL]
      
      
    }
    if(grepl("Approval",Production_Capped)){
      DT_Opti[,To_Cap := ID_Unit != "CBS0000"][To_Cap==T,Prod_Plateau := max(Prod_bin),by=ID_Unit]
      model$ub[DT_Opti[To_Cap == T]$j_Matrix] <- pmin(DT_Opti[To_Cap == T]$Prod_Plateau, model$ub[DT_Opti[To_Cap == T]$j_Matrix])
      DT_Opti[,c("To_Cap") := NULL]
      
      DT_Opti_Dvpt <- merge(DT_Opti_Dvpt,asset_lvl[,c("RE ID","Approval Year")],by.x="ID_Unit",by.y="RE ID")
      DT_Opti_Dvpt[,To_Cap := Year < `Approval Year`]
      model$ub[DT_Opti_Dvpt[To_Cap==T]$j_Matrix_Dvpt] <- 0
      
    }
  }
  if(grepl("OutageInvoluntaryPast",Production_Capped)){
    List_Affected_Assets <- outages[Year <= 2024 & !(`Outage Detail` %in% c("Production","Regulatory Order","Other Planned")) & `Production (Million bbl)` > 0 & `Oil and Gas Detail` %in% Oil_Definition, unique(paste(`RE ID`,Year))]
    temp <- outages[paste(`RE ID`,Year) %in% List_Affected_Assets][,Max_Prod_OutageInvol := sum(`Production (Million bbl)`[`Oil and Gas Detail` %in% Oil_Definition & `Outage Detail` %in% c("Production","Regulatory Order","Other Planned")]), by = .(`RE ID`,Year)][!duplicated(paste(`RE ID`,Year)),c("RE ID","Year","Max_Prod_OutageInvol")]
    DT_Opti <- merge(DT_Opti,temp,by.x=c("ID_Unit","Year"),by.y=c("RE ID","Year"),all.x=T)[is.na(Max_Prod_OutageInvol),Max_Prod_OutageInvol := Inf]
    model$ub[DT_Opti$j_Matrix] <- pmin(DT_Opti$Max_Prod_OutageInvol, model$ub[DT_Opti$j_Matrix])
  }
  
  if(grepl("Shale",Production_Capped)){
    temp <- merge(DT_Opti[Shale==1,c("ID_Unit","Shale_Play")][!duplicated(ID_Unit)],
                  df_year_full[,c("Prod_Oil","Year","RE ID")],by.x="ID_Unit",by.y="RE ID")
    temp <- setDT(temp)[CJ(ID_Unit = ID_Unit, Year = c(1900:2100), unique=T),on=.(ID_Unit,Year)][,Shale_Play := unique(Shale_Play[!is.na(Shale_Play)]),by=ID_Unit][is.na(Prod_Oil),Prod_Oil:=0]
    temp <- temp[,Prod_Oil := sum(Prod_Oil),by=.(Shale_Play,Year)][!duplicated(paste(Shale_Play,Year))]
    
    # Avg increase in production in the 5 years in which production increased yearly the most
    temp[,Increase := (Prod_Oil - shift(Prod_Oil,fill=0)),by=Shale_Play][order(-Increase),Rank := 1:.N,by=Shale_Play][,Max_Increase_Play := mean(Increase[Rank <= 5]),by = Shale_Play][,Ratio := Max_Increase_Play / max(Prod_Oil),by=Shale_Play]
    temp[,Initial_Prod := Prod_Oil[Year==Year_Start_Opti - 1],by = Shale_Play]
    
    #Useful info kept
    temp <- temp[!duplicated(Shale_Play),c("Shale_Play","Max_Increase_Play","Initial_Prod")] 
      
    DT_Opti <- merge(DT_Opti,temp[!duplicated(Shale_Play),c("Shale_Play","Max_Increase_Play","Initial_Prod")],by="Shale_Play",all.x=T)
    #DT_Opti[Reserves < .365,Max_Increase_Play:=NA]
    DT_Opti[Max_Increase_Play < .365,Max_Increase_Play:=.365]
    temp <- DT_Opti[Shale==T & !is.na(Max_Increase_Play)]
    
    # Building Year_Post and Year_Pre to match together prod in t and t+1 such as Q_t - Q_t+1 <= Max_Increase and Q_t0 <= Initial_Prod + Max_Increase
    temp[,i_Year_Post := as.numeric(factor(paste(Shale_Play,Year))),]
    temp[,i_Year_Pre := i_Year_Post + 1]

    table_constraint <- data.table(i = c(temp[,i_Year_Post],temp[Year!=Year_End_Future,i_Year_Pre]),
                       j = c(temp[,j_Matrix],temp[Year!=Year_End_Future,j_Matrix]),
                       x = c(rep(1,nrow(temp)),rep(-1,nrow(temp[Year!=Year_End_Future]))))

    #Constraint on 1st year of prod = Initial Prod + Max_Increase ; for other years x_t - x_t-1 <= Max_Increase
    model <- Add_Constraint(model = model,
                            i = table_constraint$i,
                            j = table_constraint$j,
                            x = table_constraint$x,
                            rhs = temp[order(i_Year_Post)][!duplicated(i_Year_Post)][,Max_Increase_Play + ifelse(Year==Year_Start_Opti,Initial_Prod,0)],
                            dim_i = max(table_constraint$i),
                            sense = "<=",
                            constrnames = paste(temp[order(-Reserves)][order(i_Year_Post)][!duplicated(i_Year_Post)][!duplicated(Shale_Play)]$ID_Unit,
                                                "",temp[order(i_Year_Post)][!duplicated(i_Year_Post)]$Year,"ShaleBU",sep="_"))
  }
 
  ##### Constraints - Investments Capped/Fixed #####
  if(grepl("Country",Investment_Capped)){
    Country <- merge(DT_Opti[Year %in% Year_Start_Opti:2021],asset_lvl[,c("RE ID","Country")],by.x="ID_Unit",by.y = "RE ID",all.x=T)[ID_Unit=="CBS0000",Country := "Clean backstop"]
    Country[,Investment_Obs := sum(Development_Obs * Development_Cost),by=.(Country,Year)][,Total_Investment_Obs := sum(Development_Obs * Development_Cost),by=Country]
    Country <- merge(Country,DT_Opti_Dvpt[,c("ID_Unit","Year","j_Matrix_Dvpt")],by = c("ID_Unit","Year"))
    Country <- Country[Total_Investment_Obs >= 1e4]
    if(gsub("_Fixed","",Investment_Capped) == "Country") {
      own <- own[Total_Investment >= 1e4]
    }else{
      own <- own[Total_Investment >= as.numeric(substr(gsub("_Fixed","",Investment_Capped),9,nchar(gsub("_Fixed","",Investment_Capped))))]
    }
    Country[,i_InvCap := as.numeric(as.factor(paste(Country,Year))),]
    #100Mds: 10/132 countries, 10Mds: 34/132, 1Mds: 68/132
    
    if(grepl("Fixed",Investment_Capped)) Sense <- "=" else Sense <- "<="
    
    Country[Investment_Obs <= 1e-6,Investment_Obs := 0]
    
    model <- Add_Constraint(model = model,
                            i = Country$i_InvCap,
                            j = Country$j_Matrix_Dvpt,
                            x = Country$Development_Cost,
                            rhs = Country[order(i_InvCap)][!duplicated(i_InvCap)]$Investment_Obs,
                            dim_i = max(Country$i_InvCap),
                            sense = Sense,
                            constrnames = paste(Country[order(i_InvCap)][!duplicated(i_InvCap)]$Country,0,Country[order(i_InvCap)][!duplicated(i_InvCap)]$Year,"Investment.Cap",sep="_"))
  }
  if(grepl("Company",Investment_Capped)){
    own <- fread(paste0("Output/GP_All/Results/",unlist(Unit_Param$Version),"/Misc/Ownership.csv"))
    own <- merge(own,DT_Opti[Year %in% Year_Start_Opti:2021,c("ID_Unit","Year","Development_Cost","Development_Obs")], by=c("ID_Unit","Year"))[Development_Obs < 0,Development_Obs := 0][Development_Obs > 1,Development_Obs := 1][Share < 0, Share := 0][Share > 1, Share := 1]
    own[,Investment_Obs := sum(Development_Obs * Development_Cost * Share),by=.(`Historical Company`,Year)][,Total_Investment := sum(Development_Obs * Development_Cost * Share), by = `Historical Company`]
    own <- merge(own,DT_Opti_Dvpt[,c("ID_Unit","Year","j_Matrix_Dvpt")],by=c("ID_Unit","Year"))
    if(gsub("_Fixed","",Investment_Capped) == "Company") {
      own <- own[Total_Investment >= 1e3]
    }else{
      own <- own[Total_Investment >= as.numeric(substr(gsub("_Fixed","",Investment_Capped),9,nchar(gsub("_Fixed","",Investment_Capped))))]
    }
   
    own[,i_InvCap := as.numeric(as.factor(paste(`Historical Company`,Year))),]
    #1Mds --> 310/2902 companies, 10Mds: 71/2902, 100Mds: 11/2902
    #for each company x year index i: index j investments and x = dvpt_cost * share and rhs = sum(Development_Obs * Dvpt_Cost * share)
    
    if(grepl("Fixed",Investment_Capped)) Sense <- "=" else Sense <- "<="
    
    #own[Investment_Obs <= 1e-6,Investment_Obs := 0]
    
    model <- Add_Constraint(model = model,
                            i = own$i_InvCap,
                            j = own$j_Matrix_Dvpt,
                            x = own[,Development_Cost * Share],
                            rhs = own[order(i_InvCap)][!duplicated(i_InvCap)]$Investment_Obs,
                            dim_i = max(own$i_InvCap),
                            sense = Sense,
                            constrnames = paste(own[order(i_InvCap)][!duplicated(i_InvCap)]$`Historical Company`,0,own[order(i_InvCap)][!duplicated(i_InvCap)]$Year,"Investment.Cap",sep="_"))
    
  }
  
  ##### Constraints - Market shares #####
  if(Market_Share_Fixed == "OPEC"){
    Market_Share <- df_year_full[Country %in% asset_lvl[`OPEC NON-OPEC`=="OPEC",unique(Country)] & Year >= 1946][order(Year),sum(Prod_Oil),by=Year] / df_year_full[Year >= 1946][order(Year),sum(Prod_Oil),by=Year]
    Market_Share[,Year := 1945 + .I][Year %in% c((Year_Start_Opti-1):max(Year)),V1 := V1[Year==Demand_Year_Future]]
    model <- Add_Constraint(model = model,
                            i = c(DT_Opti[OPEC==T,i_Year],DT_Opti[ID_Unit=="CBS0000",i_Year],sort(unique(DT_Opti[,i_Year]))),
                            j = c(DT_Opti[OPEC==T,j_Matrix],DT_Opti[ID_Unit=="CBS0000",j_Matrix],which(grepl("Demand",model$varnames))),
                            x = c(rep(1,sum(DT_Opti[,OPEC==T])),Market_Share[Year %in% unique(DT_Opti$Year),V1],Market_Share[Year %in% unique(DT_Opti$Year),-V1]),
                            rhs = rep(0,uniqueN(DT_Opti$Year)),
                            dim_i = max(DT_Opti[OPEC==T,i_Year]),
                            sense = "=",
                            constrnames = paste("OPEC","",sort(unique(DT_Opti[,Year])),"Market.Share",sep="_"))
    
  }
  
  if(Market_Share_Fixed == "OPEC_Plus"){
    Market_Share <- df_year_full[Country %in% asset_lvl[`OPEC Plus`=="OPEC+",unique(Country)] & Year >= 1946][order(Year),sum(Prod_Oil),by=Year] / df_year_full[Year >= 1946][order(Year),sum(Prod_Oil),by=Year]
    Market_Share[,Year := 1945 + .I][Year %in% c((Year_Start_Opti-1):max(Year)),V1 := V1[Year==Demand_Year_Future]]
    DT_Opti[,OPEC_Plus := ID_Unit %in% asset_lvl[`OPEC Plus`=="OPEC+",`RE ID`]]
    model <- Add_Constraint(model = model,
                            i = c(DT_Opti[OPEC_Plus==T,i_Year],DT_Opti[ID_Unit=="CBS0000",i_Year],sort(unique(DT_Opti[,i_Year]))),
                            j = c(DT_Opti[OPEC_Plus==T,j_Matrix],DT_Opti[ID_Unit=="CBS0000",j_Matrix],which(grepl("Demand",model$varnames))),
                            x = c(rep(1,sum(DT_Opti[,OPEC_Plus==T])),Market_Share[Year %in% unique(DT_Opti$Year),V1],Market_Share[Year %in% unique(DT_Opti$Year),-V1]),
                            rhs = rep(0,uniqueN(DT_Opti$Year)),
                            dim_i = max(DT_Opti[OPEC_Plus==T,i_Year]),
                            sense = "=",
                            constrnames = paste("OPEC","",sort(unique(DT_Opti[,Year])),"Market.Share",sep="_"))
  }
  
  ##### Rest - Trimming the model for tractability ######
  ### Imports penalty (+$2/bbl)
  # Imports <- merge(DT_Opti,asset_lvl[,c("RE ID","Country")],by.x="ID_Unit",by.y = "RE ID",all.x=T)[ID_Unit=="CBS0000",Country := "Clean backstop"]
  # Imports[,i_Country_Year := as.numeric(factor(paste(Country,Year))),]
  
  # mat <- cbind(cbind(as.matrix(model$A),model$sense),model$rhs)
  # rownames(mat) <- model$constrnames
  # colnames(mat) <- c(model$varnames,"sense","rhs")
  
  print("")
  print("--------------------------------------------------------------")
  print(paste0("Model: ",unlist(Version)," - Scenario: ",unlist(Scenario)))
  print(paste0("Before removal: ",length(model$varnames)," vars (",sum(model$vtype=="C")," Cont, ",sum(model$vtype=="B")," Bin) / ",length(model$constrnames)," constraints"))
  print("--------------------------------------------------------------")
  print("")
  
  if(Policy=="SCC") DT_Opti[,Too_Costly := ifelse(Extrac_cost + 1e-3 * theta * unlist(Policy_Values) * exp(unlist(rho) * (Year - Year_Start_Opti)) > 1.05 * unlist(Cost_Clean_Backstop),T,F),] else DT_Opti[,Too_Costly:=F]
  DT_Opti[,Impossible_Dvpt := Year > ifelse(sum(Too_Costly==F)>0, max(Year[Too_Costly==F]) - Lag_After_Development_Asset,Inf),by=ID_Unit]
  if(Development_OPEC=="Observed" | Development_Fringe=="Observed") DT_Opti[,Impossible_Dvpt:=F]#[,Too_Costly:=F][,Prod_Impossible:=F]
  if(grepl("InvFix",Scenario)) DT_Opti[Year <= 2024,Impossible_Dvpt:=F]

  Vars_to_remove <- c(paste("Prod",DT_Opti[Prod_Impossible + Too_Costly > 0]$ID_Unit,DT_Opti[Prod_Impossible + Too_Costly > 0]$Bin_Cost,DT_Opti[Prod_Impossible + Too_Costly > 0]$Year,sep="_"),
                      paste("Development",DT_Opti[Impossible_Dvpt==T]$ID_Unit,"1",DT_Opti[Impossible_Dvpt==T]$Year,sep="_"),
                      model$varnames[grepl("CBS0000",model$varnames) & grepl("_1_",model$varnames)==F])
  Vars_to_remove <- Vars_to_remove[which(Vars_to_remove!="Prod___" & Vars_to_remove!="Development__1_")]

  DT_Opti[,Prod_Impossible_Year_Asset := sum(Prod_Impossible|Too_Costly)==max(Bin_Cost),by=i_Asset_Year]
  Constraints_to_remove <- c(unique(paste(DT_Opti[Bin_Reserves==0]$ID_Duplicate,"","Reserve",sep="_")),model$constrnames[grepl("CBS0000",model$constrnames)])
  if(Production_Capacity_OPEC %in% c("Plateau","Hybrid","Observed")) Constraints_to_remove <- c(Constraints_to_remove,unique(paste(DT_Opti[Prod_Impossible_Year_Asset==T & OPEC==T]$ID_Unit,1,DT_Opti[Prod_Impossible_Year_Asset==T & OPEC==T]$Year,"Prod.Cap.Plateau",sep="_")))
  if(Production_Capacity_OPEC %in% c("Decline","Hybrid","Observed")) Constraints_to_remove <- c(Constraints_to_remove,unique(paste(DT_Opti[Prod_Impossible_Year_Asset==T & OPEC==T]$ID_Unit,1,DT_Opti[Prod_Impossible_Year_Asset==T & OPEC==T]$Year,"Prod.Cap.Decline",sep="_")))
  if(Production_Capacity_Fringe %in% c("Plateau","Hybrid","Observed")) Constraints_to_remove <- c(Constraints_to_remove,unique(paste(DT_Opti[Prod_Impossible_Year_Asset==T & OPEC==F]$ID_Unit,1,DT_Opti[Prod_Impossible_Year_Asset==T & OPEC==F]$Year,"Prod.Cap.Plateau",sep="_")))
  if(Production_Capacity_Fringe %in% c("Decline","Hybrid","Observed")) Constraints_to_remove <- c(Constraints_to_remove,unique(paste(DT_Opti[Prod_Impossible_Year_Asset==T & OPEC==F]$ID_Unit,1,DT_Opti[Prod_Impossible_Year_Asset==T & OPEC==F]$Year,"Prod.Cap.Decline",sep="_")))
  #if(Production_Capacity_OPEC == "Observed") Constraints_to_remove <- c(Constraints_to_remove,unique(paste(DT_Opti[OPEC==T & Year <= 2021]$ID_Unit,1,DT_Opti[OPEC==T & Year <= 2021]$Year,"Prod.Cap.Decline",sep="_")))
  #if(Production_Capacity_Fringe == "Observed") Constraints_to_remove <- c(Constraints_to_remove,unique(paste(DT_Opti[OPEC==F & Year <= 2021]$ID_Unit,1,DT_Opti[OPEC==F & Year <= 2021]$Year,"Prod.Cap.Decline",sep="_")))
  if(Development_Cost=="Separate") Constraints_to_remove <- c(Constraints_to_remove,unique(paste(DT_Opti[Prod_Impossible_Year_Asset==T & Development_Cost > 0 & Development_Level_Start_Opti < 1]$ID_Unit,1,DT_Opti[Prod_Impossible_Year_Asset==T & Development_Cost > 0 & Development_Level_Start_Opti < 1]$Year,"Development",sep="_")))
  Constraints_to_remove <- Constraints_to_remove[grepl("^_",Constraints_to_remove)==F][!duplicated(Constraints_to_remove)]

  Constraints_to_remove <- Constraints_to_remove[Constraints_to_remove %in% model$constrnames]
  Vars_to_remove <- Vars_to_remove[Vars_to_remove %in% model$varnames]


  if(grepl("Fixed",Investment_Capped)){
    if(length(Constraints_to_remove) > 0) model <- Remove_Constraint(model = model,constrnames = Constraints_to_remove[grepl("CBS0000",Constraints_to_remove)])
  }else{
    if(length(Constraints_to_remove) > 0) model <- Remove_Constraint(model = model,constrnames = Constraints_to_remove)
    if(length(Vars_to_remove) > 0) model <- Remove_Variable(model = model,varnames = Vars_to_remove, SOS = ifelse(exists("SOS_Constraints"),T,F), SOS_Constraints = SOS_Constraints)
  }
  
  #Index empty rows in constraint matrix --> Remove indexed values from model$A + model$rhs + model$constrnames + model$sense
  Empty_rows_index <- model$constrnames[which(rowSums(model$A!=0)==0)]
  if(length(Empty_rows_index) > 1) model <- Remove_Constraint(model=model,constrnames = Empty_rows_index)

  #Remove useless tables/vectors/lists
  
  #Setting development to 0 for years in which dvpt is impossible
  #model$ub[which(model$varnames %in% paste("Development",DT_Opti[Impossible_Dvpt==T]$ID_Unit,"1",DT_Opti[Impossible_Dvpt==T]$Year,sep="_"))] <- 0

  #Setting development to the first available year for asset with Development_Cost_Start_Opti == 0
  #model$lb[which(model$varnames %in% paste("Development",DT_Opti_Dvpt[Development_Cost==0 & Development_Level_Start_Opti<1 & duplicated(ID_Unit)]$ID_Unit,"1",DT_Opti_Dvpt[Development_Cost==0 & Development_Level_Start_Opti<1  & duplicated(ID_Unit)]$Year,sep="_"))] <- 0
  #model$ub[which(model$varnames %in% paste("Development",DT_Opti_Dvpt[Development_Cost==0 & Development_Level_Start_Opti<1 & duplicated(ID_Unit)]$ID_Unit,"1",DT_Opti_Dvpt[Development_Cost==0 & Development_Level_Start_Opti<1  & duplicated(ID_Unit)]$Year,sep="_"))] <- 0
  #model$lb[which(model$varnames %in% paste("Development",DT_Opti_Dvpt[Development_Cost==0 & Development_Level_Start_Opti<1 & !duplicated(ID_Unit)]$ID_Unit,"1",DT_Opti_Dvpt[Development_Cost==0 & Development_Level_Start_Opti<1  & !duplicated(ID_Unit)]$Year,sep="_"))] <- 1 - DT_Opti_Dvpt[Development_Cost==0 & Development_Level_Start_Opti<1 & !duplicated(ID_Unit)]$Development_Level_Start_Opti
  #model$ub[which(model$varnames %in% paste("Development",DT_Opti_Dvpt[Development_Cost==0 & Development_Level_Start_Opti<1 & !duplicated(ID_Unit)]$ID_Unit,"1",DT_Opti_Dvpt[Development_Cost==0 & Development_Level_Start_Opti<1  & !duplicated(ID_Unit)]$Year,sep="_"))] <- 1 - DT_Opti_Dvpt[Development_Cost==0 & Development_Level_Start_Opti<1 & !duplicated(ID_Unit)]$Development_Level_Start_Opti
  print(paste0("After removal: ",length(model$varnames)," vars (",sum(model$vtype=="C")," Cont, ",sum(model$vtype=="B")," Bin) / ",length(model$constrnames)," constraints"))
  
  detach(Scenario_Param)
  detach(Unit_Param)
  
  return(model)
}
##### Function - Gurobi computations and output writing to disk #####
Optim <- function(Unit_Param,Scenario_Param,Model_Param){
  #Seek result database
  # attach(Model_Param,warn.conflicts = F)
  # attach(Unit_Param,warn.conflicts = F)
  # attach(Scenario_Param,warn.conflicts = F)

  if(file.exists(paste0("Output/GP_All/Results/",Unit_Param$Version,"/Results_Raw/",Scenario_Param$Scenario,"_Raw.csv"))){
    
    Results_Raw <- fread(paste0("Output/GP_All/Results/",Unit_Param$Version,"/Results_Raw/",Scenario_Param$Scenario,"_Raw.csv"))
    
  }else{
        model <- Model_Preparation(Unit_Param = Unit_Param,
                               Scenario_Param = Scenario_Param,
                               Model_Param = Model_Param,
                               Subset = NA)
    
    print(paste0("Launching optimization - ",Unit_Param$Version," - ",Scenario_Param$Scenario))
    
    result <- gurobi(model, params = list(Method = unlist(Model_Param$Model_Method),
                                          Crossover = unlist(Model_Param$Crossover),
                                          Threads = unlist(Model_Param$Threads_count),
                                          IntFeasTol = unlist(Model_Param$IntFeas),
                                          MIPGap = unlist(Model_Param$MIP_Gap),
                                          Node_Limit = unlist(Model_Param$Nb_Nodes_Max),
                                          NodefileStart = 40,
                                          NodefileDir = paste0("Output/Temp_Gurobi/",Unit_Param$Version,"/"),
                                          MIPFocus = ifelse(Model_Param$MIP_Focus == "Solution",1,ifelse(Model_Param$MIP_Focus == "Bound",3,ifelse(Model_Param$MIP_Focus == "Optimality",2,0))),
                                          SolFiles = "Output/Temp_Gurobi",
                                          TimeLimit = 3600))
    
    #temp_prod <- result$x[grepl("Prod",model$varnames)]
    
    if(Scenario_Param$Incomplete_Development == F | Scenario_Param$Progressive_Development == F){
      model <- Remove_Constraint(model = model,
                        constrnames = model$constrnames[grepl("Sum.Development",model$constrnames)])

      model$lb[grepl("Development",model$varnames)] <- result$x[grepl("Development",model$varnames)]
      model$ub[grepl("Development",model$varnames)] <- result$x[grepl("Development",model$varnames)]
      if(Scenario_Param$Incomplete_Development == F) model$vtype[grepl("Binary.Sum.Dvpt",model$varnames)] <- "C"
      if(Scenario_Param$Progressive_Development == F) model$sos <- NULL
      temp_time <- result$runtime
      result <- gurobi(model, params = list(Method = unlist(Model_Param$Model_Method),
                                            Crossover = unlist(Model_Param$Crossover),
                                            Threads = unlist(Model_Param$Threads_count),
                                            IntFeasTol = unlist(Model_Param$IntFeas),
                                            MIPGap = unlist(Model_Param$MIP_Gap),
                                            Node_Limit = unlist(Model_Param$Nb_Nodes_Max),
                                            NodefileStart = 40,
                                            NodefileDir = paste0("Output/Temp_Gurobi/",Unit_Param$Version,"/"),
                                            MIPFocus = ifelse(Model_Param$MIP_Focus == "Solution",1,ifelse(Model_Param$MIP_Focus == "Bound",3,ifelse(Model_Param$MIP_Focus == "Optimality",2,0))),
                                            SolFiles = "Output/Temp_Gurobi"))
    }
    #prod <- result$x[grepl("Prod",model$varnames)]
    
    X <- as.data.table(matrix(unlist(strsplit(model$varnames,"_",fixed = T)),ncol=4,byrow = T))
    names(X) <- c("var","ID_Unit","Bin","Year")
    Cons <- as.data.table(matrix(unlist(strsplit(model$constrnames,"_",fixed = T)),ncol=4,byrow = T))
    names(Cons) <- c("ID_Unit","Bin","Year","var")
    
    Unit_Param$Bins_Ratio <- as.character(Unit_Param$Bins_Ratio)
    Unit_Param$Bins_Threshold <- as.character(Unit_Param$Bins_Threshold)
    Unit_Param$Costs_Included <- as.character(Unit_Param$Costs_Included)
    Unit_Param$CI_Lifecycle <- as.character(Unit_Param$CI_Lifecycle)
    Param_DT <- 0
    for(i in 1:length(Unit_Param)) Param_DT <- c(Param_DT,unlist(Unit_Param[[i]]))
    for(i in 1:length(Scenario_Param)) Param_DT <- c(Param_DT,unlist(Scenario_Param[[i]]))
    Param_DT <- Param_DT[-1]
    
    Results_Raw <- data.table(Type = c(rep("Objective",length(result$x)),
                                       rep("Constraint",length(result$pi)),
                                       rep("Info",ncol(Unit_Param) + ncol(Scenario_Param) + 11)),
                              Name = c(model$varnames,model$constrnames,rep("Unit",ncol(Unit_Param)),rep("Scenario",ncol(Scenario_Param)),rep("Opti",11)),
                              Variable = c(X$var, Cons$var,
                                           names(Unit_Param),names(Scenario_Param),"Version", "Scenario", "Result_Optimality", "ObjVal", "BestBound",
                                           "MIP_Gap", "Nb_Nodes", "Method", "Crossover", "IntFeasTol", "Run_Time"),
                              ID_Unit = c(X$ID_Unit,Cons$ID_Unit,rep("",ncol(Unit_Param) + ncol(Scenario_Param) + 11)),
                              ID_Duplicate = c(paste(X$ID_Unit,X$Bin,sep="_"),paste(Cons$ID_Unit,Cons$Bin,sep="_"),rep("",ncol(Unit_Param) + ncol(Scenario_Param) + 11)),
                              Year = c(X$Year,Cons$Year,rep("",ncol(Unit_Param) + ncol(Scenario_Param) + 11)),
                              value = c(round(result$x,6),round(result$pi,8),
                                        Param_DT,
                                        Unit_Param$Version,
                                        Scenario_Param$Scenario,
                                        result$status,
                                        result$objval,
                                        ifelse(exists("result$objbound"),result$objbound,NA),
                                        ifelse(exists("result$mipgap"),result$mipgap,NA),
                                        result$nodecount,
                                        unlist(Model_Param$Model_Method),
                                        unlist(Model_Param$Crossover),
                                        unlist(Model_Param$IntFeas),
                                        round(result$runtime + ifelse(exists("temp_time"),temp_time,0),2)))
    
    dir.create(file.path(paste0("Output/GP_All/Results/",Unit_Param$Version,"/Results_Raw")), showWarnings = FALSE)
    fwrite(Results_Raw, file = paste0("Output/GP_All/Results/",Unit_Param$Version,"/Results_Raw/",Scenario_Param$Scenario,"_Raw.csv"))
  }
  
  if(Results_Raw[Variable=="Result_Optimality"]$value == "TIME_LIMIT") next
  
  Info <- Results_Raw[Type=="Info"]
  Cons <- Results_Raw[Type=="Constraint"]
  Obj <- Results_Raw[Type=="Objective"]
  Demand_Year_Future <- 2019
  
  if(Info[Variable == "Policy"]$value == "CB"){
    Carbon_Tax <- - as.numeric(Cons[Variable=="Carbon.Budget"]$value)
  }else if(Info[Variable == "Policy"]$value == "SCC"){
    Carbon_Tax <- as.numeric(Info[Variable == "Policy_Values"]$value)
  }else if(Info[Variable == "Policy"]$value == "Uniform"){
    Carbon_Tax <- as.numeric(Info[Variable == "Policy_Values"]$value)
  }else{
    Carbon_Tax <- 0
  }
  
  DT_Opti <- fread(paste0("Output/GP_All/Results/",Unit_Param$Version,"/Misc/DT_Opti.csv"))
  
  DT_Result <- DT_Opti[,Name:=Unit][,Unit:=Info[Variable=="Unit"]$value][,c("Unit","ID_Unit","Name","Year","Discovery Year","Reserves","theta","Development_Cost","Development_Level_Start_Opti","Lag_After_Development_Asset","Extrac_cost","ID_Duplicate")]
  
  DT_Result <- merge(merge(DT_Result,
                           Obj[Variable=="Prod"][,Prod := as.numeric(value)][,Year:=as.numeric(Year)][,c("ID_Duplicate","Year","Prod")],by = c("ID_Duplicate","Year"),all.x=T),
                     Obj[Variable=="Development" & Year!="Past"][,Development := as.numeric(value)][,Year:=as.numeric(Year)][,c("ID_Duplicate","Year","Development")],by=c("ID_Duplicate","Year"),all.x=T)
  DT_Result[is.na(DT_Result)] <- 0
  DT_Result[,c("Extrac_cost","Prod","Development") := list(ifelse(sum(Prod)>0,sum(Extrac_cost*Prod)/sum(Prod),min(Extrac_cost)),sum(Prod),sum(Development)),by=.(ID_Unit,Year)]
  DT_Result <- DT_Result[!duplicated(paste(ID_Unit,Year)),-"ID_Duplicate"]
  
  Annual_Elasticity <- Annual_Elasticity_Setup[Year >= unlist(Unit_Param$Year_Start_World)]
  #Annual_Elasticity[,Elasticity := Annual_Elasticity_Setup$Elasticity[1:nrow(Annual_Elasticity)]]
  if(!is.na(as.numeric(Scenario_Param$Demand_Elasticity))){
    Annual_Elasticity[, Elasticity := as.numeric(unlist(Scenario_Param$Demand_Elasticity))]
  }else if(Scenario_Param$Demand_Elasticity=="Annual_2"){
    Annual_Elasticity[,Elasticity := Elasticity_2]
  }else if(Scenario_Param$Demand_Elasticity=="Annual_3"){
    Annual_Elasticity[,Elasticity := Elasticity_3]
  }
  # P_Ref and Q_Ref
  if(grepl("/",Scenario_Param$Demand_Ref)){
    Annual_Elasticity[, c("Q_ref","P_ref") := list(as.numeric(str_split(Demand_Ref,"/")[[1]][1]),as.numeric(str_split(Demand_Ref,"/")[[1]][2]))]
  }else if(Scenario_Param$Demand_Ref=="Annual"){
    Annual_Elasticity <- merge(Annual_Elasticity[,c("Year","Elasticity","Elasticity_2","Elasticity_3")],Annual_Ref,by="Year")
  }else if(Scenario_Param$Demand_Ref!="Annual" & grepl("Annual",Scenario_Param$Demand_Ref)){
    Annual_Ref <- data.table(Year = 1900:2100,
                             Q_ref = sum(df_year_full[Year==as.numeric(sub('.+_(.+)', '\\1', Scenario_Param$Demand_Ref))]$Prod_Oil),
                             P_ref = unique(Selling_Prices[Year==as.numeric(sub('.+_(.+)', '\\1', Scenario_Param$Demand_Ref))]$Brent_Price_Deflated)) 
    Annual_Elasticity <- merge(Annual_Elasticity,Annual_Ref,by="Year")
  }else if(Scenario_Param$Demand_Ref=="Observed"){
    Annual_Elasticity[,Q_ref := df_year_full[order(Year)][Year %in% Annual_Elasticity$Year,.SD[,sum(Prod_Oil)],by=Year]$V1]
    Annual_Elasticity[,P_ref := Selling_Prices[order(Year)][!duplicated(Year) & Year %in% Annual_Elasticity$Year & order(Year),Brent_Price_Deflated]]
    Annual_Elasticity[,c("Q_ref","P_ref") := list(ifelse(Year > Demand_Year_Future,sum(df_year_full[Year==Demand_Year_Future]$Prod_Oil),Q_ref),
                                                  ifelse(Year > Demand_Year_Future,unique(Selling_Prices[Year==Demand_Year_Future]$Brent_Price_Deflated),P_ref))]
  }  
  if(Scenario_Param$Demand!="Inelastic"){
    Annual_Elasticity <- merge(Annual_Elasticity,Obj[Variable == "Demand",c("Year","value")][,c("Year","value"):=list(as.numeric(Year),as.numeric(value))],by="Year")[,Oil_Price := P_ref*(value / Q_ref)^(1/Elasticity)]
  }else{
    Annual_Elasticity <- merge(Annual_Elasticity,Cons[Variable == "Demand",c("Year","value")][,c("Year","value"):=list(as.numeric(Year),as.numeric(value))],by="Year")[,Oil_Price := value * exp(0.03 * (Year - unlist(unlist(Unit_Param$Year_Start_World))))]
  }
  DT_Result <- merge(DT_Result,Annual_Elasticity[,c("Year","Oil_Price")],by = "Year")
  
  #DT_Result <- merge(DT_Result,Cons[Variable=="Demand"][,Year:=as.numeric(Year)][,Oil_Price := as.numeric(value) * exp(as.numeric(Info[Variable=="rho"]$value) * (Year - as.numeric(Info[Variable=="Year_Start_World"]$value)))][,c("Year","Oil_Price")], by = "Year")
  
  DT_Result[,CT := Carbon_Tax * exp(as.numeric(Info[Variable=="rho"]$value) * (Year - as.numeric(Info[Variable=="Year_Start_World"]$value)))]
  
  if(Info[Variable == "Policy"]$value == "Uniform"){
    DT_Result[,CT_per_bbl := CT ]
  }else{
    DT_Result[,CT_per_bbl := CT * theta * 1e-3]
  }
  DT_Result[,Profits_per_bbl := Oil_Price - CT_per_bbl - Extrac_cost]
  DT_Result[,CT := NULL]
  
  fwrite(DT_Result, file = paste0("Output/GP_All/Results/",Unit_Param$Version,"/",Scenario_Param$Scenario,"_Results.csv"))
  
  return(DT_Result)
}

##### Function - Wrapping all functions up to Optim to compute results for all desired paramaters/scenarios #####
Compute <- function(To_Run){
  
  if(file.exists(paste0("Output/GP_All/Results/Info_Parameters.csv"))==F){
    dir.create("Output/GP_All/Results",recursive = T)
    Info <- Unit_Parameters[Version==To_Run$Unit[1]]
    fwrite(Info[0,],paste0("Output/GP_All/Results/Info_Parameters.csv"))
  }
  
  To_Run[,Old_Unit_Name := ""][,Old_Scenario_Name := ""]
  
  #Duplicate_Version and Duplicate_Parameters
  for(unit_version in unique(To_Run$Unit)){
    Info <- fread(paste0("Output/GP_All/Results/Info_Parameters.csv"))
    Info <- rbind(Info,Unit_Parameters[Version==unit_version][,c("Costs_Included","Bins_Ratio","Bins_Threshold","CI_Lifecycle") := list(paste(unlist(Costs_Included),collapse ="."),ifelse(is.na(Bins_Ratio),NA,paste(unlist(Bins_Ratio),collapse=".")),ifelse(is.na(Bins_Threshold),NA,paste(unlist(Bins_Threshold),collapse=".")),paste(unlist(CI_Lifecycle),collapse="."))])
    Info$Check <- do.call(paste,c(Info[,2:ncol(Info)],sep="_"))
    Info[-nrow(Info),Same_Name := ifelse(Version == unlist(Info[nrow(Info),Version]),T,F)]
    Info[-nrow(Info),Same_Param := ifelse(Check == Info[nrow(Info),Check],T,F)]
    
    if(sum(Info$Same_Name,na.rm=T)>0 & sum(Info$Same_Param,na.rm=T)>0){
      if(file.exists(paste0("Output/GP_All/Results/",Info[Same_Param==T][1]$Version,"/Misc/DT_Opti.csv"))==F |
         file.exists(paste0("Output/GP_All/Results/",Info[Same_Param==T][1]$Version,"/Misc/Ownership.csv"))==F |
         file.exists(paste0("Output/GP_All/Results/",Info[Same_Param==T][1]$Version,"/Misc/Observed.csv"))==F){
        Info <- Info[which(Same_Param!=T | is.na(Same_Param))]
      }
    }

    if(sum(Info$Same_Name,na.rm=T)>0 & sum(Info$Same_Param,na.rm=T)>0){
      Info <- Info[-nrow(Info)]
    } 
    if(sum(Info$Same_Name,na.rm=T)==0 & sum(Info$Same_Param,na.rm=T)>=0){
      #dir.create(file.path(paste0("Output/GP_All/Results/",unit_version)), showWarnings = FALSE,recursive = T)
      dir.create(file.path(paste0("Output/GP_All/Results/",unit_version,"/Misc")), showWarnings = FALSE,recursive = T)
      dir.create(file.path(paste0("Output/GP_All/Results/",unit_version,"/Results_Raw")), showWarnings = FALSE,recursive = T)
      DT_Opti <- Data_Preparation(df_full = df_year_full,
                                  Unit_Param = Unit_Parameters[Version==unit_version],
                                  Data_Selec = data_selection)
    }
    # if(sum(Info$Same_Name,na.rm=T)==0 & sum(Info$Same_Param,na.rm=T)>0){
    #   To_Run[Unit == unit_version]$Unit <- Info[Same_Name==T]$Version
    #   Info <- Info[-nrow(Info)]
    # }
    if(sum(Info$Same_Name,na.rm=T)>0 & sum(Info$Same_Param,na.rm=T)==0){
      To_Run[Unit == unit_version, Old_Unit_Name := Unit]
      To_Run[Unit == unit_version]$Unit <- paste(unit_version,ncol(To_Run),sep="_")
      Unit_Parameters[Version == unit_version]$Version <- paste(unit_version,ncol(To_Run),sep="_")
      Info[ncol(Info)]$Version <- paste(unit_version,ncol(To_Run),sep="_")
      unit_version <- paste(unit_version,ncol(To_Run),sep="_")
      DT_Opti <- Data_Preparation(df_full = df_year_full,
                                  Unit_Param = Unit_Parameters[Version==unit_version],
                                  Data_Selec = data_selection)    
      }
    fwrite(Info[,1:ncol(Unit_Parameters)],paste0("Output/GP_All/Results/Info_Parameters.csv"))
    print(paste0("Data preparation - ",unit_version," done"))
  }
  
  print("Data preparation - Done")
  
  # Scenario part (within Unit_Parameters folder)
  for(unit_version in unique(To_Run$Unit)){
    if(file.exists(paste0("Output/GP_All/Results/",unit_version,"/Scenarios.csv"))==F){
      Scenario <- Scenario_Parameters[1][,Status := ""][,Time := ""]
      fwrite(Scenario[0,],paste0("Output/GP_All/Results/",unit_version,"/Scenarios.csv"))
    }
    
    for(scenar in To_Run[Unit==unit_version]$Scenario){
      skip_to_next <- F
      tryCatch({ptm_Loop <- proc.time()
        print(paste0("Optim: ", unit_version," - ",scenar))
        Scenarios <- fread(paste0("Output/GP_All/Results/",unit_version,"/Scenarios.csv"))
        Scenarios <- rbind(Scenarios,Scenario_Parameters[Scenario==scenar][,c("Status","Time"):=list("","")],fill=T)
        
        Scenarios$Check <- do.call(paste,c(Scenarios[,2:(ncol(Scenarios)-2)],sep="_"))
        Scenarios[-nrow(Scenarios),Same_Name := ifelse(Scenario == unlist(Scenarios[nrow(Scenarios),Scenario]),T,F)]
        Scenarios[-nrow(Scenarios),Same_Param := ifelse(Check == Scenarios[nrow(Scenarios),Check],T,F)]
        
        if(sum(Scenarios$Same_Name,na.rm=T)>0 & sum(Scenarios$Same_Param,na.rm=T)>0){
          Scenarios <- Scenarios[-nrow(Scenarios)]
          # Verif that outputs are there (updating if missing DT_Result, mainly)
          if(file.exists(paste0("Output/GP_All/Results/",unit_version,"/",scenar,"_Results.csv"))==F){
            DT_Result <- Optim(Unit_Param = Unit_Parameters[Version==unit_version],
                               Scenario_Param = Scenario_Parameters[Scenario == scenar],
                               Model_Param = Model_Parameters[Gurobi_Method==To_Run[Scenario==scenar & Unit==unit_version,Model]])
            temp <- fread(paste0("Output/GP_All/Results/",unit_version,"/Results_Raw/",scenar,"_Raw.csv"))
            Scenarios[Scenario==scenar]$Time <- paste0(floor(as.numeric(temp[Variable=="Run_Time",value])/60),"min",round(as.numeric(temp[Variable=="Run_Time",value])%%60),"s")
          }
        }
        if(sum(Scenarios$Same_Name,na.rm=T)==0 & sum(Scenarios$Same_Param,na.rm=T)==0){
          DT_Result <- Optim(Unit_Param = Unit_Parameters[Version==unit_version],
                             Scenario_Param = Scenario_Parameters[Scenario == scenar],
                             Model_Param = Model_Parameters[Gurobi_Method==To_Run[Scenario==scenar & Unit==unit_version,Model]])
          temp <- fread(paste0("Output/GP_All/Results/",unit_version,"/Results_Raw/",scenar,"_Raw.csv"))
          Scenarios[Scenario==scenar]$Time <- paste0(floor(as.numeric(temp[Variable=="Run_Time",value])/60),"min",round(as.numeric(temp[Variable=="Run_Time",value])%%60),"s")
        }
        if(sum(Scenarios$Same_Name,na.rm=T)==0 & sum(Scenarios$Same_Param,na.rm=T)>0){
          To_Run[Unit == unit_version & Scenario == scenar]$Scenario <- "Previously done (different name)"
          Scenarios <- Scenarios[-nrow(Scenarios)]
        }
        if(sum(Info$Same_Name,na.rm=T)>0 & sum(Info$Same_Param,na.rm=T)==0){
          To_Run[Unit == unit_version & Scenario == scenar]$Old_Scenario_Name <- To_Run[Unit == unit_version & Scenario == scenar]$Scenario
          To_Run[Unit == unit_version & Scenario == scenar]$Scenario <- paste(scenar,ncol(To_Run),sep="_")
          Scenario_Parameters[Scenario == scenar]$Scenario <- paste(scenar,ncol(To_Run),sep="_")
          
          Scenarios[ncol(Scenarios)]$Scenario <- paste(scenar,ncol(To_Run),sep="_")

          DT_Result <- Optim(Unit_Param = Unit_Parameters[Version==unit_version],
                             Scenario_Param = Scenario_Parameters[Scenario == scenar],
                             Model_Param = Model_Parameters[Gurobi_Method==To_Run[Unit==unit_version & Scenario==scenar]$Model])
          temp <- fread(paste0("Output/GP_All/Results/",unit_version,"/Results_Raw/",scenar,"_Raw.csv"))
          Scenarios[Scenario==scenar]$Time <- paste0(floor(as.numeric(temp[Variable=="Run_Time",value])/60),"min",round(as.numeric(temp[Variable=="Run_Time",value])%%60),"s")
        }
        Scenarios[Scenario==scenar]$Status <- "Done"
        fwrite(Scenarios[,1:(ncol(Scenarios)-3)],paste0("Output/GP_All/Results/",unit_version,"/Scenarios.csv"))},
        error = function(e) { skip_to_next <<- TRUE})
      if(skip_to_next) { next }
  }
  }
}

