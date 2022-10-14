if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, vroom, here, sqldf, ggthemes, fixest, modelsummary, plm, GGally, ivreg)

dir <- list.files(recursive = TRUE,
                  pattern = "\\.txt$|\\.csv$",
                  full.names = TRUE)

pfs <- read_delim("PFS_update_data.txt", 
                  delim = "\t", escape_double = FALSE, 
                  trim_ws = TRUE)

taxid.base <- read_csv("PhysicianData_2009.csv") %>%
  select(npi,group1) %>%
  mutate(npi = as.character(npi))

for (i in 2012:2017) {
  message(paste0("Merging data sets year:",i))
  #Get the path for the specific year i.
  dir.mdppas = dir[(grepl(i, dir, ignore.case = TRUE) & 
                      grepl("Physician", dir, ignore.case = TRUE))]
  dir.puf = dir[(grepl(i, dir, ignore.case = TRUE) & 
                   grepl("PUF", dir, ignore.case = TRUE))]
  
  #------
  #Read the MDPPAS data for the Year i. create int as in eq 1
  #------
  message("reading MDPPAS")
  a <- vroom(dir.mdppas)
  a$npi = as.character(a$npi) #Make sure npi has the same type 'character' on both data frames
  a <- a %>%
    select(npi, Year, pos_asc, pos_opd, pos_office, group1) %>%
    group_by(Year, npi) %>%
    mutate(
      int = ifelse( pos_opd / (pos_opd + pos_office + pos_asc) >= 0.75,1,0) #Create int variable for Q2
    ) %>%
    select(Year,npi,int, group1)
  
  #------
  #Read the PUF data for the Year i. Keep all MD observations. Collapse to Physician level; 1 observation per \{npi ~ Year\}
  #------
  
  message("Reading PUF")
  b <- vroom(dir.puf)
  names(b) = tolower(names(b))
  b <- b %>%
    select(npi, nppes_credentials, average_medicare_allowed_amt, line_srvc_cnt, bene_unique_cnt, hcpcs_code) %>%
    filter(grepl("MD|M.D.", nppes_credentials, ignore.case = TRUE)) %>%
    filter(!is.na(hcpcs_code)) %>%
    mutate(
      Total_Spending = average_medicare_allowed_amt*line_srvc_cnt, 
      Total_Claims = line_srvc_cnt, 
      Total_Patients = bene_unique_cnt
    )
  
  b_sum <- b %>%
    select(-hcpcs_code) %>%
    group_by(npi) %>%
    summarise(
      Total_Spending = sum(Total_Spending, na.rm = TRUE),
      Total_Claims   = sum(Total_Claims, na.rm = TRUE),
      Total_Patients = sum(Total_Patients, na.rm = TRUE)
    ) 
  
  if (i == 2012){
    pfs_12 <- pfs %>%
      filter(year == 2012)
    price.shock <- b %>% inner_join(taxid.base, by="npi") %>%
      inner_join(pfs_12 %>% 
                   select(hcpcs, dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007), 
                 by=c("hcpcs_code"="hcpcs")) %>%
      mutate_at(vars(dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007), replace_na, 0) %>%
      mutate(price_shock = case_when(
        i<=2013 ~ ((i-2009)/4)*dprice_rel_2010,
        i>2013  ~ dprice_rel_2010),
        denom = line_srvc_cnt*price_nonfac_orig_2010,
        numer = price_shock*line_srvc_cnt*price_nonfac_orig_2010) 
    
    price.shock_sum <- price.shock %>%
      group_by(npi) %>%
      summarize(phy_numer=sum(numer, na.rm=TRUE), phy_denom=sum(denom, na.rm=TRUE), tax_id=first(group1)) %>%
      ungroup() %>%
      mutate(phy_rev_change=phy_numer/phy_denom) %>%    
      group_by(tax_id) %>%
      summarize(practice_rev_change=sum(phy_rev_change, na.rm=TRUE)) %>%
      ungroup()
  } else{
    pfs_13 <- pfs %>%
      filter(year == 2013)
    price.shock <- b %>% inner_join(taxid.base, by="npi") %>%
      inner_join(pfs_13 %>% 
                   select(hcpcs, dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007), 
                 by=c("hcpcs_code"="hcpcs")) %>%
      mutate_at(vars(dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007), replace_na, 0) %>%
      mutate(price_shock = case_when(
        i<=2013 ~ ((i-2009)/4)*dprice_rel_2010,
        i>2013  ~ dprice_rel_2010),
        denom = line_srvc_cnt*price_nonfac_orig_2010,
        numer = price_shock*line_srvc_cnt*price_nonfac_orig_2010) 
    
    price.shock_sum <- price.shock %>%
      group_by(npi) %>%
      summarize(phy_numer=sum(numer, na.rm=TRUE), phy_denom=sum(denom, na.rm=TRUE), tax_id=first(group1)) %>%
      ungroup() %>%
      mutate(phy_rev_change=phy_numer/phy_denom) %>%    
      group_by(tax_id) %>%
      summarize(practice_rev_change=sum(phy_rev_change, na.rm=TRUE)) %>%
      ungroup()  
  }
  
  # Write the inner join of MDPPAS and PUF in Disk in rectangular form for Year i 
  # -> gives just the cases that we observe in both data sets
  dat <- inner_join(a,b_sum, by="npi")
  vroom_write(dat, paste0(here("dat_"),i,".csv"), delim = ",", col_names = TRUE)
  vroom_write(price.shock, paste0(here("price.shock.raw_"),i,".csv"), delim = ",", col_names = TRUE)
  vroom_write(price.shock_sum, paste0(here("price.shock.sum_"),i,".csv"), delim = ",", col_names = TRUE)
}

#------
# Append all the data.frames created in the loop in rectangular form
#------
dir <- list.files(recursive = TRUE,
                  pattern = "dat.*csv",
                  full.names = TRUE)
dat <- vroom(dir)
vroom_write(dat, file = here("dat.csv"), delim = ",")

for (i in 2012:2017){
  dir.shock.sum = dir[(grepl(i, dir, ignore.case = TRUE) & 
                         grepl("price.shock.sum", dir, ignore.case = TRUE))]
  
  shock.sum <- vroom(dir.shock.sum)
  shock.sum <- shock.sum %>%
    mutate(year = i)
  vroom_write(shock.sum, paste0(here("price.shock.sum2_"),i,".csv"), delim = ",", col_names = TRUE)
}


dir <- list.files(recursive = TRUE,
                  pattern = "price.shock.sum2.*csv",
                  full.names = TRUE)
price.shock.sum <- vroom(dir)
vroom_write(price.shock.sum, file = here("price.shock.sum.csv"), delim = ",")
