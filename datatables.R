getcompagnies <- function(x=c("2014","2015","2016")){
  somme_by_type <- avantage  %>% filter(year(avantage$date) %in% x ) %>% 
    # unite(col = med, benef_nom, benef_prenom, sep = " ") %>%
    group_by(denomination_sociale) %>%
    summarise(n = n(),
              TotalAmount = sum(montant, na.rm = TRUE),
              MinAmount = min(montant, na.rm = TRUE),
              MaxAmount = max(montant, na.rm = TRUE),
              MedianTransactionAmount=median(montant, na.rm=T),
              Q1=quantile(montant,probs = 0.25, na.rm=T),
              Q3=quantile(montant,probs = 0.75, na.rm=T),
              MeanTransactionAmount=round(TotalAmount/n,2)
    ) %>%
    arrange(desc(TotalAmount))
  return(somme_by_type)
}
compagnies_all <- getcompagnies()
write.csv2(compagnies_all,"top_compagnies.csv", row.names=F)

getsector <- function(x=c("2014","2015","2016")){
  somme_by_type <- avantage  %>% filter(year(avantage$date) %in% x ) %>% 
    # unite(col = med, benef_nom, benef_prenom, sep = " ") %>%
    group_by(ent_secteur_lab) %>%
    summarise(n = n(),
              TotalAmount = sum(montant, na.rm = TRUE),
              MinAmount = min(montant, na.rm = TRUE),
              MaxAmount = max(montant, na.rm = TRUE),
              MedianTransactionAmount=median(montant, na.rm=T),
              Q1=quantile(montant,probs = 0.25, na.rm=T),
              Q3=quantile(montant,probs = 0.75, na.rm=T),
              MeanTransactionAmount=round(TotalAmount/n,2)
    ) %>%
    arrange(desc(TotalAmount))
  return(somme_by_type)
}
sector_all <- getsector()
sector_2014 <- getsector("2014")
sector_2015 <- getsector("2015")
sector_2016 <- getsector("2016")
write.csv2(sector_all,"sector_all.csv", row.names=F)
write.csv2(sector_2014,"sector_2014.csv", row.names=F)
write.csv2(sector_2015,"sector_2015.csv", row.names=F)
write.csv2(sector_2016,"sector_2016.csv", row.names=F)
#
getcategory <- function(x=c("2014","2015","2016")){
  somme_by_type <- avantage  %>% filter(year(avantage$date) %in% x ) %>% 
    # unite(col = med, benef_nom, benef_prenom, sep = " ") %>%
    group_by(LIB_BENEF_CATEGORIE3) %>%
    summarise(n = n(),
              TotalAmount = sum(montant, na.rm = TRUE),
              MinAmount = min(montant, na.rm = TRUE),
              MaxAmount = max(montant, na.rm = TRUE),
              MedianTransactionAmount=median(montant, na.rm=T),
              Q1=quantile(montant,probs = 0.25, na.rm=T),
              Q3=quantile(montant,probs = 0.75, na.rm=T),
              MeanTransactionAmount=round(TotalAmount/n,2)
    ) %>%
    arrange(desc(TotalAmount))
  return(somme_by_type)
}
category_all <- getcategory()
category_2014 <- getcategory("2014")
category_2015 <- getcategory("2015")
category_2016 <- getcategory("2016")
write.csv2(category_all,"category_all.csv", row.names=F)
write.csv2(category_2014,"category_2014.csv", row.names=F)
write.csv2(category_2015,"category_2015.csv", row.names=F)
write.csv2(category_2016,"category_2016.csv", row.names=F)

#
avantage  %>% filter(year(avantage$date) %in% c("2014","2015","2016"), benef_categorie_code=="[PRS]", is.na(LIB_BENEF_QUALITE2)) %>% 
  View()
getpro <- function(x=c("2014","2015","2016")){
  somme_by_type <- avantage %>% mutate(LIB_BENEF_QUALITE2=ifelse(is.na(avantage$LIB_BENEF_QUALITE2),"Others",avantage$LIB_BENEF_QUALITE2)) %>% 
    filter(year(avantage$date) %in% x, benef_categorie_code=="[PRS]") %>% 
    # unite(col = med, benef_nom, benef_prenom, sep = " ") %>%
    group_by(LIB_BENEF_QUALITE2) %>%
    summarise(n = n(),
              TotalAmount = sum(montant, na.rm = TRUE),
              MinAmount = min(montant, na.rm = TRUE),
              MaxAmount = max(montant, na.rm = TRUE),
              MedianTransactionAmount=median(montant, na.rm=T),
              Q1=quantile(montant,probs = 0.25, na.rm=T),
              Q3=quantile(montant,probs = 0.75, na.rm=T),
              MeanTransactionAmount=round(TotalAmount/n,2)
    ) %>%
    arrange(desc(TotalAmount))
  return(somme_by_type)
}
pro_all <- getpro()
pro_2014 <- getpro("2014")
pro_2015 <- getpro("2015")
pro_2016 <- getpro("2016")
write.csv2(pro_all,"pro_all.csv", row.names=F)
write.csv2(pro_2014,"pro_2014.csv", row.names=F)
write.csv2(pro_2015,"pro_2015.csv", row.names=F)
write.csv2(pro_2016,"pro_2016.csv", row.names=F)

# Flow chart ####
# All transactions ####
sum(category_all$TotalAmount)
sum(category_all$n)
# Not individuals benefits ####
sum(category_all$TotalAmount[2:8], na.rm = T)
sum(category_all$n[2:8], na.rm = T)
# All health professionnals ####
sum(pro_all$TotalAmount)
sum(pro_all$n)
# Other health professionnals ####
sum(pro_all[2:10,"n"])
sum(pro_all[2:10,"TotalAmount"])
# Med all ####
pro_all[1,c("TotalAmount","n")]
medecins %>% filter(year(medecins$date) %in% c("2014","2015","2016"), !benef_pays_code=="[FR]") %>% summarise(amount=sum(montant,na.rm=T),n=n())
medecins %>% filter(year(medecins$date) %in% c("2014","2015","2016"), benef_pays_code=="[FR]") %>% summarise(amount=sum(montant,na.rm=T),n=n())
# French MD ####
medecins %>% filter(year(medecins$date) %in% c("2014","2015","2016"), benef_pays_code=="[FR]",identification==0) %>% summarise(amount=sum(montant,na.rm=T),n=n())
medecins %>% filter(year(medecins$date) %in% c("2014","2015","2016"), benef_pays_code=="[FR]",identification==1) %>% summarise(amount=sum(montant,na.rm=T),n=n())

# Describe id impossible vs possible ####

follow_global_amount <- medecins %>% mutate(year_mnt=year(medecins$date))%>% filter(year(medecins$date) %in% c("2014","2015","2016"), benef_pays_code=="[FR]") %>% 
  group_by(year_mnt,identification) %>% summarise(amount=sum(montant,na.rm=T)) %>% ungroup() %>% 
  spread(.,identification,amount) %>% rename("No"=`0`,"Yes"=`1`) %>% mutate(freqMiss=round(100*No/(No+Yes),1))
follow_global_transact <- medecins %>% mutate(year_mnt=year(medecins$date))%>% filter(year(medecins$date) %in% c("2014","2015","2016"), benef_pays_code=="[FR]") %>% 
  group_by(year_mnt,identification) %>% summarise(N=n() ) %>% ungroup() %>% 
  spread(.,identification,N) %>% rename("No"=`0`,"Yes"=`1`) %>% mutate(freqMiss=round(100*No/(No+Yes),1))
write.csv2(follow_global_amount,"follow_global_amount.csv", row.names=F)
write.csv2(follow_global_transact,"follow_global_transact.csv", row.names=F)

follow_evo_amount <- medecins %>% mutate(year_mnt=year(medecins$date))%>% filter(year(medecins$date) %in% c("2014","2015","2016"), benef_pays_code=="[FR]") %>% 
  group_by(year_mnt,identification,ent_secteur_lab) %>% summarise(amount=sum(montant,na.rm=T)) %>% ungroup() %>% 
  spread(.,identification,amount) %>% rename("No"=`0`,"Yes"=`1`) %>% mutate(freqMiss=round(100*No/(No+Yes),1))
follow_evo_transact <- medecins %>% mutate(year_mnt=year(medecins$date))%>% filter(year(medecins$date) %in% c("2014","2015","2016"), benef_pays_code=="[FR]") %>% 
  group_by(year_mnt,identification,ent_secteur_lab) %>% summarise(N=n() ) %>% ungroup() %>% 
  spread(.,identification,N) %>% rename("No"=`0`,"Yes"=`1`) %>% mutate(freqMiss=round(100*No/(No+Yes),1))
write.csv2(follow_evo_amount,"follow_evo_amount.csv", row.names=F)
write.csv2(follow_evo_transact,"follow_evo_transact.csv", row.names=F)


getspe <- function(x=c("2014","2015","2016")){
    somme_by_type <- medecins_ok %>% # mutate(LIB_BENEF_QUALITE2=ifelse(is.na(avantage$LIB_BENEF_QUALITE2),"Others",avantage$LIB_BENEF_QUALITE2)) %>% 
      filter(year(medecins_ok$date) %in% x) %>% 
      # unite(col = med, benef_nom, benef_prenom, sep = " ") %>%
      group_by(LIB_BENEF_SPECIALITE2) %>%
      summarise(n = n(),
                TotalAmount = sum(montant, na.rm = TRUE),
                MinAmount = min(montant, na.rm = TRUE),
                MaxAmount = max(montant, na.rm = TRUE),
                MedianTransactionAmount=median(montant, na.rm=T),
                Q1=quantile(montant,probs = 0.25, na.rm=T),
                Q3=quantile(montant,probs = 0.75, na.rm=T),
                MeanTransactionAmount=round(TotalAmount/n,2)
      ) %>%
      arrange(desc(TotalAmount))
    return(somme_by_type)
  }
spe_all <- getspe()
write.csv2(spe_all,"spe_all.csv", row.names=F)

