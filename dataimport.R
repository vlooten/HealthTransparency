library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

# Transparency national database ####
list_files <- list.files(path = "./data")
list_files <- list_files[grep("csv",list_files)]
list_names <- gsub(pattern = "declaration_",replacement = "",list_files)
list_names <- as.vector(unlist(lapply(strsplit(list_names,split = "_"), `[[`, 1)))
for(kk in 1:length(list_files)){
  if(list_names[kk]=="entreprise"){
    eval(parse(text = paste0(list_names[kk]," <- read_delim('./data/",list_files[kk],"', ',', escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c') )" )))
  }else{
    eval(parse(text = paste0(list_names[kk]," <- read_delim('./data/",list_files[kk],"', ';', escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c') )" )))
  }
  
}
# National database physicians ####

medecins_national <- read.csv2("./data/Extraction201708170826.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
# medecins_national <- medecins_national[which(medecins_national$"Code.profession"==10),]
medecins_national <- medecins_national[,c("Identifiant.PP","Identification.nationale.PP","Code.savoir.faire")]
colnames(medecins_national) <- c("RPPS","id_nat","benef_specialite_code_off")
medecins_national$benef_specialite_code_off <- paste0("[",medecins_national$benef_specialite_code_off,"]")
medecins_national$benef_specialite_code_off <- ifelse(medecins_national$benef_specialite_code_off=="[]",NA,medecins_national$benef_specialite_code_off)

# avantage$benef_identifiant_valeur[which(avantage$identifiant_type=="AUTRE")]
correctif <- unique(read.csv2(file = "./data/liste_med.csv",stringsAsFactors = FALSE))
colnames(correctif) <- c("benef_identifiant_valeur", "valid", "correctif")

# Datamanagement dates ####
avantage$avant_date_signature <- dmy(avantage$avant_date_signature)
convention$conv_date_signature <- dmy(convention$conv_date_signature)
convention$conv_date_debut <- dmy(convention$conv_date_debut)
convention$conv_date_fin <- dmy(convention$conv_date_fin)
remuneration$remu_date <- dmy(remuneration$remu_date)

# Merge all tables ####
# colnames(avantage)
# [1] "entreprise_identifiant"         "denomination_sociale"           "ligne_identifiant"             
# [4] "ligne_rectification"            "benef_categorie_code"           "categorie"                     
# [7] "benef_nom"                      "benef_prenom"                   "benef_qualite_code"            
# [10] "qualite"                        "benef_adresse1"                 "benef_adresse2"                
# [13] "benef_adresse3"                 "benef_adresse4"                 "benef_codepostal"              
# [16] "benef_ville"                    "benef_pays_code"                "pays"                          
# [19] "benef_titre_code"               "benef_titre_libelle"            "benef_specialite_code"         
# [22] "benef_speicalite_libelle"       "benef_identifiant_type_code"    "identifiant_type"              
# [25] "benef_identifiant_valeur"       "benef_etablissement"            "benef_etablissement_codepostal"
# [28] "benef_etablissement_ville"      "benef_denomination_sociale"     "benef_objet_social"            
# [31] "ligne_type"                     "avant_date_signature"           "avant_montant_ttc"             
# [34] "avant_nature"                   "avant_convention_lie"           "semestre"
colnames(avantage)[which(colnames(avantage)=="avant_montant_ttc")] <- "montant"
colnames(avantage)[which(colnames(avantage)=="avant_date_signature")] <- "date"
# colnames(remuneration)
# [1] "entreprise_identifiant"         "denomination_sociale"           "ligne_identifiant"             
# [4] "ligne_rectification"            "benef_categorie_code"           "categorie"                     
# [7] "benef_nom"                      "benef_prenom"                   "benef_qualite_code"            
# [10] "qualite"                        "benef_adresse1"                 "benef_adresse2"                
# [13] "benef_adresse3"                 "benef_adresse4"                 "benef_codepostal"              
# [16] "benef_ville"                    "benef_pays_code"                "pays"                          
# [19] "benef_titre_code"               "benef_titre_libelle"            "benef_specialite_code"         
# [22] "benef_speicalite_libelle"       "benef_identifiant_type_code"    "identifiant_type"              
# [25] "benef_identifiant_valeur"       "benef_etablissement"            "benef_etablissement_codepostal"
# [28] "benef_etablissement_ville"      "benef_denomination_sociale"     "benef_objet_social"            
# [31] "ligne_type"                     "remu_date"                      "remu_montant_ttc"              
# [34] "remu_convention_liee"   
colnames(remuneration)[which(colnames(remuneration)=="remu_montant_ttc")] <- "montant"
colnames(remuneration)[which(colnames(remuneration)=="remu_date")] <- "date"
#
remuneration <- remuneration[,which(colnames(remuneration) %in% colnames(avantage))]
remuneration$"benef_codepostal" <- as.character(remuneration$"benef_codepostal")
# remuneration$benef_identifiant_valeur
avantage$benef_identifiant_valeur <- as.character(avantage$benef_identifiant_valeur)
avantage <- bind_rows(avantage,remuneration)
# Recup info entreprises
entreprise <- entreprise[,c("identifiant", "pays_code", "pays", "secteur_activite_code", 
                            "secteur","code_postal")]
colnames(entreprise) <- c("entreprise_identifiant", "ent_pays_code", "ent_pays", "ent_secteur_code", 
                          "ent_secteur", "ent_code_postal")
avantage <- left_join(avantage,entreprise, by="entreprise_identifiant")

avantage$benef_identifiant_valeur <- as.character(avantage$benef_identifiant_valeur)
# Load dictionnaries ####
BENEF_CATEGORIE <- data.frame(matrix(c("[PRS]","professionnels de santé","Healthcare professionnals","Healthcare professionnals",
                                       "[APS]","associations de professionnels de santé","Healthcare professionnals associations","Associations",
                                       "[ETU]","étudiants","Students or associations of students","Students",
                                       "[AUS]","associations d'usagers du système de santé","Patients and caregivers associations","Associations",
                                       "[ETA]","établissements de santé","Hospitals","Hospitals",
                                       "[FON]","fondations","Foundations","Associations",
                                       "[PRE]","Presse","Press","Press",
                                       "[LOG]","editeur de logiciel","Softwares editors","Institutions and compagnies",
                                       "[PMO]","Personnes morales","Institutions and compagnies","Institutions and compagnies",
                                       "[ADU]","associations d'étudiants","Students or associations of students","Associations",
                                       "[VET]","vétérinaires","Veterinary","Others",
                                       "[SAN]", "groupements de défense sanitaire","Associations","Associations",
                                       "[AGR]","professionnels agricoles","Agricultural professions","Others"), ncol=4, byrow=T), stringsAsFactors = F)
colnames(BENEF_CATEGORIE) <- c("BENEF_CATEGORIE","LIB_BENEF_CATEGORIE","LIB_BENEF_CATEGORIE2","LIB_BENEF_CATEGORIE3")
avantage <- left_join(avantage, BENEF_CATEGORIE, by = c("benef_categorie_code"="BENEF_CATEGORIE"))

avantage$benef_qualite_code <- ifelse(avantage$benef_categorie_code=="[PRS]",avantage$benef_qualite_code,NA)
BENEF_QUALITE <- data.frame(matrix(c("[10]", "Médecin", "Medical doctors",
                                     "[40]", "Chirurgien-dentiste", "Dental surgeon",
                                     "[50]", "Sage-femme", "Midwife",
                                     "[21]", "Pharmacien","Pharmacist",
                                     "[01]", "Préparateur en pharmacie et préparateur en pharmacie hospitalière", "Others",
                                     "[60]", "Infirmier", "Nurse",
                                     "[70]", "Masseurkinésithérapeute", "physiotherapist",
                                     "[80]", "Pédicurepodologue", "Others",
                                     "[94]", "Ergothérapeute","Others",
                                     "[96]", "Psychomotricien","Others",
                                     "[91]", "Orthophoniste","Others",
                                     "[92]", "Orthoptiste","Others",
                                     "[98]", "Manipulateur d’électroradiologie médicale", "Others",
                                     "[86]", "Technicien de laboratoire médical", "Others",
                                     "[05]", "Audioprothésiste","Hearing care professional",
                                     "[28]", "Opticien-lunetier","Optician",
                                     "[82]", "Prothésiste et orthésiste pour l’appareillage des personnes handicapées","Others",
                                     "[95]", "Diététicien", "Others",
                                     "[02]", "Aide soignant","Others",
                                     "[03]", "Auxiliaire de puériculture","Others",
                                     "[04]", "Ambulancier","Others",
                                     "[ADE]", "Assistant dentaire", "Others"),ncol=3, byrow=T ),stringsAsFactors = F)
colnames(BENEF_QUALITE) <- c("BENEF_QUALITE","LIB_BENEF_QUALITE","LIB_BENEF_QUALITE2")
avantage <- left_join(avantage,BENEF_QUALITE,by=c("benef_qualite_code"="BENEF_QUALITE"))

BENEF_TITRE <- data.frame(matrix(c("[PR]", "Professeur", "Professor", 
                                   "[MG]", "Médecin Général", "Doctor",
                                   "[PG]", "Pharmacien Général","Doctor",
                                   "[PC]","Pharmacien Chef","Doctor",
                                   "[MC]","Médecin chef","Doctor",
                                   "[DR]", "Docteur","Doctor",
                                   "[AUTRE]", "Autre","Others"),ncol=3, byrow=T), stringsAsFactors = F)
colnames(BENEF_TITRE) <- c("BENEF_TITRE","LIB_BENEF_TITRE","LIB_BENEF_TITRE2")

BENEF_SPECIALITE <- data.frame(matrix(c("[SM05]", "Chirurgie générale","General surgery",
                                        "[SM04]", "Cardiologie et maladies vasculaires","Cardiology",
                                        "[SM02]",  "Anesthesie-réanimation","Anesthesiology and resuscitation",
                                        "[SM08]", "Chirurgie orthopédique et traumatologie", "Orthopedic surgery and traumatology",
                                        "[SM03]", "Biologie médicale", "Pathology",
                                        "[SM01]", "Anatomie et cytologie pathologiques","Pathology",
                                        "[SM13]", "Chirurgie vasculaire","Thoracic and Vascular surgery",
                                        "[SM11]", "Chirurgie thoracique et cardio-vasculaire","Thoracic and Vascular surgery",
                                        "[SM07]", "Chirurgie maxillo-faciale et stomatologie","Oral and maxillofacial surgery",
                                        "[SM10]", "Chirurgie plastique reconstructrice et esthétique","Plastic surgery",
                                        "[SCD02]", "Chirurgie Orale","Oral and maxillofacial surgery",
                                        "[SM12]", "Chirurgie urologique","Urology",
                                        "[SCD01]","Orthopédie dento-faciale" ,"Oral and maxillofacial surgery",
                                        "[SM06]", "Chirurgie maxillo-faciale","Oral and maxillofacial surgery",
                                        "[SM09]", "Chirurgie infantile","Pediatrics surgery",
                                        "[SCD03]", "Médecine Bucco-Dentaire","Dentists",
                                        "[SM14]","Chirurgie viscérale et digestive","Colorectal surgery",
                                        "[SM15]","Dermatologie et vénéréologie","Dermatology",
                                        "[SM16]","Endocrinologie et métabolisme","Endocrinology",
                                        "[SM17]","Génétique médicale","Pediatrics and genetics",
                                        "[SM18]","Gériatrie","Geriatrics",
                                        "[SM19]","Gynécologie médicale","Obstetrics-gynecology",
                                        "[SM20]","Gynécologie obstétrique","Obstetrics-gynecology",
                                        "[SM21]","Hématologie","Hematology",
                                        "[SM22]","Hématologie (option Maladie du sang)","Hematology",
                                        "[SM23]","Hématologie (option Oncohématologie)","Hematology",
                                        "[SM24]","Gastroentérologie et hépatologie","Hepatology and gastroenterology",
                                        "[SM25]","Médecine du travail","Public health and occupationnal health",
                                        "[SM26]","Qualifié en Médecine Générale","General medicine",
                                        "[SM27]","Médecine interne", "Internal medicine",
                                        "[SM28]","Médecine nucléaire","Nuclear medicine",
                                        "[SM29]","Médecine physique et réadaptation", "Physical and rehabilitation medicine",
                                        "[SM30]","Néphrologie", "Nephrology",
                                        "[SM31]","Neurochirurgie", "Neurosurgery",
                                        "[SM32]","Neurologie","Neurology",
                                        "[SM33]","Neuropsychiatrie","Psychiatry",
                                        "[SM34]","O.R.L et chirurgie cervico faciale" ,"ENT and ophtalmology",
                                        "[SM35]","Oncologie (option oncohématologie)","Hematology",
                                        "[SM36]","Oncologie option médicale","Oncology",
                                        "[SM37]","Oncologie option radiothérapie","Oncology",
                                        "[SM38]","Ophtalmologie","ENT and ophtalmology",
                                        "[SM39]","Oto-rhinolaryngologie","ENT and ophtalmology",
                                        "[SM40]","Pédiatrie","Pediatrics",
                                        "[SM41]","Pneumologie","Pneumology",
                                        "[SM42]","Psychiatrie","Psychiatry",
                                        "[SM43]","Psychiatrie option enfant & adolescent","Psychiatry",
                                        "[SM44]","Radiodiagnostic","Radiology",
                                        "[SM45]","Radiothérapie","Oncology",
                                        "[SM46]","Réanimation médicale","Anesthesiology and resuscitation",
                                        "[SM47]","Recherche médicale","Others",
                                        "[SM48]","Rhumatologie","Rheumatology",
                                        "[SM49]","Santé publique et médecine sociale","Public health and occupationnal health",
                                        "[SM50]","Stomatologie","Oral and maxillofacial surgery",
                                        "[SM51]","Gynécoobstétrique et gynécologie médicale option 1","Obstetrics-gynecology",
                                        "[SM52]","Gynécoobstétrique et gynécologie médicale option 2","Obstetrics-gynecology",
                                        "[SM53]","Spécialiste en Médecine Générale","General medicine",
                                        "[SM54]","Médecine Générale","General medicine",
                                        "[SM55]","Radiodiagnostic et RadioThérapie","Radiology",
                                        "[SM99]","ORL et ophtalmologie","ENT and ophtalmology",
                                        "[SP01]","Radiopharmacie","Pharmacy and clinical pharmacology",
                                        "[SP02]","Hygiène","Public health and occupationnal health",
                                        "[SP03]","Pharmacovigilance","Pharmacy and clinical pharmacology",
                                        "[SP04]","Hémovigilance","Hematology", 
                                        "[AUTRE]","Autre","Others",
                                        "[CEX22]","Obstetrics-gynecology","Obstetrics-gynecology",
                                        "[CEX24]","Obstetrics-gynecology","Obstetrics-gynecology",
                                        "[CEX26]","Obstetrics-gynecology","Obstetrics-gynecology",
                                        "[CEX64]","Urology","Urology",
                                        "[FQ01]","Autre","Others", 
                                        "[FQ03]","Autre","Others",
                                        "[PAC00]","Autre","Others",
                                        "[SCH51]","Autre","Others"), ncol=3, byrow=T),stringsAsFactors = F)
colnames(BENEF_SPECIALITE) <- c("BENEF_SPECIALITE","LIB_BENEF_SPECIALITE","LIB_BENEF_SPECIALITE2")

SECTEUR <- data.frame(matrix(c("[AUT]","Autres", "Others",
  "[DM]","Dispositifs médicaux", "Medical Devices",
  "[DMDIV]","Dispositifs médicaux de diagnostic in vitro", "Medical Devices", 
  "[MED]","Médicaments", "Pharma",
  "[PA]","Prestataires associés", "Others",
  "[PC]","Produits cosmétiques", "Cosmetic products"
), ncol=3, byrow=T),stringsAsFactors = F)
colnames(SECTEUR) <- c("ent_secteur_code","ent_secteur","ent_secteur_lab")

avantage <- left_join(avantage,SECTEUR[, c("ent_secteur_code","ent_secteur_lab")],by="ent_secteur_code")

# On supprime les lignes où il y a eu une rectification ####
avantage <- avantage[which(avantage$ligne_rectification=="N"),]
avantage$montant <- as.numeric(avantage$montant)

# Physician table ####
# valid 0: no RPPS format, valid 1: RPPS format, valide 3 : no french doctor
medecins <- avantage %>% filter(benef_qualite_code == "[10]", benef_categorie_code=="[PRS]")
medecins <- as.data.frame(medecins)
medecins$benef_identifiant_valeur <- gsub("[[:space:]]","",medecins$benef_identifiant_valeur)
medecins <- left_join(medecins,correctif,by="benef_identifiant_valeur")
medecins$valid[which(is.na(medecins$benef_identifiant_valeur))] <- 0
medecins$valid[which(medecins$benef_identifiant_valeur=="0")] <- 0
medecins$valid[which(medecins$benef_identifiant_valeur=="1")] <- 0
medecins$valid[which(medecins$benef_identifiant_valeur=="99999999999")] <- 0
medecins$valid[which(!medecins$benef_pays_code=="[FR]")] <- 3
medecins$valid[which(is.na(medecins$valid))] <- 0
# Correctif RPPS
medecins$benef_identifiant_valeur_corr <- ifelse(medecins$valid==2,medecins$correctif,medecins$benef_identifiant_valeur)
medecins$identification <- 0
medecins$identification[which(medecins$benef_identifiant_valeur_corr %in% medecins_national$RPPS |
                                medecins$benef_identifiant_valeur %in% medecins_national$id_nat)] <- 1

# Database med OK ####
medecins_ok <- medecins %>% filter(medecins$benef_pays_code=="[FR]",year(medecins$date) %in% c("2014","2015","2016"),identification==1)
medecins_ok$spe_code <- medecins_ok$benef_specialite_code
medecins_ok$benef_specialite_code <- NULL
medecins_national_ok <- read.csv2("./data/Extraction201708170826.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
colnames(medecins_national_ok)[c(2,3,12)] <- c("RPPS","id_nat","benef_specialite_code_off")
medecins_national_ok$benef_specialite_code_off <- paste0("[",medecins_national_ok$benef_specialite_code_off,"]")
medecins_national_ok$benef_specialite_code_off <- ifelse(medecins_national_ok$benef_specialite_code_off=="[]",NA,medecins_national_ok$benef_specialite_code_off)
medecins_national_ok <- medecins_national_ok %>%  filter(Code.profession==10)

medecins_ok <- left_join(medecins_ok,unique(medecins_national_ok[!duplicated(medecins_national_ok[,2]),c(2,12)]), by=c("benef_identifiant_valeur_corr"="RPPS"))
medecins_ok<- left_join(medecins_ok,BENEF_SPECIALITE, by=c("benef_specialite_code_off"="BENEF_SPECIALITE"))

# dput(names(table(medecins_ok$benef_specialite_code_off[which(is.na(medecins_ok$LIB_BENEF_SPECIALITE2))])))

# Very difficult to determine sexe with only firstname ####
# dico_firstnames <- read.csv("./data/nat2015.txt", sep="\t", encoding = "WINDOWS-1252")[,c(1,2)] #Source INSEE firstname FRANCE
# dico_firstnames <- unique(dico_firstnames)
# dico_firstnames$preusuel <- iconv(dico_firstnames$preusuel, to='ASCII//TRANSLIT')
# dico_firstnames[22567,]
# medecins_ok$benef_prenom <- toupper(iconv(medecins_ok$benef_prenom, to='ASCII//TRANSLIT') )
# medecins_ok$benef_prenom <- gsub("\\s+","-",medecins_ok$benef_prenom)
# View(medecins_ok[which(! (medecins_ok$benef_prenom %in% dico_firstnames$preusuel)),])
