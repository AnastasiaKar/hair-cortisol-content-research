---
title: "examples"
output: word_document
---


```{r }

library(haven)
path = file.path(getwd(), "Data_n449_150618.sav")
variables = read_sav(path)
sample_dates = read.delim("sample_dates2.5mo_checked.txt")[,-c(2:9)]
variables<-merge.data.frame(variables,sample_dates,by = "ID_LAPSI")


# deleting replicate samples

variables2 <- variables[!variables$ID_ajosta == 27743,]
variables2 <- variables2[!variables2$ID_ajosta == 37863,]
variables2 <- variables2[!variables2$ID_ajosta == 39133,]

#-------------------------------------------------------------------------------------------

filtered_variables<-subset(variables2, filter_HCC24 == 1)


#---------------------------------------------------------------------------------------------

# Selecting quantitative & ordinal categorical variables for Spearman test,
# binary & nominal variables removed
# Quantitative variables collection 


quantitative_variables <- data.frame(empty =rep(NA,nrow(filtered_variables)))
for(i in 1:ncol(filtered_variables)){
  if((class(filtered_variables[[i]]) == "numeric" || class(filtered_variables[[i]]) == "integer") && sum(!is.na(filtered_variables[[i]])) > 0){
    quantitative_variables<-cbind(quantitative_variables,filtered_variables[,i])
    colnames(quantitative_variables)<-c(colnames(quantitative_variables)[1:ncol(quantitative_variables)-1],colnames(filtered_variables[i]))
  }
}

drops<-c("ID_ajosta","Description", "L_SUKUP","JAKO", "Season_24", "per8_3lk_rp1",
         "per13_rp1","per20_a1_rp1","per20_a2_rp1","per20_a3_rp1","per20_a4_rp1","per20_a5_rp1",
         "per20_a6_rp1","per20_a7_rp1","per20_a8_rp1","per21_rp1","hassle1a_rp1",
         "hassle1b_rp1","hassle2a_rp1","hassle2b_rp1","hassle3a_rp1","hassle3b_rp1",
         "hassle4a_rp1","hassle4b_rp1","hassle5a_rp1","hassle5b_rp1","hassle6a_rp1",
         "hassle6b_rp1","hassle1a_rp2","RVERENVUO","RVERENPAI","RENNENAIK",
         "RMUUSYY","hassle1b_rp2","hassle2a_rp2","hassle2b_rp2","hassle3a_rp2","hassle3b_rp2",
         "hassle4a_rp2","hassle4b_rp2","hassle5a_rp2","hassle5b_rp2","hassle6a_rp2",
         "hassle6b_rp2","RASEKTIO","RSOKERI_P","RINSULIIN","RANEMIA","RKORTIKOS",
         "hassle1a_rp3","hassle1b_rp3","hassle2a_rp3","hassle2b_rp3","hassle3a_rp3",
         "hassle3b_rp3", "hassle4a_rp3","hassle4b_rp3","hassle5a_rp3","hassle5b_rp3",
         "hassle6a_rp3", "hassle6b_rp3","BNSQ8_4lk_rp1","BNSQ9_4lk_rp1","BNSQ15A_4lk_rp1",
         "BNSQ8_4lk_rp2","BNSQ9_4lk_rp2","BNSQ15A_4lk_rp2","BNSQ8_4lk_rp3",
         "BNSQ9_4lk_rp3","BNSQ15A_4lk_rp3",
         "rp1ä_on","rp2ä_on","rp3ä_on","SGAC","kk3ä_on","kk6ä_on","HCC24ON",
         "HCC40ON","HCC24ja40","HCC40_4luokkainen",
         "HCC24_4luokkainen","BATCH_24_numeric","BATCH_40_numeric",
         "ei_biologisetsisarukset_kk3","kuolleen",
         "BNSQ_sleepiness2_2_rp1","BNSQ_sleepiness2_2_rp2",
         "BNSQ_sleepiness2_2_rp3","freezertime_categ24",
         "freezertime_categ40","vk24_9_2lk","rv24_categ","SASFYKSIA","LVALVONTA",
         "LRESPIRAA","LANTIBIOO","LKVITAMII","LBCG_ROKO","vointi1_kk3","filter_HCC24","Season_40",
         "vointi2_kk3","vointi3_kk3","vointi4_kk3","vointi5_kk3","vointi6_kk3","IMETYKSENkesto_kategorinen")
quantitative_variables2<-quantitative_variables[ , !(names(quantitative_variables) %in% drops)]
quantitative_variables2<-quantitative_variables2[,-1]


#deleted categorical variables that were factors (ex. freezertime_categ24) + strongly inter-correlated variables (ex. freezertime_categ24 & freezertime24, BNSQ_sleepiness2_2_rp1 & BNSQ_sleepiness2_rp1),
# variables with insufficient data/variation for Spearman test with & without imputing (ex. kuolleen), also labels (ex. ID_ajosta)

#--------------------------------------------------------------------------------------

# Ordinal variables collection

ordinal_v_vector<-c(13,14,24:35,37:39,48:59,61:63,72:83,107,192,204,209,224,234,239:243,251,256:259) 
# per8_3lk_rp1, per13_rp1, hassle, BNSQ,IMETYKSENkesto_kategorinen,vk24_3,vk24_7,
# vk24_8,vk24_9, vk24_10,vk40_3,vk40_7,vk40_8,vk40_9, vk40_10, are ordinal variables

# are tupakoin, IMETYKSENkesto_kategorinen, vk24_3 , vk24_7,etc ordinal ? not sure

ordinal_variables <- as.data.frame(sapply(filtered_variables[,ordinal_v_vector], function(x) as.numeric(x)))
ordinal_variables_factor <- as.data.frame(sapply(filtered_variables[,ordinal_v_vector], function(x) as.factor(x)))


#------------------------------------------------------------------------------------------
# Nominal variables


# what is apgar_1m/5m ?
nominal_prefilered<-filtered_variables[,(! colnames(filtered_variables) %in% c(colnames(quantitative_variables2),colnames(ordinal_variables)))]
nominal_prefilered$Lääkekuuri[nominal_prefilered$Lääkekuuri == "ei tieto" | nominal_prefilered$Lääkekuuri == ""] <- NA
nominal_prefilered$avoliitt[nominal_prefilered$avoliitt == "" | nominal_prefilered$avoliitt == 9]<- NA
nominal_prefilered$D_Vit[nominal_prefilered$D_Vit == ""]<- NA
nominal_prefilered$Maitohappobakt[nominal_prefilered$Maitohappobakt == ""]<- NA
nominal_prefilered$Dimetikoni[nominal_prefilered$Dimetikoni == ""]<- NA
nominal_prefilered$Antibiootit[nominal_prefilered$Antibiootit == ""]<- NA



binary_factor<-as.data.frame(sapply(nominal_prefilered[,c("L_SUKUP","JAKO","per20_a1_rp1","per20_a2_rp1","per20_a3_rp1","per20_a4_rp1","per20_a6_rp1","per20_a7_rp1","per20_a8_rp1", "per21_rp1", "avoliitt","RASEKTIO","RSOKERI_P","RINSULIIN","RANEMIA","RKORTIKOS","RVERENVUO","RENNENAIK","RMUUSYY", "SASFYKSIA", "LVALVONTA","LRESPIRAA","LANTIBIOO","LKVITAMII","LBCG_ROKO","vointi1_kk3","vointi2_kk3","vointi3_kk3","vointi4_kk3","vointi5_kk3","vointi6_kk3", "Lääkekuuri","D_Vit","Maitohappobakt","Dimetikoni","Antibiootit","vk40_2","vk24_2","vk24_6","vk40_6" )], function(x) as.factor(x)))

binary_factor$birth_mode <- filtered_variables$synnyty1
binary_factor$birth_mode[binary_factor$birth_mode == "1"] <- 1
binary_factor$birth_mode[binary_factor$birth_mode == "3"] <- 1
binary_factor$birth_mode[binary_factor$birth_mode == "2"] <- 1
binary_factor$birth_mode[binary_factor$birth_mode == "4"] <- 1
binary_factor$birth_mode[binary_factor$birth_mode == "5"] <- 2
binary_factor$birth_mode[binary_factor$birth_mode == "6"] <- 2
binary_factor$birth_mode[binary_factor$birth_mode == "7"] <- 2
binary_factor$birth_mode[binary_factor$birth_mode == ""] <- 1


binary_factor$birth_mode <- as.factor(binary_factor$birth_mode)
binary_factor2<-cbind(binary_factor,filtered_variables[["HCC_24_ln"]])
names(binary_factor2)[names(binary_factor2) == 'filtered_variables[["HCC_24_ln"]]'] <- "HCC_24_ln"


#binary_factor2<-merge.data.frame(binary_factor,variables2[,c("L_SUKUP","HCC_24_ln")],by = "L_SUKUP" )
#gm_ibq <- binary_factor2[complete.cases(binary_factor2[,1:40]),]
#gm_ibq <- gm_ibq[complete.cases(gm_ibq[,41]),]


# wrote a function to assess complete cases between one continous variable and each binary factor since wilcox.test requires two vectors of the same length
equal_vector_lengths<-function(data_set,continous_v,factor_v){
  data_set2 <- data_set[complete.cases(data_set[,factor_v]),]
  data_set2 <- data_set2[complete.cases(data_set2[,continous_v]),]
  result<-wilcox.test(data_set2[[continous_v]] ~ data_set2[[factor_v]])
  return(result)
}
# drop binary_factor2[,14,17]

binary_wilcox<-lapply(names(binary_factor2[,c(1:13,15:16,18:41)]),function(x) equal_vector_lengths(binary_factor2,"HCC_24_ln",x))
#binary_wilcox<- lapply(gm_ibq[,c(1:2,10,13,15,20,27,29,30,34,35,37,38)], function(x) wilcox.test( gm_ibq$HCC_24_ln ~ x))



# adjusted p-values for results generated by running wilcox.test 
p_values<-sapply(binary_wilcox, function(x) x$p.value)
p.adj1 <- p.adjust(p_values, method = "BH")



# ran kruskal.test for nominal variables vs. cortisol concentration
nominal_factor<-as.data.frame(sapply(nominal_prefilered[,c("siviilis","synnyty1","vk24_1a","vk24_1b","vk24_4","vk24_5a","vk24_5b","vk40_1a","vk40_1b","vk40_4","vk40_5a","vk40_5b","Season_24","BATCH_24","Season_40","BATCH_40")], function(x) as.factor(x)))
nominal_factor$siviilis[nominal_factor$siviilis == ""]<-NA
nominal_factor$synnyty1[nominal_factor$synnyty1 == ""]<-NA

processed_samples2<-cbind(quantitative_variables2,ordinal_variables)
nominal_kruskal <- lapply(nominal_factor, function(x) kruskal.test(processed_samples2$HCC_24_ln ~ x))

p_values2<-sapply(nominal_kruskal, function(x) x$p.value)
p.adj12 <- p.adjust(p_values, method = "BH")


#------------------------------------------------------------------------------------------
# Spearman test applied to filtered quantitative variables & ordinal variables
# The test would allow to determine potential covariables that can be utilized in modelling more complex associations

ordinal_variables <-ordinal_variables[,-c(47,56:60)] #deleted vk40 variables because they
# couldn't be used in Spearman test directly, too little observations
processed_samples2<-cbind(quantitative_variables2,ordinal_variables)
spearmann_results2 <- lapply(processed_samples2, function(x) cor.test(x, processed_samples2$HCC_24_ln, method = "spearman",na.rm = T))
spearmann_cor2<-sapply(spearmann_results2, function(x) x$estimate)



negative_cor1<-spearmann_cor2[spearmann_cor2 <= -0.2]
positive_cor1<-spearmann_cor2[ (0.2 <= spearmann_cor2) & (spearmann_cor2 < 1) ]
names_cor2<-c(unlist(sapply(names(positive_cor1), function(x) strsplit(x,".rho"))),unlist(sapply(names(negative_cor1), function(x) strsplit(x,".rho"))))






processed_samples2$hassle1a_rp1<-as.factor(processed_samples2$hassle1a_rp1)
processed_samples2$hassle1a_rp2<-as.factor(processed_samples2$hassle1a_rp2)
processed_samples2$vk24_no<-as.factor(processed_samples2$vk24_no)
processed_samples2$freezertime_categ24<-as.factor(processed_samples2$freezertime_categ24)


#-----------------------------------------------------------------------------------------
library(phyloseq)
physeq <- import_biom(paste0(getwd(),"/otu_table_mc2_w_tax.biom"))
#View(as.data.frame(otu_table(physeq)))
#View(as.data.frame(tax_table(physeq)))


set.seed(30061993)
physeq.rarified <- rarefy_even_depth(physeq)
#hist(sample_sums(physeq),breaks = 200)
#hist(sample_sums(physeq)[sample_sums(physeq)< 100000],breaks = 100 )
#hist(sample_sums(physeq)[sample_sums(physeq)< 350000],breaks = 100 )
#View(as.data.frame(otu_table(physeq.rarified)))

filtered_variables$birth_mode <- filtered_variables$synnyty1
filtered_variables$birth_mode[filtered_variables$birth_mode == "1"] <- 1
filtered_variables$birth_mode[filtered_variables$birth_mode == "3"] <- 1
filtered_variables$birth_mode[filtered_variables$birth_mode == "2"] <- 1
filtered_variables$birth_mode[filtered_variables$birth_mode == "4"] <- 1
filtered_variables$birth_mode[filtered_variables$birth_mode == "5"] <- 2
filtered_variables$birth_mode[filtered_variables$birth_mode == "6"] <- 2
filtered_variables$birth_mode[filtered_variables$birth_mode == "7"] <- 2
filtered_variables$birth_mode[filtered_variables$birth_mode == ""] <- 1


filtered_variables$Season_24 <- as.factor(filtered_variables$Season_24)
filtered_variables$birth_mode <- as.factor(filtered_variables$birth_mode)
filtered_variables$Antibiootit <- as.factor(filtered_variables$Antibiootit)
filtered_variables$L_SUKUP<- as.factor(filtered_variables$L_SUKUP)
metadata<-sample_data(filtered_variables)
rownames(metadata)<-as.character(unlist(metadata[,4]))
rarif.ibq <- merge_phyloseq(physeq.rarified, metadata)

# calculation of diversity indexes
alpha_ibq <- estimate_richness(rarif.ibq, measures = c("Chao1", "Shannon"))
library(knitr)
kable(head(alpha_ibq))

alpha_ibq$ID_ajosta <- rownames(alpha_ibq)
library(stringr)
split <- str_split_fixed(alpha_ibq$ID_ajosta, "X", 2)
alpha_ibq$ID_ajosta <- split[,2]
alpha_ibq <- merge.data.frame(alpha_ibq, filtered_variables, by = "ID_ajosta")

#-------------------------------------------------------------------------------------------
alpha_ibq5 <- estimate_richness(rarif.ibq, measures = c("Chao1", "Shannon"))
library(knitr)
kable(head(alpha_ibq5))

alpha_ibq5$ID_ajosta <- rownames(alpha_ibq5)
library(stringr)
split <- str_split_fixed(alpha_ibq5$ID_ajosta, "X", 2)
alpha_ibq5$ID_ajosta <- split[,2]


#-------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------

quantitative_variables__filtered<-cbind(filtered_variables[,c("HCC_24_ln","ID_ajosta")],quantitative_variables2)

quantitative_variables__filtered<-merge.data.frame(alpha_ibq5,quantitative_variables__filtered,by = "ID_ajosta")
myvars<-names(quantitative_variables__filtered[,c(6:length(quantitative_variables__filtered))])

nominal_factor2<-merge.data.frame(alpha_ibq5,cbind(nominal_factor,filtered_variables[,c("HCC_24_ln","ID_ajosta")]), by = "ID_ajosta" )

ordinal_v_vector<-c(13,14,24:35,37:39,48:59,61:63,72:83,107,192,209,224,234,239:243,251,257:259) 

ordinal_variables_filtered <- as.data.frame(sapply(filtered_variables[,ordinal_v_vector], function(x) as.factor(x)))
ordinal_variables_filtered<-cbind(filtered_variables[,c("HCC_24_ln","ID_ajosta")],ordinal_variables_filtered)


ordinal_variables_filtered<-merge.data.frame(alpha_ibq5,ordinal_variables_filtered,by = "ID_ajosta")
myvars<-names(ordinal_variables_filtered[,c(6:63)])


model<-lm(Shannon ~ HCC_24_ln + äidinmaidon_kum_altist_3kk, data = quantitative_variables__filtered)
summary(model)

model<-lm(Shannon ~ HCC_24_ln + täysimetyksen_kesto_kk, data = quantitative_variables__filtered)
summary(model)

model<-lm(Chao1 ~ HCC_24_ln + Season_24, data = nominal_factor2)
summary(model)

model<-lm(HCC_24_ln ~ aiemmats, data = quantitative_variables__filtered)
summary(model)

model<-lm(Chao1 ~ HCC_24_ln + IKÄ_Ä_SYNT, data = quantitative_variables__filtered)
summary(model)

model<-lm(HCC_24_ln ~ freezertime_categ24, data = ordinal_variables_filtered)
summary(model)

cases<-alpha_ibq[complete.cases(alpha_ibq[,c("Chao1","Shannon", "HCC_24_ln","IKÄ_Ä_SYNT","Season_24","täysimetyksen_kesto_kk","äidinmaidon_kum_altist_3kk","L_SUKUP","sample_wk","birth_mode","Antibiootit", "freezertime_categ24","aiemmats")]),]
cases<-cases[,c("Chao1","Shannon", "HCC_24_ln","IKÄ_Ä_SYNT","Season_24","äidinmaidon_kum_altist_3kk", "täysimetyksen_kesto_kk","L_SUKUP","sample_wk","birth_mode","Antibiootit", "freezertime_categ24","aiemmats")]
cases$freezertime_categ24<-as.factor(cases$freezertime_categ24)
cases$Season_24<-as.factor(cases$Season_24)
cases$L_SUKUP<-as.factor(cases$L_SUKUP)
cases$birth_mode<-as.factor(cases$birth_mode)
cases$Antibiootit<-as.factor(cases$Antibiootit)

model_1 <- lm(Chao1 ~ HCC_24_ln  + Season_24   + IKÄ_Ä_SYNT + L_SUKUP + sample_wk  + äidinmaidon_kum_altist_3kk + birth_mode + Antibiootit + freezertime_categ24 + aiemmats + täysimetyksen_kesto_kk ,data = cases) 

model_2 <- lm(Shannon ~ HCC_24_ln  + Season_24   + IKÄ_Ä_SYNT + L_SUKUP + sample_wk  + äidinmaidon_kum_altist_3kk + birth_mode + Antibiootit + freezertime_categ24 + aiemmats + täysimetyksen_kesto_kk ,data = cases) 


step(model_1,scope = list(lower = ~ HCC_24_ln + L_SUKUP + sample_wk + birth_mode + Antibiootit))

step(model_2,scope = list(lower = ~ HCC_24_ln + L_SUKUP + sample_wk + birth_mode + Antibiootit))


model_1 <- lm(Shannon ~ HCC_24_ln  + Season_24  + IKÄ_Ä_SYNT  + sample_wk + äidinmaidon_kum_altist_3kk + birth_mode + Antibiootit + aiemmats,data = alpha_ibq) 
summary(model_1)

model_2 <- lm(Chao1 ~ HCC_24_ln  + Season_24  + IKÄ_Ä_SYNT + sample_wk + äidinmaidon_kum_altist_3kk + birth_mode + Antibiootit + aiemmats,data = alpha_ibq) 
summary(model_2)

```

