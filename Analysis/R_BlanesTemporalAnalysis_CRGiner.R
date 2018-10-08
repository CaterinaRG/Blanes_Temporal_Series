####### Functions used for Giner et al. #######
### 'Quantifying long-term recurrence in planktonic microbial eukaryotes'

library(vegan)
library(ggplot2)
library(tidyverse)
library(gtools)
library(plyr)
library(EcolUtils) 
library(grid)
library(gtable) 


### Load OTU table (containing pico and nano fractions)
OTU <- read.table("OTU_table99_PN_noMetNuclChar_DEF2.txt", header=TRUE, sep="\t", row.names=1)
head(OTU)
names(OTU)
dim(OTU)

### Remove taxonomy column
### Change OTU table names
names(OTU)
colnames(OTU) <- c("JAN_04_P", "FEB_04_P", "MAR_04_P",  "APR_04_P",  "MAY_04_P",  "JUN_04_P",	"JUL_04_P",	"AUG_04_P",	"SEP_04_P",	"OCT_04_P",	"NOV_04_P",	"DEC_04_P",	"JAN_05_P",	"FEB_05_P",	"MAR_05_P",	"APR_05_P",	"MAY_05_P",	"JUN_05_P",	"JUL_05_P",	"AUG_05_P",	"SEP_05_P",	"OCT_05_P",	"NOV_05_P",	"DEC_05_P",	"JAN_06_P",	"FEB_06_P",	"MAR_06_P",	"APR_06_P",	"MAY_06_P",	"JUN_06_P",	"JUL_06_P",	"AUG_06_P",	"SEP_06_P",	"OCT_06_P",	"NOV_06_P",	"DEC_06_P",	"JAN_07_P",	"FEB_07_P",	"MAR_07_P",	"APR_07_P",	"MAY_07_P",	"JUN_07_P",	"JUL_07_P",	"AUG_07_P",	"SEP_07_P",	"OCT_07_P",	"NOV_07_P",	"DEC_07_P",	"JAN_08_P",	"FEB_08_P",	"MAR_08_P",	"APR_08_P",	"MAY_08_P",	"JUN_08_P",	"JUL_08_P",	"AUG_08_P",	"SEP_08_P",	"OCT_08_P",	"NOV_08_P",	"DEC_08_P",	"JAN_09_P",	"FEB_09_P",	"MAR_09_P",	"APR_09_P",	"MAY_09_P",	"JUN_09_P",	"JUL_09_P",	"AUG_09_P",	"SEP_09_P",	"OCT_09_P",	"NOV_09_P",	"DEC_09_P",	"JAN_10_P",	"FEB_10_P",	"MAR_10_P",	"APR_10_P",	"MAY_10_P",	"JUN_10_P", "JUL_10_P",	"AUG_10_P",	"SEP_10_P",	"OCT_10_P",	"NOV_10_P",	"DEC_10_P",	"JAN_11_P",	"FEB_11_P",	"MAR_11_P",	"APR_11_P",	"MAY_11_P",	"JUN_11_P",	"JUL_11_P",	"AUG_11_P",	"SEP_11_P",	"OCT_11_P",	"NOV_11_P",	"DEC_11_P",	"JAN_12_P",	"FEB_12_P",	"MAR_12_P",	"APR_12_P",	"MAY_12_P",	"JUN_12_P",	"JUL_12_P",	"AUG_12_P",	"SEP_12_P",	"OCT_12_P",	"NOV_12_P",	"DEC_12_P",	"JAN_13_P",	"FEB_13_P",	"MAR_13_P",	"APR_13_P",	"MAY_13_P",	"JUN_13_P",	"JUL_13_P",	"AUG_13_P",	"SEP_13_P",	"OCT_13_P",	"NOV_13_P",	"DEC_13_P",	"JAN_04_N",	"FEB_04_N",	"APR_04_N",	"MAY_04_N",	"JUN_04_N",	"JUL_04_N",	"AUG_04_N",	"SEP_04_N", "OCT_04_N",	"NOV_04_N",	"DEC_04_N",	"JAN_05_N",	"MAR_05_N",	"APR_05_N",	"MAY_05_N",	"JUN_05_N", "JUL_05_N",	"AUG_05_N", "SEP_05_N",	"OCT_05_N",	"NOV_05_N",	"DEC_05_N",	"JAN_06_N",	"MAR_06_N",	"APR_06_N",	"MAY_06_N",	"JUN_06_N",	"JUL_06_N",	"AUG_06_N",	"SEP_06_N",	"OCT_06_N",	"NOV_06_N",	"DEC_06_N",	"JAN_07_N",	"FEB_07_N",	"MAR_07_N",	"APR_07_N",	"MAY_07_N",	"JUN_07_N",	"JUL_07_N",	"AUG_07_N",	"SEP_07_N",	"OCT_07_N",	"NOV_07_N",	"DEC_07_N",	"JAN_08_N",	"FEB_08_N",	"MAR_08_N",	"APR_08_N",	"MAY_08_N",	"JUN_08_N",	"JUL_08_N",	"AUG_08_N",	"SEP_08_N",	"OCT_08_N",	"NOV_08_N",	"DEC_08_N",	"JAN_09_N",	"FEB_09_N",	"MAR_09_N",	"APR_09_N",	"MAY_09_N",	"JUN_09_N",	"JUL_09_N",	"AUG_09_N",	"SEP_09_N",	"NOV_09_N",	"DEC_09_N",	"JAN_10_N",	"FEB_10_N",	"MAR_10_N",	"APR_10_N",	"AUG_12_N",	"SEP_12_N",	"OCT_12_N",	"NOV_12_N",	"DEC_12_N",	"JAN_13_N",	"FEB_13_N",	"MAR_13_N",	"APR_13_N",	"MAY_13_N",	"JUN_13_N",	"JUL_13_N",	"AUG_13_N",	"SEP_13_N",	"OCT_13_N",	"NOV_13_N",	"DEC_13_N")
head(OTU)

### Make subtable for pico and for nano fraction
Pico  <- OTU[,1:120]
head(Pico)
dim (Pico)

Nano <- OTU[,121:209]
head(Nano)
dim(Nano)
names(Nano)


### Transpose tables (for vegan)
pico_t <- t(Pico)
nano_t <- t(Nano)

OTU_t <- t(OTU) # table with pico+nano


### Normalization (subsampling OTU tables)
# picot_rarefy7553 <- rrarefy(pico_t, 7553)
# picot_rarefy7553_nocero <- picot_rarefy7553[,-(which(colSums(picot_rarefy7553) == 0))] # remove OTUs with cero value
# picot_rarefy7553_nocero2 <- as.data.frame(picot_rarefy7553_nocero)

# OTU_t_rarefy5898<-rrarefy(OTU_t, 5898)
# OTU_t_rarefy5898_nocero <- OTU_t_rarefy5898[,-(which(colSums(OTU_t_rarefy5898) == 0))] # remove OTUs with cero value
# Divide OTUt subsampled to 5898 to pico and nano subtables

# Note: Use already normalized OTUtables 


#################################
####   Environmental data   #####
#################################
metadata_all <- read.table("EnvironmentalData_2004_2013_all.txt", header= TRUE, sep="\t", row.names=1)
head(metadata_all)
names(metadata_all)

#Select pico & nano
metadata_pico  <- metadata_all[1:120,]
head(metadata_pico)
dim (metadata_pico)

metadata_nano <- metadata_all[121:209,]
dim(metadata_nano)
head(metadata_nano)

#Select variables
metadata_pico_short <- metadata_pico%>%
  select(Temp, Secchi_depth, Salinity_CTD, CHL, PO4, NH4, NO2, NO3, SI, Day_lenght)
names(metadata_pico_short)

metadata_nano_short <- metadata_nano%>%
  select(Temp, Secchi_depth, Salinity_CTD, CHL, PO4, NH4, NO2, NO3, SI, Day_lenght)
names(metadata_nano_short)

# scale:
metadata_pico_short_scale <- as.data.frame(scale(metadata_pico_short))
metadata_nano_short_scale <- as.data.frame(scale(metadata_nano_short))



#################################
###      ALPHA DIVERSITY     ####
#################################

# Richness
richness_pico <-estimateR(picot_rarefy5898_nocero)
richness_nano <-estimateR(nanot_rarefy5898_nocero)

# Shannon Index
Shannon_pico <- diversity(picot_rarefy5898_nocero, index="shannon", MARGIN=1, base=exp(1))
Shannon_pico2 <- as.data.frame(Shannon_pico) #transform to data frame

Shannon_nano <- diversity(nanot_rarefy5898_nocero, index="shannon", MARGIN=1, base=exp(1))
Shannon_nano2 <- as.data.frame(Shannon_nano)


# Plots - Figure 5
pdf(file="Figure5_Richess_and_shannon.pdf", useDingbats=FALSE, width=4, heigh=4)
par(mfrow=c(1,1)) 

boxplot(richness_boxplot, xaxt="n", ylim=c(0,1400), ylab="Richness (Num of OTUs)", col=c("burlywood3"), las=2)
axis(side=1, cex=2.1, at=1:12,labels=c("JAN", "FEB", "MAR", "APR", "MAY","JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"), las=2)

boxplot(richness_boxplot_nano, xaxt="n", ylim=c(0,1400), col=c("cornsilk3"), las=2)
axis(side=1, cex=2.1, at=1:12,labels=c("JAN", "FEB", "MAR", "APR", "MAY","JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"), las=2)

boxplot(shannon_pico2$x ~ metadata_pico$Month, col=c("burlywood3"), xaxt="n", ylab="Shannon Index", las=2, ylim=c(1,7))
axis(side=1, cex=2.1, at=1:12,labels=c("JAN", "FEB", "MAR", "APR", "MAY","JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"), las=2)

boxplot(shannon_nano2$x ~ metadata_nano$Month, col=c("cornsilk3"), xaxt="n", las=2, ylim=c(1,7))
axis(side=1, cex=2.1, at=1:12,labels=c("JAN", "FEB", "MAR", "APR", "MAY","JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"), las=2)
dev.off()


##############################
###     Beta Diversity     ###
##############################
# Bray Curtis Distance:
OTUt_rarefy.bray <- as.matrix(vegdist(OTU_t_rarefy5898_nocero, method="bray"))
picot_rarefy.bray <- as.matrix(vegdist(picot_rarefy7553_nocero, method="bray"))
nanot_rarefy.bray<-as.matrix(vegdist(nanot_rarefy5898_nocero, method="bray")) #poso el as.matrix per tal de després poder exportar la taula amb el write.table

# NMDS
NMDS_all <- metaMDS(OTUt_rarefy.bray,autotransform=F)
NMDS_pico <- metaMDS(picot_rarefy.bray,autotransform=FALSE)
NMDS_nano <- metaMDS(nanot_rarefy.bray,autotransform=FALSE)

# Plot - Figure S4
pdf(file="Figure_S4_NMDS_PicoandNano.pdf", useDingbats=FALSE) # pointsize=15  -> pq augmenti la mida del punt
par(mfrow=c(1,1))
plot(NMDS_all$points, pch=c(19, 17)[as.numeric(metadata_all$Size)], cex=1.2, las=1, col=c("cadetblue3", "darkolivegreen2", "goldenrod1","lightsalmon3")[metadata_all$Season_real2], xlim=c(-0.6,0.6), ylim=c(-0.5, 0.5))
legend("topleft",c("Pico", "Nano"),pch=c(17, 19),inset=0.01, cex=1.17)
legend("bottomleft",c("Winter", "Spring", "Summer", "Autumn"),col=c("cadetblue3", "darkolivegreen2", "goldenrod1","lightsalmon3"), pch=c(19),inset=0.01, cex=1.14)
dev.off()


# Environmental fitting pico and nano (Envfit)
Envfit_pico_subs7553 <- envfit(NMDS_pico ~ Day_length + Temp + Secchi_depth + Salinity_CTD + CHL + PO4 + NH4 + NO2 + NO3 + SI, data = metadata_pico_short_scale, na.rm=TRUE, permu=999)
Envfit_nano_subs5898 <- envfit(NMDS_nano ~ Day_length + Temp + Secchi_depth + Salinity_CTD + CHL + PO4 + NH4 + NO2 + NO3 + SI, data = metadata_nano_short_scale, na.rm=TRUE, permu=999)


# Plots - Figure 4
pdf(file="Figure_4_NMDS_PICO_envfit.pdf", useDingbats=FALSE) # poso aquí tots els gràfics PICO
par (mfrow = c(1,1)) 
plot(NMDS_pico$points, pch=c(19), cex= 1.2, col=c("cadetblue3", "darkolivegreen3", "goldenrod1","lightsalmon3")[metadata_pico$Season_real2], xlim=c(-0.5,0.5), las=1)
plot(Envfit_pico_subs7553, cex = 0.8, col=c("black"))
legend("bottomleft", c("Winter", "Spring", "Summer", "Autumn"),pch=c(19),col=c("cadetblue3", "darkolivegreen3", "goldenrod1","lightsalmon3"), inset=0.01, cex=1.14)

plot(NMDS_nano$points, pch=c(19), cex=1.2, col=c("cadetblue3", "darkolivegreen3", "goldenrod1","lightsalmon3")[metadata_nano$Season_real2], xlim=c(-0.5,0.5), ylim=c(-0.7,0.4), las=1)
plot(Envfit_nano_subs5898, cex = 0.7, col=c("black"))
legend("bottomleft", c("Winter", "Spring", "Summer", "Autumn"),pch=c(19),col=c("cadetblue3", "darkolivegreen3", "goldenrod1","lightsalmon3"), inset=0.01, cex=1.14)
dev.off()


### Bray-Curtis dissimilarity between months
# Use BrayCurtis distance table to run it, without row names and column names
file <- read.table("BrayCurtis_PICO_subs7553_nocero_forScriptLagMonth.txt",header=FALSE)
dim(file)
values <- vector("numeric", length = 119) #(number of samples -1)
ocurrences <- vector ("numeric", length = 119)

for (row in 1:(nrow(file)-1) ) {                
  initialCol = 1 + row                          
  for (col in initialCol:ncol(file) ) {     
    if(!invalid(file[row,col]))           
    {
      lag = col - row                            
      values[lag] = values[lag] + file[row, col] 
      ocurrences[lag] = ocurrences[lag]+1       
    }
  }
}

average <- vector ("numeric", length = 119)
for (i in 1:119 ) {
  if(ocurrences[i] != 0 )
  {
    average[i] = values[i]/ocurrences[i]
  }
  else
  {
    average[i] = 0
  }
}

average

# Plot - Figure 3
pdf("Braycurtis_PICO_and_NANO_dissimilarityBC.pdf", useDingbats=FALSE, paper="a4")
BC_pico <- ggplot(plot_dist_BC_pico, aes(x = position, y=AverageBC)) +
  geom_point(size=1.5) +  #si faig size=1/num_OTUs fa la inversa. per tant com menys OTUs punt més gran
  geom_line(linetype="dashed", size=0.5) +
  scale_x_continuous(breaks=seq(0, 120, 12)) +
  scale_y_continuous(limits=c(0.6,1.0),breaks=seq(0.1, 1.0, 0.1)) +
  xlab("Time lag (months)") +
  ylab("Average BrayCurtis dissimiarlity") +
  theme_classic() +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'), axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14)) + 
  theme(axis.title.x = element_text(size=17)) +
  theme(axis.title.y = element_text(size=17))
BC_pico

BC_nano <- ggplot(plot_dist_BC_nano, aes(x = position, y=averageBC)) +
  geom_point(size=1.5) +  
  geom_line(linetype="dashed", size=0.5) +
  scale_x_continuous(breaks=seq(0, 80, 12)) +
  scale_y_continuous(limits=c(0.6,1.0),breaks=seq(0.1, 1.0, 0.1)) +
  xlab("Time lag (months apart)") +
  ylab("Average BrayCurtis dissimiarlity") +
  theme_classic() +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'), axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.title.x = element_text(size=17)) +
  theme(axis.title.y = element_text(size=17))
BC_nano
dev.off()




##############################
###   Recurrence Index     ###
##############################

# See Recurrence Index repository https://github.com/CaterinaRG/Recurrence-Index 
# Function:
seasonality.test<-function(comm.tab,n=1000,probs=c(0.025, 0.975),lag.max=120,na.action=na.pass) 
{
  require(vegan)
  
  season.index<-function(x){
    acf.all<-apply(as.matrix(x),2,acf,plot=F,lag.max=lag.max,na.action=na.pass)
    acf.all<-sapply(acf.all,"[[",1)
    apply(acf.all,2,function(x) sum(abs(x)))
  }
  
  n<-n
  season.index.real<-season.index(comm.tab)
  
  names(season.index.real) <- colnames(comm.tab)
  season.index.simul<-matrix(NA, ncol = dim(comm.tab)[2],nrow = n)
  for (i in 1:n) {
    season.index.simul[i, ]<-season.index(comm.tab[sample(1:nrow(comm.tab)),])
  }
  colnames(season.index.simul) <- colnames(comm.tab)
  season.index.simul <- as.data.frame(season.index.simul)
  media <- apply(season.index.simul, 2, mean, na.rm=TRUE) #na.rm pq funcioni amb NA
  ci <- apply(season.index.simul, 2, quantile, probs = probs, na.rm=TRUE) #na.rm pq funcioni amb NA
  resultats <- data.frame(observed = season.index.real, mean.simulated = media,lowCI = ci[1, ], uppCI = ci[2, ], sign = NA)
  for (j in 1:dim(resultats)[1]) {
    if (resultats$observed[j] > resultats$uppCI[j]) 
      resultats$sign[j] <- "SIGNIFICANTLY HIGHER"
    if (resultats$observed[j] < resultats$lowCI[j]) 
      resultats$sign[j] <- "SIGNIFICANTLY LOWER"
    if (resultats$observed[j] >= resultats$lowCI[j] & resultats$observed[j] <= 
        resultats$uppCI[j]) 
      resultats$sign[j] <- "NON SIGNIFICANT"
  }
  resultats$sign <- as.factor(resultats$sign)
  resultats
}


SI_allgroups <- seasonality.test(all_groups,n=1000)


# Autocorrelations - acf
pdf("Autocorrelations_all_RecurrenceIndex.pdf")
par(mfrow=c(3,3))
for (i in 2:length(all_groups_SI)) {
  acf(all_groups_SI[,i], lag.max=110, main = names(all_groups_SI[i]))
}
dev.off()


## Figure 1a and 2a (same script for pico and nano)
# Use abundance data of the different groups
pico_abundance <- read.table("OTUtable_PICO_subs7553_plotabundance_ab0.1perc_frR.txt", header=TRUE, sep="\t")
head(pico_abundance)

pico_groups <- data.frame(group=factor((pico_abundance$Group),levels=c("MALV-I", "Dinoflagellata", "MALV-II", "Mamiellophyceae", "Ciliophora", 
                                                                "Cryptomonadales", "MAST-3", "Diatomea", "Picozoa", "Acantharia", "MALV-III",
                                                                "Cercozoa","MAST-4", "Labyrinthulomycetes","Chrysophyceae", "Katablepharidae", "Telonema",
                                                                "MAST-7","Pelagophyceae", "Chlororendrophyceae", "Dictyochophyceae",  "Choanomonada", "MAST-1",
                                                                "Chlorarachniophyta","Centrohelida", "Ichthyosporea", "MOCH-2","MAST-12",
                                                                "Prasinophyceae", "MALV-V", "MAST-9", "BasalFungi", "Bolidomonas", "MAST-8", "Bicosoecida", "MAST-2", "MAST-11",
                                                                "MOCH-5", "Gracilipodida", "Polycystinea", "Peronosporomycetes")),
                        #percent=c(pico_abundance$abundance),prop=factor(pico_abundance$seasonality), levels=c("si","no"))
                        percent=c(pico_abundance$abundance),prop=factor(pico_abundance$seasonality))


group_abundance_PICO <- ggplot(data=pico_groups, aes(x=group, y=percent, fill=prop)) + geom_bar(stat="identity", position="identity") + 
  theme_classic() +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
  coord_cartesian(expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=12)) +
  scale_y_continuous(limits=c(0,25), breaks=seq(0,25,5)) +
  scale_fill_manual(values=c("lightgoldenrod2","chocolate2")) +
  xlab("") +
  ylab("Relative abundance (%)") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1, size=12)) +
  theme(legend.position="NULL") # sense llegenda

pdf("Figure_1a_PICO.pdf", width=9, heigh=6)
par(mfrow=c(1,1))
group_abundance_PICO
dev.off()


## Figure 1c and 2b (same script for pico and nano)
barplot_3_allOTUs <- read.table("OTUs_Seasonality_Bygroup_barplot_frR.txt", header=TRUE, sep="\t")
head(barplot_3_allOTUs)
str(barplot_3_allOTUs)

# Barplot reads
SeasonalNo_3_allOTUs <- data.frame(group=factor((barplot_3_allOTUs$Group),levels=c("MALV-I","Dinoflagellata", "MALV-II", "Mamiellophyceae",
                                                                                        "Ciliophora", "Cryptomonadales", "MAST-3", "Diatomea", "Picozoa", "Acantharia",
                                                                                        "MALV-III", "Cercozoa", "MAST-4","Labyrinthulomycetes","Chrysophyceae","Katablepharidae",
                                                                                        "Telonema", "MAST-7", "Pelagophyceae", "Chloroendrophyceae",
                                                                                        "Dictyochophyceae", "Choanomonada", "MAST-1",
                                                                                        "Chlorarachniophyta","Centrohelida", "Ichthyosporea",
                                                                                        "MOCH-2", "MAST-12", "Prasinophyceae",
                                                                                        "MALV-V", "MAST-9", "BasalFungi", "Bolidomonas",
                                                                                        "MAST-8", "Bicosoecida", "MAST-2", "MarineOpisthokonts", "MAST-11", "MOCH-5", "Gracilipodida",
                                                                                        "Polycystinea", "Peronosporomycetes")),
                                   percent=c(barplot_3_allOTUs$abundanceReads),prop=factor((barplot_3_allOTUs$seasonality),levels=c("SEASONAL","NoSeasonal")))

estacionalitat_grups_OTUs_4 <- ggplot(data=SeasonalNo_3_allOTUs, aes(x=group, y=percent, fill=prop)) + geom_bar(stat="identity", position="identity") + 
  theme_classic() +
  scale_fill_manual(values=c("chocolate2","lightgoldenrod2")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12)) +
  scale_y_continuous(limits=c(-100,100),breaks=seq(-100, 100, 10))+
  coord_cartesian(expand=c(0,0))+
  theme(legend.text = element_text(size=6),legend.position=c(1.0,1.0),legend.title=element_blank())+
  theme(plot.margin=unit(c(0.5,1.5,1,0.5),"cm"))+
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
  xlab("") +
  ylab("Reads (%)") +
  theme(legend.position="NULL")


# Dotplot OTUs - OTUs amb valor absolut
num_OTUs_all_2 <- data.frame(Chl=c( 33, 44, 30, 10, 16, 10, 8, 2, 7, 1, 15, 3, 3, 3, 7, 0, 3, 6, 3, 0, 7, 3, 4, 5, 1, 1, 3, 1, 2, 4, 1, 0, 3, 3, 1, 0, 0, 1, 0, 0, 1, 0, -267,-328,-308,-75,-123,-30,-57,-41,-20,-16,-22,-66,-12,-17,-23,-10,-16,-6,-8,-5,-22,-22,-12,-11,-5,-4,-5,-7,-12,-5,-12,-3,-6,-7,-4,-5,-4,-1,-4,-1,-5,-1),
                             meses_phyto=c(0.5, 1.5,  2.5,  3.5,  4.5,  5.5,	7,	8,	9.5,	10.5,	11.5,	12.5,	13.5,	14.5,	15.5,	17,	18.5,	19.5,	20.5,	21.5,	22.5,	23.5,	25.5,	26.5,	27.5,	28.5,	29.5,	30.5,	31.5,	32.5,	33.5,	34.5,	35.5,	36.5, 37.5, 38.5, 39.5, 40.5, 41.5, 42.5, 43.5, 44.5, 0.5, 1.5,  2.5,  3.5,  4.5,  5.5,	7,	8,	9.5,	10.5,	11.5,	12.5,	13.5,	14.5,	15.5, 17,	18.5,	19.5,	20.5,	21.5,	22.5,	23.5,	25.5,	26.5,	27.5,	28.5,	29.5,	30.5,	31.5,	32.5,	33.5,	34.5,	35.5,	36.5, 37.5, 38.5, 39.5, 40.5, 41.5, 42.5, 43.5, 44.5))

plot_punts_all_2 <- ggplot(num_OTUs_all_2, aes(x=meses_phyto, y=Chl))+geom_point(colour="darkgreen", shape=18)+
  theme_classic()+ 
  labs(x="",y=expression("Num OTUs"))+theme(panel.background = element_rect(fill = NA))+
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
  scale_y_continuous(limits=c(-100,100),breaks=seq(-100,100,10))+
  scale_x_continuous(limits=c(0,46),breaks=seq(0,46,5))+
  coord_cartesian(expand=c(0,0))+  #coord_cartesian(expand=c(0,10))+
  theme(plot.margin=unit(c(0.5,1.5,1,0.5),"cm"))+
  theme(legend.text = element_text(size=14),legend.position=c(0.2,0.5),legend.title=element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=12))+
  geom_hline(yintercept=0)

plot_punts_all_2

# Put both plots together
g1 <- ggplot_gtable(ggplot_build(estacionalitat_grups_OTUs_4))
g2 <- ggplot_gtable(ggplot_build(plot_punts_all_2))

pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
g <- gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b)

grid.draw(g)

pdf("Figure_1c_PICO_OTUandReads.pdf", useDingbats=FALSE, width=7, heigh=5)
par(mfrow=c(1,1))
grid.draw(g)
dev.off()
