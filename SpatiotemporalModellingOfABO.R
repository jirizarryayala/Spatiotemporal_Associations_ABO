library(rstanarm)
library(readxl)
library(tidyverse)
library(ggridges)
library(bridgesampling)
library(bayesplot)
library(sjPlot)
library(ggplot2)
library(sf)
library(RColorBrewer)
library(glmnet)
library(gridExtra)
library(BMA)
library(ggcorrplot)
library(rstan)
library(mice)
library(rgeoda)
library(psych)
library(ggrepel)
library(FactoMineR)
library(factoextra)
library(MASS)
library(car)
library(RVAideMemoire)
library(corrplot)
set.seed(714417)

lbw=read.csv("C:/Users/jjiri/Downloads/lbw.csv")
ptb=read.csv("C:/Users/jjiri/Downloads/ptb.csv")

shp=read_sf("C:/Users/jjiri/Downloads/tl_2024_us_county/tl_2024_us_county.shp")
LAshp=shp[shp$STATEFP==22,]

CountyLBW=c()
YearLBW=c()
PctLBW=c()

for(county in 1:nrow(lbw)){
  CountyLBW=c(CountyLBW, rep(lbw[county, 1], 12))
  YearLBW=c(YearLBW, seq(2009, 2020))
  for(column in 3:14){
    PctLBW=c(PctLBW, lbw[county, column])
  }
}
lbwDF=data.frame(CountyLBW, YearLBW, PctLBW)
lbwDF$PctLBW[lbwDF$PctLBW=="*"]=NA

CountyPTB=c()
YearPTB=c()
PctPTB=c()

for(county in 1:nrow(ptb)){
  CountyPTB=c(CountyPTB, rep(ptb[county, 1], 12))
  YearPTB=c(YearPTB, seq(2009, 2020))
  for(column in 3:14){
    PctPTB=c(PctPTB, ptb[county, column])
  }
}
ptbDF=data.frame(CountyPTB, YearPTB, PctPTB)
ptbDF$PctPTB[ptbDF$PctPTB=="*"]=NA

lbwDF$PctLBW=as.numeric(lbwDF$PctLBW)
ptbDF$PctPTB=as.numeric(ptbDF$PctPTB)

lbwDF$CountyLBW[lbwDF$CountyLBW=="Desoto"]="De Soto"
lbwDF$CountyLBW[lbwDF$CountyLBW=="Lasalle"]="La Salle"

ptbDF$CountyPTB[ptbDF$CountyPTB=="Desoto"]="De Soto"
ptbDF$CountyPTB[ptbDF$CountyPTB=="Lasalle"]="La Salle"

#SDOH Data
sdoh2009=read_xlsx("C:/Users/jjiri/Downloads/SDOH_2009_COUNTY_1_0.xlsx", sheet = 2)
sdoh2009=sdoh2009[sdoh2009$STATE=="Louisiana",]
sdoh2010=read_xlsx("C:/Users/jjiri/Downloads/SDOH_2010_COUNTY_1_0.xlsx", sheet = 2)
sdoh2010=sdoh2010[sdoh2010$STATE=="Louisiana",]
sdoh2011=read_xlsx("C:/Users/jjiri/Downloads/SDOH_2011_COUNTY_1_0.xlsx", sheet = 2)
sdoh2011=sdoh2011[sdoh2011$STATE=="Louisiana",]
sdoh2012=read_xlsx("C:/Users/jjiri/Downloads/SDOH_2012_COUNTY_1_0.xlsx", sheet = 2)
sdoh2012=sdoh2012[sdoh2012$STATE=="Louisiana",]
sdoh2013=read_xlsx("C:/Users/jjiri/Downloads/SDOH_2013_COUNTY_1_0.xlsx", sheet = 2)
sdoh2013=sdoh2013[sdoh2013$STATE=="Louisiana",]
sdoh2014=read_xlsx("C:/Users/jjiri/Downloads/SDOH_2014_COUNTY_1_0.xlsx", sheet = 2)
sdoh2014=sdoh2014[sdoh2014$STATE=="Louisiana",]
sdoh2015=read_xlsx("C:/Users/jjiri/Downloads/SDOH_2015_COUNTY_1_0.xlsx", sheet = 2)
sdoh2015=sdoh2015[sdoh2015$STATE=="Louisiana",]
sdoh2016=read_xlsx("C:/Users/jjiri/Downloads/SDOH_2016_COUNTY_1_0.xlsx", sheet = 2)
sdoh2016=sdoh2016[sdoh2016$STATE=="Louisiana",]
sdoh2017=read_xlsx("C:/Users/jjiri/Downloads/SDOH_2017_COUNTY_1_0.xlsx", sheet = 2)
sdoh2017=sdoh2017[sdoh2017$STATE=="Louisiana",]
sdoh2018=read_xlsx("C:/Users/jjiri/Downloads/SDOH_2018_COUNTY_1_1.xlsx", sheet = 2)
sdoh2018=sdoh2018[sdoh2018$STATE=="Louisiana",]
sdoh2019=read_xlsx("C:/Users/jjiri/Downloads/SDOH_2019_COUNTY_1_0.xlsx", sheet = 2)
sdoh2019=sdoh2019[sdoh2019$STATE=="Louisiana",]
sdoh2020=read_xlsx("C:/Users/jjiri/Downloads/SDOH_2020_COUNTY_1_0.xlsx", sheet = 2)
sdoh2020=sdoh2020[sdoh2020$STATE=="Louisiana",]

sdoh2009$COUNTY=substr(sdoh2009$COUNTY, start=1, stop = nchar(sdoh2009$COUNTY)-7)
sdoh2010$COUNTY=sdoh2009$COUNTY
sdoh2011$COUNTY=sdoh2009$COUNTY
sdoh2012$COUNTY=sdoh2009$COUNTY
sdoh2013$COUNTY=sdoh2009$COUNTY
sdoh2014$COUNTY=sdoh2009$COUNTY
sdoh2015$COUNTY=sdoh2009$COUNTY
sdoh2016$COUNTY=sdoh2009$COUNTY
sdoh2017$COUNTY=sdoh2009$COUNTY
sdoh2018$COUNTY=sdoh2009$COUNTY
sdoh2019$COUNTY=sdoh2009$COUNTY
sdoh2020$COUNTY=sdoh2009$COUNTY

sdohFull=full_join(sdoh2009, sdoh2010)
sdohFull=full_join(sdohFull, sdoh2011)
sdohFull=full_join(sdohFull, sdoh2012)
sdohFull=full_join(sdohFull, sdoh2013)
sdohFull=full_join(sdohFull, sdoh2014)
sdohFull=full_join(sdohFull, sdoh2015)
sdohFull=full_join(sdohFull, sdoh2016)
sdohFull=full_join(sdohFull, sdoh2017)
sdohFull=full_join(sdohFull, sdoh2018)
sdohFull=full_join(sdohFull, sdoh2019)
sdohFull=full_join(sdohFull, sdoh2020)

#Variable Selection

lbwDF=left_join(lbwDF, sdohFull, by = join_by("CountyLBW"=="COUNTY", "YearLBW"=="YEAR"))
ptbDF=left_join(ptbDF, sdohFull, by = join_by("CountyPTB"=="COUNTY", "YearPTB"=="YEAR"))

dataset=ptbDF[,-c(4:8)]

varName=c()
naCount=c()
for(columnName in names(dataset)){
  naNumber=sum(is.na(dataset[,columnName]))
  varName=c(varName, columnName)
  naCount=c(naCount, naNumber)
}

#Figure 1
h=hist(naCount, plot=FALSE)
h$counts=cumsum(h$counts)
plot(NULL, xlim=c(0,800), ylim=c(0,1400), ylab="Frequency", xlab="Count of Missing Observations per Variables"
     , main="Stacked Histogram of Missing Observations per Variable")
abline(v=230.4, lty=2)
plot(h, col = "#FFFFFF", labels= TRUE, add=TRUE, cex=10)
hist(naCount, labels = TRUE, col ="darkturquoise", add=TRUE)
legend("topleft", legend=c("Cumulative Frequecy", "Bin Frequency", "Cut line for 30% Missingness"),
       fill = c("#FFFFFF", "darkturquoise"),
       border = c("black", "black"))


###Data Filtering
nums = unlist(lapply(dataset, is.numeric), use.names = FALSE)
nums[1]=TRUE # include County in nums for imputation
datasetClean=dataset[,nums]

keepVector=c()
for(variable in names(datasetClean)){
  percentMiss=sum(is.na(datasetClean[,variable]))/768*100
  if(percentMiss<=30){ #Select variables with less or equal to 30% missingness
    keepVector=c(keepVector,TRUE)
  } else{
    keepVector=c(keepVector, FALSE)
  }
}

datasetClean=datasetClean[,keepVector]

for(countyName in unique(datasetClean$CountyPTB)){
  countyDF<-datasetClean[datasetClean$CountyPTB==countyName,]
  for(variable in names(datasetClean[,-1])){
    if(sum(is.na(countyDF[,variable]))>0){
      countyMedian=median(countyDF[,variable], na.rm=TRUE)
      countyDF[,variable][is.na(countyDF[,variable])]<-countyMedian
      if(is.na(countyMedian)){
        countyDF[,variable][is.na(countyDF[,variable])]<-median(datasetClean[,variable], na.rm=TRUE)
      }
    }
  }
  datasetClean[datasetClean$CountyPTB==countyName,]<-countyDF
}

sum(complete.cases(datasetClean))==768


pcaResults=PCA(datasetClean[,-c(1:3,647)], ncp=9)
screeplot=fviz_screeplot(pcaResults, ylim=c(0,30), ncp=20, addlabels=TRUE)
screeplot+geom_vline(xintercept=9, linetype="dotted")+
  theme_minimal(base_size = 20)+ggtitle("Scree plot for AHRQ Data\nPrincipal Component Analysis") 

pcaValues=pcaResults$ind$coord

topN=200

PC1Contrib=fviz_contrib(pcaResults, choice = "var", axes = 1, top=10); PC1Contrib+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))#PC1 related to general population size
PC2Contrib=fviz_contrib(pcaResults, choice = "var", axes = 2, top=topN); PC2Contrib+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) #PC2 related to socioeconomics and ethnoracial demographics
PC3Contrib=fviz_contrib(pcaResults, choice = "var", axes = 3, top=topN); PC3Contrib+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) #PC3 also related to ethnoracial demographics
PC4Contrib=fviz_contrib(pcaResults, choice = "var", axes = 4, top=topN); PC4Contrib+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) #PC4 related to age demographics
PC5Contrib=fviz_contrib(pcaResults, choice = "var", axes = 5, top=topN); PC5Contrib+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) #PC5 related to environmental differences
PC6Contrib=fviz_contrib(pcaResults, choice = "var", axes = 6, top=topN); PC6Contrib+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) #PC6 also related to age demographics
PC7Contrib=fviz_contrib(pcaResults, choice = "var", axes = 7, top=topN); PC7Contrib+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) #PC7 related to gender breakdown and employment
PC8Contrib=fviz_contrib(pcaResults, choice = "var", axes = 8, top=topN); PC8Contrib+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) #PC8 related to healthcare utilization
PC9Contrib=fviz_contrib(pcaResults, choice = "var", axes = 9, top=topN); PC9Contrib+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) #PC9 related to disability demographics

PC2v3=fviz_contrib(pcaResults, choice = "var", axes = c(2,3), top=150); PC2v3

pointShapes=rep(c(0:6,8), 8)
fviz_pca_ind(pcaResults,axes=c(2,3), col.ind = datasetClean[,1], palette = rainbow(64), addEllipses = TRUE)+scale_shape_manual(values=pointShapes)

modelData=cbind(datasetClean[,1:3],PctLBW, pcaValues)

modelData$PctLBW[modelData$PctLBW=="*"]=NA
sum(complete.cases(modelData))
#Missing value imputation
modelData$PctLBW=as.numeric(modelData$PctLBW)
for(countyName in unique(modelData$CountyPTB)){
  variable="PctLBW"
  countyDF<-modelData[modelData$CountyPTB==countyName,]
  countyMedian=median(countyDF[,variable], na.rm=TRUE)
  countyDF[,variable][is.na(countyDF[,variable])]<-countyMedian
  if(is.na(countyMedian)){
        countyDF[,variable][is.na(countyDF[,variable])]<-median(modelData[,variable], na.rm=TRUE)
  }
  modelData[modelData$CountyPTB==countyName,]<-countyDF
}
sum(complete.cases(modelData))

names(modelData)=c("Parish", "Year", "PctPTB", "PctLBW", "PC1", "PC2", "PC3", "PC4",
                   "PC5", "PC6", "PC7", "PC8", "PC9")

#Data viz
medianLBW=c()
medianPTB=c()
for(parish in unique(modelData$Parish)){
  sample=modelData[modelData$Parish==parish,]
  countyMedianLBW=median(sample$PctLBW, na.rm=TRUE)
  countyMedianPTB=median(sample$PctPTB, na.rm=TRUE)
  medianLBW=c(medianLBW, countyMedianLBW)
  medianPTB=c(medianPTB, countyMedianPTB)
}

lbwOrder=data.frame(unique(CountyLBW), medianLBW)
lbwOrder=lbwOrder[order(lbwOrder$medianLBW),]

ptbOrder=data.frame(unique(CountyPTB), medianPTB)
ptbOrder=ptbOrder[order(ptbOrder$medianPTB),]

LAshp$NAME[LAshp$NAME=="LaSalle"]="La Salle"
#Ridgeline Plot for Low Birthweight by County
rdg1=ggplot(modelData, aes(x = PctLBW, y = fct_reorder(Parish,PctLBW), fill = Parish)) +
  geom_density_ridges() +
  xlab("Percent of Low Birthweight Births")+
  ylab("Parish")+
  theme_ridges(font_size=16)+
  theme(legend.position = "none")+
  labs(subtitle = bquote(bold("A")))

#Ridgeline Plot for Preterm Birth by County
rdg2=ggplot(modelData, aes(x = PctPTB, y = fct_reorder(Parish, PctPTB), fill = Parish)) +
  geom_density_ridges() +
  theme_ridges(font_size = 16)+
  xlab("Percent of Preterm Births")+
  ylab("Parish")+
  theme(legend.position = "none")+
  labs(subtitle = bquote(bold("C")))

#grid.arrange(rdg1, rdg2, ncol=2)

modelData$Year=factor(modelData$Year)

#Ridgeline Plot for Low Birthweight by Year
rdg3=ggplot(modelData, aes(x = PctLBW, y = Year, fill = Year)) +
  geom_density_ridges() +
  theme_ridges(font_size=16)+
  xlab("Percent of Low Birthweight Births")+
  ylab("Year")+
  theme(legend.position = "none")+
  labs(subtitle = bquote(bold("B")))

#Ridgeline Plot for Preterm Birth by Year
rdg4=ggplot(modelData, aes(x = PctPTB, y = Year, fill = Year)) +
  geom_density_ridges() +
  theme_ridges(font_size=16)+
  xlab("Percent of Preterm Births")+
  ylab("Year")+
  theme(legend.position = "none")+
  labs(subtitle = bquote(bold("D")))

grid.arrange(rdg1, rdg3, rdg2, rdg4, ncol=4, widths=c(4,3,4,3))



parishMedianLBW=data.frame(unique(modelData$Parish), medianLBW)
LAshpMedian=left_join(LAshp, parishMedianLBW, by = join_by("NAME"== unique.modelData.Parish.))

clp1=ggplot(LAshpMedian) +
  geom_sf(aes(fill = medianLBW)) +
  scale_fill_fermenter(n.breaks=5, palette = "Spectral")+
  theme_classic()+
  theme(legend.position = "inside", legend.position.inside=c(.83,.72))+
  labs(title ="Map of Louisiana Parishes Filled by Percent Median \nLow Birth Weight from 2009 - 2020",
       fill="Percent Median\nLow Birth Weight")

parishMedianPTB=data.frame(unique(modelData$Parish), medianPTB)
LAshpMedian1=left_join(LAshp, parishMedianPTB, by = join_by("NAME"== unique.modelData.Parish.))

clp2=ggplot(LAshpMedian1) +
  geom_sf(aes(fill = medianPTB)) +
  scale_fill_fermenter(n.breaks=5, palette = "Spectral")+
  theme_classic()+
  theme(legend.position = "inside", legend.position.inside=c(.83,.72))+
  labs(title ="Map of Louisiana Parishes Filled by Percent Median \nPreterm Birth from 2009 - 2020",
       fill="Percent Median\nPreterm Birth")

grid.arrange(clp1, clp2, ncol=2)


lbwMod=stan_glmer(PctLBW~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+(1|Parish)+(1|Year),data=modelData)

summary(lbwMod)
pp1=pp_check(lbwMod)
plot(lbwMod)
lbwModPlot=plot_model(lbwMod,  sort.est = TRUE)
lbwModPlot 
rePlotLBW=plot_model(lbwMod, type = "re", sort.est = TRUE)
reLBW=rePlotLBW$`(Intercept: Parish)` + ggtitle("Parish-level Random Effects for LBW")+
  geom_vline(xintercept = 0)
rePlotLBW$`(Intercept: Year)`
tab_model(lbwMod,digits=4, file = "C:/Users/jjiri/Desktop/lbwTab.html")
bR2LBW=bayes_R2(lbwMod)
median(bR2LBW)
quantile(bR2LBW, c(.025, 0.975))



ptbMod=stan_glmer(PctPTB~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+(1|Parish)+(1|Year),modelData)

ptbSum=summary(ptbMod)
pp2=pp_check(ptbMod)
plot(ptbMod)
plot_model(ptbMod, sort.est = TRUE)
rePlotPTB=plot_model(ptbMod, type = "re", sort.est = TRUE)
rePTB=rePlotPTB$`(Intercept: Parish)`+ ggtitle("Parish-level Random Effects for PTB")+
  geom_vline(xintercept = 0)
rePlotPTB$`(Intercept: Year)`
tab_model(ptbMod,digits=4, file = "C:/Users/jjiri/Desktop/ptbTab.html")
bR2PTB=bayes_R2(ptbMod)
median(bR2PTB)
quantile(bR2PTB, c(.025, 0.975))

grid.arrange(pp1, pp2, ncol=2)

grid.arrange(reLBW, rePTB , ncol =2)

LAshpMedian2=LAshpMedian1
##Interpretation

#PC2 vs. PC3
circ1=fviz_pca_var(pcaResults, col.var = "contrib",
             gradient.cols = c("skyblue", "darkblue"),
             axes=c(2,3),
             select.var = list(contrib=30),
             legend.position = "inside", legend.position.inside=c(.83,.72),
             repel = TRUE,
             title="Correlation Circle Between Principal Components 2 and 3",
             legend.title = "Contribution")

#PC1 vs. PC8
circ2=fviz_pca_var(pcaResults, col.var = "contrib",
             gradient.cols = c("skyblue", "darkblue"),
             axes=c(1,8),
             select.var = list(contrib=270),
             title="Correlation Circle Between Principal Components 1 and 8",
             legend.title = "Contribution")

grid.arrange(circ1, circ2, ncol=2)

LAshpQW=queen_weights(LAshpMedian2)
LISA1=local_moran(LAshpQW, LAshpMedian2["medianPTB"])

LISA1_colors <- lisa_colors(LISA1)
LISA1_labels <- lisa_labels(LISA1)
LISA1_clusters <- factor(lisa_clusters(LISA1))

options(digits=4)

ggLISA1=ggplot(LAshpMedian2)+
  geom_sf(aes(fill=LISA1_clusters))+
  scale_fill_manual(values=LISA1_colors[1:5], labels=LISA1_labels[1:5])+
  geom_sf_label(aes(label=round(LISA1$lisa_vals, digits=4)), size=2, alpha=0.5)+
  theme_classic()+
  theme(legend.position = "inside", legend.position.inside=c(.83,.72))+
  labs(title ="Map of Louisiana Parishes Spatial Autocorrelation for PTB",
       fill="Percent Median\nLow Birth Weight")

LISA1_p <- lisa_pvalues(LISA1)
p_labels <- c("Not significant", "p <= 0.05", "p <= 0.01", "p <= 0.001")
p_colors <- c("#eeeeee", "#84f576", "#53c53c", "#348124")

ggLISA2=ggplot(LAshpMedian2)+
  geom_sf(aes(fill=sapply(LISA1_p, function(x){
    if (x <= 0.001) return("4")
    else if (x <= 0.01) return("3")
    else if (x <= 0.05) return ("2")
    else return("1")
  })))+
  geom_sf_label(aes(label=LISA1_p), size=2, alpha=0.5)+
  scale_fill_manual(values=p_colors, labels=p_labels)+
  theme_classic()+
  theme(legend.position = "inside", legend.position.inside=c(.83,.72))+
  labs(title ="Map of Louisiana Parishes Autocorrelation P-Values for PTB",
       fill="P-Value Breakpoint")

LISA2=local_moran(LAshpQW, LAshpMedian["medianLBW"])

LISA2_colors <- lisa_colors(LISA2)
LISA2_labels <- lisa_labels(LISA2)
LISA2_clusters <- factor(lisa_clusters(LISA2))

ggLISA3=ggplot(LAshpMedian2)+
  geom_sf(aes(fill=LISA2_clusters))+
  scale_fill_manual(values=LISA2_colors[1:5], labels=LISA2_labels[1:5])+
  geom_sf_label(aes(label=round(LISA2$lisa_vals, digits=4)), size=2, alpha=0.5)+
  theme_classic()+
  theme(legend.position = "inside", legend.position.inside=c(.83,.72))+
  labs(title ="Map of Louisiana Parishes Spatial Autocorrelation for LBW",
       fill="Percent Median\nLow Birth Weight")


LISA2_p <- lisa_pvalues(LISA2)
p_labels <- c("Not significant", "p <= 0.05", "p <= 0.01", "p <= 0.001")
p_colors <- c("#eeeeee", "#84f576", "#53c53c", "#348124")


ggLISA4=ggplot(LAshpMedian2)+
  geom_sf(aes(fill=sapply(LISA2_p, function(x){
    if (x <= 0.001) return("4")
    else if (x <= 0.01) return("3")
    else if (x <= 0.05) return ("2")
    else return("1")
  })))+
  geom_sf_label(aes(label=LISA2_p), size=2, alpha=0.5)+
  scale_fill_manual(values=p_colors, labels=p_labels)+
  theme_classic()+
  theme(legend.position = "inside", legend.position.inside=c(.83,.72))+
  labs(title ="Map of Louisiana Parishes Autocorrelation P-Values for LBW",
       fill="P-Value Breakpoint")

grid.arrange(ggLISA1, ggLISA2, ggLISA3, ggLISA4, ncol=2, nrow=2)


###For Poster
grid.arrange(ggLISA1, ggLISA3, ncol=2)

grid.arrange(pp1, pp2, nrow=2)