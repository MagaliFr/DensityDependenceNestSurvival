### R version 4.1.2 was used for this analysis

### Set directory
setwd("ENTER DIRECTORY")

### Load necessary libraries
library(lme4) #version 1.1-27.1
library(ggplot2) #version 3.3.5
library(png) #version 0.1-7
library(grid) #version 4.1.2
library(ggeffects) #version 1.1.1
library(patchwork) #version 1.1.1

#read csv file
d<-read.csv("Data.csv", header=T,dec=".", sep=",", fill=T)
str(d)
summary(d)

###############################################################################################################
################################ Analysis on the country-wide dataset #########################################
###############################################################################################################
d1<-subset(d, Method=="Nestkaart",
                         select=c(OYC_Density, Dist_km,Year, Habitat, MamDominance,
                                  ObsDays,Fail, MamDominanceWeighted))
d1<-na.omit(d1) #n=300
d1$ObsDays1<-round(d1$ObsDays,0) #round value (no decimal)
d1$DNS<-1-(d1$Fail/d1$ObsDays) #calculate daily nest survival

## two models
M1<-glmer(cbind(ObsDays1,Fail)~Dist_km+OYC_Density*MamDominance+(1|Year)+(1|Habitat),nAGQ=0,family=binomial, data=d1)
summary(M1)

M2<-glmer(cbind(ObsDays1,Fail)~Dist_km+OYC_Density*MamDominanceWeighted+(1|Year)+(1|Habitat),nAGQ=0,family=binomial, data=d1)
summary(M2)

AIC(M1, M2)
#df      AIC
#M1  7 476.2585
#M2  7 481.2152

## summary of the final model
summary(M1)

## Table S7 (country-wide dataset)
#                          Estimate Std. Error z value Pr(>|z|)  
#(Intercept)               0.472885   1.145934   0.413   0.6799  
#Dist_km                  -0.002315   0.003643  -0.635   0.5252  
#OYC_Density               6.106832   2.782188   2.195   0.0282 *
#MamDominance              3.223373   1.319832   2.442   0.0146 *
#OYC_Density:MamDominance -7.173747   3.069627  -2.337   0.0194 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

###############################################################################################################
################################ Analysis on the local population dataset #####################################
###############################################################################################################
d2<-subset(d,Loc=="Ameland Oerd"|Loc=="Ameland Vierkant"|Loc=="Hoge Berg"|Loc=="STEX"|Loc=="Schier",
             select=c(Loc, Year, OYC_Density, Dist_km, Habitat, MamDominance, Fail, ObsDays))
d2$ObsDays1<-round(d2$ObsDays,0)
d2<-na.omit(d2) #n=361
d2$DNS<-1-(d2$Fail/d2$ObsDays) #daily nest survival

# replace location name by area ID
## AC = Island 1 population 1
## AI = Island 1 population 2
## S = Island 2
## TC = Island 3 population 1
## TI = Island 3 population 2
d2$Area_ID<-ifelse(d2$Loc=="Ameland Oerd", "AC",
                     ifelse(d2$Loc=="Ameland Vierkant", "AI",
                            ifelse(d2$Loc=="Hoge Berg","TI", 
                                   ifelse(d2$Loc=="Schier", "S", "TC"))))
d2$Area_ID<-factor(d2$Area_ID)

# model the effect of density on nest survival for the local population dataset
d2$Area_ID<-relevel(d2$Area_ID, ref="TI") ## Island 3 population 2 used as reference
M3<-glmer(cbind(ObsDays1,Fail)~Dist_km+OYC_Density*Area_ID+(1|Year)+(1|Habitat), data=d2, family=binomial)#(1|jaar)+(1|Hab_finalNew)
summary(M3) 
## Table S7
#                        Estimate Std. Error z value Pr(>|z|)    
#(Intercept)             4.9662     1.1044   4.497 6.91e-06 ***
#Dist_km                -1.2122     0.5161  -2.349   0.0188 *  
#OYC_Density             1.1994     0.8251   1.454   0.1460    
#Area_IDAC              -2.0021     1.0591  -1.890   0.0587 .  
#Area_IDAI               1.5716     0.8186   1.920   0.0549 .  
#Area_IDS               -1.5224     1.0722  -1.420   0.1556    
#Area_IDTC              -0.8343     1.6193  -0.515   0.6064    
#OYC_Density:Area_IDAC  -1.2541     0.8562  -1.465   0.1430    
#OYC_Density:Area_IDAI  -2.2077     0.9596  -2.301   0.0214 *  
#OYC_Density:Area_IDS   -1.9540     0.9056  -2.158   0.0310 *  
#OYC_Density:Area_IDTC  -1.5672     1.0967  -1.429   0.1530    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

###############################################################################################################
############################### Plotting Figure 4 ##############################################################
###############################################################################################################
### Preparations for Figure 4
### calculate for each local population the median mammalian dominance and standard deviation
d2HB<-subset(d2, Area_ID=="TI")
summary(d2HB$MamDominance)
sd(d2HB$MamDominance)

d2AO<-subset(d2, Area_ID=="AC")
summary(d2AO$MamDominance)
sd(d2AO$MamDominance)

d2AI<-subset(d2, Area_ID=="AI")
summary(d2AI$MamDominance)

d2S<-subset(d2, Area_ID=="S")
summary(d2S$MamDominance)

d2TC<-subset(d2, Area_ID=="TC")
summary(d2TC$MamDominance)

### bin the continous mammalian dominace for the country-wide 
# dataset and calculate the median and sd of mammalian dominance per category
d1$MamDominanceCat<-ifelse(d1$MamDominance<=0.624  , "1",
                                ifelse(d1$MamDominance>0.886 & d1$MamDominance<=1, "3","2"))

d1$MamDominanceCat<-factor(d1$MamDominanceCat)

d1L<-subset(d1, MamDominanceCat=="1")
summary(d1L$MamDominance)
sd(d1L$MamDominance)

d1M<-subset(d1, MamDominanceCat=="2") 
summary(d1M$MamDominance)
sd(d1M$MamDominance)

d1H<-subset(d1, MamDominanceCat=="3")
summary(d1H$MamDominance)
sd(d1H$MamDominance)

#### save those values (median, sd) in csv called DataFig4 under Median_MamDominance
# and SD_MamDominance

######################
## estimate the effect of each local population on nest survival
## note that you need to rerun this model once for each reference group (relevel for each local population) 
## to get correct estimate and standard error
d2$Area_ID<-relevel(d2$Area_ID, ref="AC")
M4<-glmer(cbind(ObsDays1,Fail)~Dist_km+OYC_Density*Area_ID+(1|Year)+(1|Habitat), data=d2, family=binomial)#(1|jaar)+(1|Hab_finalNew)
summary(M4) 
### estimate and standard error are filled in csv called  DataFig4 under 
## column 'Estimate' and 'Std_error'

### Repeat for the country-wide dataset
d1$MamDominanceCat<-relevel(d1$MamDominanceCat, ref="1") #change reference to get standard error for each class

M5<-glmer(cbind(ObsDays1,Fail)~Dist_km+OYC_Density*MamDominanceCat+(1|Year)+(1|Habitat),nAGQ=0,data=d1, family=binomial)
summary(M5)
### estimate and standard error are filled in csv called  DataFig4 under 
## column 'Estimate' and 'Std_error'

######### now we are ready to plot Fig. 4 ###################################
### we start with Fig. 4a
DataFig4<-read.csv("DataFig4.csv", header=T,dec=",", sep=";", fill=T)
DataFig4$n<-as.numeric(as.character(DataFig4$n))
DataFig4$Population<-factor(DataFig4$Population)
DataFig4$Dataset<-factor(DataFig4$Dataset)

## import mammalian and avian image
imgM<-readPNG("Mammal.png")
imgB<-readPNG("Bird.png")

p4a<-ggplot(DataFig4, aes(x=Median_MamDominance, y=Estimate, group=Dataset))+
  scale_x_continuous(limits = c(-0.02, 1))+
  scale_y_continuous(limits = c(-2.1, 3.8))+
  xlab("Mammalian dominance index") +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"),
        axis.text=element_text(size=11),
        legend.text = element_text(margin = margin(t = 0),size=10),
        legend.title = element_text(size=12),
        legend.key.size=unit(1.5, "line"),
        plot.tag = element_text(size = 14))+
  ylab("Effect of breeding pair density \non daily nest survival")+
  labs(tag="a)")+
  theme(legend.position="bottom",legend.box="vertical", legend.margin=margin(),
        legend.key.height = unit(.5, 'cm'))+
  geom_errorbar(aes(ymin=Estimate-Std_error, ymax=Estimate+Std_error, 
                    color=c("#339900","#66FF33","#990099","darkgray","#99CCFF",  "#0066CC", "#FFCC00", "#FF3300", "#990000")), width=.01, size=.8)+
  geom_errorbarh(aes(xmin=Median_MamDominance-SD_MamDominance, xmax=Median_MamDominance+SD_MamDominance, 
                     color=c("#339900","#66FF33", "#990099","darkgray","#99CCFF",  "#0066CC", "#FFCC00", "#FF3300", "#990000")),height=.1,size=.8)+
  geom_point(data=DataFig4, mapping=aes(x=Median_MamDominance, y=Estimate, shape=Dataset,
                                         color=c("#339900","#66FF33","#990099","darkgray","#99CCFF",  "#0066CC", "#FFCC00", "#FF3300","#990000")), size=3)+
  scale_colour_identity(guide = "legend", name="Population",
                        labels =  c("Island 1 Population 1 (n=144)", "Island 1 Population 2 (n=70)",
                                    "Island 2 Population 1 (n=95)", "Island 2 Population 2 (literature)", 
                                    "Island 3 Population 1 (n=20)", "Island 3 Population 2 (n=32)", 
                                    "Country-wide: Low mammalian \ndominance (n=96)", "Country-wide: Mid mammalian \ndominance (n=170)", "Country-wide: High mammalian \ndominance (n=34)"),
                        breaks=c("#339900","#66FF33", "#990099","darkgray","#99CCFF",  "#0066CC", "#FFCC00", "#FF3300", "#990000"))+
  ## y=intercept+slope --> intercept=OYC_Density in model summary, slope=interaction mammalian dominance and intercept in model summary
  geom_segment(aes(x = 0.91, xend = 0.53, y =6.107-7.174*0.91, yend=6.107-7.174*0.53),linetype="solid", size=1.1, color="red")+
  geom_hline(yintercept=0.0, linetype="dashed",color = "gray40", size=.6)+
  annotation_custom(rasterGrob(imgM, 
                               width = unit(0.35,"npc"),
                               height = unit(0.35,"npc")), 
                    -0.05,0.22, -2.75, -1.1)+  
  annotation_custom(rasterGrob(imgM, 
                               width = unit(0.4,"npc"),
                               height = unit(0.4,"npc")), 
                    0.41,0.7, -2.75, -1.1)+
  annotation_custom(rasterGrob(imgM, 
                               width = unit(0.5,"npc"),
                               height = unit(0.5,"npc")), 
                    0.85,1.11, -2.75, -1.1)+
  annotation_custom(rasterGrob(imgB, 
                               width = unit(0.7,"npc"),
                               height = unit(0.7,"npc")), 
                    -0.105,0.089, -2.75, -1.0)+  
  annotation_custom(rasterGrob(imgB, 
                               width = unit(0.5,"npc"),
                               height = unit(0.5,"npc")), 
                    0.35,0.571, -2.75, -1.0)+  
  annotation_custom(rasterGrob(imgB, 
                               width = unit(0.35,"npc"),
                               height = unit(0.35,"npc")), 
                    0.77,1.03, -2.75, -1.0)+
  guides(colour=guide_legend(nrow=5,ncol=2,byrow=TRUE))
print(p4a)

#######################
### now we plot Fig. 4b
#######################
d1$MamDominanceCatOrdered<-factor(d1$MamDominanceCat,
                                  levels=c("1", "2", "3"),
                                  labels=c("Country-wide \nLow mammalian dominance \n(n=96)", "Country-wide \nMid mammalian dominance \n(n=170)", "Country-wide \nHigh mammalian dominance \n(n=34)"))# to make correct order of plot
M6<-glmer(cbind(ObsDays1,Fail)~Dist_km+OYC_Density*MamDominanceCatOrdered+(1|Year)+(1|Habitat),nAGQ=0,data=d1, family=binomial)

pr4b<- ggpredict(M6, c("OYC_Density", "MamDominanceCatOrdered"))
pr4b$predicted<-pr4b$predicted
pr4b$predictedNS<-pr4b$predicted^27

## extract prediction from low mammalian dominance only
pr4b_sub<-subset(pr4b, group=="Country-wide \nLow mammalian dominance \n(n=96)")
str(pr4b_sub)
#min and max density for low mammalian dominance
min(d1L$OYC_Density) #0.32
max(d1L$OYC_Density) #1.28
pr4b_sub1<-data.frame(pr4b_sub)
pr4b_sub1<-subset(pr4b_sub1, x>=0.32&x<=1.28)

pr4b_subH<-subset(pr4b, group=="Country-wide \nHigh mammalian dominance \n(n=34)")
str(pr4b_subH)
#min and max density for high mammalian dominance
min(d1H$OYC_Density) #0.32
max(d1H$OYC_Density) #2.56
pr4b_sub1H<-data.frame(pr4b_subH)
pr4b_sub1H<-subset(pr4b_sub1H, x>=0.32&x<=2.56)

p4b<-ggplot()+
  labs(x="Density (breeding pair/ha)", y="Daily nest survival", tag="b)")+
  geom_line(data=pr4b_sub, mapping=aes(y=predicted, x=x),col="#FFCC00", size=1)+
  geom_line(data=pr4b_subH, mapping=aes(y=predicted, x=x),col="#990000", size=1)+
  theme(axis.title.x = element_text(color="black", size=10),
        axis.title.y = element_text(color="black", size=10,lineheight = .5),
        axis.text=element_text(size=9),
        plot.tag = element_text(size = 12))+
  scale_y_continuous(
    limits = c(0.84,1),
    breaks=c(0.9,1),
    name="Daily nest survival", 
    sec.axis = sec_axis(trans=~.^27, name = "Nest survival", breaks=c(0.01,0.25,0.5,1))
  )+
  theme(legend.position="none")
print(p4b)

######################## combine both plots to figure 4 and save as png or pdf #################################################
### png
png("Fig4.png", width = 177.8, height = 203.8,units = 'mm', res = 600)
p4a + inset_element(p4b, left = 0.59, bottom = 0.675, right = 0.995, top = 0.995)
grid.lines(x=unit(c(0.582,0.63),"npc"), y=unit(c(0.74,0.955), "npc"), gp = gpar(col = "#FFCC00", lty=2, alpha=0.5, lwd=2), draw=T,default.units='naif') #paars
grid.lines(x=unit(c(0.582,0.63),"npc"), y=unit(c(0.74,0.751), "npc"), gp = gpar(col = "#FFCC00", lty=2, alpha=0.5, lwd=2), draw=T,default.units='naif') #paars
grid.lines(x=unit(c(0.865,0.63),"npc"), y=unit(c(0.49,0.751), "npc"), gp = gpar(col = "#990000", lty=2, alpha=0.5, lwd=2), draw=T,default.units='naif') #paars
grid.lines(x=unit(c(0.875,0.973),"npc"), y=unit(c(0.49,0.751), "npc"), gp = gpar(col = "#990000", lty=2, alpha=0.5, lwd=2), draw=T,default.units='naif') #paars
dev.off()

### pdf
pdf("Fig4.pdf", width = 7, height = 8)
p4a + inset_element(p4b, left = 0.59, bottom = 0.675, right = 0.995, top = 0.995)
grid.lines(x=unit(c(0.582,0.63),"npc"), y=unit(c(0.74,0.955), "npc"), gp = gpar(col = "#FFCC00", lty=2, alpha=0.5, lwd=2), draw=T,default.units='naif') #paars
grid.lines(x=unit(c(0.582,0.63),"npc"), y=unit(c(0.74,0.751), "npc"), gp = gpar(col = "#FFCC00", lty=2, alpha=0.5, lwd=2), draw=T,default.units='naif') #paars
grid.lines(x=unit(c(0.865,0.63),"npc"), y=unit(c(0.49,0.751), "npc"), gp = gpar(col = "#990000", lty=2, alpha=0.5, lwd=2), draw=T,default.units='naif') #paars
grid.lines(x=unit(c(0.875,0.973),"npc"), y=unit(c(0.49,0.751), "npc"), gp = gpar(col = "#990000", lty=2, alpha=0.5, lwd=2), draw=T,default.units='naif') #paars
dev.off()

####################################################################################################################
################################## ANALYSIS ON EFFECT OF HETEROSPECIFIC DENSITY ####################################
####################################################################################################################
#### Table S8 ####
d3<-subset(d, Method=="Nestkaart",
                select=c(NestSuccess,OYC_Density, Dist_km, Year, Habitat, MamDominance,
                         AllMeadowBirdSpc_Density, Godwit_Density, Lapwing_Density, Redshank_Density, Fail, ObsDays))
d3$ObsDays1<-round(d3$ObsDays)

## only oystercatcher density
M7<-glmer(cbind(ObsDays1,Fail)~Dist_km+OYC_Density*MamDominance+(1|Year)+(1|Habitat),nAGQ=0,family=binomial, data=d3)
summary(M7)

## oystercatcher & lapwing density
d3$Dens_OL<-d3$OYC_Density+d3$Lapwing_Density
M8<-glmer(cbind(ObsDays1,Fail)~Dist_km+Dens_OL*MamDominance+(1|Year)+(1|Habitat),nAGQ=0,family=binomial, data=d3)
summary(M8)

## oystercatcher & godwit density
d3$Dens_OG<-d3$OYC_Density+d3$Godwit_Density
M9<-glmer(cbind(ObsDays1,Fail)~Dist_km+Dens_OG*MamDominance+(1|Year)+(1|Habitat),nAGQ=0,family=binomial, data=d3)
summary(M9)

# osytercatcher & redshank density
d3$Dens_OR<-d3$OYC_Density+d3$Redshank_Density
M10<-glmer(cbind(ObsDays1,Fail)~Dist_km+Dens_OR*MamDominance+(1|Year)+(1|Habitat),nAGQ=0,family=binomial, data=d3)
summary(M10)

# oystercatcher & lapwing & godwit 
d3$Dens_OLG<-d3$OYC_Density + d3$Lapwing_Density + d3$Godwit_Density
M11<-glmer(cbind(ObsDays1,Fail)~Dist_km+Dens_OLG*MamDominance+(1|Year)+(1|Habitat),nAGQ=0,family=binomial, data=d3)
summary(M11)

# all meadow bird species together
M12<-glmer(cbind(ObsDays1,Fail)~Dist_km+AllMeadowBirdSpc_Density*MamDominance+(1|Year)+(1|Habitat),nAGQ=0,family=binomial, data=d3)
summary(M12)

# AIC comparison of the models (Table S8)
AIC(M7, M8, M9, M10, M11, M12)