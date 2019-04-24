###################################Packages to be loaded###############################################

#install.packages("devtools")
library(devtools)
devtools::install_version("haven", version = "1.1.0")


library(haven)
library(Hmisc)
library(dplyr)
library(taRifx)
library(ggplot2)
library(scales)
library(qdap)
library(magrittr)
library(RColorBrewer)
library(memisc)
library(sjPlot)
library(plm)
library(lmtest)
library(captioner)


##########################Loading waves 2-8 and merging to final dataset###############################

#Child 
child8 <- read.csv("./Data/pairfam data/2008-2016/child8withscales.csv")
child7 <- read.csv("./Data/pairfam data/2008-2016/child7withscales.csv")
child6 <- read.csv("./Data/pairfam data/2008-2016/child6withscales.csv")
child5 <- read.csv("./Data/pairfam data/2008-2016/child5withscales.csv")
child4 <- read.csv("./Data/pairfam data/2008-2016/child4withscales.csv")
child3 <- read.csv("./Data/pairfam data/2008-2016/child3withscales.csv")
child2 <- read.csv("./Data/pairfam data/2008-2016/child2withscales.csv")


#Anchor
anchor8 <- haven::read_dta("./Data/pairfam data/2008-2016/anchor8.dta")
anchor7 <- haven::read_dta("./Data/pairfam data/2008-2016/anchor7.dta")
anchor6 <- haven::read_dta("./Data/pairfam data/2008-2016/anchor6.dta")
anchor5 <- haven::read_dta("./Data/pairfam data/2008-2016/anchor5.dta")
anchor4 <- haven::read_dta("./Data/pairfam data/2008-2016/anchor4.dta")
anchor3 <- haven::read_dta("./Data/pairfam data/2008-2016/anchor3.dta")
anchor2 <- haven::read_dta("./Data/pairfam data/2008-2016/anchor2.dta")

colnames(anchor8) <- paste("anchor", colnames(anchor8), sep = "_")
names(anchor8)[names(anchor8)=="anchor_cid"] <- "cid"
names(anchor8)[names(anchor8)=="anchor_id"] <- "id"

colnames(anchor7) <- paste("anchor", colnames(anchor7), sep = "_")
names(anchor7)[names(anchor7)=="anchor_cid"] <- "cid"
names(anchor7)[names(anchor7)=="anchor_id"] <- "id"

colnames(anchor6) <- paste("anchor", colnames(anchor6), sep = "_")
names(anchor6)[names(anchor6)=="anchor_cid"] <- "cid"
names(anchor6)[names(anchor6)=="anchor_id"] <- "id"

colnames(anchor5) <- paste("anchor", colnames(anchor5), sep = "_")
names(anchor5)[names(anchor5)=="anchor_cid"] <- "cid"
names(anchor5)[names(anchor5)=="anchor_id"] <- "id"

colnames(anchor4) <- paste("anchor", colnames(anchor4), sep = "_")
names(anchor4)[names(anchor4)=="anchor_cid"] <- "cid"
names(anchor4)[names(anchor4)=="anchor_id"] <- "id"

colnames(anchor3) <- paste("anchor", colnames(anchor3), sep = "_")
names(anchor3)[names(anchor3)=="anchor_cid"] <- "cid"
names(anchor3)[names(anchor3)=="anchor_id"] <- "id"

colnames(anchor2) <- paste("anchor", colnames(anchor2), sep = "_")
names(anchor2)[names(anchor2)=="anchor_cid"] <- "cid"
names(anchor2)[names(anchor2)=="anchor_id"] <- "id"


child8 <- dplyr::select(child8, SDQtotal, wave, id, cid,csex, cdoby,cedu1a,cedu1b,cedu6i2,cinc25i3, chlt0a,cedu7i2,cedu7i4,cedu15, cedu16, cpcr7i1,cpcr7i4, cpcr7i6, cpcr7i8, cpcr8i4,cpcr8i6,cpcr8i8,ctimea,ctimep,csdq1i5, csdq1i6, csdq1i24, csdq1i13,csdq1i8,cpici1, cemotion1, cconduct1, chyper1, cpeerprob1, cemotion,cconduct, chyper, cpeerprob)

child7 <- dplyr::select(child7,SDQtotal, wave,id,cid,csex, cdoby,cedu1a,cedu1b,cedu6i2,cinc25i3, chlt0a,cedu7i2,cedu7i4, cpcr7i1,cpcr7i4, cpcr7i6, cpcr7i8, cpcr8i4,cpcr8i6,cpcr8i8,ctimea,ctimep,csdq1i5, csdq1i24, csdq1i13,csdq1i8,cpici1, cemotion1, cconduct1, chyper1, cemotion,cconduct, chyper)

child6 <- dplyr::select(child6,SDQtotal,wave, id,cid,csex, cdoby,cedu1a,cedu6i2,cinc25i3, chlt0a,cedu7i2,cedu7i4, cpcr7i1,cpcr7i4, cpcr7i6, cpcr7i8, cpcr8i4,cpcr8i6,cpcr8i8,ctimea,ctimep,csdq1i5,  csdq1i24, csdq1i13,csdq1i8,cpici1, cemotion1, cconduct1, chyper1, cemotion,cconduct, chyper)

child5 <- dplyr::select(child5,SDQtotal,wave,id, cid,csex, cdoby,cedu1a,cedu6i2,cinc25i3, chlt0a,cedu7i2,cedu7i4,  cpcr7i1,cpcr7i4, cpcr7i6, cpcr7i8, cpcr8i4,cpcr8i6,cpcr8i8,csdq1i5,  csdq1i24, csdq1i13,csdq1i8, cemotion1, cconduct1, chyper1,  cemotion,cconduct, chyper)

child4 <- dplyr::select(child4,SDQtotal,wave,id, cid,csex, cdoby,cedu1a,cedu6i2,cinc25i3, chlt0a,cedu7i2,cedu7i4,  cpcr7i1,cpcr7i4, cpcr7i6, cpcr7i8, cpcr8i4,cpcr8i6,cpcr8i8,csdq1i5,  csdq1i24, csdq1i13,csdq1i8, cemotion1, cconduct1,   cemotion,cconduct)

child3 <- dplyr::select(child3,SDQtotal,wave,id, cid,csex, cdoby,cedu1a,cedu6i2,cinc25i3, chlt0a,cedu7i2,cedu7i4,  cpcr7i1,cpcr7i4, cpcr7i6, cpcr7i8, cpcr8i4,cpcr8i6,cpcr8i8,csdq1i5,  csdq1i24, csdq1i13,csdq1i8, cemotion1, cconduct1, cemotion,cconduct)

child2 <- dplyr::select(child2,SDQtotal,wave,id, cid,csex, cdoby,cedu6i2,cinc25i3, cedu7i2,cedu7i4,  cpcr7i1,cpcr7i4, cpcr7i6, cpcr7i8, cpcr8i4,cpcr8i6,cpcr8i8,csdq1i5,  csdq1i24, csdq1i13,csdq1i8, cemotion1, cconduct1, cemotion,cconduct)

finaldataset8 <- merge(child8, anchor8[, c("anchor_inc28", "id","anchor_cid1", "anchor_cid2", "anchor_cid3", "anchor_cid4", "anchor_cid5", "anchor_cid6", "anchor_cid7", "anchor_cid8", "anchor_cid9", "anchor_cid10", "anchor_sex_gen", "anchor_doby_gen", "anchor_age", "anchor_cob", "anchor_ethni", "anchor_migstatus", "anchor_relstat", "anchor_marstat", "anchor_nkidsliv", "anchor_school", "anchor_yeduc", "anchor_hhincnet", "anchor_lfs", "anchor_hpg", "anchor_np", "anchor_nmar", "anchor_reldur", "anchor_cohabdur","anchor_mardur", "anchor_nkids", "anchor_ehc27p1i2")], by = "id")
finaldataset8$wave <- "8 2015/16"

write.csv(finaldataset8,"./Data/finaldataset8.csv")

finaldataset7 <- merge(child7, anchor7[, c("anchor_inc28", "id","anchor_cid1", "anchor_cid2", "anchor_cid3", "anchor_cid4", "anchor_cid5", "anchor_cid6", "anchor_cid7", "anchor_cid8", "anchor_cid9", "anchor_cid10", "anchor_sex_gen", "anchor_doby_gen", "anchor_age", "anchor_cob", "anchor_ethni", "anchor_migstatus", "anchor_relstat", "anchor_marstat", "anchor_nkidsliv", "anchor_school", "anchor_yeduc", "anchor_hhincnet", "anchor_lfs", "anchor_hpg", "anchor_np", "anchor_nmar", "anchor_reldur", "anchor_cohabdur","anchor_mardur", "anchor_nkids", "anchor_ehc27p1i2")], by = "id")

write.csv(finaldataset7,"./Data/finaldataset7.csv")

finaldataset6 <- merge(child6, anchor6[, c("anchor_inc28", "id","anchor_cid1", "anchor_cid2", "anchor_cid3", "anchor_cid4", "anchor_cid5","anchor_cid6", "anchor_cid7", "anchor_cid8", "anchor_cid9", "anchor_cid10", "anchor_sex_gen", "anchor_doby_gen", "anchor_age", "anchor_cob", "anchor_ethni", "anchor_migstatus", "anchor_relstat", "anchor_marstat", "anchor_nkidsliv", "anchor_school", "anchor_yeduc", "anchor_hhincnet", "anchor_lfs", "anchor_hpg", "anchor_np", "anchor_nmar", "anchor_reldur", "anchor_cohabdur","anchor_mardur", "anchor_nkids", "anchor_ehc27p1i2")], by = "id")

write.csv(finaldataset6,"./Data/finaldataset6.csv")

finaldataset5 <- merge(child5, anchor5[, c("anchor_inc28", "id","anchor_cid1", "anchor_cid2", "anchor_cid3", "anchor_cid4", "anchor_cid5","anchor_cid6", "anchor_cid7", "anchor_cid8", "anchor_cid9", "anchor_cid10", "anchor_sex_gen", "anchor_doby_gen", "anchor_age", "anchor_cob", "anchor_ethni", "anchor_migstatus", "anchor_relstat", "anchor_marstat", "anchor_nkidsliv", "anchor_school", "anchor_yeduc", "anchor_hhincnet", "anchor_lfs", "anchor_hpg", "anchor_np", "anchor_nmar", "anchor_reldur", "anchor_cohabdur","anchor_mardur", "anchor_nkids", "anchor_ehc27p1i2")], by = "id")

write.csv(finaldataset5,"./Data/finaldataset5.csv")

finaldataset4 <- merge(child4, anchor4[, c("anchor_inc28", "id","anchor_cid1", "anchor_cid2", "anchor_cid3", "anchor_cid4", "anchor_cid5","anchor_cid6", "anchor_cid7", "anchor_cid8", "anchor_cid9", "anchor_cid10", "anchor_sex_gen", "anchor_doby_gen", "anchor_age", "anchor_cob", "anchor_ethni", "anchor_migstatus", "anchor_relstat", "anchor_marstat", "anchor_nkidsliv", "anchor_school", "anchor_yeduc", "anchor_hhincnet", "anchor_lfs", "anchor_hpg", "anchor_np", "anchor_nmar", "anchor_reldur", "anchor_cohabdur","anchor_mardur", "anchor_nkids", "anchor_ehc27p1i2")], by = "id")

write.csv(finaldataset4,"./Data/finaldataset4.csv")

finaldataset3 <- merge(child3, anchor3[, c("anchor_inc28", "id","anchor_cid1", "anchor_cid2", "anchor_cid3", "anchor_cid4", "anchor_cid5","anchor_cid6", "anchor_cid7", "anchor_cid8", "anchor_cid9", "anchor_cid10", "anchor_sex_gen", "anchor_doby_gen", "anchor_age", "anchor_cob", "anchor_ethni", "anchor_migstatus", "anchor_relstat", "anchor_marstat", "anchor_nkidsliv", "anchor_school", "anchor_yeduc", "anchor_hhincnet", "anchor_lfs", "anchor_hpg", "anchor_np", "anchor_nmar", "anchor_reldur", "anchor_cohabdur","anchor_mardur", "anchor_nkids", "anchor_ehc27p1i2")], by = "id")

write.csv(finaldataset3,"./Data/finaldataset3.csv")

finaldataset2 <- merge(child2, anchor2[, c("anchor_inc28", "id","anchor_cid1", "anchor_cid2", "anchor_cid3", "anchor_cid4", "anchor_cid5","anchor_cid6", "anchor_cid7", "anchor_cid8", "anchor_cid9", "anchor_cid10", "anchor_sex_gen", "anchor_doby_gen", "anchor_age", "anchor_cob", "anchor_ethni", "anchor_migstatus", "anchor_relstat", "anchor_marstat", "anchor_nkidsliv", "anchor_school", "anchor_yeduc", "anchor_hhincnet", "anchor_lfs", "anchor_np", "anchor_nmar", "anchor_reldur", "anchor_cohabdur","anchor_mardur", "anchor_nkids")], by = "id")

write.csv(finaldataset2,"./Data/finaldataset2.csv")

finaldataset <- dplyr::bind_rows(finaldataset2, finaldataset3, finaldataset4, finaldataset5, finaldataset6, finaldataset7, finaldataset8)

write.csv(finaldataset,"./Data/finaldataset.csv")

#######################################################################################################

#Checking if the children ID from the child survey is matched by reported child ID from the anchor survey
finaldataset$cidcheck <- finaldataset$cid %in% c(finaldataset$anchor_cid1, finaldataset$anchor_cid2, finaldataset$anchor_cid3, finaldataset$anchor_cid4, finaldataset$anchor_cid5, finaldataset$anchor_cid6, finaldataset$anchor_cid7, finaldataset$anchor_cid8,finaldataset$anchor_cid9, finaldataset$anchor_cid10)
table(finaldataset$cidcheck)


#Generating the dataset for final analysis
analysis <- dplyr::select(finaldataset, wave, id,cid,csex, cdoby,cedu1a,cedu1b,cedu6i2,cinc25i3, chlt0a,cedu7i2,cedu7i4,cedu15, cedu16, cpcr7i1,cpcr7i4, cpcr7i6, cpcr7i8, cpcr8i4,cpcr8i6,cpcr8i8,ctimea,ctimep,csdq1i5, csdq1i6, csdq1i24, csdq1i13,csdq1i8,cpici1, cemotion1, cconduct1, chyper1, cpeerprob1, cemotion,cconduct, chyper, cpeerprob, starts_with("anchor"))

analysis <- dplyr::select(analysis,-anchor_cid1, -anchor_cid2, -anchor_cid3, -anchor_cid4, -anchor_cid5, -anchor_cid6, -anchor_cid7, -anchor_cid8, -anchor_cid9, -anchor_cid10)

###########################################Data cleaning###############################################
analysis$cemotion <- destring(analysis$cemotion)
analysis$cconduct <- destring(analysis$cconduct)
analysis$chyper <- destring(analysis$chyper)
analysis$cpeerprob <- destring(analysis$cpeerprob)

#analysis <- subset(analysis, !(analysis$cedu1a==-1|analysis$cedu1a==-3)) 
#analysis <- subset(analysis, !(analysis$cedu6i2==-1)) 

#analysis <- subset(analysis, !(analysis$SDQtotal<0)) 
#analysis <- subset(analysis, !(analysis$cemotion1<0)) 
#analysis <- subset(analysis, !(analysis$cconduct1<0)) 
#analysis <- subset(analysis, !(analysis$chyper1<0)) 
#analysis <- subset(analysis, !(analysis$cpeerprob1<0)) 


#analysis <- subset(analysis, !(analysis$anchor_hhincnet<0)) 
analysis <- subset(analysis, !(analysis$anchor_relstat==-7))
analysis <- subset(analysis, !(analysis$anchor_marstat==-7))
analysis <- subset(analysis, !(analysis$anchor_marstat==-3))


analysis <- subset(analysis, !(analysis$anchor_ethni==-7)) 
analysis <- subset(analysis, !(analysis$anchor_inc28==-2)) 
analysis <- subset(analysis, !(analysis$anchor_inc28==-1)) 
analysis <- subset(analysis, !(analysis$anchor_yeduc<0)) 

analysis <- subset(analysis, !(analysis$cemotion1<0)) 
analysis <- subset(analysis, !(analysis$cconduct1<0)) 


###Recoding waves
analysis$wave[analysis$wave=="2 2009/10"] <- 2
analysis$wave[analysis$wave=="3 2010/11"] <- 3
analysis$wave[analysis$wave=="4 2011/12"] <- 4
analysis$wave[analysis$wave=="5 2012/13"] <- 5
analysis$wave[analysis$wave=="6 2013/14"] <- 6
analysis$wave[analysis$wave=="7 2014/15"] <- 7
analysis$wave[analysis$wave=="8 2015/16"] <- 8

analysis$wave <- destring(analysis$wave)

##Recoding relationship status
analysis$anchor_relstat[analysis$anchor_relstat==1] <- "Never married Single"
analysis$anchor_relstat[analysis$anchor_relstat==2] <- "Never married LAT"
analysis$anchor_relstat[analysis$anchor_relstat==3] <- "Never married Cohab"
analysis$anchor_relstat[analysis$anchor_relstat==4] <- "Married"
analysis$anchor_relstat[analysis$anchor_relstat==5] <- "Married"
analysis$anchor_relstat[analysis$anchor_relstat==6] <- "Divorced"
analysis$anchor_relstat[analysis$anchor_relstat==7] <- "Divorced"
analysis$anchor_relstat[analysis$anchor_relstat==8] <- "Divorced"
analysis$anchor_relstat[analysis$anchor_relstat==9] <- "Widowed"
analysis$anchor_relstat[analysis$anchor_relstat==10] <- "Widowed"
analysis$anchor_relstat[analysis$anchor_relstat==11] <- "Widowed"

analysis$anchor_marstat[analysis$anchor_marstat==1] <- "Never married"
analysis$anchor_marstat[analysis$anchor_marstat==2] <- "Married/civil union"
analysis$anchor_marstat[analysis$anchor_marstat==3] <- "Divorced"
analysis$anchor_marstat[analysis$anchor_marstat==4] <- "Widowed"

analysis$anchor_mardummy[analysis$anchor_marstat=="Married/civil union"] <- "Married"
analysis$anchor_mardummy[!analysis$anchor_marstat=="Married/civil union"] <- "Not Married"

##Recoding anchor main residence as East and West Germany

analysis$anchor_ehc27p1i2[analysis$anchor_ehc27p1i2==10] <- "East"
analysis$anchor_ehc27p1i2[analysis$anchor_ehc27p1i2==11] <- "East"
analysis$anchor_ehc27p1i2[analysis$anchor_ehc27p1i2==12] <- "East"
analysis$anchor_ehc27p1i2[analysis$anchor_ehc27p1i2==13] <- "East"
analysis$anchor_ehc27p1i2[analysis$anchor_ehc27p1i2==15] <- "East"

analysis$anchor_ehc27p1i2[!analysis$anchor_ehc27p1i2=="East"] <- "West"


##Recoding anchor ethnicity
analysis$anchor_ethni[analysis$anchor_ethni==1] <- "German native"
analysis$anchor_ethni[analysis$anchor_ethni==2] <- "Ethnic German immigrant (Aussiedler)"
analysis$anchor_ethni[analysis$anchor_ethni==3] <- "Half German"
analysis$anchor_ethni[analysis$anchor_ethni==4] <- "Turkish background"
analysis$anchor_ethni[analysis$anchor_ethni==5] <- "Other non-German background"


##Recoding adolescent gender
analysis$csex.f[analysis$csex=="1 Junge"] <- "Boy"
analysis$csex.f[!analysis$csex=="1 Junge"] <- "Girl"


##Creating factor variables
analysis$cdoby.f <- factor(analysis$cdoby)
analysis$anchor_relstat.f <- factor(analysis$anchor_relstat)
analysis$anchor_ethni.f <- factor(analysis$anchor_ethni)
analysis$anchor_inc28.f <- factor(analysis$anchor_inc28)
analysis$anchor_eastwest.f <- factor(analysis$anchor_ehc27p1i2)
analysis$anchor_marstat.f <- factor(analysis$anchor_marstat)
analysis$anchor_mardummy.f <- factor(analysis$anchor_mardummy)
analysis$anchor_mardummy.f <- factor(analysis$anchor_mardummy)
analysis$csex.f <- factor(analysis$csex.f)


write.csv(analysis,"./Data/analysis.csv")

#analysis <- read.csv("C:/Users/WINDOWS-PC/Dropbox/Hertie School of Governance/Semester 4/Thesis/analysis.csv")

length(analysis$cid)
length(unique(analysis$cid))
n_occur <- data.frame(table(analysis$cid, analysis$wave))

###########################################Data analysis: regression###################################

###Pooled OLS
pooled1 <- plm(cemotion1 ~ csex + cdoby.f + anchor_inc28.f + anchor_marstat.f + anchor_ethni.f + anchor_yeduc + anchor_eastwest.f, data=analysis, index=c("cid","wave"),  model="pooling")
summary(pooled1, robust=T)

##Clustered standard errors
coeftest(pooled1, vcov=vcovHC(pooled1,type="HC0",cluster="group"))


pooled2 <- plm(cconduct1 ~ csex + cdoby.f + anchor_inc28.f + anchor_marstat.f + anchor_ethni.f + anchor_yeduc + anchor_eastwest.f, data=analysis, index=c("cid","wave"),  model="pooling")
summary(pooled1, robust=T)

##Clustered standard errors
coeftest(pooled2, vcov=vcovHC(pooled1,type="HC0",cluster="group"))

###Fixed effects models - overall
fereg1 <- plm(cemotion1 ~ csex + cdoby.f + anchor_inc28.f + anchor_mardummy.f + anchor_ethni.f + anchor_yeduc + anchor_eastwest.f, data=analysis, index=c("cid","wave"), model="within")

summary(fereg1)

fereg2 <- plm(cconduct1 ~ csex + cdoby.f + anchor_inc28.f + anchor_mardummy.f + anchor_ethni.f + anchor_yeduc + anchor_eastwest.f, data=analysis, index=c("cid","wave"), model="within")

summary(fereg2)

###Fixed effects models - separate models by gender
analysismale <- subset(analysis, analysis$csex=="1 Junge")

feregmale1 <- plm(cemotion1 ~  cdoby.f + anchor_inc28.f + anchor_mardummy.f + anchor_ethni.f + anchor_yeduc + anchor_eastwest.f, data=analysismale, index=c("cid","wave"), model="within")
summary(feregmale1)

feregmale2 <- plm(cconduct1 ~ cdoby.f + anchor_inc28.f + anchor_mardummy.f + anchor_ethni.f + anchor_yeduc + anchor_eastwest.f, data=analysismale, index=c("cid","wave"), model="within")
summary(feregmale2)

analysisfemale <- subset(analysis, !(analysis$csex=="1 Junge"))

feregfemale1 <- plm(cemotion1 ~  cdoby.f + anchor_inc28.f + anchor_mardummy.f + anchor_ethni.f + anchor_yeduc + anchor_eastwest.f, data=analysisfemale, index=c("cid","wave"), model="within")
summary(feregmale1)

feregfemale2 <- plm(cemotion1 ~  cdoby.f + anchor_inc28.f + anchor_mardummy.f + anchor_ethni.f + anchor_yeduc + anchor_eastwest.f, data=analysisfemale, index=c("cid","wave"), model="within")
summary(feregmale2)

#Individual regressions
reg2 <- lm(cemotion1 ~ csex + cdoby + anchor_hhincnet + anchor_relstat + anchor_mardur + anchor_ethni, data=analysis)
summary(reg2)

reg3 <- lm(cconduct1 ~ csex + cdoby + anchor_hhincnet + anchor_relstat + anchor_mardur + anchor_ethni, data=analysis)
summary(reg3)

reg4 <- lm(chyper1 ~ csex + cdoby + anchor_hhincnet + anchor_relstat + anchor_mardur + anchor_ethni, data=analysis)
summary(reg4)

reg5 <- lm(cpeerprob1 ~ csex + cdoby + anchor_hhincnet + anchor_relstat + anchor_mardur + anchor_ethni, data=analysis)
summary(reg5)

stargazer::stargazer(pooled1, fereg1, fereg2, feregmale1, feregmale2, feregfemale1, feregfemale2, title ="Estimated regression models", type="html",out="./Analysis/allreg.doc")

#######################################Render RMD######################################################
rmarkdown::render(input = "./Code/Thesis_v1.Rmd",
                  output_format = "pdf_document",
                  output_file = paste0("Vaishali V_Thesis",".pdf", sep=''))




############################Data visualization#########################################################

#Plot 1: Distribution of SDQ
ggplot(analysis, aes(x=SDQtotal)) + geom_histogram(fill="maroon", bins=20) + scale_x_continuous(expand = c(0,0), limits=c(0,40)) + scale_y_continuous(expand=c(0,0)) + ggtitle("Distribution of SDQ among surveyed adolescents") + labs(caption="Source: Wave 8 of pairfam (2015-16)")

ggsave("plot1.png")

#Plot 2: Gender of adolescents surveyed
ggplot(analysis, aes(x=as.factor(csex))) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "dark red", width=0.5) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + ggtitle("Gender of adolescents surveyed") + scale_x_discrete(labels=c("Boy","Girl")) + xlab ("Gender") + labs(caption="Source: Wave 8 of pairfam (2015-16)") + ylab(" ")

ggsave("plot2.png")

#Plot 3: Age of adolescents surveyed
ggplot(analysis, aes(x=as.factor(cdoby))) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "palevioletred") + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + ggtitle("Age of adolescents when surveyed") +  xlab ("Age") + labs(caption="Source: Wave 8 of pairfam (2015-16)") + ylab(" ")

ggsave("plot3.png")


#Plot 4: Distribution of household income
ggplot(analysis, aes(x=anchor_hhincnet)) + geom_histogram(bins=20, fill="plum4") + scale_x_continuous(expand=c(0,0), breaks=pretty_breaks(n=8)) + scale_y_continuous(expand=c(0,0)) + ggtitle("Distribution of household income across respondents") + labs(caption="Source: Wave 8 of pairfam(2015-16)")

ggsave("plot4.png")

#Plot 5: Parents' relationship status
ggplot(analysis, aes(x=as.factor(anchor_relstat))) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "purple4") + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + scale_y_continuous(labels = percent) + ggtitle("Relationship status of adolescents' parents") + scale_x_discrete(labels=c("Never married single","Never married LAT", "Never married COHAB", "Married COHAB", "Married noncohabiting","Divorced/separated single","Divorced/separated LAT","Divorced/separated COHAB","Widowed single","Widowed LAT","Widowed COHAB")) + xlab ("Relationship status") + labs(caption="Note: LAT stands for 'Living apart together'. Source: Wave 8 of pairfam (2015-16)") + ylab(" ") + coord_flip()

ggsave("plot5.png")

#Plot 6: Parents' ethnicity

ggplot(analysis, aes(x=as.factor(anchor_ethni))) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "lightpink") + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +  ggtitle("Ethnicity of adolescents' parents") + scale_x_discrete(labels=c("German native, no migration","Ethnic-German immigrant (Aussiedler)", "Half German", "Turkish background", "Other non-German background")) + xlab ("Ethnicity") + labs(caption="Source: Wave 8 of pairfam (2015-16)") + ylab(" ") + coord_flip()

ggsave("plot6.png")

#Plot 7: Mean SDQ by adolescent gender
meanSDQ <- analysis %>% 
  group_by(csex) %>% 
  summarize(sdqmean = mean(SDQtotal))

ggplot(meanSDQ, aes(x=as.factor(csex), y=sdqmean))+geom_bar(stat="identity", fill="lightcoral", width=0.5) + scale_y_continuous(expand=c(0,0), limits=c(0,40)) + scale_x_discrete(labels=c("Male","Female"))+xlab("Gender")+ylab("Mean SDQ") + ggtitle("Mean SDQ by adolescent gender")+labs(caption="Source:Wave 8 of pairfam (2015-16)") + geom_text(aes(label=sprintf('%.2f', sdqmean)), vjust=-0.75)

ggsave("plot7.png")

#Plot 8: Mean SDQ by adolescent age

meanSDQ1 <- analysis %>% 
  group_by(cdoby) %>% 
  summarize(sdqmean = mean(SDQtotal))

ggplot(meanSDQ1, aes(x=as.factor(cdoby), y=sdqmean))+geom_bar(stat="identity", fill="hotpink4") + scale_y_continuous(expand=c(0,0), limits=c(0,40)) +xlab("Age")+ylab("Mean SDQ") + ggtitle("Mean SDQ by adolescent age")+labs(caption="Source:Wave 8 of pairfam (2015-16)") + geom_text(aes(label=sprintf('%.2f', sdqmean)), vjust=-0.75)

ggsave("plot8.png")

#Duration of marriage across respondents



