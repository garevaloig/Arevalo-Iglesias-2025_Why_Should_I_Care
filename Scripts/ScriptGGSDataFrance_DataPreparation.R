#####################################################################################################
## STUDY 2: Family Elderly Caregiving and Preferences for Family or State Responsibility in France ##
#####################################################################################################

####
## SCRIPT 1: DATA PREPARATION
####

library(foreign)
library(dplyr)
library(DescTools)
library(ggplot2)
library(ltm)
library(forcats)

##
# STEP 1: IMPORT THE DATA AND SUBSELECT ONLY THE FRENCH CASES
##

setwd("D:/BIGSSS/Dissertation/Study 2/Data/GGS Data/GGSAllWavesAllCountries")

GGSW1<-read.spss("GGS_Wave1_V.4.4.sav")
GGSW1<-as.data.frame(GGSW1)
GGSW1<-GGSW1[which(GGSW1$acountry=="France"),]
GGSW1$acountry<-droplevels(GGSW1$acountry)

GGSW2<-read.spss("GGS_Wave2_V.1.3.sav")
GGSW2<-as.data.frame(GGSW2)
GGSW2<-GGSW2[which(GGSW2$bcountry=="France"),]
GGSW2$bcountry<-droplevels(GGSW2$bcountry)

GGSW3<-read.spss("GGS_Wave3_V.1.0.sav")
GGSW3<-as.data.frame(GGSW3)
GGSW3<-GGSW3[which(GGSW3$ccountry=="France"),]
GGSW3$ccountry<-droplevels(GGSW3$ccountry)

###################################################

###
# STEP 2: SELECT RELEVANT VARIABLES
###

GGSW1_sub<-GGSW1[,
c("arid","ayear","acountry","aage","asex","ahhtype","ahhsize","aeduc",
"aactstat","amarstat","aparstat","ankids",
"ahg3_1","ahg3_2","ahg3_3","ahg3_4","ahg3_5",
"ahg3_6","ahg3_7","ahg3_8","ahg3_9","ahg3_10",
"ahg4_1","ahg4_2","ahg4_3","ahg4_4","ahg4_5",
"ahg4_6","ahg4_7","ahg4_8","ahg4_9","ahg4_10",
"ahg5_1","ahg5_2","ahg5_3","ahg5_4","ahg5_5",
"ahg5_6","ahg5_7","ahg5_8","ahg5_9","ahg5_10",
"ahg8_1","ahg8_2","ahg8_3","ahg8_4","ahg8_5",
"ahg8_6","ahg8_7","ahg8_8","ahg8_9","ahg8_10",
"ahg9_1","ahg9_2","ahg9_3","ahg9_4","ahg9_5",
"ahg9_6","ahg9_7","ahg9_8","ahg9_9","ahg9_10",
"a212_1","a212_2","a212_3","a212_4","a212_5",
"a212_6","a212_7","a212_8","a212_9","a212_10",
"a216a_1","a216a_2","a216a_3","a216a_4","a216a_5",
"a216a_6","a216a_7","a216a_8","a216a_9","a216a_10",
"a221h_1","a221h_2","a221h_3","a221h_4","a221h_5",
"a221h_6","a221h_7","a221h_8","a221h_9","a221h_10",
"a223u_1","a223u_2","a223u_3","a223u_4","a223u_5",
"a223u_6","a223u_7","a223u_8","a223u_9","a223u_10",
"a224_1","a224_2","a224_3","a224_4","a224_5",
"a224_6","a224_7","a224_8","a224_9","a224_10",
"a238","a370","a324h","a373y","a379","a381","a382","a383","a384","a407",
"a510","a530",
"a501","a502","a503","a504","a519","a518","a519","a520h","a521u","a522","a538","a539","a540h","a541u","a542",
"a5106b_b","a5106b_s","a5107",
#"a105","a110","a111",
"a701","a702a","a702b","a703a","a703b","a704","a705_1","a705_2","a706",
"a707_1","a707_2","a707_3","a707_4","a707_5",
"a708_1","a708_2","a708_3","a708_4","a708_5",
"a709_1","a709_2","a709_3","a709_4","a709_5",
"a710","a711_1","a711_2","a711_3","a711_4","a711_5",#"a711_6","a711_7","a711_8","a711_9","a711_10",
"a712_1","a712_2","a712_3","a712_4","a712_5",#"a712_6","a712_7","a712_8","a712_9","a712_10",
#"a719_a","a719_b","a719_c","a719_d","a719_e",
"a721_a","a721_b","a721_c","a721_d","a721_e","a721_f","a721_g",
"a828","a832","a835",
"a866_1","a866_2","a866_3","a866_4","a866_5",#"a866_6","a866_7","a866_8","a866_9","a866_10","a866_11","a866_12","a866_13",
"a867_1","a867_2","a867_3","a867_4","a867_5",#"a867_6","a867_7","a867_8","a867_9","a867_10","a867_11","a867_12","a867_13",
"a901","a921",
"a938_1","a938_2","a938_3","a938_4",#"a938_5","a938_6","a938_7","a938_8","a938_9","a938_10","a938_11","a938_12","a938_13",
"a939_1","a939_2","a939_3","a939_4",#"a939_5","a939_6","a939_7","a939_8","a939_9","a939_10","a939_11","a939_12","a939_13",
#"a1008","a1008mnth",
"a1009",
"a1110_a","a1112_a","a1112_b","a1112_c","a1112_d","a1112_e",
"a512","a532",
"aweight")]

GGSW1_sub$a512<-as.numeric(paste(GGSW1_sub$a512))
GGSW1_sub$a532<-as.numeric(paste(GGSW1_sub$a532))
GGSW1_sub$Wave<-rep(1,nrow(GGSW1_sub))


###

GGSW2_sub<-GGSW2[,
c("brid","byear","bcountry","bage","bsex","bhhtype","bhhsize","beduc",
"bactstat","bmarstat","bparstat","bnkids",
"bhg3_1","bhg3_2","bhg3_3","bhg3_4","bhg3_5",
"bhg3_6","bhg3_7","bhg3_8","bhg3_9","bhg3_10",
"bhg4_1","bhg4_2","bhg4_3","bhg4_4","bhg4_5",
"bhg4_6","bhg4_7","bhg4_8","bhg4_9","bhg4_10",
"bhg5_1","bhg5_2","bhg5_3","bhg5_4","bhg5_5",
"bhg5_6","bhg5_7","bhg5_8","bhg5_9","bhg5_10",
"bhg8_1","bhg8_2","bhg8_3","bhg8_4","bhg8_5",
"bhg8_6","bhg8_7","bhg8_8","bhg8_9","bhg8_10",
"bhg9_1","bhg9_2","bhg9_3","bhg9_4","bhg9_5",
"bhg9_6","bhg9_7","bhg9_8","bhg9_9","bhg9_10",
"b212_1","b212_2","b212_3","b212_4","b212_5",
"b212_6","b212_7","b212_8","b212_9","b212_10",
"b216a_1","b216a_2","b216a_3","b216a_4","b216a_5",
"b216a_6","b216a_7","b216a_8","b216a_9","b216a_10",
"b221h_1","b221h_2","b221h_3","b221h_4","b221h_5",
"b221h_6","b221h_7","b221h_8","b221h_9","b221h_10",
"b223u_1","b223u_2","b223u_3","b223u_4","b223u_5",
"b223u_6","b223u_7","b223u_8","b223u_9","b223u_10",
"b224_1","b224_2","b224_3","b224_4","b224_5",
"b224_6","b224_7","b224_8","b224_9","b224_10",
"b238","b370","b324h","b373y","b379","b381","b382","b383","b384","b407",
"b510","b530",
"b501","b502","b503","b504","b519","b518","b519","b520h","b521u","b522","b538","b539","b540h","b541u","b542",
"b5106b_b","b5106b_s","b5107",
#"b105","b110","b111",
"b701","b702a","b702b","b703a","b703b","b704","b705_1","b705_2","b706",
"b707_1","b707_2","b707_3","b707_4","b707_5",
"b708_1","b708_2","b708_3","b708_4","b708_5",
"b709_1","b709_2","b709_3","b709_4","b709_5",
"b710","b711_1","b711_2","b711_3","b711_4","b711_5",#"b711_6","b711_7","b711_8","b711_9","b711_10",
"b712_1","b712_2","b712_3","b712_4","b712_5",#"b712_6","b712_7","b712_8","b712_9","b712_10",
#"b719_a","b719_b","b719_c","b719_d","b719_e",
"b721_a","b721_b","b721_c","b721_d","b721_e","b721_f","b721_g",
"b828","b832","b835",
"b866_1","b866_2","b866_3","b866_4","b866_5",#"b866_6","b866_7","b866_8","b866_9","b866_10","b866_11","b866_12","b866_13",
"b867_1","b867_2","b867_3","b867_4","b867_5",#"b867_6","b867_7","b867_8","b867_9","b867_10","b867_11","b867_12","b867_13",
"b901","b921",
"b938_1","b938_2","b938_3","b938_4",#"b938_5","b938_6","b938_7","b938_8","b938_9","b938_10","b938_11","b938_12","b938_13",
"b939_1","b939_2","b939_3","b939_4",#"b939_5","b939_6","b939_7","b939_8","b939_9","b939_10","b939_11","b939_12","b939_13",
#"b1008","b1008mnth",
"b1009",
"b1110_a","b1112_a","b1112_b","b1112_c","b1112_d","b1112_e",
"bweight")]

GGSW2_sub$b512<-numeric(nrow(GGSW2_sub))
GGSW2_sub$b532<-numeric(nrow(GGSW2_sub))

GGSW2_sub$Wave<-rep(2,nrow(GGSW2_sub))

colnames(GGSW2_sub)=colnames(GGSW1_sub)

###

GGSW3_sub<-GGSW3[,
c("crid","cyear","ccountry","cage","csex","chhtype","chhsize","ceduc",
"cactstat","cmarstat","cparstat","cnkids",
"chg3_1","chg3_2","chg3_3","chg3_4","chg3_5",
"chg3_6","chg3_7","chg3_8","chg3_9","chg3_10",
"chg4_1","chg4_2","chg4_3","chg4_4","chg4_5",
"chg4_6","chg4_7","chg4_8","chg4_9","chg4_10",
"chg5_1","chg5_2","chg5_3","chg5_4","chg5_5",
"chg5_6","chg5_7","chg5_8","chg5_9","chg5_10",
"chg8_1","chg8_2","chg8_3","chg8_4","chg8_5",
"chg8_6","chg8_7","chg8_8","chg8_9","chg8_10",
"chg9_1","chg9_2","chg9_3","chg9_4","chg9_5",
"chg9_6","chg9_7","chg9_8","chg9_9","chg9_10",
"c212_1","c212_2","c212_3","c212_4","c212_5",
"c212_6","c212_7","c212_8","c212_9","c212_10",
"c216a_1","c216a_2","c216a_3","c216a_4","c216a_5",
"c216a_6","c216a_7","c216a_8","c216a_9","c216a_10",
"c221h_1","c221h_2","c221h_3","c221h_4","c221h_5",
"c221h_6","c221h_7","c221h_8","c221h_9","c221h_10",
"c223u_1","c223u_2","c223u_3","c223u_4","c223u_5",
"c223u_6","c223u_7","c223u_8","c223u_9","c223u_10",
"c224_1","c224_2","c224_3","c224_4","c224_5",
"c224_6","c224_7","c224_8","c224_9","c224_10",
"cn238","c370","c324h","c373y","c379","c381","c382","c383","c384","c407",
"c510","c530",
"c501","c502","c503","c504","c519","c518","c519","c520h","c521u","c522","c538","c539","c540h","c541u","c542",
"c5106b_b","c5106b_s","c5107",
#"c105","c110","c111",
"c701","c702a","c702b","c703a","c703b","c704","c705_1","c705_2","c706",
"c707_1","c707_2","c707_3","c707_4","c707_5",
"c708_1","c708_2","c708_3","c708_4","c708_5",
"c709_1","c709_2","c709_3","c709_4","c709_5",
"c710","c711_1","c711_2","c711_3","c711_4","c711_5",#"c711_6","c711_7","c711_8","c711_9","c711_10",
"c712_1","c712_2","c712_3","c712_4","c712_5",#"c712_6","c712_7","c712_8","c712_9","c712_10",
#"c719_a","c719_b","c719_c","c719_d","c719_e",
"c721a","c721b","c721c","c721d","c721e","c721f","c721g",
"c832","c832","c835",
"c866_1","c866_2","c866_3","c866_4","c866_5",#"c866_6","c866_7","c866_8","c866_9","c866_10","c866_11","c866_12","c866_13",
"c867_1","c867_2","c867_3","c867_4","c867_5",#"c867_6","c867_7","c867_8","c867_9","c867_10","c867_11","c867_12","c867_13",
"c901","c921",
"c938_1","c938_2","c938_3","c938_4",#"c938_5","c938_6","c938_7","c938_8","c938_9","c938_10","c938_11","c938_12","c938_13",
"c939_1","c939_2","c939_3","c939_4",#"c939_5","c939_6","c939_7","c939_8","c939_9","c939_10","c939_11","c939_12","c939_13",
#"c1008","c1008mnth",
"c1009",
"c1110_a","c1112_a","c1112_b","c1112_c","c1112_d","c1112_e",
"cweight")]

GGSW3_sub$c512<-numeric(nrow(GGSW3_sub))
GGSW3_sub$c532<-numeric(nrow(GGSW3_sub))

GGSW3_sub$Wave<-rep(3,nrow(GGSW3_sub))

colnames(GGSW3_sub)=colnames(GGSW1_sub)
GGSW3_sub$a828<-rep(NA,nrow(GGSW3_sub))

#################################################

###
# STEP 3: OBTAIN A PANEL SAMPLE FOR FRANCE
###

GGSW_France<-rbind(GGSW1_sub,GGSW2_sub,GGSW3_sub) # All respondents in France

# Final sample: Only those who are in all 3 waves
intersection<-intersect(intersect(GGSW1_sub$arid,GGSW2_sub$arid),GGSW3_sub$arid)
GGSWP_France<-GGSW_France[which(GGSW_France$arid %in% intersection),]
# A total of 5435 respondents in all three waves (16305 observations)

# Order all waves based on the IDs
GGSWP_France[which(GGSWP_France$Wave==1),]<-GGSWP_France[which(GGSWP_France$Wave==1),][order(GGSWP_France$arid[which(GGSWP_France$Wave==1)]),]
GGSWP_France[which(GGSWP_France$Wave==2),]<-GGSWP_France[which(GGSWP_France$Wave==2),][order(GGSWP_France$arid[which(GGSWP_France$Wave==2)]),]
GGSWP_France[which(GGSWP_France$Wave==3),]<-GGSWP_France[which(GGSWP_France$Wave==3),][order(GGSWP_France$arid[which(GGSWP_France$Wave==3)]),]

GGSWP_France$Wave=as.factor(GGSWP_France$Wave)

save(list=ls(),file="GGSWP_France_session_0.RData")

##################################################

#load("GGSWP_France_session_0.RData")

###
# STEP 4: CLEAN AND HARMONISE RELEVANT VARIABLES
###

# ID

# DEPENDENT VARIABLE: Preferences for family or state responsibility for elderly care
GGSWP_France$a1110_a[which(GGSWP_France$a1110_a=="does not know" |
                             GGSWP_France$a1110_a=="no response/not applicable" |
                             GGSWP_France$a1110_a=="does not know_duplicated_9997")]=NA
GGSWP_France$a1110_a<-droplevels(GGSWP_France$a1110_a)
GGSWP_France$a1110_a<-ordered(GGSWP_France$a1110_a)
GGSWP_France$a1110_a<-fct_rev(GGSWP_France$a1110_a)

GGSWP_France$a1110_a_num<-as.numeric(GGSWP_France$a1110_a) 
# Likert scale from 1 ("mainly for society") to 5 ("mainly for the family")

######

# TREATMENT: Does respondent give care to an elderly parent or partner

# Any regular help given over last 12 months?
GGSWP_France$a710<-droplevels(GGSWP_France$a710) # 1087 "yes", of which 332 are in Wave 1.


# To whom did respondent provide help (up to 5 receivers -> receivers 4 and 5 are only NAs)
GGSWP_France$a711_1<-recode_factor(GGSWP_France$a711_1,
                                   "friend,acquaintance, neighbour, colleague"="friend, acquaintance, neighbour, colleague")
GGSWP_France$a711_1<-droplevels(GGSWP_France$a711_1)

GGSWP_France$a711_1_simp<-recode_factor(GGSWP_France$a711_1,
                                        "partner/spouse"="partner/spouse",
                                        "mother"="parent or stepparent","father"="parent or stepparent", 
                                        "mother of R's partner/spouse"="parent or stepparent", # Parents of respondents' partner also included as parent
                                        "father of R's partner/spouse"="parent or stepparent",
                                        "grandmother"="grandparent or great-grandparent","grandfather"="grandparent or great-grandparent",
                                        "grandmother or great-grandmother"="grandparent or great-grandparent",
                                        "sister"="sibling","brother"="sibling",
                                        "son"="child or stepchild","daughter"="child or stepchild","step-son"="child or stepchild",
                                        "step-daughter"="child or stepchild",
                                        "granddaughter"="grandchild or great-grandchild","grandson"="grandchild or great-grandchild",
                                        "granddaughter or great-granddaughter"="grandchild or great-grandchild",
                                        "another relative"="another relative","friend,acquaintance, neighbour, colleague"="not a family member",
                                        "other person"="not a family member","friend, acquaintance, neighbour, colleague"="not a family member")

GGSWP_France$a711_2<-recode_factor(GGSWP_France$a711_2,
                                   "friend,acquaintance, neighbour, colleague"="friend, acquaintance, neighbour, colleague")
GGSWP_France$a711_2[which(GGSWP_France$a711_2=="no response/not applicable" |
                            GGSWP_France$a711_2=="does not know_duplicated_9999")]=NA
GGSWP_France$a711_2<-droplevels(GGSWP_France$a711_2)

GGSWP_France$a711_2_simp<-recode_factor(GGSWP_France$a711_2,
                                        "partner/spouse"="partner/spouse",
                                        "mother"="parent or stepparent","father"="parent or stepparent", 
                                        "mother of R's partner/spouse"="parent or stepparent", # Parents of respondents' partner also included as parent
                                        "father of R's partner/spouse"="parent or stepparent",
                                        "grandmother"="grandparent or great-grandparent","grandfather"="grandparent or great-grandparent",
                                        "grandmother or great-grandmother"="grandparent or great-grandparent",
                                        "sister"="sibling","brother"="sibling",
                                        "son"="child or stepchild","daughter"="child or stepchild","step-son"="child or stepchild",
                                        "step-daughter"="child or stepchild",
                                        "granddaughter"="grandchild or great-grandchild","grandson"="grandchild or great-grandchild",
                                        "granddaughter or great-granddaughter"="grandchild or great-grandchild",
                                        "another relative"="another relative","friend,acquaintance, neighbour, colleague"="not a family member",
                                        "other person"="not a family member","friend, acquaintance, neighbour, colleague"="not a family member")

GGSWP_France$a711_3[which(GGSWP_France$a711_3=="no response/not applicable" |
                            GGSWP_France$a711_3=="does not know_duplicated_9999")]=NA
GGSWP_France$a711_3<-droplevels(GGSWP_France$a711_3)

GGSWP_France$a711_3_simp<-recode_factor(GGSWP_France$a711_3,
                                        "partner/spouse"="partner/spouse",
                                        "mother"="parent or stepparent","father"="parent or stepparent", 
                                        "mother of R's partner/spouse"="parent or stepparent", # Parents of respondents' partner also included as parent
                                        "father of R's partner/spouse"="parent or stepparent",
                                        "grandmother"="grandparent or great-grandparent","grandfather"="grandparent or great-grandparent",
                                        "grandmother or great-grandmother"="grandparent or great-grandparent",
                                        "sister"="sibling","brother"="sibling",
                                        "son"="child or stepchild","daughter"="child or stepchild","step-son"="child or stepchild",
                                        "step-daughter"="child or stepchild",
                                        "granddaughter"="grandchild or great-grandchild","grandson"="grandchild or great-grandchild",
                                        "granddaughter or great-granddaughter"="grandchild or great-grandchild",
                                        "another relative"="another relative","friend,acquaintance, neighbour, colleague"="not a family member",
                                        "other person"="not a family member","friend, acquaintance, neighbour, colleague"="not a family member")

summary(GGSWP_France$a711_4) # No relevant info
summary(GGSWP_France$a711_5)

# Does care receiver live in the HH? (up to 5 receivers, but no info on receivers 4 and 5)
GGSWP_France$a712_1<-droplevels(GGSWP_France$a712_1)
GGSWP_France$a712_2<-droplevels(GGSWP_France$a712_2)
GGSWP_France$a712_3<-droplevels(GGSWP_France$a712_3)

summary(GGSWP_France$a712_4) # No relevant info
summary(GGSWP_France$a712_5)

# Year
GGSWP_France$ayear<-droplevels(GGSWP_France$ayear) # Responses from years 2005, 2008 and 2011. 3 year lag between responses.

# Current partner's year of birth
GGSWP_France$a373y<-as.numeric(paste(GGSWP_France$a373y))

# Current partner's age
GGSWP_France$partnerage<-numeric(nrow(GGSWP_France))
for (i in 1:nrow(GGSWP_France)){
  GGSWP_France$partnerage[i]=as.numeric(paste(GGSWP_France$ayear[i]))-GGSWP_France$a373y[i]
}

# Year of birth of biological mother
GGSWP_France$a512[which(GGSWP_France$Wave==2)]=GGSWP_France$a512[which(GGSWP_France$Wave==1)]+3
GGSWP_France$a512[which(GGSWP_France$Wave==3)]=GGSWP_France$a512[which(GGSWP_France$Wave==1)]+6

# Age of biological mother
GGSWP_France$motherage<-numeric(nrow(GGSWP_France))
for (i in 1:nrow(GGSWP_France)){
  GGSWP_France$motherage[i]=as.numeric(paste(GGSWP_France$ayear[i]))-GGSWP_France$a512[i]
}

# Year of birth of biological father
GGSWP_France$a532[which(GGSWP_France$Wave==2)]=GGSWP_France$a532[which(GGSWP_France$Wave==1)]+3
GGSWP_France$a532[which(GGSWP_France$Wave==3)]=GGSWP_France$a532[which(GGSWP_France$Wave==1)]+6

# Age of biological father
GGSWP_France$fatherage<-numeric(nrow(GGSWP_France))
for (i in 1:nrow(GGSWP_France)){
  GGSWP_France$fatherage[i]=as.numeric(paste(GGSWP_France$ayear[i]))-GGSWP_France$a532[i]
}

# A variable only with care for family members 60 or over with 5 categories
GGSWP_France$cares<-factor(nrow(GGSWP_France),levels=c("partner/spouse","mother","father","grandparent","no care"))

GGSWP_France$cares[which(GGSWP_France$a710=="no")]="no care"
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="partner/spouse" &  # Partner/spouse only when 60 or older
                            GGSWP_France$partnerage<60)]="no care"
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="partner/spouse" & 
                            GGSWP_France$partnerage>=60)]="partner/spouse"
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="mother" &   # Mother only when 60 or older
                            GGSWP_France$motherage<60)]="no care"
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="mother" & 
                            GGSWP_France$motherage>=60)]="mother"
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="father" &  # Father only when 60 or older
                            GGSWP_France$fatherage<60)]="no care"
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="father" & 
                            GGSWP_France$fatherage>=60)]="father"
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="grandmother")]="grandparent" # Grandparents always count as elderly care
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="grandmother or great-grandmother")]="grandparent"
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="grandfather")]="grandparent"
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="friend, acquaintance, neighbour, colleague")]="no care"
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="granddaughter")]="no care"
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="son")]="no care"
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="step-daughter")]="no care"
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="granddaughter or great-granddaughter")]="no care"
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="grandson")]="no care"
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="other person")]="no care"
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="daughter")]="no care"
# The rest of the cases (siblings, partners' parents and "other relatives", are marked as NA. See explanation
# under the commented code below.)
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="sister")]=NA
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="brother")]=NA
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="mother of R's partner/spouse")]=NA
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="father of R's partner/spouse")]=NA
GGSWP_France$cares[which(GGSWP_France$a710=="yes" & 
                            GGSWP_France$a711_1=="another relative")]=NA

# A binary variable for care
GGSWP_France$caresbin<-recode_factor(GGSWP_France$cares,
                                     "partner/spouse"="yes",
                                     "mother"="yes",
                                     "father"="yes",
                                     "grandparent"="no",
                                     "no care"="no")

GGSWP_France$caresbin<-factor(GGSWP_France$caresbin,levels=c("no","yes"))

GGSWP_France$caresbin_num<-as.numeric(GGSWP_France$caresbin)-1

GGSWP_France$caresparent<-recode_factor(GGSWP_France$cares,
                                        "partner/spouse"="no",
                                        "mother"="yes",
                                        "father"="yes",
                                        "grandparent"="no",
                                        "no care"="no")

GGSWP_France$caresparent<-factor(GGSWP_France$caresparent,levels=c("no","yes"))

GGSWP_France$carespartner<-recode_factor(GGSWP_France$cares,
                                         "partner/spouse"="yes",
                                         "mother"="no",
                                         "father"="no",
                                         "grandparent"="no",
                                         "no care"="no")

GGSWP_France$carespartner<-factor(GGSWP_France$carespartner,levels=c("no","yes"))

GGSWP_France$caresparent[which(GGSWP_France$carespartner=="yes")]=NA
GGSWP_France$carespartner[which(GGSWP_France$caresparent=="yes")]=NA

GGSWP_France$caresparent_num<-as.numeric(GGSWP_France$caresparent)-1
GGSWP_France$carespartner_num<-as.numeric(GGSWP_France$carespartner)-1

# A 3 level factor with levels "cares for parent", "cares for partner", "does not care"
GGSWP_France$cares_grp<-recode_factor(GGSWP_France$cares,"partner/spouse"="partner/spouse",
                                     "mother"="parent","father"="parent",
                                     "grandparent"="no care","no care"="no care")

GGSWP_France$cares_grp<-factor(GGSWP_France$cares_grp,levels=c("no care","partner/spouse","parent"))

# Fixing care to 1 for those who have cared in previous waves

GGSWP_France$caresbin_exp<-GGSWP_France$caresbin
GGSWP_France$caresbin_exp[which(GGSWP_France$arid %in% GGSWP_France$arid[which(GGSWP_France$caresbin=="yes" & GGSWP_France$Wave==1)])]="yes"

GGSWP_France$caresparent_exp<-GGSWP_France$caresparent
GGSWP_France$caresparent_exp[which(GGSWP_France$arid %in% GGSWP_France$arid[which(GGSWP_France$caresparent=="yes" & GGSWP_France$Wave==1)])]="yes"

GGSWP_France$carespartner_exp<-GGSWP_France$carespartner
GGSWP_France$carespartner_exp[which(GGSWP_France$arid %in% GGSWP_France$arid[which(GGSWP_France$carespartner=="yes" & GGSWP_France$Wave==1)])]="yes"

GGSWP_France$caresgrp_exp<-factor(nrow(GGSWP_France),levels=levels(GGSWP_France$cares_grp))
GGSWP_France$caresgrp_exp[which(GGSWP_France$caresbin_exp=="no")]="no care"
GGSWP_France$caresgrp_exp[which(GGSWP_France$caresparent_exp=="yes")]="parent"
GGSWP_France$caresgrp_exp[which(GGSWP_France$carespartner_exp=="yes")]="partner/spouse"

######

# MEDIATOR: Well-being


# Mental well-being items: I felt I could not shake off the blues (experienced last week)
GGSWP_France$a721_a<-droplevels(GGSWP_France$a721_a)
GGSWP_France$a721_a_num<-as.numeric(GGSWP_France$a721_a) # 1=seldom or never, 4=most of the time

# Mental well-being items: I felt depressed (experienced last week)
GGSWP_France$a721_b<-droplevels(GGSWP_France$a721_b)
GGSWP_France$a721_b_num<-as.numeric(GGSWP_France$a721_b) # 1=seldom or never, 4=most of the time

# Mental well-being items: I thought my life had been a failure (experienced last week)
GGSWP_France$a721_c<-droplevels(GGSWP_France$a721_c)
GGSWP_France$a721_c_num<-as.numeric(GGSWP_France$a721_c) # 1=seldom or never, 4=most of the time

# Mental well-being items: I felt fearful (experienced last week)
GGSWP_France$a721_d<-droplevels(GGSWP_France$a721_d)
GGSWP_France$a721_d_num<-as.numeric(GGSWP_France$a721_d) # 1=seldom or never, 4=most of the time

# Mental well-being items: I felt lonely (experienced last week)
GGSWP_France$a721_e<-droplevels(GGSWP_France$a721_e)
GGSWP_France$a721_e_num<-as.numeric(GGSWP_France$a721_e) # 1=seldom or never, 4=most of the time

# Mental well-being items: I had crying spells (experienced last week)
GGSWP_France$a721_f<-droplevels(GGSWP_France$a721_f)
GGSWP_France$a721_f_num<-as.numeric(GGSWP_France$a721_f) # 1=seldom or never, 4=most of the time

# Mental well-being items: I felt sad (experienced last week)
GGSWP_France$a721_g<-droplevels(GGSWP_France$a721_g)
GGSWP_France$a721_g_num<-as.numeric(GGSWP_France$a721_g) # 1=seldom or never, 4=most of the time

# Well-being factor (created from a721 variables)
cronbach.alpha(data.frame(GGSWP_France$a721_a,
                          GGSWP_France$a721_b,
                          GGSWP_France$a721_c,
                          GGSWP_France$a721_d,
                          GGSWP_France$a721_e,
                          GGSWP_France$a721_f,
                          GGSWP_France$a721_g)) # alpha=0.90, very good convergence into an index

GGSWP_France$wellbeing_index<-numeric(nrow(GGSWP_France))
for (i in 1:nrow(GGSWP_France)){
  GGSWP_France$wellbeing_index[i]=mean(c(GGSWP_France$a721_a_num[i],
                                    GGSWP_France$a721_b_num[i],
                                    GGSWP_France$a721_c_num[i],
                                    GGSWP_France$a721_d_num[i],
                                    GGSWP_France$a721_e_num[i],
                                    GGSWP_France$a721_f_num[i],
                                    GGSWP_France$a721_g_num[i]))}

GGSWP_France$wellbeing_index<-abs(GGSWP_France$wellbeing_index-5)


######

# SOCIODEMOGRAPHICS

# Age
GGSWP_France$aage<-as.numeric(paste(droplevels(GGSWP_France$aage))) # Age range: 17-85
hist(GGSWP_France$aage,freq=F) # The age distribution approximates a normal 

GGSWP_France$aage_grp<-factor(nrow(GGSWP_France),levels=c("Under 45","45-59","60 or over"))
GGSWP_France$aage_grp[which(GGSWP_France$aage<45)]="Under 45"
GGSWP_France$aage_grp[which(GGSWP_France$aage>=45 & GGSWP_France$aage<60)]="45-59"
GGSWP_France$aage_grp[which(GGSWP_France$aage>=60)]="60 or over"

# Sex
GGSWP_France$asex[which(GGSWP_France$asex=="Male")]="male"
GGSWP_France$asex[which(GGSWP_France$asex=="Female")]="female"
GGSWP_France$asex<-droplevels(GGSWP_France$asex)
summary(GGSWP_France$asex) # 9508 women and 6797 men

# Household type
GGSWP_France$ahhtype<-droplevels(GGSWP_France$ahhtype)
plot(GGSWP_France$ahhtype) # Most common answers are living alone or in a couple with or without children.
# Living with parents is not very common.

# Educational level ## THERE IS A PROBLEM HERE: NO VALUES OF EDUCATION FOR THE FIRST WAVE -> TAKE VALUES FROM SECOND WAVE AND TURN INTO CONSTANT:
GGSWP_France$aeduc[which(GGSWP_France$Wave==1)]=GGSWP_France$aeduc[which(GGSWP_France$Wave==2)]
GGSWP_France$aeduc[which(GGSWP_France$Wave==3)]=GGSWP_France$aeduc[which(GGSWP_France$Wave==2)]
GGSWP_France$aeduc[which(GGSWP_France$aeduc=="does not know")]=NA
GGSWP_France$aeduc<-droplevels(GGSWP_France$aeduc)
GGSWP_France$aeduc_grp<-recode_factor(GGSWP_France$aeduc,
"0-1-2 - isced97"="ISCED 0-1-2","0 - isced97"="ISCED 0-1-2", "1-2 - isced97"="ISCED 0-1-2",
"3A - isced97"="ISCED 3", "3B - isced97"="ISCED 3", "3C - isced97"="ISCED 3",
"5A-6 - isced97"="ISCED 5-6", "5B - isced97"="ISCED 5-6")
# Grouped the values in 3 ISCED groups: 0-2 (up to lower secondary education),
# 3 (upper secondary education) and 5-6 (tertiary education).
# Note: whole first wave is missing for France. Only data from W2 and 3 on education.
GGSWP_France$aeduc_bin<-recode_factor(GGSWP_France$aeduc_grp,
                                      "ISCED 0-1-2"="not completed higher secondary education",
                                      "ISCED 3"="completed higher secondary education",
                                      "ISCED 5-6"="completed higher secondary education")

# Marital status
GGSWP_France$amarstat[which(GGSWP_France$amarstat=="Never Married")]="never married"
GGSWP_France$amarstat[which(GGSWP_France$amarstat=="Married")]="married"
GGSWP_France$amarstat[which(GGSWP_France$amarstat=="Divorced" | GGSWP_France$amarstat=="In Divorce Process")]="divorced"
GGSWP_France$amarstat[which(GGSWP_France$amarstat=="Widowed")]="widowed"
GGSWP_France$amarstat[which(GGSWP_France$amarstat=="unknown")]=NA
GGSWP_France$amarstat<-droplevels(GGSWP_France$amarstat)
# Note: There is another level: PACS. Refers to Pacte Civil de Solidarite, which offers a legal status carrying
# some but not all the benefits of the marriage (similar to Pareja de Hecho in Spain).

GGSWP_France$amarstat_bin<-recode_factor(GGSWP_France$amarstat,
"never married" = "not married", "married" = "married", "divorced" = "not married", "widowed" = "not married",
"PACS" = "not married") # More or less half and half of the sample are respectively married and not married

# Partnership status
GGSWP_France$aparstat<-droplevels(GGSWP_France$aparstat)
GGSWP_France$aparstat<-factor(GGSWP_France$aparstat,levels=c("no partner","non-resident partner","co-resident partner"))

GGSWP_France$haspartner<-recode_factor(GGSWP_France$aparstat,
                                       "no partner"="no",
                                       "non-resident partner"="yes",
                                       "co-resident partner"="yes")

# Lives with partner
GGSWP_France$lwpartner<-factor(nrow(GGSWP_France),levels=c("no","yes"))
GGSWP_France$lwpartner[which(GGSWP_France$aparstat=="no partner")]="no"
GGSWP_France$lwpartner[which(GGSWP_France$aparstat=="non-resident partner")]="no"
GGSWP_France$lwpartner[which(GGSWP_France$aparstat=="co-resident partner")]="yes"

# Limitation or disability of partner?
GGSWP_France$dispartner<-factor(GGSWP_France$a382,levels=c("no","yes"))

# Number of kids
hist(GGSWP_France$ankids, breaks=13) # Looks like an exponential distribution. 
GGSWP_France$haskids<-factor(rep("yes",nrow(GGSWP_France)),levels=c("yes","no"))
GGSWP_France$haskids[which(GGSWP_France$ankids==0)]="no" # three quarters of the sample have at least 1 kid
GGSWP_France$haskids<-factor(GGSWP_France$haskids,levels=c("no","yes"))

##### 

# INFORMATION OF PARTNER AND PARENTS

# Time distance (hours) to partner's residence
GGSWP_France$a324h<-as.numeric(paste(GGSWP_France$a324h))
GGSWP_France$a324h[which(GGSWP_France$aparstat=="co-resident partner")]=0

# Everyday activities limitation of the current partner
GGSWP_France$a382[which(GGSWP_France$a382=="not applicable/no response")]=NA
GGSWP_France$a382[which(GGSWP_France$a382=="does not know")]=NA
GGSWP_France$a382<-droplevels(GGSWP_France$a382)

# Sex of the current partner
GGSWP_France$a384[which(GGSWP_France$a384=="not applicable/no response")]=NA
GGSWP_France$a384<-droplevels(GGSWP_France$a384)

# Satisfaction with relationship to current partner
GGSWP_France$a407[which(GGSWP_France$a407=="no response/not applicable" |
GGSWP_France$a407=="refusal_duplicated_9998" |
GGSWP_France$a407=="does not know_duplicated_9999" |
GGSWP_France$a407=="1501" |
GGSWP_France$a407=="not asked when another person present during the interview" |
GGSWP_France$a407=="others present at interview")]=NA
GGSWP_France$a407<-recode_factor(GGSWP_France$a407,
"0=not at all satisfied"="0",
"10=completely satisfied"="10")
GGSWP_France$a407<-as.numeric(paste(GGSWP_France$a407))

# Living with parent(s)? 

GGSWP_France$lwparents<-recode_factor(GGSWP_France$a501,
                                      "not living with your parents"="no",
                                      "living with your father (not your mother)"="yes",
                                      "living with your mother (not your father)"="yes",
                                      "living with both of your parents"="yes")
GGSWP_France$lwparents<-droplevels(GGSWP_France$lwparents)

# Is biological mother alive?
GGSWP_France$a510<-recode_factor(GGSWP_France$a510,
                                 "no, not alive any more"="no",
                                 "yes, still alive"="yes")
GGSWP_France$a510[which(GGSWP_France$a510=="I do not know whether she still is alive")]=NA
GGSWP_France$a510[which(GGSWP_France$a510=="I do not know anything about my biological mother")]=NA
GGSWP_France$a510<-droplevels(GGSWP_France$a510)

# Limitation or disability of mother?
GGSWP_France$a518[which(GGSWP_France$a518!="yes" & GGSWP_France$a518!="no")]=NA
GGSWP_France$a518=droplevels(GGSWP_France$a518)

# Does respondent live with mother?
GGSWP_France$a519[which(GGSWP_France$a519!="yes" & GGSWP_France$a519!="no")]=NA
GGSWP_France$a519=droplevels(GGSWP_France$a519)

# Distance to mother's house (in hours)
GGSWP_France$a520<-as.numeric(paste(GGSWP_France$a520))
GGSWP_France$a520[which(GGSWP_France$a519=="yes")]=0

# Is biological father alive?
GGSWP_France$a530<-recode_factor(GGSWP_France$a530,
                                 "no, not alive any more"="no",
                                 "yes, still alive"="yes")
GGSWP_France$a530[which(GGSWP_France$a530=="I do not know whether he still is alive")]=NA
GGSWP_France$a530[which(GGSWP_France$a530=="I do not know anything about my biological father")]=NA
GGSWP_France$a530[which(GGSWP_France$a530=="does not know")]=NA
GGSWP_France$a530<-droplevels(GGSWP_France$a530)

# Distance to father's house (in hours)
GGSWP_France$a540<-as.numeric(paste(GGSWP_France$a540))

# Limitation or disability of father?
GGSWP_France$a538[which(GGSWP_France$a538!="yes" & GGSWP_France$a538!="no")]=NA
GGSWP_France$a538=droplevels(GGSWP_France$a538)

# Any parent with disability
GGSWP_France$disparent<-factor(nrow(GGSWP_France),levels=c("no","yes"))
GGSWP_France$disparent[which(GGSWP_France$a518=="no" & GGSWP_France$a538=="no")]="no"
GGSWP_France$disparent[which(GGSWP_France$a518=="yes" & GGSWP_France$a538=="no")]="yes"
GGSWP_France$disparent[which(GGSWP_France$a518=="no" & GGSWP_France$a538=="yes")]="yes"
GGSWP_France$disparent[which(GGSWP_France$a518=="yes" & GGSWP_France$a538=="yes")]="yes"

#####################################################

# HEALTH, NEED FOR CARE AND CARE RECEIVING

# General health status
GGSWP_France$a701<-recode_factor(GGSWP_France$a701,
"Very good"="very good",
"Good"="good","Fair"="fair",
"Bad"="bad","Very bad"="very bad")
GGSWP_France$a701<-droplevels(GGSWP_France$a701)

GGSWP_France$health_num<-as.numeric(GGSWP_France$a701) # Likert scale 1-5. Higher values indicate worse health.
hist(GGSWP_France$health_num,breaks=4) # Seems to follow an exponential distribution

GGSWP_France$health_num<-abs(GGSWP_France$health_num-6)

# Any long-standing illness or chronic condition
GGSWP_France$a702a<-recode_factor(GGSWP_France$a702a,
"Yes"="yes","No"="no")
GGSWP_France$a702a[which(GGSWP_France$a702a=="no response/not applicable")]=NA
GGSWP_France$a702a<-droplevels(GGSWP_France$a702a)

# Any health-related limitation or disability
GGSWP_France$a703a[which(GGSWP_France$a703a=="no response/not applicable")]=NA
GGSWP_France$a703a<-droplevels(GGSWP_France$a703a)

# Any regular help needed in personal care
GGSWP_France$a704[which(GGSWP_France$a704=="no response/not applicable")]=NA
GGSWP_France$a704[which(is.na(GGSWP_France$a704))]="no" # I have checked that all NAs in wave 2 are respondents under 40 years of age.
                                                        # Therefore I think it is safe to mark them as "no". No NAs in the other 2 waves.
GGSWP_France$a704=droplevels(GGSWP_France$a704)
GGSWP_France$needscare<-factor(GGSWP_France$a704,levels=c("no","yes"))

# Has received any regular help over last 12 months from professional person (for persons 1 and 2)
GGSWP_France$a705_1<-recode_factor(GGSWP_France$a705_1,
                                   "yes"="yes, not specified")
GGSWP_France$a705_1<-droplevels(GGSWP_France$a705_1)

GGSWP_France$a705_1_simp<-recode_factor(GGSWP_France$a705_1,
                                        "yes, from the public sector"="yes",
                                        "yes, from a private organization"="yes",
                                        "yes, not specified"="yes")

GGSWP_France$a705_2[which(GGSWP_France$a705_2=="no response/not applicable")]=NA
GGSWP_France$a705_2<-recode_factor(GGSWP_France$a705_2,
                                   "yes"="yes, not specified")
GGSWP_France$a705_2<-droplevels(GGSWP_France$a705_2)

GGSWP_France$a705_2_simp<-recode_factor(GGSWP_France$a705_2,
                                        "yes, from the public sector"="yes",
                                        "yes, from a private organization"="yes",
                                        "yes, not specified"="yes")

GGSWP_France$a705<-factor(nrow(GGSWP_France),levels=c("yes","no"))
GGSWP_France$a705[which(GGSWP_France$a705_1_simp=="yes" | GGSWP_France$a705_2_simp=="yes")]="yes"
GGSWP_France$a705[which(GGSWP_France$a705_1_simp=="no" & GGSWP_France$a705_2_simp=="no")]="no"
GGSWP_France$a705[which(GGSWP_France$a704=="no")]="no" # Those who do not have needs are also classified as not receiving care instead of NA

GGSWP_France$rec_profcare<-GGSWP_France$a705

# Has received any regular help over last 4 12 months from other person
GGSWP_France$a706<-droplevels(GGSWP_France$a706) # 109 yes, 373 no, 15823 NA (this question is only asked if a704="yes",
# i.e. if the person has declared to need regular help in personal care.
# However for my sample, I have that 149 cases answered "yes" to a704, but up to 482 cases were asked a706. 
# In Waves 1 and 2 the numbers match, the problem seems to come from Wave 3. 
# Create new variable deleting the cases where a704="no"

GGSWP_France$a706[which(GGSWP_France$a704=="no")]="no"
GGSWP_France$rec_informalcare<-GGSWP_France$a706

#####################################

# ACTIVITY, OCCUPATION AND INCOME

# Activity status
GGSWP_France$aactstat[which(GGSWP_France$aactstat=="self-employed" | GGSWP_France$aactstat=="Employed" |
                              GGSWP_France$aactstat=="employed" | GGSWP_France$aactstat=="Self employed")]="employed or self-employed"
GGSWP_France$aactstat[which(GGSWP_France$aactstat=="Unemployed")]="unemployed"
GGSWP_France$aactstat[which(GGSWP_France$aactstat=="Retired")]="retired"
GGSWP_France$aactstat[which(GGSWP_France$aactstat=="ill or disabled for a long time" |
                              GGSWP_France$aactstat=="Ill or disabled for a long time or permanently")]="ill or disabled for a long time or permanently"
GGSWP_France$aactstat[which(GGSWP_France$aactstat=="Other status" | GGSWP_France$aactstat=="other status")]="other"
GGSWP_France$aactstat[which(GGSWP_France$aactstat=="student, in school, in vocational training" |
                              GGSWP_France$aactstat=="Student, in school, in vocational training")]="student,in school,vocational training"
GGSWP_France$aactstat[which(GGSWP_France$aactstat=="looking after the home or family" |
                              GGSWP_France$aactstat=="Homemaker")]="homemaker"
GGSWP_France$aactstat[which(GGSWP_France$aactstat=="Helping family member in a family business or farm" |
                              GGSWP_France$aactstat=="helping family member in family business")]="helping family member in a family business or farm"
GGSWP_France$aactstat[which(GGSWP_France$aactstat=="maternity leave" | GGSWP_France$aactstat=="Maternity leave" |
                              GGSWP_France$aactstat=="maternity,parental,childcare leave" | 
                              GGSWP_France$aactstat=="Parental leave, care leave")]="parental leave, care leave"
GGSWP_France$aactstat<-droplevels(GGSWP_France$aactstat)

# Some categories can be merged:
GGSWP_France$aactstat_grp<-recode_factor(GGSWP_France$aactstat,
                                         "employed or self-employed"="in paid employment","unemployed"="not in paid employment",
                                         "student,in school,vocational training"="not in paid employment",
                                         "in apprenticeship or paid training"="not in paid employment",
                                         "retired"="not in paid employment",
                                         "homemaker"="not in paid employment",
                                         "parental leave, care leave"="not in paid employment",
                                         "ill or disabled for a long time or permanently"="not in paid employment",
                                         "other"="not in paid employment",
                                         "helping family member in a family business or farm"="not in paid employment")

# Current occupation
GGSWP_France$a832<-recode_factor(GGSWP_France$a832,
"LEGISLATORS, SENIOR OFFICIALS, MANAGERS"="1100",
"LEGISLATORS AND SENIOR OFFICIALS"="1100",
"LEGISLATORS AND SENIOR OFFICIALS_duplicated_1100"="1100",
"CORPORATE MANAGERS"="1200",
"CORPORATE MANAGERS_duplicated_1200"="1200",
"GENERAL MANAGERS"="1300",
"GENERAL MANAGERS_duplicated_1300"="1300",
"PHYSICAL, MATHEMATICAL AND ENGINEERING SCIENCE PROFESSION"="2100",
"PHYSICAL, MATHEMATICAL AND ENGINEERING SCIENCE PROFESSION_duplicated_2100"="2100",
"LIFE SCIENCE AND HEALTH PROFESSIONALS"="2200",
"LIFE SCIENCE AND HEALTH PROFESSIONALS_duplicated_2200"="2200",
"TEACHING PROFESSIONALS"="2300",
"TEACHING PROFESSIONALS_duplicated_2300"="2300",
"OTHER PROFESSIONALS"="2400",
"OTHER PROFESSIONALS_duplicated_2400"="2400",
"PHYSICAL AND ENGINEERING SCIENCE ASSOCIATE PROFESSIONALS"="3100",
"PHYSICAL AND ENGINEERING SCIENCE ASSOCIATE PROFESSIONALS_duplicated_3100"="3100",
"LIFE SCIENCE AND HEALTH ASSOCIATE PROFESSIONALS"="3200",
"LIFE SCIENCE AND HEALTH ASSOCIATE PROFESSIONALS_duplicated_3200"="3200",
"TEACHING ASSOCIATE PROFESSIONALS"="3300",
"TEACHING ASSOCIATE PROFESSIONALS_duplicated_3300"="3300",
"OTHER ASSOCIATE PROFESSIONALS"="3400",
"OTHER ASSOCIATE PROFESSIONALS_duplicated_3400"="3400",
"OFFICE CLERKS"="4100",
"OFFICE CLERKS_duplicated_4100"="4100",
"CUSTOMER SERVICES CLERKS"="4200",
"CUSTOMER SERVICES CLERKS_duplicated_4200"="4200",
"PERSONAL AND PROTECTIVE SERVICES WORKERS"="5100",
"PERSONAL AND PROTECTIVE SERVICES WORKERS_duplicated_5100"="5100",
"MODELS, SALESPERSONS AND DEMONSTRATORS"="5200",
"MODELS, SALESPERSONS AND DEMONSTRATORS_duplicated_5200"="5200",
"MARKET-ORIENTED SKILLED AGRICULTURAL AND FISHERY WORKERS"="6100",
"MARKET-ORIENTED SKILLED AGRICULTURAL AND FISHERY WORKERS_duplicated_6100"="6100",
"EXTRACTION AND BUILDING TRADES WORKERS"="7100",
"EXTRACTION AND BUILDING TRADES WORKERS_duplicated_7100"="7100",
"METAL, MACHINERY AND RELATED TRADES WORKERS"="7200",
"METAL, MACHINERY AND RELATED TRADES WORKERS_duplicated_7200"="7200",
"PRECISION, HANDICRAFT, PRINTING AND RELATED TRADES WORKER"="7300",
"PRECISION, HANDICRAFT, PRINTING AND RELATED TRADES WORKER_duplicated_7300"="7300",
"OTHER CRAFT AND RELATED TRADES WORKERS"="7400",
"OTHER CRAFT AND RELATED TRADES WORKERS_duplicated_7400"="7400",
"STATIONARY-PLANT AND RELATED OPERATORS"="8100",
"STATIONARY-PLANT AND RELATED OPERATORS_duplicated_8100"="8100",
"MACHINE OPERATORS AND ASSEMBLERS"="8200",
"MACHINE OPERATORS AND ASSEMBLERS_duplicated_8200"="8200",
"DRIVERS AND MOBILE-PLANT OPERATORS"="8300",
"DRIVERS AND MOBILE-PLANT OPERATORS_duplicated_8300"="8300",
"SALES AND SERVICES ELEMENTARY OCCUPATIONS"="9100",
"SALES AND SERVICES ELEMENTARY OCCUPATIONS_duplicated_9100"="9100",
"LABOURERS IN MINING, CONSTRUCTION, MANUFACTURING AND TRAN"="9300",
"LABOURERS IN MINING, CONSTRUCTION, MANUFACTURING AND TRAN_duplicated_9300"="9300",
"0100"="0000",
"100"="0000",
"armed forces and unspecified_duplicated_100"="0100")

GGSWP_France$a832[which(GGSWP_France$a832=="does not know" | 
GGSWP_France$a832=="no response/not applicable" | 
GGSWP_France$a832=="not applicable/no response" |
GGSWP_France$a832=="does not know_duplicated_99999")]=NA  

GGSWP_France$a832<-droplevels(GGSWP_France$a832)


# Monthly HH income (approximate range)
GGSWP_France$a1009[which(GGSWP_France$a1009=="does not know" |
                           GGSWP_France$a1009=="refusal" |
                           GGSWP_France$a1009=="no response/not applicable" |
                           GGSWP_France$a1009=="does not know_duplicated_9999")]=NA
GGSWP_France$a1009<-recode_factor(GGSWP_France$a1009,
                                  "499 Eur or less"="499  Eur or less")
GGSWP_France$a1009<-droplevels(GGSWP_France$a1009)

GGSWP_France$hhincome<-recode_factor(GGSWP_France$a1009,
                                     "499  Eur or less"="<1500",
                                     "500 to 999 Eur"="<1500",
                                     "1,000 to 1,499 Eur"="<1500",
                                     "1,500 to 1,999 Eur"="1500-3000",
                                     "2,000 to 2,499 Eur"="1500-3000",
                                     "2,500 to 2,999 Eur"="1500-3000",
                                     "3,000 to 4,999 Eur"=">3000",
                                     "5,000 Eur or more"=">3000")

#####################################################

# PREFERENCES AND VALUES

# Opinion: Children should take responsibility for parental care if parents in need (used as measure of norms)
GGSWP_France$a1112_a<-recode_factor(GGSWP_France$a1112_a,
"Strongly disagree"="strongly disagree",
"Disagree"="disagree",
"Neither agree nor disagree"="neither agree nor disagree",
"Agree"="agree",
"Strongly agree"="strongly agree") # Ordering of values has been inverted with respect to original dataset
GGSWP_France$a1112_a[which(GGSWP_France$a1112_a=="does not know" |
GGSWP_France$a1112_a=="no response/not applicable")]=NA
GGSWP_France$a1112_a<-droplevels(GGSWP_France$a1112_a)
GGSWP_France$a1112_a<-ordered(GGSWP_France$a1112_a)

GGSWP_France$a1112_a_num<-as.numeric(GGSWP_France$a1112_a) 
# Likert scale from 1 ("strongly disagree") to 5 ("strongly agree")

# Opinion: Children should adjust working lives to the needs of their parents (used as measure of norms)
GGSWP_France$a1112_b<-recode_factor(GGSWP_France$a1112_b,
"Strongly disagree"="strongly disagree",
"Disagree"="disagree",
"Neither agree nor disagree"="neither agree nor disagree",
"Agree"="agree",
"Strongly agree"="strongly agree") # Ordering of values has been inverted with respect to original dataset
GGSWP_France$a1112_b[which(GGSWP_France$a1112_b=="does not know" |
GGSWP_France$a1112_b=="no response/not applicable")]=NA
GGSWP_France$a1112_b<-droplevels(GGSWP_France$a1112_b)
GGSWP_France$a1112_b<-ordered(GGSWP_France$a1112_b)

GGSWP_France$a1112_b_num<-as.numeric(GGSWP_France$a1112_b) 
# Likert scale from 1 ("strongly disagree") to 5 ("strongly agree")

# Opinion: When parents in need, daughters should take more caring responsibility (used as measure of norms)
GGSWP_France$a1112_c<-recode_factor(GGSWP_France$a1112_c,
"Strongly disagree"="strongly disagree",
"Disagree"="disagree",
"Neither agree nor disagree"="neither agree nor disagree",
"Agree"="agree",
"Strongly agree"="strongly agree") # Ordering of values has been inverted with respect to original dataset
GGSWP_France$a1112_c[which(GGSWP_France$a1112_c=="does not know" |
GGSWP_France$a1112_c=="no response/not applicable")]=NA
GGSWP_France$a1112_c<-droplevels(GGSWP_France$a1112_c)
GGSWP_France$a1112_c<-ordered(GGSWP_France$a1112_c)

GGSWP_France$a1112_c_num<-as.numeric(GGSWP_France$a1112_c) 
# Likert scale from 1 ("strongly disagree") to 5 ("strongly agree")

# Opinion: Children should provide financial help if parents financial difficulty (used as measure of norms)
GGSWP_France$a1112_d<-recode_factor(GGSWP_France$a1112_d,
"Strongly disagree"="strongly disagree",
"Disagree"="disagree",
"Neither agree nor disagree"="neither agree nor disagree",
"Agree"="agree",
"Strongly agree"="strongly agree") # Ordering of values has been inverted with respect to original dataset
GGSWP_France$a1112_d[which(GGSWP_France$a1112_d=="does not know" |
GGSWP_France$a1112_d=="no response/not applicable")]=NA
GGSWP_France$a1112_d<-droplevels(GGSWP_France$a1112_d)
GGSWP_France$a1112_d<-ordered(GGSWP_France$a1112_d)

GGSWP_France$a1112_d_num<-as.numeric(GGSWP_France$a1112_d) 
# Likert scale from 1 ("strongly disagree") to 5 ("strongly agree")

# Opinion: Children should live with parents when no longer look after themselves (used as measure of norms)
GGSWP_France$a1112_e<-recode_factor(GGSWP_France$a1112_e,
"Strongly disagree"="strongly disagree",
"Disagree"="disagree",
"Neither agree nor disagree"="neither agree nor disagree",
"Agree"="agree",
"Strongly agree"="strongly agree") # Ordering of values has been inverted with respect to original dataset
GGSWP_France$a1112_e[which(GGSWP_France$a1112_e=="does not know" |
GGSWP_France$a1112_e=="no response/not applicable")]=NA
GGSWP_France$a1112_e<-droplevels(GGSWP_France$a1112_e)
GGSWP_France$a1112_e<-ordered(GGSWP_France$a1112_e)

GGSWP_France$a1112_e_num<-as.numeric(GGSWP_France$a1112_e) 
# Likert scale from 1 ("strongly disagree") to 5 ("strongly agree")

cronbach.alpha(data.frame(GGSWP_France$a1112_a,
GGSWP_France$a1112_b,
GGSWP_France$a1112_c,
GGSWP_France$a1112_d,
GGSWP_France$a1112_e),na.rm=T) # Alpha=0.664, maybe removing some items it can be used as an index

cronbach.alpha(data.frame(GGSWP_France$a1112_a,
GGSWP_France$a1112_b,
GGSWP_France$a1112_d,
GGSWP_France$a1112_e),na.rm=T) # Alpha=0.69 seems to be the best combination, but still not completely reliable.

GGSWP_France$valuesindex<-numeric(nrow(GGSWP_France))
for (i in 1:nrow(GGSWP_France)){
GGSWP_France$valuesindex[i]=mean(c(GGSWP_France$a1112_a_num[i],
GGSWP_France$a1112_b_num[i],
GGSWP_France$a1112_d_num[i],
GGSWP_France$a1112_e_num[i]),na.rm=T)}

###############

# Population weight
GGSWP_France$aweight

##############

# ELIMINATE CASES THAT HAVE CHANGED GENDER (SO THAT GENDER CAN REMAIN A CONSTANT IN THE MODELS)

idgndchange<-GGSWP_France$arid[which(GGSWP_France$asex[which(GGSWP_France$Wave==1)]!=GGSWP_France$asex[which(GGSWP_France$Wave==3)])]
GGSWP_France<-GGSWP_France[-which(GGSWP_France$arid %in% idgndchange),]

###############################################

# CREATE ALSO DATA IN WIDE FORMAT:

GGSW1_France<-GGSWP_France[which(GGSWP_France$Wave==1),]
colnames(GGSW1_France)<-paste(colnames(GGSW1_France),"w1",sep="_")
GGSW2_France<-GGSWP_France[which(GGSWP_France$Wave==2),]
colnames(GGSW2_France)<-paste(colnames(GGSW2_France),"w2",sep="_")
GGSW3_France<-GGSWP_France[which(GGSWP_France$Wave==3),]
colnames(GGSW3_France)<-paste(colnames(GGSW3_France),"w3",sep="_")

# Data in wide format (GGSWP_France is the long format)
GGSWW_France<-cbind(GGSW1_France,GGSW2_France,GGSW3_France)

###############################################

save(list=ls(),file="GGSWP_France_session_1.RData")

###############################################




