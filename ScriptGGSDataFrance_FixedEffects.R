#####################################################################################################
## STUDY 2: Family Elderly Caregiving and Preferences for Family or State Responsibility in France ##
#####################################################################################################

####
## SCRIPT 4: FIXED-EFFECTS MODELS
####

setwd("D:/BIGSSS/Dissertation/Study 2/Data/GGS Data/GGSAllWavesAllCountries")

load("GGSWP_France_session_3.RData")

library(plm)
library(lme4)
library(datawizard)
library(texreg)

########################################################

# FIXED-EFFECTS MODELS

######
# 1. MODELS FOR PREFERENCES
######

# See "Mediation" script. Results are identical to those obtained with PLM 

########################################################

##
# 1.2 FIXED EFFECTS WITH PLM
##

## CARESBIN:

# 0.1) Just the Treatment 
mpr_caresbin_0.1<-plm(a1110_a_num~caresbin,data=Sample_CB,index="arid",weights=weightsIPW_caresbin)
summary(mpr_caresbin_0.1) 

# 0.2 Treatment and Mediator
mpr_caresbin_0.2<-plm(a1110_a_num~caresbin+wellbeing_index,data=Sample_CB,index="arid",weights=weightsIPW_caresbin)
summary(mpr_caresbin_0.2)


# 1) Treatment, Mediator and Controls
mpr_caresbin_1<-plm(a1110_a_num~caresbin+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                      health_num,
                    data=Sample_CB,index="arid",weights=weightsOVL_caresbin)
summary(mpr_caresbin_1) # Providing care significantly increases support for family responsibility


# 2) Separate samples for each gender
mpr_caresbin_1_w<-plm(a1110_a_num~caresbin+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                        health_num,
                      data=subset(Sample_CB,asex=="female"),index="arid",weights=weightsOVL_caresbin_f)
summary(mpr_caresbin_1_w) # There is no effect of providing care on the preferences of women

mpr_caresbin_1_m<-plm(a1110_a_num~caresbin+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                        health_num,
                      data=subset(Sample_CB,asex=="male"),index="arid",weights=weightsOVL_caresbin_m)
summary(mpr_caresbin_1_m) # Men who provide care become significantly more supportive of family responsibility

##########################

## CARESPARENT:

# 0.1) Just the Treatment 
mpr_caresparent_0.1<-plm(a1110_a_num~caresparent,data=Sample_CParent,index="arid",weights=weightsIPW_caresparent)
summary(mpr_caresparent_0.1)

# 0.2 Treatment and Mediator
mpr_caresparent_0.2<-plm(a1110_a_num~caresparent+wellbeing_index,data=Sample_CParent,index="arid",weights=weightsIPW_caresparent)
summary(mpr_caresparent_0.2)


# 1) Treatment, Mediator and Controls
mpr_caresparent_1<-plm(a1110_a_num~caresparent+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                         health_num,
                      data=Sample_CParent,index="arid",weights=weightsOVL_caresparent)
summary(mpr_caresparent_1) # Providing care to a parent significantly increases support for family responsibility


# 2) Separate samples for each gender
mpr_caresparent_1_w<-plm(a1110_a_num~caresparent+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                           health_num,
                         data=subset(Sample_CParent,asex=="female"),index="arid",weights=weightsOVL_caresparent_f)
summary(mpr_caresparent_1_w) # There is no effect of providing care to parent on the preferences of women

mpr_caresparent_1_m<-plm(a1110_a_num~caresparent+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                           health_num,
                         data=subset(Sample_CParent,asex=="male"),index="arid",weights=weightsOVL_caresparent_m)
summary(mpr_caresparent_1_m) # Men who provide care to a parent become significantly more supportive of family responsibility

##########################

## CARESPARTNER:

# 0.1) Just the Treatment 
mpr_carespartner_0.1<-plm(a1110_a_num~carespartner,data=Sample_CPartner,index="arid",weights=weightsIPW_carespartner)
summary(mpr_carespartner_0.1)

# 0.2 Treatment and Mediator
mpr_carespartner_0.2<-plm(a1110_a_num~carespartner+wellbeing_index,data=Sample_CPartner,index="arid",weights=weightsIPW_carespartner)
summary(mpr_carespartner_0.2)


# 1) Treatment, Mediator and Controls
mpr_carespartner_1<-plm(a1110_a_num~carespartner+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                          health_num,
                      data=Sample_CPartner,index="arid",weights=weightsOVL_carespartner)
summary(mpr_carespartner_1) # Providing care to a partner significantly increases support for family responsibility


# 2) Separate samples for each gender
mpr_carespartner_1_w<-plm(a1110_a_num~carespartner+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                            health_num,
                          data=subset(Sample_CPartner,asex=="female"),index="arid",weights=weightsOVL_carespartner_f)
summary(mpr_carespartner_1_w) # There is no effect of providing care to parter on the preferences of women

mpr_carespartner_1_m<-plm(a1110_a_num~carespartner+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                            health_num,
                          data=subset(Sample_CPartner,asex=="male"),index="arid",weights=weightsOVL_carespartner_m)
summary(mpr_carespartner_1_m) # Men who provide care to a partner become significantly more supportive of family responsibility

wordreg(list(mpr_caresparent_1_w,mpr_caresparent_1_m,
             mpr_carespartner_1_w,mpr_carespartner_1_m),
        custom.model.names=c("M1.1 (Female)","M1.2 (Male)",
                             "M2.1 (Female)","M2.2 (Male)"),
        custom.coef.names=c("Cares for parent","Well-being","Age","Activity: not in paid employment","Partnership: non-resident",
                            "Partnership: co-resident partner","Lives with parents",
                            "Marital: married","Has kids: no","HH Income: 1500-3000","HH Income: >3000",
                            "Health","Cares for partner"),
        stars = c(0.001, 0.01, 0.05, 0.1),reorder.coef=c(1,13,2:12),
        file="FixedEffectsPreferences.doc")

################################################################################################################
################################################################################################################

##
# 2. MODELS FOR WELL-BEING
##

##
# 2.1 FIXED EFFECTS WITH LM
##

# See "Mediation" script. Results are identical to those obtained with PLM 

##
# 2.2 FIXED EFFECTS WITH PML
##

## CARESBIN:

# 0.1) Just the Treatment 
mwb_caresbin_0.1<-plm(wellbeing_index~caresbin,data=Sample_CB,index="arid",weights=weightsOVL_caresbin_wb)
summary(mwb_caresbin_0.1) 

# 1) Treatment, Mediator and Controls
mwb_caresbin_1<-plm(wellbeing_index~caresbin+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                      health_num,
                    data=Sample_CB,index="arid",weights=weightsOVL_caresbin_wb)
summary(mwb_caresbin_1)  # Providing care reduces well-being

# 2) Separate samples for each gender
mwb_caresbin_1_w<-plm(wellbeing_index~caresbin+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                        health_num,
                      data=subset(Sample_CB,asex=="female"),index="arid",weights=weightsOVL_caresbin_wb_f)
summary(mwb_caresbin_1_w) # Providing care reduces well-being for women

mwb_caresbin_1_m<-plm(wellbeing_index~caresbin+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                        health_num,
                      data=subset(Sample_CB,asex=="male"),index="arid",weights=weightsOVL_caresbin_wb_m)
summary(mwb_caresbin_1_m) # Providing care reduces well-being for men

##########################

## CARESPARENT:

# 0.1) Just the Treatment 
mwb_caresparent_0.1<-plm(wellbeing_index~caresparent,data=Sample_CParent,index="arid",weights=weightsOVL_caresparent_wb)
summary(mwb_caresparent_0.1) 

# 1) Treatment, Mediator and Controls
mwb_caresparent_1<-plm(wellbeing_index~caresparent+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                         health_num,
                       data=Sample_CParent,index="arid",weights=weightsOVL_caresparent_wb)
summary(mwb_caresparent_1) # Providing care to a parent reduces well-being


# 2) Separate samples for each gender
mwb_caresparent_1_w<-plm(wellbeing_index~caresparent+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                           health_num,
                         data=subset(Sample_CParent,asex=="female"),index="arid",weights=weightsOVL_caresparent_wb_f)
summary(mwb_caresparent_1_w) # Providing care to a parent reduces well-being for women

mwb_caresparent_1_m<-plm(wellbeing_index~caresparent+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                           health_num,
                         data=subset(Sample_CParent,asex=="male"),index="arid",weights=weightsOVL_caresparent_wb_m)
summary(mwb_caresparent_1_m) # Providing care to a parent reduces well-being for men

##########################

## CARESPARTNER:

# 0.1) Just the Treatment 
mwb_carespartner_0.1<-plm(wellbeing_index~carespartner,data=Sample_CPartner,index="arid",weights=weightsOVL_carespartner_wb)
summary(mwb_carespartner_0.1) 

# 1) Treatment, Mediator and Controls
mwb_carespartner_1<-plm(wellbeing_index~carespartner+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                          health_num,
                        data=Sample_CPartner,index="arid",weights=weightsOVL_carespartner_wb)
summary(mwb_carespartner_1) # Providing care to a partner reduces well-being

# 2) Separate samples for each gender
mwb_carespartner_1_w<-plm(wellbeing_index~carespartner+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                            health_num,
                          data=subset(Sample_CPartner,asex=="female"),index="arid",weights=weightsOVL_carespartner_wb_f)
summary(mwb_carespartner_1_w) # Providing care to a partner reduces well-being for women

mwb_carespartner_1_m<-plm(wellbeing_index~carespartner+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                            health_num,
                          data=subset(Sample_CPartner,asex=="male"),index="arid",weights=weightsOVL_carespartner_m)
summary(mwb_carespartner_1_m) # Providing care to a partner reduces well-being for men

wordreg(list(mwb_caresparent_1_w,mwb_caresparent_1_m,
             mwb_carespartner_1_w,mwb_carespartner_1_m),
        custom.model.names=c("M1.1 (Female)","M1.2 (Male)",
                             "M2.1 (Female)","M2.2 (Male)"),
        custom.coef.names=c("Cares for parent","Age","Activity: not in paid employment","Partnership: non-resident",
                            "Partnership: co-resident partner","Lives with parents",
                            "Marital: married","Has kids: no","HH Income: 1500-3000","HH Income: >3000",
                            "Health","Cares for partner"),
        stars = c(0.001, 0.01, 0.05, 0.1),reorder.coef=c(1,12,2:11),
        file="FixedEffectsWB.doc")

#################################################################################################
#################################################################################################

