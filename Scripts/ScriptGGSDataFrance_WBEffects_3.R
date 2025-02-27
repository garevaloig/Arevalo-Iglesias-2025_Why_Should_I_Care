#####################################################################################################
## STUDY 2: Family Elderly Caregiving and Preferences for Family or State Responsibility in France ##
#####################################################################################################

####
## SCRIPT 5: WITHIN-BETWEEN MODELS
####

library(panelr)
library(mediation)
library(texreg)
library(car)

setwd("D:/BIGSSS/Dissertation/Study 2/Data/GGS Data/GGSAllWavesAllCountries")

load("GGSWP_France_session_3.RData")

GGSWP_France_Panel<-panel_data(GGSWP_France,id=arid,wave=Wave)

########################################################

# WITHIN-BETWEEN MODELS

##
# 1. MODELS FOR PREFERENCES
##

## 1.1 MODELS TO PREDICT PREFERENCES USING THE CURRENT STATE OF PROVIDING CARE

## CARESBIN:

Sample_CB_P<-panel_data(Sample_CB,id=arid,wave=Wave)

# 0.1) Just the Treatment 
wbm_pr_caresbin_0.1<-wbm(a1110_a_num~caresbin,
                            data=Sample_CB_P,model="w-b",weights=weightsOVL_caresbin)
summary(wbm_pr_caresbin_0.1) 

# 0.2 Treatment and Mediator
wbm_pr_caresbin_0.2<-wbm(a1110_a_num~caresbin+wellbeing_index,
                            data=Sample_CB_P,model="w-b",weights=weightsOVL_caresbin)
summary(wbm_pr_caresbin_0.2) 


# 1) Treatment, Mediator and Controls
wbm_pr_caresbin_1<-wbm(a1110_a_num~caresbin+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                         health_num|asex+aeduc_bin,
                          data=Sample_CB_P,model="w-b",weights=weightsOVL_caresbin)
summary(wbm_pr_caresbin_1)


# 2) Adding interaction with gender
wbm_pr_caresbin_2<-wbm(a1110_a_num~caresbin+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                         health_num|asex+aeduc_bin|caresbin*asex,
                          data=Sample_CB_P,model="w-b",weights=weightsOVL_caresbin)
summary(wbm_pr_caresbin_2) # Providing care increases support for family responsibility, but only for men (coherent with fixed-effects models)
summary(wbm_pr_caresbin_2)$within_table

# 3) Adding interaction with gender and well-being
wbm_pr_caresbin_3<-wbm(a1110_a_num~caresbin+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                         health_num|asex+aeduc_bin|caresbin*asex+wellbeing_index*asex,
                       data=Sample_CB_P,model="w-b",weights=weightsOVL_caresbin)
summary(wbm_pr_caresbin_3) # The effect of wb on preferences does not change across genders

##########################

## CARESPARENT:

Sample_CParent_P<-panel_data(Sample_CParent,id=arid,wave=Wave)

# 0.1) Just the Treatment 
wbm_pr_caresparent_0.1<-wbm(a1110_a_num~caresparent,
                         data=Sample_CParent_P,model="w-b",weights=weightsOVL_caresparent)
summary(wbm_pr_caresparent_0.1) 

# 0.2 Treatment and Mediator
wbm_pr_caresparent_0.2<-wbm(a1110_a_num~caresparent+wellbeing_index
                            data=Sample_CParent_P,model="w-b",weights=weightsOVL_caresparent)
summary(wbm_pr_caresparent_0.2) 


# 1) Treatment, Mediator and Controls 
wbm_pr_caresparent_1<-wbm(a1110_a_num~caresparent+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                            health_num|asex+aeduc_bin,
                          data=Sample_CParent_P,model="w-b",weights=weightsOVL_caresparent)
summary(wbm_pr_caresparent_1)


# 2) Adding interaction with gender
wbm_pr_caresparent_2<-wbm(a1110_a_num~caresparent+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                            health_num|asex+aeduc_bin|caresparent*asex,
                          data=Sample_CParent_P,model="w-b",weights=weightsOVL_caresparent)
summary(wbm_pr_caresparent_2) # Providing care to a parent increases support for family responsibility, 
                              # but only for men (coherent with fixed-effects models)
summary(wbm_pr_caresparent_2)$within_table

# 3) Adding interaction with gender and well-being
wbm_pr_caresparent_3<-wbm(a1110_a_num~caresparent+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                         health_num|asex+aeduc_bin|caresparent*asex+wellbeing_index*asex,
                       data=Sample_CParent_P,model="w-b",weights=weightsOVL_caresparent)
summary(wbm_pr_caresparent_3) # Higher well-being makes men more familialistic, but not women

##########################

## CARESPARTNER:

Sample_CPartner_P<-panel_data(Sample_CPartner,id=arid,wave=Wave)

# 0.1) Just the Treatment 
wbm_pr_carespartner_0.1<-wbm(a1110_a_num~carespartner,
                            data=Sample_CPartner_P,model="w-b",weights=weightsIPW_carespartner)
summary(wbm_pr_carespartner_0.1) 

# 0.2 Treatment and Mediator
wbm_pr_carespartner_0.2<-wbm(a1110_a_num~carespartner+wellbeing_index,
                            data=Sample_CPartner_P,model="w-b",weights=weightsIPW_carespartner)
summary(wbm_pr_carespartner_0.2) 


# 1) Treatment, Mediator and Controls 
wbm_pr_carespartner_1<-wbm(a1110_a_num~carespartner+wellbeing_index+aage+aactstat_grp+amarstat_bin+haskids+hhincome+
                            health_num|asex+aeduc_bin,
                          data=Sample_CPartner_P,model="w-b",weights=weightsIPW_carespartner)
summary(wbm_pr_carespartner_1)


# 2) Adding interaction with gender
wbm_pr_carespartner_2<-wbm(a1110_a_num~carespartner+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                             health_num|asex+aeduc_bin|carespartner*asex,
                          data=Sample_CPartner_P,model="w-b",weights=weightsOVL_carespartner)
summary(wbm_pr_carespartner_2) 
summary(wbm_pr_carespartner_2)$within_table

wordreg(list(wbm_pr_caresbin_2,wbm_pr_caresparent_2,wbm_pr_carespartner_2),
        custom.model.names=c("M1","M2","M3"),
        custom.coef.names=c("Intercept","(Mean) Well-being","(Mean) Age","(Mean) Health","(Mean) Cares","(Mean) Not in paid employment",
         "(Mean) No partner","(Mean) Non-resident partner","(Mean) Lives with parents","(Mean) Marital Status: Married","(Mean) No kids",
                           "(Mean) Edu: ISCED 3","(Mean) Edu: ISCED 5-6","(Mean) HH Income 1500-3000","(Mean) HH Income >3000",
                           "Well-being","Age","Health","Cares","Not in paid employment","No partner","Non-resident partner",
                           "Lives with parents","Marital status: Married","No kids","Edu: ISCED 3","Edu: ISCED 5-6","HH Income 1500-3000","HH Income >3000",
                           "Gender: Female","Cares:Female","(Mean) Cares for parent","Cares for parent","Cares for parent:Female",
                           "(Mean) Cares for partner","Cares for partner",
                           "Cares for partner:Female"),
reorder.coef=c(19,33,36,16,17,18,20:29,1,5,32,35,2,3,4,6:15,30,31,34,37),
groups=list("Within-Effects"=1:16,"Between-Effects"=17:34,"Interactions"=35:37),
        stars = c(0.001, 0.01, 0.05, 0.1),
        file="WithinBetweenPreferences.doc")

# 3) Adding interaction with gender and well-being
wbm_pr_carespartner_3<-wbm(a1110_a_num~carespartner+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                            health_num|asex+aeduc_bin|carespartner*asex+wellbeing_index*asex,
                          data=Sample_CPartner_P,model="w-b",weights=weightsOVL_carespartner)
summary(wbm_pr_carespartner_3) # Higher well-being makes men less familialistic, but women more familialistic


##########################

## CARESGRP:

Sample_CGroup_P<-panel_data(Sample_CGroup,id=arid,wave=Wave)

# 0.1) Just the Treatment 
wbm_pr_caresgrp_0.1<-wbm(a1110_a_num~cares_grp,
                             data=Sample_CGroup_P,model="w-b",weights=weightsOVL_caresgrp)
summary(wbm_pr_caresgrp_0.1) 

# 0.2 Treatment and Mediator
wbm_pr_caresgrp_0.2<-wbm(a1110_a_num~cares_grp+wellbeing_index,
                         data=Sample_CGroup_P,model="w-b",weights=weightsOVL_caresgrp)
summary(wbm_pr_caresgrp_0.2) 


# 1) Treatment, Mediator and Controls 
wbm_pr_caresgrp_1<-wbm(a1110_a_num~cares_grp+wellbeing_index+aage+aactstat_grp+amarstat_bin+haskids+hhincome+
                         health_num|asex+aeduc_bin,
                       data=Sample_CGroup_P,model="w-b",weights=weightsOVL_caresgrp)
summary(wbm_pr_caresgrp_1)


# 2) Adding interaction with gender
wbm_pr_caresgrp_2<-wbm(a1110_a_num~cares_grp+wellbeing_index+aage+aactstat_grp+aparstat+amarstat_bin+haskids+hhincome+
                         health_num|asex+aeduc_bin|cares_grp*asex,
                       data=Sample_CGroup_P,model="w-b",weights=weightsOVL_caresgrp)
summary(wbm_pr_caresgrp_2) 

####################################################

##
# 2. MODELS FOR WELL-BEING
##

## 2.1 MODELS TO PREDICT WELL-BEING USING THE CURRENT STATE OF PROVIDING CARE (THE EXPERIENCE OF PROVIDING CARE WON'T BE USED HERE)

## CARESBIN:

# 0.1) Just the Treatment 
wbm_wb_caresbin_0.1<-wbm(wellbeing_index~caresbin,
                         data=Sample_CB_P,model="w-b",weights=weightsOVL_caresbin_wb)
summary(wbm_wb_caresbin_0.1) 

# 1) Treatment and Controls 
wbm_wb_caresbin_1<-wbm(wellbeing_index~caresbin+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                         health_num|asex+aeduc_bin,
                       data=Sample_CB_P,model="w-b",weights=weightsOVL_caresbin_wb)
summary(wbm_wb_caresbin_1)


# 2) Adding interaction with gender
wbm_wb_caresbin_2<-wbm(wellbeing_index~caresbin+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                         health_num|asex+aeduc_bin|caresbin*asex,
                       data=Sample_CB_P,model="w-b",weights=weightsOVL_caresbin_wb)
summary(wbm_wb_caresbin_2) 
summary(wbm_wb_caresbin_2)$within_table

##########################

## CARESPARENT:

# 0.1) Just the Treatment 
wbm_wb_caresparent_0.1<-wbm(wellbeing_index~caresparent,
                            data=Sample_CParent_P,model="w-b",weights=weightsOVL_caresparent_wb)
summary(wbm_wb_caresparent_0.1) 

# 1) Treatment and Controls 
wbm_wb_caresparent_1<-wbm(wellbeing_index~caresparent+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                          health_num|asex+aeduc_bin,
                          data=Sample_CParent_P,model="w-b",weights=weightsOVL_caresparent_wb)
summary(wbm_wb_caresparent_1)


# 2) Adding interaction with gender
wbm_wb_caresparent_2<-wbm(wellbeing_index~caresparent+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                            health_num|asex+aeduc_bin|caresparent*asex,
                          data=Sample_CParent_P,model="w-b",weights=weightsOVL_caresparent_wb)
summary(wbm_wb_caresparent_2) 
summary(wbm_wb_caresparent_2)$within_table

##########################

## CARESPARTNER:

# 0.1) Just the Treatment 
wbm_wb_carespartner_0.1<-wbm(wellbeing_index~carespartner,
                             data=Sample_CPartner_P,model="w-b",weights=weightsOVL_carespartner_wb)
summary(wbm_wb_carespartner_0.1) 

# 1) Treatment and Controls

wbm_wb_carespartner_1<-wbm(wellbeing_index~carespartner+aage+aactstat_grp+amarstat_bin+haskids+hhincome+
                             health_num|asex+aeduc_bin,
                           data=Sample_CPartner_P,model="w-b",weights=weightsOVL_carespartner_wb)
summary(wbm_wb_carespartner_1)


# 2) Adding interaction with gender

wbm_wb_carespartner_2<-wbm(wellbeing_index~carespartner+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                             health_num|asex+aeduc_bin|carespartner*asex,
                           data=Sample_CPartner_P,model="w-b",weights=weightsOVL_carespartner_wb)
summary(wbm_wb_carespartner_2) 
summary(wbm_wb_carespartner_2)$within_table

wordreg(list(wbm_wb_caresbin_2,wbm_wb_caresparent_2,wbm_wb_carespartner_2),
        custom.model.names=c("M1","M2","M3"),
        #custom.coef.names=c("Intercept","Age (between)","),
        #reorder.coef=c(17,31,34,15,16,18:27,1,4,30,33,2,3,5:14,28,29,32,35),
        #groups=list("Within-Effects"=1:15,"Between-Effects"=16:32,"Interactions"=33:35),
        stars = c(0.001, 0.01, 0.05, 0.1),
        file="WithinBetweenWellBeing.doc")


## CARESGRP:

# 0.1) Just the Treatment 
wbm_wb_caresgrp_0.1<-wbm(wellbeing_index~cares_grp,
                         data=Sample_CGroup_P,model="w-b",weights=weightsOVL_cares_grp_wb)
summary(wbm_wb_caresgrp_0.1) 

# 1) Treatment and Controls 
wbm_wb_caresgrp_1<-wbm(wellbeing_index~cares_grp+aage+aactstat_grp+amarstat_bin+haskids+hhincome+
                         health_num|asex+aeduc_bin,
                       data=Sample_CGroup_P,model="w-b",weights=weightsOVL_cares_grp_wb)
summary(wbm_wb_caresgrp_1) 


# 2) Adding interaction with gender
wbm_wb_caresgrp_2<-wbm(wellbeing_index~cares_grp+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                         health_num|asex+aeduc_bin|cares_grp*asex,,
                       data=Sample_CGroup_P,model="w-b",weights=weightsOVL_cares_grp_wb)
summary(wbm_wb_caresgrp_2) 

