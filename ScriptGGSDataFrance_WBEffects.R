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

######################################################################
#########################################################################################
##################################################################

# ROBUSTNESS: SEPARATE MODELS FOR TRANSITIONS INTO AND OUT OF CARE

###
# APPROACH 1: Test models using the experience of having cared (no transition backwards)
###


# WITHIN-BETWEEN MODELS

##
# 1. MODELS FOR PREFERENCES
##

## CARESBIN:

Sample_CB_P<-panel_data(Sample_CB,id=arid,wave=Wave)

wbm_pr_caresbin_exp_2<-wbm(a1110_a_num~caresbin_exp+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                             health_num|asex+aeduc_bin|caresbin_exp*asex,
                           data=Sample_CB_P,model="w-b",weights=weightsOVL_caresbin)
summary(wbm_pr_caresbin_exp_2) # Providing care increases support for family responsibility, but only for men (coherent with fixed-effects models)
summary(wbm_pr_caresbin_exp_2)$within_table

## CARESPARENT:

Sample_CParent_P<-panel_data(Sample_CParent,id=arid,wave=Wave)

wbm_pr_caresparent_exp_2<-wbm(a1110_a_num~caresparent_exp+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                                health_num|asex+aeduc_bin|caresparent_exp*asex,
                              data=Sample_CParent_P,model="w-b",weights=weightsOVL_caresparent)
summary(wbm_pr_caresparent_exp_2) 
summary(wbm_pr_caresparent_exp_2)$within_table

## CARESPARTNER:

Sample_CPartner_P<-panel_data(Sample_CPartner,id=arid,wave=Wave)

wbm_pr_carespartner_exp_2<-wbm(a1110_a_num~carespartner_exp+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                                 health_num|asex+aeduc_bin|carespartner_exp*asex,
                               data=Sample_CPartner_P,model="w-b",weights=weightsOVL_carespartner)
summary(wbm_pr_carespartner_exp_2) 
summary(wbm_pr_carespartner_exp_2)$within_table

####################################################

##
# 2. MODELS FOR WELL-BEING
##

## CARESBIN:

wbm_wb_caresbin_exp_2<-wbm(wellbeing_index~caresbin_exp+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                             health_num|asex+aeduc_bin|caresbin_exp*asex,
                           data=Sample_CB_P,model="w-b",weights=weightsOVL_caresbin_wb)
summary(wbm_wb_caresbin_exp_2) 
summary(wbm_wb_caresbin_exp_2)$within_table

## CARESPARENT:

wbm_wb_caresparent_exp_2<-wbm(wellbeing_index~caresparent_exp+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                                health_num|asex+aeduc_bin|caresparent_exp*asex,
                              data=Sample_CParent_P,model="w-b",weights=weightsOVL_caresparent_wb)
summary(wbm_wb_caresparent_exp_2) 
summary(wbm_wb_caresparent_exp_2)$within_table

## CARESPARTNER:

wbm_wb_carespartner_exp_2<-wbm(wellbeing_index~carespartner_exp+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                                 health_num|asex+aeduc_bin|carespartner_exp*asex,
                               data=Sample_CPartner_P,model="w-b",weights=weightsOVL_carespartner_wb)
summary(wbm_wb_carespartner_exp_2) 
summary(wbm_wb_carespartner_exp_2)$within_table

##################################################################

###
# Approach 2) Use only cases with transitions in one direction
###

# Caresbin:
caresbin_mat<-as.data.frame(cbind(Sample_CB_P$arid[which(Sample_CB_P$Wave==1)],
                                  paste(Sample_CB_P$caresbin[which(Sample_CB_P$Wave==1)]),
                                  paste(Sample_CB_P$caresbin[which(Sample_CB_P$Wave==2)]),
                                  paste(Sample_CB_P$caresbin[which(Sample_CB_P$Wave==3)])))
colnames(caresbin_mat)=c("arid","caresbin_1","caresbin_2","caresbin_3")

caresbin_mat$outofcare<-numeric(nrow(caresbin_mat))
caresbin_mat$intocare<-numeric(nrow(caresbin_mat))

caresbin_mat$outofcare[which(caresbin_mat$caresbin_1=="yes" &
                               caresbin_mat$caresbin_2=="no")]=1
caresbin_mat$outofcare[which(caresbin_mat$caresbin_2=="yes" &
                               caresbin_mat$caresbin_3=="no")]=1

caresbin_mat$intocare[which(caresbin_mat$caresbin_1=="no" &
                              caresbin_mat$caresbin_2=="yes")]=1
caresbin_mat$intocare[which(caresbin_mat$caresbin_2=="no" &
                              caresbin_mat$caresbin_3=="yes")]=1

Sample_CB_P_onlyinto<-Sample_CB_P[-which(Sample_CB_P$arid%in%
                                           caresbin_mat$arid[which(caresbin_mat$outofcare==1)]),]
Sample_CB_P_onlyoutof<-Sample_CB_P[-which(Sample_CB_P$arid%in%
                                            caresbin_mat$arid[which(caresbin_mat$intocare==1)]),]

# Transitions into care on preferences
wbm_pr_caresbin_into_2<-wbm(a1110_a_num~caresbin_exp+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                              health_num|asex+aeduc_bin|caresbin_exp*asex,
                            data=Sample_CB_P_onlyinto,model="w-b",weights=weightsOVL_caresbin)
summary(wbm_pr_caresbin_into_2)

# Transitions out of care on preferences
wbm_pr_caresbin_outof_2<-wbm(a1110_a_num~caresbin_exp+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                               health_num|asex+aeduc_bin|caresbin_exp*asex,
                             data=Sample_CB_P_onlyoutof,model="w-b",weights=weightsOVL_caresbin)
summary(wbm_pr_caresbin_outof_2)

# Transitions into care on well-being
wbm_wb_caresbin_into_2<-wbm(wellbeing_index~caresbin_exp+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                              health_num|asex+aeduc_bin|caresbin_exp*asex,
                            data=Sample_CB_P_onlyinto,model="w-b",weights=weightsOVL_caresbin_wb)
summary(wbm_wb_caresbin_into_2) 

# Transitions out of care on well-being
wbm_wb_caresbin_outof_2<-wbm(wellbeing_index~caresbin_exp+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                               health_num|asex+aeduc_bin|caresbin_exp*asex,
                             data=Sample_CB_P_onlyoutof,model="w-b",weights=weightsOVL_caresbin_wb)
summary(wbm_wb_caresbin_outof_2) 

###
# Cares parent
###

caresparent_mat<-as.data.frame(cbind(Sample_CParent_P$arid[which(Sample_CB_P$Wave==1)],
                                     paste(Sample_CParent_P$caresparent[which(Sample_CB_P$Wave==1)]),
                                     paste(Sample_CParent_P$caresparent[which(Sample_CB_P$Wave==2)]),
                                     paste(Sample_CParent_P$caresparent[which(Sample_CB_P$Wave==3)])))
colnames(caresparent_mat)=c("arid","caresparent_1","caresparent_2","caresparent_3")

caresparent_mat$outofcare<-numeric(nrow(caresparent_mat))
caresparent_mat$intocare<-numeric(nrow(caresparent_mat))

caresparent_mat$outofcare[which(caresparent_mat$caresparent_1=="yes" &
                                  caresparent_mat$caresparent_2=="no")]=1
caresparent_mat$outofcare[which(caresparent_mat$caresparent_2=="yes" &
                                  caresparent_mat$caresparent_3=="no")]=1

caresparent_mat$intocare[which(caresparent_mat$caresparent_1=="no" &
                                 caresparent_mat$caresparent_2=="yes")]=1
caresparent_mat$intocare[which(caresparent_mat$caresparent_2=="no" &
                                 caresparent_mat$caresparent_3=="yes")]=1

Sample_CParent_P_onlyinto<-Sample_CParent_P[-which(Sample_CParent_P$arid%in%
                                                     caresparent_mat$arid[which(caresparent_mat$outofcare==1)]),]
Sample_CParent_P_onlyoutof<-Sample_CParent_P[-which(Sample_CParent_P$arid%in%
                                                      caresparent_mat$arid[which(caresparent_mat$intocare==1)]),]

# Transitions into care on preferences
wbm_pr_caresparent_into_2<-wbm(a1110_a_num~caresparent_exp+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                                 health_num|asex+aeduc_bin|caresparent_exp*asex,
                               data=Sample_CParent_P_onlyinto,model="w-b",weights=weightsOVL_caresparent)
summary(wbm_pr_caresparent_into_2)

# Transitions out of care on preferences
wbm_pr_caresparent_outof_2<-wbm(a1110_a_num~caresparent_exp+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                                  health_num|asex+aeduc_bin|caresparent_exp*asex,
                                data=Sample_CParent_P_onlyoutof,model="w-b",weights=weightsOVL_caresparent)
summary(wbm_pr_caresparent_outof_2)

# Transitions into care on well-being
wbm_wb_caresparent_into_2<-wbm(wellbeing_index~caresparent_exp+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                                 health_num|asex+aeduc_bin|caresparent_exp*asex,
                               data=Sample_CParent_P_onlyinto,model="w-b",weights=weightsOVL_caresparent_wb)
summary(wbm_wb_caresparent_into_2) 

# Transitions out of care on well-being
wbm_wb_caresparent_outof_2<-wbm(wellbeing_index~caresparent_exp+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                                  health_num|asex+aeduc_bin|caresparent_exp*asex,
                                data=Sample_CParent_P_onlyoutof,model="w-b",weights=weightsOVL_caresparent_wb)
summary(wbm_wb_caresparent_outof_2) 


###
# Cares partner
###

carespartner_mat<-as.data.frame(cbind(Sample_CPartner_P$arid[which(Sample_CB_P$Wave==1)],
                                      paste(Sample_CPartner_P$carespartner[which(Sample_CB_P$Wave==1)]),
                                      paste(Sample_CPartner_P$carespartner[which(Sample_CB_P$Wave==2)]),
                                      paste(Sample_CPartner_P$carespartner[which(Sample_CB_P$Wave==3)])))
colnames(carespartner_mat)=c("arid","carespartner_1","carespartner_2","carespartner_3")

carespartner_mat$outofcare<-numeric(nrow(carespartner_mat))
carespartner_mat$intocare<-numeric(nrow(carespartner_mat))

carespartner_mat$outofcare[which(carespartner_mat$carespartner_1=="yes" &
                                   carespartner_mat$carespartner_2=="no")]=1
carespartner_mat$outofcare[which(carespartner_mat$carespartner_2=="yes" &
                                   carespartner_mat$carespartner_3=="no")]=1

carespartner_mat$intocare[which(carespartner_mat$carespartner_1=="no" &
                                  carespartner_mat$carespartner_2=="yes")]=1
carespartner_mat$intocare[which(carespartner_mat$carespartner_2=="no" &
                                  carespartner_mat$carespartner_3=="yes")]=1

Sample_CPartner_P_onlyinto<-Sample_CPartner_P[-which(Sample_CPartner_P$arid%in%
                                                       carespartner_mat$arid[which(carespartner_mat$outofcare==1)]),]
Sample_CPartner_P_onlyoutof<-Sample_CPartner_P[-which(Sample_CPartner_P$arid%in%
                                                        carespartner_mat$arid[which(carespartner_mat$intocare==1)]),]

# Transitions into care on preferences
wbm_pr_carespartner_into_2<-wbm(a1110_a_num~carespartner_exp+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                                  health_num|asex+aeduc_bin|carespartner_exp*asex,
                                data=Sample_CPartner_P_onlyinto,model="w-b",weights=weightsOVL_carespartner)
summary(wbm_pr_carespartner_into_2)

# Transitions out of care on preferences
wbm_pr_carespartner_outof_2<-wbm(a1110_a_num~carespartner_exp+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                                   health_num|asex+aeduc_bin|carespartner_exp*asex,
                                 data=Sample_CPartner_P_onlyoutof,model="w-b",weights=weightsOVL_carespartner)
summary(wbm_pr_carespartner_outof_2)

# Transitions into care on well-being
wbm_wb_carespartner_into_2<-wbm(wellbeing_index~carespartner_exp+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                                  health_num|asex+aeduc_bin|carespartner_exp*asex,
                                data=Sample_CPartner_P_onlyinto,model="w-b",weights=weightsOVL_carespartner_wb)
summary(wbm_wb_carespartner_into_2) 

# Transitions out of care on well-being
wbm_wb_carespartner_outof_2<-wbm(wellbeing_index~carespartner_exp+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                                   health_num|asex+aeduc_bin|carespartner_exp*asex,
                                 data=Sample_CPartner_P_onlyoutof,model="w-b",weights=weightsOVL_carespartner_wb)
summary(wbm_wb_carespartner_outof_2) 
