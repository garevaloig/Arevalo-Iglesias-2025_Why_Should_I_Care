#####################################################################################################
## STUDY 2: Family Elderly Caregiving and Preferences for Family or State Responsibility in France ##
#####################################################################################################

####
## SCRIPT 6: MEDIATION ANALYSIS
####

library(lme4)
library(mediation)
library(datawizard)

setwd("D:/BIGSSS/Dissertation/Study 2/Data/GGS Data/GGSAllWavesAllCountries")

load("GGSWP_France_session_3.RData")

##########################################################################################################################################


###
# STEP 1) FIT THE FIXED-EFFECTS MODELS WITH LM
###


# 1.1) MODELS FOR PREFERENCES

## CARESBIN:
Sample_CB$nrp<-numeric(nrow(Sample_CB)) # Create dummies for all categorical variables before demeaning (except references)
Sample_CB$nrp[which(Sample_CB$aparstat=="non-resident partner")]=1

Sample_CB$crp<-numeric(nrow(Sample_CB))
Sample_CB$crp[which(Sample_CB$aparstat=="co-resident partner")]=1

Sample_CB$hhi_m<-numeric(nrow(Sample_CB))
Sample_CB$hhi_m[which(Sample_CB$hhincome=="1500-3000")]=1

Sample_CB$hhi_h<-numeric(nrow(Sample_CB))
Sample_CB$hhi_h[which(Sample_CB$hhincome==">3000")]=1

Sample_CB_dem<-demean(Sample_CB,select=c("a1110_a_num","caresbin","wellbeing_index","aage","aactstat_grp",
                                                   "nrp","crp","haskids","hhi_m","hhi_h","lwparents",
                                                   "health_num","amarstat_bin"),
                           group="arid",suffix_demean="w")
Sample_CB_dem<-cbind(Sample_CB,Sample_CB_dem)

dlm_bin<-lm(a1110_a_numw~caresbinw+wellbeing_indexw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
              hhi_mw+hhi_hw+health_numw-1,
            data=Sample_CB_dem,weights=weightsOVL_caresbin)
summary(dlm_bin)

dlm_bin_f<-lm(a1110_a_numw~caresbinw+wellbeing_indexw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
                hhi_mw+hhi_hw+health_numw-1,
              data=subset(Sample_CB_dem,asex=="female"),weights=weightsOVL_caresbin_f)
summary(dlm_bin_f)

dlm_bin_m<-lm(a1110_a_numw~caresbinw+wellbeing_indexw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
                hhi_mw+hhi_hw+health_numw-1,
              data=subset(Sample_CB_dem,asex=="male"),weights=weightsOVL_caresbin_m)
summary(dlm_bin_m)

###

# CARESPARENT
Sample_CParent$nrp<-numeric(nrow(Sample_CParent)) # Create dummies for all categorical variables before demeaning (except references)
Sample_CParent$nrp[which(Sample_CParent$aparstat=="non-resident partner")]=1

Sample_CParent$crp<-numeric(nrow(Sample_CParent))
Sample_CParent$crp[which(Sample_CParent$aparstat=="co-resident partner")]=1

Sample_CParent$hhi_m<-numeric(nrow(Sample_CParent))
Sample_CParent$hhi_m[which(Sample_CParent$hhincome=="1500-3000")]=1

Sample_CParent$hhi_h<-numeric(nrow(Sample_CParent))
Sample_CParent$hhi_h[which(Sample_CParent$hhincome==">3000")]=1

Sample_CParent_dem<-demean(Sample_CParent,select=c("a1110_a_num","caresparent","wellbeing_index","aage","aactstat_grp",
                                                   "nrp","crp","haskids","hhi_m","hhi_h","lwparents",
                                                   "health_num","amarstat_bin"),
                           group="arid",suffix_demean="w")
Sample_CParent_dem<-cbind(Sample_CParent,Sample_CParent_dem)

dlm_parent<-lm(a1110_a_numw~caresparentw+wellbeing_indexw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
                 hhi_mw+hhi_hw+health_numw-1,
            data=Sample_CParent_dem,weights=weightsOVL_caresparent)
summary(dlm_parent)

dlm_parent_f<-lm(a1110_a_numw~caresparentw+wellbeing_indexw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
                   hhi_mw+hhi_hw+health_numw-1,
              data=subset(Sample_CParent_dem,asex=="female"),weights=weightsOVL_caresparent_f)
summary(dlm_parent_f)

dlm_parent_m<-lm(a1110_a_numw~caresparentw+wellbeing_indexw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
                   hhi_mw+hhi_hw+health_numw-1,
              data=subset(Sample_CParent_dem,asex=="male"),weights=weightsOVL_caresparent_m)
summary(dlm_parent_m)

###

# CARESPARTNER
Sample_CPartner$nrp<-numeric(nrow(Sample_CPartner)) # Create dummies for all categorical variables before demeaning (except references)
Sample_CPartner$nrp[which(Sample_CPartner$aparstat=="non-resident partner")]=1

Sample_CPartner$crp<-numeric(nrow(Sample_CPartner))
Sample_CPartner$crp[which(Sample_CPartner$aparstat=="co-resident partner")]=1

Sample_CPartner$hhi_m<-numeric(nrow(Sample_CPartner))
Sample_CPartner$hhi_m[which(Sample_CPartner$hhincome=="1500-3000")]=1

Sample_CPartner$hhi_h<-numeric(nrow(Sample_CPartner))
Sample_CPartner$hhi_h[which(Sample_CPartner$hhincome==">3000")]=1

Sample_CPartner_dem<-demean(Sample_CPartner,select=c("a1110_a_num","carespartner","wellbeing_index","aage","aactstat_grp",
                                                     "nrp","crp","haskids","hhi_m","hhi_h","lwparents",
                                                     "health_num","amarstat_bin"),
                            group="arid",suffix_demean="w")
Sample_CPartner_dem<-cbind(Sample_CPartner,Sample_CPartner_dem)

dlm_partner<-lm(a1110_a_numw~carespartnerw+wellbeing_indexw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
                  hhi_mw+hhi_hw+health_numw-1,
            data=Sample_CPartner_dem,weights=weightsOVL_carespartner)
summary(dlm_partner)

dlm_partner_f<-lm(a1110_a_numw~carespartnerw+wellbeing_indexw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
                    hhi_mw+hhi_hw+health_numw-1,
              data=subset(Sample_CPartner_dem,asex=="female"),weights=weightsOVL_carespartner_f)
summary(dlm_partner_f)

dlm_partner_m<-lm(a1110_a_numw~carespartnerw+wellbeing_indexw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
                    hhi_mw+hhi_hw+health_numw-1,
              data=subset(Sample_CPartner_dem,asex=="male"),weights=weightsOVL_carespartner_m)
summary(dlm_partner_m)

################################

# 1.2) MODELS FOR WELL-BEING

## CARESBIN:
dlm_wb_bin<-lm(wellbeing_indexw~caresbinw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
                 hhi_mw+hhi_hw+health_numw-1,
               data=Sample_CB_dem,weights=weightsOVL_caresbin)
summary(dlm_wb_bin)

dlm_wb_bin_f<-lm(wellbeing_indexw~caresbinw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
                   hhi_mw+hhi_hw+health_numw-1,
                 data=subset(Sample_CB_dem,asex=="female"),weights=weightsOVL_caresbin_f)
summary(dlm_wb_bin_f)

dlm_wb_bin_m<-lm(wellbeing_indexw~caresbinw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
                   hhi_mw+hhi_hw+health_numw-1,
                 data=subset(Sample_CB_dem,asex=="male"),weights=weightsOVL_caresbin_m)
summary(dlm_wb_bin_m)

##############################

## CARES for parent:
dlm_wb_parent<-lm(wellbeing_indexw~caresparentw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
                    hhi_mw+hhi_hw+health_numw-1,
                  data=Sample_CParent_dem,weights=weightsOVL_caresparent)
summary(dlm_wb_parent)

dlm_wb_parent_f<-lm(wellbeing_indexw~caresparentw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
                   hhi_mw+hhi_hw+health_numw-1,
                 data=subset(Sample_CParent_dem,asex=="female"),weights=weightsOVL_caresparent_f)
summary(dlm_wb_parent_f)

dlm_wb_parent_m<-lm(wellbeing_indexw~caresparentw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
                   hhi_mw+hhi_hw+health_numw-1,
                 data=subset(Sample_CParent_dem,asex=="male"),weights=weightsOVL_caresparent_m)
summary(dlm_wb_parent_m)

##############################

## CARES for partner:
dlm_wb_partner<-lm(wellbeing_indexw~carespartnerw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
                     hhi_mw+hhi_hw+health_numw-1,
                   data=Sample_CPartner_dem,weights=weightsOVL_carespartner)
summary(dlm_wb_partner)

dlm_wb_partner_f<-lm(wellbeing_indexw~carespartnerw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
                       hhi_mw+hhi_hw+health_numw-1,
                     data=subset(Sample_CPartner_dem,asex=="female"),weights=weightsOVL_carespartner_f)
summary(dlm_wb_partner_f)

dlm_wb_partner_m<-lm(wellbeing_indexw~carespartnerw+aagew+aactstat_grpw+nrpw+crpw+lwparentsw+amarstat_binw+haskidsw+
                       hhi_mw+hhi_hw+health_numw-1,
                     data=subset(Sample_CPartner_dem,asex=="male"),weights=weightsOVL_carespartner_m)
summary(dlm_wb_partner_m)


##########################################################################################################################################

###
# STEP 2) CAUSAL MEDIATION ANALYSIS
###

##############################

set.seed(1234)

## CARES for parent or partner:

summary(dlm_bin)
summary(dlm_wb_bin)

med_bin<-mediate(dlm_wb_bin,dlm_bin,sims=1000,treat="caresbinw",mediator="wellbeing_indexw",
                 covariates=list("aagew","aactstat_grpw","nrpw","crpw","lwparentsw","amarstat_binw","haskidsw",
                                 "hhi_mw","hhi_hw","health_numw"))

summary(med_bin)

######

summary(dlm_bin_f)
summary(dlm_wb_bin_f)

med_bin_f<-mediate(dlm_wb_bin_f,dlm_bin_f,sims=1000,treat="caresbinw",mediator="wellbeing_indexw",
                   covariates=list("aagew","aactstat_grpw","nrpw","crpw","lwparentsw","amarstat_binw","haskidsw",
                                   "hhi_mw","hhi_hw","health_numw"))
summary(med_bin_f)

######

summary(dlm_bin_m)
summary(dlm_wb_bin_m)

med_bin_m<-mediate(dlm_wb_bin_m,dlm_bin_m,sims=1000,treat="caresbinw",mediator="wellbeing_indexw",
                   covariates=list("aagew","aactstat_grpw","nrpw","crpw","lwparentsw","amarstat_binw","haskidsw",
                                   "hhi_mw","hhi_hw","health_numw"))

summary(med_bin_m)

##############################

## CARES for parent:

summary(dlm_parent)
summary(dlm_wb_parent)

med_parent<-mediate(dlm_wb_parent,dlm_parent,sims=1000,treat="caresparentw",mediator="wellbeing_indexw",
                    covariates=list("aagew","aactstat_grpw","nrpw","crpw","lwparentsw","amarstat_binw","haskidsw",
                                    "hhi_mw","hhi_hw","health_numw"))

summary(med_parent)

######

summary(dlm_parent_f)
summary(dlm_wb_parent_f)

med_parent_f<-mediate(dlm_wb_parent_f,dlm_parent_f,sims=1000,treat="caresparentw",mediator="wellbeing_indexw",
                      covariates=list("aagew","aactstat_grpw","nrpw","crpw","lwparentsw","amarstat_binw","haskidsw",
                                      "hhi_mw","hhi_hw","health_numw"))

summary(med_parent_f)

######

summary(dlm_parent_m)
summary(dlm_wb_parent_m)

med_parent_m<-mediate(dlm_wb_parent_m,dlm_parent_m,sims=1000,treat="caresparentw",mediator="wellbeing_indexw",
                      covariates=list("aagew","aactstat_grpw","nrpw","crpw","lwparentsw","amarstat_binw","haskidsw",
                                      "hhi_mw","hhi_hw","health_numw"))

summary(med_parent_m)



##############################

## CARES for partner:

summary(dlm_partner)
summary(dlm_wb_partner)

med_partner<-mediate(dlm_wb_partner,dlm_partner,sims=1000,treat="carespartnerw",mediator="wellbeing_indexw",
                     covariates=list("aagew","aactstat_grpw","nrpw","crpw","lwparentsw","amarstat_binw","haskidsw",
                                     "hhi_mw","hhi_hw","health_numw"))

summary(med_partner)

######

summary(dlm_partner_f)
summary(dlm_wb_partner_f)

med_partner_f<-mediate(dlm_wb_partner_f,dlm_partner_f,sims=1000,treat="carespartnerw",mediator="wellbeing_indexw",
                       covariates=list("aagew","aactstat_grpw","nrpw","crpw","lwparentsw","amarstat_binw","haskidsw",
                                       "hhi_mw","hhi_hw","health_numw"))

summary(med_partner_f)

######

summary(dlm_partner_m)
summary(dlm_wb_partner_m)

med_partner_m<-mediate(dlm_wb_partner_m,dlm_partner_m,sims=1000,treat="carespartnerw",mediator="wellbeing_indexw",
                       covariates=list("aagew","aactstat_grpw","nrpw","crpw","lwparentsw","amarstat_binw","haskidsw",
                                       "hhi_mw","hhi_hw","health_numw"))

summary(med_partner_m)

#######

# Plot the results
par(mfrow=c(3,2))
plot(med_bin_f,main="Women giving care to old-age parent or partner")
plot(med_bin_m,main="Men giving care to old-age parent or partner")
plot(med_parent_f,main="Women giving care to old-age parent")
plot(med_parent_m,main="Men giving care to old-age parent")
plot(med_partner_f,main="Women giving care to old-age partner")
plot(med_partner_m,main="Men giving care to old-age partner")

# Plot the sensitivity analyses
s_med_bin_f<-medsens(med_bin_f,rho.by=0.05)
plot(s_med_bin_f,main="Women giving care to old-age parent or partner")
s_med_bin_m<-medsens(med_bin_m,rho.by=0.05)
plot(s_med_bin_m,main="Men giving care to old-age parent or partner")
s_med_parent_f<-medsens(med_parent_f,rho.by=0.05)
plot(s_med_parent_f,main="Women giving care to old-age parent")
s_med_parent_m<-medsens(med_parent_m,rho.by=0.05)
plot(s_med_parent_m,main="Men giving care to old-age parent")
s_med_partner_f<-medsens(med_partner_f,rho.by=0.05)
plot(s_med_partner_f,main="Women giving care to old-age partner")
s_med_partner_m<-medsens(med_partner_m,rho.by=0.05)
plot(s_med_partner_m,main="Men giving care to old-age partner")
