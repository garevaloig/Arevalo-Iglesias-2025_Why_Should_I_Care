############
###### NOTE: RESULTS ARE CONSISTENT WITH THOSE OBTAINED WEIGHTING EACH OBSERVATION SEPARATELY. THESE RESULTS CAN BE ADDED AS ROBUSTNESS CHECKS
############

setwd("D:/BIGSSS/Dissertation/Study 2/Data/GGS Data/GGSAllWavesAllCountries")

load("GGSWP_France_session_1.RData")

###########################################################################################

###
# STEP 1) DEFINE INDIVIDUAL CONSTANT TREATMENT INDICATORS
###

GGSWP_France$treat_bin=factor(nrow(GGSWP_France),levels=c("control","treatment"))

for (i in 1:nrow(GGSWP_France)){
   if ("yes" %in% GGSWP_France$caresbin[which(GGSWP_France$arid==GGSWP_France$arid[i])]){
   GGSWP_France$treat_bin[i]="treatment"}
   else{GGSWP_France$treat_bin[i]="control"}
 }

GGSWP_France$treat_parent=factor(nrow(GGSWP_France),levels=c("control","treatment"))
  
for (i in 1:nrow(GGSWP_France)){
   if ("yes" %in% GGSWP_France$caresparent[which(GGSWP_France$arid==GGSWP_France$arid[i])]){
  GGSWP_France$treat_parent[i]="treatment"}
   else{GGSWP_France$treat_parent[i]="control"}
 }
  
GGSWP_France$treat_partner=factor(nrow(GGSWP_France),levels=c("control","treatment"))
    
    for (i in 1:nrow(GGSWP_France)){
    if ("yes" %in% GGSWP_France$carespartner[which(GGSWP_France$arid==GGSWP_France$arid[i])]){
    GGSWP_France$treat_partner[i]="treatment"}
     else{GGSWP_France$treat_partner[i]="control"}
    }

#########################################################################################

###
# STEP 2) DEFINE ANALYTICAL SAMPLES FOR EACH TREATMENT
###

# The analytical sample for each Treatment will be a balanced sample without missings (so only respondents with full info for all variables and waves)

# CARESBIN
SubsetCB<-GGSWP_France[,c("a1110_a_num","caresbin","caresbin_exp","caresbin_num","wellbeing_index","aage","aage_grp","asex","aactstat_grp",
                          "hhincome","aeduc_bin","haskids","treat_bin",
                          "amarstat_bin","aparstat","haspartner","lwpartner","lwparents","needscare","rec_informalcare","rec_profcare",
                          "valuesindex","health_num","aweight","arid","Wave")]
SubsetCB_Full<-na.omit(SubsetCB)

intersectionCB<-intersect(intersect(SubsetCB_Full$arid[which(SubsetCB_Full$Wave==1)],SubsetCB_Full$arid[which(SubsetCB_Full$Wave==2)]),
                          SubsetCB_Full$arid[which(SubsetCB_Full$Wave==3)])

Sample_CB<-SubsetCB_Full[which(SubsetCB_Full$arid %in% intersectionCB),]

# CARESPARENT
SubsetCPT<-GGSWP_France[,c("a1110_a_num","caresparent","caresparent_exp","caresparent_num","wellbeing_index","aage","aage_grp","asex",
                           "aactstat_grp",
                           "hhincome","aeduc_bin","haskids","treat_parent",
                           "amarstat_bin","aparstat","haspartner","lwpartner","lwparents","needscare","rec_informalcare","rec_profcare",
                           "valuesindex","health_num","arid","Wave")]
SubsetCPT_Full<-na.omit(SubsetCPT)

intersectionCPT<-intersect(intersect(SubsetCPT_Full$arid[which(SubsetCPT_Full$Wave==1)],SubsetCPT_Full$arid[which(SubsetCPT_Full$Wave==2)]),
                           SubsetCPT_Full$arid[which(SubsetCPT_Full$Wave==3)])

Sample_CParent<-SubsetCPT_Full[which(SubsetCPT_Full$arid %in% intersectionCPT),]

# CARESPARTNER
SubsetCPR<-GGSWP_France[,c("a1110_a_num","carespartner","carespartner_exp","carespartner_num","wellbeing_index","aage","aage_grp","asex","aactstat_grp",
                           "hhincome","aeduc_bin","haskids","treat_partner",
                           "amarstat_bin","aparstat","haspartner","lwpartner","lwparents","needscare","rec_informalcare","rec_profcare",
                           "valuesindex","health_num","arid","Wave")]
SubsetCPR_Full<-na.omit(SubsetCPR)

intersectionCPR<-intersect(intersect(SubsetCPR_Full$arid[which(SubsetCPR_Full$Wave==1)],SubsetCPR_Full$arid[which(SubsetCPR_Full$Wave==2)]),
                           SubsetCPR_Full$arid[which(SubsetCPR_Full$Wave==3)])

Sample_CPartner<-SubsetCPR_Full[which(SubsetCPR_Full$arid %in% intersectionCPR),]

#########################################################################################

###
# STEP 3) CREATE WEIGHTS
###

Sample_CB_dem<-demean(Sample_CB,select=c("a1110_a_num","caresbin","wellbeing_index","aage","aactstat_grp","aeduc_bin","hhincome","lwparents",
                                         "health_num","needscare","hhincome","aparstat","amarstat_bin","haskids","aage_grp"),
                      group="arid",suffix_demean="w")
Sample_CB_dem<-cbind(Sample_CB,Sample_CB_dem)

psform_caresbin<-treat_bin~aage_between+aactstat_grp_between+aeduc_bin+hhincome_between+lwparents_between+needscare_between+health_num_between

pscores_bin<-SumStat(ps.formula=psform_caresbin,weight=c("overlap","IPW"),data=Sample_CB_dem)
pscores_bin_f<-SumStat(ps.formula=psform_caresbin,weight=c("overlap","IPW"),data=subset(Sample_CB_dem,asex=="female"))
pscores_bin_m<-SumStat(ps.formula=psform_caresbin,weight=c("overlap","IPW"),data=subset(Sample_CB_dem,asex=="male"))

plot(pscores_bin,metric="ASD")   # Covariates balanced with both weights (slight unbalance for health with IPW)
plot(pscores_bin_f,metric="ASD")  # Covariates balanced with both weights (slight unbalance for health with IPW)
plot(pscores_bin_m,metric="ASD") # Covariates balanced with both weights (slight unbalance for health with IPW)

Sample_CB_dem$weightsOVL_bin<-pscores_bin$ps.weights$overlap

Sample_CB_dem$weightsOVL_bin_f<-numeric(nrow(Sample_CB_dem))
Sample_CB_dem$weightsOVL_bin_f[which(Sample_CB_dem$asex=="female")]<-pscores_bin_f$ps.weights$overlap
Sample_CB_dem$weightsOVL_bin_f[which(Sample_CB_dem$asex=="male")]=NA

Sample_CB_dem$weightsOVL_bin_m<-numeric(nrow(Sample_CB_dem))
Sample_CB_dem$weightsOVL_bin_m[which(Sample_CB_dem$asex=="male")]<-pscores_bin_m$ps.weights$overlap
Sample_CB_dem$weightsOVL_bin_m[which(Sample_CB_dem$asex=="female")]=NA

###################

Sample_CParent_dem<-demean(Sample_CParent,select=c("a1110_a_num","caresparent","wellbeing_index","aage","aactstat_grp","aeduc_bin","hhincome","lwparents",
                                         "health_num","needscare","hhincome","aparstat","amarstat_bin","haskids","aage_grp"),
                      group="arid",suffix_demean="w")
Sample_CParent_dem<-cbind(Sample_CParent,Sample_CParent_dem)

psform_caresparent<-treat_parent~aage_between+aactstat_grp_between+aeduc_bin+hhincome_between+lwparents_between+needscare_between+health_num_between

pscores_bin<-SumStat(ps.formula=psform_caresparent,weight=c("overlap","IPW"),data=Sample_CParent_dem)
pscores_bin_f<-SumStat(ps.formula=psform_caresparent,weight=c("overlap","IPW"),data=subset(Sample_CParent_dem,asex=="female"))
pscores_bin_m<-SumStat(ps.formula=psform_caresparent,weight=c("overlap","IPW"),data=subset(Sample_CParent_dem,asex=="male"))

plot(pscores_bin,metric="ASD")   # Covariates balanced with both weights (slight unbalance for health with IPW)
plot(pscores_bin_f,metric="ASD")  # Covariates balanced with both weights (slight unbalance for health with IPW)
plot(pscores_bin_m,metric="ASD") # Covariates balanced with both weights (slight unbalance for health with IPW)

Sample_CParent_dem$weightsOVL_parent<-pscores_bin$ps.weights$overlap

Sample_CParent_dem$weightsOVL_parent_f<-numeric(nrow(Sample_CParent_dem))
Sample_CParent_dem$weightsOVL_parent_f[which(Sample_CParent_dem$asex=="female")]<-pscores_bin_f$ps.weights$overlap
Sample_CParent_dem$weightsOVL_parent_f[which(Sample_CParent_dem$asex=="male")]=NA

Sample_CParent_dem$weightsOVL_parent_m<-numeric(nrow(Sample_CParent_dem))
Sample_CParent_dem$weightsOVL_parent_m[which(Sample_CParent_dem$asex=="male")]<-pscores_bin_m$ps.weights$overlap
Sample_CParent_dem$weightsOVL_parent_m[which(Sample_CParent_dem$asex=="female")]=NA

###################

Sample_CPartner$aage_grp2<-recode_factor(Sample_CPartner$aage_grp,"Under 45"="Under 60","45-59"="Under 60","60 or over"="60 or over")

Sample_CPartner_dem<-demean(Sample_CPartner,select=c("a1110_a_num","carespartner","wellbeing_index","aage","aage_grp2","aactstat_grp",
                                                     "aeduc_bin","hhincome","lwparents",
                                                   "health_num","needscare","hhincome","aparstat","amarstat_bin","haskids","aage_grp"),
                           group="arid",suffix_demean="w")
Sample_CPartner_dem<-cbind(Sample_CPartner,Sample_CPartner_dem)

psform_carespartner<-treat_partner~aage_grp2_between+aactstat_grp_between+aeduc_bin+hhincome_between+needscare_between+health_num_between

pscores_bin<-SumStat(ps.formula=psform_carespartner,weight=c("overlap","IPW"),data=Sample_CPartner_dem)
pscores_bin_f<-SumStat(ps.formula=psform_carespartner,weight=c("overlap","IPW"),data=subset(Sample_CPartner_dem,asex=="female"))
pscores_bin_m<-SumStat(ps.formula=psform_carespartner,weight=c("overlap","IPW"),data=subset(Sample_CPartner_dem,asex=="male"))

plot(pscores_bin,metric="ASD")   # Covariates balanced with both weights (slight unbalance for health with IPW)
plot(pscores_bin_f,metric="ASD")  # Covariates balanced with both weights (slight unbalance for health with IPW)
plot(pscores_bin_m,metric="ASD") # Covariates balanced with both weights (slight unbalance for health with IPW)

Sample_CPartner_dem$weightsOVL_partner<-pscores_bin$ps.weights$overlap

Sample_CPartner_dem$weightsOVL_partner_f<-numeric(nrow(Sample_CPartner_dem))
Sample_CPartner_dem$weightsOVL_partner_f[which(Sample_CPartner_dem$asex=="female")]<-pscores_bin_f$ps.weights$overlap
Sample_CPartner_dem$weightsOVL_partner_f[which(Sample_CPartner_dem$asex=="male")]=NA

Sample_CPartner_dem$weightsOVL_partner_m<-numeric(nrow(Sample_CPartner_dem))
Sample_CPartner_dem$weightsOVL_partner_m[which(Sample_CPartner_dem$asex=="male")]<-pscores_bin_m$ps.weights$overlap
Sample_CPartner_dem$weightsOVL_partner_m[which(Sample_CPartner_dem$asex=="female")]=NA

#########################################################################################

###
# STEP 4) ANALYSIS
###

dlm_bin<-lm(a1110_a_numw~caresbinw+wellbeing_indexw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
              health_numw,
            data=Sample_CB_dem,weights=weightsOVL_bin)
summary(dlm_bin)

dlm_bin_f<-lm(a1110_a_numw~caresbinw+wellbeing_indexw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
                health_numw,
              data=subset(Sample_CB_dem,asex=="female"),weights=weightsOVL_bin_f)
summary(dlm_bin_f)

dlm_bin_m<-lm(a1110_a_numw~caresbinw+wellbeing_indexw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
                health_numw,
              data=subset(Sample_CB_dem,asex=="male"),weights=weightsOVL_bin_m)
summary(dlm_bin_m)

###################

dlm_parent<-lm(a1110_a_numw~caresparentw+wellbeing_indexw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
              health_numw,
            data=Sample_CParent_dem,weights=weightsOVL_parent)
summary(dlm_parent)

dlm_parent_f<-lm(a1110_a_numw~caresparentw+wellbeing_indexw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
                health_numw,
              data=subset(Sample_CParent_dem,asex=="female"),weights=weightsOVL_parent_f)
summary(dlm_parent_f)

dlm_parent_m<-lm(a1110_a_numw~caresparentw+wellbeing_indexw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
                health_numw,
              data=subset(Sample_CParent_dem,asex=="male"),weights=weightsOVL_parent_m)
summary(dlm_parent_m)

###################

dlm_partner<-lm(a1110_a_numw~carespartnerw+wellbeing_indexw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
                 health_numw,
               data=Sample_CPartner_dem,weights=weightsOVL_partner)
summary(dlm_partner)

dlm_partner_f<-lm(a1110_a_numw~carespartnerw+wellbeing_indexw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
                   health_numw,
                 data=subset(Sample_CPartner_dem,asex=="female"),weights=weightsOVL_partner_f)
summary(dlm_partner_f)

dlm_partner_m<-lm(a1110_a_numw~carespartnerw+wellbeing_indexw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
                   health_numw,
                 data=subset(Sample_CPartner_dem,asex=="male"),weights=weightsOVL_partner_m)
summary(dlm_partner_m)

######################################
######################################

dlm_wb_bin<-lm(wellbeing_indexw~caresbinw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
              health_numw,
            data=Sample_CB_dem,weights=weightsOVL_bin)
summary(dlm_wb_bin)

dlm_wb_bin_f<-lm(wellbeing_indexw~caresbinw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
                health_numw,
              data=subset(Sample_CB_dem,asex=="female"),weights=weightsOVL_bin_f)
summary(dlm_wb_bin_f)

dlm_wb_bin_m<-lm(wellbeing_indexw~caresbinw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
                health_numw,
              data=subset(Sample_CB_dem,asex=="male"),weights=weightsOVL_bin_m)
summary(dlm_wb_bin_m)

###################

dlm_wb_parent<-lm(wellbeing_indexw~caresparentw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
                 health_numw,
               data=Sample_CParent_dem,weights=weightsOVL_parent)
summary(dlm_wb_parent)

dlm_wb_parent_f<-lm(wellbeing_indexw~caresparentw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
                   health_numw,
                 data=subset(Sample_CParent_dem,asex=="female"),weights=weightsOVL_parent_f)
summary(dlm_wb_parent_f)

dlm_wb_parent_m<-lm(wellbeing_indexw~caresparentw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
                   health_numw,
                 data=subset(Sample_CParent_dem,asex=="male"),weights=weightsOVL_parent_m)
summary(dlm_wb_parent_m)

###################

dlm_wb_partner<-lm(wellbeing_indexw~carespartnerw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
                  health_numw,
                data=Sample_CPartner_dem,weights=weightsOVL_partner)
summary(dlm_wb_partner)

dlm_wb_partner_f<-lm(wellbeing_indexw~carespartnerw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
                    health_numw,
                  data=subset(Sample_CPartner_dem,asex=="female"),weights=weightsOVL_partner_f)
summary(dlm_wb_partner_f)

dlm_wb_partner_m<-lm(wellbeing_indexw~carespartnerw+aagew+aactstat_grpw+aparstatw+lwparentsw+amarstat_binw+haskidsw+hhincomew+
                    health_numw,
                  data=subset(Sample_CPartner_dem,asex=="male"),weights=weightsOVL_partner_m)
summary(dlm_wb_partner_m)

########################################################################################################################################################

Sample_CB_dem_P<-panel_data(Sample_CB_dem,id=arid,wave=Wave)

wbm_pr_caresbin_2<-wbm(a1110_a_num~caresbin+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                         health_num|asex+aeduc_bin|caresbin*asex,
                       data=Sample_CB_dem_P,model="w-b",weights=weightsOVL_bin)
summary(wbm_pr_caresbin_2)

####

Sample_CParent_dem_P<-panel_data(Sample_CParent_dem,id=arid,wave=Wave)

wbm_pr_caresparent_2<-wbm(a1110_a_num~caresparent+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                         health_num|asex+aeduc_bin|caresparent*asex,
                       data=Sample_CParent_dem_P,model="w-b",weights=weightsOVL_parent)
summary(wbm_pr_caresparent_2)

####

Sample_CPartner_dem_P<-panel_data(Sample_CPartner_dem,id=arid,wave=Wave)

wbm_pr_carespartner_2<-wbm(a1110_a_num~carespartner+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                         health_num|asex+aeduc_bin|carespartner*asex,
                       data=Sample_CPartner_dem_P,model="w-b",weights=weightsOVL_partner)
summary(wbm_pr_carespartner_2)

######################################
######################################

wbm_wb_caresbin_2<-wbm(wellbeing_index~caresbin+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                         health_num|asex+aeduc_bin|caresbin*asex,
                       data=Sample_CB_dem_P,model="w-b",weights=weightsOVL_bin)
summary(wbm_wb_caresbin_2)

####

wbm_wb_caresparent_2<-wbm(wellbeing_index~caresparent+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                            health_num|asex+aeduc_bin|caresparent*asex,
                          data=Sample_CParent_dem_P,model="w-b",weights=weightsOVL_parent)
summary(wbm_wb_caresparent_2)

####

wbm_wb_carespartner_2<-wbm(wellbeing_index~carespartner+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                             health_num|asex+aeduc_bin|carespartner*asex,
                           data=Sample_CPartner_dem_P,model="w-b",weights=weightsOVL_partner)
summary(wbm_wb_carespartner_2)
