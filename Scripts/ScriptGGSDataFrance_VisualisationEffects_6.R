#####################################################################################################
## STUDY 2: Family Elderly Caregiving and Preferences for Family or State Responsibility in France ##
#####################################################################################################

####
## SCRIPT 7: PLOTTING THE EFFECTS
####

setwd("D:/BIGSSS/Dissertation/Study 2/Data/GGS Data/GGSAllWavesAllCountries")

load("GGSWP_France_session_3.RData")

library(ggplot2)
library(broom)
library(plm)
library(lme4)
library(datawizard)
library(ragg)

###################################################################################################

# 1) PLOTTING THE ESTIMATED WITHIN BETWEEN FOR PREFERENCES

mpr_caresparent_1_w<-plm(a1110_a_num~caresparent+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                           health_num,
                         data=subset(Sample_CParent,asex=="female"),index="arid",weights=weightsOVL_caresparent_f)
summary(mpr_caresparent_1_w) 

mpr_caresparent_1_m<-plm(a1110_a_num~caresparent+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                           health_num,
                         data=subset(Sample_CParent,asex=="male"),index="arid",weights=weightsOVL_caresparent_m)
summary(mpr_caresparent_1_m)

mpr_carespartner_1_w<-plm(a1110_a_num~carespartner+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                            health_num,
                          data=subset(Sample_CPartner,asex=="female"),index="arid",weights=weightsOVL_carespartner_f)
summary(mpr_carespartner_1_w) 

mpr_carespartner_1_m<-plm(a1110_a_num~carespartner+wellbeing_index+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                            health_num,
                          data=subset(Sample_CPartner,asex=="male"),index="arid",weights=weightsOVL_carespartner_m)
summary(mpr_carespartner_1_m)

caresparent_w_table<-tidy(mpr_caresparent_1_w,conf.int=T,conf.level=0.95)
caresparent_m_table<-tidy(mpr_caresparent_1_m,conf.int=T,conf.level=0.95)
carespartner_w_table<-tidy(mpr_carespartner_1_w,conf.int=T,conf.level=0.95)
carespartner_m_table<-tidy(mpr_carespartner_1_m,conf.int=T,conf.level=0.95)

caresparent_male<-caresparent_m_table$estimate[1]
caresparent_female<-caresparent_w_table$estimate[1]
carespartner_male<-carespartner_m_table$estimate[1]
carespartner_female<-carespartner_w_table$estimate[1]

caresparentLCI_male<-caresparent_m_table$conf.low[1]
caresparentUCI_male<-caresparent_m_table$conf.high[1]

caresparentLCI_female<-caresparent_w_table$conf.low[1]
caresparentUCI_female<-caresparent_w_table$conf.high[1]

carespartnerLCI_male<-carespartner_m_table$conf.low[1]
carespartnerUCI_male<-carespartner_m_table$conf.high[1]

carespartnerLCI_female<-carespartner_w_table$conf.low[1]
carespartnerUCI_female<-carespartner_w_table$conf.high[1]

plot_data<-data.frame(
  Term=c(rep("Cares for parent",2),rep("Cares for partner",2)),
  Gender=rep(c("Male","Female"),2),
  Estimate=c(caresparent_male,caresparent_female,carespartner_male,carespartner_female),
  LCI=c(caresparentLCI_male,caresparentLCI_female,carespartnerLCI_male,carespartnerLCI_female),
  UCI=c(caresparentUCI_male,caresparentUCI_female,carespartnerUCI_male,carespartnerUCI_female)
)
plot_data$Term<-factor(plot_data$Term,levels=c("Cares for parent or partner", "Cares for parent","Cares for partner"))

# Plot it:
agg_png("care_preferences.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(plot_data, aes(x = Term, y = Estimate, group = Gender, color = Gender, shape = Gender)) +
  geom_point(position = position_dodge(width = 0.25), size = 3) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), 
                width = 0.2, 
                position = position_dodge(width = 0.25), 
                alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.4) +
  theme_minimal(base_family = "Arial") +
  theme(
    #plot.title = element_text(size = 16, face = "bold"),
    #plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  labs(
    #title = "",
    #subtitle = "",
    x = "Relationship to care receiver",
    y = "Estimated Effect"
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16, 17)) # 16: filled circle, 17: filled triangle
dev.off()

##############################################################################################################################

# 2) PLOTING THE ESTIMATED WITHIN-EFFECTS FOR WELL-BEING

mwb_caresparent_1_w<-plm(wellbeing_index~caresparent+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                           health_num,
                         data=subset(Sample_CParent,asex=="female"),index="arid",weights=weightsOVL_caresparent_wb_f)
summary(mwb_caresparent_1_w) 

mwb_caresparent_1_m<-plm(wellbeing_index~caresparent+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                           health_num,
                         data=subset(Sample_CParent,asex=="male"),index="arid",weights=weightsOVL_caresparent_wb_m)
summary(mwb_caresparent_1_m) 

mwb_carespartner_1_w<-plm(wellbeing_index~carespartner+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                            health_num,
                          data=subset(Sample_CPartner,asex=="female"),index="arid",weights=weightsOVL_carespartner_wb_f)
summary(mwb_carespartner_1_w) 

mwb_carespartner_1_m<-plm(wellbeing_index~carespartner+aage+aactstat_grp+aparstat+lwparents+amarstat_bin+haskids+hhincome+
                            health_num,
                          data=subset(Sample_CPartner,asex=="male"),index="arid",weights=weightsOVL_carespartner_m)
summary(mwb_carespartner_1_m) 

caresparentwb_m_table<-tidy(mwb_caresparent_1_m,conf.int=T,conf.level=0.95)
caresparentwb_w_table<-tidy(mwb_caresparent_1_w,conf.int=T,conf.level=0.95)
carespartnerwb_m_table<-tidy(mwb_carespartner_1_m,conf.int=T,conf.level=0.95)
carespartnerwb_w_table<-tidy(mwb_carespartner_1_w,conf.int=T,conf.level=0.95)

caresparentwb_male<-caresparentwb_m_table$estimate[1]
caresparentwb_female<-caresparentwb_w_table$estimate[1]
carespartnerwb_male<-carespartnerwb_m_table$estimate[1]
carespartnerwb_female<-carespartnerwb_w_table$estimate[1]

caresparentwbLCI_male<-caresparentwb_m_table$conf.low[1]
caresparentwbUCI_male<-caresparentwb_m_table$conf.high[1]
caresparentwbLCI_female<-caresparentwb_w_table$conf.low[1]
caresparentwbUCI_female<-caresparentwb_w_table$conf.high[1]
carespartnerwbLCI_male<-carespartnerwb_m_table$conf.low[1]
carespartnerwbUCI_male<-carespartnerwb_m_table$conf.high[1]
carespartnerwbLCI_female<-carespartnerwb_w_table$conf.low[1]
carespartnerwbUCI_female<-carespartnerwb_w_table$conf.high[1]


plot_datawb<-data.frame(
  Term=c(rep("Cares for parent",2),rep("Cares for partner",2)),
  Gender=rep(c("Male","Female"),2),
  Estimate=c(caresparentwb_male,caresparentwb_female,carespartnerwb_male,carespartnerwb_female),
  LCI=c(caresparentwbLCI_male,caresparentwbLCI_female,carespartnerwbLCI_male,carespartnerwbLCI_female),
  UCI=c(caresparentwbUCI_male,caresparentwbUCI_female,carespartnerwbUCI_male,carespartnerwbUCI_female)
)
plot_datawb$Term<-factor(plot_datawb$Term,levels=c("Cares for parent","Cares for partner"))

# Plot it:
agg_png("care_wb.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(plot_datawb, aes(x = Term, y = Estimate, group = Gender, color = Gender, shape = Gender)) +
  geom_point(position = position_dodge(width = 0.25), size = 3) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), 
                width = 0.2, 
                position = position_dodge(width = 0.25), 
                alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.4) +
  theme_minimal(base_family = "Arial") +
  theme(
    #plot.title = element_text(size = 16, face = "bold"),
    #plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  labs(
    #title = "Estimated Causal Effect of Caregiving on Well-Being",
    #subtitle = "Comparison by Gender and Relationship to Care Receiver",
    x = "Relationship to care receiver",
    y = "Estimated Effect"
  ) +
  scale_shape_manual(values = c(16, 17)) +  # Map shapes here
  scale_color_brewer(palette = "Set1") 
dev.off()
###################################################################################################
