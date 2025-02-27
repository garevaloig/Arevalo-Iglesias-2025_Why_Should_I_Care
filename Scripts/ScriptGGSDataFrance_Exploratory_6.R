#####################################################################################################
## STUDY 2: Family Elderly Caregiving and Preferences for Family or State Responsibility in France ##
#####################################################################################################

####
## SCRIPT 3: EXPLORATORY ANALYSES
####


library(Hmisc)
library(ggplot2)
library(dplyr)
library(sjPlot)
library(ggplot2)
library(reshape2)
library(tidyr)
library(forcats)
library(gridExtra)
library(ragg)
library(RColorBrewer)
library(patchwork)

setwd("D:/BIGSSS/Dissertation/Study 2/Data/GGS Data/GGSAllWavesAllCountries")

load("GGSWP_France_session_3.RData")

colorpalette<-c("#e60049", "#0bb4ff", "#50e991", "#e6d800", "#9b19f5", "#ffa300", "#dc0ab4", "#b3d4ff", "#00bfa0")


##################################

## DESCRIPTIVES OF MAIN VARIABLES

# 1) Preferences

# Distribution of preferences across the whole sample
ggplot(data=GGSWP_France[-which(is.na(GGSWP_France$a1110_a)),],aes(x=a1110_a))+geom_bar(aes(y=..count../sum(..count..)),fill="steelblue")+
xlab("Preference for society or family responsibility")+ylab("Proportion")+
scale_x_discrete(labels=c("Mainly family","More family than society","Equally both","More society than family","Mainly society"))+
theme_minimal()

# Distribution of preferences across genders
PFPlot<-ggplot(data=GGSWP_France[-which(is.na(GGSWP_France$a1110_a) | GGSWP_France$asex!="female"),],aes(x=a1110_a))+
  geom_bar(aes(y=..count../sum(..count..)),fill=colorpalette[1])+
  xlab("Preference for society or family responsibility")+ylab("Proportion")+ggtitle("Female")+ylim(0,0.5)+
  scale_x_discrete(labels=c("Mainly family","More family than society","Equally both","More society than family","Mainly society"))+
  theme_minimal()

PMPlot<-ggplot(data=GGSWP_France[-which(is.na(GGSWP_France$a1110_a) | GGSWP_France$asex!="male"),],aes(x=a1110_a))+
  geom_bar(aes(y=..count../sum(..count..)),fill=colorpalette[9])+
  xlab("Preference for society or family responsibility")+ylab("Proportion")+ggtitle("Male")+ylim(0,0.5)+
  scale_x_discrete(labels=c("Mainly family","More family than society","Equally both","More society than family","Mainly society"))+
  theme_minimal()

grid.arrange(PFPlot,PMPlot,nrow=2)

ggplot(data=GGSWP_France[-which(is.na(GGSWP_France$a1110_a)),],aes(fill=asex,x=a1110_a))+
geom_bar(aes(y=..count../sum(..count..)),position=position_dodge())+
xlab("Preference for society or family responsibility")+ylab("Proportion")+
  scale_x_discrete(labels=c("Mainly family","More family than society","Equally both","More society than family","Mainly society"))+
  theme_minimal()


# Distribution of preferences across caregiving groups
CParentPlot<-ggplot(data=GGSWP_France[-which(is.na(GGSWP_France$a1110_a) | GGSWP_France$cares_grp!="parent"),],aes(x=a1110_a))+
  geom_bar(aes(y=..count../sum(..count..)),fill=colorpalette[1])+
  xlab("")+ylab("Proportion")+ggtitle("Caregivers of a parent")+ylim(0,0.5)+
  scale_x_discrete(labels=c("Mainly family","More family than society","Equally both","More society than family","Mainly society"))+
  theme_minimal()

CPartnerPlot<-ggplot(data=GGSWP_France[-which(is.na(GGSWP_France$a1110_a) | GGSWP_France$cares_grp!="partner/spouse"),],aes(x=a1110_a))+
  geom_bar(aes(y=..count../sum(..count..)),fill=colorpalette[4])+
  xlab("")+ylab("Proportion")+ggtitle("Caregivers of a partner")+ylim(0,0.5)+
  scale_x_discrete(labels=c("Mainly family","More family than society","Equally both","More society than family","Mainly society"))+
  theme_minimal()

NoCarePlot<-ggplot(data=GGSWP_France[-which(is.na(GGSWP_France$a1110_a) | GGSWP_France$cares_grp!="no care"),],aes(x=a1110_a))+
  geom_bar(aes(y=..count../sum(..count..)),fill=colorpalette[9])+
  xlab("Preference for society or family responsibility")+ylab("Proportion")+ggtitle("Non caregivers")+ylim(0,0.5)+
  scale_x_discrete(labels=c("Mainly family","More family than society","Equally both","More society than family","Mainly society"))+
  theme_minimal()

grid.arrange(CParentPlot,CPartnerPlot,NoCarePlot,nrow=3)

# Average preference for each group of gender/relation to care receiver (include in paper)

care_pref<-data.frame(cbind(
  c(rep("Total",4),rep("Female",4),rep("Male",4)),
  rep(c("No family care","Cares","Cares for partner","Cares for parent"),3),
  round(c(mean(GGSWP_France$a1110_a_num[which(GGSWP_France$caresbin=="no")],na.rm=T),
          mean(GGSWP_France$a1110_a_num[which(GGSWP_France$caresbin=="yes")],na.rm=T),
          mean(GGSWP_France$a1110_a_num[which(GGSWP_France$carespartner=="yes")],na.rm=T),
          mean(GGSWP_France$a1110_a_num[which(GGSWP_France$caresparent=="yes")],na.rm=T),
          mean(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="female" & GGSWP_France$caresbin=="no")],na.rm=T),
          mean(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="female" & GGSWP_France$caresbin=="yes")],na.rm=T),
          mean(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="female" & GGSWP_France$carespartner=="yes")],na.rm=T),
          mean(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="female" & GGSWP_France$caresparent=="yes")],na.rm=T),
          mean(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="male" & GGSWP_France$caresbin=="no")],na.rm=T),
          mean(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="male" & GGSWP_France$caresbin=="yes")],na.rm=T),
          mean(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="male" & GGSWP_France$carespartner=="yes")],na.rm=T),
          mean(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="male" & GGSWP_France$caresparent=="yes")],na.rm=T)),
        2)))
care_pref$X3<-as.numeric(care_pref$X3)

care_pref$sd<-c(sd(GGSWP_France$a1110_a_num[which(GGSWP_France$caresbin=="no")],na.rm=T),
                sd(GGSWP_France$a1110_a_num[which(GGSWP_France$caresbin=="yes")],na.rm=T),
                sd(GGSWP_France$a1110_a_num[which(GGSWP_France$carespartner=="yes")],na.rm=T),
                sd(GGSWP_France$a1110_a_num[which(GGSWP_France$caresparent=="yes")],na.rm=T),
                sd(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="female" & GGSWP_France$caresbin=="no")],na.rm=T),
                sd(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="female" & GGSWP_France$caresbin=="yes")],na.rm=T),
                sd(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="female" & GGSWP_France$carespartner=="yes")],na.rm=T),
                sd(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="female" & GGSWP_France$caresparent=="yes")],na.rm=T),
                sd(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="male" & GGSWP_France$caresbin=="no")],na.rm=T),
                sd(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="male" & GGSWP_France$caresbin=="yes")],na.rm=T),
                sd(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="male" & GGSWP_France$carespartner=="yes")],na.rm=T),
                sd(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="male" & GGSWP_France$caresparent=="yes")],na.rm=T))

care_pref$N<-c(sum(!is.na(GGSWP_France$a1110_a_num[which(GGSWP_France$caresbin=="no")])),
               sum(!is.na(GGSWP_France$a1110_a_num[which(GGSWP_France$caresbin=="yes")])),
               sum(!is.na(GGSWP_France$a1110_a_num[which(GGSWP_France$carespartner=="yes")])),
               sum(!is.na(GGSWP_France$a1110_a_num[which(GGSWP_France$caresparent=="yes")])),
               sum(!is.na(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="female" & GGSWP_France$caresbin=="no")])),
               sum(!is.na(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="female" & GGSWP_France$caresbin=="yes")])),
               sum(!is.na(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="female" & GGSWP_France$carespartner=="yes")])),
               sum(!is.na(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="female" & GGSWP_France$caresparent=="yes")])),
               sum(!is.na(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="male" & GGSWP_France$caresbin=="no")])),
               sum(!is.na(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="male" & GGSWP_France$caresbin=="yes")])),
               sum(!is.na(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="male" & GGSWP_France$carespartner=="yes")])),
               sum(!is.na(GGSWP_France$a1110_a_num[which(GGSWP_France$asex=="male" & GGSWP_France$caresparent=="yes")])))

care_pref$SE<-care_pref$sd/sqrt(care_pref$N)

care_pref$CI_lower<-care_pref$X3-1.96*care_pref$SE
care_pref$CI_upper<-care_pref$X3+1.96*care_pref$SE

colnames(care_pref)[1:3]=c("Gender","Caregiver_group","Average_Preference")

agg_png("average_pref.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(care_pref, aes(x = Caregiver_group, y = Average_Preference, color = Gender)) +
  geom_point(aes(shape = Gender), size = 4, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2, position = position_dodge(width = 0.5)) +
  ylim(2, 3) +
  labs(x = "Caregiver group", y = "Average preference", title = "") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(
    #panel.background = element_rect(fill = "grey96", color = NA),  # Add grey background to plot panel
    #plot.background = element_rect(fill = "grey95", color = NA),   # Add light grey background to entire plot
    axis.title.x = element_text(margin = margin(t = 15)), # Add vertical space to x-axis title
    axis.title.y = element_text(margin = margin(r = 15)), # Add horizontal space to y-axis title
    axis.text.x = element_text(margin = margin(t = 5)),   # Add space between x-axis labels and ticks
    axis.text.y = element_text(margin = margin(r = 5))    # Add space between y-axis labels and ticks
  )
dev.off()
# 2) Care

# Distribution of care tasks across gender and age groups
care_props<-cbind(prop.table(table(GGSWP_France$cares_grp,GGSWP_France$asex),1),
                  prop.table(table(GGSWP_France$cares_grp,GGSWP_France$aage_grp),1))

care_props<-melt(care_props,id.vars = c("asex","aage_grp2"))
colnames(care_props)=c("Care_Receiver","Caregiver_Group","Proportion")

ggplot(data=care_props[which(care_props$Caregiver_Group=="female" | care_props$Caregiver_Group=="male"),],
       aes(fill=Caregiver_Group,y=Proportion,x=Care_Receiver))+
  geom_bar(stat="identity",position=position_dodge())+
  xlab("Care Receiver")+ylab("Proportion")+labs(fill="Gender")+
  scale_x_discrete(labels=c("No care","Partner","Parent"))+
  theme_minimal()

ggplot(data=care_props[which(care_props$Caregiver_Group!="female" & care_props$Caregiver_Group!="male"),],
       aes(fill=Caregiver_Group,y=Proportion,x=Care_Receiver))+
  geom_bar(stat="identity",position=position_dodge())+
  xlab("Care Receiver")+ylab("Proportion")+labs(fill="Age")+
  scale_x_discrete(labels=c("No care","Partner","Parent"))+
  theme_minimal()

care_props_gender <- subset(care_props, Caregiver_Group %in% c("male", "female"))
care_props_gender$Care_Receiver<-factor(care_props_gender$Care_Receiver,levels=c("no care","parent","partner/spouse"))
care_props_gender$Caregiver_Group<-factor(care_props_gender$Caregiver_Group, levels=c("female","male"))
care_props_age <- subset(care_props, Caregiver_Group %in% c("Under 45","45-59","60 or over"))
care_props_age$Care_Receiver<-factor(care_props_age$Care_Receiver,levels=c("no care","parent","partner/spouse"))
care_props_age$Caregiver_Group<-factor(care_props_age$Caregiver_Group,
                                       levels=c("60 or over","45-59","Under 45"))

# Plot
gpp<-ggplot(care_props_gender, aes(x = Care_Receiver, y = Proportion, fill = Caregiver_Group)) +
  geom_bar(stat = "identity") +                 # Use 'identity' because 'Proportion' already holds the values
  scale_fill_brewer(palette = "Set1") +         # Apply "Set1" color palette for fill colors
  scale_y_continuous(labels = scales::percent) + # Optional: format y-axis as percentages
  labs(x = "", y = "Proportion", fill = "Gender") +
  theme_minimal() +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

app<-ggplot(care_props_age, aes(x = Care_Receiver, y = Proportion, fill = Caregiver_Group)) +
  geom_bar(stat = "identity") +               # Use 'identity' since 'Proportion' gives actual values
  scale_fill_manual(values = brewer.pal(5, "Set1")[c(4,5,3)]) +
  scale_y_continuous(labels = scales::percent) + # Optional: format y-axis as percentages
  labs(x = "Care Receiver", y = "Proportion", fill = "Age") +
  theme_minimal() +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

combined_plot <- gpp / app

agg_png("proportions.png", width = 1000, height = 625, units = "px", res = 144)
combined_plot
dev.off()

# 3) Well-being

# Distribution of well-being across care groups
GGSWP_France$cares_gender<-fct_cross(GGSWP_France$asex,GGSWP_France$cares_grp)

ggplot(data=GGSWP_France[-which(is.na(GGSWP_France$cares_gender)),],aes(x=wellbeing_index,y=cares_gender))+
  geom_boxplot(fill=colorpalette[1:6])+
  xlab("Well-being")+ylab("Caregiver group")+
  scale_y_discrete(labels=c("Male:no care","Female:no care","Male:partner","Female:partner","Male:parent","Female:parent"))+
  theme_minimal()

##################################

## CORRELATIONS BETWEEN MAIN VARIABLES
cor.data<-GGSWP_France[,c("asex","aage","caresbin_num",
                          "a1110_a_num","wellbeing_index")]

cor.data$asex<-as.numeric(cor.data$asex)-1

cor.table<-cor(na.omit(cor.data,use="complete.obs"))

melted.cor.table<-melt(cor.table)

ggplot(data = melted.cor.table, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+geom_text(aes(label = round(value, 4)), color = "white")+
  xlab("")+ylab("")+labs(fill="Correlation")+
  scale_x_discrete(labels=c("Female","Age","Cares","Preferences","Well-being"))+
  scale_y_discrete(labels=c("Female","Age","Cares","Preferences","Well-being"))+
  scale_fill_viridis(option = "D")+
  theme_minimal()

cor.tests<-rcorr(as.matrix(cor.data))
pval.table<-cor.tests$P
melted.pval.table<-melt(pval.table)

ggplot(data = melted.pval.table, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(label = round(value, 4)), color = "red")+
  xlab("") +
  ylab("") +
  labs(fill="p-value") +
  scale_x_discrete(labels=c("Female","Age","Cares","Preferences","Well-being")) +
  scale_y_discrete(labels=c("Female","Age","Cares","Preferences","Well-being")) +
  scale_fill_viridis(option = "D", direction = -1) +  # Using the "C" option of viridis with inverted direction
  theme_minimal()

################################################################################################



