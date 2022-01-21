library(tidyverse)
library(ggplot2)
require(car)
rm(list=ls()); gc()

source("index_construction.R")
dat_all <- readRDS("data/index_calc.rds")
## Plots

#figure 1
dat_all%>%
  mutate(en_r = car::recode(enrtot, recodes = "0:999='1'; 1000:4999='2';5000:9999='3';10000:19999='4';20000:150000='5'", as.factor = T), 
         rsicat= factor(ifelse(sc_score<mu, "1Not RSI", ifelse(sc_score>=mu&sc_score<mu+sds, "2RSI", "3High RSI"))))%>%
  mutate(rsicat =relevel(rsicat,ref = "1Not RSI") )%>%
  #         rsi_cat = ifelse(sc_score <mu, "NonRSI", ifelse(sc_score>=mu+sds, "High RSI", "Non_RSI")))%>%
  group_by(rsicat, en_r)%>%
  summarize(ns = n())%>%
  mutate(pct = ns/sum(ns))%>%
  ggplot()+
  geom_col(aes(x=en_r,y=ns,
               group=rsicat,
               fill=rsicat),
           position="dodge")+
  labs(title = "Headcount Enrollment for RSIs, High RSIs, and Non-RSIs",
       caption = "Source: ARRC Compiled Dataset\n Calculations by Corey S. Sparks, Ph.D.",
       x = "Enrollment Size",
       y = "Frequency")+
  #ylim(c(0, .2))+
  scale_fill_manual( name = "RSI Category",
                     labels=c("Non-RSI", "RSI","High RSI"),
                     values = c("#00AEEF","#116882","#8DC63F"))+
  scale_x_discrete(labels=c("1" = "0-999", "2" = "1,000-4,999",
                            "3" = "5,000-9,999", "4"="10,000-19,999",
                            "5"="20,000+"))+
  # scale_y_continuous(labels = scales::percent)+
  #guides(fill=guide_legend(title="Race/Ethnicity"))+
  #geom_hline(yintercept = 0, col="red", lwd=1.1)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))

ggsave(filename = "fig1.png", width = 10, height=8, dpi="print")  

#Figure 2

dat_all%>%
  mutate(en_r = car::recode(fte,
                            recodes = "0:999='1'; 1000:4999='2';5000:9999='3';10000:19999='4';20000:150000='5'",
                            as.factor = T), 
         rsicat= factor(ifelse(sc_score<mu, "1Not RSI", 
                               ifelse(sc_score>=mu&sc_score<mu+sds, "2RSI", "3High RSI"))))%>%
  mutate(rsicat =relevel(rsicat,ref = "1Not RSI") )%>%
  #         rsi_cat = ifelse(sc_score <mu, "NonRSI", ifelse(sc_score>=mu+sds, "High RSI", "Non_RSI")))%>%
  group_by(rsicat, en_r)%>%
  summarize(ns = n())%>%
    mutate(pct = ns/sum(ns))%>%
  ggplot()+
  geom_col(aes(x=en_r,y=ns,
               group=rsicat,
               fill=rsicat),
           position="dodge")+
    labs(title = "Full-Time Equivalent Enrollment for RSIs, High RSIs, and Non-RSIs",
         caption = "Source: ARRC Compiled Dataset\n Calculations by Corey S. Sparks, Ph.D.",
         x = "Enrollment Size",
         y = "Frequency")+
    #ylim(c(0, .2))+
    scale_fill_manual( name = "RSI Category",
                        labels=c("Non-RSI", "RSI","High RSI"),
                        values = c("#00AEEF","#116882","#8DC63F"))+
    scale_x_discrete(labels=c("1" = "0-999", "2" = "1,000-4,999",
                              "3" = "5,000-9,999", "4"="10,000-19,999",
                              "5"="20,000+"))+
   # scale_y_continuous(labels = scales::percent)+
    #guides(fill=guide_legend(title="Race/Ethnicity"))+
    #geom_hline(yintercept = 0, col="red", lwd=1.1)+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45))

ggsave(filename = "fig1b.png", width = 10, height=8, dpi="print")  
  

# figure 3
#supp.labs <- c("Graduate Enrollment", "Undergrad Enrollment")
dat_all%>%
  filter(sector %in%1:2)%>%
  mutate(sector2 = factor(ifelse(sector ==1, "Public 4 Year", "Private 4 Year")), 
         rsicat= factor(ifelse(sc_score<mu, "1Not RSI",
                               ifelse(sc_score>=mu&sc_score<mu+sds,
                                      "2RSI", "3High RSI"))))%>%
  mutate(rsicat =ordered(rsicat,levels = c("1Not RSI", "2RSI", "3High RSI")),
         sector2 = relevel(sector2, ref="Public 4 Year"))%>%
  #         rsi_cat = ifelse(sc_score <mu, "NonRSI", ifelse(sc_score>=mu+sds, "High RSI", "Non_RSI")))%>%
 #   filter(rsicat %in% c("1Not RSI","2RSI" ))%>%
  group_by(rsicat, sector2)%>%
  summarize(efug = mean(efug, na.rm=T),
            efgrad = mean(efgrad, na.rm=T))%>%
  #mutate(pct = ns/sum(ns))%>%
  pivot_longer(cols= c(-rsicat, -sector2))%>%  
  mutate(name2 = factor(ifelse(name =="efug","Undergrad Enrollment", "Graduate Enrollment") ) )%>%
  mutate(name2 = relevel(name2, ref="Undergrad Enrollment"))%>%
  ggplot()+
  geom_col(aes(x=sector2,y=value,
               group=rsicat,
               fill=rsicat),
           position="dodge")+
  facet_wrap(~name2, ncol=2)+
  labs(title = "Average Undergraduate and Graduate Headcount\nEnrollment for RSIs and Non-RSIs
",
       caption = "Source: ARRC Compiled Dataset\n Calculations by Corey S. Sparks, Ph.D.",
       x = "Institution Type",
       y = "Mean")+
  #ylim(c(0, .2))+
  scale_fill_manual( name = "Category",
                     labels=c("Non-RSI", "RSI", "High RSI"),
                     values = c("#00AEEF","#116882","#8DC63F"))+
  # scale_x_discrete(labels=c("1" = "0-999", "2" = "1,000-4,999",
  #                           "3" = "5,000-9,999", "4"="10,000-19,999",
  #                           "5"="20,000+"))+
  # # scale_y_continuous(labels = scales::percent)+
  #guides(fill=guide_legend(title="Race/Ethnicity"))+
  #geom_hline(yintercept = 0, col="red", lwd=1.1)+
  theme_minimal()#+
  #theme(axis.text.x = element_text(angle = 45))

ggsave(filename = "fig2.png", width = 10, height=8, dpi="print")  



#figure 5
#  pgrnt_p

dat_all%>%
  filter(sector %in%1:5)%>%
  mutate(sector2 = car::Recode(sector, recodes = "1 ='Public 4 Year';2='Private 4 Year';
  4='Public 2 Year'; 5='Private 2 Year'", as.factor=T), 
         rsicat= factor(ifelse(sc_score<mu, "1Not RSI",
                               ifelse(sc_score>=mu&sc_score<mu+sds,
                                      "2RSI", "3High RSI"))))%>%
  mutate(rsicat =ordered(rsicat,levels = c("1Not RSI", "2RSI", "3High RSI")),
         sector2 = ordered(sector2, levels = c('Public 4 Year','Private 4 Year','Public 2 Year', 'Private 2 Year' )))%>%
  #filter(rsicat %in% c("1Not RSI","2RSI" ))%>%
  group_by(rsicat, sector2)%>%
  summarize(pgrnt_p = mean(pgrnt_p, na.rm=T))%>%
  #mutate(pct = ns/sum(ns))%>%
  #pivot_longer(cols= c(-rsicat, -sector2))%>%  
  #mutate(name2 = factor(ifelse(name =="efug","Undergrad Enrollment", "Graduate Enrollment") ) )%>%
  #mutate(name2 = relevel(name2, ref="Undergrad Enrollment"))%>%
  ggplot()+
  geom_col(aes(x=sector2,y=pgrnt_p,
               group=rsicat,
               fill=rsicat),
           position="dodge")+
  #facet_wrap(~name2, ncol=2)+
  labs(title = "Average Pell Grant Percentage for RSIs and Non-RSIs by Sector",
       caption = "Source: ARRC Compiled Dataset\nCalculations by Corey S. Sparks, Ph.D.",
       x = "Institution Type",
       y = "Percentage")+
  #ylim(c(0, .2))+
  scale_fill_manual( name = "Category",
                     labels=c("Non-RSI", "RSI", "High RSI"),
                     values = c("#00AEEF","#116882","#8DC63F"))+
  # scale_x_discrete(labels=c("1" = "0-999", "2" = "1,000-4,999",
  #                           "3" = "5,000-9,999", "4"="10,000-19,999",
  #                           "5"="20,000+"))+
  # # scale_y_continuous(labels = scales::percent)+
  #guides(fill=guide_legend(title="Race/Ethnicity"))+
  #geom_hline(yintercept = 0, col="red", lwd=1.1)+
  theme_minimal()#+
#theme(axis.text.x = element_text(angle = 45))

ggsave(filename = "fig4.png", width = 10, height=8, dpi="print")  



#figure 7

#  dvef15
dat_all%>%
  # filter(sector %in%1:2)%>%
  mutate(sector2 = car::Recode(sector, recodes = "1 ='Public 4 Year';2='Private 4 Year';
  4='Public 2 Year'; 5='Private 2 Year'", as.factor=T), 
  rsicat= factor(ifelse(sc_score<mu, "1Not RSI",
                        ifelse(sc_score>=mu&sc_score<mu+sds,
                               "2RSI", "3High RSI"))))%>%
  mutate(rrsicat =ordered(rsicat,levels = c("1Not RSI", "2RSI", "3High RSI")),
         sector2 = ordered(sector2, levels = c('Public 4 Year','Private 4 Year','Public 2 Year', 'Private 2 Year' )))%>%
  #         rsi_cat = ifelse(sc_score <mu, "NonRSI", ifelse(sc_score>=mu+sds, "High RSI", "Non_RSI")))%>%
  #filter(rsicat %in% c("1Not RSI","2RSI" ))%>%
  group_by(rsicat, sector2)%>%
  summarize(dvef15 = mean(dvef15 , na.rm=T))%>%
  #mutate(pct = ns/sum(ns))%>%
  #pivot_longer(cols= c(-rsicat, -sector2))%>%  
  #mutate(name2 = factor(ifelse(name =="efug","Undergrad Enrollment", "Graduate Enrollment") ) )%>%
  #mutate(name2 = relevel(name2, ref="Undergrad Enrollment"))%>%
  ggplot()+
  geom_col(aes(x=sector2,y=dvef15 ,
               group=rsicat,
               fill=rsicat),
           position="dodge")+
  #facet_wrap(~name2, ncol=2)+
  labs(title = "Average Adult Student Percentage for RSIs and Non-RSIs",
       caption = "Source: ARRC Compiled Dataset\nCalculations by Corey S. Sparks, Ph.D.",
       x = "Institution Type",
       y = "Percentage")+
  #ylim(c(0, .2))+
  scale_fill_manual( name = "Category",
                     labels=c("Non-RSI", "RSI", "High RSI"),
                     values = c("#00AEEF","#116882","#8DC63F"))+
  # scale_x_discrete(labels=c("1" = "0-999", "2" = "1,000-4,999",
  #                           "3" = "5,000-9,999", "4"="10,000-19,999",
  #                           "5"="20,000+"))+
  # # scale_y_continuous(labels = scales::percent)+
  #guides(fill=guide_legend(title="Race/Ethnicity"))+
  #geom_hline(yintercept = 0, col="red", lwd=1.1)+
  theme_minimal()#+
#theme(axis.text.x = element_text(angle = 45))

ggsave(filename = "fig5.png", width = 10, height=8, dpi="print")  





#Figure 9
#  FTEStaff
dat_all%>%
  # filter(sector %in%1:2)%>%
  mutate(sector2 = car::Recode(sector, recodes = "1 ='Public 4 Year';2='Private 4 Year';
  4='Public 2 Year'; 5='Private 2 Year'", as.factor=T), 
  rsicat= factor(ifelse(sc_score<mu, "1Not RSI",
                        ifelse(sc_score>=mu&sc_score<mu+sds,
                               "2RSI", "3High RSI"))))%>%
  mutate(rrsicat =ordered(rsicat,levels = c("1Not RSI", "2RSI", "3High RSI")),
         sector2 = ordered(sector2, levels = c('Public 4 Year','Private 4 Year','Public 2 Year', 'Private 2 Year' )))%>%
  group_by(rsicat, sector2)%>%
  summarize(FTEStaff = mean(FTEStaff , na.rm=T))%>%
  #mutate(pct = ns/sum(ns))%>%
  #pivot_longer(cols= c(-rsicat, -sector2))%>%  
  #mutate(name2 = factor(ifelse(name =="efug","Undergrad Enrollment", "Graduate Enrollment") ) )%>%
  #mutate(name2 = relevel(name2, ref="Undergrad Enrollment"))%>%
  ggplot()+
  geom_col(aes(x=sector2,y=FTEStaff ,
               group=rsicat,
               fill=rsicat),
           position="dodge")+
  #facet_wrap(~name2, ncol=2)+
  labs(title = "Average Full-Time Equivalent Staff for RSIs and\nNon-RSIs by Sector",
       caption = "Source: ARRC Compiled Dataset\nCalculations by Corey S. Sparks, Ph.D.",
       x = "Institution Type",
       y = "Percentage")+
  #ylim(c(0, .2))+
  scale_fill_manual( name = "Category",
                     labels=c("Non-RSI", "RSI", "High RSI"),
                     values = c("#00AEEF","#116882","#8DC63F"))+
  # scale_x_discrete(labels=c("1" = "0-999", "2" = "1,000-4,999",
  #                           "3" = "5,000-9,999", "4"="10,000-19,999",
  #                           "5"="20,000+"))+
  # # scale_y_continuous(labels = scales::percent)+
  #guides(fill=guide_legend(title="Race/Ethnicity"))+
  #geom_hline(yintercept = 0, col="red", lwd=1.1)+
  theme_minimal()#+
#theme(axis.text.x = element_text(angle = 45))

ggsave(filename = "fig6.png", width = 10, height=8, dpi="print")  




## Figure 11 
# p_stateapprop and p_tuitionrev


dat_all%>%
  mutate(sector2 = car::Recode(sector, recodes = "1 ='Public 4 Year';2='Private 4 Year';
  4='Public 2 Year'; 5='Private 2 Year'", as.factor=T), 
  rsicat= factor(ifelse(sc_score<mu, "1Not RSI",
                        ifelse(sc_score>=mu&sc_score<mu+sds,
                               "2RSI", "3High RSI"))))%>%
  mutate(rrsicat =ordered(rsicat,levels = c("1Not RSI", "2RSI", "3High RSI")),
         sector2 = ordered(sector2, levels = c('Public 4 Year','Private 4 Year','Public 2 Year', 'Private 2 Year' )))%>%
  group_by(rsicat, sector2)%>%
  summarize(p1 = mean(p_stateapprop, na.rm=T),
            p2 = mean(p_tuitionrev, na.rm=T))%>%
  #mutate(pct = ns/sum(ns))%>%
  pivot_longer(cols= c(-rsicat, -sector2))%>%  
  mutate(name2 = factor(ifelse(name =="p1","% State Appropriations", "% Tuition") ) )%>%
  mutate(name2 = relevel(name2, ref="% State Appropriations"))%>%
  ggplot()+
  geom_col(aes(x=sector2,y=value,
               group=rsicat,
               fill=rsicat),
           position="dodge")+
  facet_wrap(~name2, ncol=2)+
  labs(title = "Average Percent of Revenue from State Appropriations\nand Tuition for RSIs and Non-RSIs by Sector",
       caption = "Source: ARRC Compiled Dataset\n Calculations by Corey S. Sparks, Ph.D.",
       x = "Institution Type",
       y = "Mean")+
  #ylim(c(0, .2))+
  scale_fill_manual( name = "Category",
                     labels=c("Non-RSI", "RSI", "High RSI"),
                     values = c("#00AEEF","#116882","#8DC63F"))+
  # scale_x_discrete(labels=c("1" = "0-999", "2" = "1,000-4,999",
  #                           "3" = "5,000-9,999", "4"="10,000-19,999",
  #                           "5"="20,000+"))+
  # # scale_y_continuous(labels = scales::percent)+
  #guides(fill=guide_legend(title="Race/Ethnicity"))+
  #geom_hline(yintercept = 0, col="red", lwd=1.1)+
  theme_minimal()#+
#theme(axis.text.x = element_text(angle = 45))

ggsave(filename = "fig7.png", width = 10, height=8, dpi="print")  





## Figure 13 
#endowmentperFTE

dat_all%>%
  mutate(sector2 = car::Recode(sector, recodes = "1 ='Public 4 Year';2='Private 4 Year';
  4='Public 2 Year'; 5='Private 2 Year'", as.factor=T), 
  rsicat= factor(ifelse(sc_score<mu, "1Not RSI",
                        ifelse(sc_score>=mu&sc_score<mu+sds,
                               "2RSI", "3High RSI"))))%>%
  mutate(rrsicat =ordered(rsicat,levels = c("1Not RSI", "2RSI", "3High RSI")),
         sector2 = ordered(sector2, levels = c('Public 4 Year','Private 4 Year','Public 2 Year', 'Private 2 Year' )))%>%
  group_by(rsicat, sector2)%>%
  summarize(p1 = mean(endowmentperFTE , na.rm=T))%>%
  #mutate(pct = ns/sum(ns))%>%
  #pivot_longer(cols= c(-rsicat, -sector2))%>%  
  #mutate(name2 = factor(ifelse(name =="efug","Undergrad Enrollment", "Graduate Enrollment") ) )%>%
  #mutate(name2 = relevel(name2, ref="Undergrad Enrollment"))%>%
  ggplot()+
  geom_col(aes(x=sector2,y=p1 ,
               group=rsicat,
               fill=rsicat),
           position="dodge")+
  #facet_wrap(~name2, ncol=2)+
  labs(title = "Average Endowment Assets Per FTE Student for RSIs and Non-RSIs by Sector",
       caption = "Source: ARRC Compiled Dataset\nCalculations by Corey S. Sparks, Ph.D.",
       x = "Institution Type",
       y = "Percentage")+
  #ylim(c(0, .2))+
  scale_fill_manual( name = "Category",
                     labels=c("Non-RSI", "RSI", "High RSI"),
                     values = c("#00AEEF","#116882","#8DC63F"))+
  # scale_x_discrete(labels=c("1" = "0-999", "2" = "1,000-4,999",
  #                           "3" = "5,000-9,999", "4"="10,000-19,999",
  #                           "5"="20,000+"))+
  # # scale_y_continuous(labels = scales::percent)+
  #guides(fill=guide_legend(title="Race/Ethnicity"))+
  #geom_hline(yintercept = 0, col="red", lwd=1.1)+
  theme_minimal()#+
#theme(axis.text.x = element_text(angle = 45))

ggsave(filename = "fig8.png", width = 10, height=8, dpi="print")  




## Figure Race/Eth 1
# pctenrwh
# pctenrhs
# pctenran
# pctenrbk
# pctenrap
# pctenr2m


dat_all%>%
  filter(sector %in%1:5)%>%
  mutate(sector2 = car::Recode(sector, recodes = "1 ='Public 4 Year';2='Private 4 Year';
  4='Public 2 Year'; 5='Private 2 Year'", as.factor=T), 
         rsicat= factor(ifelse(sc_score<mu, "Not RSI", "RSI")))%>%
  mutate(rsicat =relevel(rsicat,ref = "Not RSI"),
         sector2 = ordered(sector2, levels = c('Public 4 Year','Private 4 Year','Public 2 Year', 'Private 2 Year' )))%>%
  #         rsi_cat = ifelse(sc_score <mu, "NonRSI", ifelse(sc_score>=mu+sds, "High RSI", "Non_RSI")))%>%
  #filter(rsicat %in% c( "Non-RSI","RSI"))%>%
  group_by(rsicat, sector2)%>%
  summarize(p1 = mean(pctenrwh, na.rm=T),
            p2 = mean(pctenrhs, na.rm=T),
            p3 = mean(pctenrbk, na.rm=T),
            p4 = mean(pctenran, na.rm=T),
            p5 = mean(pctenrap, na.rm=T),
            p6 = mean(pctenr2m, na.rm=T))%>%
  #mutate(pct = ns/sum(ns))%>%
  pivot_longer(cols= c(-rsicat, -sector2))%>%  
  mutate(name2 = stringr::str_wrap(car::Recode(name, recodes = " 'p1'= 'White'; 'p2' = 'Hispanic';
                             'p3' = 'Black';
                             'p4' = 'Native American/AK Native';
                             'p5' = 'Asian/Pacific Islander';
                             'p6'='2 or More Races'", as.factor=T)), width = 10) %>%
  mutate( name2 = ordered(name2,levels = c('White','Hispanic','Black', 'Native American/AK Native', 'Asian/Pacific Islander', '2 or More Races' )))%>%
  mutate(lab = factor(paste(rsicat,sector2,  sep = "\n")))%>%
  mutate(lab = ordered(lab, levels = c("RSI\nPublic 4 Year","Not RSI\nPublic 4 Year","RSI\nPrivate 4 Year" ,"Not RSI\nPrivate 4 Year","Not RSI\nPublic 2 Year","RSI\nPublic 2 Year", "RSI\nPrivate 2 Year", "Not RSI\nPrivate 2 Year"  )))%>%
  ggplot()+
  geom_col(aes(x=lab,
               y=value,
               group=name2,
               fill=name2),
           position="dodge")+
  #facet_wrap(~sector2, ncol=2)+
  labs(title = "Average Enrollment Percentage by Racial Identity for RSIs and Non-RSIs by Sector",
       caption = "Source: ARRC Compiled Dataset\n Calculations by Corey S. Sparks, Ph.D.",
       x = "Institution Type",
       y = "Mean")+
  #ylim(c(0, .2))+
  scale_fill_manual( name = "Race/Ethnicity",
                    # labels=c("Non- RSI", "RSI"),
                     values = c("#00AEEF","#116882","#8DC63F", "black" , "orange", "grey50", "grey80"))+
  # scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  # # scale_y_continuous(labels = scales::percent)+
  #guides(fill=guide_legend(title="Race/Ethnicity"))+
  #geom_hline(yintercept = 0, col="red", lwd=1.1)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))

ggsave(filename = "figR1.png", width = 10, height=8, dpi="print")  


# Figure Race/Eth 2

dat_all%>%
  filter(sector %in%1:5)%>%
  mutate(sector2 = car::Recode(sector, recodes = "1 ='Public 4 Year';2='Private 4 Year';
  4='Public 2 Year'; 5='Private 2 Year'", as.factor=T), 
         rsicat= factor(ifelse(sc_score<mu, "1Non- RSI", 
                               ifelse(sc_score>=mu&sc_score<mu+sds, "RSI", "High RSI"))))%>%
  mutate(rsicat =ordered(rsicat,levels = c("RSI", "High RSI")),
         sector2 = ordered(sector2, levels = c('Public 4 Year','Private 4 Year','Public 2 Year', 'Private 2 Year' )))%>%
  #         rsi_cat = ifelse(sc_score <mu, "NonRSI", ifelse(sc_score>=mu+sds, "High RSI", "Non_RSI")))%>%
  filter(rsicat %in% c("High RSI","RSI" ))%>%
 
  group_by(rsicat, sector2)%>%
  summarize(p1 = mean(pctenrwh, na.rm=T),
            p2 = mean(pctenrhs, na.rm=T),
            p3 = mean(pctenrbk, na.rm=T),
            p4 = mean(pctenran, na.rm=T),
            p5 = mean(pctenrap, na.rm=T),
            p6 = mean(pctenr2m, na.rm=T))%>%
  #mutate(pct = ns/sum(ns))%>%
  pivot_longer(cols= c(-rsicat, -sector2))%>%  
  mutate(name2 = stringr::str_wrap(car::Recode(name, recodes = " 'p1'= 'White'; 'p2' = 'Hispanic';
                             'p3' = 'Black';
                             'p4' = 'Native American/AK Native';
                             'p5' = 'Asian/Pacific Islander';
                             'p6'='2 or More Races'", as.factor=T)), width = 10) %>%
  mutate( name2 = ordered(name2,levels = c('White','Hispanic','Black', 'Native American/AK Native', 'Asian/Pacific Islander', '2 or More Races' )))%>%
  mutate(lab = factor(paste(rsicat,sector2,  sep = "\n")))%>%
  mutate(lab = ordered(lab, levels = c( "RSI\nPublic 4 Year","High RSI\nPublic 4 Year", "RSI\nPrivate 4 Year" ,"High RSI\nPrivate 4 Year","RSI\nPublic 2 Year","High RSI\nPublic 2 Year", "RSI\nPrivate 2 Year" ,"High RSI\nPrivate 2 Year" )))%>%
  ggplot()+
  geom_col(aes(x=lab,y=value,
               group=name2,
               fill=name2),
           position="dodge")+
  #facet_wrap(~sector2+rsicat, ncol=2)+
  labs(title = "Average Enrollment Percentage by Racial Identity for RSIs and High RSIs by Sector",
       caption = "Source: ARRC Compiled Dataset\n Calculations by Corey S. Sparks, Ph.D.",
       x = "Institution Type",
       y = "Mean")+
  #ylim(c(0, .2))+
  scale_fill_manual( name = "Race/Ethnicity",
                     # labels=c("Not RSI", "RSI"),
                     values = c("#00AEEF","#116882","#8DC63F", "black" , "orange", "grey50", "grey80"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12))+
  # # scale_y_continuous(labels = scales::percent)+
  #guides(fill=guide_legend(title="Race/Ethnicity"))+
  #geom_hline(yintercept = 0, col="red", lwd=1.1)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))

ggsave(filename = "figR2.png", width = 10, height=8, dpi="print")  
