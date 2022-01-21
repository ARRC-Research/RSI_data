library(tidyverse)
library(ggplot2)

dat<- read_csv("data/MapData11_30_correct_final.csv")
remove_id<- c(372958,
              100690,
              128780,
              196680,
              433387,
              437325,
              444130,
              446163,
              454582,
              460783,
              465812,
              474863,
              476975,
              480569,
              484473,
              485908,
              488846,
              491288)
dat_sub <- dat%>%
  filter(!unitid%in%remove_id)%>%
  select(POPPCT_RURAL, C_M_POPPCT_RURAL, newRUCC_w_o_adj, RUCC_adj_reverse, p_AgResRec)%>%
  mutate_all(.funs = scale, center = T, scale=T  )



fa<- factanal(dat_sub, factors =1, scores = "regression" )
#fa2<-psych::fa(dat_sub, nfactors = 1, scores = "regression")
dat_sub$score <- fa$scores

# re - scaling to 0-4
#maxAllowed - minAllowed) * (unscaledNum - min) / (max - min) + minAllowed

dat_sub$sc_score <- (4 - 0) * (dat_sub$score - min(dat_sub$score))/(max(dat_sub$score)-min(dat_sub$score)) + 0

# dat$sc_scorea <- (4 - 0) * (dat$RSI_Rescaled - min(dat$RSI_Rescaled))/(max(dat$RSI_Rescaled)-min(dat$RSI_Rescaled)) + 0


mu<-mean(dat_sub$sc_score)
sds <- sd(dat_sub$sc_score)
coff<- mu+sds
coff

dat<-dat%>%
  filter(!unitid %in% remove_id)

dat_all<-cbind(dat, dat_sub[, "sc_score"])

dat_all<-dat_all[, c(-83, -63)]

saveRDS(dat_all, file = "data/index_calc.rds")
write.csv(dat_all, file = "data/index_calc.csv")
haven::write_dta(dat_all,path = "data/index_calc.dta")
