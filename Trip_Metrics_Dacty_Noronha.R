library(tidyverse)
require(lmerTest)
require(visreg)
library(lattice)
library(glmmTMB)
library(lme4)
library(lmerTest)
library(emmeans)
library(pbkrtest)

data <- read.csv("trip_metadata.csv", header=T, sep=",") 

#Adjust data
data <- data %>% 
  filter(!is.na(distance)) %>%
  filter(!is.na(range_max)) %>%
  filter(!is.na(straightness_index)) %>%
  mutate(sex = as.factor(sex)) %>%
  mutate(straightness_index = as.numeric(straightness_index)) %>%
  mutate(year = as.factor(year))

summary(data)

T17 <- data[which(data$year=="2017"), ]
T18 <- data[which(data$year=="2018"), ]
T19 <- data[which(data$year=="2019"), ]
T22 <- data[which(data$year=="2022"), ]

########## Linear Mixed Models
########## Distance
model_d <- lmer(distance ~ sex + (1|id), data = T22) 
summary(model_d)
anova(model_d)
#Analysis between years
model_d <- lmer(distance ~ year + (1|id), data = data) 
summary(model_d)
anova(model_d)
emm <- emmeans(model_d, pairwise ~ year, adjust = "tukey")
summary(emm)


########## Maximum Range 
model_rmax <- lmer(range_max ~ sex + (1|id), data = T22) 
summary(model_rmax)
anova(model_rmax)
#Analysis between years
model_rmax <- lmer(range_max ~ year + (1|id), data = data) 
summary(model_rmax)
anova(model_rmax)
emm <- emmeans(model_rmax, pairwise ~ year, adjust = "tukey")
summary(emm)


########## Duration
model_dur <- lmer(duration ~ sex + (1|id), data = T22) 
summary(model_dur)
anova(model_dur)
#Analysis between years
model_dur <- lmer(duration ~ year + (1|id), data = data) 
summary(model_dur)
anova(model_dur)
emm <- emmeans(model_dur, pairwise ~ year, adjust = "tukey")
summary(emm)



########## Generalized Mixed Models
########## Straightness Index 
model_s <- glmmTMB(straightness_index ~ sex + (1|id), family = beta_family(link = "logit"), data = T17)
summary(model_s)
#Analysis between years
model_s <- glmmTMB(straightness_index ~ year + (1|id), family = beta_family(link = "logit"), data = data)
summary(model_s)
emm <- emmeans(model_s, pairwise ~ year, adjust = "tukey")
summary(emm)



########## Proportion of time diving
model_p <- glmmTMB(prop_time_diving ~ sex + (1 | id), family = beta_family(link = "logit"), ziformula = ~ 1, data = data)  
summary(model_p)
# Analysis between years
model_p <- glmmTMB(prop_time_diving ~ year + (1 | id), family = beta_family(link = "logit"), ziformula = ~ 1, data = data)
summary(model_p)
emm <- emmeans(model_, pairwise ~ year, adjust = "tukey")
summary(emm)


##### Trip metrics Graphics ######

library(FSA)
library(ggplot2)
library(ggpubr)

trip <- read.csv(file="trip_metadata.csv", sep=",", h=T)


#### MALES X FEMALES
Dtot <- ggplot(trip, aes(x=as.factor(sex), y=distance, fill=sex)) + 
  geom_boxplot() +  theme_bw() +
  facet_wrap(~year, ncol = 4) +
  ylab("Dtot (km)") + xlab(element_blank()) + theme(legend.position = "none") +
  theme(text = element_text(size=12)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), 
                                    size= 15, colour="black", face="bold")) + 
  scale_fill_manual(values=c("#E1812C", "#3274A1"))  + 
  theme(strip.text.x =element_text(size=15, face="bold")) +
  theme(axis.text.x=element_blank()) + scale_y_continuous(breaks = seq(0, 500, by=200), limits=c(0,500))

Dmax <- ggplot(trip, aes(x=as.factor(sex), y=range_max, fill=sex)) + 
  geom_boxplot() +  theme_bw() +
  facet_wrap(~year, ncol = 4) +
  ylab("Dmax (km)") + xlab(element_blank()) + theme(legend.position = "none") +
  theme(text = element_text(size=12)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), 
                                    size= 15, colour="black", face="bold")) + 
  scale_fill_manual(values=c("#E1812C", "#3274A1"))  + 
  theme(strip.text.x =element_blank()) + theme(strip.background = element_blank()) +
  theme(axis.text.x=element_blank())

Tdur <- ggplot(trip, aes(x=as.factor(sex), y=duration, fill=sex)) + 
  geom_boxplot() +  theme_bw() +
  facet_wrap(~year, ncol = 4) +
  ylab("Tdur (min)") + xlab(element_blank()) + theme(legend.position = "none") +
  theme(text = element_text(size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), 
                                    size= 15, colour="black", face="bold")) + 
  scale_fill_manual(values=c("#E1812C", "#3274A1"))  + 
  theme(strip.text.x =element_blank()) + theme(strip.background = element_blank()) +
  theme(axis.text.x=element_blank()) + scale_y_continuous(breaks = seq(0, 1200, by=200), limits=c(0,1200))

Pdiv <- ggplot(trip, aes(x=as.factor(sex), y=prop_time_diving, fill=sex)) + 
  geom_boxplot() +  theme_bw() +
  facet_wrap(~year, ncol = 4) +
  ylab("Pdiv (%)") + xlab(element_blank()) + theme(legend.position = "none") +
  theme(text = element_text(size=12)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), 
                                    size= 15, colour="black", face="bold")) + 
  scale_fill_manual(values=c("#E1812C", "#3274A1"), labels=c("Female", "Male"))  + 
  theme(strip.text.x =element_blank()) + theme(strip.background = element_blank()) +
  theme(axis.text.x=element_blank()) + theme(legend.title=element_blank()) +
  theme(legend.text = element_text(colour="black", size=18))

SI <- ggplot(trip, aes(x=as.factor(sex), y=straightness_index, fill=sex)) + 
  geom_boxplot() +  theme_bw() +
  facet_wrap(~year, ncol = 4) +
  ylab("SI") + xlab(element_blank()) + theme(legend.position = "bottom") +
  theme(text = element_text(size=12)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), 
                                    size= 15, colour="black", face="bold")) + 
  scale_fill_manual(values=c("#E1812C", "#3274A1"), labels=c("Female", "Male"))  + 
  theme(strip.text.x =element_blank()) + theme(strip.background = element_blank()) +
  theme(axis.text.x=element_blank()) + theme(legend.title=element_blank()) +
  theme(legend.text = element_text(colour="black", size=18))

legend<-get_legend(SI, position = NULL)

tiff("trip_metrics_tiff_jembe.tiff", height=225, width=170, 
     units='mm', compression="lzw", res=1000)

ggarrange(Dtot, Dmax, Tdur, Pdiv, SI, ncol=1, nrow=5, common.legend= TRUE, legend="bottom", legend.grob = legend)

dev.off()

#### MALES + FEMALES BY YEAR
str(trip)
trip$year <- as.character(trip$year)

Dtot2 <- ggplot(trip, aes(x=as.factor(year), y=distance, fill=year)) + 
  geom_boxplot() +  theme_bw() +
  ylab("Dtot (km)") + xlab(element_blank()) + theme(legend.position = "none") +
  theme(text = element_text(size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), 
                                    size= 15, colour="black", face="bold")) + 
  theme(axis.text.x=element_blank()) + scale_fill_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")) +
  scale_y_continuous(breaks = seq(0, 500, by=200), limits=c(0,500)) +
  geom_signif(
    comparisons = list(c("2017", "2022"), c("2018", "2022"), c("2019", "2022")),
    annotations = c("", "", "         ***"),  # Adicione os símbolos desejados aqui
    y_position = c(420, 420, 420),   # Ajuste conforme necessário
    tip_length = 0.01,
    textsize = 6,
    vjust = 0.5,
    color = "red" 
  )

Dmax2 <- ggplot(trip, aes(x=as.factor(year), y=range_max, fill=year)) + 
  geom_boxplot() +  theme_bw() +
  ylab("Dmax (km)") + xlab(element_blank()) + theme(legend.position = "none") +
  theme(text = element_text(size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), 
                                    size= 15, colour="black", face="bold")) + 
  scale_fill_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"))  + 
  theme(strip.text.x =element_blank()) +  theme(axis.text.x=element_blank()) + 
  scale_y_continuous(breaks = seq(0, 250, by=50), limits=c(0,250)) +
  geom_signif(
    comparisons = list(c("2017", "2022"), c("2018", "2022"), c("2019", "2022")),
    annotations = c("", "", "         ***"),  # Adicione os símbolos desejados aqui
    y_position = c(220, 220, 220),   # Ajuste conforme necessário
    tip_length = 0.01,
    textsize = 6,
    vjust = 0.5,
    color = "red" 
  )


Tdur2 <- ggplot(trip, aes(x=as.factor(year), y=duration, fill=year)) + 
  geom_boxplot() +  theme_bw() +
  ylab("Tdur (min)") + xlab(element_blank()) + theme(legend.position = "none") +
  theme(text = element_text(size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), 
                                    size= 15, colour="black", face="bold")) + 
  scale_fill_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"))  + 
  theme(strip.text.x =element_blank()) + theme(strip.background = element_blank()) +
  theme(axis.text.x=element_blank()) + scale_y_continuous(breaks = seq(0, 2000, by=400), limits=c(0,2100)) +
  geom_signif(
    comparisons = list(c("2018", "2022"), c("2019", "2022")),
    annotations = c("*", "*"),  # Adicione os símbolos desejados aqui
    y_position = c(1700, 1900),   # Ajuste conforme necessário
    tip_length = 0.01,
    textsize = 6,
    vjust = 0.5,
    color = "red" 
  )

Pdiv2 <- ggplot(trip, aes(x=as.factor(year), y=prop_time_diving, fill=year)) + 
  geom_boxplot() +  theme_bw() +
  ylab("Pdiv (%)") + xlab(element_blank()) + theme(legend.position = "none") +
  theme(text = element_text(size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), 
                                    size= 15, colour="black", face="bold")) + 
  scale_fill_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"))  + 
  theme(strip.text.x =element_blank()) +  theme(axis.text.x=element_blank()) + scale_y_continuous(limits=c(0,0.05)) +
  geom_signif(
    comparisons = list(c("2017", "2019"), c("2019", "2022")),
    annotations = c("*", "*"),  # Adicione os símbolos desejados aqui
    y_position = c(0.042, 0.045),   # Ajuste conforme necessário
    tip_length = 0.01,
    textsize = 6,
    vjust = 0.5,
    color = "red")

SI2 <- ggplot(trip, aes(x=as.factor(year), y=straightness_index, fill=year)) + 
  geom_boxplot() +  theme_bw() +
  ylab("SI") + xlab(element_blank()) + theme(legend.position = "bottom") +
  theme(text = element_text(size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), 
                                    size= 15, colour="black", face="bold")) + 
  scale_fill_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"), labels=c("2017", "2018", "2019", "2022"))  + 
  theme(strip.text.x =element_blank()) + theme(strip.background = element_blank()) +
  theme(axis.text.x=element_blank()) + theme(legend.title=element_blank()) +
  theme(legend.text = element_text(colour="black", size=16)) +  scale_y_continuous(limits=c(0,1.3)) +
  geom_signif(
    comparisons = list(c("2018", "2022"), c("2019", "2022")),
    annotations = c("*", "*"),  # Adicione os símbolos desejados aqui
    y_position = c(1.0, 1.15),   # Ajuste conforme necessário
    tip_length = 0.01,
    textsize = 6,
    vjust = 0.5,
    color = "red")

legend2<-get_legend(SI2, position = NULL)

tiff("trip_metrics_allyears_tiff_JEMBE.tiff", height=180, width=170, 
     units='mm', compression="lzw", res=1000)

ggarrange(Dtot2, Dmax2, Tdur2, Pdiv2, SI2, ncol=2, nrow=3, 
          common.legend= TRUE, legend="bottom", legend.grob = legend2)
dev.off()
