###############################################################################
############### Intraspecific trophic niche partitioning ######################
############# Sula dactylatra - Fernando de Noronha, Brazil ###################
###############################################################################
##############Stable Isotopes Analysis - Breeding season ######################
###############################################################################

##### 1. General metrics ####

library(FSA)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(ggsignif)

data.iso <- read.csv(file="dacty_sex_season_year.csv", sep=";", h=T)

iso_breeding <- data.iso[which(data.iso$season=="Breeding"), ]
table(iso_breeding$sex)

C_Br <-Summarize(d_carbon ~ year+sex,
                 data = iso_breeding, digits=2)
C_Br


N_Br <-Summarize(d_nitrogen ~ year+sex,
                 data = iso_breeding, digits=2)
N_Br

ano.ordem <- factor(iso_breeding$year, levels=c("2017", "2018","2019", "2022"))

C1 <- ggplot(data = iso_breeding, aes(x=as.factor(ano.ordem), y= d_carbon, fill=sex)) +
  geom_boxplot() + ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  xlab(expression("")) + theme_bw() + theme(legend.title = element_blank())  + 
  theme(text = element_text(size=10)) + #theme(plot.margin=unit(c(0.5,-0.5,0.5,-0.5), "cm")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = -1.4), 
                                    size= 10, colour="black", face="bold")) +
  #theme(legend.text = element_blank())  +
  scale_fill_manual(values=c("#E1812C", "#3274A1")) +  theme(legend.position="none")  + 
  theme(plot.margin = unit(c(0.5,0.2, 0,0.1), "cm")) + theme(axis.text.x = element_blank())


N1 <- ggplot(data = iso_breeding, aes(x=as.factor(ano.ordem), y= d_nitrogen, fill=sex)) +
  geom_boxplot() + ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  xlab(expression("")) + theme_bw() + theme(legend.title = element_blank())  + 
  theme(text = element_text(size=10)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = -1.4), 
                                    size= 10, colour="black", face="bold")) +  theme(legend.position="none") +
  #theme(legend.text = element_text(colour="black", size=15)) + 
  theme(plot.margin=unit(c(0.2,0.2,0,0.25), "cm"))  +
  scale_fill_manual(values=c("#E1812C", "#3274A1")) +
  geom_signif(
    comparisons = list(c("2017", "2017"), c("2018", "2018"), c("2022", "2022")),
    annotations = c("*", "*", "*"),  # Adicione os símbolos desejados aqui
    y_position = c(11.0, 11.5, 11.5),   # Ajuste conforme necessário
    tip_length = 0,
    textsize = 6,
    vjust = 0.5,
    color = "red" 
  )  +
  geom_segment(aes(x = 0.85, xend = 1.15, y = 11.085, yend = 11.085), color = "red", linewidth = 0.2) +
  geom_segment(aes(x = 1.85, xend = 2.15, y = 11.585, yend = 11.585), color = "red", linewidth = 0.2) +
  geom_segment(aes(x = 3.85, xend = 4.15, y = 11.585, yend = 11.585), color = "red", linewidth = 0.2) 


tiff("metrics_males_females_breeding_tiff_JEMBE.tiff", height=125, width=90, 
     units='mm', compression="lzw", res=1000)

ggarrange(C1, N1, ncol=1, nrow=2, common.legend= TRUE, legend="bottom")

dev.off()

##### 2. Kruskall-Wallis/ Dunn's test ####
#### MALES + FEMALES ####
# CARBON x YEARS
KWC.yearb <- kruskal.test(iso_breeding$d_carbon~iso_breeding$year)
KWC.yearb
#chi-squared = 78.647, df = 3, p-value < 2.2e-16

dunnTest(d_carbon ~ year, data = iso_breeding, method = "bonferroni")
#    Comparison          Z      P.unadj        P.adj
#1 2017 - 2018  5.2934509 1.200295e-07 7.201769e-07 ***
#2 2017 - 2019  0.8373638 4.023881e-01 1.000000e+00 ns
#3 2018 - 2019 -3.0971325 1.954025e-03 1.172415e-02 ***
#4 2017 - 2022  8.3000518 1.040660e-16 6.243959e-16 ***
#5 2018 - 2022  3.7312489 1.905329e-04 1.143197e-03 ***
#6 2019 - 2022  5.7355067 9.722126e-09 5.833275e-08 ***


# NITROGEN X YEARS
KWN.yearb <- kruskal.test(iso_breeding$d_nitrogen~iso_breeding$year)
KWN.yearb
#chi-squared = 42.106, df = 3, p-value = 3.811e-09

dunnTest(d_nitrogen ~ year, data = iso_breeding, method = "bonferroni")
#    Comparison          Z      P.unadj        P.adj
#1 2017 - 2018 -2.9299999 3.389621e-03 2.033773e-02 ***
#2 2017 - 2019 -5.0016617 5.683825e-07 3.410295e-06 ***
#3 2018 - 2019 -2.9021124 3.706555e-03 2.223933e-02 ***
#4 2017 - 2022 -5.5629202 2.652972e-08 1.591783e-07 ***
#5 2018 - 2022 -3.0583755 2.225405e-03 1.335243e-02 ***
#6 2019 - 2022  0.3471472 7.284808e-01 1.000000e+00 ns

#### MALES X FEMALES BY YEAR ####
iso_2022 <- iso_breeding[which(iso_breeding$year=="2022"), ]

kruskal.test(iso_2022$d_carbon~iso_2022$sex)
kruskal.test(iso_2022$d_nitrogen~iso_2022$sex)

#2017
#carbon chi-squared = 0.70097, df = 1, p-value = 0.4025
#nitrogen chi-squared = 4.4286, df = 1, p-value = 0.03534 ***

#2018
# carbon chi-squared = 2.6, df = 1, p-value = 0.1069
# d_nitrogen chi-squared = 11.454, df = 1, p-value = 0.0007134 ***

#2019
# d_carbon chi-squared = 3.4879, df = 1, p-value = 0.06182
# d_nitrogen chi-squared = 0.7978, df = 1, p-value = 0.3718

#2022
# d_carbon chi-squared = 0.73333, df = 1, p-value = 0.3918
# d_nitrogen chi-squared = 8.5283, df = 1, p-value = 0.003497 ***

#### FEMALES BY YEAR #####
iso_female <- iso_breeding[which(iso_breeding$sex=="Female"), ]

kruskal.test(iso_female$d_carbon~iso_female$year)
#d_carbon chi-squared = 30.725, df = 3, p-value = 9.711e-07
dunnTest(d_carbon ~ year, data = iso_female, method = "bonferroni")
#   Comparison         Z      P.unadj        P.adj
#1 2017 - 2018  3.439451 5.828964e-04 3.497378e-03 ***
#2 2017 - 2019  0.288468 7.729885e-01 1.000000e+00 ns
#3 2018 - 2019 -2.806680 5.005490e-03 3.003294e-02 ***
#4 2017 - 2022  4.776624 1.782625e-06 1.069575e-05 ***
#5 2018 - 2022  2.478218 1.320404e-02 7.922423e-02 ns
#6 2019 - 2022  4.263743 2.010304e-05 1.206183e-04 ***

kruskal.test(iso_female$d_nitrogen~iso_female$year)
#d_nitrogen chi-squared = 16.241, df = 3, p-value = 0.001012
dunnTest(d_nitrogen ~ year, data = iso_female, method = "bonferroni")
#    Comparison          Z      P.unadj       P.adj
#1 2017 - 2018 -1.6975148 0.0895993792 0.537596275 ns
#2 2017 - 2019 -2.8922052 0.0038254804 0.022952882 ***
#3 2018 - 2019 -1.7900190 0.0734508613 0.440705168 ns
#4 2017 - 2022 -3.5929616 0.0003269408 0.001961645 ***
#5 2018 - 2022 -2.6297786 0.0085440494 0.051264297 ***
#6 2019 - 2022 -0.7839016 0.4330978289 1.000000000 ns

#### MALES BY YEAR #####
iso_male <- iso_breeding[which(iso_breeding$sex=="Male"), ]

kruskal.test(iso_male$d_carbon~iso_male$year)
#d_carbon  chi-squared = 46.613, df = 3, p-value = 4.201e-10
dunnTest(d_carbon ~ year, data = iso_male, method = "bonferroni")
#    Comparison         Z      P.unadj        P.adj
#1 2017 - 2018  3.828637 1.288549e-04 7.731295e-04 ***
#2 2017 - 2019  1.243317 2.137509e-01 1.000000e+00 ns
#3 2018 - 2019 -1.391878 1.639592e-01 9.837554e-01 ns
#4 2017 - 2022  6.643563 3.061902e-11 1.837141e-10 ***
#5 2018 - 2022  2.361339 1.820905e-02 1.092543e-01 ns
#6 2019 - 2022  3.151142 1.626333e-03 9.757998e-03 ***

kruskal.test(iso_male$d_nitrogen~iso_male$year)
#d_nitrogen chi-squared = 32.754, df = 3, p-value = 3.63e-07
dunnTest(d_nitrogen ~ year, data = iso_male, method = "bonferroni")
#    Comparison          Z      P.unadj        P.adj
#1 2017 - 2018 -1.2430040 2.138664e-01 1.000000e+00 ns
#2 2017 - 2019 -3.8175437 1.347869e-04 8.087216e-04 ***
#3 2018 - 2019 -2.7763235 5.497748e-03 3.298649e-02 ***
#4 2017 - 2022 -4.9799859 6.358890e-07 3.815334e-06 ***
#5 2018 - 2022 -3.2852297 1.018993e-03 6.113957e-03 ***
#6 2019 - 2022  0.4185111 6.755735e-01 1.000000e+00 ns

##### 3. SIBER ####
##### ONLY BREEDING PERIOD x YEARS and SEX  #####
library(dplyr)
library (SIBER)
library(FSA)
library(ggplot2)
library (gridExtra)

iso.breeding.sex <- iso_breeding %>% select(d_carbon, d_nitrogen, year, sex)
colnames(iso.breeding.sex) <- c('iso1', 'iso2', 'group', 'community') 
str(iso.breeding.sex)

SIBER_br <- createSiberObject(iso.breeding.sex)

# C-N points plot

ggplot(iso.breeding.sex, aes(x=iso.breeding.sex$iso1,iso.breeding.sex$iso2, color=as.factor(iso.breeding.sex$group))) + 
  geom_point(aes(shape=iso.breeding.sex$community), size=2) + theme_bw() +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) 

community.hulls.args <- list(col = 2, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")

group.ML <- groupMetricsML(SIBER_br)
print(group.ML)

#      Female.2017 Female.2018 Female.2022 Female.2019  Male.2017  Male.2018  Male.2022  Male.2019
#TA    0.11162850   0.3965477  0.02722990  0.11172879 0.13630519 0.10101765 0.13757879 0.01777255
#SEA   0.05952641   0.1202966  0.02039456  0.08222815 0.04357207 0.04139339 0.04762688 0.01675817
#SEAc  0.06803018   0.1266280  0.02549320  0.09867378 0.04586533 0.04515643 0.05129048 0.02234423


# options for running jags
parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 2        # run this many chains

# define the priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

ellipses.posterior <- siberMVN(SIBER_br, parms, priors)

# calculate the SEA.B for each sex.
SEA.B <- siberEllipses(ellipses.posterior)

colnames(group.ML) <- c("2017", "2018", "2022", "2019", "2017", "2018", "2022", "2019")

dens_br <-siberDensityPlot(SEA.B, xticklabels = colnames(group.ML),
                           xlab = expression(""),
                           ylab = expression("Standard Ellipse Area " ('\u2030' ^2) ),
                           bty = "L",
                           las = 2) 
#with different colors for male and female
my_clrs <- matrix(c("#fff7bc", "#fec44f", "#E1812C",
                    "#fff7bc", "#fec44f", "#E1812C",
                    "#fff7bc", "#fec44f", "#E1812C",
                    "#fff7bc", "#fec44f", "#E1812C",
                    "#deebf7", "#9ecae1", "#3274A1",
                    "#deebf7", "#9ecae1", "#3274A1",
                    "#deebf7", "#9ecae1", "#3274A1",
                    "#deebf7", "#9ecae1", "#3274A1"), nrow = 3, ncol = 8)

# first remove clr=my_clrs to see the order  and then organize and plot with 
dens_dacty2 <-siberDensityPlot(SEA.B, xticklabels = colnames(group.ML), clr=my_clrs,
                               xlab = expression(""),
                               ylab = expression("Standard Ellipse Area " ('\u2030' ^2) ),
                               bty = "L",
                               las = 1) 
dens_dacty2 <-dens_dacty2  + 
  legend("topright", legend=c("Male", "Female"),
         col=c("#3274A1", "#E1812C"), pch=15, bty="n")

#----create-ellipse-df-
# how many of the posterior draws do you want 
n.posts <- 1

# how big an ellipse you want to draw
p.ell <- 0.95

# a list to store the results
all_ellipses <- list()

# loop over groups
for (i in 1:length(ellipses.posterior)){
  
  # a dummy variable to build in the loop
  ell <- NULL
  post.id <- NULL
  
  for ( j in 1:n.posts){
    
    # covariance matrix
    Sigma  <- matrix(ellipses.posterior[[i]][j,1:4], 2, 2)
    
    # mean
    mu     <- ellipses.posterior[[i]][j,5:6]
    
    # ellipse points
    out <- ellipse::ellipse(Sigma, centre = mu , level = p.ell)
    ell <- rbind(ell, out)
    post.id <- c(post.id, rep(j, nrow(out)))}
  
  ell <- as.data.frame(ell)
  ell$rep <- post.id
  all_ellipses[[i]] <- ell}

ellipse_df <- bind_rows(all_ellipses, .id = "id")

# now we need the group and community names
# extract them from the ellipses.posterior list
group_comm_names <- names(ellipses.posterior)[as.numeric(ellipse_df$id)]

# split them and conver to a matrix, NB byrow = T
split_group_comm <- matrix(unlist(strsplit(group_comm_names, "[.]")),
                           nrow(ellipse_df), 2, byrow = TRUE)

ellipse_df$community <- split_group_comm[,1]
ellipse_df$group     <- split_group_comm[,2]

ellipse_df <- dplyr::rename(ellipse_df, iso1 = x, iso2 = y)

# point+ellipse plot ####


# plot de 4 janelas por ano
cbbPalette <- c("#E1812C", "#3274A1")

point.plot <- ggplot(data = iso.breeding.sex, aes(x = iso1, y = iso2)) + 
  geom_point(aes(color = community), size = 1.9) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  labs(colour="") + scale_color_manual(values=cbbPalette) +
  theme_bw() + theme(axis.line = element_line(colour = "black", linewidth=0.5)) 

point.plot <- point.plot + guides(color = guide_legend(override.aes = list(size = 2)))

ellipse.plot <- point.plot + stat_ellipse(aes(group = community, 
                                              fill = community), 
                                          alpha = 0.35, 
                                          level = p.ell,
                                          type = "norm",
                                          geom = "polygon", show.legend = FALSE) + scale_fill_manual(values = c("Male" = "#3182bd",
                                                                                                                "Female" ="#d95f0e"))

second.plot <- ellipse.plot + facet_wrap(~factor(group, levels = c("2017", "2018", "2019", "2022")))

plot.ellipse.years <- second.plot +  theme(text = element_text (size=15)) + 
  theme(strip.text = element_text(size= 13, colour="black", face = "bold")) +
  theme(legend.text = element_text(colour="black", size=12))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 6),
                                    size= 13, colour="black", face="bold")) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = -5, l = 0),
                                    size= 13, colour="black", face="bold"))+ theme(legend.position = "right") +
  ggtitle("(b)") + theme(plot.title = element_text(face = "bold", size = (15)))

#plot de 2 janelas por sexo 
iso.breeding.sex$group = as.character(iso.breeding.sex$group)

point.plot2 <- ggplot(data = iso.breeding.sex, aes(x = iso1, y = iso2)) + 
  geom_point(aes(color = group), size = 1.9) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  labs(colour="") +
  theme_bw() + theme(axis.line = element_line(colour = "black", linewidth=0.5)) 


point.plot2 <- point.plot2 + guides(color = guide_legend(override.aes = list(size = 2)))

ellipse.plot2 <- point.plot2 + stat_ellipse(aes(group = group, 
                                                fill = group), 
                                            alpha = 0.35, 
                                            level = p.ell,
                                            type = "norm",
                                            geom = "polygon", show.legend = FALSE)

second.plot2 <- ellipse.plot2 + facet_wrap(~factor(community, levels = c("Female", "Male")))

plot.ellipse.sex <- second.plot2 +  theme(text = element_text (size=15)) + 
  theme(strip.text = element_text(size= 13, colour="black", face = "bold")) +
  theme(legend.text = element_text(colour="black", size=12))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 6),
                                    size= 13, colour="black", face= "bold")) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = -5, l = 0),
                                    size= 13, colour="black", face= "bold"))+ theme(legend.position = "right") +
  ggtitle("(a)") + theme(plot.title = element_text(face = "bold", size = (15)))


tiff("siber_year_sex_arrange_tiff_JEMBE.tiff", height=225, width=170, 
     units='mm', compression="lzw", res=1000)
grid.arrange (plot.ellipse.sex, plot.ellipse.years, nrow=2)
dev.off()

# overlap area between the ellipses ####
ellipse1 <- "Female.2022"  

ellipse2 <- "Male.2022"
# the overlap between the corresponding 95% prediction ellipses is given by: 
ellipse95.overlap <- maxLikOverlap(ellipse1, ellipse2, SIBER_br,
                                   p.interval = 0.95, n = 100)
ellipse95.overlap


################################################################################
####################### MIXTURE MODELS #########################################
################################################################################

library(vegan)
library(simmr)
library(FSA)
library(ggplot2)
set.seed(1) 

#### PREY METRICS MEAN AND SD ####

prey.data <- read.csv(file="simmr_prey_species_season_year_fork_exoc.csv", sep=";", h=T)
prey.data <- prey.data[1:158,]


box_plot <- ggplot(prey.data, aes(x = species, y = d_carbon)) + geom_boxplot()

######## Prey metrics breeding and non-breeding ####
C_met <-Summarize(d_carbon ~ species,
                  data = prey.data, digits=4)
C_met

N_met <-Summarize(d_nitrogen ~ species,
                  data = prey.data, digits=4)
N_met

prey_species <- C_met$species
N <- C_met$n
C_mean <- C_met$mean
C_sd <- C_met$sd
N_mean <- N_met$mean
N_sd <- N_met$sd


prey_met <- cbind(prey_species, N, C_mean, C_sd, N_mean, N_sd)
prey_met1 <- as.data.frame(prey_met)
str(prey_met1)

prey_met1$C_mean <- as.numeric(prey_met1$C_mean)
prey_met1$C_sd <- as.numeric(prey_met1$C_sd)
prey_met1$N_mean <- as.numeric(prey_met1$N_mean)
prey_met1$N_sd <- as.numeric(prey_met1$N_sd)

prey_met1

#                prey_species  N   C_mean   C_sd  N_mean   N_sd
#1                 Carangidae  1 -17.9100     NA  9.0000     NA #removed from model
#2    Cheilopogon_cyanopterus  4 -17.8103 0.2553  9.7518 1.5479
#3                Exocoetidae 84 -17.7569 0.6132  9.1860 1.1702 #removed from model
#4         Exocoetus_volitans 25 -17.2542 0.4255  9.3314 1.3258
#5         Harengula_clupeola  3 -17.8639 0.0540  8.9583 0.1226
#6              Hemiramphidae  4 -17.5122 0.1196  8.6697 0.6970
#7      Hirundichthys_affinis 24 -17.1259 0.3035  9.0062 0.8341
#8  Oxyporhamphus_micropterus  6 -17.3020 0.4930  7.6567 1.4382
#9    Prognichthys_gibbifrons  2 -17.4690 0.2052 10.2907 0.0243 #removed from model
#10    Selar_crumenophthalmus  1 -17.5492     NA  9.5676     NA #removed from model
#11                     Squid  4 -19.0952 0.6490  8.4753 0.8823 #removed from model


write.csv(prey_met1, "prey_metrics_all.csv", row.names=FALSE)

######Data consumers - FEMALES MODEL ####

data.iso <- read.csv(file="dacty_sex_season_year.csv", sep=";", h=T)
iso_breeding <- data.iso[which(data.iso$season=="Breeding"), ]
iso_females <- iso_breeding[which(iso_breeding$sex=="Female"), ]
table(iso_females$year)

d13C <- iso_females$d_carbon
d15N <- iso_females$d_nitrogen
simmr <- as.matrix(cbind(d13C, d15N))

mix = as.matrix(simmr)

#Groups data - years

years = iso_females$year 
grp = years


#CARREGA DADOS FONTES (s_means= m?dia de cada fonte e s_sds=desvio das fontes. s = sources
prey = read.csv(file="prey_metrics_all.csv", h=T, sep=",")


prey <- prey[-(which(prey$prey_species=="Carangidae")),] #no sd
prey <- prey[-(which(prey$prey_species=="Selar_crumenophthalmus")),] #no sd
prey <- prey[-(which(prey$prey_species=="Squid")),]
prey <- prey[-(which(prey$prey_species=="Exocoetidae")),]
prey <- prey[-(which(prey$prey_species=="Prognichthys_gibbifrons")),]

s_names = prey$prey_species
s_means = as.matrix(cbind(prey$C_mean, prey$N_mean))
s_sds = as.matrix(cbind(prey$C_sd, prey$N_sd))

#### simmr DF  ##

#values of DF from Le Croisier et al (2022) for Leucocarbo bouganvillii (guanay cormorant)

c_means <- matrix(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
                    
                    1.7, 1.7, 1.7, 1.7,1.7, 1.7), ncol=2, nrow=6)

c_sds <- matrix(c(0.3, 0.3, 0.3, 0.3, 0.3, 0.3,    
                  0.6, 0.6, 0.6, 0.6,0.6, 0.6), ncol=2, nrow=6)


##### simmr model ###
simmr_groups <- simmr_load(mixtures=mix,
                           source_names=s_names,
                           source_means=s_means,
                           source_sds=s_sds,
                           correction_means=c_means,
                           correction_sds=c_sds,
                           group=as.factor(paste('consumidor', grp)))


#GRAFICA OS DADOS: BIPLOT

simmr_plot_allprey <- plot(simmr_groups,group=1:4,xlab=expression(paste(delta^13, "C (\u2030)",sep="")),ylab=expression(paste(delta^15, "N (\u2030)",sep="")), 
                           title='Females',mix_name = NULL)

simmr_out <- simmr_mcmc(simmr_groups)

# Step 4: Checking the algorithm converged
summary(simmr_out, type = 'diagnostics')

#Step 5: Checking the model fit
post_pred = posterior_predictive(simmr_out)

print(post_pred)

#Step 6: Exploring the results

summary(simmr_out,type='statistics', group=c(1:4))
plot(simmr_out, type = 'matrix')

plot(simmr_out,
     type = "boxplot", group= 1,
     title = "F2017")

plot(simmr_out,
     type = "boxplot", group= 2,
     title = "F2018")

plot(simmr_out,
     type = "boxplot", group= 3,
     title = "F2019")

plot(simmr_out,
     type = "boxplot", group= 4,
     title = "F2022")

#### Sankey Diagram###

install.packages("networkD3")
library(networkD3)


links <- data.frame(
  source=c("2017","2017", "2017", "2017", "2017", "2017", 
           "2018","2018", "2018", "2018", "2018", "2018", 
           "2019","2019", "2019", "2019", "2019", "2019",
           "2022", "2022", "2022", "2022", "2022", "2022"), 
  target=c("C.cyanopterus","E.volitans", "H.affinis", "O. micropterus", "H.clupeola", "Hemiramphidae", 
           "C.cyanopterus","E.volitans", "H.affinis", "O. micropterus", "H.clupeola", "Hemiramphidae", 
           "C.cyanopterus","E.volitans", "H.affinis", "O. micropterus", "H.clupeola", "Hemiramphidae", 
           "C.cyanopterus","E.volitans",  "H.affinis","O. micropterus", "H.clupeola", "Hemiramphidae"), 
  value=c(8.6, 15.7, 30.7, 14.8, 12.7, 17.5, 
          10.3, 14.2, 25.1, 8.9, 20.6, 20.9,
          11.8, 19.9, 28.4, 12.1, 12.2, 15.6,
          18.7, 14.0, 13.5, 10.2, 27.9, 15.7))

nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Add a 'group' column to the nodes data frame:
nodes$group <- as.factor(c("a","b","c","d","e","e","e","e","e","e")) #group of nodes a= 2017, b=2018, c= 2019, d=2022, e=prey (total=6)

links$group <- as.factor(c("type_a","type_a","type_a","type_a","type_a","type_a",
                           "type_b","type_b","type_b","type_b","type_b","type_b",
                           "type_c","type_c","type_c","type_c","type_c","type_c",
                           "type_d","type_d","type_d","type_d","type_d","type_d")) #4 types of conections 2017, 2018,2019  e 2020 each have 6 conections


# Give a color for each group:
my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "type_c","type_d", "a", "b", "c", "d", "e"]) 
            .range(["#FCA6A0", "#A7B97A", "#B0E2FF", "#EEAEEE", "#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "#CDC9C9"])'


g <-sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "IDsource", Target = "IDtarget",
                  Value = "value", NodeID = "name", 
                  colourScale=my_color, NodeGroup="group", LinkGroup = "group",
                  fontSize= 18, fontFamily = "Helvetica", nodeWidth = 20, iterations = 0)

saveNetwork(g, "Sankey_females_new.html")

library(webshot2)
webshot2::webshot("Sankey_females_new.html", 
                  file = "Sankey_males_dpi_new.jpg", 
                  vwidth = 100, vheight = 100, 
                  zoom = 10)  

######Data consumers - MALES MODEL ####

data.iso <- read.csv(file="dacty_sex_season_year.csv", sep=";", h=T)
iso_breeding <- data.iso[which(data.iso$season=="Breeding"), ]
iso_males <- iso_breeding[which(iso_breeding$sex=="Male"), ]
table(iso_males$year)

d13C <- iso_males$d_carbon
d15N <- iso_males$d_nitrogen
simmr <- as.matrix(cbind(d13C, d15N))

mix = as.matrix(simmr)

#Groups data - years

years = iso_males$year 
grp = years

#CARREGA DADOS FONTES (s_means= m?dia de cada fonte e s_sds=desvio das fontes. s = sources
prey = read.csv(file="prey_metrics_all.csv", h=T, sep=",")


prey <- prey[-(which(prey$prey_species=="Carangidae")),] #no sd
prey <- prey[-(which(prey$prey_species=="Selar_crumenophthalmus")),] #no sd
prey <- prey[-(which(prey$prey_species=="Squid")),]
prey <- prey[-(which(prey$prey_species=="Exocoetidae")),]
prey <- prey[-(which(prey$prey_species=="Prognichthys_gibbifrons")),]


s_names = prey$prey_species
s_means = as.matrix(cbind(prey$C_mean, prey$N_mean))
s_sds = as.matrix(cbind(prey$C_sd, prey$N_sd))

#### simmr DF  ##

#values of DF from Le Croisier et al (2022) for Leucocarbo bouganvillii (guanay cormorant)

c_means <- matrix(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
                    
                    1.7, 1.7, 1.7, 1.7,1.7,1.7), ncol=2, nrow=6)

c_sds <- matrix(c(0.3, 0.3, 0.3, 0.3, 0.3, 0.3,     
                  0.6, 0.6, 0.6, 0.6,0.6, 0.6), ncol=2, nrow=6)


##### simmr model ###
simmr_groups <- simmr_load(mixtures=mix,
                           source_names=s_names,
                           source_means=s_means,
                           source_sds=s_sds,
                           correction_means=c_means,
                           correction_sds=c_sds,
                           group=as.factor(paste('consumidor', grp)))


#GRAFICA OS DADOS: BIPLOT

simmr_plot_allprey <- plot(simmr_groups,group=1:4,xlab=expression(paste(delta^13, "C (\u2030)",sep="")),ylab=expression(paste(delta^15, "N (\u2030)",sep="")), 
                           title='Males',mix_name = NULL)

simmr_out <- simmr_mcmc(simmr_groups)

# Step 4: Checking the algorithm converged
summary(simmr_out, type = 'diagnostics')

#Step 5: Checking the model fit
post_pred = posterior_predictive(simmr_out)

print(post_pred)

#Step 6: Exploring the results

summary(simmr_out,type='statistics', group=c(1:4))
plot(simmr_out, type = 'matrix')

plot(simmr_out,
     type = "boxplot", group= 1,
     title = "M2017")

plot(simmr_out,
     type = "boxplot", group= 2,
     title = "M2018")

plot(simmr_out,
     type = "boxplot", group= 3,
     title = "M2019")

plot(simmr_out,
     type = "boxplot", group= 4,
     title = "M2022")

#### Sankey Diagram###

install.packages("networkD3")
library(networkD3)


links <- data.frame(
  source=c("2017","2017", "2017", "2017", "2017", "2017", 
           "2018","2018", "2018", "2018", "2018", "2018", 
           "2019","2019", "2019", "2019", "2019", "2019",
           "2022", "2022", "2022", "2022", "2022", "2022"), 
  target=c("C.cyanopterus","E.volitans", "H.affinis", "O. micropterus", "H.clupeola", "Hemiramphidae", 
           "C.cyanopterus","E.volitans", "H.affinis", "O. micropterus", "H.clupeola", "Hemiramphidae", 
           "C.cyanopterus","E.volitans", "H.affinis", "O. micropterus", "H.clupeola", "Hemiramphidae", 
           "C.cyanopterus","E.volitans",  "H.affinis","O. micropterus", "H.clupeola", "Hemiramphidae"), 
  value=c(4.9, 13.8, 40.7, 17.6, 6.7, 16.3, 
          9.2, 10.4, 16.0, 12.5, 28.4, 23.6,
          13.3, 17.5, 22.3, 13.6, 15.9, 17.4,
          9.9, 6.5, 6.8, 5.1, 61.4, 10.3))

nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Add a 'group' column to the nodes data frame:
nodes$group <- as.factor(c("a","b","c","d","e","e","e","e","e","e")) #group of nodes a= 2017, b=2018, c= 2019, d=2022, e=prey (total=6)

links$group <- as.factor(c("type_a","type_a","type_a","type_a","type_a","type_a",
                           "type_b","type_b","type_b","type_b","type_b","type_b",
                           "type_c","type_c","type_c","type_c","type_c","type_c",
                           "type_d","type_d","type_d","type_d","type_d","type_d")) #4 types of conections 2017, 2018,2019  e 2020 each have 6 conections


# Give a color for each group:
my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "type_c","type_d", "a", "b", "c", "d", "e"]) 
            .range(["#FCA6A0", "#A7B97A", "#B0E2FF", "#EEAEEE", "#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "#CDC9C9"])'


h <-sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "IDsource", Target = "IDtarget",
                  Value = "value", NodeID = "name", 
                  colourScale=my_color, NodeGroup="group", LinkGroup = "group",
                  fontSize= 18, fontFamily = "Helvetica", nodeWidth = 20, iterations = 0)

saveNetwork(h, "Sankey_males_new.html")

library(webshot2)
webshot2::webshot("Sankey_males_new.html", 
                  file = "Sankey_males_dpi_new.jpg", 
                  vwidth = 100, vheight = 100, 
                  zoom = 10)  

