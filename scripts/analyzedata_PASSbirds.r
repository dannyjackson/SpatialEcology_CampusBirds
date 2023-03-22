#!/usr/bin/env Rscript

# https://rpubs.com/an-bui/vegan-cheat-sheet
# https://rpubs.com/Roeland-KINDT/706490


R
setwd("/Users/danjack/Documents/BirdSurveys_CAPLTER/")
library(tidyverse)
library(vegan)
library(ggvegan)


birds <- read_csv("BirdSurveys_pivoted.csv") %>%
  column_to_rownames("site_code")

env <- read_csv("sitetype.csv")


env_subset <- subset(env, site_type == 'riparian')
env_subset_list <- as.list(env_subset["site"])


birds_subset <- birds[, sapply(birds, function(col) length(unique(col))) > 3]


# Only bird species present in more than one site(5% of total sites) were included in the analysis;rare species were omitted due to their dispropor-tionate weight (McCune and Grace 2002, McGari-gal et al. 2013).

sapply(birds, function(col) length(unique(col)))

birds_dropped <- sapply(birds, function(col) length(unique(col))) < 3

birds_dropped <- birds[, sapply(birds, function(col) length(unique(col))) < 3]

birds <- birds[, sapply(birds, function(col) length(unique(col))) > 3]

# Bird abundance was square root transformed and standardized using a row relativization prior to analysis (Legendre and Gallagher 2001).

sppr <- specnumber(birds)
sppr_sqrt <- sqrt(sppr)
sum(sppr_sqrt)
sppr_sqrt_std <- sppr_sqrt / sum(sppr_sqrt)


env <- read_csv("sitetype_noriparian.csv")

sitetype <- env %>%
  select(site, site_type_season)

# How speciose are my communities?
sppr <- specnumber(birds)

# not applicable because we don't have equal site sample sizes across categories. Could compare years/seasons for PASS data though
# sppr_aov <- aov(sppr ~ site_type, data = sitetype)
# summary(sppr_aov)

sppr_df <- sppr %>%
  enframe() %>%
  full_join(sitetype, by = c("name" = "site"))



plot_sppr <- ggplot(sppr_df, aes(x = site_type_season, y = value, fill = site_type_season)) +
  geom_boxplot() +
  theme(legend.position = "none",
        plot.background = element_rect("white"),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey90"),
        axis.line = element_line("gray25"),
        axis.text = element_text(size = 10, color = "gray25"),
        axis.title = element_text(color = "gray25"),
        legend.text = element_text(size = 10)) +
  labs(x = "Survey type",
       y = "Number of species per site",
       title = "Species richness")

pdf(file = "Results/plot_sppr.pdf", width = 10, height = 10, useDingbats=FALSE)
 plot_sppr
   dev.off()






sitetype <- env %>%
  select(site, site_type_season)

# How speciose are my communities?
sppr <- specnumber(birds)

# not applicable because we don't have equal site sample sizes across categories. Could compare years/seasons for PASS data though
# sppr_aov <- aov(sppr ~ site_type_season, data = sitetype)
# summary(sppr_aov)

sppr_df <- sppr %>%
  enframe() %>%
  full_join(sitetype, by = c("name" = "site"))




  # How diverse are my communities?

  shannondiv <- diversity(birds)
  head(shannondiv)

# sppdiv_aov <- aov(shannondiv ~ site_type_season, data = sitetype)
# summary(sppdiv_aov)


shandiv_df <- shannondiv %>%
  enframe() %>%
  rename(site = name,
       shan_div = value)


div_plot_df <- shandiv_df %>%
  full_join(sitetype, ., by = "site") %>%
  group_by(site_type_season) %>%
  summarize(mean = round(mean(shan_div), 2),
            err = sd(shan_div)/sqrt(length(shan_div))) %>%
  dplyr::mutate(label = "mean") %>%
  unite("mean_label", label, mean, sep = " = ", remove = FALSE)


clean_background <- theme(plot.background = element_rect("white"),
        panel.background = element_rect("white"),
        panel.grid = element_line("white"),
        axis.line = element_line("gray25"),
        axis.text = element_text(size = 12, color = "gray25"),
        axis.title = element_text(color = "gray25"),
        legend.text = element_text(size = 12),
        legend.key = element_rect("white"))

plot_shandiv <- ggplot(div_plot_df, aes(x = site_type_season, y = mean, fill = site_type_season)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = mean - err, ymax = mean + err), width = 0.5) +
  geom_text(aes(x = site_type_season, y = mean + err + 0.07, label = mean_label)) +
  scale_y_continuous(limits = c(0, 2.75), expand = c(0,0)) +
  clean_background +
  theme(legend.position = "none") +
  labs(x = "Survey type",
       y = "Mean Shannon diversity",
       title = "Shannon diversity")

 pdf(file = "Results/shannondiv.pdf", width = 10, height = 10, useDingbats=FALSE)
  plot_shandiv
    dev.off()



# How different are my communities in species composition? Permutational Multivariate Analysis of Variance (perMANOVA)

bird_perm <- adonis(birds ~ site, data = env)
bird_perm


# Principal Components Analysis (PCA)


birdPCA <- rda(birds)
birdPCA

PCAscores <- scores(birdPCA, display = "sites") %>%
  as.data.frame() %>%
  rownames_to_column("site") %>%
  full_join(sitetype, by = "site")

PCAvect <- scores(birdPCA, display = "species") %>%
  as.data.frame()

plot_PCA <- ggplot() +
  geom_point(data = PCAscores, aes(x = PC1, y = PC2, color = site_type_season)) +
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  geom_segment(data = PCAvect, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.2, "cm"))) +
  geom_text(data = PCAvect, aes(x = PC1, y = PC2, label = rownames(PCAvect))) +
  clean_background +
  labs(x = "PC1 (23.57%)",
       y = "PC2 (12.23%)",
       title = "Principal Components Analysis")

 pdf(file = "Results/PCA.pdf", width = 10, height = 10, useDingbats=FALSE)
  plot_PCA
    dev.off()



# There’s a lot to look at here, but I would pay most attention to the arrows and their direction. In this ordination, points are sites and arrows are species. Length of the arrow indicates the amount of variation in your communities explained by that particular variable (longer arrows -> larger increase) and the angle of the arrows to each other indicates correlations (the more obtuse the angle, the less correlated).




bird_NMDS <- metaMDS(birds)


bird_NMDS

pdf(file = "Results/stressplot_bird_NMDS.pdf", width = 10, height = 10, useDingbats=FALSE)
 stressplot(bird_NMDS)
   dev.off()


pdf(file = "Results/bird_NMDS.pdf", width = 10, height = 10, useDingbats=FALSE)
  plot(bird_NMDS)
    dev.off()



plot_df <- scores(bird_NMDS) %>%
  as.data.frame() %>%
  rownames_to_column("site")

plot_df <- merge(plot_df,env, by = "site")

# + geom_text(hjust=0, vjust=0) +
# "NMDS of 2008, 2012, and 2019 (Winter and Spring surveys) and of 2000-2005, 2022 August surveys"

plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, label = site)) +
  geom_point(size = 3, alpha = 0.8, aes(colour = site_type_season, shape = site_type_season)) +
  labs(title = "NMDS")

pdf(file = "Results/bird_NMDS.pdf", width = 10, height = 10, useDingbats=FALSE)
  plot_nmds
    dev.off()





# How do species contribute to the dissimilarity of communities?

fit <- envfit(bird_NMDS, birds, perm = 999)

fit_pvals <- fit$vectors$pvals %>%
  as.data.frame() %>%
  rownames_to_column("species") %>%
  dplyr::rename("pvals" = ".")

fit_spp <- fit %>%
  scores(., display = "vectors") %>%
  as.data.frame() %>%
  rownames_to_column("species") %>%
  full_join(., fit_pvals, by = "species") %>%
  filter(pvals == 0.001)

nmds_plot_new <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
  coord_fixed() +
  geom_point(aes(color = site_type_season, shape = site_type_season), size = 3, alpha = 0.8) +
  geom_segment(data = fit_spp, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               col = "black") +
  geom_text(data = fit_spp, aes(label = species)) +
  clean_background

pdf(file = "Results/nmds_plot_new.pdf", width = 10, height = 10, useDingbats=FALSE)
  nmds_plot_new
    dev.off()

# How is community structure related to specific environmental variables?
# remove birds that are missing values
env_onlylandscaping <-env[complete.cases(env), ]
env_remove <- env[!complete.cases(env), ]
getouttahere <- as.list(env_remove[,1])
getouttahere2 <- getouttahere[[1]]

birds2 <- birds[!(row.names(birds) %in% getouttahere2),]
fullsites <- as.list(env["site"])

test <- row.names(birds2)
subset(env, ! site %in% test)


notmissing <- as.list(row.names(birds2))
env_subset <- subset(env_onlylandscaping, site %in% notmissing)

birdCCA <- cca(birds2 ~ Mesic + Oasis + Xeric, data = env_subset)
birdCCA
pdf(file = "Results/birdCCA.pdf", width = 10, height = 10, useDingbats=FALSE)
  plot(birdCCA)
    dev.off()


# Can do this in GGplot and color code the Gammage sites but don't worry about that until you have the proper landscape environmental data






# vectors
ccavectors <- as.matrix(scores(birdCCA, display = "bp", scaling = "species")*7.627807) %>%
  as.data.frame()

# site coordinates
site_data <- scores(birdCCA, display = "sites") %>%
  as.data.frame() %>%
  rownames_to_column("site") %>%
  full_join(env_subset, by = "site")

# species coordinates
species_data <- scores(birdCCA, display = "species") %>%
  as.data.frame()

# plotting
plot_cca <- ggplot(site_data) +
  geom_point(aes(x = CCA1, y = CCA2, color = site_type), shape = 19, size = 2, alpha = 0.8) +
  coord_fixed() +
  geom_segment(data = ccavectors, aes(x = 0, y = 0, xend = CCA1, yend = CCA2), arrow = arrow(length = unit(0.2, "cm"))) +
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  geom_point(data = species_data, aes(x = CCA1, y = CCA2), shape = 17, size = 2, color = "slateblue") +
  geom_text(data = ccavectors, aes(x = CCA1, y = CCA2, label = rownames(ccavectors)), nudge_x = 0.3, nudge_y = 0.3) +
  clean_background +
  labs(title = "Canonical Correspondence Analysis")

pdf(file = "Results/birdCCA_2.pdf", width = 10, height = 10, useDingbats=FALSE)
  plot(plot_cca)
    dev.off()





# RDA Analysis
https://rpubs.com/Roeland-KINDT/706490
# install.packages('BiodiversityR')
# install.packages('poppr')
# install.packages('ggforce')
# install.packages('BiodiversityR')


install.packages('permute', lib='/home/dnjacks4/R/')
library(permute, lib='/home/dnjacks4/R/')
install.packages('vegan', lib='/home/dnjacks4/R/')
library(vegan, lib='/home/dnjacks4/R/')
install.packages('carData', lib='/home/dnjacks4/R/')
library(carData, lib='/home/dnjacks4/R/')
install.packages('abind', lib='/home/dnjacks4/R/')
library(abind, lib='/home/dnjacks4/R/')
install.packages('Formula', lib='/home/dnjacks4/R/')
library(Formula, lib='/home/dnjacks4/R/')
install.packages('gtable', lib='/home/dnjacks4/R/')
library(gtable, lib='/home/dnjacks4/R/')
install.packages('crayon', lib='/home/dnjacks4/R/')
library(crayon, lib='/home/dnjacks4/R/')
install.packages('crayon', lib='/home/dnjacks4/R/')
library(crayon, lib='/home/dnjacks4/R/')
install.packages('pkgconfig', lib='/home/dnjacks4/R/')
library(pkgconfig, lib='/home/dnjacks4/R/')
install.packages('RColorBrewer', lib='/home/dnjacks4/R/')
library(RColorBrewer, lib='/home/dnjacks4/R/')
install.packages('gridExtra', lib='/home/dnjacks4/R/')
library(gridExtra, lib='/home/dnjacks4/R/')
install.packages('rstudioapi', lib='/home/dnjacks4/R/')
library(rstudioapi, lib='/home/dnjacks4/R/')
install.packages('nortest', lib='/home/dnjacks4/R/')
library(nortest, lib='/home/dnjacks4/R/')


install.packages('BiodiversityR', lib='/home/dnjacks4/R/', dependencies=TRUE)
install.packages('BiodiversityR', dependencies=TRUE)

library(BiodiversityR, lib='/home/dnjacks4/R/')

carData
library(vegan, lib='/home/dnjacks4/R/')
library(BiodiversityR, lib='/home/dnjacks4/R/')

library(BiodiversityR) # also loads vegan
library(poppr) # also loads adegenet
library(ggplot2)
library(ggsci)
library(ggforce)
library(dplyr)
library(ggrepel)


a.comm <- data.frame(as.matrix(birds2))
birdCCA <- cca(birds2 ~ Mesic + Oasis + Xeric, data = env_subset)
a.rda <- rda(birds2 ~ Mesic + Oasis + Xeric, data = env_subset)
a.rda

anova(a.rda, permutations=9999)


attach(env_subset)
plot.a <- ordiplot(a.rda, choices=c(1,2))
Pop.ellipses <- ordiellipse(plot.a, groups=site_type, display="sites", kind="sd")


pdf(file = "Results/rda.pdf", width = 10, height = 10, useDingbats=FALSE)
plot.a <- ordiplot(a.rda, choices=c(1,2))
    dev.off()


BioR.theme <- theme(
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line("gray25"),
        text = element_text(size = 12),
        axis.text = element_text(size = 10, colour = "gray25"),
        axis.title = element_text(size = 14, colour = "gray25"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.key = element_blank())

a.env <- data.frame(Pop=env_subset$site_type)


sites1 <- sites.long(plot.a, env.data=a.env)
axis1 <- axis.long(a.rda, choices=c(1, 2))
centroids.Pop1 <- centroids.long(sites1, grouping=Pop.names, centroids.only=TRUE)
centroids.Pop2 <- centroids.long(sites1, grouping=Pop.names, centroids.only=FALSE)

Pop.ellipses.long2 <- ordiellipse.long(Pop.ellipses, grouping.name="Pop.names")

plotgg.a <- ggplot() +
    geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
    geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
    xlab(axis1[1, "label"]) +
    ylab(axis1[2, "label"]) +
    scale_x_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
    scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
    geom_polygon(data=Pop.ellipses.long2,
                   aes(x=axis1, y=axis2, colour=Pop.names,
                       fill=after_scale(alpha(colour, 0.2))),
              size=0.2, show.legend=TRUE) +
    geom_segment(data=centroids.Pop2,
                 aes(x=axis1c, y=axis2c, xend=axis1, yend=axis2, colour=Pop.names),
                 size=0.7, show.legend=FALSE) +
    geom_point(data=centroids.Pop1,
               aes(x=axis1c, y=axis2c),
               shape="square", colour="black", alpha=0.7, size=2) +
    geom_label_repel(data=centroids.Pop1,
               aes(x=axis1c, y=axis2c, label=Pop.names, colour=Pop.names),
               alpha=1.0, size=2, show.legend=FALSE) +
    labs(colour = "Population") +
    BioR.theme +
    theme(legend.text = element_text(size = 10)) +
    scale_colour_npg() +
    coord_fixed(ratio=1)

plotgg.a
