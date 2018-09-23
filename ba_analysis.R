###############################################################
###############         init        ###########################
###############################################################

### libraries
library(readr)
library(magrittr)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(ggExtra)
options(tibble.width = Inf)

### filepaths
wd <- "C:\\Users\\dylan\\Documents\\job applications\\Guilford County" # add your working directory here
rawloc <- paste0(wd, "\\raw") # put raw data here
resloc <- paste0(wd, "\\results") # results will go here


### open data
untar(paste0(rawloc, "\\beer_reviews.tar.gz"), files = "beer_reviews/beer_reviews.csv", exdir = rawloc)
beers <- read_csv(paste0(rawloc, "/beer_reviews/beer_reviews.csv"))

###############################################################
###############         data prep        ######################
###############################################################

### examine data
summary(beers)
str(beers)
head(beers)

### data quality checks
## completeness 
beers %>% filter(is.na(brewery_name))
# 2 breweries are only identified by an ID, not name...one seems to be for incorrectly categorized beers and the other is simply missing

beers %>% filter(is.na(beer_name))
# all beers have a name


# further examine the incorrectly categorized breweries
beers %>% filter(str_detect(beer_name, "WRONG"))
beers %>% filter(str_detect(brewery_name, "CRAILSHEIMER"))
beers %>% filter(str_detect(brewery_name, "SCHWABISCH"))
# neither of the incorrectly classified breweries appear elsewhere in the data

## beer and brewery IDs vs. names
unique(beers$brewery_id) %>% length
unique(beers$brewery_name) %>% length
unique(beers$beer_beerid) %>% length
unique(beers$beer_name) %>% length
# some beers and breweries have the same name despite being different, so best to use IDs as units of analysis

## consistency of variables within each other (beers within breweries, styles and ABV within beers)
beers %>% group_by(beer_beerid) %>% summarise(nbreweries = n_distinct(brewery_id)) %>% summarise(max(nbreweries))
beers %>% group_by(beer_beerid) %>% summarise(nstyles = n_distinct(beer_style)) %>% summarise(max(nstyles))
beers %>% group_by(beer_beerid) %>% summarise(nabvs = n_distinct(beer_abv)) %>% summarise(max(nabvs))
# all variables consistent

## missingness of ABV and ratings 
beers %>% filter(is.na(review_overall)) %>% nrow
beers %>% filter(is.na(beer_style)) %>% nrow
beers %>% filter(is.na(review_palate)) %>% nrow
beers %>% filter(is.na(review_taste)) %>% nrow
beers %>% filter(is.na(review_aroma)) %>% nrow
beers %>% filter(is.na(review_appearance)) %>% nrow
beers %>% filter(is.na(beer_abv)) %>% nrow
beers %>% group_by(beer_beerid) %>% summarise(beer_abv = mean(beer_abv)) %>% filter(is.na(beer_abv)) %>% nrow
#review variables are all non-missing...ABV has ~17,000 missing


### create other useful datasets
# beer-level data with information on ratings and ABV
beers.overall <- beers %>%
  group_by(beer_beerid) %>%
  summarise(nreviews = n(), 
            review_overall = mean(review_overall), 
            review_aroma = mean(review_aroma),
            review_appearance = mean(review_appearance),
            review_palate = mean(review_palate),
            review_taste = mean(review_taste),
            beer_abv = mean(beer_abv))

# beer-level data with information on brewery and style frequency
beer.info <- beers %>% distinct(beer_beerid, .keep_all = TRUE) %>%
  select(beer_beerid, brewery_id, brewery_name, beer_style, beer_name) %>%
  group_by(brewery_name) %>% mutate(brewery_count = n()) %>% ungroup %>%
  group_by(beer_style) %>% mutate(style_count = n())


###############################################################
##### Which brewery produces the strongest beers by ABV%? #####
###############################################################

### examine ABV
hist(beers.overall$beer_abv)
beers.overall %>% left_join(beer.info, by = "beer_beerid") %>%
  filter(beer_abv > 15) %>%
  arrange(brewery_name) %>%
  print(n = Inf)
# a number of breweries have multiple beers with >15% ABV

### find average ABV by brewery
abvs <- beers.overall %>% left_join(beer.info, by = "beer_beerid") %>%
  group_by(brewery_id) %>%
  summarise(mean_abv = mean(beer_abv, na.rm = TRUE), n = n()) %>%
  arrange(desc(mean_abv))

abvs %>% head(1) %>% left_join(beers, by = "brewery_id")
# Schorschbrau has the highest average average ABV (and a reasonable number of beers to choose from)


###############################################################
###### Which 3 beers would you recommend from this data? ######
###############################################################

### My (admittedly smoewhat arbitrary) criteria:
# 1. I want to recommend beers with strong ratings overall and ones that aren't weak in any particular area,
# though I care more about palate and taste than about appearance and aroma 
# (this is both personal preference, and justified by the next section).
# 2. I only want to consider beers with a reasonable number of reviews, so that results aren't biased by 
# a small number of good reviews (for all I know these could be left by the brewery themselves).
# 3. I'd like to recommend beers that may be new to people, so I want either the brewery or style to be fairly uncommon.
# 4. I'd like to recommend a variety of beers so I'd prefer not to have a repeat in brewery or style
# and I will recommend a high, medium, and low ABV beer.


### examining thresholds for reviews and ABV
# reviews
nreview <- 5
hist(beers.overall$nreviews)
hist(beers.overall$nreviews[beers.overall$nreviews <= 100])
sum(beers.overall$nreviews >= nreview)
nrow(beers.overall)
# I'm choosing 5  as the number of reviews a beer should have for consideration, as it seems like enough to 
# be fairly representative, but also leaves a large selection.
# any threshold is going to remove a lot of beers though, because aabout 1/3 have only one review, so there's a definite
# trade-off between variety and confidence of quality

# ABVs
hist(beers.overall$beer_abv)
hist(beers.overall$beer_abv[beers.overall$beer_abv <= 20])
summary(beers.overall$beer_abv)

sum(is.na(beers.overall$beer_abv))
sum(is.na(beers.overall$beer_abv) & beers.overall$nreviews > nreview)
# the first thing I notice is that any direct ABV-based criteria is going to remove a lot of beers, and while many of these would
# be removed the review threshold anyway, it still seems higher than necessary. Another approach would be to categorize by average 
# ABV within a style. While this introduces the possibility of e.g. the "low" ABV choice being higher than the "medium," it also
# ensures that my three beers will have different styles, which was one of my criteria.

style.abv <- beers.overall %>% left_join(beer.info, by = "beer_beerid") %>%
  group_by(beer_style) %>%
  summarise(abv = mean(beer_abv, na.rm = TRUE), n = n()) 

low.abv <- 5
high.abv <- 7

hist(style.abv$abv)
low.styles <- style.abv %>% filter(abv <= low.abv)
med.styles <- style.abv %>% filter(abv > low.abv & abv < high.abv)
high.styles <- style.abv %>% filter(abv >= high.abv)

sum(low.styles$n)
sum(med.styles$n)
sum(high.styles$n)
# Choices in thresholds are informed by looking at the ABV summary statistics, as well as my perception of high, medium, and low ABVs.
# They also make it so that a comparable number of beers are in the high and the low categories


### finding the best beers
best <- beers.overall %>%
  filter(nreviews >= nreview & review_aroma >= 3.5 & review_appearance >= 3.5 & review_palate >= 3.5 & review_taste >= 3.5) %>%
  mutate(tot = review_overall + .5*review_aroma + review_taste + .5*review_appearance + review_palate ) %>%
  arrange(desc(tot))
best %>% print(n=30)
# removing anything with any single metric below 3.5, then creating a single score metric 
# by combining the weighted scores of different beer aspects, giving full weight to overall score, taste, and palate, 
# and 1/2 to appearance and aroma.

# Some notes:
# The choice of review threshold has a major impact here...the majority of top beers have are close to the threshold.
# Overall scores are pretty close to each other, so opting for more variety may be more meaningful than overall quality
# My choice to divide by style rather than beer ABV seems important, as a lot of the top beers have missing ABVs


### Get info about style and brewery relative frequency
beer.info$style_p <- percent_rank(beer.info$style_count) 
beer.info$brewery_p <- percent_rank(beer.info$brewery_count) 


# the recommended high ABV beer
best %>% left_join(y = beer.info, by = "beer_beerid") %>%
  right_join(y = high.styles, by = "beer_style") %>%
  filter((style_p <= .5 | brewery_p <= .5)) %>% 
  arrange(desc(tot)) %>%
  head(1)

# the recommended medium ABV beer
best %>% left_join(y = beer.info, by = "beer_beerid") %>%
  right_join(y = med.styles, by = "beer_style") %>%  
  filter((style_p <= .5 | brewery_p <= .5)) %>% 
  arrange(desc(tot)) %>%
  head(1)

# the recommended low ABV beer
best %>% left_join(y = beer.info, by = "beer_beerid") %>%
  right_join(y = low.styles, by = "beer_style") %>%  
  filter((style_p <= .5 | brewery_p <= .5)) %>% 
  arrange(desc(tot)) %>%
  head(1)

# while there was a lot of arbitrariness in my criteria, these seem like pretty good results.
# the low and medium ABV recommendations have similarities (both sour), but they sound fairly different otherwise, 
# and both are very different than the high ABV stout


###############################################################
##### Which factors most determine overall beer quality? ######
###############################################################

### We can put these in a linear regression equation to see the relative size of each coefficient
lm(review_overall ~ review_aroma + review_appearance + review_palate + review_taste, data = beers) %>% summary
# taste seems quite a bit more important than the other factors. While all are significantly positively correlated
# with overall rating, a one-point increase in taste is associated with a .55 increase in score, much larger than
# the other coefficients.
# Palate has the next highest effect, with a coefficient of .25


# we can also model each of these separately to see how much variance in overall score is explained by each factor
lm(review_overall ~ review_aroma, data = beers) %>% summary
lm(review_overall ~ review_appearance, data = beers) %>% summary
lm(review_overall ~ review_palate, data = beers) %>% summary
lm(review_overall ~ review_taste, data = beers) %>% summary
# Again, taste explains the most variance (62%). 
# Adding the other factors explains only a small amount of additional variation (66%)

### Finally, we can show this visually. I'm using the beer-level data rather than the review level data here to have more variation
### in scores
bins <- 35
breaks <- c(1000, 3000, 5000, 7000)
ar <- ggplot(beers.overall, aes(x = review_overall, y = review_aroma)) + 
  geom_bin2d(bins = bins)  +
  scale_fill_continuous(limits = c(0,max(breaks)), breaks = breaks) + 
  labs(y = "Overall", fill = element_blank(), title = "Aroma", x = element_blank()) + 
  theme_classic() +
  theme(legend.justification=c(1,0), legend.position=c(1,0), 
        legend.key.size = unit(.2, "cm"), legend.text = element_text(size = 8))

ap <- ggplot(beers.overall, aes(x = review_overall, y = review_appearance)) + 
  geom_bin2d(bins = bins)  +
  scale_fill_continuous(limits = c(0,max(breaks)), breaks = breaks) + 
  labs(y = "Overall", fill = element_blank(), title = "Appearance", x = element_blank()) + 
  theme_classic() +
  theme(legend.justification=c(1,0), legend.position=c(1,0), 
        legend.key.size = unit(.2, "cm"), legend.text = element_text(size = 8))

pa <- ggplot(beers.overall, aes(x = review_overall, y = review_palate)) + 
  geom_bin2d(bins = bins) +
  scale_fill_continuous(limits = c(0,max(breaks)), breaks = breaks)  + 
  labs(y = "Overall", fill = element_blank(), title = "Palate", x = element_blank()) + 
  theme_classic() +
  theme(legend.justification=c(1,0), legend.position=c(1,0), 
        legend.key.size = unit(.2, "cm"), legend.text = element_text(size = 8))
  
ta <- ggplot(beers.overall, aes(x = review_taste, y = review_overall)) + 
  geom_bin2d(bins = bins) +
  scale_fill_continuous(limits = c(0, max(breaks)), breaks = breaks) + 
  labs(y = "Overall", fill = element_blank(), title = "Taste", x = element_blank()) + 
  theme_classic() +
  theme(legend.justification=c(1,0), legend.position=c(1,0), 
        legend.key.size = unit(.2, "cm"), legend.text = element_text(size = 8)) 
all <- grid.arrange(ar, ap, pa, ta, nrow = 2, top = "Overall Score vs. Beer Factors")
ggsave(all, filename = paste0(resloc, "\\score_factors.png"), device = "png")
# each metric (and the overall score) has a peak at 4, but the plot for taste is significantly tighter than for the other
# features


###############################################################
####### Recommended style based on aroma and appearance #######
###############################################################

### aggregating beers by style
styles <- beers %>%
  group_by(beer_style) %>%
  summarise(avg_aroma = mean(review_aroma), avg_appearance = mean(review_appearance), avg_score = mean(review_overall), 
            sd_aroma = sd(review_aroma), sd_appearance = sd(review_appearance), sd_score = sd(review_overall),
            nreviews = n())

### finding the styles with the best combined scores
style.sub <- styles %>% mutate(ap_arr_comb = avg_appearance + avg_aroma) %>%
  arrange(desc(ap_arr_comb))
style.sub
# If your primary interest is just aroma and appearance, the American Double/Imperial Stout has the best combined score,
# though others are close. This also doesn't really account for the range in quality. It could have some very, very good 
# scores that mask a few mediocre scores, and if a different beer is more consistently high-scoring, that could be 
# a better choice.

# We can visualize these ranges by plotting confidence ellipses of the aroma and appearance scores of the different styles
# This is a fairly rough indicator, but does a good job of highlighting styles with large ranges in quality and can 
# let us know if overall scores are masking significant variation 
# that we should be taking into account.
# I'm plotting 3, 5, and 7 beers as there is a significant drop-off after 7, but the plot with 7 is a bit too busy
for (i in c(3, 5, 7)) {
  style.ranges <- beers.overall %>% left_join(y = beer.info, by = "beer_beerid") %>%
    right_join(y = style.sub[1:i,], by = "beer_style") %>% 
  ggplot(aes(review_aroma, review_appearance, color = beer_style)) +
    stat_ellipse(type = "norm", level = .9) + 
    scale_x_continuous(limits = c(2.5, 5.5)) + 
    scale_y_continuous(limits = c(2.5, 5.5)) +
    labs(y = "Appearance", x = "Aroma", color = "Style", title = "Range in aroma and appearance by style") +   
    theme_classic()
  ggsave(style.ranges, filename = paste0(resloc, "\\style_quality_", i, ".png"), device = "png")
}
### review of each style:
  # Eisbock performs pretty well by appearance, but poorly by aroma
  # American Wild Ale doesn't perform too well on either metric
  # Geuze has the "highest highs" for aroma, but also some of the "lowest lows," with a similar (but overall less 
  # impressive) range for appearance scores.
  # Quads and Imperial IPAs are fairly similar to each other. Both perform reasonably well (but not the best), 
  # and quads have some of the "lowest lows" in both metrics
  # Russian Imperial and American Imperial Stouts seem to perform the best overall 
  # These also have the two highest average scores, so while there was some interesting variation, our intuition
  # about the best styles from the averages seems to have been pretty good.

# From the above plots you could make the case for either Russian or American Imperial Stouts.
# The former has better appearance scores on the low end, while the latter has better aroma scores
# on the high end.
# Let's take a closer at the distribution both graphically and by looking at summary stats of their scores 
# to look to see if we can make a final determination.
p <- beers.overall %>% left_join(y = beer.info, by = "beer_beerid") %>%
  filter(beer_style == "Russian Imperial Stout") %>%
ggplot(aes(review_aroma, review_appearance)) + 
  geom_point(size = .5) +
  scale_y_continuous(limits = c(1,5)) +
  scale_x_continuous(limits = c(1,5)) +
  labs(y = "Appearance", x = "Aroma", size = element_blank(), title = "Russian Imperial Stout") +     
  theme_classic()
ris.hist <- ggMarginal(p, type = "histogram", fill="transparent")
ris.box <- ggMarginal(p, type = "boxplot", fill="transparent")

p <- beers.overall %>% left_join(y = beer.info, by = "beer_beerid") %>%
  filter(beer_style == "American Double / Imperial Stout") %>%
  ggplot(aes(review_aroma, review_appearance)) + 
  geom_point(size = .5) +
  scale_y_continuous(limits = c(1,5)) +
  scale_x_continuous(limits = c(1,5)) +
  labs(y = "Appearance", x = "Aroma", size = element_blank(), title = "American Imperial Stout") +     
  theme_classic()  
ais.hist <- ggMarginal(p, type = "histogram", fill="transparent")
ais.box <- ggMarginal(p, type = "boxplot", fill="transparent")

comb.hist <- grid.arrange(ris.hist, ais.hist, ncol = 2)
comb.box <- grid.arrange(ris.box, ais.box, ncol = 2)
ggsave(comb.hist, filename = paste0(resloc, "\\russian_american_stouts_comp_hist.png"), device = "png")
ggsave(comb.box, filename = paste0(resloc, "\\russian_american_stouts_comp_box.png"), device = "png")

# summary stats
beers.overall %>% left_join(y = beer.info, by = "beer_beerid") %>%
  filter(beer_style == "American Double / Imperial Stout") %>%
  select(review_aroma, review_appearance) %>% summary
beers.overall %>% left_join(y = beer.info, by = "beer_beerid") %>%
  filter(beer_style == "Russian Imperial Stout") %>%
  select(review_aroma, review_appearance) %>% summary

# The distributions are extremely similar, with RIS faring a bit better by appearance and AIS a bit better by
# aroma, but I would give the slight edge to American Imperial Stouts
# Their overall scores were higher, and they seem to do a bit better at both the high and low extremes.
# That said, you'd have a hard time going wrong with either choice.
