# Setting working directory
setwd("~/Documents/Coursera/DataScience/5_Course/c5week4")

# Setting working directory at work 
setwd("~/Documents/DataScience/c5week4")

# Data loading
NOAA <- read.csv('repdata%2Fdata%2FStormData.csv.bz2', na.strings = c('', 'NA'))
head(NOAA)

# Library Loading
install.packages('tidyr', 'dyplr', 'grid', 'gridExtra', 'ggplot2')
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)
library(ggplot2)

# Converting variable names to lowe case
names(NOAA) <- tolower(names(NOAA))

# To answer both questions the following columns are needed
# EVTYPE
# FATALITIES
# INJURIES
# PROPDMG + PROPDMGEXP
# CROPDMNG + CROPDMGEXP

# Subsetting the data frame for population health
subHealth <- subset(NOAA, select = c(evtype, fatalities, injuries))
head(subHealth)

# Problem: Messy data! The column headers are values, instead of variable names
tidyHealth <- gather(subHealth, variable, counts, -evtype)
head(tidyHealth)

# Calculate sum of counts
sumHealth <- tidyHealth %>% group_by(evtype, variable) %>% summarise(sum = sum(counts))
sumHealth <- data.frame(sumHealth)
head(sumHealth)

# Filter for health
sumfatalities <- sumHealth %>% filter(variable == 'fatalities') %>% arrange(desc(sum)) %>% slice(1:10)
head(sumfatalities)
suminjuries <- sumHealth %>% filter(variable == 'injuries') %>% arrange(desc(sum)) %>% slice(1:10)
head(suminjuries)

# Rearranging the levels of factor varibel
sumfatalities$evtype <- factor(x = sumfatalities$evtype, levels = sumfatalities$evtype)
str(sumfatalities)
suminjuries$evtype <- factor(x = suminjuries$evtype, levels = suminjuries$evtype)
str(suminjuries)

plot1.1 <- ggplot(data = sumfatalities, aes(x = evtype, y = sum)) +
                geom_point() +
                coord_flip() +
                geom_text(aes(label=sum), hjust = -0.5) +
                scale_y_log10(limits = c(1e2,1e4)) +
                ylab('Total') +
                xlab('Event') +
                ggtitle('Fatalities')
plot1.1

plot1.2 <- ggplot(data = suminjuries, aes(x = evtype, y = sum)) +
        geom_point() +
        coord_flip() +
        geom_text(aes(label=sum), hjust = -0.5) +
        scale_y_log10(limits = c(1e3,1e6)) +
        ylab('Total') +
        xlab('Event') +
        ggtitle('Injuries')
plot1.2

plot1 <- grid.arrange(plot1.1, plot1.2, ncol = 2, top = 'Top 10 most harmful events for human health in the United States')
plot1

# Subsetting property and crop damage data
subProp <- subset(NOAA, select = c(evtype, propdmg, propdmgexp))
head(subProp)

subCrop <- subset(NOAA, select = c(evtype, cropdmg, cropdmgexp))
head(subCrop)

# Adding additional variable that calculates damage costs
subPropK <- subProp %>% filter(propdmgexp == 'K') %>% mutate(costs = propdmg * 1000)
head(subPropK)
str(subPropK)
subPropM <- subProp %>% filter(propdmgexp == 'M') %>% mutate(costs = propdmg * 1000000)
head(subPropM)
str(subPropM)
subPropB <- subProp %>% filter(propdmgexp == 'B') %>% mutate(costs = propdmg * 1000000000)
head(subPropB)
str(subPropB)

subCropK <- subCrop %>% filter(cropdmgexp == 'K') %>% mutate(costs = cropdmg * 1000)
head(subCropK)
str(subCropK)
subCropM <- subCrop %>% filter(cropdmgexp == 'M') %>% mutate(costs = cropdmg * 1000000)
head(subCropM)
str(subCropM)
subCropB <- subCrop %>% filter(cropdmgexp == 'B') %>% mutate(costs = cropdmg * 1000000000)
head(subCropB)
str(subCropB)

# Binding columns together
PropTotal <- rbind(subPropK, subPropM, subPropB)
str(PropTotal)
head(PropTotal)

CropTotal <- rbind(subCropK, subCropM, subCropB)
str(CropTotal)
head(CropTotal)

# Sum of costs per event
SumPropTotal <- PropTotal %>% group_by(evtype) %>% summarize(totalCost = sum(costs)) %>% arrange(desc(totalCost)) %>% slice(1:10)
SumPropTotal

SumCropTotal <- CropTotal %>% group_by(evtype) %>% summarize(totalCost = sum(costs)) %>% arrange(desc(totalCost)) %>% slice(1:10)
SumCropTotal

SumPropTotal$evtype <- factor(x = SumPropTotal$evtype, levels = SumPropTotal$evtype)
str(SumPropTotal)

SumCropTotal$evtype <- factor(x = SumCropTotal$evtype, levels = SumCropTotal$evtype)
str(SumCropTotal)

plot2.1 <- ggplot(data = SumPropTotal, aes(x = evtype, y = round(totalCost/1000000000, 2))) +
        geom_point() +
        coord_flip() +
        geom_text(aes(label= round(totalCost/1000000000,2), hjust = -0.5)) +
        ylim(limits = c(0,200)) +
        ylab('Damage Costs in Billion US Dollar') +
        xlab('Event') +
        ggtitle('Property Damage')
plot2.1

plot2.2 <- ggplot(data = SumCropTotal, aes(x = evtype, y = round(totalCost/1000000000, 2))) +
        geom_point() +
        coord_flip() +
        geom_text(aes(label= round(totalCost/1000000000,2), hjust = -0.5)) +
        ylim(limits = c(0,30)) +
        ylab('Damage Costs in Billion US Dollar') +
        xlab('Event') +
        ggtitle('Crop Damage')
plot2.2

plot2 <- grid.arrange(plot2.1, plot2.2, ncol = 2, top = 'Top 10 greatest events for economic consequences in the United States')
plot2

