# Opioid_Medicare_D_RX_2014
Jenny  
9/30/2017  



Load packages


```r
library(tidyverse)
```

```
## Loading tidyverse: ggplot2
## Loading tidyverse: tibble
## Loading tidyverse: tidyr
## Loading tidyverse: readr
## Loading tidyverse: purrr
## Loading tidyverse: dplyr
```

```
## Conflicts with tidy packages ----------------------------------------------
```

```
## filter(): dplyr, stats
## lag():    dplyr, stats
```

```r
library(ggridges)
library(data.table)
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```
## The following object is masked from 'package:purrr':
## 
##     transpose
```

```r
library(zipcode)
library(viridis)
```

```
## Loading required package: viridisLite
```

File "Medicare_Part_D_Opioid_Prescriber_Summary_File_2014.csv" downloaded from https://data.cms.gov/Medicare-Claims/Medicare-Part-D-Opioid-Prescriber-Summary-File-201/e4ka-3ncx/data.

Description of the file:

"The Centers for Medicare & Medicaid Services (CMS) has prepared a public data set, the Medicare Part D Opioid Prescriber Summary File, which presents information on the individual opioid prescribing rates of health providers that participate in Medicare Part D program. This file is a prescriber-level data set that provides data on the number and percentage of prescription claims (includes new prescriptions and refills) for opioid drugs, and contains information on each provider’s name, specialty, state, and ZIP code. This summary file was derived from the 2014 Part D Prescriber Summary Table (Documentation available at: https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Prescriber_Methods.pdf)"

Read in file in data frame format. View first few rows to examine.


```r
opioidRX<-as.data.frame(fread("Medicare_Part_D_Opioid_Prescriber_Summary_File_2014.csv"))

View(head(opioidRX))
```


Fix up the variables:
Make new, clean variable for RX Rate, since `Opioid Prescribing Rate` is a character variable with % at the end. Clean some variable names and change to factor instead of character. Clean up zipcodes for possible map visualization.


```r
opioidRX$MD_RXRate <- (opioidRX$`Opioid Claim Count`)/(opioidRX$`Total Claim Count`)

names(opioidRX)[6] = "Specialty"
opioidRX$Specialty <-as.factor(opioidRX$Specialty)
names(opioidRX)[5] ="State"
opioidRX$State <- as.factor(opioidRX$State)

opioidRX$Zip<- as.factor(clean.zipcodes(opioidRX$`NPPES Provider Zip Code`))
```

Include only States, DC, and Puerto Rico. Remove territories or military locations: "XX" "ZZ" "VI" "MP" "GU" "AS" "AP" "AA" "AE". Then remove unused State factor levels.

```r
opioidRX <- subset(opioidRX, !(State %in% c("XX" ,"ZZ", "VI", "MP", "GU", "AS", "AP", "AA", "AE")))

opioidRX$State <- droplevels(opioidRX$State)
```

Check distribution of data across States. Might want to add State population size to get ratio of RX/10,000 residents.

```r
table(opioidRX$State)
```

```
## 
##     AK     AL     AR     AZ     CA     CO     CT     DC     DE     FL 
##   2293  13100   8010  21253 111395  18116  16506   4503   3270  63155 
##     GA     HI     IA     ID     IL     IN     KS     KY     LA     MA 
##  26471   4070  10064   4877  43078  19897   9374  14677  15217  33379 
##     MD     ME     MI     MN     MO     MS     MT     NC     ND     NE 
##  21535   5885  38696  20272  20200   8420   3484  32599   2726   6491 
##     NH     NJ     NM     NV     NY     OH     OK     OR     PA     PR 
##   5543  30025   6870   6971  86814  41663  11283  15096  53490  10932 
##     RI     SC     SD     TN     TX     UT     VA     VT     WA     WI 
##   4690  14142   3088  23034  66920   7980  24844   2550  24599  20148 
##     WV     WY 
##   6892   1717
```
Download 2014 State OD Death Rate (age-adjusted death rate per 100,000 residents) data from 
https://www.cdc.gov/drugoverdose/data/statedeaths.html
and read in file.

Combine 2014 OD Death rate (column 2 of OD_DeathRates) with opioidRX by State.

Rename OD Death Rate variable (column 13 of opioidRX)


```r
OD_DeathRates<-read.csv("drug_poisoning_deaths_by_state-_us_2013_2014-v7.csv")

head(OD_DeathRates)
```

```
##   State X2014Rate X2014Number  X2014Range X2013Rate X2013Number
## 1    ND       6.3          43 2.8 to 11.0       2.8          20
## 2    NE       7.2         125 2.8 to 11.0       6.5         117
## 3    SD       7.8          63 2.8 to 11.0       6.9          55
## 4    IA       8.8         264 2.8 to 11.0       9.3         275
## 5    TX       9.7       2,601 2.8 to 11.0       9.3       2,446
## 6    MN       9.6         517 2.8 to 11.0       9.6         523
##    X2013Range Change     Significant
## 1 2.8 to 11.0  125.0    Significant 
## 2 2.8 to 11.0   10.8 Not Significant
## 3 2.8 to 11.0   13.0 Not Significant
## 4 2.8 to 11.0   -5.4 Not Significant
## 5 2.8 to 11.0    4.3 Not Significant
## 6 2.8 to 11.0    0.0 Not Significant
```

```r
opioidRX <- merge(opioidRX, OD_DeathRates[,1:2], by = "State")

names(opioidRX)[12] <- "StateODRate"
```


Make variable for median RXRate by Specialty. View and rank by Median.


```r
totalRX <- sum(opioidRX$`Opioid Claim Count`, na.rm = TRUE)

opioidRX <- opioidRX %>%
        group_by(Specialty) %>%
        mutate(MedianBySpecialty = median(MD_RXRate, na.rm=TRUE)) %>%
        mutate(RXnumberBySpecialty = sum(`Opioid Claim Count`, na.rm=TRUE)) %>%
        mutate(RXPercentBySpecialty = RXnumberBySpecialty/totalRX, na.rm=TRUE)


MedianBySpecialty <- opioidRX[,c(6,13)][!duplicated(opioidRX[,c(6,13)]), ]
View(MedianBySpecialty)
```


Use MedianBySpecialty to select Specialties to include in heatmap. Remove Specialites with median = 0 as well as surgical and oncology specialties, which are expected to have high opioid RX rates, no matter what.

Create tidy dataframe.


```r
View(MedianBySpecialty)

Specialties <- c("Emergency Medicine", "Dentist", "Osteopathic Manipulative Medicine", "Physician Assistant", "Family Practice", "Geriatric Medicine", "Internal Medicine", "General Practice", "Nurse Practitioner", "Family Medicine")

opioidRX_subset <- droplevels(subset(opioidRX, Specialty %in% Specialties))

TidyOpioidRX <- opioidRX_subset %>%
        group_by(State, Specialty) %>%
        summarise(SpecialtyMedian = median(MD_RXRate, na.rm = TRUE)) %>%
        complete(State, Specialty, fill = list(SpecialtyMedian = 0))

TidyOpioidRX <- merge(TidyOpioidRX,OD_DeathRates[,1:2], by = "State")
```


Make heatmap with States ranked from highest (top) to lowest (bottom) OD Death Rate per 100,000 and Specialties ranked from lowest (left) to highest (right) mean opioid RX rate.


```r
TidyOpioidRX$xlabel <- paste(TidyOpioidRX$State, TidyOpioidRX$X2014Rate, sep = ":  ")


OpioidHeatMap <- ggplot(TidyOpioidRX, 
              aes(x = reorder(Specialty, SpecialtyMedian, median, na.rm=TRUE), 
                  y = reorder(xlabel, X2014Rate, mean, na.rm=TRUE))) +
        geom_tile(aes(fill = SpecialtyMedian), color = "gray") + 
        scale_fill_viridis(direction = -1, option = "B") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
        theme(axis.text.y = element_text(hjust=0, size = 7)) +
        labs(fill = "% Opioid of \nTotal Prescriptions", title = "2014 Medicare Opioid Prescription Rates \nBy Specialty & State", x = "", y = "State OD Death Rate Per 100,000") +
        coord_fixed(ratio = .5)

OpioidHeatMap
```

![](Opioid_Medicare_D_RX_2014_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

