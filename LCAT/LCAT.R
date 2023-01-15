if(!any(rownames(installed.packages()) == "pacman"))
        install.packages("pacman")

pacman::p_load(tidyverse,
               DT,
               here)


#Read CSV
LCAT <- read_csv(here("LCAT","tidy_data","20210615_LCAT.csv"))

#Add CV
LCAT <- LCAT %>%
        group_by(sample, concentration) %>%
        mutate(ratio = 1/ratio,
                cv = sd(ratio)/mean(ratio)*100) %>% 
        unite("Sample", sample:units, remove = FALSE)

LCAT$Sample<- as.factor(LCAT$Sample)


LCAT_summary <- LCAT %>%
        group_by(Sample) %>%
        summarize(mean = mean(ratio), sd = sd(ratio), cv = sd/mean)



## Data Table
datatable(LCAT_summary) %>%
        formatRound(c('mean', 'sd', 'cv'), digits = 2) %>%
        formatPercentage('cv', 2)


## GGPLOT

LCAT_summary$Sample <-  factor(LCAT_summary$Sample, levels = c("plasma_2_ul", "plasma_4_ul", "plasma_8_ul",
                                                  "ApoB_dep_2_ul", "ApoB_dep_4_ul", "ApoB_dep_8_ul", 
                                                  "HDL_2.5_ug", "HDL_5_ug", "HDL_10_ug", "HDL_15_ug",
                                                  "HDL_20_ug", "HDL_25_ug"))

ggplot(LCAT_summary, aes(x = Sample, y=mean, fill = Sample)) + 
        geom_bar(stat = 'identity') +
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(legend.position = "none") +
        labs(y = "390/470 nm")

