### Data Analysis ###
## Library

library(tidyverse)
library(rstatix)
library(DT)


### Read CSV
CEC_plate <- read.csv(file = "./Cholesterol-efflux-capacity/tidy/cec_plate3.csv", header = TRUE)


## change data type for subject and plate
CEC_plate$subject_id <- as.character(CEC_plate$subject_id)
CEC_plate$plate <- as.character(CEC_plate$plate)

CEC_all <- CEC_plate %>%
        group_by(subject_id, plate) %>%
        dplyr::summarize(
                cec_1 = fluorescence_1/(fluorescence_1 + lyse)*100,
                cec_2 = fluorescence_2/(fluorescence_2 + lyse)*100,
                cec_3 = fluorescence_3/(fluorescence_3 + lyse)*100,
                cec_average = (cec_1 + cec_2 + cec_3)/3)

Blank <- mean(CEC_all$cec_average[CEC_all$subject_id =='blank'])

CEC_all_filter <- CEC_all %>%
        filter(!(subject_id == "blank")) %>%
        group_by(subject_id, plate) %>%
        dplyr::summarize(norm_cec_average = cec_average - Blank
        )


CEC_summary <- CEC_all_filter %>%
        group_by(subject_id, plate) %>%
        dplyr::summarize(
                mean_cec_average = mean(norm_cec_average),
                sd_cec_average = sd(norm_cec_average),
                cv_cec_average = sd_cec_average/mean_cec_average*100
        )

ERCC <- CEC_summary$mean_cec_average[CEC_summary$subject_id =='ercc']

CEC_summary <- CEC_summary %>%
        mutate(index_cec_average = mean_cec_average/ERCC)

datatable(CEC_summary)
#write.csv(CEC_summary, file = "./CEC/tidy/CEC_plate3_summary.csv")
