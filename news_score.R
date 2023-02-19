# Author: Chang Ho Yoon
# Created: Feb. 18, 2023

# Utilities script for functions that compute NEWS2 (https://www.rcplondon.ac.uk/projects/outputs/national-early-warning-score-news-2)

# *** NOTE that there is not yet any capability to consider the presence of
# *** chronic hypercapnic respiratory failure, which requires the use of
# *** SpO2 scale 2 (see ref. above).

###############################################################################

# Compute NEWS2 sub-scores and scores

compute_news <- function(df, sub_scores){
  
  ### function that computes the NEWS scores (and sub-scores of each NEWS subsection)
  ### input: data frame containing vital signs
  ### output: data frame with added columns of NEWS
  
  output <- df %>%
    mutate(resp_score = case_when(RespiratoryRate <= 8 ~ 3,
                                  RespiratoryRate >= 9 & RespiratoryRate <= 11 ~ 1,
                                  RespiratoryRate >= 12 & RespiratoryRate <= 20 ~ 0,
                                  RespiratoryRate >= 21 & RespiratoryRate <= 24 ~ 2,
                                  RespiratoryRate >= 25 ~ 3,
                                  TRUE ~ 0),
           sat_score = case_when(OxygenSaturation <= 91 ~ 3,
                                 OxygenSaturation >= 92 & OxygenSaturation <= 93 ~ 2,
                                 OxygenSaturation >= 94 & OxygenSaturation <= 95 ~ 1,
                                 OxygenSaturation >= 96 ~ 0,
                                 TRUE ~ 0),
           ox_score = case_when(SupplementaryOxygen == 1 ~ 2, TRUE ~ 0),
           sbp_score = case_when(SBP <= 90 ~ 3,
                                 SBP >= 91 & SBP <= 100 ~ 2,
                                 SBP >= 101 & SBP <= 110 ~ 1,
                                 SBP >= 111 & SBP <= 219 ~ 0,
                                 SBP >= 220 ~ 3,
                                 TRUE ~ 0),
           hr_score = case_when(HeartRate <= 40 ~ 3,
                                HeartRate >= 41 & HeartRate <= 50 ~ 1,
                                HeartRate >= 51 & HeartRate <= 90 ~ 0,
                                HeartRate >= 91 & HeartRate <= 110 ~ 1,
                                HeartRate >= 111 & HeartRate <= 130 ~ 2,
                                HeartRate >= 131 ~ 3,
                                TRUE ~ 0),
           avpu_score = case_when(AVPU == "Alert" ~ 0, is.na(AVPU) ~ 0, TRUE ~ 3),
           temp_score = case_when(Temperature <= 35 ~ 3,
                                  Temperature >= 35.1 & Temperature <= 36 ~ 1,
                                  Temperature >= 36.1 & Temperature <= 38 ~ 0,
                                  Temperature >= 38.1 & Temperature <= 39 ~ 1,
                                  Temperature >= 39.1 ~ 2,
                                  TRUE ~ 0)) %>%
    
    # compute total NEWS2 score *** NOTE: does not consider people with chronic hypercapnic resp failure ***
    mutate(news_score = resp_score + sat_score + ox_score + sbp_score + hr_score + avpu_score + temp_score,
           news_class = case_when(news_score <= 4 ~ 'low',
                                  news_score >=5 & news_score <=6 ~ 'medium',
                                  news_score >= 7 ~ 'high'),
           news_class = case_when(news_score < 5 & (resp_score == 3 | sat_score == 3 | ox_score == 3 | sbp_score == 3 | ox_score == 3 | sbp_score == 3 | hr_score == 3 | avpu_score == 3 | temp_score == 3) ~ 'low-medium', TRUE ~ news_class),
           news_class = factor(news_class, levels = c('low', 'low-medium', 'medium', 'high'))) %>%
    
    # make baseline versions of these
    group_by(id) %>%
    mutate(news_score_base = news_score[which(time == 0)],
           news_class_base = news_class[which(time==0)]) %>%
    ungroup() %>%
    
    assert(not_na, c(resp_score, sat_score, ox_score, sbp_score, hr_score, avpu_score, temp_score, news_score, news_class, news_score_base, news_class_base))
  
  if(!missing(sub_scores)){
    
    return(output)
    
  }else{
    
    output = output %>% select(-c(resp_score, sat_score, ox_score, sbp_score, hr_score, avpu_score, temp_score)) 
    return(output)
    
  }
  
}


###############################################################################

# Create variables that determine worsening NEWS or persistently bad NEWS (haha!)
# bad NEWS class = medium or high
# worsening NEWS if moving up in NEWS class or increasing NEWS score

bad_news <- function(df){
  
  output = df %>%
    
    group_by(id) %>%
    
    mutate(worsening_news = case_when(news_score > lag(news_score) ~ 1, TRUE ~ 0)) %>%
    mutate(bad_news = case_when(news_score > lag(news_score) ~ 1,
                                news_score == lag(news_score) & news_score >= 5 ~ 1, TRUE ~ 0)) %>%
    
    mutate(worsening_news_class = case_when(news_class == 'high' & lag(news_class) != 'high' ~ 1,
                                            news_class == 'medium' & lag(news_class) %in% c('low-medium', 'low') ~ 1,
                                            news_class == 'low-medium' & lag(news_class) == 'low' ~ 1,
                                            TRUE ~ 0)) %>%
    mutate(bad_news_class = case_when(worsening_news_class == 1 ~ 1,
                                      news_class == lag(news_class) & news_class %in% c('medium', 'high') ~ 1, TRUE ~ 0)) %>%
    ungroup() %>%
    
    assert(not_na, c(worsening_news, bad_news, worsening_news_class, bad_news_class))
  
  return(output)
  
}


