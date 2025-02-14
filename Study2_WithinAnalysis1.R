### GPT social decision-making
### Within-group analysis 1: proportion z-tests
### Programmed by Feng XIAO (2025.2.2)
### This R script requires one excel file: 'rawdata_study2'

### Preparation
package_list <- c('tidyr','dplyr','readxl','openxlsx')
lapply(package_list, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Proportion z-test
z.prop = function(x1, x2, n1, n2){
  numerator = (x1/n1) - (x2/n2)
  p.common = (x1 + x2) / (n1 + n2)
  denominator = sqrt(p.common * (1 - p.common) * (1/n1 + 1/n2))
  z.prop.ris = numerator / denominator
  p_value = 2 * (1 - pnorm(abs(z.prop.ris)))
  margin_of_error = qnorm(0.975) * denominator
  ci_lower = z.prop.ris - margin_of_error
  ci_upper = z.prop.ris + margin_of_error
  return(list(z = z.prop.ris, p = p_value, ci_low = ci_lower, ci_up = ci_upper))
}

### Analysis
########################################################################################################
## GPT-3.5
gpt3.5 <- read_excel('rawdata_study2.xlsx', sheet = 'gpt3.5', na = '---')
Gpt3.5 <- gpt3.5[,-1]

cTable_gpt3.5 <- data.frame(frame = c('6 lives (fair)','600 lives (fair)',
                                      'YSO-6 kin','YSY-6 kin','MSO-6 kin',
                                      'MSY-6 kin', '6 kin', '6 lives', '600 lives',
                                      '6 kin (two-thirds)', '6 lives (two-thirds)', '600 lives (two-thirds)'),
                            domain =c('Sense of fairness','Sense of fairness','Young decider: old vs. young kin',
                                      'Young decider: old vs. young kin','Middle-aged decider: old vs. young kin',
                                      'Middle-aged decider: old vs. young kin','Group-dependent goal settings (inequal EV)',
                                      'Group-dependent goal settings (inequal EV)','Group-dependent goal settings (inequal EV)',
                                      'Group-dependent goal settings (equal EV)','Group-dependent goal settings (equal EV)',
                                      'Group-dependent goal settings (equal EV)'),
                            type = c('GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5',
                                     'GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5'),
                      z = numeric(12),
                      p = numeric(12),
                      ci_low = numeric(12),
                      ci_up = numeric(12))

for (i in 1:9) {
  a_p <- sum(Gpt3.5[,i] == 'a', na.rm = TRUE)+1
  b_p <- sum(Gpt3.5[,i] == 'b', na.rm = TRUE)+1 #plus-one corrections
  cTable_gpt3.5$z[i] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$z
  cTable_gpt3.5$p[i] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$p
  cTable_gpt3.5$ci_low[i] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$ci_low
  cTable_gpt3.5$ci_up[i] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$ci_up
} #selective saving - random saving for the domain "Sense of fairness"

for (i in 10:12) {
  a_p2 <- sum(Gpt3.5[,i] == 'a', na.rm = TRUE)+1
  b_p2 <- sum(Gpt3.5[,i] == 'b', na.rm = TRUE)+1
  a_p1 <- sum(Gpt3.5[,i+3] == 'a', na.rm = TRUE)+1
  b_p1 <- sum(Gpt3.5[,i+3] == 'b', na.rm = TRUE)+1 #plus-one corrections
  cTable_gpt3.5$z[i] <- z.prop(b_p2, b_p1, a_p2+b_p2, a_p1+b_p1)$z
  cTable_gpt3.5$p[i] <- z.prop(b_p2, b_p1, a_p2+b_p2, a_p1+b_p1)$p
  cTable_gpt3.5$ci_low[i] <- z.prop(b_p2, b_p1, a_p2+b_p2, a_p1+b_p1)$ci_low
  cTable_gpt3.5$ci_up[i] <- z.prop(b_p2, b_p1, a_p2+b_p2, a_p1+b_p1)$ci_up
} #comparisons for RS: two-thirds - one-third

write.xlsx(cTable_gpt3.5, 
           file = 'Outputs_study2/WithinAnalysis_GPT3.5.xlsx',
           rowNames = FALSE)
###############################################################################################################
## GPT-4
gpt4 <- read_excel('rawdata_study2.xlsx', sheet = 'gpt4', na = '---')
Gpt4 <- gpt4[,-1]

cTable_gpt4 <- data.frame(frame = c('6 lives (fair)','600 lives (fair)',
                                    'YSO-6 kin','YSY-6 kin','MSO-6 kin',
                                    'MSY-6 kin', '6 kin', '6 lives', '600 lives',
                                    '6 kin (two-thirds)', '6 lives (two-thirds)', '600 lives (two-thirds)',
                                    '6 kin (high)', '6 lives (high)', '600 lives (high)',
                                    '6 kin (low)', '6 lives (low)', '600 lives (low)'),
                          domain =c('Sense of fairness','Sense of fairness','Young decider: old vs. young kin',
                                    'Young decider: old vs. young kin','Middle-aged decider: old vs. young kin',
                                    'Middle-aged decider: old vs. young kin','Group-dependent goal settings (inequal EV)',
                                    'Group-dependent goal settings (inequal EV)','Group-dependent goal settings (inequal EV)',
                                    'Group-dependent goal settings (equal EV)','Group-dependent goal settings (equal EV)',
                                    'Group-dependent goal settings (equal EV)',
                                    'Group-dependent goal settings (equal EV)','Group-dependent goal settings (equal EV)',
                                    'Group-dependent goal settings (equal EV)',
                                    'Group-dependent goal settings (equal EV)','Group-dependent goal settings (equal EV)',
                                    'Group-dependent goal settings (equal EV)'),
                          type = c('GPT-4','GPT-4','GPT-4','GPT-4','GPT-4','GPT-4',
                                   'GPT-4','GPT-4','GPT-4','GPT-4','GPT-4','GPT-4',
                                   'GPT-4','GPT-4','GPT-4','GPT-4','GPT-4','GPT-4'),
                          z = numeric(18),
                          p = numeric(18),
                          ci_low = numeric(18),
                          ci_up = numeric(18))

for (i in 1:9) {
  a_p <- sum(Gpt4[,i] == 'a', na.rm = TRUE)+1
  b_p <- sum(Gpt4[,i] == 'b', na.rm = TRUE)+1 #plus-one corrections
  cTable_gpt4$z[i] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$z
  cTable_gpt4$p[i] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$p
  cTable_gpt4$ci_low[i] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$ci_low
  cTable_gpt4$ci_up[i] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$ci_up
}

for (i in 10:12) {
  a_p2 <- sum(Gpt4[,i] == 'a', na.rm = TRUE)+1
  b_p2 <- sum(Gpt4[,i] == 'b', na.rm = TRUE)+1
  a_p1 <- sum(Gpt4[,i+3] == 'a', na.rm = TRUE)+1
  b_p1 <- sum(Gpt4[,i+3] == 'b', na.rm = TRUE)+1 #plus-one corrections
  cTable_gpt4$z[i] <- z.prop(b_p2, b_p1, a_p2+b_p2, a_p1+b_p1)$z
  cTable_gpt4$p[i] <- z.prop(b_p2, b_p1, a_p2+b_p2, a_p1+b_p1)$p
  cTable_gpt4$ci_low[i] <- z.prop(b_p2, b_p1, a_p2+b_p2, a_p1+b_p1)$ci_low
  cTable_gpt4$ci_up[i] <- z.prop(b_p2, b_p1, a_p2+b_p2, a_p1+b_p1)$ci_up
} #comparisons for RS: two-thirds - one-third

for (i in 10:15) {
  a_p <- sum(Gpt4[,i] == 'a', na.rm = TRUE)+1
  b_p <- sum(Gpt4[,i] == 'b', na.rm = TRUE)+1 #plus-one corrections
  cTable_gpt4$z[i+3] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$z
  cTable_gpt4$p[i+3] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$p
  cTable_gpt4$ci_low[i+3] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$ci_low
  cTable_gpt4$ci_up[i+3] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$ci_up
} #comparisons for RS with 50-50

write.xlsx(cTable_gpt4, 
           file = 'Outputs_study2/WithinAnalysis_GPT4.xlsx',
           rowNames = FALSE)
###############################################################################################################
## Human
human <- read_excel('rawdata_study2.xlsx', sheet = 'human', na = '---')
Human <- human[,-1]

cTable_h <- data.frame(frame = c('6 lives (fair)','600 lives (fair)',
                                 'YSO-6 kin','YSY-6 kin','MSO-6 kin',
                                 'MSY-6 kin', '6 kin', '6 lives', '600 lives',
                                 '6 kin (two-thirds)', '6 lives (two-thirds)', '600 lives (two-thirds)'),
                       domain =c('Sense of fairness','Sense of fairness','Young decider: old vs. young kin',
                                 'Young decider: old vs. young kin','Middle-aged decider: old vs. young kin',
                                 'Middle-aged decider: old vs. young kin','Group-dependent goal settings (inequal EV)',
                                 'Group-dependent goal settings (inequal EV)','Group-dependent goal settings (inequal EV)',
                                 'Group-dependent goal settings (equal EV)','Group-dependent goal settings (equal EV)',
                                 'Group-dependent goal settings (equal EV)'),
                       type = c('Human','Human','Human','Human','Human','Human',
                                'Human','Human','Human','Human','Human','Human'),
                       z = numeric(12),
                       p = numeric(12),
                       ci_low = numeric(12),
                       ci_up = numeric(12))

for (i in 1:9) {
  a_p <- sum(Human[,i] == 'a', na.rm = TRUE)+1
  b_p <- sum(Human[,i] == 'b', na.rm = TRUE)+1 #plus-one corrections
  cTable_h$z[i] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$z
  cTable_h$p[i] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$p
  cTable_h$ci_low[i] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$ci_low
  cTable_h$ci_up[i] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$ci_up
}

for (i in 10:12) {
  a_p2 <- sum(Human[,i] == 'a', na.rm = TRUE)+1
  b_p2 <- sum(Human[,i] == 'b', na.rm = TRUE)+1
  a_p1 <- sum(Human[,i+3] == 'a', na.rm = TRUE)+1
  b_p1 <- sum(Human[,i+3] == 'b', na.rm = TRUE)+1 #plus-one corrections
  cTable_h$z[i] <- z.prop(b_p2, b_p1, a_p2+b_p2, a_p1+b_p1)$z
  cTable_h$p[i] <- z.prop(b_p2, b_p1, a_p2+b_p2, a_p1+b_p1)$p
  cTable_h$ci_low[i] <- z.prop(b_p2, b_p1, a_p2+b_p2, a_p1+b_p1)$ci_low
  cTable_h$ci_up[i] <- z.prop(b_p2, b_p1, a_p2+b_p2, a_p1+b_p1)$ci_up
} #comparisons for RS: two-thirds - one-third

write.xlsx(cTable_h, 
           file = 'Outputs_study2/WithinAnalysis_Human.xlsx',
           rowNames = FALSE)