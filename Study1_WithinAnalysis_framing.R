### GPT social decision-making
### Within-group analysis: proportion z-tests
### Programmed by Feng XIAO (2024.12.1)
### This R script requires one excel file: 'rawdata_study1'

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
p_gpt3.5 <- read_excel('rawdata_study1.xlsx', sheet = 'P_gpt3.5', na = '---')
n_gpt3.5 <- read_excel('rawdata_study1.xlsx', sheet = 'N_gpt3.5', na = '---')
P_gpt3.5 <- p_gpt3.5[,-1]
N_gpt3.5 <- n_gpt3.5[,-1]

cTable_gpt3.5 <- data.frame(frame = c('6 lives','60 lives','600 lives','6000 lives',
                                      '1 kin','2 kin','3 kin','6 kin','Entire humans','Entire ET',
                                      '6 paintings','60 paintings','600 paintings','6000 paintings',
                                      '6 dollars','60 dollars','600 dollars','6000 dollars'),
                            domain =c('Human lives','Human lives','Human lives',
                                      'Human lives','Kinship group','Kinship group','Kinship group','Kinship group','Entire human and ET species',
                                      'Entire human and ET species','Public properties','Public properties',
                                      'Public properties','Public properties',
                                      'Personal stock shares','Personal stock shares',
                                      'Personal stock shares','Personal stock shares'),
                            type = c('GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5',
                                     'GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5',
                                     'GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5'),
                      z = numeric(18),
                      p = numeric(18),
                      ci_low = numeric(18),
                      ci_up = numeric(18))

for (i in 1:ncol(P_gpt3.5)) {
  a_p <- sum(P_gpt3.5[,i] == 'a', na.rm = TRUE)+1
  b_p <- sum(P_gpt3.5[,i] == 'b', na.rm = TRUE)+1
  a_n <- sum(N_gpt3.5[,i] == 'a', na.rm = TRUE)+1
  b_n <- sum(N_gpt3.5[,i] == 'b', na.rm = TRUE)+1 #plus-one corrections
  cTable_gpt3.5$z[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$z
  cTable_gpt3.5$p[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$p
  cTable_gpt3.5$ci_low[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$ci_low
  cTable_gpt3.5$ci_up[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$ci_up
}

write.xlsx(cTable_gpt3.5, 
           file = 'Outputs_study1/WithinAnalysis_GPT3.5.xlsx',
           rowNames = FALSE)
###############################################################################################################
## GPT-4
p_gpt4 <- read_excel('rawdata_study1.xlsx', sheet = 'P_gpt4', na = '---')
n_gpt4 <- read_excel('rawdata_study1.xlsx', sheet = 'N_gpt4', na = '---')
P_gpt4 <- p_gpt4[,-1]
N_gpt4 <- n_gpt4[,-1]

cTable_gpt4 <- data.frame(frame = c('6 lives','60 lives','600 lives','6000 lives',
                                    '1 kin','2 kin','3 kin','6 kin','Entire humans','Entire ET',
                                    '6 paintings','60 paintings','600 paintings','6000 paintings',
                                    '6 dollars','60 dollars','600 dollars','6000 dollars'),
                          domain =c('Human lives','Human lives','Human lives',
                                    'Human lives','Kinship group','Kinship group','Kinship group','Kinship group','Entire human and ET species',
                                    'Entire human and ET species','Public properties','Public properties',
                                    'Public properties','Public properties',
                                    'Personal stock shares','Personal stock shares',
                                    'Personal stock shares','Personal stock shares'),
                          type = c('GPT-4','GPT-4','GPT-4','GPT-4','GPT-4','GPT-4',
                                   'GPT-4','GPT-4','GPT-4','GPT-4','GPT-4','GPT-4',
                                   'GPT-4','GPT-4','GPT-4','GPT-4','GPT-4','GPT-4'),
                            z = numeric(18),
                            p = numeric(18),
                            ci_low = numeric(18),
                            ci_up = numeric(18))

for (i in 1:ncol(P_gpt4)) {
  a_p <- sum(P_gpt4[,i] == 'a', na.rm = TRUE)+1
  b_p <- sum(P_gpt4[,i] == 'b', na.rm = TRUE)+1
  a_n <- sum(N_gpt4[,i] == 'a', na.rm = TRUE)+1
  b_n <- sum(N_gpt4[,i] == 'b', na.rm = TRUE)+1 #plus-one corrections
  cTable_gpt4$z[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$z
  cTable_gpt4$p[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$p
  cTable_gpt4$ci_low[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$ci_low
  cTable_gpt4$ci_up[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$ci_up
}

write.xlsx(cTable_gpt4, 
           file = 'Outputs_study1/WithinAnalysis_GPT4.xlsx',
           rowNames = FALSE)
##############################################################################################################
## Human
p_h <- read_excel('rawdata_study1.xlsx', sheet = 'P_human', na = '---')
n_h <- read_excel('rawdata_study1.xlsx', sheet = 'N_human', na = '---')
P_h<- p_h[,-1]
N_h <- n_h[,-1]

cTable_h <- data.frame(frame = c('6 lives','60 lives','600 lives','6000 lives',
                                 '1 kin','2 kin','3 kin','6 kin','Entire humans','Entire ET',
                                 '6 paintings','60 paintings','600 paintings','6000 paintings',
                                 '6 dollars','60 dollars','600 dollars','6000 dollars'),
                       domain =c('Human lives','Human lives','Human lives',
                                 'Human lives','Kinship group','Kinship group','Kinship group','Kinship group','Entire human and ET species',
                                 'Entire human and ET species','Public properties','Public properties',
                                 'Public properties','Public properties',
                                 'Personal stock shares','Personal stock shares',
                                 'Personal stock shares','Personal stock shares'),
                       type = c('Human','Human','Human','Human','Human','Human',
                                'Human','Human','Human','Human','Human','Human',
                                'Human','Human','Human','Human','Human','Human'),
                       z = numeric(18),
                       p = numeric(18),
                       ci_low = numeric(18),
                       ci_up = numeric(18))

for (i in 1:ncol(P_h)) {
  a_p <- sum(P_h[,i] == 'a', na.rm = TRUE)+1
  b_p <- sum(P_h[,i] == 'b', na.rm = TRUE)+1
  a_n <- sum(N_h[,i] == 'a', na.rm = TRUE)+1
  b_n <- sum(N_h[,i] == 'b', na.rm = TRUE)+1 #plus-one corrections
  cTable_h$z[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$z
  cTable_h$p[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$p
  cTable_h$ci_low[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$ci_low
  cTable_h$ci_up[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$ci_up
}

write.xlsx(cTable_h, 
           file = 'Outputs_study1/WithinAnalysis_Human.xlsx',
           rowNames = FALSE)