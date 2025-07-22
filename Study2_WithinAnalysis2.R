### GPT social decision-making
### Within-group analysis 2: proportion z-tests
### Programmed by Feng XIAO (2025.7.20)
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

cTable_3.5 <- data.frame(
  comparison = c('6kin vs 6lives', '6lives vs 600lives', '6kin vs 600lives'),
  z = numeric(3),
  p = numeric(3),
  ci_low = numeric(3),
  ci_up = numeric(3)
)

a_p_7 <- sum(Gpt3.5[,7] == 'a', na.rm = TRUE) + 1
b_p_7 <- sum(Gpt3.5[,7] == 'b', na.rm = TRUE) + 1
a_p_8 <- sum(Gpt3.5[,8] == 'a', na.rm = TRUE) + 1
b_p_8 <- sum(Gpt3.5[,8] == 'b', na.rm = TRUE) + 1
a_p_9 <- sum(Gpt3.5[,9] == 'a', na.rm = TRUE) + 1
b_p_9 <- sum(Gpt3.5[,9] == 'b', na.rm = TRUE) + 1

# 6kin vs 6 lives
result_7_8 <- z.prop(b_p_7, b_p_8, a_p_7 + b_p_7, a_p_8 + b_p_8)
cTable_3.5$z[1] <- result_7_8$z
cTable_3.5$p[1] <- result_7_8$p
cTable_3.5$ci_low[1] <- result_7_8$ci_low
cTable_3.5$ci_up[1] <- result_7_8$ci_up

# 6lives vs 600lives
result_8_9 <- z.prop(b_p_8, b_p_9, a_p_8 + b_p_8, a_p_9 + b_p_9)
cTable_3.5$z[2] <- result_8_9$z
cTable_3.5$p[2] <- result_8_9$p
cTable_3.5$ci_low[2] <- result_8_9$ci_low
cTable_3.5$ci_up[2] <- result_8_9$ci_up

# 6kin vs 600lives
result_7_9 <- z.prop(b_p_7, b_p_9, a_p_7 + b_p_7, a_p_9 + b_p_9)
cTable_3.5$z[3] <- result_7_9$z
cTable_3.5$p[3] <- result_7_9$p
cTable_3.5$ci_low[3] <- result_7_9$ci_low
cTable_3.5$ci_up[3] <- result_7_9$ci_up

cTable_3.5 <- cTable_3.5 %>%
  mutate(
    p_adj = p.adjust(p, method = "BH")
  )
  #Benjamini¨CHochberg procedure for multiple comparison corrections

write.xlsx(cTable_3.5,
           file = 'Outputs_study2/WithinAnalysis_GPT3.5_1vs2.xlsx',
           rowNames = FALSE)
########################################################################################################
## GPT-4
gpt4 <- read_excel('rawdata_study2.xlsx', sheet = 'gpt4', na = '---')
Gpt4 <- gpt4[,-1]

cTable_4 <- data.frame(
  comparison = c('6kin vs 6lives', '6lives vs 600lives', '6kin vs 600lives'),
  z = numeric(3),
  p = numeric(3),
  ci_low = numeric(3),
  ci_up = numeric(3)
)

a_p_7 <- sum(Gpt4[,7] == 'a', na.rm = TRUE) + 1
b_p_7 <- sum(Gpt4[,7] == 'b', na.rm = TRUE) + 1
a_p_8 <- sum(Gpt4[,8] == 'a', na.rm = TRUE) + 1
b_p_8 <- sum(Gpt4[,8] == 'b', na.rm = TRUE) + 1
a_p_9 <- sum(Gpt4[,9] == 'a', na.rm = TRUE) + 1
b_p_9 <- sum(Gpt4[,9] == 'b', na.rm = TRUE) + 1

# 6kin vs 6 lives
result_7_8 <- z.prop(b_p_7, b_p_8, a_p_7 + b_p_7, a_p_8 + b_p_8)
cTable_4$z[1] <- result_7_8$z
cTable_4$p[1] <- result_7_8$p
cTable_4$ci_low[1] <- result_7_8$ci_low
cTable_4$ci_up[1] <- result_7_8$ci_up

# 6lives vs 600lives
result_8_9 <- z.prop(b_p_8, b_p_9, a_p_8 + b_p_8, a_p_9 + b_p_9)
cTable_4$z[2] <- result_8_9$z
cTable_4$p[2] <- result_8_9$p
cTable_4$ci_low[2] <- result_8_9$ci_low
cTable_4$ci_up[2] <- result_8_9$ci_up

# 6kin vs 600lives
result_7_9 <- z.prop(b_p_7, b_p_9, a_p_7 + b_p_7, a_p_9 + b_p_9)
cTable_4$z[3] <- result_7_9$z
cTable_4$p[3] <- result_7_9$p
cTable_4$ci_low[3] <- result_7_9$ci_low
cTable_4$ci_up[3] <- result_7_9$ci_up

cTable_4 <- cTable_4 %>%
  mutate(
    p_adj = p.adjust(p, method = "BH")
  )
#Benjamini¨CHochberg procedure for multiple comparison corrections

write.xlsx(cTable_4,
           file = 'Outputs_study2/WithinAnalysis_GPT4_1vs2.xlsx',
           rowNames = FALSE)
########################################################################################################
## Human
human <- read_excel('rawdata_study2.xlsx', sheet = 'human', na = '---')
Human <- human[,-1]

cTable_h <- data.frame(
  comparison = c('6kin vs 6lives', '6lives vs 600lives', '6kin vs 600lives'),
  z = numeric(3),
  p = numeric(3),
  ci_low = numeric(3),
  ci_up = numeric(3)
)

a_p_7 <- sum(Human[,7] == 'a', na.rm = TRUE) + 1
b_p_7 <- sum(Human[,7] == 'b', na.rm = TRUE) + 1
a_p_8 <- sum(Human[,8] == 'a', na.rm = TRUE) + 1
b_p_8 <- sum(Human[,8] == 'b', na.rm = TRUE) + 1
a_p_9 <- sum(Human[,9] == 'a', na.rm = TRUE) + 1
b_p_9 <- sum(Human[,9] == 'b', na.rm = TRUE) + 1

# 6kin vs 6 lives
result_7_8 <- z.prop(b_p_7, b_p_8, a_p_7 + b_p_7, a_p_8 + b_p_8)
cTable_h$z[1] <- result_7_8$z
cTable_h$p[1] <- result_7_8$p
cTable_h$ci_low[1] <- result_7_8$ci_low
cTable_h$ci_up[1] <- result_7_8$ci_up

# 6lives vs 600lives
result_8_9 <- z.prop(b_p_8, b_p_9, a_p_8 + b_p_8, a_p_9 + b_p_9)
cTable_h$z[2] <- result_8_9$z
cTable_h$p[2] <- result_8_9$p
cTable_h$ci_low[2] <- result_8_9$ci_low
cTable_h$ci_up[2] <- result_8_9$ci_up

# 6kin vs 600lives
result_7_9 <- z.prop(b_p_7, b_p_9, a_p_7 + b_p_7, a_p_9 + b_p_9)
cTable_h$z[3] <- result_7_9$z
cTable_h$p[3] <- result_7_9$p
cTable_h$ci_low[3] <- result_7_9$ci_low
cTable_h$ci_up[3] <- result_7_9$ci_up

cTable_h <- cTable_h %>%
  mutate(
    p_adj = p.adjust(p, method = "BH")
  )
#Benjamini¨CHochberg procedure for multiple comparison corrections

write.xlsx(cTable_h,
           file = 'Outputs_study2/WithinAnalysis_Human_1vs2.xlsx',
           rowNames = FALSE)