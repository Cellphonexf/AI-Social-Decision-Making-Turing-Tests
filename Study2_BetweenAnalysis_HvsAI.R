### GPT social decision-making
### Between-group analysis: Human vs. AI
### Programmed by Feng XIAO (2024.12.6)
### This R script requires one excel file: 'rawdata_study2'

### Preparation
package_list <- c('tidyr','dplyr','readxl','openxlsx','meta')
lapply(package_list, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Analysis
rd <- read_excel('rawdata_study2.xlsx', sheet = 'Summary1', na = '---')
epsilon <- 1 #continuity correction
## GPT-3.5
rd_gpt3.5 <- rd %>% filter(model == 'GPT-3.5')
rd_gpt3.5 <- rd_gpt3.5 %>%
  mutate(
    GPT_gamble = GPT_gamble + epsilon,
    `GPT_sure-thing` = `GPT_sure-thing` + epsilon,
    H_gamble = H_gamble + epsilon,
    `H_sure-thing` = `H_sure-thing` + epsilon
  )
# Positive frames
p_gpt3.5 <- rd_gpt3.5 %>% filter(description == 'positive')
p_gpt3.5 <- p_gpt3.5 %>%
  mutate(
    OR = (GPT_gamble / `GPT_sure-thing`) / (H_gamble / `H_sure-thing`),
    log_OR = log(OR),
    SE = sqrt(1 / GPT_gamble + 1 / `GPT_sure-thing` + 1 / H_gamble + 1 / `H_sure-thing`),
  )

p_gpt3.5_aor <- data.frame(
  frame = character(),
  log_AOR = numeric(),
  log_AOR_SE = numeric()
)
frames <- unique(p_gpt3.5$frame)

for (frame in frames) {
  frame_data <- p_gpt3.5 %>% filter(frame == !!frame)
  
  meta_analysis <- metagen(
    TE = frame_data$log_OR, #logOR for analysis
    seTE = frame_data$SE,
    studlab = frame_data$frame,
    comb.fixed = FALSE,
    comb.random = TRUE,
    method.tau = "DL"
  ) # Perform meta-analysis for the current frame
  
  log_AOR <- meta_analysis$TE.random
  log_AOR_SE <- meta_analysis$seTE.random
  
  p_gpt3.5_aor <- rbind(p_gpt3.5_aor, data.frame(
    frame = frame,
    log_AOR = log_AOR,
    log_AOR_SE = log_AOR_SE
  ))
}

p_meta_analysis <- metagen(
  TE = p_gpt3.5_aor$log_AOR,
  seTE = p_gpt3.5_aor$log_AOR_SE,
  studlab = p_gpt3.5_aor$frame, 
  byvar = p_gpt3.5$domain,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL"  #DerSimonian-Laird methods for REM
)
sum_meta <- summary(p_meta_analysis)

frame_p_3.5 <- data.frame(
  domain = p_meta_analysis$byvar,
  frame = p_meta_analysis$studlab,
  log_AOR = p_meta_analysis$TE,
  lower_CI = p_meta_analysis$lower,
  upper_CI = p_meta_analysis$upper
)
frame_p_3.5$description <- 'positive'
frame_p_3.5$subject <- 'GPT-3.5'
subgroup_p_3.5 <- data.frame(
  domain = names(sum_meta$TE.random.w),
  log_AOR = sum_meta$TE.random.w,
  lower_CI = sum_meta$lower.random.w,
  upper_CI = sum_meta$upper.random.w
)
subgroup_p_3.5$description <- 'positive'
subgroup_p_3.5$subject <- 'GPT-3.5'
sum_p_3.5 <- data.frame(
  domain = 'Total',
  log_AOR = sum_meta$TE.random,
  lower_CI = sum_meta$lower.random,
  upper_CI = sum_meta$upper.random,
  description = 'positive',
  subject = 'GPT-3.5'
)
domain_p_3.5 <- rbind(subgroup_p_3.5,sum_p_3.5)
## GPT-4
rd_gpt4 <- rd %>% filter(model == 'GPT-4')
rd_gpt4 <- rd_gpt4 %>%
  mutate(
    GPT_gamble = GPT_gamble + epsilon,
    `GPT_sure-thing` = `GPT_sure-thing` + epsilon,
    H_gamble = H_gamble + epsilon,
    `H_sure-thing` = `H_sure-thing` + epsilon
  )
# Positive frames
p_gpt4 <- rd_gpt4 %>% filter(description == 'positive')
p_gpt4 <- p_gpt4 %>%
  mutate(
    OR = (GPT_gamble / `GPT_sure-thing`) / (H_gamble / `H_sure-thing`),
    log_OR = log(OR),
    SE = sqrt(1 / GPT_gamble + 1 / `GPT_sure-thing` + 1 / H_gamble + 1 / `H_sure-thing`),
  )

p_gpt4_aor <- data.frame(
  frame = character(),
  log_AOR = numeric(),
  log_AOR_SE = numeric()
)
frames <- unique(p_gpt4$frame)

for (frame in frames) {
  frame_data <- p_gpt4 %>% filter(frame == !!frame)
  
  meta_analysis <- metagen(
    TE = frame_data$log_OR, #logOR for analysis
    seTE = frame_data$SE,
    studlab = frame_data$frame,
    comb.fixed = FALSE,
    comb.random = TRUE,
    method.tau = "DL"
  ) # Perform meta-analysis for the current frame
  
  log_AOR <- meta_analysis$TE.random
  log_AOR_SE <- meta_analysis$seTE.random
  
  p_gpt4_aor <- rbind(p_gpt4_aor, data.frame(
    frame = frame,
    log_AOR = log_AOR,
    log_AOR_SE = log_AOR_SE
  ))
}

p_meta_analysis <- metagen(
  TE = p_gpt4_aor$log_AOR,
  seTE = p_gpt4_aor$log_AOR_SE,
  studlab = p_gpt4_aor$frame, 
  byvar = p_gpt4$domain,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL"  #DerSimonian-Laird methods for REM
)
sum_meta <- summary(p_meta_analysis)

frame_p_4 <- data.frame(
  domain = p_meta_analysis$byvar,
  frame = p_meta_analysis$studlab,
  log_AOR = p_meta_analysis$TE,
  lower_CI = p_meta_analysis$lower,
  upper_CI = p_meta_analysis$upper
)
frame_p_4$description <- 'positive'
frame_p_4$subject <- 'GPT-4'
subgroup_p_4 <- data.frame(
  domain = names(sum_meta$TE.random.w),
  log_AOR = sum_meta$TE.random.w,
  lower_CI = sum_meta$lower.random.w,
  upper_CI = sum_meta$upper.random.w
)
subgroup_p_4$description <- 'positive'
subgroup_p_4$subject <- 'GPT-4'
sum_p_4 <- data.frame(
  domain = 'Total',
  log_AOR = sum_meta$TE.random,
  lower_CI = sum_meta$lower.random,
  upper_CI = sum_meta$upper.random,
  description = 'positive',
  subject = 'GPT-4'
)
domain_p_4 <- rbind(subgroup_p_4,sum_p_4)

## Log OR for each frame
frame_table <- rbind(frame_p_3.5,frame_p_4)
frame_table$frame <- factor(frame_table$frame, levels =  c('6 lives (fairness)','600 lives (fairness)',
                                                           'YSO-6 kin','YSY-6 kin','MSO-6 kin',
                                                           'MSY-6 kin', '6 kin', '6 lives', '600 lives',
                                                           '6 kin (two-thirds)', '6 lives (two-thirds)', '600 lives (two-thirds)'))
frame_table$domain <- factor(frame_table$domain, levels = c('Sense of fairness', 'Young: old vs. young kin',
                                                            'Middle-aged: old vs. young kin','Group-dependent & goal settings (inequal EV)',
                                                            'Group-dependent & goal settings (equal EV)'))
frame_table$subject <- factor(frame_table$subject, levels = c('GPT-3.5','GPT-4'))

## Log OR for each domain
domain_table <- rbind(domain_p_3.5,domain_p_4)
domain_table <- domain_table %>% filter(domain != 'Total')
domain_table$frame <-c('Total1','Total2','Total3','Total4','Total5',
                       'Total1','Total2','Total3','Total4','Total5')
domain_table$domain <- factor(domain_table$domain, levels = c('Sense of fairness', 'Young: old vs. young kin',
                                                              'Middle-aged: old vs. young kin','Group-dependent & goal settings (inequal EV)',
                                                              'Group-dependent & goal settings (equal EV)'))
domain_table$subject <- factor(domain_table$subject, levels = c('GPT-3.5','GPT-4'))
ef_table <- rbind(frame_table,domain_table)
write.xlsx(ef_table, file = 'Outputs_study2/BetweenAnalysis (AI-Humans).xlsx',
           rowNames = FALSE)