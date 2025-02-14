### GPT social decision-making
### Between-group analysis: GPT4 vs. GPT3.5
### Programmed by Feng XIAO (2024.12.6)
### This R script requires one excel file: 'rawdata_study2'

### Preparation
package_list <- c('tidyr','dplyr','readxl','openxlsx','meta')
lapply(package_list, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Analysis
rd <- read_excel('rawdata_study2.xlsx', sheet = 'Summary2', na = '---')
epsilon <- 1 #continuity correction
rd_gpt <- rd %>%
  mutate(
    GPT4_gamble = GPT4_gamble + epsilon,
    `GPT4_sure-thing` = `GPT4_sure-thing` + epsilon,
    GPT3.5_gamble = GPT3.5_gamble + epsilon,
    `GPT3.5_sure-thing` = `GPT3.5_sure-thing` + epsilon
  )
# Positive frames
p_gpt <- rd_gpt %>% filter(description == 'positive')
p_gpt <- p_gpt %>%
  mutate(
    OR = (GPT4_gamble / `GPT4_sure-thing`) / (GPT3.5_gamble / `GPT3.5_sure-thing`),
    log_OR = log(OR),
    SE = sqrt(1 / GPT4_gamble + 1 / `GPT4_sure-thing` + 1 / GPT3.5_gamble + 1 / `GPT3.5_sure-thing`),
  )

p_gpt_aor <- data.frame(
  frame = character(),
  log_AOR = numeric(),
  log_AOR_SE = numeric()
)
frames <- unique(p_gpt$frame)

for (frame in frames) {
  frame_data <- p_gpt %>% filter(frame == !!frame)
  
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
  
  p_gpt_aor <- rbind(p_gpt_aor, data.frame(
    frame = frame,
    log_AOR = log_AOR,
    log_AOR_SE = log_AOR_SE
  ))
}

p_meta_analysis <- metagen(
  TE = p_gpt_aor$log_AOR,
  seTE = p_gpt_aor$log_AOR_SE,
  studlab = p_gpt_aor$frame, 
  byvar = p_gpt$domain,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL"  #DerSimonian-Laird methods for REM
)
sum_meta <- summary(p_meta_analysis)

frame_p <- data.frame(
  domain = p_meta_analysis$byvar,
  frame = p_meta_analysis$studlab,
  log_AOR = p_meta_analysis$TE,
  lower_CI = p_meta_analysis$lower,
  upper_CI = p_meta_analysis$upper
)
frame_p$description <- 'positive'
subgroup_p <- data.frame(
  domain = names(sum_meta$TE.random.w),
  log_AOR = sum_meta$TE.random.w,
  lower_CI = sum_meta$lower.random.w,
  upper_CI = sum_meta$upper.random.w
)
subgroup_p$description <- 'positive'
sum_p <- data.frame(
  domain = 'Total',
  log_AOR = sum_meta$TE.random,
  lower_CI = sum_meta$lower.random,
  upper_CI = sum_meta$upper.random,
  description = 'positive'
)
domain_p <- rbind(subgroup_p,sum_p)

## Log OR for each frame
frame_p$frame <- factor(frame_p$frame, levels =  c('6 lives (fairness)','600 lives (fairness)',
                                                   'YSO-6 kin','YSY-6 kin','MSO-6 kin',
                                                   'MSY-6 kin', '6 kin', '6 lives', '600 lives',
                                                   '6 kin (two-thirds)', '6 lives (two-thirds)', '600 lives (two-thirds)'))
frame_p$domain <- factor(frame_p$domain, levels = c('Sense of fairness', 'Young: old vs. young kin',
                                                    'Middle-aged: old vs. young kin','Group-dependent & goal settings (inequal EV)',
                                                    'Group-dependent & goal settings (equal EV)'))

## Log OR for each domain
domain_p <- domain_p %>% filter(domain != 'Total')
domain_p$frame <-c('Total1','Total2','Total3','Total4','Total5')
domain_p$domain <- factor(domain_p$domain, levels = c('Sense of fairness', 'Young: old vs. young kin',
                                                      'Middle-aged: old vs. young kin','Group-dependent & goal settings (inequal EV)',
                                                      'Group-dependent & goal settings (equal EV)'))
ef_table <- rbind(frame_p,domain_p)
write.xlsx(ef_table, file = 'Outputs_study2/BetweenAnalysis (GPT4-GPT3.5).xlsx',
           rowNames = FALSE)