### GPT social decision-making
### Between-group analysis: GPT-4 vs. GPT-3.5
### Programmed by Feng XIAO (2024.12.1)
### This R script requires one excel file: 'rawdata_study1'

### Preparation
package_list <- c('tidyr','dplyr','readxl','openxlsx','meta')
lapply(package_list, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Analysis
rd <- read_excel('rawdata_study1.xlsx', sheet = 'Summary2', na = '---')
epsilon <- 1 #continuity correction
rd <- rd %>%
  mutate(
    GPT4_gamble = GPT4_gamble + epsilon,
    `GPT4_sure-thing` = `GPT4_sure-thing` + epsilon,
    GPT3.5_gamble = GPT3.5_gamble + epsilon,
    `GPT3.5_sure-thing` = `GPT3.5_sure-thing` + epsilon
  )
# Positive frames
p_gpt <- rd %>% filter(description == 'positive')
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
# Negative frames
n_gpt <- rd %>% filter(description == 'negative')
n_gpt <- n_gpt %>%
  mutate(
    OR = (GPT4_gamble / `GPT4_sure-thing`) / (GPT3.5_gamble / `GPT3.5_sure-thing`),
    log_OR = log(OR),
    SE = sqrt(1 / GPT4_gamble + 1 / `GPT4_sure-thing` + 1 / GPT3.5_gamble + 1 / `GPT3.5_sure-thing`),
  )

n_gpt_aor <- data.frame(
  frame = character(),
  log_AOR = numeric(),
  log_AOR_SE = numeric()
)
frames <- unique(n_gpt$frame)

for (frame in frames) {
  frame_data <- n_gpt %>% filter(frame == !!frame)
  
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
  
  n_gpt_aor <- rbind(n_gpt_aor, data.frame(
    frame = frame,
    log_AOR = log_AOR,
    log_AOR_SE = log_AOR_SE
  ))
}

n_meta_analysis <- metagen(
  TE = n_gpt_aor$log_AOR,
  seTE = n_gpt_aor$log_AOR_SE,
  studlab = n_gpt_aor$frame, 
  byvar = n_gpt$domain,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL"  #DerSimonian-Laird methods for REM
)
sum_meta <- summary(n_meta_analysis)

frame_n <- data.frame(
  domain = n_meta_analysis$byvar,
  frame = n_meta_analysis$studlab,
  log_AOR = n_meta_analysis$TE,
  lower_CI = n_meta_analysis$lower,
  upper_CI = n_meta_analysis$upper
)
frame_n$description <- 'negative'
subgroup_n <- data.frame(
  domain = names(sum_meta$TE.random.w),
  log_AOR = sum_meta$TE.random.w,
  lower_CI = sum_meta$lower.random.w,
  upper_CI = sum_meta$upper.random.w
)
subgroup_n$description <- 'negative'
sum_n <- data.frame(
  domain = 'Total',
  log_AOR = sum_meta$TE.random,
  lower_CI = sum_meta$lower.random,
  upper_CI = sum_meta$upper.random,
  description = 'negative'
)
domain_n <- rbind(subgroup_n,sum_n)

## Log OR for each frame
frame_table <- rbind(frame_p,frame_n)
frame_table$frame <- factor(frame_table$frame, levels =  c('6 lives','60 lives','600 lives','6000 lives',
                                                           '1 kin','2 kin','3 kin','6 kin','Entire humans','Entire ETs',
                                                           '6 paintings','60 paintings','600 paintings','6000 paintings',
                                                           '6 dollars','60 dollars','600 dollars','6000 dollars'))
frame_table$domain <- factor(frame_table$domain, levels = c('Human lives','Kinship group',
                                                            'Entire human and ET species','Public properties',
                                                            'Personal stock shares'))
frame_table$description <- factor(frame_table$description, levels = c('positive','negative'))

## Log OR for each domain
domain_table <- rbind(domain_p,domain_n)
domain_table <- domain_table %>% filter(domain != 'Total')
domain_table$frame <-c('Total1','Total2','Total3','Total4','Total5',
                       'Total1','Total2','Total3','Total4','Total5')
domain_table$domain <- factor(domain_table$domain, levels = c('Human lives','Kinship group',
                                                              'Entire human and ET species','Public properties',
                                                              'Personal stock shares'))
domain_table$description <- factor(domain_table$description, levels = c('positive','negative'))
## Data output and saved
ef_table <- rbind(frame_table,domain_table)
write.xlsx(ef_table, file = 'Outputs_study1/BetweenAnalysis (GPT4-GPT3.5).xlsx',
           rowNames = FALSE)