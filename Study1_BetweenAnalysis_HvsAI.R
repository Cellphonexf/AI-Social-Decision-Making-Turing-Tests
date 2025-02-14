### GPT social decision-making
### Between-group analysis: Human vs. AI
### Programmed by Feng XIAO (2024.12.1)
### This R script requires one excel file: 'rawdata_study1'

### Preparation
package_list <- c('tidyr','dplyr','readxl','openxlsx','meta')
lapply(package_list, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Analysis
rd <- read_excel('rawdata_study1.xlsx', sheet = 'Summary1', na = '---')
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
# Negative frames
n_gpt3.5 <- rd_gpt3.5 %>% filter(description == 'negative')
n_gpt3.5 <- n_gpt3.5 %>%
  mutate(
    OR = (GPT_gamble / `GPT_sure-thing`) / (H_gamble / `H_sure-thing`),
    log_OR = log(OR),
    SE = sqrt(1 / GPT_gamble + 1 / `GPT_sure-thing` + 1 / H_gamble + 1 / `H_sure-thing`),
  )

n_gpt3.5_aor <- data.frame(
  frame = character(),
  log_AOR = numeric(),
  log_AOR_SE = numeric()
)
frames <- unique(n_gpt3.5$frame)

for (frame in frames) {
  frame_data <- n_gpt3.5 %>% filter(frame == !!frame)
  
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
  
  n_gpt3.5_aor <- rbind(n_gpt3.5_aor, data.frame(
    frame = frame,
    log_AOR = log_AOR,
    log_AOR_SE = log_AOR_SE
  ))
}

n_meta_analysis <- metagen(
  TE = n_gpt3.5_aor$log_AOR,
  seTE = n_gpt3.5_aor$log_AOR_SE,
  studlab = n_gpt3.5_aor$frame, 
  byvar = n_gpt3.5$domain,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL"  #DerSimonian-Laird methods for REM
)
sum_meta <- summary(n_meta_analysis)

frame_n_3.5 <- data.frame(
  domain = n_meta_analysis$byvar,
  frame = n_meta_analysis$studlab,
  log_AOR = n_meta_analysis$TE,
  lower_CI = n_meta_analysis$lower,
  upper_CI = n_meta_analysis$upper
)
frame_n_3.5$description <- 'negative'
frame_n_3.5$subject <- 'GPT-3.5'
subgroup_n_3.5 <- data.frame(
  domain = names(sum_meta$TE.random.w),
  log_AOR = sum_meta$TE.random.w,
  lower_CI = sum_meta$lower.random.w,
  upper_CI = sum_meta$upper.random.w
)
subgroup_n_3.5$description <- 'negative'
subgroup_n_3.5$subject <- 'GPT-3.5'
sum_n_3.5 <- data.frame(
  domain = 'Total',
  log_AOR = sum_meta$TE.random,
  lower_CI = sum_meta$lower.random,
  upper_CI = sum_meta$upper.random,
  description = 'negative',
  subject = 'GPT-3.5'
)
domain_n_3.5 <- rbind(subgroup_n_3.5,sum_n_3.5)
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
# Negative frames
n_gpt4 <- rd_gpt4 %>% filter(description == 'negative')
n_gpt4 <- n_gpt4 %>%
  mutate(
    OR = (GPT_gamble / `GPT_sure-thing`) / (H_gamble / `H_sure-thing`),
    log_OR = log(OR),
    SE = sqrt(1 / GPT_gamble + 1 / `GPT_sure-thing` + 1 / H_gamble + 1 / `H_sure-thing`),
  )

n_gpt4_aor <- data.frame(
  frame = character(),
  log_AOR = numeric(),
  log_AOR_SE = numeric()
)
frames <- unique(n_gpt4$frame)

for (frame in frames) {
  frame_data <- n_gpt4 %>% filter(frame == !!frame)
  
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
  
  n_gpt4_aor <- rbind(n_gpt4_aor, data.frame(
    frame = frame,
    log_AOR = log_AOR,
    log_AOR_SE = log_AOR_SE
  ))
}

n_meta_analysis <- metagen(
  TE = n_gpt4_aor$log_AOR,
  seTE = n_gpt4_aor$log_AOR_SE,
  studlab = n_gpt4_aor$frame, 
  byvar = n_gpt4$domain,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL"  #DerSimonian-Laird methods for REM
)
sum_meta <- summary(n_meta_analysis)

frame_n_4 <- data.frame(
  domain = n_meta_analysis$byvar,
  frame = n_meta_analysis$studlab,
  log_AOR = n_meta_analysis$TE,
  lower_CI = n_meta_analysis$lower,
  upper_CI = n_meta_analysis$upper
)
frame_n_4$description <- 'negative'
frame_n_4$subject <- 'GPT-4'
subgroup_n_4 <- data.frame(
  domain = names(sum_meta$TE.random.w),
  log_AOR = sum_meta$TE.random.w,
  lower_CI = sum_meta$lower.random.w,
  upper_CI = sum_meta$upper.random.w
)
subgroup_n_4$description <- 'negative'
subgroup_n_4$subject <- 'GPT-4'
sum_n_4 <- data.frame(
  domain = 'Total',
  log_AOR = sum_meta$TE.random,
  lower_CI = sum_meta$lower.random,
  upper_CI = sum_meta$upper.random,
  description = 'negative',
  subject = 'GPT-4'
)
domain_n_4 <- rbind(subgroup_n_4,sum_n_4)

## Log OR for each frame
frame_table <- rbind(frame_p_3.5,frame_p_4,frame_n_3.5,frame_n_4)
frame_table$frame <- factor(frame_table$frame, levels =  c('6 lives','60 lives','600 lives','6000 lives',
                                                           '1 kin','2 kin','3 kin','6 kin','Entire humans','Entire ETs',
                                                           '6 paintings','60 paintings','600 paintings','6000 paintings',
                                                           '6 dollars','60 dollars','600 dollars','6000 dollars'))
frame_table$domain <- factor(frame_table$domain, levels = c('Human lives','Kinship group',
                                                            'Entire human and ET species','Public properties',
                                                            'Personal stock shares'))
frame_table$subject <- factor(frame_table$subject, levels = c('GPT-3.5','GPT-4'))
frame_table$description <- factor(frame_table$description, levels = c('positive','negative'))

## Log OR for each domain
domain_table <- rbind(domain_p_3.5,domain_p_4,domain_n_3.5,domain_n_4)
domain_table <- domain_table %>% filter(domain != 'Total')
domain_table$frame <-c('Total1','Total2','Total3','Total4','Total5',
                       'Total1','Total2','Total3','Total4','Total5',
                       'Total1','Total2','Total3','Total4','Total5',
                       'Total1','Total2','Total3','Total4','Total5')
domain_table$domain <- factor(domain_table$domain, levels = c('Human lives','Kinship group',
                                                              'Entire human and ET species','Public properties',
                                                              'Personal stock shares'))
domain_table$subject <- factor(domain_table$subject, levels = c('GPT-3.5','GPT-4'))
domain_table$description <- factor(domain_table$description, levels = c('positive','negative'))
## Data output and saved
ef_table <- rbind(frame_table,domain_table)
write.xlsx(ef_table, file = 'Outputs_study1/BetweenAnalysis (AI-Humans).xlsx',
           rowNames = FALSE)