### GPT social decision-making
### Between-group analysis: Human vs. AI
### Programmed by Feng XIAO (2025.7.20)
### This R script requires one excel file: 'rawdata_replication'

### Preparation
package_list <- c('tidyr','dplyr','readxl','openxlsx','meta')
lapply(package_list, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Analysis
rd <- read_excel('rawdata_replication.xlsx', sheet = 'Summary1', na = '---')
epsilon <- 1 #continuity correction
## GPT-4o
rd_gpt4o <- rd %>% filter(model == 'GPT-4o')
rd_gpt4o <- rd_gpt4o %>%
  mutate(
    GPT_gamble = GPT_gamble + epsilon,
    `GPT_sure-thing` = `GPT_sure-thing` + epsilon,
    H_gamble = H_gamble + epsilon,
    `H_sure-thing` = `H_sure-thing` + epsilon
  )
# Positive frames
p_gpt4o <- rd_gpt4o %>% filter(description == 'positive')
p_gpt4o <- p_gpt4o %>%
  mutate(
    OR = (GPT_gamble / `GPT_sure-thing`) / (H_gamble / `H_sure-thing`),
    log_OR = log(OR),
    SE = sqrt(1 / GPT_gamble + 1 / `GPT_sure-thing` + 1 / H_gamble + 1 / `H_sure-thing`),
  )

p_gpt4o_aor <- data.frame(
  frame = character(),
  log_AOR = numeric(),
  log_AOR_SE = numeric()
)
frames <- unique(p_gpt4o$frame)

for (frame in frames) {
  frame_data <- p_gpt4o %>% filter(frame == !!frame)
  
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
  
  p_gpt4o_aor <- rbind(p_gpt4o_aor, data.frame(
    frame = frame,
    log_AOR = log_AOR,
    log_AOR_SE = log_AOR_SE
  ))
}

p_meta_analysis <- metagen(
  TE = p_gpt4o_aor$log_AOR,
  seTE = p_gpt4o_aor$log_AOR_SE,
  studlab = p_gpt4o_aor$frame, 
  byvar = p_gpt4o$domain,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL"  #DerSimonian-Laird methods for REM
)
sum_meta <- summary(p_meta_analysis)

frame_p_4o <- data.frame(
  domain = p_meta_analysis$byvar,
  frame = p_meta_analysis$studlab,
  log_AOR = p_meta_analysis$TE,
  lower_CI = p_meta_analysis$lower,
  upper_CI = p_meta_analysis$upper
)
frame_p_4o$description <- 'positive'
frame_p_4o$subject <- 'GPT-4o'
subgroup_p_4o <- data.frame(
  domain = names(sum_meta$TE.random.w),
  log_AOR = sum_meta$TE.random.w,
  lower_CI = sum_meta$lower.random.w,
  upper_CI = sum_meta$upper.random.w
)
subgroup_p_4o$description <- 'positive'
subgroup_p_4o$subject <- 'GPT-4o'
sum_p_4o <- data.frame(
  domain = 'Total',
  log_AOR = sum_meta$TE.random,
  lower_CI = sum_meta$lower.random,
  upper_CI = sum_meta$upper.random,
  description = 'positive',
  subject = 'GPT-4o'
)
domain_p_4o <- rbind(subgroup_p_4o,sum_p_4o)
# Negative frames
n_gpt4o <- rd_gpt4o %>% filter(description == 'negative')
n_gpt4o <- n_gpt4o %>%
  mutate(
    OR = (GPT_gamble / `GPT_sure-thing`) / (H_gamble / `H_sure-thing`),
    log_OR = log(OR),
    SE = sqrt(1 / GPT_gamble + 1 / `GPT_sure-thing` + 1 / H_gamble + 1 / `H_sure-thing`),
  )

n_gpt4o_aor <- data.frame(
  frame = character(),
  log_AOR = numeric(),
  log_AOR_SE = numeric()
)
frames <- unique(n_gpt4o$frame)

for (frame in frames) {
  frame_data <- n_gpt4o %>% filter(frame == !!frame)
  
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
  
  n_gpt4o_aor <- rbind(n_gpt4o_aor, data.frame(
    frame = frame,
    log_AOR = log_AOR,
    log_AOR_SE = log_AOR_SE
  ))
}

n_meta_analysis <- metagen(
  TE = n_gpt4o_aor$log_AOR,
  seTE = n_gpt4o_aor$log_AOR_SE,
  studlab = n_gpt4o_aor$frame, 
  byvar = n_gpt4o$domain,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL"  #DerSimonian-Laird methods for REM
)
sum_meta <- summary(n_meta_analysis)

frame_n_4o <- data.frame(
  domain = n_meta_analysis$byvar,
  frame = n_meta_analysis$studlab,
  log_AOR = n_meta_analysis$TE,
  lower_CI = n_meta_analysis$lower,
  upper_CI = n_meta_analysis$upper
)
frame_n_4o$description <- 'negative'
frame_n_4o$subject <- 'GPT-4o'
subgroup_n_4o <- data.frame(
  domain = names(sum_meta$TE.random.w),
  log_AOR = sum_meta$TE.random.w,
  lower_CI = sum_meta$lower.random.w,
  upper_CI = sum_meta$upper.random.w
)
subgroup_n_4o$description <- 'negative'
subgroup_n_4o$subject <- 'GPT-4o'
sum_n_4o <- data.frame(
  domain = 'Total',
  log_AOR = sum_meta$TE.random,
  lower_CI = sum_meta$lower.random,
  upper_CI = sum_meta$upper.random,
  description = 'negative',
  subject = 'GPT-4o'
)
domain_n_4o <- rbind(subgroup_n_4o,sum_n_4o)

## Log OR for each frame
frame_table <- rbind(frame_p_4o,frame_n_4o)
frame_table$frame <- factor(frame_table$frame, levels =  c('6 lives','60 lives','600 lives','6000 lives',
                                                           '1 kin','2 kin','3 kin','6 kin'))
frame_table$domain <- factor(frame_table$domain, levels = c('Human lives','Kinship group'))
frame_table$description <- factor(frame_table$description, levels = c('positive','negative'))

## Log OR for each domain
domain_table <- rbind(domain_p_4o,domain_n_4o)
domain_table <- domain_table %>% filter(domain != 'Total')
domain_table$frame <-c('Total1','Total2','Total1','Total2')
domain_table$domain <- factor(domain_table$domain, levels = c('Human lives','Kinship group'))
domain_table$description <- factor(domain_table$description, levels = c('positive','negative'))

## Benjamini-Hochberg corrections for p-values
frame_table <- frame_table %>%
  mutate(
    p_value = 2 * (1 - pnorm(abs(log_AOR / log_AOR_SE)))
  )
domain_table <- domain_table %>%
  mutate(
    p_value = 2 * (1 - pnorm(abs(log_AOR / log_AOR_SE)))
  )
frame_table <- frame_table %>%
  group_by(domain, description) %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  ) %>%
  ungroup()
domain_table <- domain_table %>%
  group_by(domain, description) %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  ) %>%
  ungroup()

## Data output and saved
ef_table <- rbind(frame_table,domain_table)
write.xlsx(ef_table, file = 'Outputs_replication/BetweenAnalysis (AI-Humans).xlsx',
           rowNames = FALSE)