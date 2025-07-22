### GPT social decision-making
### Within-group analysis: proportion z-tests
### Programmed by Feng XIAO (2025.7.20)
### This R script requires one excel file: 'rawdata_replication'

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

### Analysis: framing
########################################################################################################
## GPT-4o
p_gpt4o <- read_excel('rawdata_replication.xlsx', sheet = 'P_gpt4o', na = '---')
n_gpt4o <- read_excel('rawdata_replication.xlsx', sheet = 'N_gpt4o', na = '---')
P_gpt4o <- p_gpt4o[,-1]
N_gpt4o <- n_gpt4o[,-1]

cTable_gpt4o <- data.frame(frame = c('6 lives','60 lives','600 lives','6000 lives',
                                      '1 kin','2 kin','3 kin','6 kin'),
                            domain =c('Human lives','Human lives','Human lives',
                                      'Human lives','Kinship group','Kinship group','Kinship group','Kinship group'),
                            type = c('GPT-4o','GPT-4o','GPT-4o','GPT-4o','GPT-4o','GPT-4o',
                                     'GPT-4o','GPT-4o'),
                      z = numeric(8),
                      p = numeric(8),
                      ci_low = numeric(8),
                      ci_up = numeric(8))

for (i in 1:ncol(P_gpt4o)) {
  a_p <- sum(P_gpt4o[,i] == 'a', na.rm = TRUE)+1
  b_p <- sum(P_gpt4o[,i] == 'b', na.rm = TRUE)+1
  a_n <- sum(N_gpt4o[,i] == 'a', na.rm = TRUE)+1
  b_n <- sum(N_gpt4o[,i] == 'b', na.rm = TRUE)+1 #plus-one corrections
  cTable_gpt4o$z[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$z
  cTable_gpt4o$p[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$p
  cTable_gpt4o$ci_low[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$ci_low
  cTable_gpt4o$ci_up[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$ci_up
}

cTable_gpt4o$p_adj <- p.adjust(cTable_gpt4o$p, method = "BH")
  #Benjamini¨CHochberg procedure for multiple comparison corrections

write.xlsx(cTable_gpt4o, 
           file = 'Outputs_replication/WithinAnalysis_GPT4o.xlsx',
           rowNames = FALSE)

### Analysis: scenario
########################################################################################################
## GPT-4o
# Human lives: 6, 60, 600, 6000 lives
HL_gpt4o <- read_excel('rawdata_replication.xlsx', sheet = 'HL_gpt4o', na = '---')
summary_data <- HL_gpt4o %>%
  group_by(Group_size) %>%
  summarise(
    Count_b = sum(Option == "b"), #risk-seeking
    Total = n(),
    .groups = 'drop'
  )
binom_results <- summary_data %>%
  rowwise() %>%
  mutate(
    z_test = list(z.prop(Count_b, Total / 2, Total, Total)),
    z = z_test$z,
    p_value = z_test$p,
    ci_low = z_test$ci_low,
    ci_up = z_test$ci_up
  ) %>%
  select(Group_size, Count_b, Total, z, p_value, ci_low, ci_up) #comparisons with 50-50 for each scenario
group_pairs <- combn(unique(summary_data$Group_size), 2, simplify = FALSE)
pairwise_results <- lapply(group_pairs, function(pair) {
  data1 <- summary_data %>% filter(Group_size == pair[1])
  data2 <- summary_data %>% filter(Group_size == pair[2])
  
  res <- z.prop(data1$Count_b, data2$Count_b, data1$Total, data2$Total)
  
  data.frame(
    Group1 = pair[1],
    Group2 = pair[2],
    z = res$z,
    p_value = res$p,
    ci_low = res$ci_low,
    ci_up = res$ci_up
  )
}) %>%
  bind_rows()

binom_results <- binom_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini¨CHochberg procedure for multiple comparison corrections (vs. 50-50)

pairwise_results <- pairwise_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini¨CHochberg procedure for multiple comparison corrections (pairwise)

write.xlsx(binom_results,
           file = 'Outputs_replication/WithinAnalysis_GPT4o_ScenarioHL.xlsx',
           rowNames = FALSE)
write.xlsx(pairwise_results,
           file = 'Outputs_replication/WithinAnalysis_GPT4o_PairwiseHL.xlsx',
           rowNames = FALSE)