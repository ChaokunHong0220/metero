# 使用metero包处理包含抗生素缩写的AMR数据示例
library(metero)
library(dplyr)

# 创建模拟数据 - 南亚人类AMR数据
south_asia_amr <- data.frame(
  year = rep(2020, 10),
  region = rep("South Asia", 10),
  country = rep("India", 10),
  r_AML_Ecoli = c(0.45), # Amoxyclav (AML) 对 E.coli 的耐药率
  n_AML_Ecoli = c(200),  # Amoxyclav (AML) 对 E.coli 的总样本数
  d_AML_Ecoli = c(90),   # Amoxyclav (AML) 对 E.coli 的耐药样本数
  r_AMP_Ecoli = c(0.63), # Ampicillin (AMP) 对 E.coli 的耐药率
  n_AMP_Ecoli = c(200),
  d_AMP_Ecoli = c(126),
  r_CIP_Ecoli = c(0.58), # Ciprofloxacin (CIP) 对 E.coli 的耐药率
  n_CIP_Ecoli = c(200),
  d_CIP_Ecoli = c(116),
  r_CTX_Ecoli = c(0.52), # Cefotaxime (CTX) 对 E.coli 的耐药率
  n_CTX_Ecoli = c(200),
  d_CTX_Ecoli = c(104),
  r_MEM_Ecoli = c(0.15), # Meropenem (MEM) 对 E.coli 的耐药率
  n_MEM_Ecoli = c(200),
  d_MEM_Ecoli = c(30),
  r_SXT_Ecoli = c(0.67), # Cotrimoxazole (SXT) 对 E.coli 的耐药率
  n_SXT_Ecoli = c(200),
  d_SXT_Ecoli = c(134),
  r_CIP_Kpneumo = c(0.64), # Ciprofloxacin 对 K.pneumoniae 的耐药率
  n_CIP_Kpneumo = c(150),
  d_CIP_Kpneumo = c(96),
  r_MEM_Kpneumo = c(0.28), # Meropenem 对 K.pneumoniae 的耐药率
  n_MEM_Kpneumo = c(150),
  d_MEM_Kpneumo = c(42),
  r_VAN_Saureus = c(0.04), # Vancomycin 对 S.aureus 的耐药率
  n_VAN_Saureus = c(120),
  d_VAN_Saureus = c(5)
)

# 1. 解析列名，提取抗生素和病原体信息
parsed_columns <- parse_amr_columns(names(south_asia_amr))
print(parsed_columns)

# 2. 将宽格式转换为长格式，包括WHO分类
long_data <- amr_wide_to_long(
  south_asia_amr,
  expand_codes = TRUE,
  add_who_class = TRUE
)

# 输出长格式数据的前几行
print(head(long_data))

# 3. 按WHO分类汇总耐药率
who_summary <- long_data %>%
  group_by(who_class) %>%
  summarize(
    avg_resistance = mean(resistance_rate, na.rm = TRUE),
    median_resistance = median(resistance_rate, na.rm = TRUE),
    min_resistance = min(resistance_rate, na.rm = TRUE),
    max_resistance = max(resistance_rate, na.rm = TRUE),
    count = n()
  )

print(who_summary)

# 4. 单独使用抗生素代码转换
# 转换为全名
antibiotic_names <- antibiotic_code_to_name(c("AMP", "CIP", "MEM", "VAN"))
print(antibiotic_names)

# 获取WHO分类
who_classes <- antibiotic_code_to_name(c("AMP", "CIP", "MEM", "VAN"), return_type = "who_class")
print(who_classes)

# 获取抗生素类别
ab_classes <- antibiotic_code_to_name(c("AMP", "CIP", "MEM", "VAN"), return_type = "class")
print(ab_classes)

# 获取所有信息
all_info <- antibiotic_code_to_name(c("AMP", "CIP", "MEM", "VAN"), return_type = "all")
print(all_info)

# 5. 按抗生素类别汇总数据
class_summary <- long_data %>%
  group_by(antibiotic_class) %>%
  summarize(
    avg_resistance = mean(resistance_rate, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(avg_resistance))

print(class_summary)

# 6. 准备映射以便与import_amr_data一起使用
# 通常情况下，我们会先使用amr_wide_to_long准备数据，然后使用import_amr_data导入
# 示例：
long_data_subset <- long_data[, c("year", "region", "country", "antibiotic_name", "pathogen", 
                                  "resistance_rate", "sample_count", "resistant_count")]

# 创建映射
mapping <- list(
  year = "year",
  region = "region",
  country = "country",
  antibiotic_name = "antibiotic_name",
  pathogen_name = "pathogen",
  resistance_rate = "resistance_rate",
  sample_count = "sample_count",
  resistant_count = "resistant_count"
)

# 使用import_amr_data将其转换为标准格式
standard_data <- import_amr_data(
  data = long_data_subset,
  mapping = mapping,
  domain = "human"
)

# 显示标准化数据
print(head(standard_data)) 