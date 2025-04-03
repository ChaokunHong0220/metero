# 数据导入帮助

## 支持的文件格式

METEOR支持以下文件格式:

- **CSV文件** (.csv): 逗号分隔的文本文件
- **Excel文件** (.xlsx, .xls): Microsoft Excel电子表格
- **R数据文件** (.RData, .rda): R数据格式

## 导入步骤

1. 在"数据导入"标签中，单击"选择文件"按钮并选择您要导入的AMR数据文件
2. 选择正确的文件类型(或使用自动检测)
3. 如果是Excel文件，指定工作表编号
4. 指定文件是否有标题行以及要跳过的行数(如果有)
5. 单击"加载文件"按钮预览数据

## 列映射

导入数据后，您需要将数据列映射到METEOR标准列:

- **Study ID**: 研究或数据集的唯一标识符
- **Pathogen/Pathogen Name**: 病原体名称(如E. coli)
- **Antibiotic/Antibiotic Name**: 抗生素名称(如Ciprofloxacin)
- **Resistance Rate**: 耐药率(0-1之间的值)
- **Sample Count**: 测试的样本数量
- **Resistant Count**: 耐药样本数量
- **Country**: 国家
- **Region**: 区域或省份
- **Year**: 数据收集年份

将您的数据列映射到这些标准列后，METEOR将能够正确分析和可视化您的数据。

## 南亚数据格式

对于南亚地区的数据，您可能会遇到使用抗生素缩写的格式，如"r_AMP_Ecoli"(表示大肠杆菌对氨苄青霉素的耐药率)、"n_AMP_Ecoli"(样本总数)和"d_AMP_Ecoli"(耐药样本数)。

METEOR可以自动处理这种格式并将其转换为标准格式。只需确保在列映射中将这些列适当映射到标准列即可。

## 提示

- 必填字段: Pathogen、Antibiotic和(Resistance Rate或Sample Count+Resistant Count)
- 如果您的数据缺少Resistance Rate，但有Sample Count和Resistant Count，METEOR将自动计算耐药率
- 确保您的数据使用统一的命名约定，特别是对于病原体和抗生素
- 如果您不确定如何映射某些列，可以暂时将其留空 