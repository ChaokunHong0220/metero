# Data Import Help

## Supported File Formats

metero supports the following file formats:

- **CSV Files** (.csv): Comma-separated text files
- **Excel Files** (.xlsx, .xls): Microsoft Excel spreadsheets
- **R Data Files** (.RData, .rda): R data formats

## Import Steps

1. In the "Data Import" tab, click the "Choose File" button and select your AMR data file
2. Select the correct file type (or use auto-detection)
3. For Excel files, specify the sheet number
4. Specify whether the file has a header row and the number of rows to skip (if any)
5. Click the "Load File" button to preview the data

## Column Mapping

After importing data, you need to map your data columns to metero standard columns:

- **Study ID**: Unique identifier for the study or dataset
- **Pathogen/Pathogen Name**: Name of the pathogen (e.g., E. coli)
- **Antibiotic/Antibiotic Name**: Name of the antibiotic (e.g., Ciprofloxacin)
- **Resistance Rate**: Resistance rate (a value between 0-1)
- **Sample Count**: Number of samples tested
- **Resistant Count**: Number of resistant samples
- **Country**: Country
- **Region**: Region or province
- **Year**: Year of data collection

After mapping your data columns to these standard columns, metero will be able to properly analyze and visualize your data.

## South Asia Data Format

For South Asian data, you may encounter formats using antibiotic abbreviations, such as "r_AMP_Ecoli" (for E. coli resistance rate to Ampicillin), "n_AMP_Ecoli" (total sample count), and "d_AMP_Ecoli" (resistant sample count).

metero can automatically process this format and convert it to the standard format. Just make sure to map these columns appropriately in the column mapping.

## Tips

- Required fields: Pathogen, Antibiotic, and (Resistance Rate or Sample Count+Resistant Count)
- If your data is missing Resistance Rate but has Sample Count and Resistant Count, metero will automatically calculate the resistance rate
- Ensure your data uses consistent naming conventions, especially for pathogens and antibiotics
- If you're unsure how to map certain columns, you can leave them temporarily unmapped 