
# TUM-MA-Thesis: Text embedding and stock return
========================================================
## Overview

This repository contains scripts and resources for data processing, feature extraction, and machine learning model training developed as part of my thesis. It includes tools for Word2Vec and FinBERT model annotation, Stata data file chunking and processing, as well as visualization utilities.

### Repository Files and Uses

| File/Script                           | Description                                                                 |
|---------------------------------------|-----------------------------------------------------------------------------|
| `dta_splitting.ipynb`                 | Notebook for splitting and processing `.dta` files for efficient handling.  |
| `custom_w2v_rf.bin`                   | Pre-trained EDGAR Word2Vec model for used in feature generation.            |
| `10X_processor.ipynb`                 | Data processing pipeline for 10X datasets and clustering.                   |
| `Stata_data_file_chunking.ipynb`      | Script for chunking large Stata datasets into manageable files.             |
| `plot_files_type.ipynb`               | Visualization utilities for analyzing file types and related metrics.       |
| `mainScript-W2V_annotated.ipynb`      | Annotated notebook for Word2Vec model with ML training and regression analysis   |
| `mainScript-FinBERT_annotated-.ipynb` | Annotated notebook for FinBERT model training and evaluation.               |
| `FinBERT-demo.ipynb`                  | Demonstration notebook for using the FinBERT model in NLP tasks.            |

##  Prerequisites

### Dependencies

Ensure you have the following installed:

- **Python**: Version 3.8 or higher
- **Required Python libraries**:
  - `pandas`
  - `numpy`
  - `matplotlib`
  - `sklearn`
  - `gensim`
  - `transformers`

Install all dependencies via the provided requirements file:
```
pip install -r requirements.txt
```
### Additional Resources

- Ensure `custom_w2v_rf.bin` is placed in the appropriate directory for Word2Vec-related scripts.
- FinBERT model weights are required for `mainScript-FinBERT_annotated-.ipynb`.
  
## Usage

1. **Data Processing**
   - Use `10X_processor.ipynb` to prepare cleaned 10X filings from Loughran-MacDonald. [Source]([https://sraf.nd.edu/sec-edgar-data/cleaned-10x-files/])
   - Use `Stata_data_file_chunking.ipynb` to prepare stock dta file
   - For `.dta` files, refer to `dta_splitting.ipynb`.

2. **Model Training**
   - **Word2Vec**: Run `mainScript-W2V_annotated.ipynb` for feature extraction, analysis and training with Word2Vec models.
   - **FinBERT**: Use `mainScript-FinBERT_annotated-.ipynb` for feature extraction, analysis and training the FinBERT model on the dataset.

3. **Visualization**
   - `plot_files_type.ipynb` contains some scripts to create insightful plots of the data.

4. **Demo**
   - `FinBERT-demo.ipynb` is a practical example of FinBERT in text classification or sentiment analysis by Yang et. al.

---

## Key Features

- **Data Handling**: Efficient processing of large datasets using Stata chunking and `.dta` file splitting.
- **Model Training**: Preconfigured scripts for Word2Vec and FinBERT fine-tuning.
- **Visualization**: Tools for data exploration and visualization.
- **Pre-trained Models**: Includes a custom Word2Vec binary file (`custom_w2v_rf.bin`).

---

## Authors

- **Emmanuel Mensah Boateng**

---

## License

This project is licensed under the **MIT License**. See the `LICENSE` file for details.

---

## Acknowledgments

- Special thanks to the open-source community for their contributions to Word2Vec and FinBERT.
