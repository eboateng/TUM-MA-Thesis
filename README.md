
README: Data Processing and Model Training

Overview

This repository contains scripts and resources for data processing, feature engineering, and machine learning model training with a focus on natural language processing (NLP) tasks. It includes tools for Word2Vec and FinBERT model annotation, Stata data file chunking, and visualization utilities.

Repository Structure

| File/Script                           | Description                                                                 |
|---------------------------------------|-----------------------------------------------------------------------------|
| `dta_splitting.ipynb`                 | Notebook for splitting and processing `.dta` files for efficient handling.  |
| `custom_w2v_rf.bin`                   | Pre-trained Word2Vec model for use in custom NLP tasks.                     |
| `10X_processor.ipynb`                 | Data processing pipeline optimized for 10X datasets.                        |
| `Stata_data_file_chunking.ipynb`      | Script for chunking large Stata datasets into manageable files.             |
| `plot_files_type.ipynb`               | Visualization utilities for analyzing file types and related metrics.       |
| `mainScript-W2V_annotated.ipynb`      | Main pipeline for Word2Vec model annotation and training.                   |
| `mainScript-FinBERT_annotated-.ipynb` | Annotated notebook for FinBERT model training and evaluation.               |
| `FinBERT-demo.ipynb`                  | Demonstration notebook for using the FinBERT model in NLP tasks.            |

Prerequisites

Dependencies

Ensure you have the following installed:
	•	Python: Version 3.8 or higher
	•	Required Python libraries:
	•	pandas
	•	numpy
	•	matplotlib
	•	sklearn
	•	gensim
	•	transformers

Install all dependencies via the provided requirements file:
```
pip install -r requirements.txt
```
Additional Resources

	•	Ensure custom_w2v_rf.bin is placed in the appropriate directory for Word2Vec-related scripts.
	•	FinBERT model weights are required for mainScript-FinBERT_annotated-.ipynb.

Usage

1. Data Processing
   
	•	Use 10X_processor.ipynb or Stata_data_file_chunking.ipynb to prepare datasets for modeling.

	•	For .dta files, refer to dta_splitting.ipynb.

3. Model Training
   
	•	Word2Vec: Run mainScript-W2V_annotated.ipynb for training or fine-tuning Word2Vec models.

	•	FinBERT: Use mainScript-FinBERT_annotated-.ipynb to train the FinBERT model on your dataset.

5. Visualization
   
	•	Utilize plot_files_type.ipynb to create insightful plots of your processed data.

6. Demo
   
	•	Explore FinBERT-demo.ipynb for a practical example of FinBERT in text classification or sentiment analysis.

Key Features

	•	Data Handling: Efficient processing of large datasets using Stata chunking and .dta file splitting.
 
	•	Model Training: Preconfigured scripts for Word2Vec and FinBERT fine-tuning.
 
	•	Visualization: Tools for data exploration and visualization.
 
	•	Pre-trained Models: Includes a custom Word2Vec binary file (custom_w2v_rf.bin).



Authors

	•	Emmanuel Mensah Boateng
	•	Contributors: List additional contributors or team members here.

License

This project is licensed under the MIT License. See the LICENSE file for details.

Acknowledgments

	•	Special thanks to the open-source community for their contributions to Word2Vec and FinBERT.
