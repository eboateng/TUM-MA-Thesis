#!/usr/bin/env python
# coding: utf-8

# In[ ]:


import os
import csv
from multiprocessing import Pool
from functools import partial
import pandas as pd
from tqdm import tqdm

# Adjusted for the correct base and output directories
base_directory = r'C:\Users\Public\Refinitiv_Filing_API\Annual_Reports\Extracted_txt'
output_directory = r'C:\Users\ge27xix\SEC\Processed10-X\Python-Code\Preprocessing\International'

# Adjust output directory handling to check if it's not a directory
if not os.path.isdir(output_directory):
    os.makedirs(output_directory, exist_ok=True)

output_filename = os.path.join(output_directory, 'output.csv')  # Specify the output file directly

def process_file(file_path):
    file_size = os.path.getsize(file_path)
    filename = os.path.basename(file_path)
    parts = filename.split('_')
    if len(parts) >= 3:
        return parts[0], parts[1], parts[2], file_size
    return None

def find_csv_files(directory):
    csv_files = []
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.endswith('.csv'):
                csv_files.append(os.path.join(root, file))
    return csv_files

def compile_information(base_directory, output_filename):
    all_csv_files = find_csv_files(base_directory)

    # Using tqdm to create a progress bar for the file processing
    with Pool(processes=min(os.cpu_count(), 40)) as pool:
        results = list(tqdm(pool.imap(process_file, all_csv_files), total=len(all_csv_files)))

    valid_results = [result for result in results if result]
    df = pd.DataFrame(valid_results, columns=['Organization_ID', 'DocID', 'FilingDate', 'FileSize'])

    df.to_csv(output_filename, index=False)
    print(f"Compiled CSV file created: {output_filename}")

if __name__ == "__main__":
    compile_information(base_directory, output_filename)

