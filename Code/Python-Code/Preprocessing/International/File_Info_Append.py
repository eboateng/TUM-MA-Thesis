import pandas as pd
import os
from concurrent.futures import ProcessPoolExecutor
from tqdm import tqdm

def read_csv_file(file_path):
    """Read a single CSV file into a DataFrame."""
    return pd.read_csv(file_path)

def main():
    directory = r'C:\Users\Public\Refinitiv_Filing_API\Annual_Reports\doc_id_overview'
    output_file = r"C:\Users\ge27xix\SEC\Processed10-X\Python-Code\Preprocessing\International\output_full.csv"
    csv_files = [os.path.join(directory, f) for f in os.listdir(directory) if f.endswith('.csv')]

    # Initialize a ProcessPoolExecutor to use multiple cores
    with ProcessPoolExecutor(max_workers=40) as executor:
        # Use tqdm for progress bar with executor.map
        futures = list(tqdm(executor.map(read_csv_file, csv_files), total=len(csv_files), desc="Reading CSVs"))

    # Concatenate all DataFrames into one
    combined_df = pd.concat(futures, ignore_index=True)
    
    # Save the combined DataFrame to a new CSV file
    combined_df.to_csv(output_file, index=False)
    

if __name__ == '__main__':
    main()

