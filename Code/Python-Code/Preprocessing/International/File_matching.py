import pandas as pd
import os
from concurrent.futures import ProcessPoolExecutor, as_completed
from tqdm import tqdm

# Function to load and merge a single CSV file
def merge_csv(main_df, csv_path, unique_id='DocID'):
    temp_df = pd.read_csv(csv_path)
    return main_df.merge(temp_df, on=unique_id, how='left')

def main():
    # Paths setup
    main_output_csv_path = r"C:\Users\ge27xix\SEC\Processed10-X\Python-Code\Preprocessing\International\output.csv"
    other_csv_files_directory = r'C:\Users\Public\Refinitiv_Filing_API\Annual_Reports\doc_id_overview'

    # Load the main output CSV into a DataFrame
    main_df = pd.read_csv(main_output_csv_path)

    # List all CSV files in the other CSV files directory (excluding the main output CSV)
    csv_files_to_join = [os.path.join(other_csv_files_directory, f) for f in os.listdir(other_csv_files_directory) if f.endswith('.csv') and f != 'output.csv']

    # Prepare for parallel processing
    futures = []
    max_workers = min(40, len(csv_files_to_join))  # Use 40 cores or the number of files, whichever is smaller

    with ProcessPoolExecutor(max_workers=max_workers) as executor:
        # Submit tasks
        for csv_file in csv_files_to_join:
            futures.append(executor.submit(merge_csv, main_df.copy(), csv_file))

        # Progress bar for completing merge tasks
        for future in tqdm(as_completed(futures), total=len(futures), desc="Merging Files"):
            main_df = future.result()

    # Save the final, joined DataFrame to a new CSV file
    final_output_path = r"C:\Users\ge27xix\SEC\Processed10-X\Python-Code\Preprocessing\International\output_full.csv"
    main_df.to_csv(final_output_path, index=False)

   
if __name__ == '__main__':
    main()

