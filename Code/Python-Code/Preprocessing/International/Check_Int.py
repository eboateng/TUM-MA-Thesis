import os
import pandas as pd
from multiprocessing import Pool, cpu_count, freeze_support
from tqdm import tqdm

def process_csv(file_path):
    try:
        # Load specified columns from the CSV file.
        df = pd.read_csv(file_path, usecols=['FeedName', 'SubmissionTypeDescription', 'organization_id'])
        # Drop duplicates based on 'FeedName' and 'SubmissionTypeDescription' to find unique pairs,
        # keeping the first occurrence which includes the corresponding 'organization_id'.
        unique_pairs_with_org_id = df.drop_duplicates(subset=['FeedName', 'SubmissionTypeDescription']).copy()
        return unique_pairs_with_org_id
    except Exception as e:
        print(f"Error processing file {file_path}: {e}")
        # Return an empty DataFrame with the specified columns if an error occurs.
        return pd.DataFrame(columns=['FeedName', 'SubmissionTypeDescription', 'organization_id'])

def main():
    # Define the path to your CSV file.
    file_path = r'C:\Users\ge27xix\SEC\Processed10-X\Python-Code\Preprocessing\International\Info_for_Files_Available.csv'
    # Process the CSV file.
    unique_pairs_with_org_id_df = process_csv(file_path)
    # Define the path where you want to save the output CSV.
    output_csv_path = r'C:\Users\ge27xix\SEC\Processed10-X\Python-Code\Preprocessing\International\Feedname_Classification.csv'
    # Save the processed DataFrame to a CSV file, without the index.
    unique_pairs_with_org_id_df.to_csv(output_csv_path, index=False)
    print(f"Saved unique pairs with organization_id to {output_csv_path}")

if __name__ == '__main__':
    main()