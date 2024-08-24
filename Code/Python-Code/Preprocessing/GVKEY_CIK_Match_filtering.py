import pandas as pd
import os

# Path to the cleaned compustat file
csv_path = r"C:\Users\ge27xix\SEC\Identifier\Rohdaten_April21\cleaned_compustat.csv"

# Directory containing the files to rename
root_directory_path = r"C:\Users\ge27xix\SEC\Processed10-X\Raw"

compustat_data = pd.read_csv(csv_path)

# Drop duplicates based on 'hcik'
compustat_data = compustat_data.drop_duplicates(subset='hcik', keep='first')

# Create the mapping series from 'hcik' to 'gvkey'
mapping_series = compustat_data.set_index('hcik')['gvkey']

# Walk through the directory and its subdirectories
for dirpath, dirnames, filenames in os.walk(root_directory_path):
    for filename in filenames:
        if filename.endswith(".csv"):  # Check if the file is a CSV
            parts = filename.split('_')
            if len(parts) >= 5:  # Ensure the filename has enough parts
                try:
                    cik = int(parts[4])  # Extract the CIK part
                except ValueError:
                    continue  # Skip the file if CIK part is not an integer

                # Check if the CIK is in the mapping and prepend the gvkey or XXXXX
                gvkey = mapping_series.get(cik, "XXXXX")

                # Convert gvkey to string and prepend it to the filename
                gvkey_str = str(gvkey)
                new_filename = f"{gvkey_str}_{filename}"

                # Full path for the original and new file
                original_file = os.path.join(dirpath, filename)
                new_file = os.path.join(dirpath, new_filename)

                # Rename the file
                os.rename(original_file, new_file)

print("Files have been renamed.")