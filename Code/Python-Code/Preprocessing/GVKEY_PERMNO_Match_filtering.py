#!/usr/bin/env python
# coding: utf-8

# In[20]:


# Path to the CSV file with gvkey, date, and permno information
csv_path = r'C:\Users\ge27xix\SEC\Processed10-X\data\CRSP\linking_gvkey_permno.csv'

# Directory containing the files to rename
root_directory_path = r"C:/Users/ge27xix/SEC/Processed10-X/Raw"


# In[21]:


# Load the CSV file into a DataFrame
compustat_data = pd.read_csv(csv_path)

# Format 'gvkey' as integer to remove the decimal part, then convert to string
compustat_data['gvkey'] = compustat_data['gvkey'].astype(int).astype(str)
compustat_data['date'] = compustat_data['date'].astype(str)

# Create a unique key by combining 'gvkey' and 'date'
compustat_data['unique_key'] = compustat_data['gvkey'] + '_' + compustat_data['date']

# Create a series for mapping unique_key to permno
mapping_series = compustat_data.set_index('unique_key')['permno']
print(mapping_series)


# In[22]:



# Walk through the directory and its subdirectories
for dirpath, dirnames, filenames in os.walk(root_directory_path):
    for filename in filenames:
        if filename.endswith(".csv"):  # Filter for CSV files
            parts = filename.split('_')
            if len(parts) >= 2:  # Ensure the filename has enough parts
                gvkey = parts[0]
                date = parts[1][:6]  # Extract yyyymm part for matching
                unique_key = f"{gvkey}_{date}"

                # Find the corresponding permno or use "XXXXX" if not found
                permno = mapping_series.get(unique_key, "XXXXX")
                
                # Convert permno to string and prepend it to the filename
                new_filename = f"{permno}_{filename}"

                # Full path for the original and new file
                original_file = os.path.join(dirpath, filename)
                new_file = os.path.join(dirpath, new_filename)

                # Rename the file
                os.rename(original_file, new_file)

print("Files have been renamed.")


# In[ ]:




