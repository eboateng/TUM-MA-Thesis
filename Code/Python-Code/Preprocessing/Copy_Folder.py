#!/usr/bin/env python
# coding: utf-8

# In[ ]:


import shutil
import os

# Define the source and destination directory paths
source_directory_path = r'C:\Users\ge27xix\SEC\Processed10-X\Raw'
destination_directory_path = r'C:\Users\ge27xix\SEC\Processed10-X\10K'

for dirpath, dirnames, filenames in os.walk(source_directory_path):
    # Determine the path to the current folder in the destination directory
    relative_path = os.path.relpath(dirpath, source_directory_path)
    destination_dir_path = os.path.join(destination_directory_path, relative_path)
    
    # Ensure the current folder exists in the destination directory
    os.makedirs(destination_dir_path, exist_ok=True)
    
    for filename in filenames:
        # Construct the full file paths for source and destination
        src_file_path = os.path.join(dirpath, filename)
        dst_file_path = os.path.join(destination_dir_path, filename)

        # Copy each file to the corresponding folder in the destination directory
        shutil.copy2(src_file_path, dst_file_path)  # copy2 preserves metadata

print("Files and folder structure have been copied.")

