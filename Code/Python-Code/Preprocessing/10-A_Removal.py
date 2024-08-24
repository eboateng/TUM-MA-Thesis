import os
import shutil

# Source directory containing the files
source_directory_path = r"C:\Users\ge27xix\SEC\Processed10-X\10K"

# Destination directory for moving files
destination_directory = r"C:\Users\ge27xix\SEC\Processed10-X\10K_Useless"

# Ensure the destination directory exists
os.makedirs(destination_directory, exist_ok=True)

# Counter for the number of files moved
moved_files_count = 0

# Walk through the directory and its subdirectories
for dirpath, dirnames, filenames in os.walk(source_directory_path):
    for filename in filenames:
        # Check if the file matches the criteria
        if filename.endswith(".csv") and ("10-K-A" in filename or "10KSB-A" in filename or "10-K405-A" in filename or "10KSB40-A" in filename or "10-KT-A" in filename or filename.startswith("XXXXX")):
            # Construct the destination path
            relative_path = os.path.relpath(dirpath, source_directory_path)
            destination_path = os.path.join(destination_directory, relative_path)

            # Ensure the destination subdirectory exists
            os.makedirs(destination_path, exist_ok=True)

            # Move the file
            shutil.move(os.path.join(dirpath, filename), os.path.join(destination_path, filename))
            moved_files_count += 1
            print(f"Moved file: {filename} to {destination_path}")

print(f"File moving process completed. Total files moved: {moved_files_count}")