import os
import shutil

# Define the source and target directories
source_dir = r'C:\Users\ge27xix\SEC\Processed10-X\Raw'  # Replace with the path to your source directory
target_dir = r'C:\Users\ge27xix\SEC\Processed10-X\10Q'  # Replace with the path to your target directory

# Function to move files
def move_files(src_folder, dst_folder):
    for filename in os.listdir(src_folder):
        if filename.endswith('.csv') and ("10-Q" in filename or "10QSB" in filename):  # Check if the file name contains "10-Q" or "10QSB"
            source_file = os.path.join(src_folder, filename)
            target_file = os.path.join(dst_folder, filename)

            if not os.path.exists(dst_folder):
                os.makedirs(dst_folder)

            shutil.move(source_file, target_file)
            print(f"Moved file: {source_file} to {target_file}")

# Iterate over each directory and subdirectory
for dirpath, dirnames, filenames in os.walk(source_dir):
    # Construct the corresponding target directory path
    rel_path = os.path.relpath(dirpath, source_dir)
    target_path = os.path.join(target_dir, rel_path)

    # Move the files in the current directory
    move_files(dirpath, target_path)

print("File moving completed.")
