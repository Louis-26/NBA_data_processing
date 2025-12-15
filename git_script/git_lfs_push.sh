# use git lfs to push large files or folders to remote repository, for main branch

# this script only pushes those large files, but doesn't push other normal files.

# Uncomment if you need to convert DOS to Unix line endings
# sed -i 's/\r$//' git_script/git_lfs_push.sh

# Initialize Git LFS
git lfs install

# Determine if it is a file or folder
while true; do
    # Ask for the file/folder path
    read -p "Enter the path of the file or folder to track and push (or directly enter to finish): " TARGET_PATH

    # Exit condition
    if [ "$TARGET_PATH" == "" ]; then
        echo "Finished processing all files/folders."
        break
    fi

    # Check if the path exists
    if [ ! -e "$TARGET_PATH" ]; then
        echo "Error: '$TARGET_PATH' does not exist."
        continue
    fi

    # Determine if it's a file or folder
    if [ -f "$TARGET_PATH" ]; then
        echo "Tracking a single file: $TARGET_PATH"
        git lfs track "$TARGET_PATH"
    elif [ -d "$TARGET_PATH" ]; then
        echo "Tracking all files in folder: $TARGET_PATH"
        git lfs track "$TARGET_PATH/**"
    else
        echo "Error: '$TARGET_PATH' is neither a file nor a folder."
        continue
    fi

    # Stage .gitattributes and the selected file/folder
    git add .gitattributes "$TARGET_PATH"
done
# Commit
git commit -m "update"

# Push to main branch
git push origin main

