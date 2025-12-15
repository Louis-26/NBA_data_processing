#!/bin/bash
# Description: This script is used to download a specific folder from a git repository.
DEFAULT_SAVE_DIR="${HOME//\\//}/Downloads"

read -r -p "Enter the url of the target repo from the github
(e.g., https://github.com/Louis-26/personal_note): " REPO_URL
read -p "Enter the folder name from git repo: " FOLDER_NAME
read -p "Enter the branch name (default is main): " BRANCH_NAME
read -r -p "Enter the directory name of folder to save the downloaded folder locally
(e.g., C:/Users/USERNAME/Downloads), by default it is $DEFAULT_SAVE_DIR: " SAVE_DIR

# Default branch to main if empty
BRANCH_NAME=${BRANCH_NAME:-main}
SAVE_DIR="${SAVE_DIR:-$DEFAULT_SAVE_DIR}"
echo $SAVE_DIR
SAVE_DIR="${SAVE_DIR//\\//}"
cd $SAVE_DIR
mkdir download_folder
cd download_folder

git init
git remote add origin ${REPO_URL}.git
git sparse-checkout init --no-cone
git sparse-checkout set "$FOLDER_NAME"
git pull $REPO_URL $BRANCH_NAME

# Now clean everything except the target folder inside temp_repo
shopt -s extglob  # enable extended globbing

# Delete all files and directories except the folder we want
#rm -rf !("$FOLDER_NAME")
#rm -rf .[^.]* .??*

# move the target folder to the parent directory
mv "$FOLDER_NAME" $SAVE_DIR
cd $SAVE_DIR
rm -rf download_folder

echo "Done! The folder '$FOLDER_NAME' has been downloaded to $SAVE_DIR/$FOLDER_NAME"
