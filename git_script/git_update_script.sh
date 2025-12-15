#!/bin/bash
# This script is used to update a specific folder locally, with the folder the same name
# from a remote git repo.

ORIGINAL_DIR="$(pwd)"

# step 1: download the target folder from the remote git repo
DEFAULT_SAVE_DIR="${HOME//\\//}/Downloads"
DEFAULT_REPO_URL="https://github.com/Louis-26/git_script_template"
DEFAULT_FOLDER_NAME="git_script"

read -r -p "Enter the url of the target repo from the github
(e.g., https://github.com/Louis-26/personal_note): " REPO_URL
read -p "Enter the folder name from git repo: " FOLDER_NAME
read -p "Enter the branch name (default is main): " BRANCH_NAME
read -r -p "Enter the directory name of folder to save the downloaded folder locally
(e.g., C:/Users/USERNAME/Downloads), by default it is $DEFAULT_SAVE_DIR: " SAVE_DIR

# Default branch to main if empty
BRANCH_NAME=${BRANCH_NAME:-main}
SAVE_DIR="${SAVE_DIR:-$DEFAULT_SAVE_DIR}"

# Default repo url if empty
REPO_URL=${REPO_URL:-$DEFAULT_REPO_URL}
# Default folder name if empty
FOLDER_NAME=${FOLDER_NAME:-$DEFAULT_FOLDER_NAME}

echo $SAVE_DIR
SAVE_DIR="${SAVE_DIR//\\//}"
cd $SAVE_DIR
mkdir download_folder
cd download_folder

git init
git remote add origin ${REPO_URL}.git
git sparse-checkout init --no-cone
git sparse-checkout set "$FOLDER_NAME"
git pull "$REPO_URL" "$BRANCH_NAME"

# Now clean everything except the target folder inside temp_repo
shopt -s extglob  # enable extended globbing

# Delete all files and directories except the folder we want
#rm -rf !("$FOLDER_NAME")
#rm -rf .[^.]* .??*

# move the target folder to the parent directory
mv "$FOLDER_NAME" "$SAVE_DIR"
cd "$SAVE_DIR"
rm -rf download_folder

# step 2: replace the local folder with the downloaded folder
rm -rf "$ORIGINAL_DIR/$FOLDER_NAME"
mv "$SAVE_DIR/$FOLDER_NAME" "$ORIGINAL_DIR"
echo "Done! The folder '$FOLDER_NAME' has been updated in $ORIGINAL_DIR/$FOLDER_NAME"