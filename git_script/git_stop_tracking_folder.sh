read -p "Enter the name of folder you want to stop tracking: " FOLDER_NAME

git rm -r --cached $FOLDER_NAME
