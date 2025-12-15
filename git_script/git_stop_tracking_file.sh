read -p "Enter the name of file you want to stop tracking: " FILE_NAME

git rm --cached $FILE_NAME
