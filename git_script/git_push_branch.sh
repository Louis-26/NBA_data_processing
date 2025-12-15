read -p "Enter the name of your feature branch: " BRANCH_NAME

git add .

git commit -m "update"

git push origin "$BRANCH_NAME"