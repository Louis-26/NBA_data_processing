# download from remote repository to local directory of other branch
read -p "Enter the name of your feature branch that check out: " BRANCH_NAME

git fetch

git checkout $BRANCH_NAME

#git pull origin $BRANCH_NAME
git pull