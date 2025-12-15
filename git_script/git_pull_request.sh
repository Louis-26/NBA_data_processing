# first to remote, then need git fetch to local
read -p "Enter the name of your feature branch that you pull request from: " BRANCH_NAME_FROM

read -p "Enter the name of your feature branch that you pull request to: " BRANCH_NAME_TO

gh pr create --base $BRANCH_NAME_TO --head $BRANCH_NAME_FROM --title "title" --body "body"

gh pr checkout $BRANCH_NAME_FROM

gh pr merge $BRANCH_NAME_FROM 