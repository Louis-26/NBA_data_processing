#I'm in local branch1, someone made some update on main branch remotely, I want to pull it and merge to my local branch, for instance, we share some files, such as main_test.txt, with different content, and I also have some files that the main branch don't have, such as only_in_branch1.txt

#what I want: I want to pull request at my branch locally where I already do git clone, and I will overwrite those files shared with the same file name but different content into the content of the main branch remotely, and keep all my files that he doesn't have on main branch
# first to local, then need git push to remote
read -p "Enter the name of your feature branch that you pull request from: " BRANCH_NAME_FROM

read -p "Enter the name of your feature branch that you pull request to: " BRANCH_NAME_TO

git checkout $BRANCH_NAME_TO

git fetch origin

git merge -X theirs origin/$BRANCH_NAME_FROM