# uncomment if it is in linux, and need to convert dos to unix
# sed -i 's/\r$//' git_script/git_discard_change_branch.sh

# Prompt to choose the stage
echo "Run this script to come back to the last commit, and discard changes."
echo "Which stage are you in now?"
echo "1 - Before staged"
echo "2 - After staged, but not committed"
echo "3 - After staged, committed, but not pushed"
echo "4 - Staged, committed, and pushed"
read -p "Enter the number (1-4): " stage

# Stage 1: Before staged
if [ "$stage" -eq 1 ]; then
    git restore . 

# Stage 2: After staged, but not committed
elif [ "$stage" -eq 2 ]; then
    git restore --staged . && git restore .

# Stage 3: After staged, committed, but not pushed
elif [ "$stage" -eq 3 ]; then
    git reset --hard HEAD~1 && git restore --staged . && git restore .

# Stage 4: Staged, committed, and pushed
elif [ "$stage" -eq 4 ]; then
    # Prompt to choose the branch
    read -p "Enter the branch name you want to work with: " branch
    git reset --hard HEAD~1 && git push --force-with-lease origin $branch

else
    echo "Invalid choice. Please enter a number between 1 and 4."
    exit 1
fi