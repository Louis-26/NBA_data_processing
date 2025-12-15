## push local changes to remote 
main branch: git_push.sh
other branch: git_push_branch.sh

## pull between branch, from one remote branch to another local branch(replace local files by remote files with the same name)
git_pull_between_branch.sh

## pull request, from one remote branch to another remote branch(pull request record on github)
git_pull_request.sh

## fetch remote files and download locally
main branch: git_fetch_pull.sh
other branch: git_fetch_pull_branch.sh

# linux machine change permission
in linux, add `chmod -R +x git_script` for a folder, or `chmod +x git_script/git_push.sh` for a single file, to make it executable.

# other command
in linux, convert dos to unix: `sed -i 's/\r$//' FILE_NAME`, e.g., `sed -i 's/\r$//' git_script git_push.sh`

if you meet fatal error on divergent branches when pulling, 
use `git config pull.rebase false`
