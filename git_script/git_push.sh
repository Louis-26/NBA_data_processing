# uncomment if it is in linux, and need to convert dos to unix
# sed -i 's/\r$//' git_script/git_push.sh
git add .

git commit -m "update"

git push origin main
