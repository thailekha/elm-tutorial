heroku apps:create super-fancy-word-lookup-app
./heroku-envvars.sh
git push heroku master
heroku logs --tail