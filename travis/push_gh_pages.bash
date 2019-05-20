#!/bin/bash

cd $1
git init
git config user.name "${GH_USER_NAME}"
git config user.email "{GH_USER_EMAIL}"
git add .
git commit -m "Deploy to GitHub Pages"
git push --force --quiet "https://${GH_TOKEN}@${GH_REF}" master:gh-pages > /dev/null 2>&1
