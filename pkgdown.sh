#!/bin/sh
set -e

[ -z "${GITHUB_PAT}" ] && exit 0
echo "Environment variables:"
echo "  PATH: ${PATH}"
echo "  TRAVIS_BRANCH: ${TRAVIS_BRANCH}"
echo "  TRAVIS_PULL_REQUEST: ${TRAVIS_PULL_REQUEST}"
echo "  TRAVIS_PULL_REQUEST_BRANCH: ${TRAVIS_PULL_REQUEST_BRANCH}"
[ "${TRAVIS_BRANCH}" != "master" || "${TRAVIS_PULL_REQUEST}" == "true" ] && exit 0

git config --global user.email "will.landau@gmail.com"
git config --global user.name "wlandau"

git clone -b gh-pages https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git gh-pages
cd gh-pages
cp -r ../docs/* ./
git add --all *
git commit -m "Update pkgdown site" || true
git push -q origin gh-pages
