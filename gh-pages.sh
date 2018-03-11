# Taken from https://github.com/richfitz/storr/blob/61dc868aea96327692d4ec298ce3880a36cba734/update_web.sh
#!/bin/sh
set -e

DOCS_DIR=docs
VERSION=$(git rev-parse --short HEAD)
REMOTE_URL=$(git config --get remote.origin.url)

mkdir -p ${DOCS_DIR}
rm -rf ${DOCS_DIR}/.git
git init ${DOCS_DIR}
git -C ${DOCS_DIR} checkout --orphan gh-pages
git -C ${DOCS_DIR} add .
git -C ${DOCS_DIR} commit --no-verify -m "Update docs for version ${VERSION}"
git -C ${DOCS_DIR} remote add origin -m "gh-pages" ${REMOTE_URL}
git -C ${DOCS_DIR} push --force -u origin gh-pages
