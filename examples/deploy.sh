#!/bin/bash

pushd examples
rm -rf dist
elm-make Counter.elm --warn --output=dist/counter.html
elm-make CounterWithCats.elm --warn --output=dist/counter-with-cats.html
elm-make TextEditor.elm --warn --output=dist/text-editor.html
cd dist
git init
git add .
git commit -m "Deploy to Github Pages"
git push --force git@github.com:cbenz/undo-redo.git master:gh-pages
popd