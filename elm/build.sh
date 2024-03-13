#! /bin/sh
rm -r /var/www/html/*
elm make src/Main.elm --output=../elm.js
rsync -azP  ~/projects/adventure-archivist.floodedrealms.com/* /var/www/html/
