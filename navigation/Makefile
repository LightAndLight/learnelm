assets/app.js: src/*.elm
	elm make src/Main.elm --optimize --output assets/app.js

run: assets/app.js
	python3 -m http.server 8000
