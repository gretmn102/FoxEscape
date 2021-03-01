# FoxEscape
## Build

```batch
dotnet tool restore
paket install
npm install

cd src
dotnet restore
cd ../

npm start
```

And open http://localhost:8080/ in your browser.