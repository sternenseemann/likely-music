# likely music

![screenshot on 2017-09-05](screenshot.png)

## TODO

- [x] overlay styling
- [ ] graph validation
- [ ] WAV export (using fluidsynth)
- [ ] custom manipulation bar style
- [ ] in browser player (by exporting a mp3 or similar using fluidsynth and
  loading using javascript)
- [ ] JS refactoring
- [ ] switch to SASS or similar
- [x] quicksave in localstorage
- [ ] add help text
- [ ] nice unicode musical symbols

## Setup

```
cabal sandbox init
cabal install --only-dependencies
cabal build

cd web
yarn install
yarn run build:dev

cd ..
cabal run likely-music-backend
```
