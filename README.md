# PTO Viewer

A tool for seeing how much PTO you're taking relative to everyone else, as a way to encourage taking more PTO. Add your PTO here https://pto-viewer.firebaseapp.com/. By default you're name is `Anonymous`, but you can optionally set your name.

### Development
- Clone this repo
- Copy `firebaseConfig.example.json` to `firebaseConfig.json` and fill in the empty values
- Run `yarn`
- Run `npx firebase login` (if you haven't before)
- Run `yarn dev`

#### Dev: Optional
- Install [elm-doc-preview](https://github.com/dmy/elm-doc-preview)
  - Run `edp` to view the docs for the Elm code

### Build
- Run `yarn build`


### Future Plans (in no particular order)
- [ ] Select a year to view
- [x] Add a button to refresh the data manually
- [ ] Easier way of subtracting days if you've added too many. A negative number isn't very user friendly :)
- [ ] Add ToS
- [ ] Add Privacy Policy