import firebase from "firebase/app";
import "firebase/auth";
import "firebase/firestore";
import * as firebaseui from "firebaseui";
import { Elm } from "./Main.elm";
import firebaseConfig from "./firebaseConfig.json";

firebase.initializeApp(firebaseConfig);
const db = firebase.firestore();
const firebaseuiAuth = new firebaseui.auth.AuthUI(firebase.auth());

customElements.define(
  "firebase-auth",
  class extends HTMLElement {
    connectedCallback() {
      if (this.id !== "") {
        this.childId = this.id;
        this.id = "";
      } else {
        this.childId = "firebase-auth";
      }

      this.innerHTML = `
      <div id="${this.childId}"></div>
      `;
      firebaseuiAuth.start(`#${this.childId}`, {
        signInFlow: "popup",
        callbacks: {
          signInSuccessWithAuthResult: function() {
            // Prevent redirects and such
            return false;
          },
        },
        signInOptions: [
          firebase.auth.EmailAuthProvider.PROVIDER_ID,
          firebase.auth.GoogleAuthProvider.PROVIDER_ID,
        ],
        tosUrl: "TODO",
        privacyPolicyUrl: "TODO",
      });
    }
  },
);

const app = Elm.Main.init({ 
  node: document.getElementById("elm-root"),
  flags: (new Date()).getFullYear()
});

// AUTH

app.ports.logout.subscribe(function() {
  firebase.auth().signOut();
});

firebase.auth().onAuthStateChanged(function(user) {
  if (user) {
    app.ports.login.send(user);
  } else {
    app.ports.loggedOut.send(null);
  }
});

// DATABASE

app.ports.createSelf.subscribe(function(uid) {
  db.collection("pto").doc(uid).set({ years: { 2019: { days: 0 } } });
});

app.ports.getPto.subscribe(function() {
  getPto();
});

app.ports.updatePto.subscribe(function([uid, years]) {
  db
    .collection("pto")
    .doc(uid)
    .update({
      years,
    })
    .then(function() {
      getPto();
    });
});

app.ports.setName.subscribe(function([uid, name]) {
  db
    .collection("pto")
    .doc(uid)
    .update({
      name,
    })
    .then(function() { 
      getPto(); 
    });
});

app.ports.removeName.subscribe(function(uid) {
  db
    .collection("pto")
    .doc(uid)
    .update({
      name: null,
    })
    .then(function() {
      getPto(); 
    });
});

function getPto() {
  db
    .collection("pto")
    .get()
    .then(function(querySnapshot) {
        const data = {};
        console.log("carl", 0)
        querySnapshot.forEach(function(doc) {
          data[doc.id] = doc.data();
        });
        app.ports.setPto.send(data);
      })
      .catch(function(error) {
        console.log("carl", 1, error)
        // TODO:
        // app.ports.ptoGetError.send(error);
      });
}
