import * as firebase from "firebase/app";
import "firebase/firebase-firestore";
import "firebase/firebase-auth";

import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

// Just checking envs are defined - Debug statement
// console.log(process.env.ELM_APP_API_KEY !== undefined);

const firebaseConfig = {
  apiKey: process.env.ELM_APP_API_KEY,
  authDomain: process.env.ELM_APP_AUTH_DOMAIN,
  projectId: process.env.ELM_APP_PROJECT_ID,
  storageBucket: process.env.ELM_APP_STORAGE_BUCKET,
  messagingSenderId: process.env.ELM_APP_MESSAGING_SENDER_ID,
  appId: process.env.ELM_APP_APP_ID
};

firebase.initializeApp(firebaseConfig);

const db = firebase.firestore();

var randomValues = new Int32Array(4);
window.crypto.getRandomValues(randomValues);

const uuidSeeds = {
  seed1: randomValues[0],
  seed2: randomValues[1],
  seed3: randomValues[2],
  seed4: randomValues[3],
};

const flags = {
  timestampField: firebase.firestore.FieldValue.serverTimestamp(),
  workspaceQuery: window.location.search,
  uuidSeeds,
};

console.log("Initializing Elm with flags:\n", flags);

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags,
});


// Sign in and pass the user to Elm.

firebase.auth().onAuthStateChanged((currentUser) => {
  if (currentUser !== null) {
    if (currentUser.isAnonymous) {
      const id = currentUser.uid;
      console.log(`User is logged in anonymously with id ${id}.`);

      app.ports.receiveUser_.send({ provider: "anonymous", id });
    } else {
      const provider = currentUser.providerData[0].providerId;
      const id = currentUser.uid;
      const email = currentUser.email;
      console.log(`User is logged in via ${provider} with id ${id}.`);

      app.ports.receiveUser_.send({ provider, id, email });

      console.debug("Checking admin rights.");
      db.collection("admins").doc(id).get()
        .then(() => {
          app.ports.receiveIsAdmin_.send(true);
        })
        .catch(error => {
          if (error.code === "permission-denied") {
            app.ports.receiveIsAdmin_.send(false);
          } else {
            sendErrorToElm(error);
          }
        });
    }

  } else {
    console.log("Logging in user anonymously.");
    firebase.auth()
      .setPersistence(firebase.auth.Auth.Persistence.LOCAL) // Persist the authentication across page loads.
      .then(() => {
        return firebase.auth().signInAnonymously();
      })
      .catch(sendErrorToElm);
  }
});

app.ports.logInWithGoogle_.subscribe(() => {
  var provider = new firebase.auth.GoogleAuthProvider();
  firebase.auth()
    .setPersistence(firebase.auth.Auth.Persistence.LOCAL) // Persist the authentication across page loads.
    .then(() => {
      console.log("Logging in user via google.com.");
      return firebase.auth().signInWithRedirect(provider);
    })
    .catch(sendErrorToElm);
});

app.ports.logOut_.subscribe(() => {
  console.log("Logging out user.");
  firebase.auth().signOut()
    .catch(sendErrorToElm);
});

firebase.auth()
  .getRedirectResult()
  .catch(sendErrorToElm);

// Set up ports so that we can subscribe to collections and docs from Elm.

app.ports.subscribe_.subscribe(info => {
  if (info.kind === "collection") {
    subscribeToCollection(info.path, info.tag);
  } else if (info.kind === "collectionChanges") {
    subscribeToCollectionChanges(info.path, info.tag);
  } else if (info.kind === "doc") {
    subscribeToDoc(info.path, info.tag);
  } else {
    console.error(`Invalid subscription kind ${info.kind}.`);
  }
});

function subscribeToCollection(path, tag) {
  console.debug(`Subscribing to collection at ${path}.`);

  db.collection(path).onSnapshot(snapshot => {
    const docs = [];

    snapshot.forEach(doc => {
      docs.push({
        id: doc.id,
        data: doc.data()
      });
    });

    console.debug(`Received new snapshot for collection ${tag}:\n`, docs);
    app.ports.receive_.send({
      tag: tag,
      data: docs,
    });
  }, sendErrorToElm);
}

function subscribeToCollectionChanges(path, tag) {
  console.debug(`Subscribing to collection changes at ${path}.`);

  db.collection(path).onSnapshot(snapshot => {
    const changes = [];

    snapshot.docChanges().forEach(change => {
      changes.push({
        changeType: change.type,
        id: change.doc.id,
        data: change.doc.data()
      });
    });

    console.debug(`Received new changes for collection ${tag}:\n`, changes);
    app.ports.receive_.send({
      tag: tag,
      data: changes,
    });
  }, sendErrorToElm);
}

function subscribeToDoc(path, tag) {
  console.debug(`Subscribing to doc at ${path}.`);

  db.doc(path).onSnapshot(snapshot => {
    const doc = snapshot.data();

    console.debug(`Received new snapshot for doc ${tag}:\n`, doc);
    app.ports.receive_.send({
      tag: tag,
      data: doc,
    });
  }, sendErrorToElm);
}


// Set up ports so that we can add and delete docs from Elm.

app.ports.insertDoc_.subscribe(info => {
  console.debug(`Adding doc at ${info.path}:\n`, info.doc);

  db.collection(info.path)
    .add(info.doc)
    .catch(sendErrorToElm);
});

app.ports.setDoc_.subscribe(info => {
  console.debug(`Setting the doc at ${info.path}:\n`, info.doc);

  db.doc(info.path)
    .set(info.doc)
    .catch(sendErrorToElm);
});

app.ports.deleteDocs_.subscribe(info => {
  console.debug(`Deleting the docs at ${info.paths}`);

  info.paths.forEach(path => {
    db.doc(path)
      .delete()
      .catch(sendErrorToElm);
  });
});

app.ports.selectTextarea_.subscribe(id => {
  // We cannot do this immediately, as the view will not yet have updated.
  requestAnimationFrame(() => {
    const textarea = document.getElementById(id);

    if (textarea === null) {
      console.warn(`Tried to focus textarea, but could not find any with id ${id}.`);
      return;
    }

    textarea.focus();
    textarea.select();
  });
});

function sendErrorToElm(error) {
  console.error(error);

  app.ports.receiveError_.send({
    code: error.code,
    message: error.message
  });
}

registerServiceWorker();
