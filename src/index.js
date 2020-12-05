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

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
    timestampField: firebase.firestore.FieldValue.serverTimestamp(),
    isAdmin: window.location.hash === "#admin"
  }
});


// Sign in and pass the user to Elm.

firebase.auth().signInAnonymously()
  .then(user => {
    const id = user.user.uid
    console.log(`User is logged in with id ${id}.`)

    app.ports.receiveUser_.send({ id: id })
  })
  .catch(sendErrorToElm);


// Set up ports so that we can subscribe to collections and docs from Elm.

app.ports.subscribe_.subscribe(info => {
  if (info.kind === "collection") {
    subscribeToCollection(info.path, info.tag)
  } else if (info.kind === "doc") {
    subscribeToDoc(info.path, info.tag)
  } else {
    console.error(`Invalid subscription kind ${info.kind}.`);
  }
});

function subscribeToCollection(path, tag) {
  console.log(`Subscribing to collection at ${path}.`);

  db.collection(path).onSnapshot(snapshot => {
    const docs = [];

    snapshot.forEach(doc => {
      docs.push({
        id: doc.id,
        data: doc.data()
      });
    });

    console.log(`Received new snapshot for collection ${tag}:\n`, docs);
    app.ports.receive_.send({
      tag: tag,
      data: docs,
    });
  })
}

function subscribeToDoc(path, tag) {
  console.log(`Subscribing to doc at ${path}.`);

  db.doc(path).onSnapshot(snapshot => {
    const doc = snapshot.data();

    console.log(`Received new snapshot for doc ${tag}:\n`, doc);
    app.ports.receive_.send({
      tag: tag,
      data: doc,
    });
  })
}


// Set up ports so that we can add and delete docs from Elm.

app.ports.insertDoc_.subscribe(info => {
  console.log(`Adding doc at ${info.path}:\n`, info.doc);

  db.collection(info.path)
    .add(info.doc)
    .catch(sendErrorToElm);
});

app.ports.setDoc_.subscribe(info => {
  console.log(`Setting the doc at ${info.path}:\n`, info.doc);

  db.doc(info.path)
    .set(info.doc)
    .catch(sendErrorToElm);
})

app.ports.deleteDocs_.subscribe(info => {
  console.log(`Deleting the docs at ${info.paths}`);

  info.paths.forEach(path => {
    db.doc(path)
      .delete()
      .catch(sendErrorToElm);
  })
});


function sendErrorToElm(error) {
  console.error(error);

  app.ports.receiveError_.send({
    code: error.code,
    message: error.message
  });
}

registerServiceWorker();
