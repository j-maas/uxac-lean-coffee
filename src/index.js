import "./main.css";
import * as firebase from "firebase/app";
import "firebase/firebase-firestore";

import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

// Just checking envs are defined - Debug statement
// console.log(process.env.ELM_APP_API_KEY !== undefined);

const firebaseConfig = {
  apiKey: process.env.ELM_APP_API_KEY,
  authDomain: process.env.ELM_APP_AUTH_DOMAIN,
  databaseURL: process.env.ELM_APP_DATABASE_URL,
  projectId: process.env.ELM_APP_PROJECT_ID,
  storageBucket: process.env.ELM_APP_STORAGE_BUCKET,
  messagingSenderId: process.env.ELM_APP_MESSAGING_SENDER_ID,
  appId: process.env.ELM_APP_APP_ID
};

firebase.initializeApp(firebaseConfig);

const db = firebase.firestore();

const app = Elm.Main.init({
  node: document.getElementById("root")
});

// Set up listened on new messages
db.collection(`test`).onSnapshot(docs => {
  console.log("Received new snapshot: ", docs);
  const messages = [];

  docs.forEach(doc => {
    if (doc.data().content) {
      messages.push(doc.data().content);
    }
  });

  app.ports.receiveMessages.send({
    messages: messages
  });
});

app.ports.saveMessage.subscribe(data => {
  console.log("Saving message to database: ",  data.content);

  db.collection(`test`)
    .add({
      content: data.content
    })
    .catch(error => {
      app.ports.error.send({
        code: error.code,
        message: error.message
      });
    });
});

registerServiceWorker();
