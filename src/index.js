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

const question_collection_path = "test"

// Set up listened on new messages
db.collection(question_collection_path).onSnapshot(docs => {
  const questions = [];

  docs.forEach(doc => {
    questions.push(doc.data());
  });

  console.log("Received new messages: ", questions);
  app.ports.receiveQuestions.send(questions);
});

app.ports.submitQuestion.subscribe(data => {
  console.log("Submitting question to database: ",  data);

  db.collection(question_collection_path)
    .add(data)
    .catch(error => {
      app.ports.error.send({
        code: error.code,
        message: error.message
      });
    });
});

registerServiceWorker();
