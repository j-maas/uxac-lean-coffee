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
  databaseURL: process.env.ELM_APP_DATABASE_URL,
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
  }
});

const question_collection_path = "test_questions"
const votes_collection_path = "test_votes"

firebase.auth().signInAnonymously()
  .then(user => {
    const id = user.user.uid
    console.log(`User is logged in with id ${id}.`)

    app.ports.receiveUser.send({ id: id })
  })

db.collection(question_collection_path).onSnapshot(docs => {
  const questions = [];

  docs.forEach(doc => {
    const question = doc.data();
    question.id = doc.id;
    questions.push(question);
  });

  console.log("Received new questions: ", questions);
  app.ports.receiveQuestions.send(questions);
});

db.collection(votes_collection_path).onSnapshot(docs => {
  const votes = [];

  docs.forEach(doc => {
    const vote = doc.data();
    votes.push(vote);
  });

  console.log("Received new votes: ", votes);
  app.ports.receiveVotes.send(votes);
});

app.ports.submitQuestion.subscribe(data => {
  console.log("Submitting question to database: ", data);

  db.collection(question_collection_path)
    .add(data)
    .catch(sendErrorToElm);
});

app.ports.deleteQuestion.subscribe(id => {
  console.log(`Deleting question with id ${id}.`);

  db.collection(votes_collection_path).where("questionId", "==", id)
    .get()
    .then(votes => {
      votes.forEach(vote => {
        vote.ref.delete();
      });
    })
    .catch(sendErrorToElm);

  db.collection(question_collection_path).doc(id)
    .delete()
    .catch(sendErrorToElm);
});

app.ports.submitVote.subscribe(data => {
  db.collection(votes_collection_path)
    .doc(getVoteId(data))
    .set(data)
    .catch(sendErrorToElm);
});

app.ports.retractVote.subscribe(data => {
  db.collection(votes_collection_path)
    .doc(getVoteId(data))
    .delete()
    .catch(sendErrorToElm);
});

function getVoteId(data) {
  return `${data.userId}:${data.questionId}`;
}

function sendErrorToElm(error) {
  app.ports.error.send({
    code: error.code,
    message: error.message
  });
}

registerServiceWorker();
