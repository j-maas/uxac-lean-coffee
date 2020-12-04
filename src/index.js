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

const discussion_collection_path = "test_discussion"
const topic_collection_path = "test_topics"
const votes_collection_path = "test_votes"

firebase.auth().signInAnonymously()
  .then(user => {
    const id = user.user.uid
    console.log(`User is logged in with id ${id}.`)

    app.ports.receiveUser.send({ id: id })
  })
  .catch(sendErrorToElm);

db.collection(topic_collection_path).onSnapshot(docs => {
  const topics = [];

  docs.forEach(doc => {
    const topic = doc.data();
    topic.id = doc.id;
    topics.push(topic);
  });

  console.log("Received new topics: ", topics);
  app.ports.receiveTopics.send(topics);
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

db.collection(discussion_collection_path).doc("discussed").onSnapshot(snapshot => {
  const data = snapshot.data();
  console.log("Received new discussed topic: ", data);
  app.ports.receiveDiscussedTopic.send(data);
});

app.ports.submitTopic.subscribe(data => {
  console.log("Submitting topic to database: ", data);

  db.collection(topic_collection_path)
    .add(data)
    .catch(sendErrorToElm);
});

app.ports.deleteTopic.subscribe(id => {
  console.log(`Deleting topic with id ${id}.`);

  db.collection(votes_collection_path).where("topicId", "==", id)
    .get()
    .then(votes => {
      votes.forEach(vote => {
        vote.ref.delete();
      });
    })
    .catch(sendErrorToElm);

  db.collection(topic_collection_path).doc(id)
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

app.ports.submitDiscussedTopic.subscribe(data => {
  db.collection(discussion_collection_path)
    .doc("discussed")
    .set(data)
    .catch(sendErrorToElm);
});

function getVoteId(data) {
  return `${data.userId}:${data.topicId}`;
}

function sendErrorToElm(error) {
  app.ports.errorReceived.send({
    code: error.code,
    message: error.message
  });
}

registerServiceWorker();
