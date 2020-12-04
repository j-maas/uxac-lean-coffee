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
  console.error(error);

  app.ports.errorReceived.send({
    code: error.code,
    message: error.message
  });
}

registerServiceWorker();
