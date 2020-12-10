# UXAC Lean Coffee
[![test status](https://github.com/Y0hy0h/uxac-lean-coffee/workflows/Tests/badge.svg)](https://github.com/Y0hy0h/uxac-lean-coffee/actions?query=workflow%3ATests)

## Getting started
1. `$ npm install`
2. Create an `.env` next to this file that uses the information your Firebase SDK [config object](https://firebase.google.com/docs/web/setup#config-object) like so:
    ```
    ELM_APP_API_KEY="AIpaSyCjjxds-Qrk2Vfjj8qktPuzEgwerggeg"
    ELM_APP_AUTH_DOMAIN="your-app.firebaseapp.com"
    ELM_APP_PROJECT_ID="your-app"
    ELM_APP_STORAGE_BUCKET="your-app.appspot.com"
    ELM_APP_MESSAGING_SENDER_ID="926651954567"
    ELM_APP_APP_ID="1:916641954567:web:2a765b103fea3041"
    
    # Set the following, if you deploy the app to a subpath.
    # Otherwise links will be relative to the root and the app will not load, because its file can't be found.
    # For more information, see https://github.com/halfzebra/create-elm-app/blob/master/template/README.md#changing-the-base-path-of-the-assets-in-the-html.
    #
    # PUBLIC_URL="my-domain.org/lean-coffee"
    ```

    _Note: All the information in the .env file is not secret, and can be shared. In fact, it will be accessible to anyone using your app via the console. [This is not a problem](https://stackoverflow.com/questions/37482366/is-it-safe-to-expose-firebase-apikey-to-the-public)!_
3. `$ npm start`

## History
The initial setup was a clone of the very nice and compact https://github.com/jlengrand/elm-firebase. We then adapted it to our needs.