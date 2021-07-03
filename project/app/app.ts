import express = require('express');

// Create a new express application instance
const app: express.Application = express();

app.get('/', function (req, res) {
  res.send('Hello from another world!');
});

app.get('/ping', function (req, res) {
  res.send('pong');
});

app.listen(3000, function () {
  console.log('Example test app listening on port 3000!');
});