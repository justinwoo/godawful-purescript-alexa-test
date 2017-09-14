var Alexa = require('./output/Alexa');

exports.handler = function(event, context) {
  Alexa.handler(event)(context)();
}
