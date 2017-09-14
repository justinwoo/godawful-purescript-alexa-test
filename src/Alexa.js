'use strict';

// Alexa

var Alexa = require('alexa-sdk');

exports._init = function(event, context) {
  return function() {
    return Alexa.handler(event, context);
  }
}

exports._registerHandler = function(alexa, label, fn) {
  return function() {
    var handler = {label: fn};
    alexa.registerHandlers(handler);
  }
}

exports._execute = function(alexa) {
  return function() {
    alexa.execute();
  }
}

exports._speak = function(self, say) {
  return function() {
    self.speak(say);
    self.emit(':responseReady');
  }
}

exports._speakAndListen = function(self, say, listen) {
  return function() {
    self.speak(say).listen(listen);
    self.emit(':responseReady');
  }
}
