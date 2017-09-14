'use strict';

// Alexa

var Alexa = require('alexa-sdk');

exports._init = function(event, context) {
  return Alexa.handler(event, context);
};

exports._registerHandler = function(alexa, label, fn) {
  var handler = {};
  handler[label] = function() {
    this.emit(':tell', 'wtf!!!'); // this works
    // fn(this) // this doesn't??
  };
  alexa.registerHandlers(handler);
};

exports._execute = function(alexa) {
  alexa.execute();
};

exports._speak = function(self, say) {
  self.emit(':tell', say);
};

exports._speakAndListen = function(self, say, listen) {
  self.emit(':ask', say, listen);
};
