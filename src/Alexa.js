'use strict';

// Alexa

var Alexa = require('alexa-sdk');

exports._init = function(event, context) {
  return Alexa.handler(event, context);
};

exports._registerSay = function(alexa, label, say) {
  var handler = {};
  handler[label] = function() {
    this.response.speak(say);
    this.emit(':responseReady');
  };
  alexa.registerHandlers(handler);
};

exports._registerSayAndListen = function(alexa, label, say, listen) {
  var handler = {};
  handler[label] = function() {
    this.response.speak(say).listen(listen);
    this.emit(':responseReady');
  };
  alexa.registerHandlers(handler);
};

exports._execute = function(alexa) {
  alexa.execute();
};

exports._registerHandler = function(alexa, label, fn) {
  var handler = {};
  handler[label] = function() {
    fn(this)();
  };
  alexa.registerHandlers(handler);
};

exports._speak = function(self, say) {
  self.response.speak(say);
  self.emit(':responseReady');
};

exports._speakAndListen = function(self, say, listen) {
  self.response.speak(say).listen(listen);
  self.emit(':responseReady');
};
