/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/

var js_validator = true;
var alert = function () {};
var id = function () {};
var element = {
  innerHTML: "",
  style: {},
  insertBefore: id,
  removeChild : id,
  createComment: id,
  appendChild: id,
  getElementById: id
};

element.documentElement = element;
var element_fun = function () {
  return element;
};

element.createElement = element_fun;
element.getElementsByTagName = element_fun;
var setTimeout = id;
var location = {};
var document = element;
document.cookie = "OK";
var window = { addEventListener: id };
var navigator = { userAgent : "" };

function release(){}
function eval(){}
function setInterval(){}
function clearInterval(){}
function setTimeout(){}
function parseFloat(){}
function parseInt(){}
function isFinite(){
}




var SyntaxError = {};


var print;
print = (print === undefined ? function(){} : print);

function def(x){
  return (x === undefined ? {} : x);
}
var Math;
Math = def(Math);
var Date;
Date = def(Date);
var arguments;
arguments = def(arguments);
var Array;
Array = def(Array);
var Error;
Error = def(Error);
var String;
String = def(String);
var $=4;
var Object;
Object = def(Object);

var Number;
Number = def(Number);

var RegExp;
RegExp = def(RegExp);

var Node;
Node = def(Node);

var Boolean;
Boolean = def(Boolean);

var JSON = {}
var history = {}

var scrollX = 4;
var scrollY = 2;
