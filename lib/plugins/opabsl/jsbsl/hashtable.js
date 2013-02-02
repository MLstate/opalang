/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * @side{both}
 */

/**
 * @constructor
 */
function Hashtable(hash, equal){
   if(typeof hash != 'undefined'){
     this.hash = hash
   } else {
     this.hash = function(x){return x}
   }

   if(typeof equal != 'undefined'){
     this.equal = equal;
   } else {
     this.equal = function(x, y){return x==y}
   }

   this.size = 0;

   this.entries = {};
}

Hashtable.prototype.clear = function(){
  this.entries = {};
  this.size = 0;
}

Hashtable.prototype.put = function(key, value){
  var entries = this.entries;
  var h = this.hash(key);
  var x = entries[h];
  if(typeof x == 'undefined'){
    entries[h] = [{key:key, value:value}];
    this.size++;
    return;
  }
  for(var i in x){
    var tmp = x[i];
    if (this.equal(tmp.key, key)){
      tmp.value = value;
      return;
    }
  }
  this.size++;
  x.push({key:key, value:value});
  return;
}

Hashtable.prototype.get = function(key){
  var entries = this.entries;
  var h = this.hash(key);
  var x = entries[h];
  if(typeof x == 'undefined'){
    return;
  }
  for(var i in x){
    var tmp = x[i];
    if (this.equal(tmp.key, key)){
      return tmp.value;
    }
  }
  return;
}

Hashtable.prototype.remove = function(key){
  var entries = this.entries;
  var h = this.hash(key);
  var x = entries[h];
  if(typeof x == 'undefined'){
    return;
  }
  for(var i in x){
    var tmp = x[i];
    if (this.equal(tmp.key, key)){
      delete x[i];
      if (x.length == 0) delete entries[h];
      this.size--;
      return;
    }
  }
  return;
}

Hashtable.prototype.containsKey = function(key){
  return (typeof this.get(key) != 'undefined');
}

Hashtable.prototype.values = function(){
  var entries = this.entries;
  var ret = new Array(this.size);
  var x = 0;
  for(var i in entries){
    var tmp = entries[i];
    for(var j in tmp){
      ret[x++] = tmp[j];
    }
  }
  return ret;
}

/**
 * @constructor
 */
function SimpleTable(){
  this.entries = {};
  this.size = 0;
}
SimpleTable.prototype.clear = function(){
  this.entries = {};
  this.size = 0;
}
SimpleTable.prototype.put = function(key, value){
  var prev = this.entries[key];
  if (typeof prev == 'undefined') this.size++;
  this.entries[key]=value;
  return;
}
SimpleTable.prototype.get = function(key){
  return this.entries[key];
}
SimpleTable.prototype.remove = function(key){
  var x = this.entries[key];
  if (typeof x != 'undefined') this.size--;
  delete this.entries[key];
}
SimpleTable.prototype.containsKey = function(key){
  return (typeof this.get(key) != 'undefined');
}
SimpleTable.prototype.values = function(){
  var ret = new Array(this.size);
  var entries = this.entries;
  var x = 0;
  for(var i in entries){
    ret[x++] = {key:i, value:entries[i]};
  }
  return ret;
}
