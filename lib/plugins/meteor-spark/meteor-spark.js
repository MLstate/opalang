// MIT License applied to the following libraries:
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// ------------------
// Meteor Spark, MIT License
// Copyright (C) 2011-2012 Meteor Development Group
// ------------------
// Underscore.js 1.3.3, MIT License
// Copyright (C) 2009-2012 Jeremy Ashkenas, DocumentCloud Inc.
// Portions of Underscore are inspired or borrowed from Prototype, Oliver Steele's Functional, and John Resig's Micro-Templating.
// For all details and documentation: http://documentcloud.github.com/underscor
// ------------------
// Alea, MIT License
// Copyright (C) 2010 Johannes Baagøe <baagoe@baagoe.org>
// http://baagoe.org/en/wiki/Better_random_numbers_for_javascript
// ------------------
var Meteor = {};
(function() {

  // Baseline setup
  // --------------

  // Establish the root object, `window` in the browser, or `global` on the server.
  var root = this;

  // Save the previous value of the `_` variable.
  var previousUnderscore = root._;

  // Establish the object that gets returned to break out of a loop iteration.
  var breaker = {};

  // Save bytes in the minified (but not gzipped) version:
  var ArrayProto = Array.prototype, ObjProto = Object.prototype, FuncProto = Function.prototype;

  // Create quick reference variables for speed access to core prototypes.
  var slice            = ArrayProto.slice,
      unshift          = ArrayProto.unshift,
      toString         = ObjProto.toString,
      hasOwnProperty   = ObjProto.hasOwnProperty;

  // All **ECMAScript 5** native function implementations that we hope to use
  // are declared here.
  var
    nativeForEach      = ArrayProto.forEach,
    nativeMap          = ArrayProto.map,
    nativeReduce       = ArrayProto.reduce,
    nativeReduceRight  = ArrayProto.reduceRight,
    nativeFilter       = ArrayProto.filter,
    nativeEvery        = ArrayProto.every,
    nativeSome         = ArrayProto.some,
    nativeIndexOf      = ArrayProto.indexOf,
    nativeLastIndexOf  = ArrayProto.lastIndexOf,
    nativeIsArray      = Array.isArray,
    nativeKeys         = Object.keys,
    nativeBind         = FuncProto.bind;

  // Create a safe reference to the Underscore object for use below.
  var _ = function(obj) { return new wrapper(obj); };

  // Export the Underscore object for **Node.js**, with
  // backwards-compatibility for the old `require()` API. If we're in
  // the browser, add `_` as a global object via a string identifier,
  // for Closure Compiler "advanced" mode.
  if (typeof exports !== 'undefined') {
    if (typeof module !== 'undefined' && module.exports) {
      exports = module.exports = _;
    }
    exports._ = _;
  } else {
    root['_'] = _;
  }

  // Current version.
  _.VERSION = '1.3.3';

  // Collection Functions
  // --------------------

  // The cornerstone, an `each` implementation, aka `forEach`.
  // Handles objects with the built-in `forEach`, arrays, and raw objects.
  // Delegates to **ECMAScript 5**'s native `forEach` if available.
  var each = _.each = _.forEach = function(obj, iterator, context) {
    if (obj == null) return;
    if (nativeForEach && obj.forEach === nativeForEach) {
      obj.forEach(iterator, context);
    } else if (obj.length === +obj.length) {
      for (var i = 0, l = obj.length; i < l; i++) {
        if (i in obj && iterator.call(context, obj[i], i, obj) === breaker) return;
      }
    } else {
      for (var key in obj) {
        if (_.has(obj, key)) {
          if (iterator.call(context, obj[key], key, obj) === breaker) return;
        }
      }
    }
  };

  // Return the results of applying the iterator to each element.
  // Delegates to **ECMAScript 5**'s native `map` if available.
  _.map = _.collect = function(obj, iterator, context) {
    var results = [];
    if (obj == null) return results;
    if (nativeMap && obj.map === nativeMap) return obj.map(iterator, context);
    each(obj, function(value, index, list) {
      results[results.length] = iterator.call(context, value, index, list);
    });
    if (obj.length === +obj.length) results.length = obj.length;
    return results;
  };

  // **Reduce** builds up a single result from a list of values, aka `inject`,
  // or `foldl`. Delegates to **ECMAScript 5**'s native `reduce` if available.
  _.reduce = _.foldl = _.inject = function(obj, iterator, memo, context) {
    var initial = arguments.length > 2;
    if (obj == null) obj = [];
    if (nativeReduce && obj.reduce === nativeReduce) {
      if (context) iterator = _.bind(iterator, context);
      return initial ? obj.reduce(iterator, memo) : obj.reduce(iterator);
    }
    each(obj, function(value, index, list) {
      if (!initial) {
        memo = value;
        initial = true;
      } else {
        memo = iterator.call(context, memo, value, index, list);
      }
    });
    if (!initial) throw new TypeError('Reduce of empty array with no initial value');
    return memo;
  };

  // The right-associative version of reduce, also known as `foldr`.
  // Delegates to **ECMAScript 5**'s native `reduceRight` if available.
  _.reduceRight = _.foldr = function(obj, iterator, memo, context) {
    var initial = arguments.length > 2;
    if (obj == null) obj = [];
    if (nativeReduceRight && obj.reduceRight === nativeReduceRight) {
      if (context) iterator = _.bind(iterator, context);
      return initial ? obj.reduceRight(iterator, memo) : obj.reduceRight(iterator);
    }
    var reversed = _.toArray(obj).reverse();
    if (context && !initial) iterator = _.bind(iterator, context);
    return initial ? _.reduce(reversed, iterator, memo, context) : _.reduce(reversed, iterator);
  };

  // Return the first value which passes a truth test. Aliased as `detect`.
  _.find = _.detect = function(obj, iterator, context) {
    var result;
    any(obj, function(value, index, list) {
      if (iterator.call(context, value, index, list)) {
        result = value;
        return true;
      }
    });
    return result;
  };

  // Return all the elements that pass a truth test.
  // Delegates to **ECMAScript 5**'s native `filter` if available.
  // Aliased as `select`.
  _.filter = _.select = function(obj, iterator, context) {
    var results = [];
    if (obj == null) return results;
    if (nativeFilter && obj.filter === nativeFilter) return obj.filter(iterator, context);
    each(obj, function(value, index, list) {
      if (iterator.call(context, value, index, list)) results[results.length] = value;
    });
    return results;
  };

  // Return all the elements for which a truth test fails.
  _.reject = function(obj, iterator, context) {
    var results = [];
    if (obj == null) return results;
    each(obj, function(value, index, list) {
      if (!iterator.call(context, value, index, list)) results[results.length] = value;
    });
    return results;
  };

  // Determine whether all of the elements match a truth test.
  // Delegates to **ECMAScript 5**'s native `every` if available.
  // Aliased as `all`.
  _.every = _.all = function(obj, iterator, context) {
    var result = true;
    if (obj == null) return result;
    if (nativeEvery && obj.every === nativeEvery) return obj.every(iterator, context);
    each(obj, function(value, index, list) {
      if (!(result = result && iterator.call(context, value, index, list))) return breaker;
    });
    return !!result;
  };

  // Determine if at least one element in the object matches a truth test.
  // Delegates to **ECMAScript 5**'s native `some` if available.
  // Aliased as `any`.
  var any = _.some = _.any = function(obj, iterator, context) {
    iterator || (iterator = _.identity);
    var result = false;
    if (obj == null) return result;
    if (nativeSome && obj.some === nativeSome) return obj.some(iterator, context);
    each(obj, function(value, index, list) {
      if (result || (result = iterator.call(context, value, index, list))) return breaker;
    });
    return !!result;
  };

  // Determine if a given value is included in the array or object using `===`.
  // Aliased as `contains`.
  _.include = _.contains = function(obj, target) {
    var found = false;
    if (obj == null) return found;
    if (nativeIndexOf && obj.indexOf === nativeIndexOf) return obj.indexOf(target) != -1;
    found = any(obj, function(value) {
      return value === target;
    });
    return found;
  };

  // Invoke a method (with arguments) on every item in a collection.
  _.invoke = function(obj, method) {
    var args = slice.call(arguments, 2);
    return _.map(obj, function(value) {
      return (_.isFunction(method) ? method || value : value[method]).apply(value, args);
    });
  };

  // Convenience version of a common use case of `map`: fetching a property.
  _.pluck = function(obj, key) {
    return _.map(obj, function(value){ return value[key]; });
  };

  // Return the maximum element or (element-based computation).
  _.max = function(obj, iterator, context) {
    if (!iterator && _.isArray(obj) && obj[0] === +obj[0]) return Math.max.apply(Math, obj);
    if (!iterator && _.isEmpty(obj)) return -Infinity;
    var result = {computed : -Infinity};
    each(obj, function(value, index, list) {
      var computed = iterator ? iterator.call(context, value, index, list) : value;
      computed >= result.computed && (result = {value : value, computed : computed});
    });
    return result.value;
  };

  // Return the minimum element (or element-based computation).
  _.min = function(obj, iterator, context) {
    if (!iterator && _.isArray(obj) && obj[0] === +obj[0]) return Math.min.apply(Math, obj);
    if (!iterator && _.isEmpty(obj)) return Infinity;
    var result = {computed : Infinity};
    each(obj, function(value, index, list) {
      var computed = iterator ? iterator.call(context, value, index, list) : value;
      computed < result.computed && (result = {value : value, computed : computed});
    });
    return result.value;
  };

  // Shuffle an array.
  _.shuffle = function(obj) {
    var shuffled = [], rand;
    each(obj, function(value, index, list) {
      rand = Math.floor(Math.random() * (index + 1));
      shuffled[index] = shuffled[rand];
      shuffled[rand] = value;
    });
    return shuffled;
  };

  // Sort the object's values by a criterion produced by an iterator.
  _.sortBy = function(obj, val, context) {
    var iterator = _.isFunction(val) ? val : function(obj) { return obj[val]; };
    return _.pluck(_.map(obj, function(value, index, list) {
      return {
        value : value,
        criteria : iterator.call(context, value, index, list)
      };
    }).sort(function(left, right) {
      var a = left.criteria, b = right.criteria;
      if (a === void 0) return 1;
      if (b === void 0) return -1;
      return a < b ? -1 : a > b ? 1 : 0;
    }), 'value');
  };

  // Groups the object's values by a criterion. Pass either a string attribute
  // to group by, or a function that returns the criterion.
  _.groupBy = function(obj, val) {
    var result = {};
    var iterator = _.isFunction(val) ? val : function(obj) { return obj[val]; };
    each(obj, function(value, index) {
      var key = iterator(value, index);
      (result[key] || (result[key] = [])).push(value);
    });
    return result;
  };

  // Use a comparator function to figure out at what index an object should
  // be inserted so as to maintain order. Uses binary search.
  _.sortedIndex = function(array, obj, iterator) {
    iterator || (iterator = _.identity);
    var low = 0, high = array.length;
    while (low < high) {
      var mid = (low + high) >> 1;
      iterator(array[mid]) < iterator(obj) ? low = mid + 1 : high = mid;
    }
    return low;
  };

  // Safely convert anything iterable into a real, live array.
  _.toArray = function(obj) {
    if (!obj)                                     return [];
    if (_.isArray(obj))                           return slice.call(obj);
    if (_.isArguments(obj))                       return slice.call(obj);
    if (obj.toArray && _.isFunction(obj.toArray)) return obj.toArray();
    return _.values(obj);
  };

  // Return the number of elements in an object.
  _.size = function(obj) {
    return _.isArray(obj) ? obj.length : _.keys(obj).length;
  };

  // Array Functions
  // ---------------

  // Get the first element of an array. Passing **n** will return the first N
  // values in the array. Aliased as `head` and `take`. The **guard** check
  // allows it to work with `_.map`.
  _.first = _.head = _.take = function(array, n, guard) {
    return (n != null) && !guard ? slice.call(array, 0, n) : array[0];
  };

  // Returns everything but the last entry of the array. Especcialy useful on
  // the arguments object. Passing **n** will return all the values in
  // the array, excluding the last N. The **guard** check allows it to work with
  // `_.map`.
  _.initial = function(array, n, guard) {
    return slice.call(array, 0, array.length - ((n == null) || guard ? 1 : n));
  };

  // Get the last element of an array. Passing **n** will return the last N
  // values in the array. The **guard** check allows it to work with `_.map`.
  _.last = function(array, n, guard) {
    if ((n != null) && !guard) {
      return slice.call(array, Math.max(array.length - n, 0));
    } else {
      return array[array.length - 1];
    }
  };

  // Returns everything but the first entry of the array. Aliased as `tail`.
  // Especially useful on the arguments object. Passing an **index** will return
  // the rest of the values in the array from that index onward. The **guard**
  // check allows it to work with `_.map`.
  _.rest = _.tail = function(array, index, guard) {
    return slice.call(array, (index == null) || guard ? 1 : index);
  };

  // Trim out all falsy values from an array.
  _.compact = function(array) {
    return _.filter(array, function(value){ return !!value; });
  };

  // Return a completely flattened version of an array.
  _.flatten = function(array, shallow) {
    return _.reduce(array, function(memo, value) {
      if (_.isArray(value)) return memo.concat(shallow ? value : _.flatten(value));
      memo[memo.length] = value;
      return memo;
    }, []);
  };

  // Return a version of the array that does not contain the specified value(s).
  _.without = function(array) {
    return _.difference(array, slice.call(arguments, 1));
  };

  // Produce a duplicate-free version of the array. If the array has already
  // been sorted, you have the option of using a faster algorithm.
  // Aliased as `unique`.
  _.uniq = _.unique = function(array, isSorted, iterator) {
    var initial = iterator ? _.map(array, iterator) : array;
    var results = [];
    // The `isSorted` flag is irrelevant if the array only contains two elements.
    if (array.length < 3) isSorted = true;
    _.reduce(initial, function (memo, value, index) {
      if (isSorted ? _.last(memo) !== value || !memo.length : !_.include(memo, value)) {
        memo.push(value);
        results.push(array[index]);
      }
      return memo;
    }, []);
    return results;
  };

  // Produce an array that contains the union: each distinct element from all of
  // the passed-in arrays.
  _.union = function() {
    return _.uniq(_.flatten(arguments, true));
  };

  // Produce an array that contains every item shared between all the
  // passed-in arrays. (Aliased as "intersect" for back-compat.)
  _.intersection = _.intersect = function(array) {
    var rest = slice.call(arguments, 1);
    return _.filter(_.uniq(array), function(item) {
      return _.every(rest, function(other) {
        return _.indexOf(other, item) >= 0;
      });
    });
  };

  // Take the difference between one array and a number of other arrays.
  // Only the elements present in just the first array will remain.
  _.difference = function(array) {
    var rest = _.flatten(slice.call(arguments, 1), true);
    return _.filter(array, function(value){ return !_.include(rest, value); });
  };

  // Zip together multiple lists into a single array -- elements that share
  // an index go together.
  _.zip = function() {
    var args = slice.call(arguments);
    var length = _.max(_.pluck(args, 'length'));
    var results = new Array(length);
    for (var i = 0; i < length; i++) results[i] = _.pluck(args, "" + i);
    return results;
  };

  // If the browser doesn't supply us with indexOf (I'm looking at you, **MSIE**),
  // we need this function. Return the position of the first occurrence of an
  // item in an array, or -1 if the item is not included in the array.
  // Delegates to **ECMAScript 5**'s native `indexOf` if available.
  // If the array is large and already in sort order, pass `true`
  // for **isSorted** to use binary search.
  _.indexOf = function(array, item, isSorted) {
    if (array == null) return -1;
    var i, l;
    if (isSorted) {
      i = _.sortedIndex(array, item);
      return array[i] === item ? i : -1;
    }
    if (nativeIndexOf && array.indexOf === nativeIndexOf) return array.indexOf(item);
    for (i = 0, l = array.length; i < l; i++) if (i in array && array[i] === item) return i;
    return -1;
  };

  // Delegates to **ECMAScript 5**'s native `lastIndexOf` if available.
  _.lastIndexOf = function(array, item) {
    if (array == null) return -1;
    if (nativeLastIndexOf && array.lastIndexOf === nativeLastIndexOf) return array.lastIndexOf(item);
    var i = array.length;
    while (i--) if (i in array && array[i] === item) return i;
    return -1;
  };

  // Generate an integer Array containing an arithmetic progression. A port of
  // the native Python `range()` function. See
  // [the Python documentation](http://docs.python.org/library/functions.html#range).
  _.range = function(start, stop, step) {
    if (arguments.length <= 1) {
      stop = start || 0;
      start = 0;
    }
    step = arguments[2] || 1;

    var len = Math.max(Math.ceil((stop - start) / step), 0);
    var idx = 0;
    var range = new Array(len);

    while(idx < len) {
      range[idx++] = start;
      start += step;
    }

    return range;
  };

  // Function (ahem) Functions
  // ------------------

  // Reusable constructor function for prototype setting.
  var ctor = function(){};

  // Create a function bound to a given object (assigning `this`, and arguments,
  // optionally). Binding with arguments is also known as `curry`.
  // Delegates to **ECMAScript 5**'s native `Function.bind` if available.
  // We check for `func.bind` first, to fail fast when `func` is undefined.
  _.bind = function bind(func, context) {
    var bound, args;
    if (func.bind === nativeBind && nativeBind) return nativeBind.apply(func, slice.call(arguments, 1));
    if (!_.isFunction(func)) throw new TypeError;
    args = slice.call(arguments, 2);
    return bound = function() {
      if (!(this instanceof bound)) return func.apply(context, args.concat(slice.call(arguments)));
      ctor.prototype = func.prototype;
      var self = new ctor;
      var result = func.apply(self, args.concat(slice.call(arguments)));
      if (Object(result) === result) return result;
      return self;
    };
  };

  // Bind all of an object's methods to that object. Useful for ensuring that
  // all callbacks defined on an object belong to it.
  _.bindAll = function(obj) {
    var funcs = slice.call(arguments, 1);
    if (funcs.length == 0) funcs = _.functions(obj);
    each(funcs, function(f) { obj[f] = _.bind(obj[f], obj); });
    return obj;
  };

  // Memoize an expensive function by storing its results.
  _.memoize = function(func, hasher) {
    var memo = {};
    hasher || (hasher = _.identity);
    return function() {
      var key = hasher.apply(this, arguments);
      return _.has(memo, key) ? memo[key] : (memo[key] = func.apply(this, arguments));
    };
  };

  // Delays a function for the given number of milliseconds, and then calls
  // it with the arguments supplied.
  _.delay = function(func, wait) {
    var args = slice.call(arguments, 2);
    return setTimeout(function(){ return func.apply(null, args); }, wait);
  };

  // Defers a function, scheduling it to run after the current call stack has
  // cleared.
  _.defer = function(func) {
    return _.delay.apply(_, [func, 1].concat(slice.call(arguments, 1)));
  };

  // Returns a function, that, when invoked, will only be triggered at most once
  // during a given window of time.
  _.throttle = function(func, wait) {
    var context, args, timeout, throttling, more, result;
    var whenDone = _.debounce(function(){ more = throttling = false; }, wait);
    return function() {
      context = this; args = arguments;
      var later = function() {
        timeout = null;
        if (more) func.apply(context, args);
        whenDone();
      };
      if (!timeout) timeout = setTimeout(later, wait);
      if (throttling) {
        more = true;
      } else {
        result = func.apply(context, args);
      }
      whenDone();
      throttling = true;
      return result;
    };
  };

  // Returns a function, that, as long as it continues to be invoked, will not
  // be triggered. The function will be called after it stops being called for
  // N milliseconds. If `immediate` is passed, trigger the function on the
  // leading edge, instead of the trailing.
  _.debounce = function(func, wait, immediate) {
    var timeout;
    return function() {
      var context = this, args = arguments;
      var later = function() {
        timeout = null;
        if (!immediate) func.apply(context, args);
      };
      if (immediate && !timeout) func.apply(context, args);
      clearTimeout(timeout);
      timeout = setTimeout(later, wait);
    };
  };

  // Returns a function that will be executed at most one time, no matter how
  // often you call it. Useful for lazy initialization.
  _.once = function(func) {
    var ran = false, memo;
    return function() {
      if (ran) return memo;
      ran = true;
      return memo = func.apply(this, arguments);
    };
  };

  // Returns the first function passed as an argument to the second,
  // allowing you to adjust arguments, run code before and after, and
  // conditionally execute the original function.
  _.wrap = function(func, wrapper) {
    return function() {
      var args = [func].concat(slice.call(arguments, 0));
      return wrapper.apply(this, args);
    };
  };

  // Returns a function that is the composition of a list of functions, each
  // consuming the return value of the function that follows.
  _.compose = function() {
    var funcs = arguments;
    return function() {
      var args = arguments;
      for (var i = funcs.length - 1; i >= 0; i--) {
        args = [funcs[i].apply(this, args)];
      }
      return args[0];
    };
  };

  // Returns a function that will only be executed after being called N times.
  _.after = function(times, func) {
    if (times <= 0) return func();
    return function() {
      if (--times < 1) { return func.apply(this, arguments); }
    };
  };

  // Object Functions
  // ----------------

  // Retrieve the names of an object's properties.
  // Delegates to **ECMAScript 5**'s native `Object.keys`
  _.keys = nativeKeys || function(obj) {
    if (obj !== Object(obj)) throw new TypeError('Invalid object');
    var keys = [];
    for (var key in obj) if (_.has(obj, key)) keys[keys.length] = key;
    return keys;
  };

  // Retrieve the values of an object's properties.
  _.values = function(obj) {
    return _.map(obj, _.identity);
  };

  // Return a sorted list of the function names available on the object.
  // Aliased as `methods`
  _.functions = _.methods = function(obj) {
    var names = [];
    for (var key in obj) {
      if (_.isFunction(obj[key])) names.push(key);
    }
    return names.sort();
  };

  // Extend a given object with all the properties in passed-in object(s).
  _.extend = function(obj) {
    each(slice.call(arguments, 1), function(source) {
      for (var prop in source) {
        obj[prop] = source[prop];
      }
    });
    return obj;
  };

  // Return a copy of the object only containing the whitelisted properties.
  _.pick = function(obj) {
    var result = {};
    each(_.flatten(slice.call(arguments, 1)), function(key) {
      if (key in obj) result[key] = obj[key];
    });
    return result;
  };

  // Fill in a given object with default properties.
  _.defaults = function(obj) {
    each(slice.call(arguments, 1), function(source) {
      for (var prop in source) {
        if (obj[prop] == null) obj[prop] = source[prop];
      }
    });
    return obj;
  };

  // Create a (shallow-cloned) duplicate of an object.
  _.clone = function(obj) {
    if (!_.isObject(obj)) return obj;
    return _.isArray(obj) ? obj.slice() : _.extend({}, obj);
  };

  // Invokes interceptor with the obj, and then returns obj.
  // The primary purpose of this method is to "tap into" a method chain, in
  // order to perform operations on intermediate results within the chain.
  _.tap = function(obj, interceptor) {
    interceptor(obj);
    return obj;
  };

  // Internal recursive comparison function.
  function eq(a, b, stack) {
    // Identical objects are equal. `0 === -0`, but they aren't identical.
    // See the Harmony `egal` proposal: http://wiki.ecmascript.org/doku.php?id=harmony:egal.
    if (a === b) return a !== 0 || 1 / a == 1 / b;
    // A strict comparison is necessary because `null == undefined`.
    if (a == null || b == null) return a === b;
    // Unwrap any wrapped objects.
    if (a._chain) a = a._wrapped;
    if (b._chain) b = b._wrapped;
    // Invoke a custom `isEqual` method if one is provided.
    if (a.isEqual && _.isFunction(a.isEqual)) return a.isEqual(b);
    if (b.isEqual && _.isFunction(b.isEqual)) return b.isEqual(a);
    // Compare `[[Class]]` names.
    var className = toString.call(a);
    if (className != toString.call(b)) return false;
    switch (className) {
      // Strings, numbers, dates, and booleans are compared by value.
      case '[object String]':
        // Primitives and their corresponding object wrappers are equivalent; thus, `"5"` is
        // equivalent to `new String("5")`.
        return a == String(b);
      case '[object Number]':
        // `NaN`s are equivalent, but non-reflexive. An `egal` comparison is performed for
        // other numeric values.
        return a != +a ? b != +b : (a == 0 ? 1 / a == 1 / b : a == +b);
      case '[object Date]':
      case '[object Boolean]':
        // Coerce dates and booleans to numeric primitive values. Dates are compared by their
        // millisecond representations. Note that invalid dates with millisecond representations
        // of `NaN` are not equivalent.
        return +a == +b;
      // RegExps are compared by their source patterns and flags.
      case '[object RegExp]':
        return a.source == b.source &&
               a.global == b.global &&
               a.multiline == b.multiline &&
               a.ignoreCase == b.ignoreCase;
    }
    if (typeof a != 'object' || typeof b != 'object') return false;
    // Assume equality for cyclic structures. The algorithm for detecting cyclic
    // structures is adapted from ES 5.1 section 15.12.3, abstract operation `JO`.
    var length = stack.length;
    while (length--) {
      // Linear search. Performance is inversely proportional to the number of
      // unique nested structures.
      if (stack[length] == a) return true;
    }
    // Add the first object to the stack of traversed objects.
    stack.push(a);
    var size = 0, result = true;
    // Recursively compare objects and arrays.
    if (className == '[object Array]') {
      // Compare array lengths to determine if a deep comparison is necessary.
      size = a.length;
      result = size == b.length;
      if (result) {
        // Deep compare the contents, ignoring non-numeric properties.
        while (size--) {
          // Ensure commutative equality for sparse arrays.
          if (!(result = size in a == size in b && eq(a[size], b[size], stack))) break;
        }
      }
    } else {
      // Objects with different constructors are not equivalent.
      if ('constructor' in a != 'constructor' in b || a.constructor != b.constructor) return false;
      // Deep compare objects.
      for (var key in a) {
        if (_.has(a, key)) {
          // Count the expected number of properties.
          size++;
          // Deep compare each member.
          if (!(result = _.has(b, key) && eq(a[key], b[key], stack))) break;
        }
      }
      // Ensure that both objects contain the same number of properties.
      if (result) {
        for (key in b) {
          if (_.has(b, key) && !(size--)) break;
        }
        result = !size;
      }
    }
    // Remove the first object from the stack of traversed objects.
    stack.pop();
    return result;
  }

  // Perform a deep comparison to check if two objects are equal.
  _.isEqual = function(a, b) {
    return eq(a, b, []);
  };

  // Is a given array, string, or object empty?
  // An "empty" object has no enumerable own-properties.
  _.isEmpty = function(obj) {
    if (obj == null) return true;
    if (_.isArray(obj) || _.isString(obj)) return obj.length === 0;
    for (var key in obj) if (_.has(obj, key)) return false;
    return true;
  };

  // Is a given value a DOM element?
  _.isElement = function(obj) {
    return !!(obj && obj.nodeType == 1);
  };

  // Is a given value an array?
  // Delegates to ECMA5's native Array.isArray
  _.isArray = nativeIsArray || function(obj) {
    return toString.call(obj) == '[object Array]';
  };

  // Is a given variable an object?
  _.isObject = function(obj) {
    return obj === Object(obj);
  };

  // Is a given variable an arguments object?
  _.isArguments = function(obj) {
    return toString.call(obj) == '[object Arguments]';
  };
  if (!_.isArguments(arguments)) {
    _.isArguments = function(obj) {
      return !!(obj && _.has(obj, 'callee'));
    };
  }

  // Is a given value a function?
  _.isFunction = function(obj) {
    return toString.call(obj) == '[object Function]';
  };

  // Is a given value a string?
  _.isString = function(obj) {
    return toString.call(obj) == '[object String]';
  };

  // Is a given value a number?
  _.isNumber = function(obj) {
    return toString.call(obj) == '[object Number]';
  };

  // Is a given object a finite number?
  _.isFinite = function(obj) {
    return _.isNumber(obj) && isFinite(obj);
  };

  // Is the given value `NaN`?
  _.isNaN = function(obj) {
    // `NaN` is the only value for which `===` is not reflexive.
    return obj !== obj;
  };

  // Is a given value a boolean?
  _.isBoolean = function(obj) {
    return obj === true || obj === false || toString.call(obj) == '[object Boolean]';
  };

  // Is a given value a date?
  _.isDate = function(obj) {
    return toString.call(obj) == '[object Date]';
  };

  // Is the given value a regular expression?
  _.isRegExp = function(obj) {
    return toString.call(obj) == '[object RegExp]';
  };

  // Is a given value equal to null?
  _.isNull = function(obj) {
    return obj === null;
  };

  // Is a given variable undefined?
  _.isUndefined = function(obj) {
    return obj === void 0;
  };

  // Has own property?
  _.has = function(obj, key) {
    return hasOwnProperty.call(obj, key);
  };

  // Utility Functions
  // -----------------

  // Run Underscore.js in *noConflict* mode, returning the `_` variable to its
  // previous owner. Returns a reference to the Underscore object.
  _.noConflict = function() {
    root._ = previousUnderscore;
    return this;
  };

  // Keep the identity function around for default iterators.
  _.identity = function(value) {
    return value;
  };

  // Run a function **n** times.
  _.times = function (n, iterator, context) {
    for (var i = 0; i < n; i++) iterator.call(context, i);
  };

  // Escape a string for HTML interpolation.
  _.escape = function(string) {
    return (''+string).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;').replace(/'/g, '&#x27;').replace(/\//g,'&#x2F;');
  };

  // If the value of the named property is a function then invoke it;
  // otherwise, return it.
  _.result = function(object, property) {
    if (object == null) return null;
    var value = object[property];
    return _.isFunction(value) ? value.call(object) : value;
  };

  // Add your own custom functions to the Underscore object, ensuring that
  // they're correctly added to the OOP wrapper as well.
  _.mixin = function(obj) {
    each(_.functions(obj), function(name){
      addToWrapper(name, _[name] = obj[name]);
    });
  };

  // Generate a unique integer id (unique within the entire client session).
  // Useful for temporary DOM ids.
  var idCounter = 0;
  _.uniqueId = function(prefix) {
    var id = idCounter++;
    return prefix ? prefix + id : id;
  };

  // Add a "chain" function, which will delegate to the wrapper.
  _.chain = function(obj) {
    return _(obj).chain();
  };

  // The OOP Wrapper
  // ---------------

  // If Underscore is called as a function, it returns a wrapped object that
  // can be used OO-style. This wrapper holds altered versions of all the
  // underscore functions. Wrapped objects may be chained.
  var wrapper = function(obj) { this._wrapped = obj; };

  // Expose `wrapper.prototype` as `_.prototype`
  _.prototype = wrapper.prototype;

  // Helper function to continue chaining intermediate results.
  var result = function(obj, chain) {
    return chain ? _(obj).chain() : obj;
  };

  // A method to easily add functions to the OOP wrapper.
  var addToWrapper = function(name, func) {
    wrapper.prototype[name] = function() {
      var args = slice.call(arguments);
      unshift.call(args, this._wrapped);
      return result(func.apply(_, args), this._chain);
    };
  };

  // Add all of the Underscore functions to the wrapper object.
  _.mixin(_);

  // Add all mutator Array functions to the wrapper.
  each(['pop', 'push', 'reverse', 'shift', 'sort', 'splice', 'unshift'], function(name) {
    var method = ArrayProto[name];
    wrapper.prototype[name] = function() {
      var wrapped = this._wrapped;
      method.apply(wrapped, arguments);
      var length = wrapped.length;
      if ((name == 'shift' || name == 'splice') && length === 0) delete wrapped[0];
      return result(wrapped, this._chain);
    };
  });

  // Add all accessor Array functions to the wrapper.
  each(['concat', 'join', 'slice'], function(name) {
    var method = ArrayProto[name];
    wrapper.prototype[name] = function() {
      return result(method.apply(this._wrapped, arguments), this._chain);
    };
  });

  // Start chaining a wrapped Underscore object.
  wrapper.prototype.chain = function() {
    this._chain = true;
    return this;
  };

  // Extracts the result from a wrapped and chained object.
  wrapper.prototype.value = function() {
    return this._wrapped;
  };

}).call(this);
// XXX dups packages/minimongo/uuid.js

// Meteor.random() -- known good PRNG, replaces Math.random()
// Meteor.uuid() -- returns RFC 4122 v4 UUID.

// see http://baagoe.org/en/wiki/Better_random_numbers_for_javascript
// for a full discussion and Alea implementation.

// Copyright (C) 2010 by Johannes Baagøe <baagoe@baagoe.org>
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

Meteor._Alea = function () {
  function Mash() {
    var n = 0xefc8249d;

    var mash = function(data) {
      data = data.toString();
      for (var i = 0; i < data.length; i++) {
        n += data.charCodeAt(i);
        var h = 0.02519603282416938 * n;
        n = h >>> 0;
        h -= n;
        h *= n;
        n = h >>> 0;
        h -= n;
        n += h * 0x100000000; // 2^32
      }
      return (n >>> 0) * 2.3283064365386963e-10; // 2^-32
    };

    mash.version = 'Mash 0.9';
    return mash;
  }

  return (function (args) {
    var s0 = 0;
    var s1 = 0;
    var s2 = 0;
    var c = 1;

    if (args.length == 0) {
      args = [+new Date];
    }
    var mash = Mash();
    s0 = mash(' ');
    s1 = mash(' ');
    s2 = mash(' ');

    for (var i = 0; i < args.length; i++) {
      s0 -= mash(args[i]);
      if (s0 < 0) {
        s0 += 1;
      }
      s1 -= mash(args[i]);
      if (s1 < 0) {
        s1 += 1;
      }
      s2 -= mash(args[i]);
      if (s2 < 0) {
        s2 += 1;
      }
    }
    mash = null;

    var random = function() {
      var t = 2091639 * s0 + c * 2.3283064365386963e-10; // 2^-32
      s0 = s1;
      s1 = s2;
      return s2 = t - (c = t | 0);
    };
    random.uint32 = function() {
      return random() * 0x100000000; // 2^32
    };
    random.fract53 = function() {
      return random() +
        (random() * 0x200000 | 0) * 1.1102230246251565e-16; // 2^-53
    };
    random.version = 'Alea 0.9';
    random.args = args;
    return random;

  } (Array.prototype.slice.call(arguments)));
};

// instantiate RNG.  use the default seed, which is current time.
Meteor.random = new Meteor._Alea();

// RFC 4122 v4 UUID.
Meteor.uuid = function () {
  var s = [];
  var hexDigits = "0123456789abcdef";
  for (var i = 0; i < 36; i++) {
    s[i] = hexDigits.substr(Math.floor(Meteor.random() * 0x10), 1);
  }
  s[14] = "4";
  s[19] = hexDigits.substr((s[19] & 0x3) | 0x8, 1);
  s[8] = s[13] = s[18] = s[23] = "-";

  var uuid = s.join("");
  return uuid;
};
(function () {
  var pending_invalidate = [];
  var next_id = 1;

  var Context = function () {
    // Each context has a unique number. You can use this to avoid
    // storing multiple copies of the same context in your
    // invalidation list. The id is an integer >= 1.
    this.id = next_id++;
    this._callbacks = [];
    this._invalidated = false;
  };
  Context.current = null;

  _.extend(Context.prototype, {
    run: function (f) {
      var previous = Context.current;
      Context.current = this;
      try { return f(); }
      finally { Context.current = previous; }
    },

    // we specifically guarantee that this doesn't call any
    // invalidation functions (before returning) -- it just marks the
    // context as invalidated.
    invalidate: function () {
      if (!this._invalidated) {
        this._invalidated = true;
        // If this is first invalidation, schedule a flush.
        // We may be inside a flush already, in which case this
        // is unnecessary but harmless.
        if (!pending_invalidate.length)
          setTimeout(Meteor.flush, 0);
        pending_invalidate.push(this);
      }
    },

    // calls f immediately if this context was already
    // invalidated. receives one argument, the context.
    onInvalidate: function (f) {
      if (this._invalidated)
        f(this);
      else
        this._callbacks.push(f);
    }
  });

  _.extend(Meteor, {
    // XXX specify what happens when flush calls flush. eg, flushing
    // causes a dom update, which causes onblur, which invokes an
    // event handler that calls flush. it's probably an exception --
    // no flushing from inside onblur. can also imagine routing onblur
    // through settimeout(0), which is probably what the user wants.
    // https://app.asana.com/0/159908330244/385138233856
    flush: function () {
      while (pending_invalidate.length) {
        var pending = pending_invalidate;
        pending_invalidate = [];

        _.each(pending, function (ctx) {
          _.each(ctx._callbacks, function (f) {
            try {
              f(ctx);
            } catch (e) {
              Meteor._debug("Exception from Meteor.flush:", e.stack);
            }
          });
          delete ctx._callbacks; // maybe help the GC
        });
      }
    },

    deps: {
      Context: Context
    }
  });
})();
(function () {
  // XXX Document, test, and remove the leading underscore from everything.

  ////////// Meteor.deps._ContextSet

  // Constructor for an empty _ContextSet.
  //
  // A _ContextSet is used to hold a set of Meteor.deps.Contexts that
  // are to be invalidated at some future time.  If a Context in the
  // set becomes invalidated for any reason, it's immediately removed
  // from the set.
  var _ContextSet = function () {
    this._contextsById = {};
  };

  // Adds the Context `ctx` to this set if it is not already
  // present.  Returns true if the context is new to this set.
  _ContextSet.prototype.add = function (ctx) {
    var self = this;
    if (ctx && ! (ctx.id in self._contextsById)) {
      self._contextsById[ctx.id] = ctx;
      ctx.onInvalidate(function () {
        delete self._contextsById[ctx.id];
      });
      return true;
    }
    return false;
  };

  // Adds the current Context to this set if there is one.  Returns
  // true if there is a current Context and it's new to the set.
  _ContextSet.prototype.addCurrentContext = function () {
    var self = this;
    var context = Meteor.deps.Context.current;
    if (! context)
      return false;
    return self.add(context);
  };

  // Invalidate all Contexts in this set.  They will be removed
  // from the set as a consequence.
  _ContextSet.prototype.invalidateAll = function () {
    var self = this;
    for (var id in self._contextsById)
      self._contextsById[id].invalidate();
  };

  // Returns true if there are no Contexts in this set.
  _ContextSet.prototype.isEmpty = function () {
    var self = this;
    for(var id in self._contextsById)
      return false;
    return true;
  };

  Meteor.deps._ContextSet = _ContextSet;

  ////////// Meteor.autorun

  // Run f(). Record its dependencies. Rerun it whenever the
  // dependencies change.
  //
  // Returns an object with a stop() method. Call stop() to stop the
  // rerunning.  Also passes this object as an argument to f.
  Meteor.autorun = function (f) {
    var ctx;
    var slain = false;
    var handle = {
      stop: function () {
        slain = true;
        ctx.invalidate();
      }
    };
    var rerun = function () {
      if (slain)
        return;
      ctx = new Meteor.deps.Context;
      ctx.run(function () { f.call(this, handle); });
      ctx.onInvalidate(rerun);
    };
    rerun();
    return handle;
  };

  ////////// Meteor._atFlush

  // Run 'f' at Meteor.flush()-time. If atFlush is called multiple times,
  // we guarantee that the 'f's will run in the same order that
  // atFlush was called on them.  If we are inside a Meteor.flush() already,
  // f will be scheduled as part of the current flush().

  var atFlushQueue = [];
  var atFlushContext = null;
  Meteor._atFlush = function (f) {
    atFlushQueue.push(f);

    if (! atFlushContext) {
      atFlushContext = new Meteor.deps.Context;
      atFlushContext.onInvalidate(function () {
        var f;
        while ((f = atFlushQueue.shift())) {
          // Since atFlushContext is truthy, if f() calls atFlush
          // reentrantly, it's guaranteed to append to atFlushQueue and
          // not contruct a new atFlushContext.
          try {
            f();
          } catch (e) {
            Meteor._debug("Exception from Meteor._atFlush:", e.stack);
          }
        }
        atFlushContext = null;
      });

      atFlushContext.invalidate();
    }
  };

})();// Stand back, I'm going to try SCIENCE.

(function () {
  // Possible optimization: get rid of _startIndex/_endIndex and just search
  // the list. Not clear which strategy will be faster.

  // Possible extension: could allow zero-length ranges is some cases,
  // by encoding both 'enter' and 'leave' type events in the same list

  var canSetTextProps = (function () {
    // IE8 and earlier don't support expando attributes on text nodes,
    // but fortunately they are allowed on comments.
    var testElem = document.createTextNode("");
    var exception;
    try {
      testElem.test = 123;
    } catch (exception) { }

    return (testElem.test === 123);
  })();

  var wrapEndpoints = function (start, end) {
    if (canSetTextProps) {
      return [start, end];
    } else {
      // IE8 workaround: insert some empty comments.
      // Comments whose text is "IE" are stripped out
      // in cross-browser testing.
      if (start.nodeType === 3 /* text node */) {
        var placeholder = document.createComment("IE");
        start.parentNode.insertBefore(placeholder, start);
        start = placeholder;
      }
      if (end.nodeType === 3 /* text node */) {
        var placeholder = document.createComment("IE");
        end.parentNode.insertBefore(placeholder, end.nextSibling);
        end = placeholder;
      }
      return [start, end];
    }
  };


  // This is a constructor (invoke it as 'new LiveRange').
  //
  // Create a range, tagged 'tag', that includes start, end, and all
  // the nodes between them, and the children of all of those nodes,
  // but includes no other nodes. If there are other ranges tagged
  // 'tag' that contain this exact set of nodes, then: if inner is
  // false (the default), the new range will be outside all of them
  // (will contain all of them), or if inner is true, then it will be
  // inside all of them (be contained by all of them.) If there are no
  // other ranges tagged 'tag' that contain this exact set of nodes,
  // then 'inner' is ignored because the nesting of the new range with
  // respect to other ranges is uniquely determined. (Nesting of
  // ranges with different tags is undefined.)
  //
  // To track the range as it's relocated, some of the DOM nodes that
  // are part of the range will have an expando attribute set on
  // them. The name of the expando attribute will be the value of
  // 'tag', so pick something that won't collide.
  //
  // Instead of start and end, you can pass a document or
  // documentfragment for start and leave end undefined. Or you can
  // pass a node for start and leave end undefined, in which case end
  // === start. If start and end are distinct nodes, they must be
  // siblings.
  //
  // You can set any attributes you like on the returned LiveRange
  // object, with two exceptions. First, attribute names that start
  // with '_' are reserved. Second, the attribute 'tag' contains the
  // tag name of this range and mustn't be changed.
  //
  // It would be possible to add a fast path through this function
  // when caller can promise that there is no range that starts on
  // start that does not end by end, and vice versa. eg: when start
  // and end are the first and last child of their parent respectively
  // or when caller is building up the range tree from the inside
  // out. Let's wait for the profiler to tell us to add this.
  //
  // XXX Should eventually support LiveRanges where start === end
  // and start.parentNode is null.
  LiveRange = function (tag, start, end, inner) {
    if (start.nodeType === 11 /* DocumentFragment */) {
      end = start.lastChild;
      start = start.firstChild;
    } else {
      if (! start.parentNode)
        throw new Error("LiveRange start and end must have a parent");
    }
    end = end || start;

    this.tag = tag; // must be set before calling _ensureTag

    var endpoints = wrapEndpoints(start, end);
    start = this._ensureTag(endpoints[0]);
    end = this._ensureTag(endpoints[1]);

    // Decide at what indices in start[tag][0] and end[tag][1] we
    // should insert the new range.
    //
    // The start[tag][0] array lists the other ranges that start at
    // `start`, and we must choose an insertion index that puts us
    // inside the ones that end at later siblings, and outside the ones
    // that end at earlier siblings.  The ones that end at the same
    // sibling (i.e. share both our start and end) we must be inside
    // or outside of depending on `inner`.  The array lists ranges
    // from the outside in.
    //
    // The same logic applies to end[tag][1], which lists the other ranges
    // that happen to end at `end` from in the inside out.
    //
    // Liveranges technically start just before, and end just after, their
    // start and end nodes to which the liverange data is attached.

    var startIndex = findPosition(start[tag][0], true, end, start, inner);
    var endIndex = findPosition(end[tag][1], false, start, end, inner);

    // this._start is the node N such that we begin before N, but not
    // before the node before N in the preorder traversal of the
    // document (if there is such a node.) this._start[this.tag][0]
    // will be the list of all LiveRanges for which this._start is N,
    // including us, sorted in the order that the ranges start. and
    // finally, this._startIndex is the value such that
    // this._start[this.tag][0][this._startIndex] === this.
    //
    // Similarly for this._end, except it's the node N such that we end
    // after N, but not after the node after N in the postorder
    // traversal; and the data is stored in this._end[this.tag][1], and
    // it's sorted in the order that the ranges end.

    // Set this._start, this._end, this._startIndex, this._endIndex
    this._insertEntries(start, 0, startIndex, [this]);
    this._insertEntries(end, 1, endIndex, [this]);
  };

  var findPosition = function(ranges, findEndNotStart, edge, otherEdge, inner) {
    var index;
    // For purpose of finding where we belong in start[tag][0],
    // walk the array and determine where we start to see ranges
    // end at `end` (==edge) or earlier.  For the purpose of finding
    // where we belong in end[tag][1], walk the array and determine
    // where we start to see ranges start at `start` (==edge) or
    // earlier.  In both cases, we slide a sibling pointer backwards
    // looking for `edge`, though the details are slightly different.
    //
    // Use `inner` to take first or last candidate index for insertion.
    // Candidate indices are:  Right before a range whose edge is `edge`
    // (i.e., a range with same start and end as we are creating),
    // or the index where ranges start to have edges earlier than `edge`
    // (treating the end of the list as such an index).  We detect the
    // latter case when `n` hits `edge` without hitting the edge of the
    // current range; that is, it is about to move past `edge`.  This is
    // always an appropriate time to stop.
    //
    // Joint traversal of the array and DOM should be fast.  The most
    // expensive thing to happen would be a single walk from lastChild
    // to end looking for range ends, or from end to start looking for
    // range starts.
    //
    // invariant: n >= edge ("n is after, or is, edge")
    var initialN = (findEndNotStart ? edge.parentNode.lastChild : otherEdge);
    var takeFirst = (findEndNotStart ? ! inner : inner);
    for(var i=0, n=initialN; i<=ranges.length; i++) {
      var r = ranges[i];
      var curEdge = r && (findEndNotStart ? r._end : r._start);
      while (n !== curEdge && n !== edge) {
        n = n.previousSibling;
      }
      if (curEdge === edge) {
        index = i;
        if (takeFirst) break;
      } else if (n === edge) {
        index = i;
        break;
      }
    }
    return index;
  };

  LiveRange.prototype._ensureTag = function (node) {
    if (!(this.tag in node))
      node[this.tag] = [[], []];
    return node;
  };

  var canDeleteExpandos = (function() {
    // IE7 can't remove expando attributes from DOM nodes with
    // delete. Instead you must remove them with node.removeAttribute.
    var node = document.createElement("DIV");
    var exception;
    var result = false;
    try {
      node.test = 12;
      delete node.test;
      result = true;
    } catch (exception) { }
    return result;
  })();

  LiveRange._cleanNode = function (tag, node, force) {
    var data = node[tag];
    if (data && (!(data[0].length + data[1].length) || force)) {
      if (canDeleteExpandos)
        delete node[tag];
      else
        node.removeAttribute(tag);
    }
  };

  // Delete a LiveRange. This is analogous to removing a DOM node from
  // its parent -- it will no longer appear when traversing the tree
  // with visit().
  //
  // On modern browsers there is no requirement to delete LiveRanges on
  // defunct nodes. They will be garbage collected just like any other
  // object. However, on old versions of IE, you probably do need to
  // manually remove all ranges because IE can't GC reference cycles
  // through the DOM.
  //
  // Pass true for `recursive` to also destroy all descendent ranges.
  LiveRange.prototype.destroy = function (recursive) {
    var self = this;

    if (recursive) {
      // recursive case: destroy all descendent ranges too
      // (more efficient than actually recursing)

      this.visit(function(isStart, range) {
        if (isStart) {
          range._start = null;
          range._end = null;
        }
      }, function(isStart, node) {
        if (! isStart) {
          // when leaving a node, force-clean its children
          for(var n = node.firstChild; n; n = n.nextSibling) {
            LiveRange._cleanNode(self.tag, n, true);
          }
        }
      });

      this._removeEntries(this._start, 0, this._startIndex);
      this._removeEntries(this._end, 1, 0, this._endIndex + 1);

      if (this._start !== this._end) {
        // force-clean the top-level nodes in this, besides _start and _end
        for(var n = this._start.nextSibling;
            n !== this._end;
            n = n.nextSibling) {
          LiveRange._cleanNode(self.tag, n, true);
        }

        // clean ends on this._start and starts on this._end
        if (this._start[self.tag])
          this._removeEntries(this._start, 1);
        if (this._end[self.tag])
          this._removeEntries(this._end, 0);
      }

      this._start = this._end = null;

    } else {
      this._removeEntries(this._start, 0, this._startIndex, this._startIndex + 1);
      this._removeEntries(this._end, 1, this._endIndex, this._endIndex + 1);
      this._start = this._end = null;
    }
  };

  // Return the first node in the range (in preorder traversal)
  LiveRange.prototype.firstNode = function () {
    return this._start;
  };

  // Return the last node in the range (in postorder traversal)
  LiveRange.prototype.lastNode = function () {
    return this._end;
  };

  // Return the node that immediately contains this LiveRange, that is,
  // the parentNode of firstNode and lastNode.
  LiveRange.prototype.containerNode = function() {
    return this._start.parentNode;
  };

  // Walk through the current contents of a LiveRange, enumerating
  // either the contained ranges (with the same tag as this range),
  // the contained elements, or both.
  //
  // visitRange(isStart, range) is invoked for each range
  // start-point or end-point that we encounter as we walk the range
  // stored in 'this' (not counting the endpoints of 'this' itself.)
  // visitNode(isStart, node) is similar but for nodes.  Both
  // functions are optional.
  //
  // If you return false (i.e. a value === false) from visitRange
  // or visitNode when isStart is true, the children of that range
  // or node are skipped, and the next callback will be the same
  // range or node with isStart false.
  //
  // If you create or destroy ranges with this tag from a visitation
  // function, results are undefined!
  LiveRange.prototype.visit = function(visitRange, visitNode) {
    visitRange = visitRange || function() {};
    visitNode = visitNode || function() {};

    var tag = this.tag;

    var recurse = function(start, end, startRangeSkip) {
      var startIndex = startRangeSkip || 0;
      var after = end.nextSibling;
      for(var n = start; n && n !== after; n = n.nextSibling) {
        var startData = n[tag] && n[tag][0];
        if (startData && startIndex < startData.length) {
          // immediate child range that starts with n
          var range = startData[startIndex];
          // be robust if visitRange mutates _start or _end;
          // useful in destroy(true)
          var rangeStart = range._start;
          var rangeEnd = range._end;
          if (visitRange(true, range) !== false)
            recurse(rangeStart, rangeEnd, startIndex+1);
          visitRange(false, range);
          n = rangeEnd;
        }
        else {
          // bare node
          if (visitNode(true, n) !== false && n.firstChild)
            recurse(n.firstChild, n.lastChild);
          visitNode(false, n);
        }
        startIndex = 0;
      }
    };

    recurse(this._start, this._end, this._startIndex + 1);
  };

  // startEnd === 0 for starts, 1 for ends
  LiveRange.prototype._removeEntries =
    function(node, startEnd, i, j)
  {
    var entries = node[this.tag][startEnd];
    i = i || 0;
    j = (j || j === 0) ? j : entries.length;
    var removed = entries.splice(i, j-i);
    // fix up remaining ranges (not removed ones)
    for(var a = i; a < entries.length; a++) {
      if (startEnd) entries[a]._endIndex = a;
      else entries[a]._startIndex = a;
    }

    // potentially remove empty liverange data
    if (! entries.length) {
      LiveRange._cleanNode(this.tag, node);
    }

    return removed;
  };

  LiveRange.prototype._insertEntries =
    function(node, startEnd, i, newRanges)
  {
    // insert the new ranges and "adopt" them by setting node pointers
    var entries = node[this.tag][startEnd];
    Array.prototype.splice.apply(entries, [i, 0].concat(newRanges));
    for(var a=i; a < entries.length; a++) {
      if (startEnd) {
        entries[a]._end = node;
        entries[a]._endIndex = a;
      } else {
        entries[a]._start = node;
        entries[a]._startIndex = a;
      }
    }
  };

  // Replace the contents of this range with the provided
  // DocumentFragment. Returns the previous contents as a
  // DocumentFragment.
  //
  // "The right thing happens" with child LiveRanges:
  // - If there were child LiveRanges inside us, they will end up in
  //   the returned DocumentFragment.
  // - If the input DocumentFragment has LiveRanges, they will become
  //   our children.
  //
  // It is illegal for newFrag to be empty.
  LiveRange.prototype.replaceContents = function (newFrag) {
    if (! newFrag.firstChild)
      throw new Error("replaceContents requires non-empty fragment");

    return this.operate(function(oldStart, oldEnd) {
      // Insert new fragment
      oldStart.parentNode.insertBefore(newFrag, oldStart);

      // Pull out departing fragment
      // Possible optimization: use W3C Ranges on browsers that support them
      var retFrag = oldStart.ownerDocument.createDocumentFragment();
      var walk = oldStart;
      while (true) {
        var next = walk.nextSibling;
        retFrag.appendChild(walk);
        if (walk === oldEnd)
          break;
        walk = next;
        if (!walk)
          throw new Error("LiveRanges must begin and end on siblings in order");
      }

      return retFrag;
    });
  };


  // Perform a user-specified DOM mutation on the contents of this range.
  //
  // `func` is called with two parameters, `oldStart` and `oldEnd`, equal
  // to the original firstNode() and lastNode() of this range.  `func` is allowed
  // to perform arbitrary operations on the sequence of nodes from `oldStart`
  // to `oldEnd` and on child ranges of this range.  `func` may NOT call methods
  // on this range itself or otherwise rely on the existence of this range and
  // enclosing ranges.  `func` must leave at least one node to become the new
  // contents of this range.
  //
  // The return value of `func` is returned.
  //
  // This method is a generalization of replaceContents that works by
  // temporarily removing this LiveRange from the DOM and restoring it after
  // `func` has been called.
  LiveRange.prototype.operate = function (func) {
    // boundary nodes of departing fragment
    var oldStart = this._start;
    var oldEnd = this._end;

    // pull off outer liverange data
    var outerStarts =
          this._removeEntries(oldStart, 0, 0, this._startIndex + 1);
    var outerEnds =
          this._removeEntries(oldEnd, 1, this._endIndex);

    var containerNode = oldStart.parentNode;
    var beforeNode = oldStart.previousSibling;
    var afterNode = oldEnd.nextSibling;

    var ret = null;

    // perform user-specifiedDOM manipulation
    ret = func(oldStart, oldEnd);

    // see what we've got...

    var newStart =
          beforeNode ? beforeNode.nextSibling : containerNode.firstChild;
    var newEnd =
          afterNode ? afterNode.previousSibling : containerNode.lastChild;

    if (! newStart || newStart === afterNode) {
      throw new Error("Ranges must contain at least one element");
    }

    // wrap endpoints if necessary
    var newEndpoints = wrapEndpoints(newStart, newEnd);
    newStart = this._ensureTag(newEndpoints[0]);
    newEnd = this._ensureTag(newEndpoints[1]);

    // put the outer liveranges back

    this._insertEntries(newStart, 0, 0, outerStarts);
    this._insertEntries(newEnd, 1, newEnd[this.tag][1].length, outerEnds);

    return ret;
  };

  // Move all liverange data represented in the DOM from sourceNode to
  // targetNode.  targetNode must be capable of receiving liverange tags
  // (for example, a node that has been the first or last node of a liverange
  // before; not a text node in IE).
  //
  // This is a low-level operation suitable for moving liveranges en masse
  // from one DOM tree to another, where transplantTag is called on every
  // pair of nodes such that targetNode takes the place of sourceNode.
  LiveRange.transplantTag = function(tag, targetNode, sourceNode) {

    if (! sourceNode[tag])
      return;

    // copy data pointer
    targetNode[tag] = sourceNode[tag];
    sourceNode[tag] = null;

    var starts = targetNode[tag][0];
    var ends = targetNode[tag][1];

    // fix _start and _end pointers
    for(var i=0;i<starts.length;i++)
      starts[i]._start = targetNode;
    for(var i=0;i<ends.length;i++)
      ends[i]._end = targetNode;
  };

  // Takes two sibling nodes tgtStart and tgtEnd with no LiveRange data on them
  // and a LiveRange srcRange in a separate DOM tree.  Transplants srcRange
  // to span from tgtStart to tgtEnd, and also copies info about enclosing ranges
  // starting on srcRange._start or ending on srcRange._end.  tgtStart and tgtEnd
  // must be capable of receiving liverange tags (for example, nodes that have
  // held liverange data in the past; not text nodes in IE).
  //
  // This is a low-level operation suitable for moving liveranges en masse
  // from one DOM tree to another.
  LiveRange.transplantRange = function(tgtStart, tgtEnd, srcRange) {
    srcRange._ensureTag(tgtStart);
    if (tgtEnd !== tgtStart)
      srcRange._ensureTag(tgtEnd);

    srcRange._insertEntries(
      tgtStart, 0, 0,
      srcRange._start[srcRange.tag][0].slice(0, srcRange._startIndex + 1));
    srcRange._insertEntries(
      tgtEnd, 1, 0,
      srcRange._end[srcRange.tag][1].slice(srcRange._endIndex));
  };

  // Inserts a DocumentFragment immediately before this range.
  // The new nodes are outside this range but inside all
  // enclosing ranges.
  LiveRange.prototype.insertBefore = function(frag) {
    var fragStart = frag.firstChild;

    if (! fragStart) // empty frag
      return;

    // insert into DOM
    this._start.parentNode.insertBefore(frag, this._start);

    // move starts of ranges that begin on this._start, but are
    // outside this, to beginning of fragStart
    this._ensureTag(fragStart);
    this._insertEntries(fragStart, 0, 0,
                         this._removeEntries(this._start, 0, 0,
                                              this._startIndex));
  };

  // Inserts a DocumentFragment immediately after this range.
  // The new nodes are outside this range but inside all
  // enclosing ranges.
  LiveRange.prototype.insertAfter = function(frag) {
    var fragEnd = frag.lastChild;

    if (! fragEnd) // empty frag
      return;

    // insert into DOM
    this._end.parentNode.insertBefore(frag, this._end.nextSibling);

    // move ends of ranges that end on this._end, but are
    // outside this, to end of fragEnd
    this._ensureTag(fragEnd);
    this._insertEntries(fragEnd, 1, fragEnd[this.tag][1].length,
                         this._removeEntries(this._end, 1,
                                              this._endIndex + 1));
  };

  // Extracts this range and its contents from the DOM and
  // puts it into a DocumentFragment, which is returned.
  // All nodes and ranges outside this range are properly
  // preserved.
  //
  // Because liveranges must contain at least one node,
  // it is illegal to perform `extract` if the immediately
  // enclosing range would become empty.  If this precondition
  // is violated, no action is taken and null is returned.
  LiveRange.prototype.extract = function() {
    if (this._startIndex > 0 &&
        this._start[this.tag][0][this._startIndex - 1]._end === this._end) {
      // immediately enclosing range wraps same nodes, so can't extract because
      // it would empty it.
      return null;
    }

    var before = this._start.previousSibling;
    var after = this._end.nextSibling;
    var parent = this._start.parentNode;

    if (this._startIndex > 0) {
      // must be a later node where outer ranges that start here end;
      // move their starts to after
      this._ensureTag(after);
      this._insertEntries(after, 0, 0,
                           this._removeEntries(this._start, 0, 0,
                                                this._startIndex));
    }

    if (this._endIndex < this._end[this.tag][1].length - 1) {
      // must be an earlier node where outer ranges that end here
      // start; move their ends to before
      this._ensureTag(before);
      this._insertEntries(before, 1, before[this.tag][1].length,
                           this._removeEntries(this._end, 1,
                                                this._endIndex + 1));
    }

    var result = document.createDocumentFragment();

    for(var n;
        n = before ? before.nextSibling : parent.firstChild,
        n && n !== after;)
      result.appendChild(n);

    return result;
  };

  // Find the immediately enclosing parent range of this range, or
  // null if this range has no enclosing ranges.
  //
  // If `withSameContainer` is true, we stop looking when we reach
  // this range's container node (the parent of its endpoints) and
  // only return liveranges whose first and last nodes are siblings
  // of this one's.
  LiveRange.prototype.findParent = function(withSameContainer) {
    var result = enclosingRangeSearch(this.tag, this._end, this._endIndex);
    if (result)
      return result;

    if (withSameContainer)
      return null;

    return LiveRange.findRange(this.tag, this.containerNode());
  };

  // Find the nearest enclosing range containing `node`, if any.
  LiveRange.findRange = function(tag, node) {
    var result = enclosingRangeSearch(tag, node);
    if (result)
      return result;

    if (! node.parentNode)
      return null;

    return LiveRange.findRange(tag, node.parentNode);
  };

  var enclosingRangeSearch = function(tag, end, endIndex) {
    // Search for an enclosing range, at the same level,
    // starting at node `end` or after the range whose
    // position in the end array of `end` is `endIndex`.
    // The search works by scanning forwards for range ends
    // while skipping over ranges whose starts we encounter.

    if (typeof endIndex === "undefined")
      endIndex = -1;

    if (end[tag] && endIndex + 1 < end[tag][1].length) {
      // immediately enclosing range ends at same node as this one
      return end[tag][1][endIndex + 1];
    }

    var node = end.nextSibling;
    while (node) {
      var endIndex = 0;
      var startData = node[tag] && node[tag][0];
      if (startData && startData.length) {
        // skip over sibling of this range
        var r = startData[0];
        node = r._end;
        endIndex = r._endIndex + 1;
      }
      if (node[tag] && endIndex < node[tag][1].length)
        return node[tag][1][endIndex];
      node = node.nextSibling;
    }

    return null;
  };

})();
// Meteor Universal Events -- Normalized cross-browser event handling library
//
// This module lets you set up a function f that will be called
// whenever an event fires on any element in the DOM. Specifically,
// when an event fires on node N, f will be called with N. Then, if
// the event is a bubbling event, f will be called again with N's
// parent, then called again with N's grandparent, etc, until the root
// of the document is reached. This provides a good base on top of
// which custom event handling systems can be implemented.
//
// f also receives the event object for the event that fired. The
// event object is normalized and extended to smooth over
// cross-browser differences in event handling. See the details in
// setHandler.
//
// Usage:
//   var listener = new UniversalEventListener(function (event) { ... });
//   listener.addType("click");
//
// If you want to support IE <= 8, you must also call installHandler
// on each subtree of DOM nodes on which you wish to receive events,
// eg, before inserting them into the document.
//
// Universal Events works reliably for events that fire on any DOM
// element. It may not work consistently across browsers for events
// that are intended to fire on non-element nodes (eg, text nodes).
// We're not sure if it's possible to handle those events consistently
// across browsers, but in any event, it's not a common use case.
//
// Implementation notes:
//
// Internally, there are two separate implementations, one for modern
// browsers (in events-w3c.js), and one for old browsers with no
// event capturing support (in events-ie.js.) The correct
// implementation will be chosen for you automatically at runtime.

(function () {

  var listeners = [];

  var returnFalse = function() { return false; };
  var returnTrue = function() { return true; };

  // inspired by jquery fix()
  var normalizeEvent = function (event) {
    var originalStopPropagation = event.stopPropagation;
    var originalPreventDefault = event.preventDefault;
    event.isPropagationStopped = returnFalse;
    event.isImmediatePropagationStopped = returnFalse;
    event.isDefaultPrevented = returnFalse;
    event.stopPropagation = function() {
      event.isPropagationStopped = returnTrue;
      if (originalStopPropagation)
        originalStopPropagation.call(event);
      else
        event.cancelBubble = true; // IE
    };
    event.preventDefault = function() {
      event.isDefaultPrevented = returnTrue;
      if (originalPreventDefault)
        originalPreventDefault.call(event);
      else
        event.returnValue = false; // IE
    };
    event.stopImmediatePropagation = function() {
      event.stopPropagation();
      event.isImmediatePropagationStopped = returnTrue;
    };

    var type = event.type;

    // adapted from jquery
    if (event.metaKey === undefined)
      event.metaKey = event.ctrlKey;
    if (/^key/.test(type)) {
      // KEY EVENTS
      // Add which.  Technically char codes and key codes are
      // different things; the former is ASCII/unicode/etc and the
      // latter is arbitrary.  But browsers that lack charCode
      // seem to put character info in keyCode.
      // (foo == null) tests for null or undefined
      if (event.which == null)
	event.which = (event.charCode != null ? event.charCode : event.keyCode);
    } else if (/^(?:mouse|contextmenu)|click/.test(type)) {
      // MOUSE EVENTS
      // Add relatedTarget, if necessary
      if (! event.relatedTarget && event.fromElement)
	event.relatedTarget = (event.fromElement === event.target ?
                               event.toElement : event.fromElement);
      // Add which for click: 1 === left; 2 === middle; 3 === right
      if (! event.which && event.button !== undefined ) {
        var button = event.button;
	event.which = (button & 1 ? 1 :
                       (button & 2 ? 3 :
                         (button & 4 ? 2 : 0 )));
      }
    }

    return event;
  };

  var deliver = function (event) {
    event = normalizeEvent(event);
    _.each(listeners, function (listener) {
      if (listener.types[event.type]) {
        // if in debug mode, filter out events where the user forgot
        // to call installHandler, even if we're not on IE
        if (!(listener._checkIECompliance &&
              ! event.currentTarget['_uevents_test_eventtype_' + event.type]))
          listener.handler.call(null, event);
      }
    });
  };

  // When IE8 is dead, we can remove this springboard logic.
  var impl;
  var getImpl = function () {
    if (! impl)
      impl = (document.addEventListener ?
              new UniversalEventListener._impl.w3c(deliver) :
              new UniversalEventListener._impl.ie(deliver));
    return impl;
  };

  var typeCounts = {};


  ////////// PUBLIC API

  // Create a new universal event listener with a given handler.
  // Until some event types are turned on with `addType`, the handler
  // will not receive any events.
  //
  // Whenever an event of the appropriate type fires anywhere in the
  // document, `handler` will be called with one argument, the
  // event. If the event is a bubbling event (most events are
  // bubbling, eg, 'click'), then `handler` will be called not only
  // for the element that was the origin of the event (eg, the button
  // that was clicked), but for each parent element as the event
  // bubbles up to the top of the tree.
  //
  // The event object that's passed to `handler` will be normalized
  // across browsers so that it contains the following fields and
  // methods:
  //
  // - type (e.g. "click")
  // - target
  // - currentTarget
  // - stopPropagation()
  // - preventDefault()
  // - isPropagationStopped()
  // - isDefaultPrevented()
  //
  // NOTE: If you want compatibility with IE <= 8, you will need to
  // call `installHandler` to prepare each subtree of the DOM to receive
  // the events you are interested in.
  //
  // Debugging only:
  //
  // The _checkIECompliance flag enables extra checking that the user
  // is correctly registering new DOM nodes with installHandler, even
  // in browsers that don't require it. In other words, when the flag
  // is set, modern browsers will require the same API calls as IE <=
  // 8. This is only used for tests and is private for now.
  UniversalEventListener = function (handler, _checkIECompliance) {
    this.handler = handler;
    this.types = {}; // map from event type name to 'true'
    this.impl = getImpl();
    this._checkIECompliance = _checkIECompliance;
    listeners.push(this);
  };

  _.extend(UniversalEventListener.prototype, {
    // Adds `type` to the set of event types that this listener will
    // listen to and deliver to the handler.  Has no effect if `type`
    // is already in the set.
    addType: function (type) {
      if (!this.types[type]) {
        this.types[type] = true;
        typeCounts[type] = (typeCounts[type] || 0) + 1;
        if (typeCounts[type] === 1)
          this.impl.addType(type);
      }
    },

    // Removes `type` from the set of event types that this listener
    // will listen to and deliver to the handler.  Has no effect if `type`
    // is not in the set.
    removeType: function (type) {
      if (this.types[type]) {
        delete this.types[type];
        typeCounts[type]--;
        if (! typeCounts[type])
          this.impl.removeType(type);
      }
    },

    // It is only necessary to call this method if you want to support
    // IE <= 8. On those browsers, you must call this method on each
    // set of nodes before adding them to the DOM (or at least, before
    // expecting to receive events on them), and you must specify the
    // types of events you'll be receiving.
    //
    // `node` and all of its descendents will be set up to handle
    // events of type `type` (eg, 'click'). Only current descendents
    // of `node` are affected; if new nodes are added to the subtree
    // later, installHandler must be called again to ensure events are
    // received on those nodes. To set up to handle multiple event
    // types, make multiple calls.
    //
    // It is safe to call installHandler any number of times on the same
    // arguments (it is idempotent).
    //
    // If you forget to call this function for a given node, it's
    // unspecified whether you'll receive events on IE <= 8 (you may,
    // you may not.) If you don't care about supporting IE <= 8 you
    // can ignore this function.
    installHandler: function (node, type) {
      // Only work on element nodes, not e.g. text nodes or fragments
      if (node.nodeType !== 1)
        return;
      this.impl.installHandler(node, type);

      // When in checkIECompliance mode, mark all the nodes in the current subtree.
      // We will later block events on nodes that weren't marked.  This
      // tests that Spark is generating calls to registerEventType
      // with proper subtree information, even in browsers that don't need
      // it.
      if (this._checkIECompliance) {
        // set flag to mark the node for this type, recording the
        // fact that installHandler was called for this node and type.
        // the property value can be any non-primitive value (to prevent
        // showing up as an HTML attribute in IE) so we use `node` itself.
        node['_uevents_test_eventtype_'+type] = node;
        if (node.firstChild) {
          _.each(node.getElementsByTagName('*'), function(x) {
            x['_uevents_test_eventtype_'+type] = x;
          });
        }
      }
    },

    // Tear down this UniversalEventListener so that no more events
    // are delivered.
    destroy: function () {
      var self = this;

      listeners = _.without(listeners, self);
      _.each(_.keys(self.types), function (type) {
        self.removeType(type);
      });
    }
  });
})();
// Universal Events implementation for IE versions 6-8, which lack
// addEventListener and event capturing.
//
// The strategy is very different.  We walk the subtree in question
// and just attach the handler to all elements.  If the handler is
// foo and the eventType is 'click', we assign node.onclick = foo
// everywhere.  Since there is only one function object and we are
// just assigning a property, hopefully this is somewhat lightweight.
//
// We use the node.onfoo method of binding events, also called "DOM0"
// or the "traditional event registration", rather than the IE-native
// node.attachEvent(...), mainly because we have the benefit of
// referring to `this` from the handler in order to populate
// event.currentTarget.  It seems that otherwise we'd have to create
// a closure per node to remember what node we are handling.
//
// We polyfill the usual event properties from their various locations.
// We also make 'change' and 'submit' bubble, and we fire 'change'
// events on checkboxes and radio buttons immediately rather than
// only when the user blurs them, another old IE quirk.

UniversalEventListener._impl = UniversalEventListener._impl ||  {};

// Singleton
UniversalEventListener._impl.ie = function (deliver) {
  var self = this;
  this.deliver = deliver;
  this.curriedHandler = function () {
    self.handler.call(this, self);
  };

  // The 'submit' event on IE doesn't bubble.  We want to simulate
  // bubbling submit to match other browsers, and to do that we use
  // IE's own event machinery.  We can't dispatch events with arbitrary
  // names in IE, so we appropriate the obscure "datasetcomplete" event
  // for this purpose.
  document.attachEvent('ondatasetcomplete', function () {
    var evt = window.event;
    var target = evt && evt.srcElement;
    if (evt.synthetic && target &&
        target.nodeName === 'FORM' &&
        evt.returnValue !== false)
      // No event handler called preventDefault on the simulated
      // submit event.  That means the form should be submitted.
      target.submit();
  });
};

_.extend(UniversalEventListener._impl.ie.prototype, {
  addType: function (type) {
    // not necessary for IE
  },

  removeType: function (type) {
    // not necessary for IE
  },

  installHandler: function (node, type) {
    // use old-school event binding, so that we can
    // access the currentTarget as `this` in the handler.
    // note: handler is never removed from node
    var prop = 'on' + type;

    if (node.nodeType === 1) { // ELEMENT
      this._install(node, prop);

      // hopefully fast traversal, since the browser is doing it
      var descendents = node.getElementsByTagName('*');

      for(var i=0, N = descendents.length; i<N; i++)
        this._install(descendents[i], prop);
    }
  },

  _install: function (node, prop) {
    var props = [prop];

    // install handlers for faking focus/blur if necessary
    if (prop === 'onfocus')
      props.push('onfocusin');
    else if (prop === 'onblur')
      props.push('onfocusout');
    // install handlers for faking bubbling change/submit
    else if (prop === 'onchange') {
      // if we're looking at a checkbox or radio button,
      // sign up for propertychange and NOT change
      if (node.nodeName === 'INPUT' &&
          (node.type === 'checkbox' || node.type === 'radio'))
        props = ['onpropertychange'];
      props.push('oncellchange');
    } else if (prop === 'onsubmit')
      props.push(node, 'ondatasetcomplete');

    for(var i = 0; i < props.length; i++)
      node[props[i]] = this.curriedHandler;
  },

  // This is the handler we assign to DOM nodes, so it shouldn't close over
  // anything that would create a circular reference leading to a memory leak.
  //
  // This handler is called via this.curriedHandler. When it is called:
  //  - 'this' is the node currently handling the event (set by IE)
  //  - 'self' is what would normally be 'this'
  handler: function (self) {
    var sendEvent = function (ontype, target) {
      var e = document.createEventObject();
      e.synthetic = true;
      target.fireEvent(ontype, e);
      return e.returnValue;
    };


    var event = window.event;
    var type = event.type;
    var target = event.srcElement || document;
    event.target = target;
    if (this.nodeType !== 1)
      return; // sanity check that we have a real target (always an element)
    event.currentTarget = this;
    var curNode = this;

    // simulate focus/blur so that they are synchronous;
    // simulate change/submit so that they bubble.
    // The IE-specific 'cellchange' and 'datasetcomplete' events actually
    // have nothing to do with change and submit, we are just using them
    // as dummy events because we need event types that IE considers real
    // (and apps are unlikely to use them).
    if (curNode === target && ! event.synthetic) {
      if (type === 'focusin')
        sendEvent('onfocus', curNode);
      else if (type === 'focusout')
        sendEvent('onblur', curNode);
      else if (type === 'change')
        sendEvent('oncellchange', curNode);
      else if (type === 'propertychange') {
        if (event.propertyName === 'checked')
          sendEvent('oncellchange', curNode);
      } else if (type === 'submit') {
        sendEvent('ondatasetcomplete', curNode);
      }
    }
    // ignore non-simulated events of types we simulate
    if ((type === 'focus' || event.type === 'blur'
         || event.type === 'change' ||
         event.type === 'submit') && ! event.synthetic) {
      if (event.type === 'submit')
        event.returnValue = false; // block all native submits, we will submit
      return;
    }

    // morph the event
    if (type === 'cellchange' && event.synthetic) {
      type = event.type = 'change';
    }
    if (type === 'datasetcomplete' && event.synthetic) {
      type = event.type = 'submit';
    }

    self.deliver(event);
  }

});
// Universal Events implementation that depends on the W3C event
// model, i.e. addEventListener and capturing.  It's intended for all
// browsers except IE <= 8.
//
// We take advantage of the fact that event handlers installed during
// the capture phase are live during the bubbling phase.  By installing
// a capturing listener on the document, we bind the handler to the
// event target and its ancestors "just in time".

(function () {
  var SIMULATE_NEITHER = 0;
  var SIMULATE_FOCUS_BLUR = 1;
  var SIMULATE_FOCUSIN_FOCUSOUT = 2;

  UniversalEventListener._impl = UniversalEventListener._impl ||  {};

  // Singleton
  UniversalEventListener._impl.w3c = function (deliver) {
    this.deliver = deliver;
    this.typeCounts = {}; // map from event type name to count

    this.boundHandler = _.bind(this.handler, this);
    this.boundCapturer = _.bind(this.capturer, this);

    // Focusin/focusout are the bubbling versions of focus/blur, and
    // are part of the W3C spec, but are absent from Firefox as of
    // today (v11), so we supply them.
    //
    // In addition, while most browsers fire these events sync in
    // response to a programmatic action (like .focus()), not all do.
    // IE 9+ fires focusin/focusout sync but focus/blur async.  Opera
    // fires them all async.  We don't do anything about this right
    // now, but simulating focus/blur on IE would make them sync.
    //
    // We have the capabiilty here to simulate focusin/focusout from
    // focus/blur, vice versa, or neither.
    //
    // We do a browser check that fails in old Firefox (3.6) but will
    // succeed if Firefox ever implements focusin/focusout.  Old
    // Firefox fails all tests of the form ('onfoo' in node), while
    // new Firefox and all other known browsers will pass if 'foo' is
    // a known event.
    this.focusBlurMode = ('onfocusin' in document.createElement("DIV")) ?
      SIMULATE_NEITHER : SIMULATE_FOCUSIN_FOCUSOUT;

    // mouseenter/mouseleave is non-bubbling mouseover/mouseout.  It's
    // standard but only IE and Opera seem to support it,
    // so we simulate it (which works in IE but not in Opera for some reason).
    this.simulateMouseEnterLeave = (! window.opera);
  };

  _.extend(UniversalEventListener._impl.w3c.prototype, {
    addType: function (eventType) {
      this._listen(this._expandEventType(eventType));
    },

    removeType: function (type) {
      this._unlisten(this._expandEventType(type));
    },

    installHandler: function (node, type) {
      // Unnecessary in w3c implementation
    },

    _expandEventType: function (type) {
      var ret = [type];

      // install handlers for the events used to fake events of this
      // type, in addition to handlers for the real type

      if (this.focusBlurMode === SIMULATE_FOCUS_BLUR) {
        if (type === 'focus')
          ret.push('focusin');
        else if (type === 'blur')
          ret.push('focusout');
      } else if (this.focusBlurMode === SIMULATE_FOCUSIN_FOCUSOUT) {
        if (type === 'focusin')
          ret.push('focus');
        else if (type === 'focusout')
          ret.push('blur');
      }
      if (this.simulateMouseEnterLeave) {
        if (type === 'mouseenter')
          ret.push('mouseover');
        else if (type === 'mouseleave')
          ret.push('mouseout');
      }

      return ret;
    },

    _listen: function (types) {
      var self = this;
      _.each(types, function (type) {
        if ((self.typeCounts[type] = (self.typeCounts[type] || 0) + 1) === 1)
          document.addEventListener(type, self.boundCapturer, true);
      });
    },

    _unlisten: function (types) {
      var self = this;
      _.each(types, function (type) {
        if (!(--self.typeCounts[type])) {
          document.removeEventListener(type, self.boundCapturer, true);
        }
      });
    },

    capturer: function (event) {
      if (event.target.nodeType === 3) // fix text-node target
        event.target = event.target.parentNode;

      var type = event.type;
      var bubbles = event.bubbles;
      var target = event.target;

      target.addEventListener(type, this.boundHandler, false);

      // According to the DOM event spec, if the DOM is mutated during
      // event handling, the original bubbling order still applies.
      // So we can determine the chain of nodes that could possibly
      // be bubbled to right now.
      var ancestors;
      if (bubbles) {
        ancestors = [];
        for(var n = target.parentNode; n; n = n.parentNode) {
          n.addEventListener(type, this.boundHandler, false);
          ancestors.push(n);
        };
      }

      // Unbind the handlers later.
      setTimeout(function() {
        target.removeEventListener(type, this.boundHandler, false);
        if (bubbles) {
          _.each(ancestors, function(n) {
            n.removeEventListener(type, this.boundHandler, false);
          });
        };
      }, 0);
    },

    handler: function (event) {
      var sendUIEvent = function (type, target, bubbles, cancelable, detail) {
        var evt = document.createEvent("UIEvents");
        evt.initUIEvent(type, bubbles, cancelable, window, detail);
        evt.synthetic = true;
        target.dispatchEvent(evt);
      };

      // fire synthetic focusin/focusout on blur/focus or vice versa
      if (event.currentTarget === event.target) {
        if (this.focusBlurMode === SIMULATE_FOCUS_BLUR) {
          if (event.type === 'focusin')
            sendUIEvent('focus', event.target, false);
          else if (event.type === 'focusout')
            sendUIEvent('blur', event.target, false);
        } else if (this.focusBlurMode === SIMULATE_FOCUSIN_FOCUSOUT) {
          if (event.type === 'focus')
            sendUIEvent('focusin', event.target, true);
          else if (event.type === 'blur')
            sendUIEvent('focusout', event.target, true);
        }
      }
      // only respond to synthetic events of the types we are faking
      if (this.focusBlurMode === SIMULATE_FOCUS_BLUR) {
        if (event.type === 'focus' || event.type === 'blur') {
          if (! event.synthetic)
            return;
        }
      } else if (this.focusBlurMode === SIMULATE_FOCUSIN_FOCUSOUT) {
        if (event.type === 'focusin' || event.type === 'focusout') {
          if (! event.synthetic)
            return;
        }
      }
      if (this.simulateMouseEnterLeave) {
        if (event.type === 'mouseenter' || event.type === 'mouseleave') {
          if (! event.synthetic)
            return;
        }
      }

      this.deliver(event);

      // event ordering: fire mouseleave after mouseout
      if (this.simulateMouseEnterLeave &&
          // We respond to mouseover/mouseout here even on
          // bubble, i.e. when event.currentTarget !== event.target,
          // to ensure we see every enter and leave.
          // We ignore the case where the mouse enters from
          // a child or leaves to a child (by checking if
          // relatedTarget is present and a descendent).
          (! event.relatedTarget ||
           (event.currentTarget !== event.relatedTarget &&
            ! DomUtils.elementContains(
              event.currentTarget, event.relatedTarget)))) {
        if (event.type === 'mouseover'){
          sendUIEvent('mouseenter', event.currentTarget, false);
        }
        else if (event.type === 'mouseout') {
          sendUIEvent('mouseleave', event.currentTarget, false);
        }
      }
    }
  });

})();


DomUtils = {};

(function () {

  var qsaFindAllBySelector = function (selector, contextNode) {
    // If IE7 users report the following error message, you
    // can fix it with "meteor add jquery".
    if (! document.querySelectorAll)
      throw new Error("This browser doesn't support querySelectorAll.");

    // the search is constrained to descendants of `ancestor`,
    // but it doesn't affect the scope of the query.
    var ancestor = contextNode;

    return withElementId(
      contextNode, "DomUtils_findAllBySelector_scope",
      function (idSelector) {
        // scope the entire selector to contextNode by prepending
        // id of contextNode to the selector.
        var doctoredSelector = _.map(selector.split(','), function (selec) {
          return idSelector + " " + selec;
        }).join(',');
        return ancestor.querySelectorAll(doctoredSelector);
      });
  };

  // We have our own, querySelectorAll-based implementation of scoped
  // selector matching; it's all you need in IE 8+ and modern browsers.
  //
  // However, we use Sizzle or jQuery if it's present on the client because of:
  // - apps that want jQuery's selector extensions (:visible, :input, etc.)
  // - apps that include jQuery anyway
  // - apps that want IE 7 support
  //
  // XXX others? zepto?
  var findAllBySelector = (window.Sizzle
                           || (window.jQuery && window.jQuery.find)
                           || qsaFindAllBySelector);

  ///// Common look-up tables used by htmlToFragment et al.

  var testDiv = document.createElement("div");
  testDiv.innerHTML = "   <link/><table></table><select><!----></select>";

  // Tests that, if true, indicate browser quirks present.
  var quirks = {
    // IE loses initial whitespace when setting innerHTML.
    leadingWhitespaceKilled: (testDiv.firstChild.nodeType !== 3),

    // IE may insert an empty tbody tag in a table.
    tbodyInsertion: testDiv.getElementsByTagName("tbody").length > 0,

    // IE loses some tags in some environments (requiring extra wrapper).
    tagsLost: testDiv.getElementsByTagName("link").length === 0,

    // IE <= 8 loses HTML comments in <select> and <option> tags.
    // Assert that we have IE's mergeAttributes to use in our work-around.
    commentsLost: ((! testDiv.getElementsByTagName("select")[0].firstChild)
                   && testDiv.mergeAttributes)
  };

  // Set up map of wrappers for different nodes.
  var wrapMap = {
    option: [ 1, "<select multiple='multiple'>", "</select>" ],
    legend: [ 1, "<fieldset>", "</fieldset>" ],
    thead: [ 1, "<table>", "</table>" ],
    tr: [ 2, "<table><tbody>", "</tbody></table>" ],
    td: [ 3, "<table><tbody><tr>", "</tr></tbody></table>" ],
    col: [ 2, "<table><tbody></tbody><colgroup>", "</colgroup></table>" ],
    area: [ 1, "<map>", "</map>" ],
    _default: [ 0, "", "" ]
  };
  _.extend(wrapMap, {
    optgroup: wrapMap.option,
    tbody: wrapMap.thead,
    tfoot: wrapMap.thead,
    colgroup: wrapMap.thead,
    caption: wrapMap.thead,
    th: wrapMap.td
  });
  if (quirks.tagsLost) {
    // trick from jquery.  initial text is ignored when we take lastChild.
    wrapMap._default = [ 1, "div<div>", "</div>" ];
  }

  var rleadingWhitespace = /^\s+/,
      rxhtmlTag = /<(?!area|br|col|embed|hr|img|input|link|meta|param)(([\w:]+)[^>]*)\/>/ig,
      rtagName = /<([\w:]+)/,
      rtbody = /<tbody/i,
      rhtml = /<|&#?\w+;/,
      rnoInnerhtml = /<(?:script|style)/i;


  // Parse an HTML string, which may contain multiple top-level tags,
  // and return a DocumentFragment.
  DomUtils.htmlToFragment = function (html) {
    var doc = document; // node factory
    var frag = doc.createDocumentFragment();

    if (! html.length) {
      // empty, do nothing
    } else if (! rhtml.test(html)) {
      // Just text.
      frag.appendChild(doc.createTextNode(html));
    } else {
      // General case.
      // Replace self-closing tags
      html = html.replace(rxhtmlTag, "<$1></$2>");
      // Use first tag to determine wrapping needed.
      var firstTagMatch = rtagName.exec(html);
      var firstTag = (firstTagMatch ? firstTagMatch[1].toLowerCase() : "");
      var wrapData = wrapMap[firstTag] || wrapMap._default;
      var fullHtml = wrapData[1] + html + wrapData[2];
      if (quirks.commentsLost) {
        // rewrite <select> and <option> tags into fake tags
        fullHtml = fullHtml.replace(/<\s*(select|option)\b/ig,
                                    '<ins domutilsrealtagname="$1"');
        fullHtml = fullHtml.replace(/<\/\s*(select|option)\b/ig,
                                    '</ins');
      }

      var container = doc.createElement("div");
      // insert wrapped HTML into a DIV
      container.innerHTML = fullHtml;
      // set "container" to inner node of wrapper
      var unwraps = wrapData[0];
      while (unwraps--) {
        container = container.lastChild;
      }

      if (quirks.tbodyInsertion && ! rtbody.test(html)) {
        // Any tbody we find was created by the browser.
        var tbodies = container.getElementsByTagName("tbody");
        _.each(tbodies, function (n) {
          if (! n.firstChild) {
            // spurious empty tbody
            n.parentNode.removeChild(n);
          }
        });
      }

      if (quirks.leadingWhitespaceKilled) {
        var wsMatch = rleadingWhitespace.exec(html);
        if (wsMatch) {
          container.insertBefore(doc.createTextNode(wsMatch[0]),
                                 container.firstChild);
        }
      }

      if (quirks.commentsLost) {
        // replace fake select tags with real <select> tags
        var fakeTags = [];
        // getElementsByTagName returns a "live" collection, so avoid
        // factorings of this code that iterate over it while mutating
        // the DOM.
        // Here we build an array of fake tags and iterate over that.
        _.each(container.getElementsByTagName("ins"), function (ins) {
          if (ins.getAttribute("domutilsrealtagname"))
            fakeTags.push(ins);
        });
        _.each(fakeTags, function (fakeTag) {
          var realTag = document.createElement(
            fakeTag.getAttribute('domutilsrealtagname'));
          fakeTag.removeAttribute('domutilsrealtagname');
          // copy all attributes
          realTag.mergeAttributes(fakeTag, false);
          // move all children
          while (fakeTag.firstChild)
            realTag.appendChild(fakeTag.firstChild);
          // replace
          fakeTag.parentNode.replaceChild(realTag, fakeTag);
        });
      }

      // Reparent children of container to frag.
      while (container.firstChild)
        frag.appendChild(container.firstChild);
    }

    return frag;
  };

  // Return an HTML string representing the contents of frag,
  // a DocumentFragment.  (This is what innerHTML would do if
  // it were defined on DocumentFragments.)
  DomUtils.fragmentToHtml = function (frag) {
    frag = frag.cloneNode(true); // deep copy, don't touch original!

    return DomUtils.fragmentToContainer(frag).innerHTML;
  };

  // Given a DocumentFragment, return a node whose children are the
  // reparented contents of the DocumentFragment.  In most cases this
  // is as simple as creating a DIV, but in the case of a fragment
  // containing TRs, for example, it's necessary to create a TABLE and
  // a TBODY and return the TBODY.
  DomUtils.fragmentToContainer = function (frag) {
    var doc = document; // node factory

    var firstElement = frag.firstChild;
    while (firstElement && firstElement.nodeType !== 1) {
      firstElement = firstElement.nextSibling;
    }

    var container = doc.createElement("div");

    if (! firstElement) {
      // no tags!
      container.appendChild(frag);
    } else {
      var firstTag = firstElement.nodeName;
      var wrapData = wrapMap[firstTag] || wrapMap._default;

      container.innerHTML = wrapData[1] + wrapData[2];
      var unwraps = wrapData[0];
      while (unwraps--) {
        container = container.lastChild;
      }

      container.appendChild(frag);
    }

    return container;
  };

  // Returns true if element a contains node b and is not node b.
  DomUtils.elementContains = function (a, b) {
    if (a.nodeType !== 1) /* ELEMENT */
      return false;
    if (a === b)
      return false;

    if (a.compareDocumentPosition) {
      return a.compareDocumentPosition(b) & 0x10;
    } else {
      // Should be only old IE and maybe other old browsers here.
      // Modern Safari has both functions but seems to get contains() wrong.
      // IE can't handle b being a text node.  We work around this
      // by doing a direct parent test now.
      b = b.parentNode;
      if (! (b && b.nodeType === 1)) /* ELEMENT */
        return false;
      if (a === b)
        return true;

      return a.contains(b);
    }
  };

  // Returns an array containing the children of contextNode that
  // match `selector`. Unlike querySelectorAll, `selector` is
  // interpreted as if the document were rooted at `contextNode` --
  // the only nodes that can be used to match components of the
  // selector are the descendents of `contextNode`. `contextNode`
  // itself is not included (it can't be used to match a component of
  // the selector, and it can never be included in the returned
  // array.)
  //
  // `contextNode` may be either a node, a document, or a DocumentFragment.
  DomUtils.findAll = function (contextNode, selector) {
    if (contextNode.nodeType === 11 /* DocumentFragment */) {
      // contextNode is a DocumentFragment.
      //
      // We don't expect to be able to run selectors on a DocumentFragment
      // (Sizzle won't work) but we can on a normal elements that aren't
      // in the document.  Fortunately we can manipulate offscreen nodes
      // as much as we want as long as we put them back the way they were
      // when we're done.
      var frag = contextNode;
      var container = DomUtils.fragmentToContainer(frag);
      var results = findAllBySelector(selector, container);
      // put nodes back into frag
      while (container.firstChild)
        frag.appendChild(container.firstChild);
      return results;
    }

    return findAllBySelector(selector, contextNode);
  };

  // Like `findAll` but finds one element (or returns null).
  DomUtils.find = function (contextNode, selector) {
    var results = DomUtils.findAll(contextNode, selector);
    return (results.length ? results[0] : null);
  };

  var isElementInClipRange = function (elem, clipStart, clipEnd) {
    // elem is not in clip range if it contains the clip range
    if (DomUtils.elementContains(elem, clipStart))
      return false;
    // elem is in clip range if clipStart <= elem <= clipEnd
    return (DomUtils.compareElementIndex(clipStart, elem) <= 0) &&
      (DomUtils.compareElementIndex(elem, clipEnd) <= 0);
  };

  // Like `findAll` but searches the nodes from `start` to `end`
  // inclusive. `start` and `end` must be siblings, and they participate
  // in the search (they can be used to match selector components, and
  // they can appear in the returned results). It's as if the parent of
  // `start` and `end` serves as contextNode, but matches from children
  // that aren't between `start` and `end` (inclusive) are ignored.
  //
  // If `selector` involves sibling selectors, child index selectors, or
  // the like, the results are undefined.
  //
  // precond: clipStart/clipEnd are descendents of contextNode
  // XXX document
  DomUtils.findAllClipped = function (contextNode, selector, clipStart, clipEnd) {

    // Ensure the clip range starts and ends on element nodes.  This is possible
    // to do without changing the result set because non-element nodes can't
    // be or contain matches.
    while (clipStart !== clipEnd && clipStart.nodeType !== 1)
      clipStart = clipStart.nextSibling;
    while (clipStart !== clipEnd && clipEnd.nodeType !== 1)
      clipEnd = clipEnd.previousSibling;
    if (clipStart.nodeType !== 1)
      return []; // no top-level elements!  start === end and it's not an element

    // resultsPlus includes matches all matches descended from contextNode,
    // including those that aren't in the clip range.
    var resultsPlus = DomUtils.findAll(contextNode, selector);

    // Filter the list of nodes to remove nodes that occur before start
    // or after end.
    return _.reject(resultsPlus, function (n) {
      return ! isElementInClipRange(n, clipStart, clipEnd);
    });
  };

  // Like `findAllClipped` but finds one element (or returns null).
  DomUtils.findClipped = function (contextNode, selector, clipStart, clipEnd) {
    var results = DomUtils.findAllClipped(
      contextNode, selector, clipStart, clipEnd);
    return (results.length ? results[0] : null);
  };

  // Executes `func` while ensuring that `element` has an ID.  If `element`
  // doesn't have an ID, it is assigned `magicId` temporarily.
  // Calls func with a selector of the form "[id='...']" as an argument.
  var withElementId = function (element, magicId, func) {
    var didSetId = false;
    if (! element.id) {
      element.setAttribute('id', magicId);
      didSetId = true;
    }
    try {
      var escapedNodeId = element.id.replace(/'/g, "\\$&");
      return func("[id='" + escapedNodeId + "']");
    } finally {
      if (didSetId)
        element.removeAttribute('id');
    }
  };

  var matchesSelectorMaybeClipped = function (element, contextNode, selector,
                                             clipStart, clipEnd) {
    var selecs = selector.split(',');
    for(var i = 0, N = selecs.length; i < N; i++) {
      var matches = withElementId(
        element, "DomUtils_matchesSelector_target",
        function (idSelector) {
          var trimmedSelector = selector.match(/\S.*?(?=\s*$)/)[0];
          // appending [id='foo'] to a selector with no whitespace ought to
          // simply restrict the set of possible outputs regardless of the
          // form of the selector.
          var doctoredSelector = trimmedSelector + idSelector;
          var result;
          if (clipStart)
            result = DomUtils.findClipped(contextNode, doctoredSelector,
                                          clipStart, clipEnd);
          else
            result = DomUtils.find(contextNode, doctoredSelector);
          return (result === element);
        });

      if (matches)
        return true;
    }

    return false;
  };

  // Check if `element` matches `selector`, scoped to `contextNode`.
  DomUtils.matchesSelector = function (element, contextNode, selector) {
    return matchesSelectorMaybeClipped(element, contextNode, selector);
  };

  // Check if `element` matches `selector`, scoped to `contextNode`,
  // clipped to ordered siblings `clipStart`..`clipEnd`.
  DomUtils.matchesSelectorClipped = function (element, contextNode, selector,
                                              clipStart, clipEnd) {
    return matchesSelectorMaybeClipped(element, contextNode, selector,
                                       clipStart, clipEnd);
  };

  // Returns 0 if the nodes are the same or either one contains the other;
  // otherwise, -1 if a comes before b, or else 1 if b comes before a in
  // document order.
  // Requires: `a` and `b` are element nodes in the same document tree.
  DomUtils.compareElementIndex = function (a, b) {
    // See http://ejohn.org/blog/comparing-document-position/
    if (a === b)
      return 0;
    if (a.compareDocumentPosition) {
      var n = a.compareDocumentPosition(b);
      return ((n & 0x18) ? 0 : ((n & 0x4) ? -1 : 1));
    } else {
      // Only old IE is known to not have compareDocumentPosition (though Safari
      // originally lacked it).  Thankfully, IE gives us a way of comparing elements
      // via the "sourceIndex" property.
      if (a.contains(b) || b.contains(a))
        return 0;
      return (a.sourceIndex < b.sourceIndex ? -1 : 1);
    }
  };

  // Wrap `frag` as necessary to prepare it for insertion in
  // `container`. For example, if `frag` has TR nodes at top level,
  // and `container` is a TABLE, then it's necessary to wrap `frag` in
  // a TBODY to avoid IE quirks.
  //
  // `frag` is a DocumentFragment and will be modified in
  // place. `container` is a DOM element.
  DomUtils.wrapFragmentForContainer = function (frag, container) {
    if (container && container.nodeName === "TABLE" &&
        _.any(frag.childNodes,
              function (n) { return n.nodeName === "TR"; })) {
      // Avoid putting a TR directly in a TABLE without an
      // intervening TBODY, because it doesn't work in IE.  We do
      // the same thing on all browsers for ease of testing
      // and debugging.
      var tbody = document.createElement("TBODY");
      tbody.appendChild(frag);
      frag.appendChild(tbody);
    }
  };

  // Return true if `node` is part of the global DOM document. Like
  // elementContains(document, node), except (1) it works for any node
  // (eg, text nodes), not just elements; (2) it works around browser
  // quirks that would otherwise come up when passing 'document' as
  // the first argument to elementContains.
  //
  // Returns true if node === document.
  DomUtils.isInDocument = function (node) {
    // Deal with all cases where node is not an element
    // node descending from the body first...
    if (node === document)
      return true;

    if (node.nodeType !== 1 /* Element */)
      node = node.parentNode;
    if (! (node && node.nodeType === 1))
      return false;
    if (node === document.body)
      return true;

    return DomUtils.elementContains(document.body, node);
  };

  // Return an HTML string representation of the nodes from
  // firstNode to lastNode, which must be siblings.
  // The tags representing firstNode and lastNode are included,
  // but not their parent or outer siblings.
  DomUtils.rangeToHtml = function (firstNode, lastNode) {
    var frag = document.createDocumentFragment();
    for(var n = firstNode, after = lastNode.nextSibling;
        n && n !== after;
        n = n.nextSibling)
      frag.appendChild(n.cloneNode(true)); // deep copy
    return DomUtils.fragmentToHtml(frag);
  };

  // Return an HTML string representation of node, including its
  // own open and close tag.
  DomUtils.outerHtml = function (node) {
    return DomUtils.rangeToHtml(node, node);
  };


})();
// XXX adjust Spark API so that the modules (eg, list, events) could
// have been written by third parties on top of the public API?

// XXX rename isolate to reflect that it is the only root of
// deps-based reactivity ('track'? 'compute'? 'sync'?)

// XXX specify flush order someday (context dependencies? is this in
// the domain of spark -- overdraw concerns?)

// XXX if not on IE6-8, don't do the extra work (traversals for event
// setup) those browsers require

// XXX flag errors if you have two landmarks with the same branch
// path, or if you have multiple preserve nodes in a landmark with the
// same selector and label

// XXX should functions with an htmlFunc use try/finally inside?

// XXX test that non-Spark.render case works for each function (eg,
// list() returns the expected HTML, Spark.createLandmark creates and
// then destroys a landmark -- may already be tested?)

// XXX in landmark-demo, if Template.timer.created throws an exception,
// then it is never called again, even if you push the 'create a
// timer' button again. the problem is almost certainly in atFlush
// (not hard to see what it is.)

(function() {

Spark = {};

Spark._currentRenderer = (function () {
  var current = null;
  return {
    get: function () {
      return current;
    },
    withValue: function (v, func) {
      var previous = current;
      current = v;
      try { return func(); }
      finally { current = previous; }
    }
  };
})();

Spark._TAG = "_spark_" + Meteor.uuid();
// XXX document contract for each type of annotation?
Spark._ANNOTATION_NOTIFY = "notify";
Spark._ANNOTATION_DATA = "data";
Spark._ANNOTATION_ISOLATE = "isolate";
Spark._ANNOTATION_EVENTS = "events";
Spark._ANNOTATION_WATCH = "watch";
Spark._ANNOTATION_LABEL = "label";
Spark._ANNOTATION_LANDMARK = "landmark";
Spark._ANNOTATION_LIST = "list";
Spark._ANNOTATION_LIST_ITEM = "item";
// XXX why do we need, eg, _ANNOTATION_ISOLATE? it has no semantics?

// Set in tests to turn on extra UniversalEventListener sanity checks
Spark._checkIECompliance = false;

Spark._globalPreserves = {};

var makeRange = function (type, start, end, inner) {
  var range = new LiveRange(Spark._TAG, start, end, inner);
  range.type = type;
  return range;
};

var findRangeOfType = function (type, node) {
  var range = LiveRange.findRange(Spark._TAG, node);
  while (range && range.type !== type)
    range = range.findParent();

  return range;
};

var findParentOfType = function (type, range) {
  do {
    range = range.findParent();
  } while (range && range.type !== type);

  return range;
};

var notifyWatchers = function (start, end) {
  var tempRange = new LiveRange(Spark._TAG, start, end, true /* innermost */);
  for (var walk = tempRange; walk; walk = walk.findParent())
    if (walk.type === Spark._ANNOTATION_WATCH)
      walk.notify();
  tempRange.destroy();
};

var eventGuardActive = false;
// Spark does DOM manipulation inside an event guard to prevent events
// like "blur" from firing.  It would be nice to deliver these events
// in some cases, but running fresh event handling code on an invalid
// LiveRange tree can easily produce errors.
// This guard was motivated by seeing errors in Todos when switching
// windows while an input field is focused.
var withEventGuard = function (func) {
  var previous = eventGuardActive;
  eventGuardActive = true;
  try { return func(); }
  finally { eventGuardActive = previous; }
};

Spark._createId = function () {
  // Chars can't include '-' to be safe inside HTML comments.
  var chars =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+_";
  var id = "";
  for (var i = 0; i < 8; i++)
    id += chars.substr(Math.floor(Meteor.random() * 64), 1);
  return id;
};

Spark._Renderer = function () {
  // Map from annotation ID to an annotation function, which is called
  // at render time and receives (startNode, endNode).
  this.annotations = {};

  // Map from branch path to "notes" object, organized as a tree.
  // Each node in the tree has child pointers named ('_'+label).
  // Properties that don't start with '_' are arbitrary notes.
  // For example, the "happiness" of the branch path consisting
  // of labels "foo" and then "bar" would be
  // `this._branchNotes._foo._bar.happiness`.
  // Access to these notes is provided by LabelStack objects, of
  // which `this.currentBranch` is one.
  this._branchNotes = {};

  // The label stack representing the current branch path we
  // are in (based on calls to `Spark.labelBranch(label, htmlFunc)`).
  this.currentBranch = this.newLabelStack();

  // All landmark ranges created during this rendering.
  this.landmarkRanges = [];

  // Assembles the preservation information for patching.
  this.pc = new PreservationController;
};

_.extend(Spark._Renderer.prototype, {
  // `what` can be a function that takes a LiveRange, or just a set of
  // attributes to add to the liverange.  type and what are optional.
  // if no type is passed, no liverange will be created.
  // If what is a function, it will be called no matter what, even
  // if the annotated HTML was not used and no LiveRange was created,
  // in which case it gets null as an argument.
  annotate: function (html, type, what) {
    if (typeof what !== 'function') {
      var attribs = what;
      what = function (range) {
        if (range)
          _.extend(range, attribs);
      };
    }
    // The annotation tags that we insert into HTML strings must be
    // unguessable in order to not create potential cross-site scripting
    // attack vectors, so we use random strings.  Even a well-written app
    // that avoids XSS vulnerabilities might, for example, put
    // unescaped < and > in HTML attribute values, where they are normally
    // safe.  We can't assume that a string like '<1>' came from us
    // and not arbitrary user-entered data.
    var id = (type || '') + ":" + Spark._createId();
    this.annotations[id] = function (start, end) {
      if ((! start) || (! type)) {
        // ! start: materialize called us with no args because this
        // annotation wasn't used
        // ! type: no type given, don't generate a LiveRange
        what(null);
        return;
      }
      var range = makeRange(type, start, end);
      what(range);
    };

    return "<$" + id + ">" + html + "</$" + id + ">";
  },

  // A LabelStack is a mutable branch path that you can modify
  // by pushing or popping labels.  At any time, you can ask for
  // this Renderer's notes for the current branch path.
  // Renderer's `currentBranch` field is a LabelStack, but you
  // can create your own for the purpose of walking the branches
  // and accessing notes.
  newLabelStack: function () {
    var stack = [this._branchNotes];
    return {
      pushLabel: function (label) {
        var top = stack[stack.length - 1];
        var key = '_' + label;
        stack.push(top[key] = (top[key] || {}));
      },
      popLabel: function () {
        stack.pop();
      },
      getNotes: function () {
        var top = stack[stack.length - 1];
        return top;
      },
      // Mark this branch with `getNotes()[prop] = true` and also
      // walk up the stack marking parent branches (until an
      // existing truthy value for `prop` is found).
      // This makes it easy to test whether any descendent of a
      // branch has the mark.
      mark: function (prop) {
        for (var i = stack.length - 1;
             i >= 0 && ! stack[i][prop];
             i--)
          stack[i][prop] = true;
      }
    };
  },

  // Turn the `html` string into a fragment, applying the annotations
  // from 'renderer' in the process.
  materialize: function (htmlFunc) {
    var self = this;

    var html = Spark._currentRenderer.withValue(self, htmlFunc);
    html = self.annotate(html); // wrap with an anonymous annotation

    var fragById = {};
    var replaceInclusions = function (container) {
      var n = container.firstChild;
      while (n) {
        var next = n.nextSibling;
        if (n.nodeType === 8) { // COMMENT
          var frag = fragById[n.nodeValue];
          if (frag === false) {
            // id already used!
            throw new Error("Spark HTML fragments may only be used once. " +
                            "Second use in " +
                            DomUtils.fragmentToHtml(container));
          } else if (frag) {
            fragById[n.nodeValue] = false; // mark as used
            DomUtils.wrapFragmentForContainer(frag, n.parentNode);
            n.parentNode.replaceChild(frag, n);
          }
        } else if (n.nodeType === 1) { // ELEMENT
          replaceInclusions(n);
        }
        n = next;
      }
    };

    var bufferStack = [[]];
    var idStack = [];
    var ret;

    var regex = /<(\/?)\$([^<>]+)>|<|[^<]+/g;
    regex.lastIndex = 0;
    var parts;
    while ((parts = regex.exec(html))) {
      var isOpen = ! parts[1];
      var id = parts[2];
      var annotationFunc = self.annotations[id];
      if (annotationFunc === false) {
        throw new Error("Spark HTML fragments may be used only once. " +
                        "Second use of: " +
                        DomUtils.fragmentToHtml(fragById[id]));
      } else if (! annotationFunc) {
        bufferStack[bufferStack.length - 1].push(parts[0]);
      } else if (isOpen) {
        idStack.push(id);
        bufferStack.push([]);
      } else {
        var idOnStack = idStack.pop();
        if (idOnStack !== id)
          throw new Error("Range mismatch: " + idOnStack + " / " + id);
        var frag = DomUtils.htmlToFragment(bufferStack.pop().join(''));
        replaceInclusions(frag);
        // empty frag becomes HTML comment <!--empty--> so we have start/end
        // nodes to pass to the annotation function
        if (! frag.firstChild)
          frag.appendChild(document.createComment("empty"));
        annotationFunc(frag.firstChild, frag.lastChild);
        self.annotations[id] = false; // mark as used
        if (! idStack.length) {
          // we're done; we just rendered the contents of the top-level
          // annotation that we wrapped around htmlFunc ourselves.
          // there may be unused fragments in fragById that include
          // LiveRanges, but only if the user broke the rules by including
          // an annotation somewhere besides element level, like inside
          // an attribute (which is not allowed).
          ret = frag;
          break;
        }
        fragById[id] = frag;
        bufferStack[bufferStack.length - 1].push('<!--' + id + '-->');
      }
    }

    scheduleOnscreenSetup(ret, self.landmarkRanges);
    self.landmarkRanges = [];

    _.each(self.annotations, function(annotationFunc) {
      if (annotationFunc)
        // call annotation func with no arguments to mean "you weren't used"
        annotationFunc();
    });
    self.annotations = {};

    return ret;
  }

});

// Decorator for Spark annotations that take `html` and are
// pass-through without a renderer.  With this decorator,
// the annotation routine gets the current renderer, and
// if there isn't one returns `html` (the last argument).
var withRenderer = function (f) {
  return function (/* arguments */) {
    var renderer = Spark._currentRenderer.get();
    var args = _.toArray(arguments);
    if (!renderer)
      return args.pop();
    args.push(renderer);
    return f.apply(null, args);
  };
};

/******************************************************************************/
/* Render and finalize                                                        */
/******************************************************************************/

// Schedule setup tasks to run at the next flush, which is when the
// newly rendered fragment must be on the screen (if it doesn't want
// to get garbage-collected.)
//
// 'landmarkRanges' is a list of the landmark ranges in 'frag'. It may be
// omitted if frag doesn't contain any landmarks.
//
// XXX expose in the public API, eg as Spark.introduce(), so the user
// can call it when manually inserting nodes? (via, eg, jQuery?) -- of
// course in that case 'landmarkRanges' would be empty.
var scheduleOnscreenSetup = function (frag, landmarkRanges) {
  var renderedRange = new LiveRange(Spark._TAG, frag);
  var finalized = false;
  renderedRange.finalize = function () {
    finalized = true;
  };

  Meteor._atFlush(function () {
    if (finalized)
      return;

    if (!DomUtils.isInDocument(renderedRange.firstNode())) {
      // We've detected that some nodes were taken off the screen
      // without calling Spark.finalize(). This could be because the
      // user rendered them, but didn't insert them in the document
      // before the next flush(). Or it could be because they used to
      // be onscreen, but they were manually taken offscreen (eg, with
      // jQuery) and the user neglected to call finalize() on the
      // removed nodes. Help the user out by finalizing the entire
      // subtree that is offscreen.
      var node = renderedRange.firstNode();
      while (node.parentNode)
        node = node.parentNode;
      if (node["_protect"]) {
        // test code can use this property to mark a root-level node
        // (such as a DocumentFragment) as immune from
        // autofinalization. effectively, the DocumentFragment is
        // considered to be a first-class peer of `document`.
      } else {
        Spark.finalize(node);
        return;
      }
    }

    // Deliver render callbacks to all landmarks that are now
    // onscreen (possibly not for the first time.)
    _.each(landmarkRanges, function (landmarkRange) {
      if (! landmarkRange.isPreservedConstant)
        landmarkRange.rendered.call(landmarkRange.landmark);
    });

    // Deliver render callbacks to all landmarks that enclose the
    // updated region.
    //
    // XXX unify with notifyWatchers. maybe remove _ANNOTATION_WATCH
    // and just give everyone a contentsModified callback (sibling to
    // 'finalize')
    //
    // future: include an argument in the callback to distinguish this
    // case from the previous
    var walk = renderedRange;
    while ((walk = findParentOfType(Spark._ANNOTATION_LANDMARK, walk)))
      walk.rendered.call(walk.landmark);

    // This code can run several times on the same nodes (if the
    // output of a render is included in a render), so it must be
    // idempotent. This is not the best, asymptotically. There are
    // things we could do to improve it.
    notifyWatchers(renderedRange.firstNode(), renderedRange.lastNode());
    renderedRange.destroy();
  });
};

Spark.render = function (htmlFunc) {
  var renderer = new Spark._Renderer;
  var frag = renderer.materialize(htmlFunc);
  return frag;
};


// Find all of all nodes and regions that should be preserved in
// patching. Return a list of objects. There are two kinds of objects
// in the list:
//
// A preserved node:
//   {type: "node", from: Node, to: Node}
//
// A preserved (constant) region:
//   {type: "region", fromStart: Node, fromEnd: Node,
//      newRange: LiveRange}
//
// `existingRange` is the range in the document whose contents are to
// be replaced. `newRange` holds the new contents and is not part of
// the document DOM tree.  The implementation will temporarily reparent
// the nodes in `newRange` into the document to check for selector matches.
var PreservationController = function () {
  this.roots = []; // keys 'landmarkRange', 'fromRange', 'toRange'
  this.regionPreservations = [];
};

_.extend(PreservationController.prototype, {
  // Specify preservations that should be in effect on a fromRange/toRange
  // pair.  If specified, `optContextNode` should be an ancestor node of
  // fromRange that selectors are to be considered relative to.
  addRoot: function (preserve, fromRange, toRange, optContextNode) {
    var self = this;
    self.roots.push({ context: optContextNode, preserve: preserve,
                      fromRange: fromRange, toRange: toRange});
  },

  addConstantRegion: function (from, to) {
    var self = this;
    self.regionPreservations.push({
      type: "region",
      fromStart: from.firstNode(), fromEnd: from.lastNode(),
      newRange: to
    });
  },

  computePreservations: function (existingRange, newRange) {
    var self = this;
    var preservations = _.clone(self.regionPreservations);

    var visitLabeledNodes = function (context, clipRange, nodeLabeler, selector, func) {
      context = (context || clipRange.containerNode());
      var nodes = DomUtils.findAllClipped(
        context, selector, clipRange.firstNode(), clipRange.lastNode());

      _.each(nodes, function (n) {
        var label = nodeLabeler(n);
        label && func(n, label);
      });
    };

    // Find the old incarnation of each of the preserved nodes
    _.each(self.roots, function (root) {
      root.fromNodesByLabel = {};
      _.each(root.preserve, function (nodeLabeler, selector) {
        root.fromNodesByLabel[selector] = {};
        visitLabeledNodes(
          root.context, root.fromRange, nodeLabeler, selector,
          function (n, label) {
            root.fromNodesByLabel[selector][label] = n;
          });
      });
    });

    // Temporarily put newRange into the document so that we can do
    // properly contextualized selector queries against it.
    //
    // Create a temporary range around newRange, and also around any enclosing
    // ranges that happen to also start and end on those nodes.  It is ok
    // to temporarily put these in the document as well, because CSS selectors
    // don't care and we will put them back.  `tempRange` will hold our place
    // in the tree `newRange` came from.
    var tempRange = new LiveRange(Spark._TAG, newRange.firstNode(), newRange.lastNode());
    var commentFrag = document.createDocumentFragment();
    commentFrag.appendChild(document.createComment(""));
    var newRangeFrag = tempRange.replaceContents(commentFrag);
    // `wrapperRange` will mark where we inserted newRange into the document.
    var wrapperRange = new LiveRange(Spark._TAG, newRangeFrag);
    existingRange.insertBefore(newRangeFrag);

    _.each(self.roots, function (root) {
      _.each(root.preserve, function (nodeLabeler, selector) {
        visitLabeledNodes(root.context, root.toRange, nodeLabeler, selector, function (n, label) {
          var match = root.fromNodesByLabel[selector][label];
          if (match) {
            preservations.push({ type: "node", from: match, to: n });
            root.fromNodesByLabel[selector][label] = null;
          }
        });
      });
    });

    // Extraction is legal because we're just taking the document
    // back to the state it was in before insertBefore.
    var extractedFrag = wrapperRange.extract();
    wrapperRange.destroy();
    tempRange.replaceContents(extractedFrag);
    tempRange.destroy();

    return preservations;
  }
});


// XXX debugging
var pathForRange = function (r) {
  var path = [], r;
  while ((r = findParentOfType(Spark._ANNOTATION_LABEL, r)))
    path.unshift(r.label);
  return path.join(' :: ');
};

// `range` is a region of `document`. Modify it in-place so that it
// matches the result of Spark.render(htmlFunc), preserving landmarks.
Spark.renderToRange = function (range, htmlFunc) {
  var renderer = new Spark._Renderer();

  // Call 'func' for each landmark in 'range'. Pass two arguments to
  // 'func', the range, and an extra "notes" object such that two
  // landmarks receive the same (===) notes object iff they have the
  // same branch path. 'func' can write to the notes object so long as
  // it limits itself to attributes that do not start with '_'.
  var visitLandmarksInRange = function (range, func) {
    var stack = renderer.newLabelStack();

    range.visit(function (isStart, r) {
      if (r.type === Spark._ANNOTATION_LABEL) {
        if (isStart)
          stack.pushLabel(r.label);
        else
          stack.popLabel();
      } else if (r.type === Spark._ANNOTATION_LANDMARK && isStart) {
        func(r, stack.getNotes());
      }
    });
  };

  // Find all of the landmarks in the old contents of the range
  visitLandmarksInRange(range, function (landmarkRange, notes) {
    notes.originalRange = landmarkRange;
  });

  var frag = renderer.materialize(htmlFunc);

  DomUtils.wrapFragmentForContainer(frag, range.containerNode());

  var tempRange = new LiveRange(Spark._TAG, frag);

  // find preservation roots from matched landmarks inside the
  // rerendered region
  var pc = renderer.pc;
  visitLandmarksInRange(
    tempRange, function (landmarkRange, notes) {
      if (notes.originalRange) {
        if (landmarkRange.constant)
          pc.addConstantRegion(notes.originalRange, landmarkRange);

        pc.addRoot(landmarkRange.preserve,
                   notes.originalRange, landmarkRange);
      }
    });

  // find preservation roots that come from landmarks enclosing the
  // updated region
  var walk = range;
  while ((walk = findParentOfType(Spark._ANNOTATION_LANDMARK, walk)))
    pc.addRoot(walk.preserve, range, tempRange, walk.containerNode());

  pc.addRoot(Spark._globalPreserves, range, tempRange);

  // compute preservations (must do this before destroying tempRange)
  var preservations = pc.computePreservations(range, tempRange);

  tempRange.destroy();

  var results = {};

  // Patch! (using preservations)
  withEventGuard(function () {
    range.operate(function (start, end) {
      // XXX this will destroy all liveranges, including ones
      // inside constant regions whose DOM nodes we are going
      // to preserve untouched
      Spark.finalize(start, end);
      Spark._patch(start.parentNode, frag, start.previousSibling,
                   end.nextSibling, preservations, results);
    });
  });

  _.each(results.regionPreservations, function (landmarkRange) {
    // Rely on the fact that computePreservations only emits
    // region preservations whose ranges are landmarks.
    // This flag means that landmarkRange is a new constant landmark
    // range that matched an old one *and* was DOM-preservable by
    // the patcher.
    landmarkRange.isPreservedConstant = true;
  });
};

// Delete all of the liveranges in the range of nodes between `start`
// and `end`, and call their 'finalize' function if any. Or instead of
// `start` and `end` you may pass a fragment in `start`.
Spark.finalize = function (start, end) {
  if (! start.parentNode && start.nodeType !== 11 /* DocumentFragment */) {
    // Workaround for LiveRanges' current inability to contain
    // a node with no parentNode.
    var frag = document.createDocumentFragment();
    frag.appendChild(start);
    start = frag;
    end = null;
  }
  var wrapper = new LiveRange(Spark._TAG, start, end);
  wrapper.visit(function (isStart, range) {
    isStart && range.finalize && range.finalize();
  });
  wrapper.destroy(true /* recursive */);
};

/******************************************************************************/
/* Data contexts                                                              */
/******************************************************************************/

Spark.setDataContext = withRenderer(function (dataContext, html, _renderer) {
  return _renderer.annotate(
    html, Spark._ANNOTATION_DATA, { data: dataContext });
});

Spark.getDataContext = function (node) {
  var range = findRangeOfType(Spark._ANNOTATION_DATA, node);
  return range && range.data;
};

/******************************************************************************/
/* Events                                                                     */
/******************************************************************************/

var universalListener = null;
var getListener = function () {
  if (!universalListener)
    universalListener = new UniversalEventListener(function (event) {
      // Handle a currently-propagating event on a particular node.
      // We walk each enclosing liverange of the node and offer it the
      // chance to handle the event. It's range.handler's
      // responsibility to check isImmediatePropagationStopped()
      // before delivering events to the user. We precompute the list
      // of enclosing liveranges to defend against the case where user
      // event handlers change the DOM.

      if (eventGuardActive)
        // swallow the event
        return;

      var ranges = [];
      var walk = findRangeOfType(Spark._ANNOTATION_EVENTS,
                                 event.currentTarget);
      while (walk) {
        ranges.push(walk);
        walk = findParentOfType(Spark._ANNOTATION_EVENTS, walk);
      }
      _.each(ranges, function (r) {
        r.handler(event);
      });
    }, Spark._checkIECompliance);

  return universalListener;
};

Spark.attachEvents = withRenderer(function (eventMap, html, _renderer) {
  var listener = getListener();

  var handlerMap = {}; // type -> [{selector, callback}, ...]
  // iterate over eventMap, which has form {"type selector, ...": callback},
  // and populate handlerMap
  _.each(eventMap, function(callback, spec) {
    var clauses = spec.split(/,\s+/);
    // iterate over clauses of spec, e.g. ['click .foo', 'click .bar']
    _.each(clauses, function (clause) {
      var parts = clause.split(/\s+/);
      if (parts.length === 0)
        return;

      var type = parts.shift();
      var selector = parts.join(' ');

      handlerMap[type] = handlerMap[type] || [];
      handlerMap[type].push({selector: selector, callback: callback});
    });
  });

  var eventTypes = _.keys(handlerMap);

  var installHandlers = function (range) {
    _.each(eventTypes, function (t) {
      for(var n = range.firstNode(),
              after = range.lastNode().nextSibling;
          n && n !== after;
          n = n.nextSibling)
        listener.installHandler(n, t);
    });
  };

  html = _renderer.annotate(
    html, Spark._ANNOTATION_WATCH, {
      notify: function () {
        installHandlers(this);
      }
    });

  var finalized = false;

  html = _renderer.annotate(
    html, Spark._ANNOTATION_EVENTS, function (range) {
      if (! range)
        return;

      _.each(eventTypes, function (t) {
        listener.addType(t);
      });
      installHandlers(range);

      range.finalize = function () {
        finalized = true;
      };

      range.handler = function (event) {
        var handlers = handlerMap[event.type] || [];

        for (var i = 0; i < handlers.length; i++) {
          if (finalized || event.isImmediatePropagationStopped())
            return;

          var handler = handlers[i];
          var callback = handler.callback;
          var selector = handler.selector;

          if (selector) {
            if (! DomUtils.matchesSelectorClipped(
              event.currentTarget, range.containerNode(), selector,
              range.firstNode(), range.lastNode())) {
              continue;
            }
          } else {
            // if no selector, only match the event target
            if (event.currentTarget !== event.target)
              continue;
          }

          // Found a matching handler. Call it.
          var eventData = Spark.getDataContext(event.currentTarget);
          var landmarkRange =
                findParentOfType(Spark._ANNOTATION_LANDMARK, range);
          var landmark = (landmarkRange && landmarkRange.landmark);

          // Note that the handler can do arbitrary things, like call
          // Meteor.flush() or otherwise remove and finalize parts of
          // the DOM.  We can't assume `range` is valid past this point,
          // and we'll check the `finalized` flag at the top of the loop.
          var returnValue = callback.call(eventData, event, landmark);

          // allow app to `return false` from event handler, just like
          // you can in a jquery event handler
          if (returnValue === false) {
            event.stopImmediatePropagation();
            event.preventDefault();
          }
        }
      };
    });

  return html;
});

/******************************************************************************/
/* Isolate                                                                    */
/******************************************************************************/

Spark.isolate = function (htmlFunc) {
  var renderer = Spark._currentRenderer.get();
  if (!renderer)
    return htmlFunc();

  var range;
  var firstRun = true;
  var retHtml;
  Meteor.autorun(function (handle) {
    if (firstRun) {
      retHtml = renderer.annotate(
        htmlFunc(), Spark._ANNOTATION_ISOLATE,
        function (r) {
          if (! r) {
            // annotation not used; kill our context
            handle.stop();
          } else {
            range = r;
            range.finalize = function () {
              // Spark.finalize() was called on our range (presumably
              // because it was removed from the document.)  Kill
              // this context and stop rerunning.
              handle.stop();
            };
          }
        });
      firstRun = false;
    } else {
      Spark.renderToRange(range, htmlFunc);
    }
  });

  return retHtml;
};

/******************************************************************************/
/* Lists                                                                      */
/******************************************************************************/

Spark.list = function (cursor, itemFunc, elseFunc) {
  elseFunc = elseFunc || function () { return ''; };

  // Create a level of indirection around our cursor callbacks so we
  // can change them later
  var callbacks = {};
  var observerCallbacks = {};
  _.each(["added", "removed", "moved", "changed"], function (name) {
    observerCallbacks[name] = function () {
      return callbacks[name].apply(null, arguments);
    };
  });

  // Get the current contents of the cursor.
  // XXX currently we count on observe() using only added() to deliver
  // the initial contents. are we allow to do that, or do we need to
  // implement removed/moved/changed here as well?
  var initialContents = [];
  _.extend(callbacks, {
    added: function (item, beforeIndex) {
      initialContents.splice(beforeIndex, 0, item);
    }
  });
  var handle = cursor.observe(observerCallbacks);

  // Get the renderer, if any
  var renderer = Spark._currentRenderer.get();
  var maybeAnnotate = renderer ?
        _.bind(renderer.annotate, renderer) :
    function (html) { return html; };

  // Render the initial contents. If we have a renderer, create a
  // range around each item as well as around the list, and save them
  // off for later.
  var html = '';
  var outerRange;
  var itemRanges = [];
  if (! initialContents.length)
    html = elseFunc();
  else {
    for (var i = 0; i < initialContents.length; i++) {
      (function (i) {
        html += maybeAnnotate(itemFunc(initialContents[i]),
                              Spark._ANNOTATION_LIST_ITEM,
                              function (range) {
                                itemRanges[i] = range;
                              });
      })(i); // scope i to closure
    }
  }
  initialContents = null; // save memory
  var stopped = false;
  var cleanup = function () {
    handle.stop();
    stopped = true;
  };
  html = maybeAnnotate(html, Spark._ANNOTATION_LIST, function (range) {
    if (! range) {
      // We never ended up on the screen (caller discarded our return
      // value)
      cleanup();
    } else {
      outerRange = range;
      outerRange.finalize = cleanup;
    }
  });

  // No renderer? Then we have no way to update the returned html and
  // we can close the observer.
  if (! renderer)
    cleanup();

  // Called by `removed` and `moved` in order to cause render callbacks on
  // parent landmarks.
  // XXX This is not the final solution.  1) This code should be unified
  // with the code in scheduleOnscreenSetup.  2) In general, lists are
  // going to cause a lot of callbacks (one per collection callback).
  // Maybe that will make sense if we give render callbacks subrange info.
  var notifyParentsRendered = function () {
    var walk = outerRange;
    while ((walk = findParentOfType(Spark._ANNOTATION_LANDMARK, walk)))
      walk.rendered.call(walk.landmark);
  };

  var later = function (f) {
    Meteor._atFlush(function () {
      if (! stopped)
        withEventGuard(f);
    });
  };

  // The DOM update callbacks.
  _.extend(callbacks, {
    added: function (item, beforeIndex) {
      later(function () {
        var frag = Spark.render(_.bind(itemFunc, null, item));
        DomUtils.wrapFragmentForContainer(frag, outerRange.containerNode());
        var range = makeRange(Spark._ANNOTATION_LIST_ITEM, frag);

        if (! itemRanges.length) {
          Spark.finalize(outerRange.replaceContents(frag));
        } else if (beforeIndex === itemRanges.length) {
          itemRanges[itemRanges.length - 1].insertAfter(frag);
        } else {
          itemRanges[beforeIndex].insertBefore(frag);
        }

        itemRanges.splice(beforeIndex, 0, range);
      });
    },

    removed: function (item, atIndex) {
      later(function () {
        if (itemRanges.length === 1) {
          var frag = Spark.render(elseFunc);
          DomUtils.wrapFragmentForContainer(frag, outerRange.containerNode());
          Spark.finalize(outerRange.replaceContents(frag));
        } else
          Spark.finalize(itemRanges[atIndex].extract());

        itemRanges.splice(atIndex, 1);

        notifyParentsRendered();
      });
    },

    moved: function (item, oldIndex, newIndex) {
      later(function () {
        if (oldIndex === newIndex)
          return;

        var frag = itemRanges[oldIndex].extract();
        var range = itemRanges.splice(oldIndex, 1)[0];
        if (newIndex === itemRanges.length)
          itemRanges[itemRanges.length - 1].insertAfter(frag);
        else
          itemRanges[newIndex].insertBefore(frag);

        itemRanges.splice(newIndex, 0, range);

        notifyParentsRendered();
      });
    },

    changed: function (item, atIndex) {
      later(function () {
        Spark.renderToRange(itemRanges[atIndex], _.bind(itemFunc, null, item));
      });
    }
  });

  return html;
};

/******************************************************************************/
/* Labels and landmarks                                                       */
/******************************************************************************/

var nextLandmarkId = 1;

Spark.Landmark = function () {
  this.id = nextLandmarkId++;
  this._range = null; // will be set when put onscreen
};

_.extend(Spark.Landmark.prototype, {
  firstNode: function () {
    return this._range.firstNode();
  },
  lastNode: function () {
    return this._range.lastNode();
  },
  find: function (selector) {
    var r = this._range;
    return DomUtils.findClipped(r.containerNode(), selector,
                                r.firstNode(), r.lastNode());
  },
  findAll: function (selector) {
    var r = this._range;
    return DomUtils.findAllClipped(r.containerNode(), selector,
                                   r.firstNode(), r.lastNode());
  },
  hasDom: function () {
    return !! this._range;
  }
});

Spark.UNIQUE_LABEL = ['UNIQUE_LABEL'];

// label must be a string.
// or pass label === null to not drop a label after all (meaning that
// this function is a noop)
Spark.labelBranch = function (label, htmlFunc) {
  var renderer = Spark._currentRenderer.get();
  if (! renderer || label === null)
    return htmlFunc();

  if (label === Spark.UNIQUE_LABEL)
    label = Spark._createId();

  renderer.currentBranch.pushLabel(label);
  var html = htmlFunc();
  var occupied = renderer.currentBranch.getNotes().occupied;
  renderer.currentBranch.popLabel();

  if (! occupied)
    // don't create annotation if branch doesn't contain any landmarks.
    // if this label isn't on an element-level HTML boundary, then that
    // is certainly the case.
    return html;

  return renderer.annotate(
    html, Spark._ANNOTATION_LABEL, { label: label });

  // XXX what happens if the user doesn't use the return value, or
  // doesn't use it directly, eg, swaps the branches of the tree
  // around? "that's an error?" the result would be that the apparent
  // branch path of a landmark at render time would be different from
  // its apparent branch path in the actual document. seems like the
  // answer is to have labelBranch not drop an annotation, and keep
  // the branch label info outside of the DOM in a parallel tree of
  // labels and landmarks (likely similar to the one we're already
  // keeping?) a little tricky since not every node in the label tree
  // is actually populated with a landmark? (though we could change
  // that I guess -- they would be landmarks without any specific DOM
  // nodes?)
};

Spark.createLandmark = function (options, htmlFunc) {
  var renderer = Spark._currentRenderer.get();
  if (! renderer) {
    // no renderer -- create and destroy Landmark inline
    var landmark = new Spark.Landmark;
    options.created && options.created.call(landmark);
    var html = htmlFunc(landmark);
    options.destroyed && options.destroyed.call(landmark);
    return html;
  }

  // Normalize preserve map
  var preserve = {};
  if (_.isArray(options.preserve))
    _.each(options.preserve, function (selector) {
      preserve[selector] = true;
    });
  else
    preserve = options.preserve || {};
  for (var selector in preserve)
    if (typeof preserve[selector] !== 'function')
      preserve[selector] = function () { return true; };

  renderer.currentBranch.mark('occupied');
  var notes = renderer.currentBranch.getNotes();
  var landmark;
  if (notes.originalRange) {
    if (notes.originalRange.superceded)
      throw new Error("Can't create second landmark in same branch");
    notes.originalRange.superceded = true; // prevent destroyed(), second match
    landmark = notes.originalRange.landmark; // the old Landmark
  } else {
    landmark = new Spark.Landmark;
    if (options.created) {
      // Run callback outside the current Spark.isolate's deps context.
      // XXX Can't call run() on null, so this is a hack.  Running inside
      // a fresh context wouldn't be equivalent.
      var oldCx = Meteor.deps.Context.current;
      Meteor.deps.Context.current = null;
      try {
        options.created.call(landmark);
      } finally {
        Meteor.deps.Context.current = oldCx;
      }
    }
  }
  notes.landmark = landmark;

  var html = htmlFunc(landmark);
  return renderer.annotate(
    html, Spark._ANNOTATION_LANDMARK, function (range) {
      if (! range) {
        // annotation not used
        options.destroyed && options.destroyed.call(landmark);
        return;
      }

      _.extend(range, {
        preserve: preserve,
        constant: !! options.constant,
        rendered: options.rendered || function () {},
        destroyed: options.destroyed || function () {},
        landmark: landmark,
        finalize: function () {
          if (! this.superceded) {
            this.landmark._range = null;
            this.destroyed.call(this.landmark);
          }
        }
      });

      landmark._range = range;
      renderer.landmarkRanges.push(range);
    });
};

// used by unit tests
Spark._getEnclosingLandmark = function (node) {
  var range = findRangeOfType(Spark._ANNOTATION_LANDMARK, node);
  return range ? range.landmark : null;
};

})();

Spark._patch = function(tgtParent, srcParent, tgtBefore, tgtAfter, preservations,
                        results) {

  var copyFunc = function(t, s) {
    LiveRange.transplantTag(Spark._TAG, t, s);
  };

  var patcher = new Spark._Patcher(
    tgtParent, srcParent, tgtBefore, tgtAfter);


  var visitNodes = function(parent, before, after, func) {
    for(var n = before ? before.nextSibling : parent.firstChild;
        n && n !== after;
        n = n.nextSibling) {
      if (func(n) !== false && n.firstChild)
        visitNodes(n, null, null, func);
    }
  };

  // results arg is optional; it is mutated if provided; returned either way
  results = (results || {});
  // array of LiveRanges that were successfully preserved from
  // the region preservations
  var regionPreservations = (results.regionPreservations =
                             results.regionPreservations || []);

  var lastTgtMatch = null;

  visitNodes(srcParent, null, null, function(src) {
    // XXX inefficient to scan for match for every node!
    // We could at least skip non-element nodes, except for "range matches"
    // used for constant chunks, which may begin on a non-element.
    // But really this shouldn't be a linear search.
    var pres = _.find(preservations, function (p) {
      // find preserved region starting at `src`, if any
      return p.type === 'region' && p.newRange.firstNode() === src;
    }) || _.find(preservations, function (p) {
      // else, find preservation of `src`
      return p.type === 'node' && p.to === src;
    });

    if (pres) {
      var tgt = (pres.type === 'region' ? pres.fromStart : pres.from);
      if (! lastTgtMatch ||
          DomUtils.compareElementIndex(lastTgtMatch, tgt) < 0) {
        if (pres.type === 'region') {
          // preserved region for constant landmark
          if (patcher.match(pres.fromStart, pres.newRange.firstNode(),
                            copyFunc, true)) {
            patcher.skipToSiblings(pres.fromEnd, pres.newRange.lastNode());
            // without knowing or caring what DOM nodes are in pres.newRange,
            // transplant the range data to pres.fromStart and pres.fromEnd
            // (including references to enclosing ranges).
            LiveRange.transplantRange(
              pres.fromStart, pres.fromEnd, pres.newRange);
            regionPreservations.push(pres.newRange);
          }
        } else if (pres.type === 'node') {
          if (patcher.match(tgt, src, copyFunc)) {
            // match succeeded
            lastTgtMatch = tgt;
            if (tgt.firstChild || src.firstChild) {
              // Don't patch contents of TEXTAREA tag,
              // which are only the initial contents but
              // may affect the tag's .value in IE.
              if (tgt.nodeName !== "TEXTAREA") {
                // recurse!
                Spark._patch(tgt, src, null, null, preservations);
              }
            }
            return false; // tell visitNodes not to recurse
          }
        }
      }
    }
    return true;
  });

  patcher.finish();

  return results;
};


// A Patcher manages the controlled replacement of a region of the DOM.
// The target region is changed in place to match the source region.
//
// The target region consists of the children of tgtParent, extending from
// the child after tgtBefore to the child before tgtAfter.  A null
// or absent tgtBefore or tgtAfter represents the beginning or end
// of tgtParent's children.  The source region consists of all children
// of srcParent, which may be a DocumentFragment.
//
// To use a new Patcher, call `match` zero or more times followed by
// `finish`.
//
// A match is a correspondence between an old node in the target region
// and a new node in the source region that will replace it.  Based on
// this correspondence, the target node is preserved and the attributes
// and children of the source node are copied over it.  The `match`
// method declares such a correspondence.  A Patcher that makes no matches,
// for example, just removes the target nodes and inserts the source nodes
// in their place.
//
// Constructor:
Spark._Patcher = function(tgtParent, srcParent, tgtBefore, tgtAfter) {
  this.tgtParent = tgtParent;
  this.srcParent = srcParent;

  this.tgtBefore = tgtBefore;
  this.tgtAfter = tgtAfter;

  this.lastKeptTgtNode = null;
  this.lastKeptSrcNode = null;
};


// Advances the patching process up to tgtNode in the target tree,
// and srcNode in the source tree.  tgtNode will be preserved, with
// the attributes of srcNode copied over it, in essence identifying
// the two nodes with each other.  The same treatment is given to
// any parents of the nodes that are newly implicated as corresponding.
// In the process of traversing from the last matched nodes to these
// ones, all nodes "in between" in the target document, at any level,
// are removed, and all nodes "in between" in the source document
// are copied over to their appropriate positions.
//
// For example, if match() is called only once, and then finish()
// is called, the effect is to preserve tgtNode, its children,
// and its ancestors (parent chain), while swapping out all its
// siblings and the siblings of its ancestors, so that the target
// tree is mutated to look like the source tree did.
//
// The caller is responsible for ensuring the precondition that
// subsequent tgtNodes and subsequent srcNodes are strictly "in order."
// The ordering referred to here is a partial order in which A comes
// before B if their tags would be disjoint in HTML, i.e. the end of
// A comes before the beginning of B.  Put another way, there is some
// ancestor of A and some ancestor of B that have the same parent,
// are different, and are in order.
//
// There are other requirements for two nodes to be "matched,"
// but match() can detect them and exit gracefully returning false.
// For example, the tag-names must be the same, and the tag-names
// of their parents.  More subtly, it may be impossible to match
// the parents of tgtNode or srcNode because they have been
// previously matched.  If we are to match a series of P tags
// that are each inside one DIV, for example, is it the same DIV
// or not?  If the source and target disagree, we will have to
// reparent one of the Ps.  Users should not be moving identified
// nodes, but we want to still be correct (fall back on replacement)
// if they do.
//
// If false is returned, the match was impossible, but patching
// can continue and will still be otherwise correct.  The next call
// to match() must still obey the order constraint, as the patcher
// internally only moves forwards and patches as it goes.
//
// copyCallback is called on every new matched (tgt, src) pair
// right after copying attributes.  It's a good time to transplant
// liveranges and patch children.
Spark._Patcher.prototype.match = function(
  tgtNode, srcNode, copyCallback, onlyAdvance) {

  // last nodes "kept" (matched/identified with each other)
  var lastKeptTgt = this.lastKeptTgtNode;
  var lastKeptSrc = this.lastKeptSrcNode;
  // nodes to match and keep, this time around
  var tgt = tgtNode;
  var src = srcNode;

  if ((! tgt) != (! src)) {
    return false; // truthinesses don't match
  }

  var starting = ! lastKeptTgt;
  var finishing = ! tgt;

  if (! starting) {
    // move lastKeptTgt/lastKeptSrc forward and out,
    // until they are siblings of tgt/src or of an ancestor of tgt/src,
    // replacing as we go.  If tgt/src is falsy, we make it to the
    // top level.
    while (lastKeptTgt.parentNode !== this.tgtParent &&
           ! (tgt && DomUtils.elementContains(lastKeptTgt.parentNode, tgt))) {
      // Last-kept nodes are inside parents that are not
      // parents of the newly matched nodes.  Must finish
      // replacing their contents and back out.
      this._replaceNodes(lastKeptTgt, null, lastKeptSrc, null);
      lastKeptTgt = lastKeptTgt.parentNode;
      lastKeptSrc = lastKeptSrc.parentNode;
    }

    // update instance vars; there's no going back inside these nodes
    this.lastKeptTgtNode = lastKeptTgt;
    this.lastKeptSrcNode = lastKeptSrc;

    // Make sure same number of levels of "moving up" are
    // appropriate for src as well, i.e. we aren't trying
    // to match <c> in (<a><b/><c/></a>, <a><b/></a><a><c/></a>)
    // after matching <b>, or vice versa.  In other words,
    // if tag names and depths match, but identities of parents
    // are inconsistent relative to previous matches, we catch it
    // here.  In the example, lastKeptTgt would be the <b/> node
    // on the left, which is not sibling of <c/> or of an ancestor
    // of <c/> on the right.  If the example were reversed,
    // lastKeptTgt would be the first <a> node, which is an
    // ancestor of <c/> on the left rather than a sibling of an
    // ancestor.
    if (! finishing &&
        (DomUtils.elementContains(lastKeptSrc, src) ||
         ! (lastKeptSrc.parentNode === this.srcParent ||
            DomUtils.elementContains(lastKeptSrc.parentNode, src)))) {
      return false;
    }
  }

  if (finishing) {
    this._replaceNodes(lastKeptTgt, null, lastKeptSrc, null,
                       this.tgtParent, this.srcParent);
  } else {
    // Compare tag names and depths to make sure we can match nodes...
    if (! onlyAdvance) {
      if (tgt.nodeName !== src.nodeName)
        return false;
    }

    // Look at tags of parents until we hit parent of last-kept,
    // which we know is ok.
    for(var a=tgt.parentNode, b=src.parentNode;
        a !== (starting ? this.tgtParent : lastKeptTgt.parentNode);
        a = a.parentNode, b = b.parentNode) {
      if (b === (starting ? this.srcParent : lastKeptSrc.parentNode))
        return false; // src is shallower, b hit top first
      if (a.nodeName !== b.nodeName)
        return false; // tag names don't match
    }
    if (b !== (starting ? this.srcParent : lastKeptSrc.parentNode)) {
      return false; // src is deeper, b didn't hit top when a did
    }

    var firstIter = true;
    // move tgt and src backwards and out, replacing as we go
    while (true) {
      if (! (firstIter && onlyAdvance)) {
        if (tgt.nodeType === 1) /* ELEMENT */
          Spark._Patcher._copyAttributes(tgt, src);
        if (copyCallback)
          copyCallback(tgt, src);
      }

      firstIter = false;

      if ((starting ? this.tgtParent : lastKeptTgt.parentNode)
          === tgt.parentNode) {
        // we've worked our way up to the same level as the last-kept nodes
        this._replaceNodes(lastKeptTgt, tgt, lastKeptSrc, src);
        break;
      } else {
        this._replaceNodes(null, tgt, null, src);
        // move up to keep (match) parents as well
        tgt = tgt.parentNode;
        src = src.parentNode;
      }
    }
  }

  this.lastKeptTgtNode = tgtNode;
  this.lastKeptSrcNode = srcNode;

  return true;
};

// After a match, skip ahead to later siblings of the last kept nodes,
// without performing any replacements.
Spark._Patcher.prototype.skipToSiblings = function(tgt, src) {
  var lastTgt = this.lastKeptTgtNode;
  var lastSrc = this.lastKeptSrcNode;

  if (! (lastTgt && lastTgt.parentNode === tgt.parentNode))
    return false;

  if (! (lastSrc && lastSrc.parentNode === src.parentNode))
    return false;

  this.lastKeptTgtNode = tgt;
  this.lastKeptSrcNode = src;

  return true;
};

// Completes patching assuming no more matches.
//
// Patchers are single-use, so no more methods can be called
// on the Patcher.
Spark._Patcher.prototype.finish = function() {
  return this.match(null, null);
};

// Replaces the siblings between tgtBefore and tgtAfter (exclusive on both
// sides) with the siblings between srcBefore and srcAfter (exclusive on both
// sides).  Falsy values indicate start or end of siblings as appropriate.
//
// Precondition: tgtBefore and tgtAfter have same parent; either may be falsy,
// but not both, unless optTgtParent is provided.  Same with srcBefore/srcAfter.
Spark._Patcher.prototype._replaceNodes = function(
  tgtBefore, tgtAfter, srcBefore, srcAfter, optTgtParent, optSrcParent)
{
  var tgtParent = optTgtParent || (tgtBefore || tgtAfter).parentNode;
  var srcParent = optSrcParent || (srcBefore || srcAfter).parentNode;

  // deal with case where top level is a range
  if (tgtParent === this.tgtParent) {
    tgtBefore = tgtBefore || this.tgtBefore;
    tgtAfter = tgtAfter || this.tgtAfter;
  }
  if (srcParent === this.srcParent) {
    srcBefore = srcBefore || this.srcBefore;
    srcAfter = srcAfter || this.srcAfter;
  }


  // remove old children
  var n;
  while ((n = tgtBefore ? tgtBefore.nextSibling : tgtParent.firstChild)
         && n !== tgtAfter) {
    tgtParent.removeChild(n);
  }

  // add new children
  var m;
  while ((m = srcBefore ? srcBefore.nextSibling : srcParent.firstChild)
         && m !== srcAfter) {
    tgtParent.insertBefore(m, tgtAfter || null);
  }
};

// Copy HTML attributes of node `src` onto node `tgt`.
//
// The effect we are trying to achieve is best expresed in terms of
// HTML.  Whatever HTML generated `tgt`, we want to mutate the DOM element
// so that it is as if it were the HTML that generated `src`.
// We want to preserve JavaScript properties in general (tgt.foo),
// while syncing the HTML attributes (tgt.getAttribute("foo")).
//
// This is complicated by form controls and the fact that old IE
// can't keep the difference straight between properties and attributes.
Spark._Patcher._copyAttributes = function(tgt, src) {
  var srcAttrs = src.attributes;
  var tgtAttrs = tgt.attributes;

  // Determine whether tgt has focus; works in all browsers
  // as of FF3, Safari4
  var target_focused = (tgt === document.activeElement);

  // Is this a control with a user-mutated "value" property?
  var has_user_value = (
    (tgt.nodeName === "INPUT" &&
     (tgt.type === "text" || tgt.type === "range")) ||
      tgt.nodeName === "TEXTAREA");

  ///// Clear current attributes

  if (tgt.style.cssText)
    tgt.style.cssText = '';

  var isRadio = false;
  if (tgt.nodeName === "INPUT") {
    // Record for later whether this is a radio button.
    isRadio = (tgt.type === 'radio');
    // Clearing the attributes of a checkbox won't necessarily
    // uncheck it, eg in FF12, so we uncheck explicitly
    // (if necessary; we don't want to generate spurious
    // propertychange events in old IE).
    if (tgt.checked === true && src.checked === false) {
      tgt.checked = false;
    }
  }

  for(var i=tgtAttrs.length-1; i>=0; i--) {
    var attr = tgtAttrs[i];
    // In old IE, attributes that are possible on a node
    // but not actually present will show up in this loop
    // with specified=false.  All other browsers support
    // 'specified' (because it's part of the spec) and
    // set it to true.
    if (! attr.specified)
      continue;
    var name = attr.name;
    // Filter out attributes that are indexable by number
    // but not by name.  This kills the weird "propdescname"
    // attribute in IE 8.
    if (! tgtAttrs[name])
      continue;
    // Some properties don't mutate well, and we simply
    // don't try to patch them.  For example, you can't
    // change a control's type in IE.
    if (name === "id" || name === "type")
      continue;
    // Removing a radio button's "name" property and restoring
    // it is harmless in most browsers but breaks in IE 7.
    // It seems unlikely enough that a radio button will
    // sometimes have a group and sometimes not.
    if (isRadio && name === "name")
      continue;
    // Never delete the "value" attribute.  It's more effective
    // to simply overwrite it in the next phase.
    if (name === "value")
      continue;
    // Removing 'src' (e.g. in an iframe) can only be bad.
    if (name === "src")
      continue;

    // We want to patch any HTML attributes that were specified in the
    // source, but preserve DOM properties set programmatically.
    // Old IE makes this difficult by exposing properties as attributes.
    // Expando properties will even appear in innerHTML, though not if the
    // value is an object rather than a primitive.
    //
    // We use a heuristic to determine if we are looking at a programmatic
    // property (an expando) rather than a DOM attribute.
    //
    // Losing jQuery's expando (whose value is a number) is very bad,
    // because it points to event handlers that only jQuery can detach,
    // and only if the expando is in place.
    var possibleExpando = tgt[name];
    if (possibleExpando &&
        (typeof possibleExpando === "object" ||
         /^jQuery/.test(name)))
      continue; // for object properties that surface attributes only in IE
    tgt.removeAttributeNode(attr);
  }

  ///// Copy over src's attributes

  if (tgt.mergeAttributes) {
    // IE code path:
    //
    // Only IE (all versions) has mergeAttributes.
    // It's probably a good bit faster in old IE than
    // iterating over all the attributes, and the treatment
    // of form controls is sufficiently different in IE from
    // other browsers that we keep the special cases separate.

    tgt.mergeAttributes(src);

    if (typeof tgt.checked !== "undefined" && src.checked)
      tgt.checked = src.checked;

    if (src.name)
      tgt.name = src.name;

  } else {
    // Non-IE code path:

    for(var i=0, L=srcAttrs.length; i<L; i++) {
      var srcA = srcAttrs.item(i);
      if (srcA.specified) {
        var name = srcA.name.toLowerCase();
        var value = String(srcA.value);
        if (name === "type") {
        // can't change type of INPUT in IE; don't support it
        } else if (name === "checked") {
          tgt.checked = tgt.defaultChecked = (value && value !== "false");
          tgt.setAttribute("checked", "checked");
        } else if (name === "style") {
          tgt.style.cssText = src.style.cssText;
        } else if (name === "class") {
          tgt.className = src.className;
        } else if (name === "value") {
          // don't set attribute, just overwrite property
          // (in next phase)
        } else if (name === "src") {
          // only set if different.  protects iframes
          if (src.src !== tgt.src)
            tgt.src = src.src;
        } else {
          tgt.setAttribute(name, value);
        }
      }
    }
  }

  // Copy the control's value, only if tgt doesn't have focus.
  if (has_user_value) {
    if (! target_focused)
      tgt.value = src.value;
  }

};
(function() {
  var suppress = 0;

  // replacement for console.log. This is a temporary API. We should
  // provide a real logging API soon (possibly just a polyfill for
  // console?)
  //
  // NOTE: this is used on the server to print the warning about
  // having autopublish enabled when you probably meant to turn it
  // off. it's not really the proper use of something called
  // _debug. the intent is for this message to go to the terminal and
  // be very visible. if you change _debug to go someplace else, etc,
  // please fix the autopublish code to do something reasonable.
  Meteor._debug = function (/* arguments */) {
    if (suppress) {
      suppress--;
      return;
    }
    if (typeof console !== 'undefined' &&
        typeof console.log !== 'undefined') {
      if (arguments.length == 0) { // IE Companion breaks otherwise
        // IE10 PP4 requires at least one argument
        console.log('');
      } else {
        // IE doesn't have console.log.apply, it's not a real Object.
        // http://stackoverflow.com/questions/5538972/console-log-apply-not-working-in-ie9
        // http://patik.com/blog/complete-cross-browser-console-log/
        if (typeof console.log.apply === "function") {
          // Most browsers
          console.log.apply(console, arguments);
        } else if (typeof Function.prototype.bind === "function") {
          // IE9
          var log = Function.prototype.bind.call(console.log, console);
          log.apply(console, arguments);
        } else {
          // IE8
          Function.prototype.call.call(console.log, console, Array.prototype.slice.call(arguments));
        }
      }
    }
  };

  // Suppress the next 'count' Meteor._debug messsages. Use this to
  // stop tests from spamming the console.
  Meteor._suppress_log = function (count) {
    suppress += count;
  };
})();

(function () {

  Spark._labelFromIdOrName = function(n) {
    var label = null;

    if (n.nodeType === 1 /*ELEMENT_NODE*/) {
      if (n.id) {
        label = '#' + n.id;
      } else if (n.getAttribute("name")) {
        label = n.getAttribute("name");
        // Radio button special case:  radio buttons
        // in a group all have the same name.  Their value
        // determines their identity.
        // Checkboxes with the same name and different
        // values are also sometimes used in apps, so
        // we treat them similarly.
        if (n.nodeName === 'INPUT' &&
            (n.type === 'radio' || n.type === 'checkbox') &&
            n.value)
          label = label + ':' + n.value;

        // include parent names and IDs up to enclosing ID
        // in the label
        while (n.parentNode &&
               n.parentNode.nodeType === 1 /*ELEMENT_NODE*/) {
          n = n.parentNode;
          if (n.id) {
            label = '#' + n.id + "/" + label;
            break;
          } else if (n.getAttribute('name')) {
            label = n.getAttribute('name') + "/" + label;
          }
        }
      }
    }

    return label;
  };

})();

(function () {

var inputTags = 'input textarea button select option'.split(' ');

var selector = _.map(inputTags, function (t) {
  return t.replace(/^.*$/, '$&[id], $&[name]');
}).join(', ');


Spark._globalPreserves[selector] = Spark._labelFromIdOrName;

})();

