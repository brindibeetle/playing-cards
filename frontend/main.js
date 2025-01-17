(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_enqueueEffects(managers, result.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$document = _Browser_document;
var $author$project$Main$ShuffleMsg = function (a) {
	return {$: 'ShuffleMsg', a: a};
};
var $author$project$Main$Shuffling = {$: 'Shuffling'};
var $author$project$Card$Ace = {$: 'Ace'};
var $author$project$Card$Jack = {$: 'Jack'};
var $author$project$Card$King = {$: 'King'};
var $author$project$Card$N10 = {$: 'N10'};
var $author$project$Card$N2 = {$: 'N2'};
var $author$project$Card$N3 = {$: 'N3'};
var $author$project$Card$N4 = {$: 'N4'};
var $author$project$Card$N5 = {$: 'N5'};
var $author$project$Card$N6 = {$: 'N6'};
var $author$project$Card$N7 = {$: 'N7'};
var $author$project$Card$N8 = {$: 'N8'};
var $author$project$Card$N9 = {$: 'N9'};
var $author$project$Card$Queen = {$: 'Queen'};
var $author$project$Card$allRanks = _List_fromArray(
	[$author$project$Card$Ace, $author$project$Card$King, $author$project$Card$Queen, $author$project$Card$Jack, $author$project$Card$N10, $author$project$Card$N9, $author$project$Card$N8, $author$project$Card$N7, $author$project$Card$N6, $author$project$Card$N5, $author$project$Card$N4, $author$project$Card$N3, $author$project$Card$N2]);
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $author$project$Card$thisCard = F2(
	function (suit, rank) {
		return {rank: rank, suit: suit};
	});
var $author$project$Card$addCardsofSuit = F2(
	function (suit, cards) {
		return A2(
			$elm$core$List$append,
			cards,
			A2(
				$elm$core$List$map,
				$author$project$Card$thisCard(suit),
				$author$project$Card$allRanks));
	});
var $author$project$Card$Clubs = {$: 'Clubs'};
var $author$project$Card$Diamonds = {$: 'Diamonds'};
var $author$project$Card$Hearts = {$: 'Hearts'};
var $author$project$Card$Spades = {$: 'Spades'};
var $author$project$Card$allSuits = _List_fromArray(
	[$author$project$Card$Spades, $author$project$Card$Hearts, $author$project$Card$Clubs, $author$project$Card$Diamonds]);
var $author$project$Card$allCards = A3($elm$core$List$foldl, $author$project$Card$addCardsofSuit, _List_Nil, $author$project$Card$allSuits);
var $author$project$Distribute$init = {distributingDone: false, distributorCardIndex: 0, pile: $elm$core$Array$empty, piles: $elm$core$Array$empty};
var $author$project$EndAnimation$init = {cardIndex: 0, cards: $elm$core$Array$empty, offset: 0};
var $author$project$FlyingHome$initCoordinates = {x: 0, y: 0};
var $author$project$FlyingHome$init = {current: $author$project$FlyingHome$initCoordinates, from: $author$project$FlyingHome$initCoordinates, iteration: 0, maybeCard: $elm$core$Maybe$Nothing, to: $author$project$FlyingHome$initCoordinates};
var $norpan$elm_html5_drag_drop$Html5$DragDrop$NotDragging = {$: 'NotDragging'};
var $norpan$elm_html5_drag_drop$Html5$DragDrop$init = $norpan$elm_html5_drag_drop$Html5$DragDrop$NotDragging;
var $author$project$Home$numberOfHomes = 4;
var $elm$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			$elm$core$Array$initialize,
			n,
			function (_v0) {
				return e;
			});
	});
var $author$project$Home$init = {
	dragDrop: $norpan$elm_html5_drag_drop$Html5$DragDrop$init,
	homes: A2($elm$core$Array$repeat, $author$project$Home$numberOfHomes, $elm$core$Maybe$Nothing)
};
var $author$project$Pile$numberOfPiles = 8;
var $author$project$Pile$init = {
	piles: A2($elm$core$Array$repeat, $author$project$Pile$numberOfPiles, $elm$core$Array$empty)
};
var $author$project$Space$numberOfSpaces = 4;
var $author$project$Space$init = {
	dragDrop: $norpan$elm_html5_drag_drop$Html5$DragDrop$init,
	spaces: A2($elm$core$Array$repeat, $author$project$Space$numberOfSpaces, $elm$core$Maybe$Nothing)
};
var $author$project$ModelHistory$initMoment = {homesModel: $author$project$Home$init, pilesModel: $author$project$Pile$init, spacesModel: $author$project$Space$init};
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $author$project$ModelHistory$init = $elm$core$List$singleton($author$project$ModelHistory$initMoment);
var $author$project$Shuffle$DoRandom = {$: 'DoRandom'};
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$core$Process$sleep = _Process_sleep;
var $author$project$Shuffle$randomShuffle = function (turn) {
	return A2(
		$elm$core$Task$perform,
		$elm$core$Basics$always($author$project$Shuffle$DoRandom),
		$elm$core$Process$sleep(
			(!turn) ? 1300 : 200));
};
var $author$project$Shuffle$init = function (pile) {
	return _Utils_Tuple2(
		{pile: pile, shuffledTimes: 0, shufflingDone: false},
		$author$project$Shuffle$randomShuffle(0));
};
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $author$project$Pile$makePile = function (cards) {
	return $elm$core$Array$fromList(cards);
};
var $elm$core$Platform$Cmd$map = _Platform_map;
var $author$project$Main$init = function (flags) {
	var _v0 = $author$project$Shuffle$init(
		$author$project$Pile$makePile($author$project$Card$allCards));
	var shuffleModel = _v0.a;
	var shuffleCmd = _v0.b;
	return _Utils_Tuple2(
		{afterAnimationModel: $author$project$ModelHistory$initMoment, distributeModel: $author$project$Distribute$init, doing: $author$project$Main$Shuffling, dragDrop: $norpan$elm_html5_drag_drop$Html5$DragDrop$init, endAnimationModel: $author$project$EndAnimation$init, flyingHomeModel: $author$project$FlyingHome$init, modelHistory: $author$project$ModelHistory$init, shuffleModel: shuffleModel},
		A2($elm$core$Platform$Cmd$map, $author$project$Main$ShuffleMsg, shuffleCmd));
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Main$DistributeMsg = function (a) {
	return {$: 'DistributeMsg', a: a};
};
var $author$project$Main$Distributing = {$: 'Distributing'};
var $author$project$Main$EndAnimationMsg = function (a) {
	return {$: 'EndAnimationMsg', a: a};
};
var $author$project$Main$FlyingHome = {$: 'FlyingHome'};
var $author$project$Main$FlyingHomeMsg = function (a) {
	return {$: 'FlyingHomeMsg', a: a};
};
var $author$project$Main$Playing = {$: 'Playing'};
var $author$project$ModelHistory$addMoment = F2(
	function (modelHistory, modelHistoryMoment) {
		return A2($elm$core$List$cons, modelHistoryMoment, modelHistory);
	});
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $author$project$Card$getRank = function (_v0) {
	var rank = _v0.rank;
	switch (rank.$) {
		case 'Ace':
			return 0;
		case 'N2':
			return 1;
		case 'N3':
			return 2;
		case 'N4':
			return 3;
		case 'N5':
			return 4;
		case 'N6':
			return 5;
		case 'N7':
			return 6;
		case 'N8':
			return 7;
		case 'N9':
			return 8;
		case 'N10':
			return 9;
		case 'Jack':
			return 10;
		case 'Queen':
			return 11;
		default:
			return 12;
	}
};
var $author$project$Card$getSuit = function (_v0) {
	var suit = _v0.suit;
	return suit;
};
var $author$project$Card$cardsSuccessiveHome = F2(
	function (maybeCard, nextCard) {
		if (maybeCard.$ === 'Nothing') {
			return !$author$project$Card$getRank(nextCard);
		} else {
			var card = maybeCard.a;
			return _Utils_eq(
				$author$project$Card$getSuit(card),
				$author$project$Card$getSuit(nextCard)) && _Utils_eq(
				$author$project$Card$getRank(card) + 1,
				$author$project$Card$getRank(nextCard));
		}
	});
var $elm$core$Elm$JsArray$foldl = _JsArray_foldl;
var $elm$core$Array$foldl = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldl, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldl, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldl,
			func,
			A3($elm$core$Elm$JsArray$foldl, helper, baseCase, tree),
			tail);
	});
var $elm$core$Elm$JsArray$indexedMap = _JsArray_indexedMap;
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$indexedMap = F2(
	function (func, _v0) {
		var len = _v0.a;
		var tree = _v0.c;
		var tail = _v0.d;
		var initialBuilder = {
			nodeList: _List_Nil,
			nodeListSize: 0,
			tail: A3(
				$elm$core$Elm$JsArray$indexedMap,
				func,
				$elm$core$Array$tailIndex(len),
				tail)
		};
		var helper = F2(
			function (node, builder) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldl, helper, builder, subTree);
				} else {
					var leaf = node.a;
					var offset = builder.nodeListSize * $elm$core$Array$branchFactor;
					var mappedLeaf = $elm$core$Array$Leaf(
						A3($elm$core$Elm$JsArray$indexedMap, func, offset, leaf));
					return {
						nodeList: A2($elm$core$List$cons, mappedLeaf, builder.nodeList),
						nodeListSize: builder.nodeListSize + 1,
						tail: builder.tail
					};
				}
			});
		return A2(
			$elm$core$Array$builderToArray,
			true,
			A3($elm$core$Elm$JsArray$foldl, helper, initialBuilder, tree));
	});
var $author$project$Home$canReceiveCard = F2(
	function (card, _v0) {
		var homes = _v0.homes;
		return A3(
			$elm$core$Array$foldl,
			F2(
				function (_v1, maybeHomeId) {
					var homeId = _v1.a;
					var maybeCard = _v1.b;
					if (maybeHomeId.$ === 'Nothing') {
						return A2($author$project$Card$cardsSuccessiveHome, maybeCard, card) ? $elm$core$Maybe$Just(homeId) : $elm$core$Maybe$Nothing;
					} else {
						return maybeHomeId;
					}
				}),
			$elm$core$Maybe$Nothing,
			A2(
				$elm$core$Array$indexedMap,
				F2(
					function (homeId, maybeCard) {
						return _Utils_Tuple2(homeId, maybeCard);
					}),
				homes));
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $author$project$Main$dragstart = _Platform_outgoingPort('dragstart', $elm$core$Basics$identity);
var $author$project$FlyingHome$flyingHome = function (_v0) {
	var maybeCard = _v0.maybeCard;
	if (maybeCard.$ === 'Nothing') {
		return false;
	} else {
		return true;
	}
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $author$project$Space$getCard = F2(
	function (spaceId, model) {
		var _v0 = A2($elm$core$Array$get, spaceId, model.spaces);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var maybeCard = _v0.a;
			return maybeCard;
		}
	});
var $author$project$Home$getCoordinates = function (homeIndex) {
	return {x: 50 + (homeIndex * 11), y: 10};
};
var $elm$core$Array$length = function (_v0) {
	var len = _v0.a;
	return len;
};
var $author$project$Pile$getNumberOfCards = F3(
	function (pileIndex, cardIndex, model) {
		var _v0 = A2($elm$core$Array$get, pileIndex, model.piles);
		if (_v0.$ === 'Nothing') {
			return 0;
		} else {
			var pile = _v0.a;
			return $elm$core$Array$length(pile) - cardIndex;
		}
	});
var $author$project$Pile$getCoordinates = F2(
	function (model, pileIndex) {
		return {
			x: (2 + 2) + (pileIndex * 12),
			y: (32 + 2) + (A3($author$project$Pile$getNumberOfCards, pileIndex, 0, model) * 2)
		};
	});
var $author$project$Space$getCoordinates = function (spaceIndex) {
	return {x: 5 + (spaceIndex * 11), y: 4};
};
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$ModelHistory$getCurrent = function (modelHistory) {
	return A2(
		$elm$core$Maybe$withDefault,
		$author$project$ModelHistory$initMoment,
		$elm$core$List$head(modelHistory));
};
var $norpan$elm_html5_drag_drop$Html5$DragDrop$getDragstartEvent = function (msg) {
	if (msg.$ === 'DragStart') {
		var dragId = msg.a;
		var event = msg.b;
		return $elm$core$Maybe$Just(
			{dragId: dragId, event: event});
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$ModelHistory$getHomes = function (modelHistory) {
	return $author$project$ModelHistory$getCurrent(modelHistory).homesModel;
};
var $author$project$ModelHistory$getPiles = function (modelHistory) {
	return $author$project$ModelHistory$getCurrent(modelHistory).pilesModel;
};
var $author$project$ModelHistory$getSpaces = function (modelHistory) {
	return $author$project$ModelHistory$getCurrent(modelHistory).spacesModel;
};
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Pile$getTopCardOfPile = function (pile) {
	return A2(
		$elm$core$Array$get,
		$elm$core$Array$length(pile) - 1,
		pile);
};
var $author$project$Pile$getTopCard = F2(
	function (pileIndex, model) {
		return A2(
			$elm$core$Maybe$andThen,
			$author$project$Pile$getTopCardOfPile,
			A2($elm$core$Array$get, pileIndex, model.piles));
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Elm$JsArray$appendN = _JsArray_appendN;
var $elm$core$Elm$JsArray$slice = _JsArray_slice;
var $elm$core$Array$appendHelpBuilder = F2(
	function (tail, builder) {
		var tailLen = $elm$core$Elm$JsArray$length(tail);
		var notAppended = ($elm$core$Array$branchFactor - $elm$core$Elm$JsArray$length(builder.tail)) - tailLen;
		var appended = A3($elm$core$Elm$JsArray$appendN, $elm$core$Array$branchFactor, builder.tail, tail);
		return (notAppended < 0) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: A3($elm$core$Elm$JsArray$slice, notAppended, tailLen, tail)
		} : ((!notAppended) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: $elm$core$Elm$JsArray$empty
		} : {nodeList: builder.nodeList, nodeListSize: builder.nodeListSize, tail: appended});
	});
var $elm$core$Elm$JsArray$push = _JsArray_push;
var $elm$core$Elm$JsArray$singleton = _JsArray_singleton;
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$insertTailInTree = F4(
	function (shift, index, tail, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		if (_Utils_cmp(
			pos,
			$elm$core$Elm$JsArray$length(tree)) > -1) {
			if (shift === 5) {
				return A2(
					$elm$core$Elm$JsArray$push,
					$elm$core$Array$Leaf(tail),
					tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, $elm$core$Elm$JsArray$empty));
				return A2($elm$core$Elm$JsArray$push, newSub, tree);
			}
		} else {
			var value = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (value.$ === 'SubTree') {
				var subTree = value.a;
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, subTree));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4(
						$elm$core$Array$insertTailInTree,
						shift - $elm$core$Array$shiftStep,
						index,
						tail,
						$elm$core$Elm$JsArray$singleton(value)));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			}
		}
	});
var $elm$core$Array$unsafeReplaceTail = F2(
	function (newTail, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		var originalTailLen = $elm$core$Elm$JsArray$length(tail);
		var newTailLen = $elm$core$Elm$JsArray$length(newTail);
		var newArrayLen = len + (newTailLen - originalTailLen);
		if (_Utils_eq(newTailLen, $elm$core$Array$branchFactor)) {
			var overflow = _Utils_cmp(newArrayLen >>> $elm$core$Array$shiftStep, 1 << startShift) > 0;
			if (overflow) {
				var newShift = startShift + $elm$core$Array$shiftStep;
				var newTree = A4(
					$elm$core$Array$insertTailInTree,
					newShift,
					len,
					newTail,
					$elm$core$Elm$JsArray$singleton(
						$elm$core$Array$SubTree(tree)));
				return A4($elm$core$Array$Array_elm_builtin, newArrayLen, newShift, newTree, $elm$core$Elm$JsArray$empty);
			} else {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					newArrayLen,
					startShift,
					A4($elm$core$Array$insertTailInTree, startShift, len, newTail, tree),
					$elm$core$Elm$JsArray$empty);
			}
		} else {
			return A4($elm$core$Array$Array_elm_builtin, newArrayLen, startShift, tree, newTail);
		}
	});
var $elm$core$Array$appendHelpTree = F2(
	function (toAppend, array) {
		var len = array.a;
		var tree = array.c;
		var tail = array.d;
		var itemsToAppend = $elm$core$Elm$JsArray$length(toAppend);
		var notAppended = ($elm$core$Array$branchFactor - $elm$core$Elm$JsArray$length(tail)) - itemsToAppend;
		var appended = A3($elm$core$Elm$JsArray$appendN, $elm$core$Array$branchFactor, tail, toAppend);
		var newArray = A2($elm$core$Array$unsafeReplaceTail, appended, array);
		if (notAppended < 0) {
			var nextTail = A3($elm$core$Elm$JsArray$slice, notAppended, itemsToAppend, toAppend);
			return A2($elm$core$Array$unsafeReplaceTail, nextTail, newArray);
		} else {
			return newArray;
		}
	});
var $elm$core$Array$builderFromArray = function (_v0) {
	var len = _v0.a;
	var tree = _v0.c;
	var tail = _v0.d;
	var helper = F2(
		function (node, acc) {
			if (node.$ === 'SubTree') {
				var subTree = node.a;
				return A3($elm$core$Elm$JsArray$foldl, helper, acc, subTree);
			} else {
				return A2($elm$core$List$cons, node, acc);
			}
		});
	return {
		nodeList: A3($elm$core$Elm$JsArray$foldl, helper, _List_Nil, tree),
		nodeListSize: (len / $elm$core$Array$branchFactor) | 0,
		tail: tail
	};
};
var $elm$core$Array$append = F2(
	function (a, _v0) {
		var aTail = a.d;
		var bLen = _v0.a;
		var bTree = _v0.c;
		var bTail = _v0.d;
		if (_Utils_cmp(bLen, $elm$core$Array$branchFactor * 4) < 1) {
			var foldHelper = F2(
				function (node, array) {
					if (node.$ === 'SubTree') {
						var tree = node.a;
						return A3($elm$core$Elm$JsArray$foldl, foldHelper, array, tree);
					} else {
						var leaf = node.a;
						return A2($elm$core$Array$appendHelpTree, leaf, array);
					}
				});
			return A2(
				$elm$core$Array$appendHelpTree,
				bTail,
				A3($elm$core$Elm$JsArray$foldl, foldHelper, a, bTree));
		} else {
			var foldHelper = F2(
				function (node, builder) {
					if (node.$ === 'SubTree') {
						var tree = node.a;
						return A3($elm$core$Elm$JsArray$foldl, foldHelper, builder, tree);
					} else {
						var leaf = node.a;
						return A2($elm$core$Array$appendHelpBuilder, leaf, builder);
					}
				});
			return A2(
				$elm$core$Array$builderToArray,
				true,
				A2(
					$elm$core$Array$appendHelpBuilder,
					bTail,
					A3(
						$elm$core$Elm$JsArray$foldl,
						foldHelper,
						$elm$core$Array$builderFromArray(a),
						bTree)));
		}
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$Array$sliceLeft = F2(
	function (from, array) {
		var len = array.a;
		var tree = array.c;
		var tail = array.d;
		if (!from) {
			return array;
		} else {
			if (_Utils_cmp(
				from,
				$elm$core$Array$tailIndex(len)) > -1) {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					len - from,
					$elm$core$Array$shiftStep,
					$elm$core$Elm$JsArray$empty,
					A3(
						$elm$core$Elm$JsArray$slice,
						from - $elm$core$Array$tailIndex(len),
						$elm$core$Elm$JsArray$length(tail),
						tail));
			} else {
				var skipNodes = (from / $elm$core$Array$branchFactor) | 0;
				var helper = F2(
					function (node, acc) {
						if (node.$ === 'SubTree') {
							var subTree = node.a;
							return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
						} else {
							var leaf = node.a;
							return A2($elm$core$List$cons, leaf, acc);
						}
					});
				var leafNodes = A3(
					$elm$core$Elm$JsArray$foldr,
					helper,
					_List_fromArray(
						[tail]),
					tree);
				var nodesToInsert = A2($elm$core$List$drop, skipNodes, leafNodes);
				if (!nodesToInsert.b) {
					return $elm$core$Array$empty;
				} else {
					var head = nodesToInsert.a;
					var rest = nodesToInsert.b;
					var firstSlice = from - (skipNodes * $elm$core$Array$branchFactor);
					var initialBuilder = {
						nodeList: _List_Nil,
						nodeListSize: 0,
						tail: A3(
							$elm$core$Elm$JsArray$slice,
							firstSlice,
							$elm$core$Elm$JsArray$length(head),
							head)
					};
					return A2(
						$elm$core$Array$builderToArray,
						true,
						A3($elm$core$List$foldl, $elm$core$Array$appendHelpBuilder, initialBuilder, rest));
				}
			}
		}
	});
var $elm$core$Array$fetchNewTail = F4(
	function (shift, end, treeEnd, tree) {
		fetchNewTail:
		while (true) {
			var pos = $elm$core$Array$bitMask & (treeEnd >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var sub = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$end = end,
					$temp$treeEnd = treeEnd,
					$temp$tree = sub;
				shift = $temp$shift;
				end = $temp$end;
				treeEnd = $temp$treeEnd;
				tree = $temp$tree;
				continue fetchNewTail;
			} else {
				var values = _v0.a;
				return A3($elm$core$Elm$JsArray$slice, 0, $elm$core$Array$bitMask & end, values);
			}
		}
	});
var $elm$core$Array$hoistTree = F3(
	function (oldShift, newShift, tree) {
		hoistTree:
		while (true) {
			if ((_Utils_cmp(oldShift, newShift) < 1) || (!$elm$core$Elm$JsArray$length(tree))) {
				return tree;
			} else {
				var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, 0, tree);
				if (_v0.$ === 'SubTree') {
					var sub = _v0.a;
					var $temp$oldShift = oldShift - $elm$core$Array$shiftStep,
						$temp$newShift = newShift,
						$temp$tree = sub;
					oldShift = $temp$oldShift;
					newShift = $temp$newShift;
					tree = $temp$tree;
					continue hoistTree;
				} else {
					return tree;
				}
			}
		}
	});
var $elm$core$Array$sliceTree = F3(
	function (shift, endIdx, tree) {
		var lastPos = $elm$core$Array$bitMask & (endIdx >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, lastPos, tree);
		if (_v0.$ === 'SubTree') {
			var sub = _v0.a;
			var newSub = A3($elm$core$Array$sliceTree, shift - $elm$core$Array$shiftStep, endIdx, sub);
			return (!$elm$core$Elm$JsArray$length(newSub)) ? A3($elm$core$Elm$JsArray$slice, 0, lastPos, tree) : A3(
				$elm$core$Elm$JsArray$unsafeSet,
				lastPos,
				$elm$core$Array$SubTree(newSub),
				A3($elm$core$Elm$JsArray$slice, 0, lastPos + 1, tree));
		} else {
			return A3($elm$core$Elm$JsArray$slice, 0, lastPos, tree);
		}
	});
var $elm$core$Array$sliceRight = F2(
	function (end, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		if (_Utils_eq(end, len)) {
			return array;
		} else {
			if (_Utils_cmp(
				end,
				$elm$core$Array$tailIndex(len)) > -1) {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					end,
					startShift,
					tree,
					A3($elm$core$Elm$JsArray$slice, 0, $elm$core$Array$bitMask & end, tail));
			} else {
				var endIdx = $elm$core$Array$tailIndex(end);
				var depth = $elm$core$Basics$floor(
					A2(
						$elm$core$Basics$logBase,
						$elm$core$Array$branchFactor,
						A2($elm$core$Basics$max, 1, endIdx - 1)));
				var newShift = A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep);
				return A4(
					$elm$core$Array$Array_elm_builtin,
					end,
					newShift,
					A3(
						$elm$core$Array$hoistTree,
						startShift,
						newShift,
						A3($elm$core$Array$sliceTree, startShift, endIdx, tree)),
					A4($elm$core$Array$fetchNewTail, startShift, end, endIdx, tree));
			}
		}
	});
var $elm$core$Array$translateIndex = F2(
	function (index, _v0) {
		var len = _v0.a;
		var posIndex = (index < 0) ? (len + index) : index;
		return (posIndex < 0) ? 0 : ((_Utils_cmp(posIndex, len) > 0) ? len : posIndex);
	});
var $elm$core$Array$slice = F3(
	function (from, to, array) {
		var correctTo = A2($elm$core$Array$translateIndex, to, array);
		var correctFrom = A2($elm$core$Array$translateIndex, from, array);
		return (_Utils_cmp(correctFrom, correctTo) > 0) ? $elm$core$Array$empty : A2(
			$elm$core$Array$sliceLeft,
			correctFrom,
			A2($elm$core$Array$sliceRight, correctTo, array));
	});
var $author$project$Pile$moveCard2 = F2(
	function (index, _v0) {
		var pileFrom = _v0.a;
		var pileTo = _v0.b;
		var cards = A3(
			$elm$core$Array$slice,
			index,
			$elm$core$Array$length(pileFrom),
			pileFrom);
		return _Utils_Tuple2(
			A3($elm$core$Array$slice, 0, index, pileFrom),
			A2($elm$core$Array$append, pileTo, cards));
	});
var $elm$core$Array$setHelp = F4(
	function (shift, index, value, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
		if (_v0.$ === 'SubTree') {
			var subTree = _v0.a;
			var newSub = A4($elm$core$Array$setHelp, shift - $elm$core$Array$shiftStep, index, value, subTree);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$SubTree(newSub),
				tree);
		} else {
			var values = _v0.a;
			var newLeaf = A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, values);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$Leaf(newLeaf),
				tree);
		}
	});
var $elm$core$Array$set = F3(
	function (index, value, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? array : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			tree,
			A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, tail)) : A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A4($elm$core$Array$setHelp, startShift, index, value, tree),
			tail));
	});
var $author$project$Pile$moveCard = F4(
	function (pileIndexFrom, index, pileIndexTo, model) {
		if (_Utils_eq(pileIndexFrom, pileIndexTo)) {
			return model;
		} else {
			var _v0 = _Utils_Tuple2(
				A2($elm$core$Array$get, pileIndexFrom, model.piles),
				A2($elm$core$Array$get, pileIndexTo, model.piles));
			if (_v0.a.$ === 'Nothing') {
				var _v1 = _v0.a;
				return model;
			} else {
				if (_v0.b.$ === 'Nothing') {
					var _v2 = _v0.b;
					return model;
				} else {
					var pileFrom = _v0.a.a;
					var pileTo = _v0.b.a;
					var _v3 = A2(
						$author$project$Pile$moveCard2,
						index,
						_Utils_Tuple2(pileFrom, pileTo));
					var pileFrom1 = _v3.a;
					var pileTo1 = _v3.b;
					return _Utils_update(
						model,
						{
							piles: A3(
								$elm$core$Array$set,
								pileIndexTo,
								pileTo1,
								A3($elm$core$Array$set, pileIndexFrom, pileFrom1, model.piles))
						});
				}
			}
		}
	});
var $author$project$Space$moveCard = F3(
	function (spaceFromId, spaceToId, model) {
		var _v0 = A2($elm$core$Array$get, spaceFromId, model.spaces);
		if (_v0.$ === 'Nothing') {
			return model;
		} else {
			var maybeCard = _v0.a;
			return _Utils_update(
				model,
				{
					spaces: A3(
						$elm$core$Array$set,
						spaceToId,
						maybeCard,
						A3($elm$core$Array$set, spaceFromId, $elm$core$Maybe$Nothing, model.spaces))
				});
		}
	});
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$ModelHistory$popHistory = function (modelHistory) {
	var allButOne = $elm$core$List$length(modelHistory) - 1;
	return A2($elm$core$List$drop, allButOne, modelHistory);
};
var $author$project$ModelHistory$popMoment = function (modelHistory) {
	return A2($elm$core$List$drop, 1, modelHistory);
};
var $author$project$Main$EndAnimation = {$: 'EndAnimation'};
var $author$project$Main$FlyingAllHome = {$: 'FlyingAllHome'};
var $author$project$EndAnimation$DoAnimate = {$: 'DoAnimate'};
var $author$project$EndAnimation$animate = A2(
	$elm$core$Task$perform,
	$elm$core$Basics$always($author$project$EndAnimation$DoAnimate),
	$elm$core$Process$sleep(80));
var $elm$core$Basics$not = _Basics_not;
var $author$project$Home$playingDone = function (model) {
	return A3(
		$elm$core$Array$foldl,
		F2(
			function (maybeCard, done) {
				if (maybeCard.$ === 'Nothing') {
					return false;
				} else {
					var card = maybeCard.a;
					return done && _Utils_eq(card.rank, $author$project$Card$King);
				}
			}),
		true,
		model.homes);
};
var $author$project$Pile$playingDonePileHelper = F2(
	function (card, _v0) {
		var okay = _v0.a;
		var maybeLastCard = _v0.b;
		var _v1 = _Utils_Tuple2(okay, maybeLastCard);
		if (!_v1.a) {
			return _Utils_Tuple2(
				false,
				$elm$core$Maybe$Just(card));
		} else {
			if (_v1.b.$ === 'Nothing') {
				var _v2 = _v1.b;
				return _Utils_Tuple2(
					true,
					$elm$core$Maybe$Just(card));
			} else {
				var lastCard = _v1.b.a;
				return _Utils_Tuple2(
					_Utils_cmp(
						$author$project$Card$getRank(card),
						$author$project$Card$getRank(lastCard)) < 1,
					$elm$core$Maybe$Just(card));
			}
		}
	});
var $author$project$Pile$playingDonePile = F2(
	function (cards, okay) {
		return (!okay) ? false : A3(
			$elm$core$Array$foldl,
			$author$project$Pile$playingDonePileHelper,
			_Utils_Tuple2(true, $elm$core$Maybe$Nothing),
			cards).a;
	});
var $author$project$Pile$playingDone = function (_v0) {
	var piles = _v0.piles;
	return A3($elm$core$Array$foldl, $author$project$Pile$playingDonePile, true, piles);
};
var $author$project$ModelHistory$playingDone = function (modelHistory) {
	return $author$project$Pile$playingDone(
		$author$project$ModelHistory$getCurrent(modelHistory).pilesModel);
};
var $author$project$Main$closingTheGame = function (_v0) {
	var modelHistory = _v0.modelHistory;
	return (!$author$project$Home$playingDone(
		$author$project$ModelHistory$getHomes(modelHistory))) && $author$project$ModelHistory$playingDone(modelHistory);
};
var $author$project$Main$SentHomeFromPileMsg = F2(
	function (a, b) {
		return {$: 'SentHomeFromPileMsg', a: a, b: b};
	});
var $author$project$Main$SentHomeFromSpaceMsg = F2(
	function (a, b) {
		return {$: 'SentHomeFromSpaceMsg', a: a, b: b};
	});
var $elm$core$Array$filter = F2(
	function (isGood, array) {
		return $elm$core$Array$fromList(
			A3(
				$elm$core$Array$foldr,
				F2(
					function (x, xs) {
						return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
					}),
				_List_Nil,
				array));
	});
var $author$project$Card$defaultCard = {rank: $author$project$Card$N2, suit: $author$project$Card$Diamonds};
var $elm$core$Elm$JsArray$map = _JsArray_map;
var $elm$core$Array$map = F2(
	function (func, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = function (node) {
			if (node.$ === 'SubTree') {
				var subTree = node.a;
				return $elm$core$Array$SubTree(
					A2($elm$core$Elm$JsArray$map, helper, subTree));
			} else {
				var values = node.a;
				return $elm$core$Array$Leaf(
					A2($elm$core$Elm$JsArray$map, func, values));
			}
		};
		return A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A2($elm$core$Elm$JsArray$map, helper, tree),
			A2($elm$core$Elm$JsArray$map, func, tail));
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$Space$getCards = function (_v0) {
	var spaces = _v0.spaces;
	return A2(
		$elm$core$Array$map,
		function (_v2) {
			var maybeCard = _v2.a;
			var i = _v2.b;
			return _Utils_Tuple2(
				A2($elm$core$Maybe$withDefault, $author$project$Card$defaultCard, maybeCard),
				i);
		},
		A2(
			$elm$core$Array$filter,
			function (_v1) {
				var maybeCard = _v1.a;
				var i = _v1.b;
				return !_Utils_eq(maybeCard, $elm$core$Maybe$Nothing);
			},
			A2(
				$elm$core$Array$indexedMap,
				F2(
					function (i, maybeCard) {
						return _Utils_Tuple2(maybeCard, i);
					}),
				spaces)));
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $author$project$Main$trySendMeHome = F2(
	function (homes, _v0) {
		var card = _v0.a;
		var index = _v0.b;
		var _v1 = A2($author$project$Home$canReceiveCard, card, homes);
		if (_v1.$ === 'Nothing') {
			return _Utils_Tuple2($elm$core$Maybe$Nothing, -1);
		} else {
			return _Utils_Tuple2(
				$elm$core$Maybe$Just(card),
				index);
		}
	});
var $author$project$Main$doSentHomeAllFromSpace = function (modelHistory) {
	var _v0 = $elm$core$Array$toList(
		A2(
			$elm$core$Array$filter,
			function (_v1) {
				var maybeCard = _v1.a;
				return !_Utils_eq(maybeCard, $elm$core$Maybe$Nothing);
			},
			A2(
				$elm$core$Array$map,
				$author$project$Main$trySendMeHome(
					$author$project$ModelHistory$getHomes(modelHistory)),
				$author$project$Space$getCards(
					$author$project$ModelHistory$getSpaces(modelHistory)))));
	if (!_v0.b) {
		return $elm$core$Platform$Cmd$none;
	} else {
		if (_v0.a.a.$ === 'Just') {
			var _v2 = _v0.a;
			var card = _v2.a.a;
			var homeIndex = _v2.b;
			return A2(
				$elm$core$Task$perform,
				$elm$core$Basics$always(
					A2($author$project$Main$SentHomeFromSpaceMsg, homeIndex, card)),
				$elm$core$Process$sleep(40));
		} else {
			var _v3 = _v0.a;
			var _v4 = _v3.a;
			return $elm$core$Platform$Cmd$none;
		}
	}
};
var $author$project$Pile$getTopCards = function (_v0) {
	var piles = _v0.piles;
	return A2(
		$elm$core$Array$map,
		function (_v2) {
			var maybeCard = _v2.a;
			var i = _v2.b;
			return _Utils_Tuple2(
				A2($elm$core$Maybe$withDefault, $author$project$Card$defaultCard, maybeCard),
				i);
		},
		A2(
			$elm$core$Array$filter,
			function (_v1) {
				var maybeCard = _v1.a;
				var i = _v1.b;
				return !_Utils_eq(maybeCard, $elm$core$Maybe$Nothing);
			},
			A2(
				$elm$core$Array$indexedMap,
				F2(
					function (i, maybeCard) {
						return _Utils_Tuple2(maybeCard, i);
					}),
				A2($elm$core$Array$map, $author$project$Pile$getTopCardOfPile, piles))));
};
var $author$project$Main$doSentHomeAll = function (modelHistory) {
	var _v0 = $elm$core$Array$toList(
		A2(
			$elm$core$Array$filter,
			function (_v1) {
				var maybeCard = _v1.a;
				return !_Utils_eq(maybeCard, $elm$core$Maybe$Nothing);
			},
			A2(
				$elm$core$Array$map,
				$author$project$Main$trySendMeHome(
					$author$project$ModelHistory$getHomes(modelHistory)),
				$author$project$Pile$getTopCards(
					$author$project$ModelHistory$getPiles(modelHistory)))));
	if (!_v0.b) {
		return $author$project$Main$doSentHomeAllFromSpace(modelHistory);
	} else {
		if (_v0.a.a.$ === 'Just') {
			var _v2 = _v0.a;
			var card = _v2.a.a;
			var pileIndex = _v2.b;
			return A2(
				$elm$core$Task$perform,
				$elm$core$Basics$always(
					A2($author$project$Main$SentHomeFromPileMsg, pileIndex, card)),
				$elm$core$Process$sleep(40));
		} else {
			var _v3 = _v0.a;
			var _v4 = _v3.a;
			return $author$project$Main$doSentHomeAllFromSpace(modelHistory);
		}
	}
};
var $author$project$Main$possiblyCloseTheGame = function (model) {
	return $author$project$Main$closingTheGame(model) ? _Utils_Tuple2(
		_Utils_update(
			model,
			{doing: $author$project$Main$FlyingAllHome}),
		$author$project$Main$doSentHomeAll(model.modelHistory)) : ($author$project$Home$playingDone(
		$author$project$ModelHistory$getHomes(model.modelHistory)) ? _Utils_Tuple2(
		_Utils_update(
			model,
			{doing: $author$project$Main$EndAnimation}),
		A2($elm$core$Platform$Cmd$map, $author$project$Main$EndAnimationMsg, $author$project$EndAnimation$animate)) : _Utils_Tuple2(model, $elm$core$Platform$Cmd$none));
};
var $author$project$Pile$pullCard = F2(
	function (pileIndex, model) {
		var maybePile = A2(
			$elm$core$Maybe$map,
			A2($elm$core$Array$slice, 0, -1),
			A2($elm$core$Array$get, pileIndex, model.piles));
		if (maybePile.$ === 'Nothing') {
			return model;
		} else {
			var pile = maybePile.a;
			return _Utils_update(
				model,
				{
					piles: A3($elm$core$Array$set, pileIndex, pile, model.piles)
				});
		}
	});
var $author$project$Space$pullCard = F2(
	function (spaceId, model) {
		return _Utils_update(
			model,
			{
				spaces: A3($elm$core$Array$set, spaceId, $elm$core$Maybe$Nothing, model.spaces)
			});
	});
var $author$project$Home$pushCard = F3(
	function (homeId, card, model) {
		return _Utils_update(
			model,
			{
				homes: A3(
					$elm$core$Array$set,
					homeId,
					$elm$core$Maybe$Just(card),
					model.homes)
			});
	});
var $elm$core$Array$push = F2(
	function (a, array) {
		var tail = array.d;
		return A2(
			$elm$core$Array$unsafeReplaceTail,
			A2($elm$core$Elm$JsArray$push, a, tail),
			array);
	});
var $author$project$Pile$pushCard = F3(
	function (pileIndex, card, model) {
		var maybePile = A2(
			$elm$core$Maybe$map,
			$elm$core$Array$push(card),
			A2($elm$core$Array$get, pileIndex, model.piles));
		if (maybePile.$ === 'Nothing') {
			return model;
		} else {
			var pile = maybePile.a;
			return _Utils_update(
				model,
				{
					piles: A3($elm$core$Array$set, pileIndex, pile, model.piles)
				});
		}
	});
var $author$project$Space$pushCard = F3(
	function (spaceId, card, model) {
		return _Utils_update(
			model,
			{
				spaces: A3(
					$elm$core$Array$set,
					spaceId,
					$elm$core$Maybe$Just(card),
					model.spaces)
			});
	});
var $author$project$ModelHistory$setCurrent = F2(
	function (modelHistory, modelHistoryMoment) {
		return A2(
			$elm$core$List$cons,
			modelHistoryMoment,
			A2($elm$core$List$drop, 1, modelHistory));
	});
var $author$project$ModelHistory$setHomes = F2(
	function (homesModel, modelHistoryCurrent) {
		return _Utils_update(
			modelHistoryCurrent,
			{homesModel: homesModel});
	});
var $author$project$ModelHistory$setPiles = F2(
	function (pilesModel, modelHistoryCurrent) {
		return _Utils_update(
			modelHistoryCurrent,
			{pilesModel: pilesModel});
	});
var $author$project$Pile$setPiles = function (piles) {
	return {piles: piles};
};
var $author$project$ModelHistory$setSpaces = F2(
	function (spacesModel, modelHistoryCurrent) {
		return _Utils_update(
			modelHistoryCurrent,
			{spacesModel: spacesModel});
	});
var $author$project$Distribute$DoDistribute = {$: 'DoDistribute'};
var $author$project$Distribute$distribute = function (index) {
	return A2(
		$elm$core$Task$perform,
		$elm$core$Basics$always($author$project$Distribute$DoDistribute),
		$elm$core$Process$sleep(
			(!index) ? 800 : 80));
};
var $author$project$Distribute$start = function (pile) {
	return _Utils_Tuple2(
		{
			distributingDone: false,
			distributorCardIndex: 0,
			pile: pile,
			piles: A2($elm$core$Array$repeat, $author$project$Pile$numberOfPiles, $elm$core$Array$empty)
		},
		$author$project$Distribute$distribute(0));
};
var $author$project$FlyingHome$DoFly = {$: 'DoFly'};
var $author$project$FlyingHome$fly = function (index) {
	return A2(
		$elm$core$Task$perform,
		$elm$core$Basics$always($author$project$FlyingHome$DoFly),
		$elm$core$Process$sleep(25));
};
var $author$project$FlyingHome$start = F3(
	function (card, from, to) {
		return _Utils_Tuple2(
			{
				current: from,
				from: from,
				iteration: 0,
				maybeCard: $elm$core$Maybe$Just(card),
				to: to
			},
			$author$project$FlyingHome$fly(0));
	});
var $author$project$Distribute$distributeCard = F3(
	function (card, piles, pileIndex) {
		var _v0 = A2($elm$core$Array$get, pileIndex, piles);
		if (_v0.$ === 'Nothing') {
			return piles;
		} else {
			var pile = _v0.a;
			var pile1 = A2($elm$core$Array$push, card, pile);
			return A3($elm$core$Array$set, pileIndex, pile1, piles);
		}
	});
var $author$project$Distribute$getCurrentCard = F2(
	function (cards, index) {
		return A2($elm$core$Array$get, index, cards);
	});
var $elm$core$Basics$modBy = _Basics_modBy;
var $author$project$Distribute$update = F2(
	function (_v0, model) {
		var _v1 = model;
		var pile = _v1.pile;
		var piles = _v1.piles;
		var distributorCardIndex = _v1.distributorCardIndex;
		var maybeCard = A2($author$project$Distribute$getCurrentCard, pile, distributorCardIndex);
		var distributorPileIndex = A2(
			$elm$core$Basics$modBy,
			$elm$core$Array$length(piles),
			distributorCardIndex);
		if (maybeCard.$ === 'Nothing') {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{distributingDone: true}),
				$elm$core$Platform$Cmd$none);
		} else {
			var card = maybeCard.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						distributorCardIndex: distributorCardIndex + 1,
						piles: A3($author$project$Distribute$distributeCard, card, piles, distributorPileIndex)
					}),
				$author$project$Distribute$distribute(distributorCardIndex + 1));
		}
	});
var $author$project$Card$getRankFromNumber = function (_int) {
	return A2(
		$elm$core$Array$get,
		_int,
		$elm$core$Array$fromList($author$project$Card$allRanks));
};
var $author$project$Card$getSuitFromNumber = function (_int) {
	return A2(
		$elm$core$Array$get,
		_int,
		$elm$core$Array$fromList($author$project$Card$allSuits));
};
var $author$project$Card$getCardFromNumber = function (_int) {
	var _v0 = _Utils_Tuple2(
		$author$project$Card$getRankFromNumber(
			A2($elm$core$Basics$modBy, 13, _int)),
		$author$project$Card$getSuitFromNumber((_int / 13) | 0));
	if (_v0.a.$ === 'Just') {
		if (_v0.b.$ === 'Just') {
			var rank = _v0.a.a;
			var suit = _v0.b.a;
			return $elm$core$Maybe$Just(
				{rank: rank, suit: suit});
		} else {
			var _v2 = _v0.b;
			return $elm$core$Maybe$Nothing;
		}
	} else {
		var _v1 = _v0.a;
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$EndAnimation$update = F2(
	function (_v0, model) {
		var _v1 = model;
		var cards = _v1.cards;
		var cardIndex = _v1.cardIndex;
		var offset = _v1.offset;
		var cardIndexOffset = cardIndex - offset;
		var maybeCard = (cardIndexOffset < 0) ? $author$project$Card$getCardFromNumber(52 + cardIndexOffset) : $author$project$Card$getCardFromNumber(cardIndexOffset);
		var _v2 = _Utils_Tuple2(offset, maybeCard);
		if (_v2.b.$ === 'Just') {
			if (!_v2.a) {
				var card = _v2.b.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cardIndex: cardIndex + 1,
							cards: A2($elm$core$Array$push, card, model.cards)
						}),
					$author$project$EndAnimation$animate);
			} else {
				var card = _v2.b.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cardIndex: cardIndex + 1,
							cards: A3($elm$core$Array$set, cardIndex, card, model.cards)
						}),
					$author$project$EndAnimation$animate);
			}
		} else {
			var _v3 = _v2.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{cardIndex: 0, offset: model.offset + 1}),
				$author$project$EndAnimation$animate);
		}
	});
var $author$project$FlyingHome$distance = F2(
	function (from, to) {
		return {x: to.x - from.x, y: to.y - from.y};
	});
var $elm$core$Debug$log = _Debug_log;
var $author$project$FlyingHome$move = F2(
	function (from, offset) {
		return {x: from.x + offset.x, y: from.y + offset.y};
	});
var $author$project$FlyingHome$numberOfIterations = 8;
var $author$project$FlyingHome$stepDistance = F3(
	function (step, steps, offset) {
		return {x: ((step * offset.x) / steps) | 0, y: ((step * offset.y) / steps) | 0};
	});
var $author$project$FlyingHome$update = F2(
	function (_v0, model) {
		var _v1 = model;
		var maybeCard = _v1.maybeCard;
		var from = _v1.from;
		var to = _v1.to;
		var current = _v1.current;
		var iteration = _v1.iteration;
		if (maybeCard.$ === 'Nothing') {
			return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		} else {
			var card = maybeCard.a;
			return _Utils_eq(iteration, $author$project$FlyingHome$numberOfIterations + 1) ? _Utils_Tuple2(
				_Utils_update(
					model,
					{maybeCard: $elm$core$Maybe$Nothing}),
				$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
				_Utils_update(
					model,
					{
						current: A2(
							$elm$core$Debug$log,
							'Animate current = ',
							A2(
								$author$project$FlyingHome$move,
								from,
								A3(
									$author$project$FlyingHome$stepDistance,
									iteration,
									$author$project$FlyingHome$numberOfIterations,
									A2($author$project$FlyingHome$distance, from, to)))),
						iteration: iteration + 1
					}),
				$author$project$FlyingHome$fly(iteration));
		}
	});
var $norpan$elm_html5_drag_drop$Html5$DragDrop$DraggedOver = F4(
	function (a, b, c, d) {
		return {$: 'DraggedOver', a: a, b: b, c: c, d: d};
	});
var $norpan$elm_html5_drag_drop$Html5$DragDrop$Dragging = function (a) {
	return {$: 'Dragging', a: a};
};
var $norpan$elm_html5_drag_drop$Html5$DragDrop$updateCommon = F3(
	function (sticky, msg, model) {
		var _v0 = _Utils_Tuple3(msg, model, sticky);
		_v0$9:
		while (true) {
			switch (_v0.a.$) {
				case 'DragStart':
					var _v1 = _v0.a;
					var dragId = _v1.a;
					return _Utils_Tuple2(
						$norpan$elm_html5_drag_drop$Html5$DragDrop$Dragging(dragId),
						$elm$core$Maybe$Nothing);
				case 'DragEnd':
					var _v2 = _v0.a;
					return _Utils_Tuple2($norpan$elm_html5_drag_drop$Html5$DragDrop$NotDragging, $elm$core$Maybe$Nothing);
				case 'DragEnter':
					switch (_v0.b.$) {
						case 'Dragging':
							var dropId = _v0.a.a;
							var dragId = _v0.b.a;
							return _Utils_Tuple2(
								A4($norpan$elm_html5_drag_drop$Html5$DragDrop$DraggedOver, dragId, dropId, 0, $elm$core$Maybe$Nothing),
								$elm$core$Maybe$Nothing);
						case 'DraggedOver':
							var dropId = _v0.a.a;
							var _v3 = _v0.b;
							var dragId = _v3.a;
							var pos = _v3.d;
							return _Utils_Tuple2(
								A4($norpan$elm_html5_drag_drop$Html5$DragDrop$DraggedOver, dragId, dropId, 0, pos),
								$elm$core$Maybe$Nothing);
						default:
							break _v0$9;
					}
				case 'DragLeave':
					if ((_v0.b.$ === 'DraggedOver') && (!_v0.c)) {
						var dropId_ = _v0.a.a;
						var _v4 = _v0.b;
						var dragId = _v4.a;
						var dropId = _v4.b;
						return _Utils_eq(dropId_, dropId) ? _Utils_Tuple2(
							$norpan$elm_html5_drag_drop$Html5$DragDrop$Dragging(dragId),
							$elm$core$Maybe$Nothing) : _Utils_Tuple2(model, $elm$core$Maybe$Nothing);
					} else {
						break _v0$9;
					}
				case 'DragOver':
					switch (_v0.b.$) {
						case 'Dragging':
							var _v5 = _v0.a;
							var dropId = _v5.a;
							var timeStamp = _v5.b;
							var pos = _v5.c;
							var dragId = _v0.b.a;
							return _Utils_Tuple2(
								A4(
									$norpan$elm_html5_drag_drop$Html5$DragDrop$DraggedOver,
									dragId,
									dropId,
									timeStamp,
									$elm$core$Maybe$Just(pos)),
								$elm$core$Maybe$Nothing);
						case 'DraggedOver':
							var _v6 = _v0.a;
							var dropId = _v6.a;
							var timeStamp = _v6.b;
							var pos = _v6.c;
							var _v7 = _v0.b;
							var dragId = _v7.a;
							var currentDropId = _v7.b;
							var currentTimeStamp = _v7.c;
							var currentPos = _v7.d;
							return _Utils_eq(timeStamp, currentTimeStamp) ? _Utils_Tuple2(model, $elm$core$Maybe$Nothing) : _Utils_Tuple2(
								A4(
									$norpan$elm_html5_drag_drop$Html5$DragDrop$DraggedOver,
									dragId,
									dropId,
									timeStamp,
									$elm$core$Maybe$Just(pos)),
								$elm$core$Maybe$Nothing);
						default:
							break _v0$9;
					}
				default:
					switch (_v0.b.$) {
						case 'Dragging':
							var _v8 = _v0.a;
							var dropId = _v8.a;
							var pos = _v8.b;
							var dragId = _v0.b.a;
							return _Utils_Tuple2(
								$norpan$elm_html5_drag_drop$Html5$DragDrop$NotDragging,
								$elm$core$Maybe$Just(
									_Utils_Tuple3(dragId, dropId, pos)));
						case 'DraggedOver':
							var _v9 = _v0.a;
							var dropId = _v9.a;
							var pos = _v9.b;
							var _v10 = _v0.b;
							var dragId = _v10.a;
							return _Utils_Tuple2(
								$norpan$elm_html5_drag_drop$Html5$DragDrop$NotDragging,
								$elm$core$Maybe$Just(
									_Utils_Tuple3(dragId, dropId, pos)));
						default:
							break _v0$9;
					}
			}
		}
		return _Utils_Tuple2(model, $elm$core$Maybe$Nothing);
	});
var $norpan$elm_html5_drag_drop$Html5$DragDrop$update = $norpan$elm_html5_drag_drop$Html5$DragDrop$updateCommon(false);
var $author$project$Shuffle$Shuffle = function (a) {
	return {$: 'Shuffle', a: a};
};
var $elm$random$Random$Generate = function (a) {
	return {$: 'Generate', a: a};
};
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$random$Random$init = A2(
	$elm$core$Task$andThen,
	function (time) {
		return $elm$core$Task$succeed(
			$elm$random$Random$initialSeed(
				$elm$time$Time$posixToMillis(time)));
	},
	$elm$time$Time$now);
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $elm$random$Random$onEffects = F3(
	function (router, commands, seed) {
		if (!commands.b) {
			return $elm$core$Task$succeed(seed);
		} else {
			var generator = commands.a.a;
			var rest = commands.b;
			var _v1 = A2($elm$random$Random$step, generator, seed);
			var value = _v1.a;
			var newSeed = _v1.b;
			return A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$random$Random$onEffects, router, rest, newSeed);
				},
				A2($elm$core$Platform$sendToApp, router, value));
		}
	});
var $elm$random$Random$onSelfMsg = F3(
	function (_v0, _v1, seed) {
		return $elm$core$Task$succeed(seed);
	});
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v1 = genA(seed0);
				var a = _v1.a;
				var seed1 = _v1.b;
				return _Utils_Tuple2(
					func(a),
					seed1);
			});
	});
var $elm$random$Random$cmdMap = F2(
	function (func, _v0) {
		var generator = _v0.a;
		return $elm$random$Random$Generate(
			A2($elm$random$Random$map, func, generator));
	});
_Platform_effectManagers['Random'] = _Platform_createManager($elm$random$Random$init, $elm$random$Random$onEffects, $elm$random$Random$onSelfMsg, $elm$random$Random$cmdMap);
var $elm$random$Random$command = _Platform_leaf('Random');
var $elm$random$Random$generate = F2(
	function (tagger, generator) {
		return $elm$random$Random$command(
			$elm$random$Random$Generate(
				A2($elm$random$Random$map, tagger, generator)));
	});
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$int = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
				var lo = _v0.a;
				var hi = _v0.b;
				var range = (hi - lo) + 1;
				if (!((range - 1) & range)) {
					return _Utils_Tuple2(
						(((range - 1) & $elm$random$Random$peel(seed0)) >>> 0) + lo,
						$elm$random$Random$next(seed0));
				} else {
					var threshhold = (((-range) >>> 0) % range) >>> 0;
					var accountForBias = function (seed) {
						accountForBias:
						while (true) {
							var x = $elm$random$Random$peel(seed);
							var seedN = $elm$random$Random$next(seed);
							if (_Utils_cmp(x, threshhold) < 0) {
								var $temp$seed = seedN;
								seed = $temp$seed;
								continue accountForBias;
							} else {
								return _Utils_Tuple2((x % range) + lo, seedN);
							}
						}
					};
					return accountForBias(seed0);
				}
			});
	});
var $author$project$Shuffle$shuffleOverhand = F2(
	function (pile, numberOfCards) {
		return A2(
			$elm$core$Array$append,
			A3(
				$elm$core$Array$slice,
				numberOfCards,
				$elm$core$Array$length(pile),
				pile),
			$elm$core$Array$fromList(
				$elm$core$List$reverse(
					$elm$core$Array$toList(
						A3($elm$core$Array$slice, 0, numberOfCards, pile)))));
	});
var $author$project$Shuffle$shuffleTimesTodo = 30;
var $author$project$Shuffle$update = F2(
	function (msg, model) {
		if (msg.$ === 'DoRandom') {
			return _Utils_Tuple2(
				model,
				A2(
					$elm$random$Random$generate,
					$author$project$Shuffle$Shuffle,
					A2(
						$elm$random$Random$int,
						1,
						$elm$core$Array$length(model.pile) - 1)));
		} else {
			var random = msg.a;
			var shuffledTimes1 = model.shuffledTimes + 1;
			var shufflingDone1 = _Utils_cmp(shuffledTimes1, $author$project$Shuffle$shuffleTimesTodo) > -1;
			var pile1 = A2($author$project$Shuffle$shuffleOverhand, model.pile, random);
			return _Utils_Tuple2(
				{pile: pile1, shuffledTimes: shuffledTimes1, shufflingDone: shufflingDone1},
				(!shufflingDone1) ? $author$project$Shuffle$randomShuffle(shuffledTimes1) : $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		var _v0 = _Utils_Tuple2(model.doing, msg);
		_v0$1:
		while (true) {
			_v0$3:
			while (true) {
				_v0$5:
				while (true) {
					_v0$6:
					while (true) {
						_v0$7:
						while (true) {
							_v0$11:
							while (true) {
								_v0$13:
								while (true) {
									switch (_v0.a.$) {
										case 'Shuffling':
											switch (_v0.b.$) {
												case 'ShuffleMsg':
													var _v1 = _v0.a;
													var shuffleMsg = _v0.b.a;
													var _v2 = A2($author$project$Shuffle$update, shuffleMsg, model.shuffleModel);
													var shuffleModel = _v2.a;
													var shuffleCmd = _v2.b;
													if (shuffleModel.shufflingDone) {
														var _v3 = $author$project$Distribute$start(model.shuffleModel.pile);
														var distributeModel = _v3.a;
														var distributeCmd = _v3.b;
														return _Utils_Tuple2(
															_Utils_update(
																model,
																{distributeModel: distributeModel, doing: $author$project$Main$Distributing, shuffleModel: shuffleModel}),
															A2($elm$core$Platform$Cmd$map, $author$project$Main$DistributeMsg, distributeCmd));
													} else {
														return _Utils_Tuple2(
															_Utils_update(
																model,
																{shuffleModel: shuffleModel}),
															A2($elm$core$Platform$Cmd$map, $author$project$Main$ShuffleMsg, shuffleCmd));
													}
												case 'ButtonsMsg':
													break _v0$1;
												default:
													break _v0$1;
											}
										case 'Distributing':
											switch (_v0.b.$) {
												case 'DistributeMsg':
													var _v5 = _v0.a;
													var distributeMsg = _v0.b.a;
													var _v6 = A2($author$project$Distribute$update, distributeMsg, model.distributeModel);
													var distributeModel = _v6.a;
													var distributeCmd = _v6.b;
													return distributeModel.distributingDone ? _Utils_Tuple2(
														_Utils_update(
															model,
															{
																distributeModel: distributeModel,
																doing: $author$project$Main$Playing,
																modelHistory: A2(
																	$author$project$ModelHistory$setCurrent,
																	model.modelHistory,
																	A2(
																		$author$project$ModelHistory$setPiles,
																		$author$project$Pile$setPiles(distributeModel.piles),
																		$author$project$ModelHistory$getCurrent(model.modelHistory)))
															}),
														$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
														_Utils_update(
															model,
															{
																distributeModel: distributeModel,
																modelHistory: A2(
																	$author$project$ModelHistory$setCurrent,
																	model.modelHistory,
																	A2(
																		$author$project$ModelHistory$setPiles,
																		$author$project$Pile$setPiles(distributeModel.piles),
																		$author$project$ModelHistory$getCurrent(model.modelHistory)))
															}),
														A2($elm$core$Platform$Cmd$map, $author$project$Main$DistributeMsg, distributeCmd));
												case 'ButtonsMsg':
													break _v0$3;
												default:
													break _v0$3;
											}
										case 'FlyingHome':
											switch (_v0.b.$) {
												case 'FlyingHomeMsg':
													var _v8 = _v0.a;
													var flyingHomeMsg = _v0.b.a;
													var _v9 = A2($author$project$FlyingHome$update, flyingHomeMsg, model.flyingHomeModel);
													var flyingHomeModel = _v9.a;
													var flyingHomeCmd = _v9.b;
													return $author$project$FlyingHome$flyingHome(flyingHomeModel) ? _Utils_Tuple2(
														_Utils_update(
															model,
															{flyingHomeModel: flyingHomeModel}),
														A2($elm$core$Platform$Cmd$map, $author$project$Main$FlyingHomeMsg, flyingHomeCmd)) : $author$project$Main$possiblyCloseTheGame(
														_Utils_update(
															model,
															{
																doing: $author$project$Main$Playing,
																flyingHomeModel: flyingHomeModel,
																modelHistory: A2($author$project$ModelHistory$setCurrent, model.modelHistory, model.afterAnimationModel)
															}));
												case 'ButtonsMsg':
													break _v0$5;
												default:
													break _v0$5;
											}
										case 'FlyingAllHome':
											switch (_v0.b.$) {
												case 'ButtonsMsg':
													switch (_v0.b.a.$) {
														case 'NewClicked':
															break _v0$6;
														case 'RestartClicked':
															break _v0$7;
														default:
															break _v0$11;
													}
												case 'FlyingHomeMsg':
													var _v13 = _v0.a;
													var flyingHomeMsg = _v0.b.a;
													var _v14 = A2($author$project$FlyingHome$update, flyingHomeMsg, model.flyingHomeModel);
													var flyingHomeModel = _v14.a;
													var flyingHomeCmd = _v14.b;
													return $author$project$FlyingHome$flyingHome(flyingHomeModel) ? _Utils_Tuple2(
														_Utils_update(
															model,
															{flyingHomeModel: flyingHomeModel}),
														A2($elm$core$Platform$Cmd$map, $author$project$Main$FlyingHomeMsg, flyingHomeCmd)) : $author$project$Main$possiblyCloseTheGame(
														_Utils_update(
															model,
															{
																doing: $author$project$Main$Playing,
																flyingHomeModel: flyingHomeModel,
																modelHistory: A2($author$project$ModelHistory$setCurrent, model.modelHistory, model.afterAnimationModel)
															}));
												case 'SentHomeFromPileMsg':
													var _v15 = _v0.a;
													var _v16 = _v0.b;
													var pileIndex = _v16.a;
													var card = _v16.b;
													var _v17 = A2(
														$author$project$Home$canReceiveCard,
														card,
														$author$project$ModelHistory$getHomes(model.modelHistory));
													if (_v17.$ === 'Nothing') {
														return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
													} else {
														var index = _v17.a;
														var _v18 = A3(
															$author$project$FlyingHome$start,
															card,
															A2(
																$author$project$Pile$getCoordinates,
																$author$project$ModelHistory$getPiles(model.modelHistory),
																pileIndex),
															$author$project$Home$getCoordinates(index));
														var flyingHomeModel = _v18.a;
														var flyingHomeCmd = _v18.b;
														return _Utils_Tuple2(
															_Utils_update(
																model,
																{
																	afterAnimationModel: A2(
																		$author$project$ModelHistory$setHomes,
																		A3(
																			$author$project$Home$pushCard,
																			index,
																			card,
																			$author$project$ModelHistory$getHomes(model.modelHistory)),
																		A2(
																			$author$project$ModelHistory$setPiles,
																			A2(
																				$author$project$Pile$pullCard,
																				pileIndex,
																				$author$project$ModelHistory$getPiles(model.modelHistory)),
																			$author$project$ModelHistory$getCurrent(model.modelHistory))),
																	flyingHomeModel: flyingHomeModel,
																	modelHistory: A2(
																		$author$project$ModelHistory$addMoment,
																		model.modelHistory,
																		A2(
																			$author$project$ModelHistory$setPiles,
																			A2(
																				$author$project$Pile$pullCard,
																				pileIndex,
																				$author$project$ModelHistory$getPiles(model.modelHistory)),
																			$author$project$ModelHistory$getCurrent(model.modelHistory)))
																}),
															A2($elm$core$Platform$Cmd$map, $author$project$Main$FlyingHomeMsg, flyingHomeCmd));
													}
												case 'SentHomeFromSpaceMsg':
													var _v19 = _v0.a;
													var _v20 = _v0.b;
													var spaceIndex = _v20.a;
													var card = _v20.b;
													var _v21 = A2(
														$author$project$Home$canReceiveCard,
														card,
														$author$project$ModelHistory$getHomes(model.modelHistory));
													if (_v21.$ === 'Nothing') {
														return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
													} else {
														var index = _v21.a;
														var _v22 = A3(
															$author$project$FlyingHome$start,
															card,
															$author$project$Space$getCoordinates(spaceIndex),
															$author$project$Home$getCoordinates(index));
														var flyingHomeModel = _v22.a;
														var flyingHomeCmd = _v22.b;
														return _Utils_Tuple2(
															_Utils_update(
																model,
																{
																	afterAnimationModel: A2(
																		$author$project$ModelHistory$setHomes,
																		A3(
																			$author$project$Home$pushCard,
																			index,
																			card,
																			$author$project$ModelHistory$getHomes(model.modelHistory)),
																		A2(
																			$author$project$ModelHistory$setSpaces,
																			A2(
																				$author$project$Space$pullCard,
																				spaceIndex,
																				$author$project$ModelHistory$getSpaces(model.modelHistory)),
																			$author$project$ModelHistory$getCurrent(model.modelHistory))),
																	flyingHomeModel: flyingHomeModel,
																	modelHistory: A2(
																		$author$project$ModelHistory$addMoment,
																		model.modelHistory,
																		A2(
																			$author$project$ModelHistory$setSpaces,
																			A2(
																				$author$project$Space$pullCard,
																				spaceIndex,
																				$author$project$ModelHistory$getSpaces(model.modelHistory)),
																			$author$project$ModelHistory$getCurrent(model.modelHistory)))
																}),
															A2($elm$core$Platform$Cmd$map, $author$project$Main$FlyingHomeMsg, flyingHomeCmd));
													}
												default:
													break _v0$11;
											}
										case 'EndAnimation':
											switch (_v0.b.$) {
												case 'ButtonsMsg':
													switch (_v0.b.a.$) {
														case 'NewClicked':
															break _v0$6;
														case 'RestartClicked':
															break _v0$7;
														default:
															break _v0$13;
													}
												case 'EndAnimationMsg':
													var _v24 = _v0.a;
													var endAnimationMsg = _v0.b.a;
													var _v25 = A2($author$project$EndAnimation$update, endAnimationMsg, model.endAnimationModel);
													var endAnimationModel = _v25.a;
													var endAnimationCmd = _v25.b;
													return _Utils_Tuple2(
														_Utils_update(
															model,
															{endAnimationModel: endAnimationModel}),
														A2($elm$core$Platform$Cmd$map, $author$project$Main$EndAnimationMsg, endAnimationCmd));
												default:
													break _v0$13;
											}
										default:
											switch (_v0.b.$) {
												case 'DragDropMsg':
													var _v27 = _v0.a;
													var msg_ = _v0.b.a;
													var _v28 = A2($norpan$elm_html5_drag_drop$Html5$DragDrop$update, msg_, model.dragDrop);
													var dragDropModel = _v28.a;
													var dragDropResult = _v28.b;
													var _v29 = function () {
														if (dragDropResult.$ === 'Nothing') {
															return _Utils_Tuple2(
																_Utils_update(
																	model,
																	{dragDrop: dragDropModel}),
																$elm$core$Platform$Cmd$none);
														} else {
															if (dragDropResult.a.a.$ === 'PileFrom') {
																switch (dragDropResult.a.b.$) {
																	case 'PileTo':
																		var _v31 = dragDropResult.a;
																		var _v32 = _v31.a;
																		var pileFromId = _v32.a;
																		var cardFromId = _v32.b;
																		var pileToId = _v31.b.a;
																		return $author$project$Main$possiblyCloseTheGame(
																			_Utils_update(
																				model,
																				{
																					dragDrop: dragDropModel,
																					modelHistory: A2(
																						$author$project$ModelHistory$addMoment,
																						model.modelHistory,
																						A2(
																							$author$project$ModelHistory$setPiles,
																							A4(
																								$author$project$Pile$moveCard,
																								pileFromId,
																								cardFromId,
																								pileToId,
																								$author$project$ModelHistory$getPiles(model.modelHistory)),
																							$author$project$ModelHistory$getCurrent(model.modelHistory)))
																				}));
																	case 'SpaceTo':
																		var _v33 = dragDropResult.a;
																		var _v34 = _v33.a;
																		var pileFromId = _v34.a;
																		var spaceId = _v33.b.a;
																		var _v35 = A2(
																			$author$project$Pile$getTopCard,
																			pileFromId,
																			$author$project$ModelHistory$getPiles(model.modelHistory));
																		if (_v35.$ === 'Nothing') {
																			return _Utils_Tuple2(
																				_Utils_update(
																					model,
																					{dragDrop: dragDropModel}),
																				$elm$core$Platform$Cmd$none);
																		} else {
																			var card = _v35.a;
																			return $author$project$Main$possiblyCloseTheGame(
																				_Utils_update(
																					model,
																					{
																						dragDrop: dragDropModel,
																						modelHistory: A2(
																							$author$project$ModelHistory$addMoment,
																							model.modelHistory,
																							A2(
																								$author$project$ModelHistory$setPiles,
																								A2(
																									$author$project$Pile$pullCard,
																									pileFromId,
																									$author$project$ModelHistory$getPiles(model.modelHistory)),
																								A2(
																									$author$project$ModelHistory$setSpaces,
																									A3(
																										$author$project$Space$pushCard,
																										spaceId,
																										card,
																										$author$project$ModelHistory$getSpaces(model.modelHistory)),
																									$author$project$ModelHistory$getCurrent(model.modelHistory))))
																					}));
																		}
																	default:
																		var _v39 = dragDropResult.a;
																		var _v40 = _v39.a;
																		var pileFromId = _v40.a;
																		var homeId = _v39.b.a;
																		var _v41 = A2(
																			$author$project$Pile$getTopCard,
																			pileFromId,
																			$author$project$ModelHistory$getPiles(model.modelHistory));
																		if (_v41.$ === 'Nothing') {
																			return _Utils_Tuple2(
																				_Utils_update(
																					model,
																					{dragDrop: dragDropModel}),
																				$elm$core$Platform$Cmd$none);
																		} else {
																			var card = _v41.a;
																			return $author$project$Main$possiblyCloseTheGame(
																				_Utils_update(
																					model,
																					{
																						dragDrop: dragDropModel,
																						modelHistory: A2(
																							$author$project$ModelHistory$addMoment,
																							model.modelHistory,
																							A2(
																								$author$project$ModelHistory$setPiles,
																								A2(
																									$author$project$Pile$pullCard,
																									pileFromId,
																									$author$project$ModelHistory$getPiles(model.modelHistory)),
																								A2(
																									$author$project$ModelHistory$setHomes,
																									A3(
																										$author$project$Home$pushCard,
																										homeId,
																										card,
																										$author$project$ModelHistory$getHomes(model.modelHistory)),
																									$author$project$ModelHistory$getCurrent(model.modelHistory))))
																					}));
																		}
																}
															} else {
																switch (dragDropResult.a.b.$) {
																	case 'SpaceTo':
																		var _v36 = dragDropResult.a;
																		var spaceFromId = _v36.a.a;
																		var spaceToId = _v36.b.a;
																		return _Utils_Tuple2(
																			_Utils_update(
																				model,
																				{
																					dragDrop: dragDropModel,
																					modelHistory: A2(
																						$author$project$ModelHistory$addMoment,
																						model.modelHistory,
																						A2(
																							$author$project$ModelHistory$setSpaces,
																							A3(
																								$author$project$Space$moveCard,
																								spaceFromId,
																								spaceToId,
																								$author$project$ModelHistory$getSpaces(model.modelHistory)),
																							$author$project$ModelHistory$getCurrent(model.modelHistory)))
																				}),
																			$elm$core$Platform$Cmd$none);
																	case 'PileTo':
																		var _v37 = dragDropResult.a;
																		var spaceFromId = _v37.a.a;
																		var pileToId = _v37.b.a;
																		var _v38 = A2(
																			$author$project$Space$getCard,
																			spaceFromId,
																			$author$project$ModelHistory$getSpaces(model.modelHistory));
																		if (_v38.$ === 'Nothing') {
																			return _Utils_Tuple2(
																				_Utils_update(
																					model,
																					{dragDrop: dragDropModel}),
																				$elm$core$Platform$Cmd$none);
																		} else {
																			var card = _v38.a;
																			return _Utils_Tuple2(
																				_Utils_update(
																					model,
																					{
																						dragDrop: dragDropModel,
																						modelHistory: A2(
																							$author$project$ModelHistory$addMoment,
																							model.modelHistory,
																							A2(
																								$author$project$ModelHistory$setPiles,
																								A3(
																									$author$project$Pile$pushCard,
																									pileToId,
																									card,
																									$author$project$ModelHistory$getPiles(model.modelHistory)),
																								A2(
																									$author$project$ModelHistory$setSpaces,
																									A2(
																										$author$project$Space$pullCard,
																										spaceFromId,
																										$author$project$ModelHistory$getSpaces(model.modelHistory)),
																									$author$project$ModelHistory$getCurrent(model.modelHistory))))
																					}),
																				$elm$core$Platform$Cmd$none);
																		}
																	default:
																		var _v42 = dragDropResult.a;
																		var spaceFromId = _v42.a.a;
																		var homeId = _v42.b.a;
																		var _v43 = A2(
																			$author$project$Space$getCard,
																			spaceFromId,
																			$author$project$ModelHistory$getSpaces(model.modelHistory));
																		if (_v43.$ === 'Nothing') {
																			return _Utils_Tuple2(
																				_Utils_update(
																					model,
																					{dragDrop: dragDropModel}),
																				$elm$core$Platform$Cmd$none);
																		} else {
																			var card = _v43.a;
																			return _Utils_Tuple2(
																				_Utils_update(
																					model,
																					{
																						dragDrop: dragDropModel,
																						modelHistory: A2(
																							$author$project$ModelHistory$addMoment,
																							model.modelHistory,
																							A2(
																								$author$project$ModelHistory$setHomes,
																								A3(
																									$author$project$Home$pushCard,
																									homeId,
																									card,
																									$author$project$ModelHistory$getHomes(model.modelHistory)),
																								A2(
																									$author$project$ModelHistory$setSpaces,
																									A2(
																										$author$project$Space$pullCard,
																										spaceFromId,
																										$author$project$ModelHistory$getSpaces(model.modelHistory)),
																									$author$project$ModelHistory$getCurrent(model.modelHistory))))
																					}),
																				$elm$core$Platform$Cmd$none);
																		}
																}
															}
														}
													}();
													var model1 = _v29.a;
													var cmd = _v29.b;
													return _Utils_Tuple2(
														model1,
														$elm$core$Platform$Cmd$batch(
															_List_fromArray(
																[
																	cmd,
																	A2(
																	$elm$core$Maybe$withDefault,
																	$elm$core$Platform$Cmd$none,
																	A2(
																		$elm$core$Maybe$map,
																		A2(
																			$elm$core$Basics$composeR,
																			function ($) {
																				return $.event;
																			},
																			$author$project$Main$dragstart),
																		$norpan$elm_html5_drag_drop$Html5$DragDrop$getDragstartEvent(msg_)))
																])));
												case 'SentHomeFromPileMsg':
													var _v44 = _v0.a;
													var _v45 = _v0.b;
													var pileIndex = _v45.a;
													var card = _v45.b;
													var _v46 = A2(
														$author$project$Home$canReceiveCard,
														card,
														$author$project$ModelHistory$getHomes(model.modelHistory));
													if (_v46.$ === 'Nothing') {
														return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
													} else {
														var index = _v46.a;
														var _v47 = A3(
															$author$project$FlyingHome$start,
															card,
															A2(
																$author$project$Pile$getCoordinates,
																$author$project$ModelHistory$getPiles(model.modelHistory),
																pileIndex),
															$author$project$Home$getCoordinates(index));
														var flyingHomeModel = _v47.a;
														var flyingHomeCmd = _v47.b;
														return _Utils_Tuple2(
															_Utils_update(
																model,
																{
																	afterAnimationModel: A2(
																		$author$project$ModelHistory$setHomes,
																		A3(
																			$author$project$Home$pushCard,
																			index,
																			card,
																			$author$project$ModelHistory$getHomes(model.modelHistory)),
																		A2(
																			$author$project$ModelHistory$setPiles,
																			A2(
																				$author$project$Pile$pullCard,
																				pileIndex,
																				$author$project$ModelHistory$getPiles(model.modelHistory)),
																			$author$project$ModelHistory$getCurrent(model.modelHistory))),
																	doing: $author$project$Main$FlyingHome,
																	flyingHomeModel: flyingHomeModel,
																	modelHistory: A2(
																		$author$project$ModelHistory$addMoment,
																		model.modelHistory,
																		A2(
																			$author$project$ModelHistory$setPiles,
																			A2(
																				$author$project$Pile$pullCard,
																				pileIndex,
																				$author$project$ModelHistory$getPiles(model.modelHistory)),
																			$author$project$ModelHistory$getCurrent(model.modelHistory)))
																}),
															A2($elm$core$Platform$Cmd$map, $author$project$Main$FlyingHomeMsg, flyingHomeCmd));
													}
												case 'SentHomeFromSpaceMsg':
													var _v48 = _v0.a;
													var _v49 = _v0.b;
													var spaceIndex = _v49.a;
													var card = _v49.b;
													var _v50 = A2(
														$author$project$Home$canReceiveCard,
														card,
														$author$project$ModelHistory$getHomes(model.modelHistory));
													if (_v50.$ === 'Nothing') {
														return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
													} else {
														var index = _v50.a;
														var _v51 = A3(
															$author$project$FlyingHome$start,
															card,
															$author$project$Space$getCoordinates(spaceIndex),
															$author$project$Home$getCoordinates(index));
														var flyingHomeModel = _v51.a;
														var flyingHomeCmd = _v51.b;
														return _Utils_Tuple2(
															_Utils_update(
																model,
																{
																	afterAnimationModel: A2(
																		$author$project$ModelHistory$setHomes,
																		A3(
																			$author$project$Home$pushCard,
																			index,
																			card,
																			$author$project$ModelHistory$getHomes(model.modelHistory)),
																		A2(
																			$author$project$ModelHistory$setSpaces,
																			A2(
																				$author$project$Space$pullCard,
																				spaceIndex,
																				$author$project$ModelHistory$getSpaces(model.modelHistory)),
																			$author$project$ModelHistory$getCurrent(model.modelHistory))),
																	doing: $author$project$Main$FlyingHome,
																	flyingHomeModel: flyingHomeModel,
																	modelHistory: A2(
																		$author$project$ModelHistory$addMoment,
																		model.modelHistory,
																		A2(
																			$author$project$ModelHistory$setSpaces,
																			A2(
																				$author$project$Space$pullCard,
																				spaceIndex,
																				$author$project$ModelHistory$getSpaces(model.modelHistory)),
																			$author$project$ModelHistory$getCurrent(model.modelHistory)))
																}),
															A2($elm$core$Platform$Cmd$map, $author$project$Main$FlyingHomeMsg, flyingHomeCmd));
													}
												case 'ButtonsMsg':
													switch (_v0.b.a.$) {
														case 'NewClicked':
															break _v0$6;
														case 'RestartClicked':
															break _v0$7;
														default:
															var _v52 = _v0.a;
															var _v53 = _v0.b.a;
															return _Utils_Tuple2(
																_Utils_update(
																	model,
																	{
																		modelHistory: $author$project$ModelHistory$popMoment(model.modelHistory)
																	}),
																$elm$core$Platform$Cmd$none);
													}
												default:
													return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
											}
									}
								}
								var _v26 = _v0.a;
								return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
							}
							var _v23 = _v0.a;
							return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
						}
						var _v12 = _v0.b.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									endAnimationModel: $author$project$EndAnimation$init,
									modelHistory: $author$project$ModelHistory$popHistory(model.modelHistory)
								}),
							$elm$core$Platform$Cmd$none);
					}
					var _v11 = _v0.b.a;
					return $author$project$Main$init('new');
				}
				var _v10 = _v0.a;
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			}
			var _v7 = _v0.a;
			return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
		var _v4 = _v0.a;
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $author$project$Main$ButtonsMsg = function (a) {
	return {$: 'ButtonsMsg', a: a};
};
var $norpan$elm_html5_drag_drop$Html5$DragDrop$getDragId = function (model) {
	switch (model.$) {
		case 'NotDragging':
			return $elm$core$Maybe$Nothing;
		case 'Dragging':
			var dragId = model.a;
			return $elm$core$Maybe$Just(dragId);
		default:
			var dragId = model.a;
			var dropId = model.b;
			return $elm$core$Maybe$Just(dragId);
	}
};
var $author$project$ModelHistory$hasHistory = function (modelHistory) {
	return $elm$core$List$length(modelHistory) > 1;
};
var $author$project$Main$DragDropMsg = function (a) {
	return {$: 'DragDropMsg', a: a};
};
var $author$project$Main$HomeTo = function (a) {
	return {$: 'HomeTo', a: a};
};
var $norpan$elm_html5_drag_drop$Html5$DragDrop$DragEnter = function (a) {
	return {$: 'DragEnter', a: a};
};
var $norpan$elm_html5_drag_drop$Html5$DragDrop$DragLeave = function (a) {
	return {$: 'DragLeave', a: a};
};
var $norpan$elm_html5_drag_drop$Html5$DragDrop$DragOver = F3(
	function (a, b, c) {
		return {$: 'DragOver', a: a, b: b, c: c};
	});
var $norpan$elm_html5_drag_drop$Html5$DragDrop$Drop = F2(
	function (a, b) {
		return {$: 'Drop', a: a, b: b};
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$virtual_dom$VirtualDom$Custom = function (a) {
	return {$: 'Custom', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$custom = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Custom(decoder));
	});
var $norpan$elm_html5_drag_drop$Html5$DragDrop$onWithOptions = F3(
	function (name, _v0, decoder) {
		var stopPropagation = _v0.stopPropagation;
		var preventDefault = _v0.preventDefault;
		return A2(
			$elm$html$Html$Events$custom,
			name,
			A2(
				$elm$json$Json$Decode$map,
				function (msg) {
					return {message: msg, preventDefault: preventDefault, stopPropagation: stopPropagation};
				},
				decoder));
	});
var $norpan$elm_html5_drag_drop$Html5$DragDrop$Position = F4(
	function (width, height, x, y) {
		return {height: height, width: width, x: x, y: y};
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$map4 = _Json_map4;
var $elm$core$Basics$round = _Basics_round;
var $norpan$elm_html5_drag_drop$Html5$DragDrop$positionDecoder = A5(
	$elm$json$Json$Decode$map4,
	$norpan$elm_html5_drag_drop$Html5$DragDrop$Position,
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['currentTarget', 'clientWidth']),
		$elm$json$Json$Decode$int),
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['currentTarget', 'clientHeight']),
		$elm$json$Json$Decode$int),
	A2(
		$elm$json$Json$Decode$map,
		$elm$core$Basics$round,
		A2(
			$elm$json$Json$Decode$at,
			_List_fromArray(
				['offsetX']),
			$elm$json$Json$Decode$float)),
	A2(
		$elm$json$Json$Decode$map,
		$elm$core$Basics$round,
		A2(
			$elm$json$Json$Decode$at,
			_List_fromArray(
				['offsetY']),
			$elm$json$Json$Decode$float)));
var $norpan$elm_html5_drag_drop$Html5$DragDrop$timeStampDecoder = A2(
	$elm$json$Json$Decode$map,
	$elm$core$Basics$round,
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['timeStamp']),
		$elm$json$Json$Decode$float));
var $norpan$elm_html5_drag_drop$Html5$DragDrop$droppable = F2(
	function (wrap, dropId) {
		return _List_fromArray(
			[
				A3(
				$norpan$elm_html5_drag_drop$Html5$DragDrop$onWithOptions,
				'dragenter',
				{preventDefault: true, stopPropagation: true},
				$elm$json$Json$Decode$succeed(
					wrap(
						$norpan$elm_html5_drag_drop$Html5$DragDrop$DragEnter(dropId)))),
				A3(
				$norpan$elm_html5_drag_drop$Html5$DragDrop$onWithOptions,
				'dragleave',
				{preventDefault: true, stopPropagation: true},
				$elm$json$Json$Decode$succeed(
					wrap(
						$norpan$elm_html5_drag_drop$Html5$DragDrop$DragLeave(dropId)))),
				A3(
				$norpan$elm_html5_drag_drop$Html5$DragDrop$onWithOptions,
				'dragover',
				{preventDefault: true, stopPropagation: false},
				A2(
					$elm$json$Json$Decode$map,
					wrap,
					A3(
						$elm$json$Json$Decode$map2,
						$norpan$elm_html5_drag_drop$Html5$DragDrop$DragOver(dropId),
						$norpan$elm_html5_drag_drop$Html5$DragDrop$timeStampDecoder,
						$norpan$elm_html5_drag_drop$Html5$DragDrop$positionDecoder))),
				A3(
				$norpan$elm_html5_drag_drop$Html5$DragDrop$onWithOptions,
				'drop',
				{preventDefault: true, stopPropagation: true},
				A2(
					$elm$json$Json$Decode$map,
					A2(
						$elm$core$Basics$composeL,
						wrap,
						$norpan$elm_html5_drag_drop$Html5$DragDrop$Drop(dropId)),
					$norpan$elm_html5_drag_drop$Html5$DragDrop$positionDecoder))
			]);
	});
var $author$project$Main$droppableHomes = function (spaceIndex) {
	return A2(
		$norpan$elm_html5_drag_drop$Html5$DragDrop$droppable,
		$author$project$Main$DragDropMsg,
		$author$project$Main$HomeTo(spaceIndex));
};
var $author$project$Main$helperForHome = F2(
	function (maybeFrom, model) {
		var _v0 = $author$project$ModelHistory$getCurrent(model.modelHistory);
		var pilesModel = _v0.pilesModel;
		var spacesModel = _v0.spacesModel;
		var homesModel = _v0.homesModel;
		if (maybeFrom.$ === 'Just') {
			if (maybeFrom.a.$ === 'PileFrom') {
				var _v2 = maybeFrom.a;
				var pileIndex = _v2.a;
				var cardIndex = _v2.b;
				return {
					draggedNumberOfCards: A3($author$project$Pile$getNumberOfCards, pileIndex, cardIndex, pilesModel),
					droppableAttribute: $author$project$Main$droppableHomes,
					maybeDraggedCard: A2($author$project$Pile$getTopCard, pileIndex, pilesModel)
				};
			} else {
				var spaceIndex = maybeFrom.a.a;
				return {
					draggedNumberOfCards: 1,
					droppableAttribute: $author$project$Main$droppableHomes,
					maybeDraggedCard: A2($author$project$Space$getCard, spaceIndex, spacesModel)
				};
			}
		} else {
			return {draggedNumberOfCards: 0, droppableAttribute: $author$project$Main$droppableHomes, maybeDraggedCard: $elm$core$Maybe$Nothing};
		}
	});
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onDoubleClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'dblclick',
		$elm$json$Json$Decode$succeed(msg));
};
var $author$project$Main$clickToSendHomeFromPile = F2(
	function (pileIndex, card) {
		return $elm$html$Html$Events$onDoubleClick(
			A2($author$project$Main$SentHomeFromPileMsg, pileIndex, card));
	});
var $author$project$Main$PileFrom = F2(
	function (a, b) {
		return {$: 'PileFrom', a: a, b: b};
	});
var $norpan$elm_html5_drag_drop$Html5$DragDrop$DragEnd = {$: 'DragEnd'};
var $norpan$elm_html5_drag_drop$Html5$DragDrop$DragStart = F2(
	function (a, b) {
		return {$: 'DragStart', a: a, b: b};
	});
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $norpan$elm_html5_drag_drop$Html5$DragDrop$draggable = F2(
	function (wrap, drag) {
		return _List_fromArray(
			[
				A2($elm$html$Html$Attributes$attribute, 'draggable', 'true'),
				A3(
				$norpan$elm_html5_drag_drop$Html5$DragDrop$onWithOptions,
				'dragstart',
				{preventDefault: false, stopPropagation: true},
				A2(
					$elm$json$Json$Decode$map,
					A2(
						$elm$core$Basics$composeL,
						wrap,
						$norpan$elm_html5_drag_drop$Html5$DragDrop$DragStart(drag)),
					$elm$json$Json$Decode$value)),
				A3(
				$norpan$elm_html5_drag_drop$Html5$DragDrop$onWithOptions,
				'dragend',
				{preventDefault: false, stopPropagation: true},
				$elm$json$Json$Decode$succeed(
					wrap($norpan$elm_html5_drag_drop$Html5$DragDrop$DragEnd)))
			]);
	});
var $author$project$Main$draggablePiles = function (_v0) {
	var pileIndex = _v0.a;
	var cardIndex = _v0.b;
	return A2(
		$norpan$elm_html5_drag_drop$Html5$DragDrop$draggable,
		$author$project$Main$DragDropMsg,
		A2($author$project$Main$PileFrom, pileIndex, cardIndex));
};
var $author$project$Main$PileTo = function (a) {
	return {$: 'PileTo', a: a};
};
var $author$project$Main$droppablePiles = function (pileIndex) {
	return A2(
		$norpan$elm_html5_drag_drop$Html5$DragDrop$droppable,
		$author$project$Main$DragDropMsg,
		$author$project$Main$PileTo(pileIndex));
};
var $author$project$Pile$getCard = F3(
	function (pileIndex, cardIndex, model) {
		return A2(
			$elm$core$Maybe$andThen,
			$elm$core$Array$get(cardIndex),
			A2($elm$core$Array$get, pileIndex, model.piles));
	});
var $elm$core$Array$isEmpty = function (_v0) {
	var len = _v0.a;
	return !len;
};
var $author$project$Pile$getEmptyPiles = function (_v0) {
	var piles = _v0.piles;
	return A3(
		$elm$core$Array$foldl,
		F2(
			function (pile, i) {
				return $elm$core$Array$isEmpty(pile) ? (i + 1) : i;
			}),
		0,
		piles);
};
var $author$project$Space$getEmptySpaces = function (_v0) {
	var spaces = _v0.spaces;
	return A3(
		$elm$core$Array$foldl,
		F2(
			function (space, i) {
				return _Utils_eq(space, $elm$core$Maybe$Nothing) ? (i + 1) : i;
			}),
		0,
		spaces);
};
var $author$project$Main$helperForPile = F2(
	function (maybeFrom, model) {
		var distributingDone = model.distributeModel.distributingDone;
		var _v0 = $author$project$ModelHistory$getCurrent(model.modelHistory);
		var pilesModel = _v0.pilesModel;
		var spacesModel = _v0.spacesModel;
		var homesModel = _v0.homesModel;
		if (!distributingDone) {
			return {
				cardClass: _List_fromArray(
					[
						$elm$html$Html$Attributes$class('card-distributing')
					]),
				draggedNumberOfCards: 0,
				emptyPiles: 0,
				emptySpaces: 0,
				maybeClickToSendHome: $elm$core$Maybe$Nothing,
				maybeDragCard: $elm$core$Maybe$Nothing,
				maybeDragFromCardId: $elm$core$Maybe$Nothing,
				maybeDragFromPileId: $elm$core$Maybe$Nothing,
				maybeDraggableAttribute: $elm$core$Maybe$Nothing,
				maybeDroppableAttribute: $elm$core$Maybe$Nothing
			};
		} else {
			if ($author$project$FlyingHome$flyingHome(model.flyingHomeModel)) {
				return {cardClass: _List_Nil, draggedNumberOfCards: 0, emptyPiles: 0, emptySpaces: 0, maybeClickToSendHome: $elm$core$Maybe$Nothing, maybeDragCard: $elm$core$Maybe$Nothing, maybeDragFromCardId: $elm$core$Maybe$Nothing, maybeDragFromPileId: $elm$core$Maybe$Nothing, maybeDraggableAttribute: $elm$core$Maybe$Nothing, maybeDroppableAttribute: $elm$core$Maybe$Nothing};
			} else {
				if ($author$project$Main$closingTheGame(model)) {
					return {cardClass: _List_Nil, draggedNumberOfCards: 0, emptyPiles: 0, emptySpaces: 0, maybeClickToSendHome: $elm$core$Maybe$Nothing, maybeDragCard: $elm$core$Maybe$Nothing, maybeDragFromCardId: $elm$core$Maybe$Nothing, maybeDragFromPileId: $elm$core$Maybe$Nothing, maybeDraggableAttribute: $elm$core$Maybe$Nothing, maybeDroppableAttribute: $elm$core$Maybe$Nothing};
				} else {
					if (maybeFrom.$ === 'Just') {
						if (maybeFrom.a.$ === 'PileFrom') {
							var _v2 = maybeFrom.a;
							var pileIndex = _v2.a;
							var cardIndex = _v2.b;
							return {
								cardClass: _List_Nil,
								draggedNumberOfCards: A3($author$project$Pile$getNumberOfCards, pileIndex, cardIndex, pilesModel),
								emptyPiles: $author$project$Pile$getEmptyPiles(pilesModel),
								emptySpaces: $author$project$Space$getEmptySpaces(spacesModel),
								maybeClickToSendHome: $elm$core$Maybe$Just($author$project$Main$clickToSendHomeFromPile),
								maybeDragCard: A3($author$project$Pile$getCard, pileIndex, cardIndex, pilesModel),
								maybeDragFromCardId: $elm$core$Maybe$Just(cardIndex),
								maybeDragFromPileId: $elm$core$Maybe$Just(pileIndex),
								maybeDraggableAttribute: $elm$core$Maybe$Just($author$project$Main$draggablePiles),
								maybeDroppableAttribute: $elm$core$Maybe$Just($author$project$Main$droppablePiles)
							};
						} else {
							var spaceIndex = maybeFrom.a.a;
							return {
								cardClass: _List_Nil,
								draggedNumberOfCards: 1,
								emptyPiles: $author$project$Pile$getEmptyPiles(pilesModel),
								emptySpaces: $author$project$Space$getEmptySpaces(spacesModel),
								maybeClickToSendHome: $elm$core$Maybe$Just($author$project$Main$clickToSendHomeFromPile),
								maybeDragCard: A2($author$project$Space$getCard, spaceIndex, spacesModel),
								maybeDragFromCardId: $elm$core$Maybe$Nothing,
								maybeDragFromPileId: $elm$core$Maybe$Nothing,
								maybeDraggableAttribute: $elm$core$Maybe$Just($author$project$Main$draggablePiles),
								maybeDroppableAttribute: $elm$core$Maybe$Just($author$project$Main$droppablePiles)
							};
						}
					} else {
						return {
							cardClass: _List_Nil,
							draggedNumberOfCards: 0,
							emptyPiles: $author$project$Pile$getEmptyPiles(pilesModel),
							emptySpaces: $author$project$Space$getEmptySpaces(spacesModel),
							maybeClickToSendHome: $elm$core$Maybe$Just($author$project$Main$clickToSendHomeFromPile),
							maybeDragCard: $elm$core$Maybe$Nothing,
							maybeDragFromCardId: $elm$core$Maybe$Nothing,
							maybeDragFromPileId: $elm$core$Maybe$Nothing,
							maybeDraggableAttribute: $elm$core$Maybe$Just($author$project$Main$draggablePiles),
							maybeDroppableAttribute: $elm$core$Maybe$Just($author$project$Main$droppablePiles)
						};
					}
				}
			}
		}
	});
var $author$project$Main$clickToSendHomeFromSpace = F2(
	function (homeIndex, card) {
		return $elm$html$Html$Events$onDoubleClick(
			A2($author$project$Main$SentHomeFromSpaceMsg, homeIndex, card));
	});
var $author$project$Main$SpaceFrom = function (a) {
	return {$: 'SpaceFrom', a: a};
};
var $author$project$Main$draggableSpaces = function (spaceIndex) {
	return A2(
		$norpan$elm_html5_drag_drop$Html5$DragDrop$draggable,
		$author$project$Main$DragDropMsg,
		$author$project$Main$SpaceFrom(spaceIndex));
};
var $author$project$Main$SpaceTo = function (a) {
	return {$: 'SpaceTo', a: a};
};
var $author$project$Main$droppableSpaces = function (spaceIndex) {
	return A2(
		$norpan$elm_html5_drag_drop$Html5$DragDrop$droppable,
		$author$project$Main$DragDropMsg,
		$author$project$Main$SpaceTo(spaceIndex));
};
var $author$project$Main$helperForSpace = F2(
	function (maybeFrom, model) {
		var _v0 = $author$project$ModelHistory$getCurrent(model.modelHistory);
		var pilesModel = _v0.pilesModel;
		var spacesModel = _v0.spacesModel;
		var homesModel = _v0.homesModel;
		if ($author$project$FlyingHome$flyingHome(model.flyingHomeModel) || $author$project$Main$closingTheGame(model)) {
			return {draggedNumberOfCards: 0, maybeClickToSendHomeFromSpace: $elm$core$Maybe$Nothing, maybeDragFromSpaceId: $elm$core$Maybe$Nothing, maybeDraggableAttribute: $elm$core$Maybe$Nothing, maybeDroppableAttribute: $elm$core$Maybe$Nothing};
		} else {
			if (maybeFrom.$ === 'Just') {
				if (maybeFrom.a.$ === 'PileFrom') {
					var _v2 = maybeFrom.a;
					var pileIndex = _v2.a;
					var cardIndex = _v2.b;
					return {
						draggedNumberOfCards: A3($author$project$Pile$getNumberOfCards, pileIndex, cardIndex, pilesModel),
						maybeClickToSendHomeFromSpace: $elm$core$Maybe$Just($author$project$Main$clickToSendHomeFromSpace),
						maybeDragFromSpaceId: $elm$core$Maybe$Nothing,
						maybeDraggableAttribute: $elm$core$Maybe$Just($author$project$Main$draggableSpaces),
						maybeDroppableAttribute: $elm$core$Maybe$Just($author$project$Main$droppableSpaces)
					};
				} else {
					var spaceIndex = maybeFrom.a.a;
					return {
						draggedNumberOfCards: 1,
						maybeClickToSendHomeFromSpace: $elm$core$Maybe$Just($author$project$Main$clickToSendHomeFromSpace),
						maybeDragFromSpaceId: $elm$core$Maybe$Just(spaceIndex),
						maybeDraggableAttribute: $elm$core$Maybe$Just($author$project$Main$draggableSpaces),
						maybeDroppableAttribute: $elm$core$Maybe$Just($author$project$Main$droppableSpaces)
					};
				}
			} else {
				return {
					draggedNumberOfCards: 0,
					maybeClickToSendHomeFromSpace: $elm$core$Maybe$Just($author$project$Main$clickToSendHomeFromSpace),
					maybeDragFromSpaceId: $elm$core$Maybe$Nothing,
					maybeDraggableAttribute: $elm$core$Maybe$Just($author$project$Main$draggableSpaces),
					maybeDroppableAttribute: $elm$core$Maybe$Just($author$project$Main$droppableSpaces)
				};
			}
		}
	});
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $elm$html$Html$Attributes$rel = _VirtualDom_attribute('rel');
var $author$project$CardsCDN$stylesheet = A3(
	$elm$html$Html$node,
	'link',
	_List_fromArray(
		[
			$elm$html$Html$Attributes$rel('stylesheet'),
			$elm$html$Html$Attributes$href('src/resources/cards.css')
		]),
	_List_Nil);
var $author$project$Buttons$NewClicked = {$: 'NewClicked'};
var $author$project$Buttons$RestartClicked = {$: 'RestartClicked'};
var $author$project$Buttons$UndoClicked = {$: 'UndoClicked'};
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Buttons$view = function (_v0) {
	var newEnabled = _v0.newEnabled;
	var restartEnabled = _v0.restartEnabled;
	var undoEnabled = _v0.undoEnabled;
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('buttons-container')
			]),
		_List_fromArray(
			[
				newEnabled ? A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('button'),
						$elm$html$Html$Events$onClick($author$project$Buttons$NewClicked)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('New game')
					])) : A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('button button-disabled')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('New game')
					])),
				restartEnabled ? A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('button'),
						$elm$html$Html$Events$onClick($author$project$Buttons$RestartClicked)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Restart')
					])) : A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('button button-disabled')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Restart')
					])),
				undoEnabled ? A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('button'),
						$elm$html$Html$Events$onClick($author$project$Buttons$UndoClicked)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Undo')
					])) : A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('button button-disabled')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Undo')
					]))
			]));
};
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Char$fromCode = _Char_fromCode;
var $author$project$Card$getColorClass = F2(
	function (color, postString) {
		switch (color.$) {
			case 'Black':
				return $elm$html$Html$Attributes$class('color-black' + postString);
			case 'Red':
				return $elm$html$Html$Attributes$class('color-red' + postString);
			case 'DarkBrown':
				return $elm$html$Html$Attributes$class('color-darkbrown' + postString);
			case 'LightBrown':
				return $elm$html$Html$Attributes$class('color-lightbrown' + postString);
			case 'Whitish':
				return $elm$html$Html$Attributes$class('color-whitish' + postString);
			case 'LightBlue':
				return $elm$html$Html$Attributes$class('color-lightblue' + postString);
			default:
				return $elm$html$Html$Attributes$class('color-darkblue' + postString);
		}
	});
var $author$project$Card$animateChar = function (_v0) {
	var _int = _v0.a;
	var color = _v0.b;
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('char '),
				A2($author$project$Card$getColorClass, color, '-animate')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				$elm$core$String$fromChar(
					$elm$core$Char$fromCode(_int)))
			]));
};
var $author$project$Card$Black = {$: 'Black'};
var $author$project$Card$DarkBrown = {$: 'DarkBrown'};
var $author$project$Card$LightBrown = {$: 'LightBrown'};
var $author$project$Card$Red = {$: 'Red'};
var $author$project$Card$Whitish = {$: 'Whitish'};
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $author$project$Card$getChars = function (_v0) {
	var suit = _v0.suit;
	var rank = _v0.rank;
	var _v1 = function () {
		switch (suit.$) {
			case 'Clubs':
				return _Utils_Tuple2(33, $author$project$Card$Black);
			case 'Diamonds':
				return _Utils_Tuple2(57, $author$project$Card$Red);
			case 'Hearts':
				return _Utils_Tuple2(81, $author$project$Card$Red);
			default:
				return _Utils_Tuple2(105, $author$project$Card$Black);
		}
	}();
	var suitOffset = _v1.a;
	var suitColor = _v1.b;
	return $elm$core$List$concat(
		_List_fromArray(
			[
				_List_fromArray(
				[
					_Utils_Tuple2(161, $author$project$Card$Whitish),
					_Utils_Tuple2(162, $author$project$Card$Black)
				]),
				function () {
				switch (rank.$) {
					case 'Ace':
						return _List_fromArray(
							[
								_Utils_Tuple2(suitOffset + 0, suitColor)
							]);
					case 'N2':
						return _List_fromArray(
							[
								_Utils_Tuple2(suitOffset + 1, suitColor)
							]);
					case 'N3':
						return _List_fromArray(
							[
								_Utils_Tuple2(suitOffset + 2, suitColor)
							]);
					case 'N4':
						return _List_fromArray(
							[
								_Utils_Tuple2(suitOffset + 3, suitColor)
							]);
					case 'N5':
						return _List_fromArray(
							[
								_Utils_Tuple2(suitOffset + 4, suitColor)
							]);
					case 'N6':
						return _List_fromArray(
							[
								_Utils_Tuple2(suitOffset + 5, suitColor)
							]);
					case 'N7':
						return _List_fromArray(
							[
								_Utils_Tuple2(suitOffset + 6, suitColor)
							]);
					case 'N8':
						return _List_fromArray(
							[
								_Utils_Tuple2(suitOffset + 7, suitColor)
							]);
					case 'N9':
						return _List_fromArray(
							[
								_Utils_Tuple2(suitOffset + 8, suitColor)
							]);
					case 'N10':
						return _List_fromArray(
							[
								_Utils_Tuple2(suitOffset + 9, suitColor)
							]);
					case 'Jack':
						return _List_fromArray(
							[
								_Utils_Tuple2(suitOffset + 10, $author$project$Card$Black),
								_Utils_Tuple2(suitOffset + 11, $author$project$Card$DarkBrown),
								_Utils_Tuple2(suitOffset + 12, $author$project$Card$LightBrown),
								_Utils_Tuple2(suitOffset + 13, $author$project$Card$Red)
							]);
					case 'Queen':
						return _List_fromArray(
							[
								_Utils_Tuple2(suitOffset + 14, $author$project$Card$Black),
								_Utils_Tuple2(suitOffset + 15, $author$project$Card$DarkBrown),
								_Utils_Tuple2(suitOffset + 16, $author$project$Card$LightBrown),
								_Utils_Tuple2(suitOffset + 17, $author$project$Card$Red)
							]);
					default:
						return _List_fromArray(
							[
								_Utils_Tuple2(suitOffset + 18, $author$project$Card$Black),
								_Utils_Tuple2(suitOffset + 19, $author$project$Card$DarkBrown),
								_Utils_Tuple2(suitOffset + 20, $author$project$Card$LightBrown),
								_Utils_Tuple2(suitOffset + 21, $author$project$Card$Red)
							]);
				}
			}()
			]));
};
var $author$project$Card$animate = function (card) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('char-holder')
			]),
		A2(
			$elm$core$List$map,
			$author$project$Card$animateChar,
			$author$project$Card$getChars(card)));
};
var $author$project$EndAnimation$viewEndAnimationCard = F2(
	function (index, card) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(
					'end-animation-card' + $elm$core$String$fromInt(
						A2($elm$core$Basics$modBy, 3, index)))
				]),
			_List_fromArray(
				[
					$author$project$Card$animate(card)
				]));
	});
var $author$project$EndAnimation$viewGrid = function (_v0) {
	var cards = _v0.cards;
	var offset = _v0.offset;
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('end-animation-grid')
			]),
		$elm$core$Array$toList(
			A2($elm$core$Array$indexedMap, $author$project$EndAnimation$viewEndAnimationCard, cards)));
};
var $author$project$EndAnimation$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('end-animation-container')
			]),
		_List_fromArray(
			[
				$author$project$EndAnimation$viewGrid(model)
			]));
};
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $author$project$FlyingHome$getStyles = F2(
	function (x, y) {
		return _List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
				A2(
				$elm$html$Html$Attributes$style,
				'left',
				$elm$core$String$fromInt(x) + 'vw'),
				A2(
				$elm$html$Html$Attributes$style,
				'top',
				$elm$core$String$fromInt(y) + 'vh')
			]);
	});
var $author$project$Card$viewChar = function (_v0) {
	var _int = _v0.a;
	var color = _v0.b;
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('char '),
				A2($author$project$Card$getColorClass, color, '')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				$elm$core$String$fromChar(
					$elm$core$Char$fromCode(_int)))
			]));
};
var $author$project$Card$view = function (card) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('char-holder')
			]),
		A2(
			$elm$core$List$map,
			$author$project$Card$viewChar,
			$author$project$Card$getChars(card)));
};
var $author$project$FlyingHome$view = function (model) {
	var _v0 = model;
	var maybeCard = _v0.maybeCard;
	var from = _v0.from;
	var to = _v0.to;
	var current = _v0.current;
	var iteration = _v0.iteration;
	if (maybeCard.$ === 'Nothing') {
		return A2($elm$html$Html$div, _List_Nil, _List_Nil);
	} else {
		var card = maybeCard.a;
		return A2(
			$elm$html$Html$div,
			A2($author$project$FlyingHome$getStyles, current.x, current.y),
			_List_fromArray(
				[
					$author$project$Card$view(card)
				]));
	}
};
var $author$project$Card$DarkBlue = {$: 'DarkBlue'};
var $author$project$Card$LightBlue = {$: 'LightBlue'};
var $author$project$Card$getCharsBack = _List_fromArray(
	[
		_Utils_Tuple2(161, $author$project$Card$Whitish),
		_Utils_Tuple2(162, $author$project$Card$Black),
		_Utils_Tuple2(165, $author$project$Card$DarkBlue),
		_Utils_Tuple2(166, $author$project$Card$LightBlue)
	]);
var $author$project$Card$viewBack = A2(
	$elm$html$Html$div,
	_List_fromArray(
		[
			$elm$html$Html$Attributes$class('char-holder')
		]),
	A2($elm$core$List$map, $author$project$Card$viewChar, $author$project$Card$getCharsBack));
var $author$project$Card$cardPlaceholder = A2(
	$elm$html$Html$div,
	_List_fromArray(
		[
			$elm$html$Html$Attributes$class('card card-placeholder')
		]),
	_List_fromArray(
		[$author$project$Card$viewBack]));
var $author$project$Home$viewHome = F3(
	function (helper, index, maybeCard) {
		var _v0 = helper;
		var maybeDraggedCard = _v0.maybeDraggedCard;
		var draggedNumberOfCards = _v0.draggedNumberOfCards;
		var droppableAttribute = _v0.droppableAttribute;
		var cardsSuccessive = function () {
			if (maybeDraggedCard.$ === 'Nothing') {
				return false;
			} else {
				var draggedCard = maybeDraggedCard.a;
				return A2($author$project$Card$cardsSuccessiveHome, maybeCard, draggedCard);
			}
		}();
		var _v1 = _Utils_Tuple2(maybeCard, cardsSuccessive);
		if (_v1.a.$ === 'Nothing') {
			if (!_v1.b) {
				var _v2 = _v1.a;
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[$author$project$Card$cardPlaceholder]));
			} else {
				var _v3 = _v1.a;
				return A2(
					$elm$html$Html$div,
					droppableAttribute(index),
					_List_fromArray(
						[$author$project$Card$cardPlaceholder]));
			}
		} else {
			if (!_v1.b) {
				var card = _v1.a.a;
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							$author$project$Card$cardPlaceholder,
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-home')
								]),
							_List_fromArray(
								[
									$author$project$Card$view(card)
								]))
						]));
			} else {
				var card = _v1.a.a;
				return A2(
					$elm$html$Html$div,
					droppableAttribute(index),
					_List_fromArray(
						[
							$author$project$Card$cardPlaceholder,
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-home')
								]),
							_List_fromArray(
								[
									$author$project$Card$view(card)
								]))
						]));
			}
		}
	});
var $author$project$Home$view = F2(
	function (model, helper) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('homes-container')
				]),
			$elm$core$Array$toList(
				A2(
					$elm$core$Array$indexedMap,
					$author$project$Home$viewHome(helper),
					model.homes)));
	});
var $author$project$Card$getColor = function (_v0) {
	var suit = _v0.suit;
	switch (suit.$) {
		case 'Hearts':
			return $author$project$Card$Red;
		case 'Diamonds':
			return $author$project$Card$Red;
		case 'Clubs':
			return $author$project$Card$Black;
		default:
			return $author$project$Card$Black;
	}
};
var $author$project$Card$cardsSuccessivePile = F2(
	function (maybeCard, nextCard) {
		if (maybeCard.$ === 'Nothing') {
			return true;
		} else {
			var card = maybeCard.a;
			return (!_Utils_eq(
				$author$project$Card$getColor(card),
				$author$project$Card$getColor(nextCard))) && _Utils_eq(
				$author$project$Card$getRank(card),
				$author$project$Card$getRank(nextCard) + 1);
		}
	});
var $author$project$Pile$canBeDraggedFromHelper = F2(
	function (card, _v0) {
		var cardIndex = _v0.a;
		var _v1 = _v0.b;
		var maybeLastCard = _v1.a;
		var connecting = _v1.b;
		var _v2 = _Utils_Tuple2(maybeLastCard, connecting);
		if (!_v2.b) {
			return _Utils_Tuple2(
				cardIndex,
				_Utils_Tuple2(maybeLastCard, false));
		} else {
			if (_v2.a.$ === 'Nothing') {
				var _v3 = _v2.a;
				return _Utils_Tuple2(
					cardIndex - 1,
					_Utils_Tuple2(
						$elm$core$Maybe$Just(card),
						true));
			} else {
				var lastCard = _v2.a.a;
				return A2(
					$author$project$Card$cardsSuccessivePile,
					$elm$core$Maybe$Just(card),
					lastCard) ? _Utils_Tuple2(
					cardIndex - 1,
					_Utils_Tuple2(
						$elm$core$Maybe$Just(card),
						true)) : _Utils_Tuple2(
					cardIndex + 1,
					_Utils_Tuple2($elm$core$Maybe$Nothing, false));
			}
		}
	});
var $author$project$Pile$canBeDraggedFrom = function (cards) {
	var lastCard = A2(
		$elm$core$Array$get,
		$elm$core$Array$length(cards) - 1,
		cards);
	if (lastCard.$ === 'Nothing') {
		return 99;
	} else {
		var card = lastCard.a;
		return A3(
			$elm$core$Array$foldr,
			$author$project$Pile$canBeDraggedFromHelper,
			_Utils_Tuple2(
				$elm$core$Array$length(cards) - 1,
				_Utils_Tuple2($elm$core$Maybe$Nothing, true)),
			cards).a;
	}
};
var $author$project$Pile$draggableCards = F2(
	function (emptySpaces, emptyPiles) {
		return (!emptyPiles) ? (emptySpaces + 1) : (2 * A2($author$project$Pile$draggableCards, emptySpaces, emptyPiles - 1));
	});
var $author$project$Pile$viewCardsRecursively = F6(
	function (helper, pileIndex, cardIndex, cards, draggableFrom, maybeDragFromCardId) {
		var cardHide = (_Utils_cmp(
			cardIndex,
			A2($elm$core$Maybe$withDefault, 99, maybeDragFromCardId)) > -1) ? ' card-hide' : ' card-show';
		var cardBottom = (!cardIndex) ? ' card-pile-bottom' : '';
		var _v0 = helper;
		var maybeDraggableAttribute = _v0.maybeDraggableAttribute;
		var maybeClickToSendHome = _v0.maybeClickToSendHome;
		var cardClass = _v0.cardClass;
		var draggableAttributes = function () {
			var _v3 = _Utils_Tuple2(
				maybeDraggableAttribute,
				_Utils_cmp(cardIndex, draggableFrom) > -1);
			if ((_v3.a.$ === 'Just') && _v3.b) {
				var draggableAttribute = _v3.a.a;
				return A2(
					$elm$core$List$cons,
					$elm$html$Html$Attributes$class('card-draggable'),
					draggableAttribute(
						_Utils_Tuple2(pileIndex, cardIndex)));
			} else {
				return _List_Nil;
			}
		}();
		var _v1 = A2($elm$core$Array$get, cardIndex, cards);
		if (_v1.$ === 'Nothing') {
			return A2($elm$html$Html$div, _List_Nil, _List_Nil);
		} else {
			var card = _v1.a;
			var onClick = function () {
				var _v2 = _Utils_Tuple2(
					maybeClickToSendHome,
					_Utils_eq(
						cardIndex,
						$elm$core$Array$length(cards) - 1));
				if ((_v2.a.$ === 'Just') && _v2.b) {
					var clickToSendHome = _v2.a.a;
					return _List_fromArray(
						[
							A2(clickToSendHome, pileIndex, card)
						]);
				} else {
					return _List_Nil;
				}
			}();
			return A2(
				$elm$html$Html$div,
				$elm$core$List$concat(
					_List_fromArray(
						[
							_List_fromArray(
							[
								$elm$html$Html$Attributes$class('card card-pile' + (cardBottom + cardHide))
							]),
							cardClass,
							draggableAttributes,
							onClick
						])),
				_List_fromArray(
					[
						$author$project$Card$view(card),
						A6($author$project$Pile$viewCardsRecursively, helper, pileIndex, cardIndex + 1, cards, draggableFrom, maybeDragFromCardId)
					]));
		}
	});
var $author$project$Pile$viewPile = F3(
	function (helper, pileIndex, pile) {
		var _v0 = helper;
		var maybeDragFromPileId = _v0.maybeDragFromPileId;
		var maybeDragFromCardId = _v0.maybeDragFromCardId;
		var maybeDragCard = _v0.maybeDragCard;
		var draggedNumberOfCards = _v0.draggedNumberOfCards;
		var maybeDroppableAttribute = _v0.maybeDroppableAttribute;
		var emptyPiles = _v0.emptyPiles;
		var emptySpaces = _v0.emptySpaces;
		var draggableFrom = A2(
			$elm$core$Basics$max,
			$author$project$Pile$canBeDraggedFrom(pile),
			$elm$core$Array$length(pile) - A2($author$project$Pile$draggableCards, emptySpaces, emptyPiles));
		var droppableAttributeList = function () {
			if (maybeDroppableAttribute.$ === 'Nothing') {
				return _List_Nil;
			} else {
				var droppableAttribute = maybeDroppableAttribute.a;
				return droppableAttribute(pileIndex);
			}
		}();
		if (maybeDragCard.$ === 'Just') {
			var draggedCard = maybeDragCard.a;
			return _Utils_eq(
				pileIndex,
				A2($elm$core$Maybe$withDefault, 99, maybeDragFromPileId)) ? A2(
				$elm$html$Html$div,
				A2(
					$elm$core$List$cons,
					$elm$html$Html$Attributes$class('pile'),
					droppableAttributeList),
				_List_fromArray(
					[
						$author$project$Card$cardPlaceholder,
						A6($author$project$Pile$viewCardsRecursively, helper, pileIndex, 0, pile, draggableFrom, maybeDragFromCardId)
					])) : (($elm$core$Array$isEmpty(pile) && (_Utils_cmp(
				draggedNumberOfCards,
				A2($author$project$Pile$draggableCards, emptySpaces, emptyPiles - 1)) > 0)) ? A2(
				$elm$html$Html$div,
				$elm$core$List$concat(
					_List_fromArray(
						[
							_List_fromArray(
							[
								$elm$html$Html$Attributes$class('pile')
							])
						])),
				_List_fromArray(
					[
						$author$project$Card$cardPlaceholder,
						A6($author$project$Pile$viewCardsRecursively, helper, pileIndex, 0, pile, draggableFrom, $elm$core$Maybe$Nothing)
					])) : (A2(
				$author$project$Card$cardsSuccessivePile,
				$author$project$Pile$getTopCardOfPile(pile),
				draggedCard) ? A2(
				$elm$html$Html$div,
				A2(
					$elm$core$List$cons,
					$elm$html$Html$Attributes$class('pile'),
					droppableAttributeList),
				_List_fromArray(
					[
						$author$project$Card$cardPlaceholder,
						A6($author$project$Pile$viewCardsRecursively, helper, pileIndex, 0, pile, draggableFrom, $elm$core$Maybe$Nothing)
					])) : A2(
				$elm$html$Html$div,
				$elm$core$List$concat(
					_List_fromArray(
						[
							_List_fromArray(
							[
								$elm$html$Html$Attributes$class('pile')
							])
						])),
				_List_fromArray(
					[
						$author$project$Card$cardPlaceholder,
						A6($author$project$Pile$viewCardsRecursively, helper, pileIndex, 0, pile, draggableFrom, $elm$core$Maybe$Nothing)
					]))));
		} else {
			return A2(
				$elm$html$Html$div,
				$elm$core$List$concat(
					_List_fromArray(
						[
							_List_fromArray(
							[
								$elm$html$Html$Attributes$class('pile')
							])
						])),
				_List_fromArray(
					[
						$author$project$Card$cardPlaceholder,
						A6($author$project$Pile$viewCardsRecursively, helper, pileIndex, 0, pile, draggableFrom, $elm$core$Maybe$Nothing)
					]));
		}
	});
var $author$project$Pile$viewPiles = F2(
	function (piles, helper) {
		return $elm$core$Array$toList(
			A2(
				$elm$core$Array$indexedMap,
				$author$project$Pile$viewPile(helper),
				piles));
	});
var $author$project$Pile$view = F2(
	function (model, helper) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('piles-container')
				]),
			A2($author$project$Pile$viewPiles, model.piles, helper));
	});
var $author$project$Shuffle$viewShuffleCard = F2(
	function (index, card) {
		return (!index) ? A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('card-shuffle-bottom')
				]),
			_List_fromArray(
				[
					$author$project$Card$view(card)
				])) : A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('card-shuffle')
				]),
			_List_fromArray(
				[
					$author$project$Card$view(card)
				]));
	});
var $author$project$Shuffle$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('shuffle-container')
			]),
		$elm$core$Array$toList(
			A2($elm$core$Array$indexedMap, $author$project$Shuffle$viewShuffleCard, model.pile)));
};
var $author$project$Space$viewSpace = F3(
	function (helper, index, maybeCard) {
		var _v0 = helper;
		var maybeDragFromSpaceId = _v0.maybeDragFromSpaceId;
		var draggedNumberOfCards = _v0.draggedNumberOfCards;
		var maybeDroppableAttribute = _v0.maybeDroppableAttribute;
		var maybeDraggableAttribute = _v0.maybeDraggableAttribute;
		var maybeClickToSendHomeFromSpace = _v0.maybeClickToSendHomeFromSpace;
		var draggableAttributeList = function () {
			if (maybeDraggableAttribute.$ === 'Nothing') {
				return _List_Nil;
			} else {
				var draggableAttribute = maybeDraggableAttribute.a;
				return draggableAttribute(index);
			}
		}();
		var droppableAttributeList = function () {
			if (maybeDroppableAttribute.$ === 'Nothing') {
				return _List_Nil;
			} else {
				var droppableAttribute = maybeDroppableAttribute.a;
				return droppableAttribute(index);
			}
		}();
		if (maybeCard.$ === 'Nothing') {
			return (draggedNumberOfCards === 1) ? A2(
				$elm$html$Html$div,
				droppableAttributeList,
				_List_fromArray(
					[$author$project$Card$cardPlaceholder])) : A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[$author$project$Card$cardPlaceholder]));
		} else {
			var card = maybeCard.a;
			var clickToSendHomeFromSpaceList = function () {
				if (maybeClickToSendHomeFromSpace.$ === 'Nothing') {
					return _List_Nil;
				} else {
					var clickToSendHomeFromSpace = maybeClickToSendHomeFromSpace.a;
					return _List_fromArray(
						[
							A2(clickToSendHomeFromSpace, index, card)
						]);
				}
			}();
			return _Utils_eq(
				A2($elm$core$Maybe$withDefault, 99, maybeDragFromSpaceId),
				index) ? A2(
				$elm$html$Html$div,
				A2($elm$core$List$append, draggableAttributeList, droppableAttributeList),
				_List_fromArray(
					[
						$author$project$Card$cardPlaceholder,
						A2(
						$elm$html$Html$div,
						A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class('card-space card-hide'),
							clickToSendHomeFromSpaceList),
						_List_fromArray(
							[
								$author$project$Card$view(card)
							]))
					])) : A2(
				$elm$html$Html$div,
				draggableAttributeList,
				_List_fromArray(
					[
						$author$project$Card$cardPlaceholder,
						A2(
						$elm$html$Html$div,
						A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class('card-space card-show'),
							clickToSendHomeFromSpaceList),
						_List_fromArray(
							[
								$author$project$Card$view(card)
							]))
					]));
		}
	});
var $author$project$Space$view = F2(
	function (model, helper) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('spaces-container')
				]),
			$elm$core$Array$toList(
				A2(
					$elm$core$Array$indexedMap,
					$author$project$Space$viewSpace(helper),
					model.spaces)));
	});
var $author$project$Main$view = function (model) {
	var helpForSpace = A2(
		$author$project$Main$helperForSpace,
		$norpan$elm_html5_drag_drop$Html5$DragDrop$getDragId(model.dragDrop),
		model);
	var helpForPile = A2(
		$author$project$Main$helperForPile,
		$norpan$elm_html5_drag_drop$Html5$DragDrop$getDragId(model.dragDrop),
		model);
	var helpForHome = A2(
		$author$project$Main$helperForHome,
		$norpan$elm_html5_drag_drop$Html5$DragDrop$getDragId(model.dragDrop),
		model);
	var _v0 = $author$project$ModelHistory$getCurrent(model.modelHistory);
	var pilesModel = _v0.pilesModel;
	var spacesModel = _v0.spacesModel;
	var homesModel = _v0.homesModel;
	var hasHistory = $author$project$ModelHistory$hasHistory(model.modelHistory) && (!$author$project$Pile$playingDone(pilesModel));
	var buttonsModel = {newEnabled: true, restartEnabled: hasHistory, undoEnabled: hasHistory};
	return {
		body: function () {
			var _v1 = model.doing;
			switch (_v1.$) {
				case 'Shuffling':
					return _List_fromArray(
						[
							$author$project$CardsCDN$stylesheet,
							A2(
							$elm$html$Html$map,
							$author$project$Main$ButtonsMsg,
							$author$project$Buttons$view(buttonsModel)),
							A2($author$project$Space$view, spacesModel, helpForSpace),
							A2($author$project$Home$view, homesModel, helpForHome),
							$author$project$Shuffle$view(model.shuffleModel)
						]);
				case 'Distributing':
					return _List_fromArray(
						[
							$author$project$CardsCDN$stylesheet,
							A2(
							$elm$html$Html$map,
							$author$project$Main$ButtonsMsg,
							$author$project$Buttons$view(buttonsModel)),
							A2($author$project$Space$view, spacesModel, helpForSpace),
							A2($author$project$Home$view, homesModel, helpForHome),
							A2($author$project$Pile$view, pilesModel, helpForPile)
						]);
				case 'Playing':
					return _List_fromArray(
						[
							$author$project$CardsCDN$stylesheet,
							A2(
							$elm$html$Html$map,
							$author$project$Main$ButtonsMsg,
							$author$project$Buttons$view(buttonsModel)),
							A2($author$project$Space$view, spacesModel, helpForSpace),
							A2($author$project$Home$view, homesModel, helpForHome),
							A2($author$project$Pile$view, pilesModel, helpForPile),
							$author$project$FlyingHome$view(model.flyingHomeModel)
						]);
				case 'FlyingHome':
					return _List_fromArray(
						[
							$author$project$CardsCDN$stylesheet,
							A2(
							$elm$html$Html$map,
							$author$project$Main$ButtonsMsg,
							$author$project$Buttons$view(buttonsModel)),
							A2($author$project$Space$view, spacesModel, helpForSpace),
							A2($author$project$Home$view, homesModel, helpForHome),
							A2($author$project$Pile$view, pilesModel, helpForPile),
							$author$project$FlyingHome$view(model.flyingHomeModel)
						]);
				case 'FlyingAllHome':
					return _List_fromArray(
						[
							$author$project$CardsCDN$stylesheet,
							A2(
							$elm$html$Html$map,
							$author$project$Main$ButtonsMsg,
							$author$project$Buttons$view(buttonsModel)),
							A2($author$project$Space$view, spacesModel, helpForSpace),
							A2($author$project$Home$view, homesModel, helpForHome),
							A2($author$project$Pile$view, pilesModel, helpForPile),
							$author$project$FlyingHome$view(model.flyingHomeModel)
						]);
				default:
					return _List_fromArray(
						[
							$author$project$CardsCDN$stylesheet,
							A2(
							$elm$html$Html$map,
							$author$project$Main$ButtonsMsg,
							$author$project$Buttons$view(buttonsModel)),
							$author$project$EndAnimation$view(model.endAnimationModel)
						]);
			}
		}(),
		title: 'Cards'
	};
};
var $author$project$Main$main = $elm$browser$Browser$document(
	{
		init: $author$project$Main$init,
		subscriptions: function (_v0) {
			return $elm$core$Platform$Sub$none;
		},
		update: $author$project$Main$update,
		view: $author$project$Main$view
	});
_Platform_export({'Main':{'init':$author$project$Main$main($elm$json$Json$Decode$string)(0)}});}(this));