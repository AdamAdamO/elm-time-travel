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

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.0/optimize for better performance and smaller assets.');


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
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



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
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
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
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
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
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
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

	if (typeof File === 'function' && value instanceof File)
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
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
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
	return word
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
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
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
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
	return elm$json$Json$Decode$errorToString(error);
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
		? elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? elm$core$Result$Ok(value)
		: (value instanceof String)
			? elm$core$Result$Ok(value + '')
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
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
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
				? elm$core$Result$Ok(decoder.c)
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
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

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
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

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
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
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
	elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

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
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
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
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

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
		r: converter,
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
	var converter = _Platform_effectManagers[name].r;

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

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

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
	var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2(elm$json$Json$Decode$map, func, handler.a)
				:
			A3(elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				elm$json$Json$Decode$succeed(func),
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
			&& { passive: elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
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

		if (!elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

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


// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.multiline) { flags += 'm'; }
	if (options.caseInsensitive) { flags += 'i'; }

	try
	{
		return elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? elm$core$Maybe$Just(submatch)
				: elm$core$Maybe$Nothing;
		}
		out.push(A4(elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch
				? elm$core$Maybe$Just(submatch)
				: elm$core$Maybe$Nothing;
		}
		return replacer(A4(elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;



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
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
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
	return elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
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
	return elm$core$Result$isOk(result) ? elm$core$Maybe$Just(result.a) : elm$core$Maybe$Nothing;
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
				: _Scheduler_fail(elm$browser$Browser$Dom$NotFound(id))
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
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
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
var author$project$Sandbox$Increment = function (a) {
	return {$: 'Increment', a: a};
};
var author$project$Sandbox$init = {
	counter: 0,
	lastMsg: author$project$Sandbox$Increment(0),
	message: ''
};
var elm$core$Basics$EQ = {$: 'EQ'};
var elm$core$Basics$GT = {$: 'GT'};
var elm$core$Basics$LT = {$: 'LT'};
var elm$core$Dict$foldr = F3(
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
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$List$cons = _List_cons;
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0.a;
	return elm$core$Dict$keys(dict);
};
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Basics$add = _Basics_add;
var elm$core$Basics$sub = _Basics_sub;
var author$project$Sandbox$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'Increment':
				var value = msg.a;
				return _Utils_update(
					model,
					{counter: model.counter + value, lastMsg: msg});
			case 'Decrement':
				var value = msg.a;
				return _Utils_update(
					model,
					{counter: model.counter - value, lastMsg: msg});
			default:
				var value = msg.a;
				return _Utils_update(
					model,
					{lastMsg: msg, message: value});
		}
	});
var author$project$Sandbox$Decrement = function (a) {
	return {$: 'Decrement', a: a};
};
var author$project$Sandbox$Message = function (a) {
	return {$: 'Message', a: a};
};
var elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var elm$core$Maybe$Nothing = {$: 'Nothing'};
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$Basics$identity = function (x) {
	return x;
};
var elm$core$Basics$False = {$: 'False'};
var elm$core$Basics$True = {$: 'True'};
var elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Basics$toFloat = _Basics_toFloat;
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$List$foldl = F3(
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
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Basics$eq = _Utils_equal;
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$gt = _Utils_gt;
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var elm$core$Basics$idiv = _Basics_idiv;
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
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
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$append = _Utils_append;
var elm$core$Basics$or = _Basics_or;
var elm$core$Char$toCode = _Char_toCode;
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$String$all = _String_all;
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$uncons = _String_uncons;
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 'Nothing') {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
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
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
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
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
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
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$json$Json$Decode$map = _Json_map1;
var elm$json$Json$Decode$map2 = _Json_map2;
var elm$json$Json$Decode$succeed = _Json_succeed;
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
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
var elm$html$Html$button = _VirtualDom_node('button');
var elm$html$Html$div = _VirtualDom_node('div');
var elm$html$Html$input = _VirtualDom_node('input');
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var elm$core$List$foldrHelper = F4(
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
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
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
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3(elm$core$List$foldr, elm$json$Json$Decode$field, decoder, fields);
	});
var elm$json$Json$Decode$string = _Json_decodeString;
var elm$html$Html$Events$targetValue = A2(
	elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	elm$json$Json$Decode$string);
var elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			elm$json$Json$Decode$map,
			elm$html$Html$Events$alwaysStop,
			A2(elm$json$Json$Decode$map, tagger, elm$html$Html$Events$targetValue)));
};
var author$project$Sandbox$view = function (model) {
	return A2(
		elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$button,
				_List_fromArray(
					[
						elm$html$Html$Events$onClick(
						author$project$Sandbox$Decrement(1))
					]),
				_List_fromArray(
					[
						elm$html$Html$text('-')
					])),
				A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text(
						elm$core$String$fromInt(model.counter))
					])),
				A2(
				elm$html$Html$button,
				_List_fromArray(
					[
						elm$html$Html$Events$onClick(
						author$project$Sandbox$Increment(1))
					]),
				_List_fromArray(
					[
						elm$html$Html$text('+')
					])),
				A2(
				elm$html$Html$input,
				_List_fromArray(
					[
						elm$html$Html$Events$onInput(author$project$Sandbox$Message)
					]),
				_List_Nil)
			]));
};
var author$project$TimeTravel$Browser$DebuggerMsg = function (a) {
	return {$: 'DebuggerMsg', a: a};
};
var author$project$TimeTravel$Browser$UserMsg = function (a) {
	return {$: 'UserMsg', a: a};
};
var author$project$TimeTravel$Internal$Model$newItem = F4(
	function (id, msg, causedBy, model) {
		return {causedBy: causedBy, id: id, lazyDiff: elm$core$Maybe$Nothing, lazyModelAst: elm$core$Maybe$Nothing, lazyMsgAst: elm$core$Maybe$Nothing, model: model, msg: msg};
	});
var author$project$TimeTravel$Internal$MsgLike$Init = {$: 'Init'};
var author$project$TimeTravel$Internal$Model$initItem = function (model) {
	return A4(author$project$TimeTravel$Internal$Model$newItem, 0, author$project$TimeTravel$Internal$MsgLike$Init, elm$core$Maybe$Nothing, model);
};
var author$project$TimeTravel$Internal$Util$Nel$Nel = F2(
	function (a, b) {
		return {$: 'Nel', a: a, b: b};
	});
var elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var elm$core$Set$empty = elm$core$Set$Set_elm_builtin(elm$core$Dict$empty);
var author$project$TimeTravel$Internal$Model$init = function (model) {
	return {
		expand: false,
		expandedTree: elm$core$Set$empty,
		filter: _List_Nil,
		fixedToLeft: false,
		future: _List_Nil,
		history: A2(
			author$project$TimeTravel$Internal$Util$Nel$Nel,
			author$project$TimeTravel$Internal$Model$initItem(model),
			_List_Nil),
		minimized: false,
		modelFilter: '',
		msgId: 1,
		selectedMsg: elm$core$Maybe$Nothing,
		showDiff: false,
		showModelDetail: true,
		sync: true,
		watch: elm$core$Maybe$Nothing
	};
};
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$map = _Platform_map;
var author$project$TimeTravel$Browser$wrapInit = function (_n0) {
	var model = _n0.a;
	var cmd = _n0.b;
	return _Utils_Tuple2(
		author$project$TimeTravel$Internal$Model$init(model),
		elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					A2(
					elm$core$Platform$Cmd$map,
					function (msg) {
						return author$project$TimeTravel$Browser$UserMsg(
							_Utils_Tuple2(
								elm$core$Maybe$Just(0),
								msg));
					},
					cmd)
				])));
};
var author$project$TimeTravel$Internal$Model$Receive = function (a) {
	return {$: 'Receive', a: a};
};
var author$project$TimeTravel$Internal$Util$Nel$head = function (_n0) {
	var head_ = _n0.a;
	var tail = _n0.b;
	return head_;
};
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var elm$core$Platform$Sub$batch = _Platform_batch;
var elm$core$Platform$Sub$map = _Platform_map;
var author$project$TimeTravel$Browser$wrapSubscriptions = F3(
	function (subscriptions, incomingMsg, model) {
		var item = author$project$TimeTravel$Internal$Util$Nel$head(model.history);
		return elm$core$Platform$Sub$batch(
			_List_fromArray(
				[
					A2(
					elm$core$Platform$Sub$map,
					function (c) {
						return author$project$TimeTravel$Browser$UserMsg(
							_Utils_Tuple2(elm$core$Maybe$Nothing, c));
					},
					subscriptions(item.model)),
					incomingMsg(
					A2(elm$core$Basics$composeL, author$project$TimeTravel$Browser$DebuggerMsg, author$project$TimeTravel$Internal$Model$Receive))
				]));
	});
var author$project$TimeTravel$Internal$Model$createTuple = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var author$project$TimeTravel$Internal$Model$selectFirstIfSync = function (model) {
	return model.sync ? _Utils_update(
		model,
		{
			selectedMsg: elm$core$Maybe$Just(
				author$project$TimeTravel$Internal$Util$Nel$head(model.history).id)
		}) : model;
};
var elm$core$Debug$toString = _Debug_toString;
var elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var elm$core$String$words = _String_words;
var author$project$TimeTravel$Internal$Model$updateFilter = F2(
	function (msgLike, filterOptions) {
		var str = function () {
			if (msgLike.$ === 'Message') {
				var msg = msgLike.a;
				return elm$core$Debug$toString(msg);
			} else {
				return '';
			}
		}();
		var _n0 = elm$core$String$words(str);
		if (_n0.b) {
			var head = _n0.a;
			var exists = A2(
				elm$core$List$any,
				function (_n1) {
					var name = _n1.a;
					return _Utils_eq(name, head);
				},
				filterOptions);
			return exists ? filterOptions : A2(
				elm$core$List$cons,
				_Utils_Tuple2(head, true),
				filterOptions);
		} else {
			return filterOptions;
		}
	});
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var author$project$TimeTravel$Internal$Util$Nel$map = F2(
	function (f, _n0) {
		var head_ = _n0.a;
		var tail = _n0.b;
		return A2(
			author$project$TimeTravel$Internal$Util$Nel$Nel,
			f(head_),
			A2(elm$core$List$map, f, tail));
	});
var author$project$TimeTravel$Internal$Model$mapHistory = F2(
	function (f, model) {
		return _Utils_update(
			model,
			{
				history: A2(author$project$TimeTravel$Internal$Util$Nel$map, f, model.history)
			});
	});
var author$project$TimeTravel$Internal$Parser$AST$ListLiteralX = F2(
	function (a, b) {
		return {$: 'ListLiteralX', a: a, b: b};
	});
var author$project$TimeTravel$Internal$Parser$AST$PropertyX = F3(
	function (a, b, c) {
		return {$: 'PropertyX', a: a, b: b, c: c};
	});
var author$project$TimeTravel$Internal$Parser$AST$RecordX = F2(
	function (a, b) {
		return {$: 'RecordX', a: a, b: b};
	});
var author$project$TimeTravel$Internal$Parser$AST$StringLiteralX = F2(
	function (a, b) {
		return {$: 'StringLiteralX', a: a, b: b};
	});
var author$project$TimeTravel$Internal$Parser$AST$TupleLiteralX = F2(
	function (a, b) {
		return {$: 'TupleLiteralX', a: a, b: b};
	});
var author$project$TimeTravel$Internal$Parser$AST$UnionX = F3(
	function (a, b, c) {
		return {$: 'UnionX', a: a, b: b, c: c};
	});
var author$project$TimeTravel$Internal$Parser$AST$ValueX = F2(
	function (a, b) {
		return {$: 'ValueX', a: a, b: b};
	});
var author$project$TimeTravel$Internal$Parser$AST$attachId = F2(
	function (id, ast) {
		switch (ast.$) {
			case 'Record':
				var children = ast.a;
				return A2(
					author$project$TimeTravel$Internal$Parser$AST$RecordX,
					id,
					A2(author$project$TimeTravel$Internal$Parser$AST$attachIdToList, id, children));
			case 'StringLiteral':
				var s = ast.a;
				return A2(author$project$TimeTravel$Internal$Parser$AST$StringLiteralX, id, s);
			case 'ListLiteral':
				var children = ast.a;
				return A2(
					author$project$TimeTravel$Internal$Parser$AST$ListLiteralX,
					id,
					A2(author$project$TimeTravel$Internal$Parser$AST$attachIdToListWithIndex, id, children));
			case 'TupleLiteral':
				var children = ast.a;
				if (children.b && (!children.b.b)) {
					var x = children.a;
					return A2(
						author$project$TimeTravel$Internal$Parser$AST$TupleLiteralX,
						id,
						A2(author$project$TimeTravel$Internal$Parser$AST$attachIdToList, id, children));
				} else {
					return A2(
						author$project$TimeTravel$Internal$Parser$AST$TupleLiteralX,
						id,
						A2(author$project$TimeTravel$Internal$Parser$AST$attachIdToListWithIndex, id, children));
				}
			case 'Value':
				var s = ast.a;
				return A2(author$project$TimeTravel$Internal$Parser$AST$ValueX, id, s);
			case 'Union':
				var tag = ast.a;
				var children = ast.b;
				var id_ = id + ('.' + tag);
				return A3(
					author$project$TimeTravel$Internal$Parser$AST$UnionX,
					id_,
					tag,
					A2(author$project$TimeTravel$Internal$Parser$AST$attachIdToListWithIndex, id_, children));
			default:
				var key = ast.a;
				var value = ast.b;
				var id_ = id + ('.' + key);
				return A3(
					author$project$TimeTravel$Internal$Parser$AST$PropertyX,
					id_,
					key,
					A2(author$project$TimeTravel$Internal$Parser$AST$attachId, id_, value));
		}
	});
var author$project$TimeTravel$Internal$Parser$AST$attachIdToList = F2(
	function (id, list) {
		return A2(
			elm$core$List$map,
			author$project$TimeTravel$Internal$Parser$AST$attachId(id),
			list);
	});
var author$project$TimeTravel$Internal$Parser$AST$attachIdToListWithIndex = F2(
	function (id, list) {
		return A2(
			elm$core$List$indexedMap,
			F2(
				function (index, p) {
					return A2(
						author$project$TimeTravel$Internal$Parser$AST$attachId,
						id + ('.' + elm$core$String$fromInt(index)),
						p);
				}),
			list);
	});
var andre_dietrich$parser_combinators$Combine$app = function (_n0) {
	var inner = _n0.a;
	return inner;
};
var andre_dietrich$parser_combinators$Combine$InputStream = F3(
	function (data, input, position) {
		return {data: data, input: input, position: position};
	});
var andre_dietrich$parser_combinators$Combine$initStream = function (s) {
	return A3(andre_dietrich$parser_combinators$Combine$InputStream, s, s, 0);
};
var andre_dietrich$parser_combinators$Combine$runParser = F3(
	function (p, st, s) {
		var _n0 = A3(
			andre_dietrich$parser_combinators$Combine$app,
			p,
			st,
			andre_dietrich$parser_combinators$Combine$initStream(s));
		if (_n0.c.$ === 'Ok') {
			var state = _n0.a;
			var stream = _n0.b;
			var res = _n0.c.a;
			return elm$core$Result$Ok(
				_Utils_Tuple3(state, stream, res));
		} else {
			var state = _n0.a;
			var stream = _n0.b;
			var ms = _n0.c.a;
			return elm$core$Result$Err(
				_Utils_Tuple3(state, stream, ms));
		}
	});
var andre_dietrich$parser_combinators$Combine$parse = function (p) {
	return A2(andre_dietrich$parser_combinators$Combine$runParser, p, _Utils_Tuple0);
};
var andre_dietrich$parser_combinators$Combine$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var andre_dietrich$parser_combinators$Combine$andThen = F2(
	function (f, p) {
		return andre_dietrich$parser_combinators$Combine$Parser(
			F2(
				function (state, stream) {
					var _n0 = A3(andre_dietrich$parser_combinators$Combine$app, p, state, stream);
					if (_n0.c.$ === 'Ok') {
						var rstate = _n0.a;
						var rstream = _n0.b;
						var res = _n0.c.a;
						return A3(
							andre_dietrich$parser_combinators$Combine$app,
							f(res),
							rstate,
							rstream);
					} else {
						var estate = _n0.a;
						var estream = _n0.b;
						var ms = _n0.c.a;
						return _Utils_Tuple3(
							estate,
							estream,
							elm$core$Result$Err(ms));
					}
				}));
	});
var andre_dietrich$parser_combinators$Combine$bimap = F3(
	function (fok, ferr, p) {
		return andre_dietrich$parser_combinators$Combine$Parser(
			F2(
				function (state, stream) {
					var _n0 = A3(andre_dietrich$parser_combinators$Combine$app, p, state, stream);
					if (_n0.c.$ === 'Ok') {
						var rstate = _n0.a;
						var rstream = _n0.b;
						var res = _n0.c.a;
						return _Utils_Tuple3(
							rstate,
							rstream,
							elm$core$Result$Ok(
								fok(res)));
					} else {
						var estate = _n0.a;
						var estream = _n0.b;
						var ms = _n0.c.a;
						return _Utils_Tuple3(
							estate,
							estream,
							elm$core$Result$Err(
								ferr(ms)));
					}
				}));
	});
var andre_dietrich$parser_combinators$Combine$map = F2(
	function (f, p) {
		return A3(andre_dietrich$parser_combinators$Combine$bimap, f, elm$core$Basics$identity, p);
	});
var pilatch$flip$Flip$flip = F3(
	function (_function, argB, argA) {
		return A2(_function, argA, argB);
	});
var andre_dietrich$parser_combinators$Combine$andMap = F2(
	function (rp, lp) {
		return A2(
			andre_dietrich$parser_combinators$Combine$andThen,
			A2(pilatch$flip$Flip$flip, andre_dietrich$parser_combinators$Combine$map, rp),
			lp);
	});
var elm$core$Basics$always = F2(
	function (a, _n0) {
		return a;
	});
var andre_dietrich$parser_combinators$Combine$ignore = F2(
	function (p1, p2) {
		return A2(
			andre_dietrich$parser_combinators$Combine$andMap,
			p1,
			A2(andre_dietrich$parser_combinators$Combine$map, elm$core$Basics$always, p2));
	});
var andre_dietrich$parser_combinators$Combine$keep = F2(
	function (p1, p2) {
		return A2(
			andre_dietrich$parser_combinators$Combine$andMap,
			p1,
			A2(
				andre_dietrich$parser_combinators$Combine$map,
				pilatch$flip$Flip$flip(elm$core$Basics$always),
				p2));
	});
var andre_dietrich$parser_combinators$Combine$between = F3(
	function (lp, rp, p) {
		return A2(
			andre_dietrich$parser_combinators$Combine$ignore,
			rp,
			A2(andre_dietrich$parser_combinators$Combine$keep, p, lp));
	});
var elm$core$String$length = _String_length;
var elm$core$String$slice = _String_slice;
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var elm$core$String$startsWith = _String_startsWith;
var andre_dietrich$parser_combinators$Combine$string = function (s) {
	return andre_dietrich$parser_combinators$Combine$Parser(
		F2(
			function (state, stream) {
				if (A2(elm$core$String$startsWith, s, stream.input)) {
					var len = elm$core$String$length(s);
					var pos = stream.position + len;
					var rem = A2(elm$core$String$dropLeft, len, stream.input);
					return _Utils_Tuple3(
						state,
						_Utils_update(
							stream,
							{input: rem, position: pos}),
						elm$core$Result$Ok(s));
				} else {
					return _Utils_Tuple3(
						state,
						stream,
						elm$core$Result$Err(
							_List_fromArray(
								['expected \"' + (s + '\"')])));
				}
			}));
};
var andre_dietrich$parser_combinators$Combine$braces = A2(
	andre_dietrich$parser_combinators$Combine$between,
	andre_dietrich$parser_combinators$Combine$string('{'),
	andre_dietrich$parser_combinators$Combine$string('}'));
var andre_dietrich$parser_combinators$Combine$brackets = A2(
	andre_dietrich$parser_combinators$Combine$between,
	andre_dietrich$parser_combinators$Combine$string('['),
	andre_dietrich$parser_combinators$Combine$string(']'));
var andre_dietrich$parser_combinators$Combine$succeed = function (res) {
	return andre_dietrich$parser_combinators$Combine$Parser(
		F2(
			function (state, stream) {
				return _Utils_Tuple3(
					state,
					stream,
					elm$core$Result$Ok(res));
			}));
};
var andre_dietrich$parser_combinators$Combine$lazy = function (t) {
	return A2(
		andre_dietrich$parser_combinators$Combine$andThen,
		t,
		andre_dietrich$parser_combinators$Combine$succeed(_Utils_Tuple0));
};
var andre_dietrich$parser_combinators$Combine$many = function (p) {
	var accumulate = F3(
		function (acc, state, stream) {
			accumulate:
			while (true) {
				var _n0 = A3(andre_dietrich$parser_combinators$Combine$app, p, state, stream);
				if (_n0.c.$ === 'Ok') {
					var rstate = _n0.a;
					var rstream = _n0.b;
					var res = _n0.c.a;
					if (_Utils_eq(stream, rstream)) {
						return _Utils_Tuple3(
							rstate,
							rstream,
							elm$core$List$reverse(acc));
					} else {
						var $temp$acc = A2(elm$core$List$cons, res, acc),
							$temp$state = rstate,
							$temp$stream = rstream;
						acc = $temp$acc;
						state = $temp$state;
						stream = $temp$stream;
						continue accumulate;
					}
				} else {
					return _Utils_Tuple3(
						state,
						stream,
						elm$core$List$reverse(acc));
				}
			}
		});
	return andre_dietrich$parser_combinators$Combine$Parser(
		F2(
			function (state, stream) {
				var _n1 = A3(accumulate, _List_Nil, state, stream);
				var rstate = _n1.a;
				var rstream = _n1.b;
				var res = _n1.c;
				return _Utils_Tuple3(
					rstate,
					rstream,
					elm$core$Result$Ok(res));
			}));
};
var andre_dietrich$parser_combinators$Combine$or = F2(
	function (lp, rp) {
		return andre_dietrich$parser_combinators$Combine$Parser(
			F2(
				function (state, stream) {
					var _n0 = A3(andre_dietrich$parser_combinators$Combine$app, lp, state, stream);
					if (_n0.c.$ === 'Ok') {
						var res = _n0;
						return res;
					} else {
						var lms = _n0.c.a;
						var _n1 = A3(andre_dietrich$parser_combinators$Combine$app, rp, state, stream);
						if (_n1.c.$ === 'Ok') {
							var res = _n1;
							return res;
						} else {
							var rms = _n1.c.a;
							return _Utils_Tuple3(
								state,
								stream,
								elm$core$Result$Err(
									_Utils_ap(lms, rms)));
						}
					}
				}));
	});
var andre_dietrich$parser_combinators$Combine$parens = A2(
	andre_dietrich$parser_combinators$Combine$between,
	andre_dietrich$parser_combinators$Combine$string('('),
	andre_dietrich$parser_combinators$Combine$string(')'));
var andre_dietrich$parser_combinators$Combine$sepBy1 = F2(
	function (sep, p) {
		return A2(
			andre_dietrich$parser_combinators$Combine$andMap,
			andre_dietrich$parser_combinators$Combine$many(
				A2(andre_dietrich$parser_combinators$Combine$keep, p, sep)),
			A2(andre_dietrich$parser_combinators$Combine$map, elm$core$List$cons, p));
	});
var andre_dietrich$parser_combinators$Combine$sepBy = F2(
	function (sep, p) {
		return A2(
			andre_dietrich$parser_combinators$Combine$or,
			A2(andre_dietrich$parser_combinators$Combine$sepBy1, sep, p),
			andre_dietrich$parser_combinators$Combine$succeed(_List_Nil));
	});
var author$project$TimeTravel$Internal$Parser$AST$ListLiteral = function (a) {
	return {$: 'ListLiteral', a: a};
};
var author$project$TimeTravel$Internal$Parser$AST$Property = F2(
	function (a, b) {
		return {$: 'Property', a: a, b: b};
	});
var author$project$TimeTravel$Internal$Parser$AST$Record = function (a) {
	return {$: 'Record', a: a};
};
var author$project$TimeTravel$Internal$Parser$AST$TupleLiteral = function (a) {
	return {$: 'TupleLiteral', a: a};
};
var author$project$TimeTravel$Internal$Parser$AST$Union = F2(
	function (a, b) {
		return {$: 'Union', a: a, b: b};
	});
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {index: index, match: match, number: number, submatches: submatches};
	});
var elm$regex$Regex$findAtMost = _Regex_findAtMost;
var elm$regex$Regex$never = _Regex_never;
var andre_dietrich$parser_combinators$Combine$regexer = F5(
	function (input, output, pat, state, stream) {
		var pattern = A2(elm$core$String$startsWith, '^', pat) ? pat : ('^' + pat);
		var _n0 = A3(
			elm$regex$Regex$findAtMost,
			1,
			A2(
				elm$core$Maybe$withDefault,
				elm$regex$Regex$never,
				input(pattern)),
			stream.input);
		if (_n0.b && (!_n0.b.b)) {
			var match = _n0.a;
			var len = elm$core$String$length(match.match);
			var pos = stream.position + len;
			var rem = A2(elm$core$String$dropLeft, len, stream.input);
			return _Utils_Tuple3(
				state,
				_Utils_update(
					stream,
					{input: rem, position: pos}),
				elm$core$Result$Ok(
					output(match)));
		} else {
			return _Utils_Tuple3(
				state,
				stream,
				elm$core$Result$Err(
					_List_fromArray(
						['expected input matching Regexp /' + (pattern + '/')])));
		}
	});
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var elm$regex$Regex$fromString = function (string) {
	return A2(
		elm$regex$Regex$fromStringWith,
		{caseInsensitive: false, multiline: false},
		string);
};
var andre_dietrich$parser_combinators$Combine$regex = A2(
	elm$core$Basics$composeR,
	A2(
		andre_dietrich$parser_combinators$Combine$regexer,
		elm$regex$Regex$fromString,
		function ($) {
			return $.match;
		}),
	andre_dietrich$parser_combinators$Combine$Parser);
var author$project$TimeTravel$Internal$Parser$AST$Value = function (a) {
	return {$: 'Value', a: a};
};
var author$project$TimeTravel$Internal$Parser$Parser$internalStructure = A2(
	andre_dietrich$parser_combinators$Combine$map,
	author$project$TimeTravel$Internal$Parser$AST$Value,
	andre_dietrich$parser_combinators$Combine$regex('<[^>]*>'));
var author$project$TimeTravel$Internal$Parser$Parser$null = A2(
	andre_dietrich$parser_combinators$Combine$map,
	author$project$TimeTravel$Internal$Parser$AST$Value,
	andre_dietrich$parser_combinators$Combine$regex('[a-z]+'));
var author$project$TimeTravel$Internal$Parser$Parser$numberLiteral = A2(
	andre_dietrich$parser_combinators$Combine$map,
	author$project$TimeTravel$Internal$Parser$AST$Value,
	andre_dietrich$parser_combinators$Combine$regex('(\\-)?[0-9][0-9.]*'));
var author$project$TimeTravel$Internal$Parser$Parser$propertyKey = andre_dietrich$parser_combinators$Combine$regex('[^ ]+');
var author$project$TimeTravel$Internal$Parser$Parser$tag = andre_dietrich$parser_combinators$Combine$regex('[A-Z][a-zA-Z0-9_.]*');
var author$project$TimeTravel$Internal$Parser$Parser$singleUnion = andre_dietrich$parser_combinators$Combine$lazy(
	function (_n0) {
		return A2(
			andre_dietrich$parser_combinators$Combine$map,
			function (tag_) {
				return A2(author$project$TimeTravel$Internal$Parser$AST$Union, tag_, _List_Nil);
			},
			author$project$TimeTravel$Internal$Parser$Parser$tag);
	});
var author$project$TimeTravel$Internal$Parser$AST$StringLiteral = function (a) {
	return {$: 'StringLiteral', a: a};
};
var author$project$TimeTravel$Internal$Parser$Parser$stringLiteral = A2(
	andre_dietrich$parser_combinators$Combine$map,
	author$project$TimeTravel$Internal$Parser$AST$StringLiteral,
	A3(
		andre_dietrich$parser_combinators$Combine$between,
		andre_dietrich$parser_combinators$Combine$string('\"'),
		andre_dietrich$parser_combinators$Combine$string('\"'),
		andre_dietrich$parser_combinators$Combine$regex('(\\\\"|[^"])*')));
var author$project$TimeTravel$Internal$Parser$Util$comma = andre_dietrich$parser_combinators$Combine$string(',');
var author$project$TimeTravel$Internal$Parser$Util$equal = andre_dietrich$parser_combinators$Combine$string('=');
var author$project$TimeTravel$Internal$Parser$Util$spaces = andre_dietrich$parser_combinators$Combine$regex('[ ]*');
var author$project$TimeTravel$Internal$Parser$Util$spaced = function (p) {
	return A3(andre_dietrich$parser_combinators$Combine$between, author$project$TimeTravel$Internal$Parser$Util$spaces, author$project$TimeTravel$Internal$Parser$Util$spaces, p);
};
function author$project$TimeTravel$Internal$Parser$Parser$cyclic$expression() {
	return andre_dietrich$parser_combinators$Combine$lazy(
		function (_n15) {
			return A2(
				andre_dietrich$parser_combinators$Combine$or,
				author$project$TimeTravel$Internal$Parser$Parser$cyclic$union(),
				author$project$TimeTravel$Internal$Parser$Parser$cyclic$expressionWithoutUnion());
		});
}
function author$project$TimeTravel$Internal$Parser$Parser$cyclic$expressionWithoutUnion() {
	return andre_dietrich$parser_combinators$Combine$lazy(
		function (_n14) {
			return A2(
				andre_dietrich$parser_combinators$Combine$or,
				author$project$TimeTravel$Internal$Parser$Parser$null,
				A2(
					andre_dietrich$parser_combinators$Combine$or,
					author$project$TimeTravel$Internal$Parser$Parser$numberLiteral,
					A2(
						andre_dietrich$parser_combinators$Combine$or,
						author$project$TimeTravel$Internal$Parser$Parser$stringLiteral,
						A2(
							andre_dietrich$parser_combinators$Combine$or,
							author$project$TimeTravel$Internal$Parser$Parser$internalStructure,
							A2(
								andre_dietrich$parser_combinators$Combine$or,
								author$project$TimeTravel$Internal$Parser$Parser$cyclic$tupleLiteral(),
								A2(
									andre_dietrich$parser_combinators$Combine$or,
									author$project$TimeTravel$Internal$Parser$Parser$cyclic$listLiteral(),
									author$project$TimeTravel$Internal$Parser$Parser$cyclic$record()))))));
		});
}
function author$project$TimeTravel$Internal$Parser$Parser$cyclic$items() {
	return andre_dietrich$parser_combinators$Combine$lazy(
		function (_n13) {
			return author$project$TimeTravel$Internal$Parser$Util$spaced(
				A2(
					andre_dietrich$parser_combinators$Combine$sepBy,
					author$project$TimeTravel$Internal$Parser$Util$comma,
					author$project$TimeTravel$Internal$Parser$Util$spaced(
						author$project$TimeTravel$Internal$Parser$Parser$cyclic$expression())));
		});
}
function author$project$TimeTravel$Internal$Parser$Parser$cyclic$listLiteral() {
	return andre_dietrich$parser_combinators$Combine$lazy(
		function (_n12) {
			return A2(
				andre_dietrich$parser_combinators$Combine$map,
				author$project$TimeTravel$Internal$Parser$AST$ListLiteral,
				andre_dietrich$parser_combinators$Combine$brackets(
					author$project$TimeTravel$Internal$Parser$Parser$cyclic$items()));
		});
}
function author$project$TimeTravel$Internal$Parser$Parser$cyclic$properties() {
	return andre_dietrich$parser_combinators$Combine$lazy(
		function (_n11) {
			return author$project$TimeTravel$Internal$Parser$Util$spaced(
				A2(
					andre_dietrich$parser_combinators$Combine$sepBy,
					author$project$TimeTravel$Internal$Parser$Util$comma,
					author$project$TimeTravel$Internal$Parser$Parser$cyclic$property()));
		});
}
function author$project$TimeTravel$Internal$Parser$Parser$cyclic$property() {
	return andre_dietrich$parser_combinators$Combine$lazy(
		function (_n5) {
			return A2(
				andre_dietrich$parser_combinators$Combine$andMap,
				author$project$TimeTravel$Internal$Parser$Util$spaces,
				A2(
					andre_dietrich$parser_combinators$Combine$andMap,
					author$project$TimeTravel$Internal$Parser$Parser$cyclic$expression(),
					A2(
						andre_dietrich$parser_combinators$Combine$andMap,
						author$project$TimeTravel$Internal$Parser$Util$spaces,
						A2(
							andre_dietrich$parser_combinators$Combine$andMap,
							author$project$TimeTravel$Internal$Parser$Util$equal,
							A2(
								andre_dietrich$parser_combinators$Combine$andMap,
								author$project$TimeTravel$Internal$Parser$Util$spaces,
								A2(
									andre_dietrich$parser_combinators$Combine$andMap,
									author$project$TimeTravel$Internal$Parser$Parser$propertyKey,
									A2(
										andre_dietrich$parser_combinators$Combine$map,
										F7(
											function (_n6, key, _n7, _n8, _n9, value, _n10) {
												return A2(author$project$TimeTravel$Internal$Parser$AST$Property, key, value);
											}),
										author$project$TimeTravel$Internal$Parser$Util$spaces)))))));
		});
}
function author$project$TimeTravel$Internal$Parser$Parser$cyclic$record() {
	return andre_dietrich$parser_combinators$Combine$lazy(
		function (_n4) {
			return A2(
				andre_dietrich$parser_combinators$Combine$map,
				author$project$TimeTravel$Internal$Parser$AST$Record,
				andre_dietrich$parser_combinators$Combine$braces(
					author$project$TimeTravel$Internal$Parser$Parser$cyclic$properties()));
		});
}
function author$project$TimeTravel$Internal$Parser$Parser$cyclic$tupleLiteral() {
	return andre_dietrich$parser_combinators$Combine$lazy(
		function (_n3) {
			return A2(
				andre_dietrich$parser_combinators$Combine$map,
				author$project$TimeTravel$Internal$Parser$AST$TupleLiteral,
				andre_dietrich$parser_combinators$Combine$parens(
					author$project$TimeTravel$Internal$Parser$Parser$cyclic$items()));
		});
}
function author$project$TimeTravel$Internal$Parser$Parser$cyclic$union() {
	return andre_dietrich$parser_combinators$Combine$lazy(
		function (_n2) {
			return A2(
				andre_dietrich$parser_combinators$Combine$andMap,
				andre_dietrich$parser_combinators$Combine$many(
					author$project$TimeTravel$Internal$Parser$Parser$cyclic$unionParam()),
				A2(
					andre_dietrich$parser_combinators$Combine$map,
					F2(
						function (tag_, tail) {
							return A2(author$project$TimeTravel$Internal$Parser$AST$Union, tag_, tail);
						}),
					author$project$TimeTravel$Internal$Parser$Parser$tag));
		});
}
function author$project$TimeTravel$Internal$Parser$Parser$cyclic$unionParam() {
	return andre_dietrich$parser_combinators$Combine$lazy(
		function (_n0) {
			return A2(
				andre_dietrich$parser_combinators$Combine$andMap,
				A2(
					andre_dietrich$parser_combinators$Combine$or,
					author$project$TimeTravel$Internal$Parser$Parser$singleUnion,
					author$project$TimeTravel$Internal$Parser$Parser$cyclic$expressionWithoutUnion()),
				A2(
					andre_dietrich$parser_combinators$Combine$map,
					F2(
						function (_n1, exp) {
							return exp;
						}),
					author$project$TimeTravel$Internal$Parser$Util$spaces));
		});
}
try {
	var author$project$TimeTravel$Internal$Parser$Parser$expression = author$project$TimeTravel$Internal$Parser$Parser$cyclic$expression();
	author$project$TimeTravel$Internal$Parser$Parser$cyclic$expression = function () {
		return author$project$TimeTravel$Internal$Parser$Parser$expression;
	};
	var author$project$TimeTravel$Internal$Parser$Parser$expressionWithoutUnion = author$project$TimeTravel$Internal$Parser$Parser$cyclic$expressionWithoutUnion();
	author$project$TimeTravel$Internal$Parser$Parser$cyclic$expressionWithoutUnion = function () {
		return author$project$TimeTravel$Internal$Parser$Parser$expressionWithoutUnion;
	};
	var author$project$TimeTravel$Internal$Parser$Parser$items = author$project$TimeTravel$Internal$Parser$Parser$cyclic$items();
	author$project$TimeTravel$Internal$Parser$Parser$cyclic$items = function () {
		return author$project$TimeTravel$Internal$Parser$Parser$items;
	};
	var author$project$TimeTravel$Internal$Parser$Parser$listLiteral = author$project$TimeTravel$Internal$Parser$Parser$cyclic$listLiteral();
	author$project$TimeTravel$Internal$Parser$Parser$cyclic$listLiteral = function () {
		return author$project$TimeTravel$Internal$Parser$Parser$listLiteral;
	};
	var author$project$TimeTravel$Internal$Parser$Parser$properties = author$project$TimeTravel$Internal$Parser$Parser$cyclic$properties();
	author$project$TimeTravel$Internal$Parser$Parser$cyclic$properties = function () {
		return author$project$TimeTravel$Internal$Parser$Parser$properties;
	};
	var author$project$TimeTravel$Internal$Parser$Parser$property = author$project$TimeTravel$Internal$Parser$Parser$cyclic$property();
	author$project$TimeTravel$Internal$Parser$Parser$cyclic$property = function () {
		return author$project$TimeTravel$Internal$Parser$Parser$property;
	};
	var author$project$TimeTravel$Internal$Parser$Parser$record = author$project$TimeTravel$Internal$Parser$Parser$cyclic$record();
	author$project$TimeTravel$Internal$Parser$Parser$cyclic$record = function () {
		return author$project$TimeTravel$Internal$Parser$Parser$record;
	};
	var author$project$TimeTravel$Internal$Parser$Parser$tupleLiteral = author$project$TimeTravel$Internal$Parser$Parser$cyclic$tupleLiteral();
	author$project$TimeTravel$Internal$Parser$Parser$cyclic$tupleLiteral = function () {
		return author$project$TimeTravel$Internal$Parser$Parser$tupleLiteral;
	};
	var author$project$TimeTravel$Internal$Parser$Parser$union = author$project$TimeTravel$Internal$Parser$Parser$cyclic$union();
	author$project$TimeTravel$Internal$Parser$Parser$cyclic$union = function () {
		return author$project$TimeTravel$Internal$Parser$Parser$union;
	};
	var author$project$TimeTravel$Internal$Parser$Parser$unionParam = author$project$TimeTravel$Internal$Parser$Parser$cyclic$unionParam();
	author$project$TimeTravel$Internal$Parser$Parser$cyclic$unionParam = function () {
		return author$project$TimeTravel$Internal$Parser$Parser$unionParam;
	};
} catch ($) {
throw 'Some top-level definitions from `TimeTravel.Internal.Parser.Parser` are causing infinite recursion:\n\n  ┌─────┐\n  │    expression\n  │     ↓\n  │    expressionWithoutUnion\n  │     ↓\n  │    items\n  │     ↓\n  │    listLiteral\n  │     ↓\n  │    properties\n  │     ↓\n  │    property\n  │     ↓\n  │    record\n  │     ↓\n  │    tupleLiteral\n  │     ↓\n  │    union\n  │     ↓\n  │    unionParam\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.0/halting-problem to learn how to fix it!';}
var author$project$TimeTravel$Internal$Parser$Parser$parse = function (s) {
	var _n0 = A2(
		andre_dietrich$parser_combinators$Combine$parse,
		author$project$TimeTravel$Internal$Parser$Util$spaced(author$project$TimeTravel$Internal$Parser$Parser$expression),
		s);
	if (_n0.$ === 'Ok') {
		var _n1 = _n0.a;
		var ast = _n1.c;
		return elm$core$Result$Ok(ast);
	} else {
		var _n2 = _n0.a;
		var errors = _n2.c;
		return elm$core$Result$Err(
			A2(elm$core$String$join, ',', errors));
	}
};
var elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return elm$core$Result$Err(e);
		}
	});
var author$project$TimeTravel$Internal$Model$updateLazyModelAst = function (item) {
	return _Utils_update(
		item,
		{
			lazyModelAst: _Utils_eq(item.lazyModelAst, elm$core$Maybe$Nothing) ? elm$core$Maybe$Just(
				A2(
					elm$core$Result$map,
					author$project$TimeTravel$Internal$Parser$AST$attachId('@'),
					author$project$TimeTravel$Internal$Parser$Parser$parse(
						elm$core$Debug$toString(item.model)))) : item.lazyModelAst
		});
};
var author$project$TimeTravel$Internal$Model$updateLazyAstForWatch = function (model) {
	var _n0 = _Utils_Tuple2(
		model.watch,
		author$project$TimeTravel$Internal$Util$Nel$head(model.history).id);
	if (_n0.a.$ === 'Just') {
		var id = _n0.b;
		return A2(
			author$project$TimeTravel$Internal$Model$mapHistory,
			function (item) {
				return _Utils_eq(item.id, id) ? author$project$TimeTravel$Internal$Model$updateLazyModelAst(item) : item;
			},
			model);
	} else {
		return model;
	}
};
var author$project$TimeTravel$Internal$MsgLike$Message = function (a) {
	return {$: 'Message', a: a};
};
var author$project$TimeTravel$Internal$Util$Nel$cons = F2(
	function (_new, _n0) {
		var h = _n0.a;
		var t = _n0.b;
		return A2(
			author$project$TimeTravel$Internal$Util$Nel$Nel,
			_new,
			A2(elm$core$List$cons, h, t));
	});
var elm$core$Basics$not = _Basics_not;
var author$project$TimeTravel$Internal$Model$updateOnIncomingUserMsg = F4(
	function (transformMsg, update, _n0, model) {
		var causedBy = _n0.a;
		var msg = _n0.b;
		var megLike = author$project$TimeTravel$Internal$MsgLike$Message(msg);
		var _n1 = model.history;
		var last = _n1.a;
		var past = _n1.b;
		var _n2 = A2(update, msg, last.model);
		var newRawUserModel = _n2.a;
		var userCmd = _n2.b;
		var nextItem = A4(author$project$TimeTravel$Internal$Model$newItem, model.msgId, megLike, causedBy, newRawUserModel);
		var newModel = author$project$TimeTravel$Internal$Model$updateLazyAstForWatch(
			author$project$TimeTravel$Internal$Model$selectFirstIfSync(
				_Utils_update(
					model,
					{
						filter: A2(author$project$TimeTravel$Internal$Model$updateFilter, megLike, model.filter),
						future: (!model.sync) ? A2(elm$core$List$cons, nextItem, model.future) : model.future,
						history: model.sync ? A2(author$project$TimeTravel$Internal$Util$Nel$cons, nextItem, model.history) : model.history,
						msgId: model.msgId + 1
					})));
		var cmds = _List_fromArray(
			[
				A2(
				elm$core$Platform$Cmd$map,
				transformMsg,
				A2(
					elm$core$Platform$Cmd$map,
					author$project$TimeTravel$Internal$Model$createTuple(model.msgId),
					userCmd))
			]);
		return _Utils_Tuple2(
			newModel,
			elm$core$Platform$Cmd$batch(cmds));
	});
var author$project$TimeTravel$Internal$Model$Settings = F2(
	function (fixedToLeft, filter) {
		return {filter: filter, fixedToLeft: fixedToLeft};
	});
var elm$json$Json$Decode$bool = _Json_decodeBool;
var elm$json$Json$Decode$index = _Json_decodeIndex;
var elm$json$Json$Decode$list = _Json_decodeList;
var author$project$TimeTravel$Internal$Model$settingsDecoder = A3(
	elm$json$Json$Decode$map2,
	author$project$TimeTravel$Internal$Model$Settings,
	A2(elm$json$Json$Decode$field, 'fixedToLeft', elm$json$Json$Decode$bool),
	A2(
		elm$json$Json$Decode$field,
		'filter',
		elm$json$Json$Decode$list(
			A3(
				elm$json$Json$Decode$map2,
				author$project$TimeTravel$Internal$Model$createTuple,
				A2(elm$json$Json$Decode$index, 0, elm$json$Json$Decode$string),
				A2(elm$json$Json$Decode$index, 1, elm$json$Json$Decode$bool)))));
var elm$json$Json$Decode$decodeString = _Json_runOnString;
var author$project$TimeTravel$Internal$Model$decodeSettings = elm$json$Json$Decode$decodeString(author$project$TimeTravel$Internal$Model$settingsDecoder);
var author$project$TimeTravel$Internal$Util$Nel$concat = F2(
	function (list, _n0) {
		var h = _n0.a;
		var t = _n0.b;
		if (list.b) {
			var head_ = list.a;
			var tail = list.b;
			return A2(
				author$project$TimeTravel$Internal$Util$Nel$Nel,
				head_,
				_Utils_ap(
					tail,
					A2(elm$core$List$cons, h, t)));
		} else {
			return A2(author$project$TimeTravel$Internal$Util$Nel$Nel, h, t);
		}
	});
var author$project$TimeTravel$Internal$Model$futureToHistory = function (model) {
	return _Utils_update(
		model,
		{
			future: _List_Nil,
			history: A2(author$project$TimeTravel$Internal$Util$Nel$concat, model.future, model.history)
		});
};
var elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var author$project$TimeTravel$Internal$Model$encodeTuple = F3(
	function (aEncoder, bEncoder, _n0) {
		var a = _n0.a;
		var b = _n0.b;
		return A2(
			elm$json$Json$Encode$list,
			elm$core$Basics$identity,
			_List_fromArray(
				[
					aEncoder(a),
					bEncoder(b)
				]));
	});
var elm$json$Json$Encode$bool = _Json_wrap;
var elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			elm$core$List$foldl,
			F2(
				function (_n0, obj) {
					var k = _n0.a;
					var v = _n0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var elm$json$Json$Encode$string = _Json_wrap;
var author$project$TimeTravel$Internal$Model$encodeSetting = function (settings) {
	return A2(
		elm$json$Json$Encode$encode,
		0,
		elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'fixedToLeft',
					elm$json$Json$Encode$bool(settings.fixedToLeft)),
					_Utils_Tuple2(
					'filter',
					A2(
						elm$json$Json$Encode$list,
						elm$core$Basics$identity,
						A2(
							elm$core$List$map,
							A2(author$project$TimeTravel$Internal$Model$encodeTuple, elm$json$Json$Encode$string, elm$json$Json$Encode$bool),
							settings.filter)))
				])));
};
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0.a;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var author$project$TimeTravel$Internal$Model$saveSetting = F2(
	function (save, model) {
		return A2(
			elm$core$Platform$Cmd$map,
			elm$core$Basics$never,
			save(
				{
					settings: author$project$TimeTravel$Internal$Model$encodeSetting(
						{filter: model.filter, fixedToLeft: model.fixedToLeft}),
					type_: 'save'
				}));
	});
var author$project$TimeTravel$Internal$Model$updateLazyMsgAst = function (item) {
	return _Utils_update(
		item,
		{
			lazyMsgAst: function () {
				if (_Utils_eq(item.lazyMsgAst, elm$core$Maybe$Nothing)) {
					var _n0 = item.msg;
					if (_n0.$ === 'Message') {
						var msg = _n0.a;
						return elm$core$Maybe$Just(
							A2(
								elm$core$Result$map,
								author$project$TimeTravel$Internal$Parser$AST$attachId('@'),
								author$project$TimeTravel$Internal$Parser$Parser$parse(
									elm$core$Debug$toString(msg))));
					} else {
						return elm$core$Maybe$Just(
							elm$core$Result$Err(''));
					}
				} else {
					return item.lazyMsgAst;
				}
			}()
		});
};
var author$project$TimeTravel$Internal$Model$updateLazyAst = function (model) {
	var _n0 = model.selectedMsg;
	if (_n0.$ === 'Just') {
		var id = _n0.a;
		return A2(
			author$project$TimeTravel$Internal$Model$mapHistory,
			function (item) {
				return (_Utils_eq(item.id, id) || _Utils_eq(item.id, id - 1)) ? A2(elm$core$Basics$composeL, author$project$TimeTravel$Internal$Model$updateLazyMsgAst, author$project$TimeTravel$Internal$Model$updateLazyModelAst)(item) : item;
			},
			model);
	} else {
		return model;
	}
};
var author$project$TimeTravel$Internal$Parser$Formatter$formatHelp = F5(
	function (formatPlain, formatLink, formatListed, formatLong, model) {
		switch (model.$) {
			case 'Plain':
				var s = model.a;
				return formatPlain(s);
			case 'Link':
				var id = model.a;
				var s = model.b;
				return A2(formatLink, id, s);
			case 'Listed':
				var list = model.a;
				return formatListed(list);
			default:
				var id = model.a;
				var alt = model.b;
				var s = model.c;
				return A3(formatLong, id, alt, s);
		}
	});
var author$project$TimeTravel$Internal$Parser$Formatter$formatAsString = function (model) {
	return A5(
		author$project$TimeTravel$Internal$Parser$Formatter$formatHelp,
		elm$core$Basics$identity,
		F2(
			function (_n0, s) {
				return s;
			}),
		A2(
			elm$core$Basics$composeL,
			elm$core$String$join(''),
			elm$core$List$map(author$project$TimeTravel$Internal$Parser$Formatter$formatAsString)),
		F3(
			function (_n1, _n2, children) {
				return A2(
					elm$core$String$join,
					'',
					A2(elm$core$List$map, author$project$TimeTravel$Internal$Parser$Formatter$formatAsString, children));
			}),
		model);
};
var author$project$TimeTravel$Internal$Parser$Formatter$Link = F2(
	function (a, b) {
		return {$: 'Link', a: a, b: b};
	});
var author$project$TimeTravel$Internal$Parser$Formatter$Listed = function (a) {
	return {$: 'Listed', a: a};
};
var author$project$TimeTravel$Internal$Parser$Formatter$Plain = function (a) {
	return {$: 'Plain', a: a};
};
var elm$core$Bitwise$and = _Bitwise_and;
var elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3(elm$core$String$repeatHelp, n, chunk, '');
	});
var author$project$TimeTravel$Internal$Parser$Formatter$indent = function (context) {
	return A2(elm$core$String$repeat, context.nest, '  ');
};
var author$project$TimeTravel$Internal$Parser$Formatter$joinX = F2(
	function (s, list) {
		if (!list.b) {
			return _List_Nil;
		} else {
			if (!list.b.b) {
				var head = list.a;
				return _List_fromArray(
					[head]);
			} else {
				var head = list.a;
				var tail = list.b;
				return A2(
					elm$core$List$cons,
					head,
					A2(
						elm$core$List$cons,
						author$project$TimeTravel$Internal$Parser$Formatter$Plain(s),
						A2(author$project$TimeTravel$Internal$Parser$Formatter$joinX, s, tail)));
			}
		}
	});
var author$project$TimeTravel$Internal$Parser$Formatter$Long = F3(
	function (a, b, c) {
		return {$: 'Long', a: a, b: b, c: c};
	});
var elm$core$Basics$neq = _Utils_notEqual;
var elm$core$String$contains = _String_contains;
var author$project$TimeTravel$Internal$Parser$Formatter$makeModelFromListLike = F7(
	function (canFold, id, indent_, wordsLimit, start, end, list) {
		if (!list.b) {
			return author$project$TimeTravel$Internal$Parser$Formatter$Plain(
				_Utils_ap(start, end));
		} else {
			var singleLine = author$project$TimeTravel$Internal$Parser$Formatter$Listed(
				A2(
					elm$core$List$cons,
					author$project$TimeTravel$Internal$Parser$Formatter$Plain(start + ' '),
					_Utils_ap(
						A2(author$project$TimeTravel$Internal$Parser$Formatter$joinX, ', ', list),
						_List_fromArray(
							[
								author$project$TimeTravel$Internal$Parser$Formatter$Plain(' ' + end)
							]))));
			var singleLineStr = author$project$TimeTravel$Internal$Parser$Formatter$formatAsString(singleLine);
			var _long = (_Utils_cmp(
				elm$core$String$length(singleLineStr),
				wordsLimit) > 0) || A2(elm$core$String$contains, '\n', singleLineStr);
			return (((indent_ !== '') && canFold) && _long) ? A3(
				author$project$TimeTravel$Internal$Parser$Formatter$Long,
				id,
				start + (' .. ' + end),
				A2(
					elm$core$List$cons,
					author$project$TimeTravel$Internal$Parser$Formatter$Plain(start + ' '),
					_Utils_ap(
						A2(author$project$TimeTravel$Internal$Parser$Formatter$joinX, '\n' + (indent_ + ', '), list),
						_Utils_ap(
							_List_fromArray(
								[
									author$project$TimeTravel$Internal$Parser$Formatter$Plain('\n' + indent_)
								]),
							_List_fromArray(
								[
									author$project$TimeTravel$Internal$Parser$Formatter$Plain(end)
								]))))) : (_long ? author$project$TimeTravel$Internal$Parser$Formatter$Listed(
				A2(
					elm$core$List$cons,
					author$project$TimeTravel$Internal$Parser$Formatter$Plain(start + ' '),
					_Utils_ap(
						A2(author$project$TimeTravel$Internal$Parser$Formatter$joinX, '\n' + (indent_ + ', '), list),
						_Utils_ap(
							_List_fromArray(
								[
									author$project$TimeTravel$Internal$Parser$Formatter$Plain('\n' + indent_)
								]),
							_List_fromArray(
								[
									author$project$TimeTravel$Internal$Parser$Formatter$Plain(end)
								]))))) : singleLine);
		}
	});
var elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var author$project$TimeTravel$Internal$Parser$Formatter$makeModelWithContext = F2(
	function (c, ast) {
		switch (ast.$) {
			case 'RecordX':
				var id = ast.a;
				var properties = ast.b;
				return A7(
					author$project$TimeTravel$Internal$Parser$Formatter$makeModelFromListLike,
					true,
					id,
					author$project$TimeTravel$Internal$Parser$Formatter$indent(c),
					c.wordsLimit,
					'{',
					'}',
					A2(
						elm$core$List$map,
						author$project$TimeTravel$Internal$Parser$Formatter$makeModelWithContext(
							_Utils_update(
								c,
								{nest: c.nest + 1})),
						properties));
			case 'PropertyX':
				var id = ast.a;
				var key = ast.b;
				var value = ast.c;
				var s = A2(
					author$project$TimeTravel$Internal$Parser$Formatter$makeModelWithContext,
					_Utils_update(
						c,
						{nest: c.nest + 1, parens: false}),
					value);
				var str = author$project$TimeTravel$Internal$Parser$Formatter$formatAsString(s);
				return author$project$TimeTravel$Internal$Parser$Formatter$Listed(
					A2(
						elm$core$List$cons,
						A2(author$project$TimeTravel$Internal$Parser$Formatter$Link, id, key),
						A2(
							elm$core$List$cons,
							author$project$TimeTravel$Internal$Parser$Formatter$Plain(' = '),
							(A2(elm$core$String$contains, '\n', str) || (_Utils_cmp(
								elm$core$String$length(key + (' = ' + str)),
								c.wordsLimit) > 0)) ? _List_fromArray(
								[
									author$project$TimeTravel$Internal$Parser$Formatter$Plain(
									'\n' + author$project$TimeTravel$Internal$Parser$Formatter$indent(
										_Utils_update(
											c,
											{nest: c.nest + 1}))),
									s
								]) : _List_fromArray(
								[s]))));
			case 'StringLiteralX':
				var id = ast.a;
				var s = ast.b;
				return author$project$TimeTravel$Internal$Parser$Formatter$Plain('\"' + (s + '\"'));
			case 'ValueX':
				var id = ast.a;
				var s = ast.b;
				return author$project$TimeTravel$Internal$Parser$Formatter$Plain(s);
			case 'UnionX':
				var id = ast.a;
				var tag = ast.b;
				var tail = ast.c;
				var tailX = A2(
					elm$core$List$map,
					author$project$TimeTravel$Internal$Parser$Formatter$makeModelWithContext(
						_Utils_update(
							c,
							{nest: c.nest + 1, parens: true})),
					tail);
				var joinedTailStr = author$project$TimeTravel$Internal$Parser$Formatter$formatAsString(
					author$project$TimeTravel$Internal$Parser$Formatter$Listed(tailX));
				var multiLine = A2(elm$core$String$contains, '\n', joinedTailStr) || (_Utils_cmp(
					elm$core$String$length(
						_Utils_ap(tag, joinedTailStr)),
					c.wordsLimit) > 0);
				var s = author$project$TimeTravel$Internal$Parser$Formatter$Listed(
					multiLine ? A2(
						elm$core$List$cons,
						author$project$TimeTravel$Internal$Parser$Formatter$Plain(
							tag + ('\n' + author$project$TimeTravel$Internal$Parser$Formatter$indent(
								_Utils_update(
									c,
									{nest: c.nest + 1})))),
						A2(
							author$project$TimeTravel$Internal$Parser$Formatter$joinX,
							'\n' + author$project$TimeTravel$Internal$Parser$Formatter$indent(
								_Utils_update(
									c,
									{nest: c.nest + 1})),
							tailX)) : A2(
						author$project$TimeTravel$Internal$Parser$Formatter$joinX,
						' ',
						A2(
							elm$core$List$cons,
							author$project$TimeTravel$Internal$Parser$Formatter$Plain(tag),
							tailX)));
				return ((!elm$core$List$isEmpty(tail)) && c.parens) ? author$project$TimeTravel$Internal$Parser$Formatter$Listed(
					_List_fromArray(
						[
							author$project$TimeTravel$Internal$Parser$Formatter$Plain('('),
							s,
							author$project$TimeTravel$Internal$Parser$Formatter$Plain(
							multiLine ? ('\n' + (author$project$TimeTravel$Internal$Parser$Formatter$indent(c) + ')')) : ')')
						])) : s;
			case 'ListLiteralX':
				var id = ast.a;
				var list = ast.b;
				return A7(
					author$project$TimeTravel$Internal$Parser$Formatter$makeModelFromListLike,
					true,
					id,
					author$project$TimeTravel$Internal$Parser$Formatter$indent(c),
					c.wordsLimit,
					'[',
					']',
					A2(
						elm$core$List$map,
						author$project$TimeTravel$Internal$Parser$Formatter$makeModelWithContext(
							_Utils_update(
								c,
								{nest: c.nest + 1, parens: false})),
						list));
			default:
				var id = ast.a;
				var list = ast.b;
				return A7(
					author$project$TimeTravel$Internal$Parser$Formatter$makeModelFromListLike,
					false,
					id,
					author$project$TimeTravel$Internal$Parser$Formatter$indent(c),
					c.wordsLimit,
					'(',
					')',
					A2(
						elm$core$List$map,
						author$project$TimeTravel$Internal$Parser$Formatter$makeModelWithContext(
							_Utils_update(
								c,
								{nest: c.nest + 1, parens: false})),
						list));
		}
	});
var author$project$TimeTravel$Internal$Parser$Formatter$makeModel = author$project$TimeTravel$Internal$Parser$Formatter$makeModelWithContext(
	{nest: 0, parens: false, wordsLimit: 40});
var elm$core$String$lines = _String_lines;
var elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, list);
			var jsArray = _n0.a;
			var remainingItems = _n0.b;
			if (_Utils_cmp(
				elm$core$Elm$JsArray$length(jsArray),
				elm$core$Array$branchFactor) < 0) {
				return A2(
					elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					elm$core$List$cons,
					elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return elm$core$Array$empty;
	} else {
		return A3(elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var elm$core$Array$bitMask = 4294967295 >>> (32 - elm$core$Array$shiftStep);
var elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = elm$core$Array$bitMask & (index >>> shift);
			var _n0 = A2(elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_n0.$ === 'SubTree') {
				var subTree = _n0.a;
				var $temp$shift = shift - elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _n0.a;
				return A2(elm$core$Elm$JsArray$unsafeGet, elm$core$Array$bitMask & index, values);
			}
		}
	});
var elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var elm$core$Basics$ge = _Utils_ge;
var elm$core$Array$get = F2(
	function (index, _n0) {
		var len = _n0.a;
		var startShift = _n0.b;
		var tree = _n0.c;
		var tail = _n0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			elm$core$Array$tailIndex(len)) > -1) ? elm$core$Maybe$Just(
			A2(elm$core$Elm$JsArray$unsafeGet, elm$core$Array$bitMask & index, tail)) : elm$core$Maybe$Just(
			A3(elm$core$Array$getHelp, startShift, index, tree)));
	});
var elm$core$Array$length = function (_n0) {
	var len = _n0.a;
	return len;
};
var jinjor$elm_diff$Diff$Added = function (a) {
	return {$: 'Added', a: a};
};
var jinjor$elm_diff$Diff$CannotGetA = function (a) {
	return {$: 'CannotGetA', a: a};
};
var jinjor$elm_diff$Diff$CannotGetB = function (a) {
	return {$: 'CannotGetB', a: a};
};
var jinjor$elm_diff$Diff$NoChange = function (a) {
	return {$: 'NoChange', a: a};
};
var jinjor$elm_diff$Diff$Removed = function (a) {
	return {$: 'Removed', a: a};
};
var jinjor$elm_diff$Diff$UnexpectedPath = F2(
	function (a, b) {
		return {$: 'UnexpectedPath', a: a, b: b};
	});
var jinjor$elm_diff$Diff$makeChangesHelp = F5(
	function (changes, getA, getB, _n0, path) {
		makeChangesHelp:
		while (true) {
			var x = _n0.a;
			var y = _n0.b;
			if (!path.b) {
				return elm$core$Result$Ok(changes);
			} else {
				var _n2 = path.a;
				var prevX = _n2.a;
				var prevY = _n2.b;
				var tail = path.b;
				var change = function () {
					if (_Utils_eq(x - 1, prevX) && _Utils_eq(y - 1, prevY)) {
						var _n4 = getA(x);
						if (_n4.$ === 'Just') {
							var a = _n4.a;
							return elm$core$Result$Ok(
								jinjor$elm_diff$Diff$NoChange(a));
						} else {
							return elm$core$Result$Err(
								jinjor$elm_diff$Diff$CannotGetA(x));
						}
					} else {
						if (_Utils_eq(x, prevX)) {
							var _n5 = getB(y);
							if (_n5.$ === 'Just') {
								var b = _n5.a;
								return elm$core$Result$Ok(
									jinjor$elm_diff$Diff$Added(b));
							} else {
								return elm$core$Result$Err(
									jinjor$elm_diff$Diff$CannotGetB(y));
							}
						} else {
							if (_Utils_eq(y, prevY)) {
								var _n6 = getA(x);
								if (_n6.$ === 'Just') {
									var a = _n6.a;
									return elm$core$Result$Ok(
										jinjor$elm_diff$Diff$Removed(a));
								} else {
									return elm$core$Result$Err(
										jinjor$elm_diff$Diff$CannotGetA(x));
								}
							} else {
								return elm$core$Result$Err(
									A2(
										jinjor$elm_diff$Diff$UnexpectedPath,
										_Utils_Tuple2(x, y),
										path));
							}
						}
					}
				}();
				if (change.$ === 'Ok') {
					var c = change.a;
					var $temp$changes = A2(elm$core$List$cons, c, changes),
						$temp$getA = getA,
						$temp$getB = getB,
						$temp$_n0 = _Utils_Tuple2(prevX, prevY),
						$temp$path = tail;
					changes = $temp$changes;
					getA = $temp$getA;
					getB = $temp$getB;
					_n0 = $temp$_n0;
					path = $temp$path;
					continue makeChangesHelp;
				} else {
					var e = change.a;
					return elm$core$Result$Err(e);
				}
			}
		}
	});
var jinjor$elm_diff$Diff$makeChanges = F3(
	function (getA, getB, path) {
		if (!path.b) {
			return elm$core$Result$Ok(_List_Nil);
		} else {
			var latest = path.a;
			var tail = path.b;
			return A5(jinjor$elm_diff$Diff$makeChangesHelp, _List_Nil, getA, getB, latest, tail);
		}
	});
var elm$core$Basics$negate = function (n) {
	return -n;
};
var jinjor$elm_diff$Diff$Continue = function (a) {
	return {$: 'Continue', a: a};
};
var jinjor$elm_diff$Diff$Found = function (a) {
	return {$: 'Found', a: a};
};
var elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var elm$core$Array$setHelp = F4(
	function (shift, index, value, tree) {
		var pos = elm$core$Array$bitMask & (index >>> shift);
		var _n0 = A2(elm$core$Elm$JsArray$unsafeGet, pos, tree);
		if (_n0.$ === 'SubTree') {
			var subTree = _n0.a;
			var newSub = A4(elm$core$Array$setHelp, shift - elm$core$Array$shiftStep, index, value, subTree);
			return A3(
				elm$core$Elm$JsArray$unsafeSet,
				pos,
				elm$core$Array$SubTree(newSub),
				tree);
		} else {
			var values = _n0.a;
			var newLeaf = A3(elm$core$Elm$JsArray$unsafeSet, elm$core$Array$bitMask & index, value, values);
			return A3(
				elm$core$Elm$JsArray$unsafeSet,
				pos,
				elm$core$Array$Leaf(newLeaf),
				tree);
		}
	});
var elm$core$Array$set = F3(
	function (index, value, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? array : ((_Utils_cmp(
			index,
			elm$core$Array$tailIndex(len)) > -1) ? A4(
			elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			tree,
			A3(elm$core$Elm$JsArray$unsafeSet, elm$core$Array$bitMask & index, value, tail)) : A4(
			elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A4(elm$core$Array$setHelp, startShift, index, value, tree),
			tail));
	});
var jinjor$elm_diff$Diff$step = F4(
	function (snake_, offset, k, v) {
		var fromTop = A2(
			elm$core$Maybe$withDefault,
			_List_Nil,
			A2(elm$core$Array$get, (k + 1) + offset, v));
		var fromLeft = A2(
			elm$core$Maybe$withDefault,
			_List_Nil,
			A2(elm$core$Array$get, (k - 1) + offset, v));
		var _n0 = function () {
			var _n2 = _Utils_Tuple2(fromLeft, fromTop);
			if (!_n2.a.b) {
				if (!_n2.b.b) {
					return _Utils_Tuple2(
						_List_Nil,
						_Utils_Tuple2(0, 0));
				} else {
					var _n3 = _n2.b;
					var _n4 = _n3.a;
					var topX = _n4.a;
					var topY = _n4.b;
					return _Utils_Tuple2(
						fromTop,
						_Utils_Tuple2(topX + 1, topY));
				}
			} else {
				if (!_n2.b.b) {
					var _n5 = _n2.a;
					var _n6 = _n5.a;
					var leftX = _n6.a;
					var leftY = _n6.b;
					return _Utils_Tuple2(
						fromLeft,
						_Utils_Tuple2(leftX, leftY + 1));
				} else {
					var _n7 = _n2.a;
					var _n8 = _n7.a;
					var leftX = _n8.a;
					var leftY = _n8.b;
					var _n9 = _n2.b;
					var _n10 = _n9.a;
					var topX = _n10.a;
					var topY = _n10.b;
					return (_Utils_cmp(leftY + 1, topY) > -1) ? _Utils_Tuple2(
						fromLeft,
						_Utils_Tuple2(leftX, leftY + 1)) : _Utils_Tuple2(
						fromTop,
						_Utils_Tuple2(topX + 1, topY));
				}
			}
		}();
		var path = _n0.a;
		var _n1 = _n0.b;
		var x = _n1.a;
		var y = _n1.b;
		var _n11 = A3(
			snake_,
			x + 1,
			y + 1,
			A2(
				elm$core$List$cons,
				_Utils_Tuple2(x, y),
				path));
		var newPath = _n11.a;
		var goal = _n11.b;
		return goal ? jinjor$elm_diff$Diff$Found(newPath) : jinjor$elm_diff$Diff$Continue(
			A3(elm$core$Array$set, k + offset, newPath, v));
	});
var jinjor$elm_diff$Diff$onpLoopK = F4(
	function (snake_, offset, ks, v) {
		onpLoopK:
		while (true) {
			if (!ks.b) {
				return jinjor$elm_diff$Diff$Continue(v);
			} else {
				var k = ks.a;
				var ks_ = ks.b;
				var _n1 = A4(jinjor$elm_diff$Diff$step, snake_, offset, k, v);
				if (_n1.$ === 'Found') {
					var path = _n1.a;
					return jinjor$elm_diff$Diff$Found(path);
				} else {
					var v_ = _n1.a;
					var $temp$snake_ = snake_,
						$temp$offset = offset,
						$temp$ks = ks_,
						$temp$v = v_;
					snake_ = $temp$snake_;
					offset = $temp$offset;
					ks = $temp$ks;
					v = $temp$v;
					continue onpLoopK;
				}
			}
		}
	});
var jinjor$elm_diff$Diff$onpLoopP = F5(
	function (snake_, delta, offset, p, v) {
		onpLoopP:
		while (true) {
			var ks = (delta > 0) ? _Utils_ap(
				elm$core$List$reverse(
					A2(elm$core$List$range, delta + 1, delta + p)),
				A2(elm$core$List$range, -p, delta)) : _Utils_ap(
				elm$core$List$reverse(
					A2(elm$core$List$range, delta + 1, p)),
				A2(elm$core$List$range, (-p) + delta, delta));
			var _n0 = A4(jinjor$elm_diff$Diff$onpLoopK, snake_, offset, ks, v);
			if (_n0.$ === 'Found') {
				var path = _n0.a;
				return path;
			} else {
				var v_ = _n0.a;
				var $temp$snake_ = snake_,
					$temp$delta = delta,
					$temp$offset = offset,
					$temp$p = p + 1,
					$temp$v = v_;
				snake_ = $temp$snake_;
				delta = $temp$delta;
				offset = $temp$offset;
				p = $temp$p;
				v = $temp$v;
				continue onpLoopP;
			}
		}
	});
var jinjor$elm_diff$Diff$snake = F5(
	function (getA, getB, nextX, nextY, path) {
		snake:
		while (true) {
			var _n0 = _Utils_Tuple2(
				getA(nextX),
				getB(nextY));
			_n0$2:
			while (true) {
				if (_n0.a.$ === 'Just') {
					if (_n0.b.$ === 'Just') {
						var a = _n0.a.a;
						var b = _n0.b.a;
						if (_Utils_eq(a, b)) {
							var $temp$getA = getA,
								$temp$getB = getB,
								$temp$nextX = nextX + 1,
								$temp$nextY = nextY + 1,
								$temp$path = A2(
								elm$core$List$cons,
								_Utils_Tuple2(nextX, nextY),
								path);
							getA = $temp$getA;
							getB = $temp$getB;
							nextX = $temp$nextX;
							nextY = $temp$nextY;
							path = $temp$path;
							continue snake;
						} else {
							return _Utils_Tuple2(path, false);
						}
					} else {
						break _n0$2;
					}
				} else {
					if (_n0.b.$ === 'Nothing') {
						var _n1 = _n0.a;
						var _n2 = _n0.b;
						return _Utils_Tuple2(path, true);
					} else {
						break _n0$2;
					}
				}
			}
			return _Utils_Tuple2(path, false);
		}
	});
var jinjor$elm_diff$Diff$onp = F4(
	function (getA, getB, m, n) {
		var v = A2(
			elm$core$Array$initialize,
			(m + n) + 1,
			elm$core$Basics$always(_List_Nil));
		var delta = n - m;
		return A5(
			jinjor$elm_diff$Diff$onpLoopP,
			A2(jinjor$elm_diff$Diff$snake, getA, getB),
			delta,
			m,
			0,
			v);
	});
var jinjor$elm_diff$Diff$testDiff = F2(
	function (a, b) {
		var arrB = elm$core$Array$fromList(b);
		var getB = function (y) {
			return A2(elm$core$Array$get, y - 1, arrB);
		};
		var n = elm$core$Array$length(arrB);
		var arrA = elm$core$Array$fromList(a);
		var getA = function (x) {
			return A2(elm$core$Array$get, x - 1, arrA);
		};
		var m = elm$core$Array$length(arrA);
		var path = A4(jinjor$elm_diff$Diff$onp, getA, getB, m, n);
		return A3(jinjor$elm_diff$Diff$makeChanges, getA, getB, path);
	});
var jinjor$elm_diff$Diff$diff = F2(
	function (a, b) {
		var _n0 = A2(jinjor$elm_diff$Diff$testDiff, a, b);
		if (_n0.$ === 'Ok') {
			var changes = _n0.a;
			return changes;
		} else {
			return _List_Nil;
		}
	});
var jinjor$elm_diff$Diff$diffLines = F2(
	function (a, b) {
		return A2(
			jinjor$elm_diff$Diff$diff,
			elm$core$String$lines(a),
			elm$core$String$lines(b));
	});
var author$project$TimeTravel$Internal$Model$makeChanges = F2(
	function (oldAst, newAst) {
		return _Utils_eq(oldAst, newAst) ? _List_Nil : A2(
			jinjor$elm_diff$Diff$diffLines,
			author$project$TimeTravel$Internal$Parser$Formatter$formatAsString(
				author$project$TimeTravel$Internal$Parser$Formatter$makeModel(oldAst)),
			author$project$TimeTravel$Internal$Parser$Formatter$formatAsString(
				author$project$TimeTravel$Internal$Parser$Formatter$makeModel(newAst)));
	});
var author$project$TimeTravel$Internal$Util$Nel$findMapManyHelp = F4(
	function (result, n, f, list) {
		findMapManyHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				if (!list.b) {
					return result;
				} else {
					var h = list.a;
					var t = list.b;
					var _n1 = f(h);
					if (_n1.$ === 'Just') {
						var b = _n1.a;
						var $temp$result = A2(elm$core$List$cons, b, result),
							$temp$n = n - 1,
							$temp$f = f,
							$temp$list = t;
						result = $temp$result;
						n = $temp$n;
						f = $temp$f;
						list = $temp$list;
						continue findMapManyHelp;
					} else {
						var $temp$result = result,
							$temp$n = n,
							$temp$f = f,
							$temp$list = t;
						result = $temp$result;
						n = $temp$n;
						f = $temp$f;
						list = $temp$list;
						continue findMapManyHelp;
					}
				}
			}
		}
	});
var author$project$TimeTravel$Internal$Util$Nel$toList = function (_n0) {
	var head_ = _n0.a;
	var tail = _n0.b;
	return A2(elm$core$List$cons, head_, tail);
};
var author$project$TimeTravel$Internal$Util$Nel$findMapMany = F3(
	function (n, f, nel) {
		return elm$core$List$reverse(
			A4(
				author$project$TimeTravel$Internal$Util$Nel$findMapManyHelp,
				_List_Nil,
				n,
				f,
				author$project$TimeTravel$Internal$Util$Nel$toList(nel)));
	});
var author$project$TimeTravel$Internal$Model$selectedAndOldAst = function (model) {
	var _n0 = model.selectedMsg;
	if (_n0.$ === 'Just') {
		var id = _n0.a;
		var newAndOld = A3(
			author$project$TimeTravel$Internal$Util$Nel$findMapMany,
			2,
			function (item) {
				return (_Utils_eq(item.id, id) || _Utils_eq(item.id, id - 1)) ? elm$core$Maybe$Just(item.lazyModelAst) : elm$core$Maybe$Nothing;
			},
			model.history);
		_n1$2:
		while (true) {
			if ((newAndOld.b && (newAndOld.a.$ === 'Just')) && (newAndOld.a.a.$ === 'Ok')) {
				if (newAndOld.b.b) {
					if ((newAndOld.b.a.$ === 'Just') && (newAndOld.b.a.a.$ === 'Ok')) {
						var newAst = newAndOld.a.a.a;
						var _n2 = newAndOld.b;
						var oldAst = _n2.a.a.a;
						return elm$core$Maybe$Just(
							_Utils_Tuple2(oldAst, newAst));
					} else {
						break _n1$2;
					}
				} else {
					var ast = newAndOld.a.a.a;
					return elm$core$Maybe$Just(
						_Utils_Tuple2(ast, ast));
				}
			} else {
				break _n1$2;
			}
		}
		return elm$core$Maybe$Nothing;
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var author$project$TimeTravel$Internal$Model$updateLazyDiffHelp = F2(
	function (model, item) {
		var newDiff = function () {
			var _n0 = item.lazyDiff;
			if (_n0.$ === 'Just') {
				var changes = _n0.a;
				return elm$core$Maybe$Just(changes);
			} else {
				var _n1 = author$project$TimeTravel$Internal$Model$selectedAndOldAst(model);
				if (_n1.$ === 'Just') {
					var _n2 = _n1.a;
					var oldAst = _n2.a;
					var newAst = _n2.b;
					return elm$core$Maybe$Just(
						A2(author$project$TimeTravel$Internal$Model$makeChanges, oldAst, newAst));
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}();
		return _Utils_update(
			item,
			{lazyDiff: newDiff});
	});
var author$project$TimeTravel$Internal$Model$updateLazyDiff = function (model) {
	if (model.showModelDetail) {
		return model;
	} else {
		var _n0 = model.selectedMsg;
		if (_n0.$ === 'Just') {
			var id = _n0.a;
			return A2(
				author$project$TimeTravel$Internal$Model$mapHistory,
				function (item) {
					return _Utils_eq(item.id, id) ? A2(author$project$TimeTravel$Internal$Model$updateLazyDiffHelp, model, item) : item;
				},
				model);
		} else {
			return model;
		}
	}
};
var elm$core$Dict$Black = {$: 'Black'};
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Basics$compare = _Utils_compare;
var elm$core$Dict$Red = {$: 'Red'};
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _n5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _n6 = left.d;
				var _n7 = _n6.a;
				var llK = _n6.b;
				var llV = _n6.c;
				var llLeft = _n6.d;
				var llRight = _n6.e;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1.$) {
				case 'LT':
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5(elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3(elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _n0 = A3(elm$core$Dict$insertHelp, key, value, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Set$insert = F2(
	function (key, _n0) {
		var dict = _n0.a;
		return elm$core$Set$Set_elm_builtin(
			A3(elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _n1 = A2(elm$core$Basics$compare, targetKey, key);
				switch (_n1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var elm$core$Dict$member = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$get, key, dict);
		if (_n0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var elm$core$Set$member = F2(
	function (key, _n0) {
		var dict = _n0.a;
		return A2(elm$core$Dict$member, key, dict);
	});
var elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var lLeft = _n1.d;
			var lRight = _n1.e;
			var _n2 = dict.e;
			var rClr = _n2.a;
			var rK = _n2.b;
			var rV = _n2.c;
			var rLeft = _n2.d;
			var _n3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _n2.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n4 = dict.d;
			var lClr = _n4.a;
			var lK = _n4.b;
			var lV = _n4.c;
			var lLeft = _n4.d;
			var lRight = _n4.e;
			var _n5 = dict.e;
			var rClr = _n5.a;
			var rK = _n5.b;
			var rV = _n5.c;
			var rLeft = _n5.d;
			var rRight = _n5.e;
			if (clr.$ === 'Black') {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var _n2 = _n1.d;
			var _n3 = _n2.a;
			var llK = _n2.b;
			var llV = _n2.c;
			var llLeft = _n2.d;
			var llRight = _n2.e;
			var lRight = _n1.e;
			var _n4 = dict.e;
			var rClr = _n4.a;
			var rK = _n4.b;
			var rV = _n4.c;
			var rLeft = _n4.d;
			var rRight = _n4.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				elm$core$Dict$Red,
				lK,
				lV,
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n5 = dict.d;
			var lClr = _n5.a;
			var lK = _n5.b;
			var lV = _n5.c;
			var lLeft = _n5.d;
			var lRight = _n5.e;
			var _n6 = dict.e;
			var rClr = _n6.a;
			var rK = _n6.b;
			var rV = _n6.c;
			var rLeft = _n6.d;
			var rRight = _n6.e;
			if (clr.$ === 'Black') {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _n1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_n2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _n3 = right.a;
							var _n4 = right.d;
							var _n5 = _n4.a;
							return elm$core$Dict$moveRedRight(dict);
						} else {
							break _n2$2;
						}
					} else {
						var _n6 = right.a;
						var _n7 = right.d;
						return elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _n2$2;
				}
			}
			return dict;
		}
	});
var elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _n3 = lLeft.a;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					elm$core$Dict$removeMin(left),
					right);
			} else {
				var _n4 = elm$core$Dict$moveRedLeft(dict);
				if (_n4.$ === 'RBNode_elm_builtin') {
					var nColor = _n4.a;
					var nKey = _n4.b;
					var nValue = _n4.c;
					var nLeft = _n4.d;
					var nRight = _n4.e;
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _n4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _n6 = lLeft.a;
						return A5(
							elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2(elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _n7 = elm$core$Dict$moveRedLeft(dict);
						if (_n7.$ === 'RBNode_elm_builtin') {
							var nColor = _n7.a;
							var nKey = _n7.b;
							var nValue = _n7.c;
							var nLeft = _n7.d;
							var nRight = _n7.e;
							return A5(
								elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2(elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2(elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7(elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _n1 = elm$core$Dict$getMin(right);
				if (_n1.$ === 'RBNode_elm_builtin') {
					var minKey = _n1.b;
					var minValue = _n1.c;
					return A5(
						elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						elm$core$Dict$removeMin(right));
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2(elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var elm$core$Dict$remove = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$removeHelp, key, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Set$remove = F2(
	function (key, _n0) {
		var dict = _n0.a;
		return elm$core$Set$Set_elm_builtin(
			A2(elm$core$Dict$remove, key, dict));
	});
var author$project$TimeTravel$Internal$Update$toggleSet = F2(
	function (a, set) {
		return A2(
			A2(elm$core$Set$member, a, set) ? elm$core$Set$remove : elm$core$Set$insert,
			a,
			set);
	});
var elm$core$Debug$log = _Debug_log;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var author$project$TimeTravel$Internal$Update$update = F3(
	function (save, message, model) {
		switch (message.$) {
			case 'Receive':
				var incomingMsg = message.a;
				if (incomingMsg.type_ === 'load') {
					var _n1 = author$project$TimeTravel$Internal$Model$decodeSettings(incomingMsg.settings);
					if (_n1.$ === 'Ok') {
						var fixedToLeft = _n1.a.fixedToLeft;
						var filter = _n1.a.filter;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{filter: filter, fixedToLeft: fixedToLeft}),
							elm$core$Platform$Cmd$none);
					} else {
						var error = _n1.a;
						return A2(
							elm$core$Debug$log,
							'err decoding',
							_Utils_Tuple2(model, elm$core$Platform$Cmd$none));
					}
				} else {
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				}
			case 'ToggleSync':
				var nextSync = !model.sync;
				var newModel = (nextSync ? author$project$TimeTravel$Internal$Model$futureToHistory : elm$core$Basics$identity)(
					author$project$TimeTravel$Internal$Model$selectFirstIfSync(
						_Utils_update(
							model,
							{
								selectedMsg: nextSync ? elm$core$Maybe$Nothing : model.selectedMsg,
								showModelDetail: false,
								sync: nextSync
							})));
				return _Utils_Tuple2(newModel, elm$core$Platform$Cmd$none);
			case 'ToggleExpand':
				var newModel = _Utils_update(
					model,
					{expand: !model.expand});
				return _Utils_Tuple2(newModel, elm$core$Platform$Cmd$none);
			case 'ToggleFilter':
				var name = message.a;
				var newModel = _Utils_update(
					model,
					{
						filter: A2(
							elm$core$List$map,
							function (_n2) {
								var name_ = _n2.a;
								var visible = _n2.b;
								return _Utils_eq(name, name_) ? _Utils_Tuple2(name_, !visible) : _Utils_Tuple2(name_, visible);
							},
							model.filter)
					});
				return _Utils_Tuple2(
					newModel,
					elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								A2(author$project$TimeTravel$Internal$Model$saveSetting, save, newModel)
							])));
			case 'SelectMsg':
				var id = message.a;
				var newModel = author$project$TimeTravel$Internal$Model$updateLazyDiff(
					author$project$TimeTravel$Internal$Model$updateLazyAst(
						_Utils_update(
							model,
							{
								selectedMsg: elm$core$Maybe$Just(id),
								sync: false
							})));
				return _Utils_Tuple2(newModel, elm$core$Platform$Cmd$none);
			case 'Resync':
				var newModel = author$project$TimeTravel$Internal$Model$futureToHistory(
					author$project$TimeTravel$Internal$Model$selectFirstIfSync(
						_Utils_update(
							model,
							{sync: true})));
				return _Utils_Tuple2(newModel, elm$core$Platform$Cmd$none);
			case 'ToggleLayout':
				var newModel = _Utils_update(
					model,
					{fixedToLeft: !model.fixedToLeft});
				return _Utils_Tuple2(
					newModel,
					elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								A2(author$project$TimeTravel$Internal$Model$saveSetting, save, newModel)
							])));
			case 'ToggleModelDetail':
				var showModelDetail = message.a;
				return _Utils_Tuple2(
					author$project$TimeTravel$Internal$Model$updateLazyDiff(
						_Utils_update(
							model,
							{showModelDetail: showModelDetail})),
					elm$core$Platform$Cmd$none);
			case 'ToggleModelTree':
				var id = message.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							expandedTree: A2(author$project$TimeTravel$Internal$Update$toggleSet, id, model.expandedTree)
						}),
					elm$core$Platform$Cmd$none);
			case 'ToggleMinimize':
				return _Utils_Tuple2(
					author$project$TimeTravel$Internal$Model$futureToHistory(
						author$project$TimeTravel$Internal$Model$selectFirstIfSync(
							_Utils_update(
								model,
								{minimized: !model.minimized, sync: true}))),
					elm$core$Platform$Cmd$none);
			case 'InputModelFilter':
				var s = message.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{modelFilter: s}),
					elm$core$Platform$Cmd$none);
			case 'SelectModelFilter':
				var id = message.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{modelFilter: id}),
					elm$core$Platform$Cmd$none);
			case 'SelectModelFilterWatch':
				var id = message.a;
				return _Utils_Tuple2(
					author$project$TimeTravel$Internal$Model$updateLazyAstForWatch(
						_Utils_update(
							model,
							{
								modelFilter: id,
								watch: elm$core$Maybe$Just(id)
							})),
					elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{watch: elm$core$Maybe$Nothing}),
					elm$core$Platform$Cmd$none);
		}
	});
var author$project$TimeTravel$Internal$Update$updateAfterUserMsg = F2(
	function (save, model) {
		return _Utils_Tuple2(
			model,
			elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						A2(author$project$TimeTravel$Internal$Model$saveSetting, save, model)
					])));
	});
var author$project$TimeTravel$Browser$wrapUpdate = F4(
	function (update, outgoingMsg, msg, model) {
		if (msg.$ === 'UserMsg') {
			var msgWithId = msg.a;
			var _n1 = A4(
				author$project$TimeTravel$Internal$Model$updateOnIncomingUserMsg,
				function (_n2) {
					var id = _n2.a;
					var msg_ = _n2.b;
					return author$project$TimeTravel$Browser$UserMsg(
						_Utils_Tuple2(
							elm$core$Maybe$Just(id),
							msg_));
				},
				update,
				msgWithId,
				model);
			var m = _n1.a;
			var c1 = _n1.b;
			var _n3 = A2(author$project$TimeTravel$Internal$Update$updateAfterUserMsg, outgoingMsg, m);
			var m_ = _n3.a;
			var c2 = _n3.b;
			return _Utils_Tuple2(
				m_,
				elm$core$Platform$Cmd$batch(
					_List_fromArray(
						[
							c1,
							A2(elm$core$Platform$Cmd$map, author$project$TimeTravel$Browser$DebuggerMsg, c2)
						])));
		} else {
			var msg_ = msg.a;
			var _n4 = A3(author$project$TimeTravel$Internal$Update$update, outgoingMsg, msg_, model);
			var m = _n4.a;
			var c = _n4.b;
			return _Utils_Tuple2(
				m,
				elm$core$Platform$Cmd$batch(
					_List_fromArray(
						[
							A2(elm$core$Platform$Cmd$map, author$project$TimeTravel$Browser$DebuggerMsg, c)
						])));
		}
	});
var avh4$elm_color$Color$toRgba = function (_n0) {
	var r = _n0.a;
	var g = _n0.b;
	var b = _n0.c;
	var a = _n0.d;
	return {alpha: a, blue: b, green: g, red: r};
};
var elm$core$String$fromFloat = _String_fromNumber;
var author$project$Material$Icons$Internal$toRgbaString = function (color) {
	var _n0 = avh4$elm_color$Color$toRgba(color);
	var red = _n0.red;
	var green = _n0.green;
	var blue = _n0.blue;
	var alpha = _n0.alpha;
	return 'rgba(' + (elm$core$String$fromFloat(red) + (',' + (elm$core$String$fromFloat(green) + (',' + (elm$core$String$fromFloat(blue) + (',' + (elm$core$String$fromFloat(alpha) + ')')))))));
};
var elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var elm$svg$Svg$g = elm$svg$Svg$trustedNode('g');
var elm$svg$Svg$svg = elm$svg$Svg$trustedNode('svg');
var elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var author$project$Material$Icons$Internal$icon = F4(
	function (viewBox, children, color, size) {
		var stringSize = elm$core$String$fromInt(size);
		var stringColor = author$project$Material$Icons$Internal$toRgbaString(color);
		return A2(
			elm$svg$Svg$svg,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$width(stringSize),
					elm$svg$Svg$Attributes$height(stringSize),
					elm$svg$Svg$Attributes$viewBox(viewBox)
				]),
			_List_fromArray(
				[
					A2(
					elm$svg$Svg$g,
					_List_fromArray(
						[
							elm$svg$Svg$Attributes$fill(stringColor)
						]),
					children)
				]));
	});
var elm$svg$Svg$path = elm$svg$Svg$trustedNode('path');
var elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var author$project$Material$Icons$Content$add = A2(
	author$project$Material$Icons$Internal$icon,
	'0 0 48 48',
	_List_fromArray(
		[
			A2(
			elm$svg$Svg$path,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$d('M38 26H26v12h-4V26H10v-4h12V10h4v12h12v4z')
				]),
			_List_Nil)
		]));
var author$project$Material$Icons$Content$remove = A2(
	author$project$Material$Icons$Internal$icon,
	'0 0 48 48',
	_List_fromArray(
		[
			A2(
			elm$svg$Svg$path,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$d('M38 26H10v-4h28v4z')
				]),
			_List_Nil)
		]));
var avh4$elm_color$Color$RgbaSpace = F4(
	function (a, b, c, d) {
		return {$: 'RgbaSpace', a: a, b: b, c: c, d: d};
	});
var avh4$elm_color$Color$white = A4(avh4$elm_color$Color$RgbaSpace, 255 / 255, 255 / 255, 255 / 255, 1.0);
var author$project$TimeTravel$Internal$Icons$minimize = function (minimized) {
	return A2(
		minimized ? author$project$Material$Icons$Content$add : author$project$Material$Icons$Content$remove,
		avh4$elm_color$Color$white,
		24);
};
var author$project$TimeTravel$Internal$Model$ToggleMinimize = {$: 'ToggleMinimize'};
var author$project$TimeTravel$Internal$Styles$debugViewTheme = _List_fromArray(
	[
		_Utils_Tuple2('background-color', '#444'),
		_Utils_Tuple2('color', '#eee'),
		_Utils_Tuple2('font-family', 'calibri, helvetica, arial, sans-serif'),
		_Utils_Tuple2('font-size', '14px')
	]);
var author$project$TimeTravel$Internal$Styles$pointer = _List_fromArray(
	[
		_Utils_Tuple2('cursor', 'pointer')
	]);
var author$project$TimeTravel$Internal$Styles$iconButton = _Utils_ap(
	_List_fromArray(
		[
			_Utils_Tuple2('padding', '10px 10px 6px 10px'),
			_Utils_Tuple2('border', 'solid 1px #666'),
			_Utils_Tuple2('border-radius', '3px')
		]),
	author$project$TimeTravel$Internal$Styles$pointer);
var author$project$TimeTravel$Internal$Styles$zIndex = {debugView: '2147483646', modelDetailView: '2147483646', resyncView: '2147483645'};
var author$project$TimeTravel$Internal$Styles$minimizedButton = function (fixedToLeft) {
	return _Utils_ap(
		_List_fromArray(
			[
				_Utils_Tuple2('position', 'fixed'),
				_Utils_Tuple2('bottom', '0'),
				_Utils_Tuple2(
				fixedToLeft ? 'left' : 'right',
				'0'),
				_Utils_Tuple2('z-index', author$project$TimeTravel$Internal$Styles$zIndex.debugView)
			]),
		_Utils_ap(author$project$TimeTravel$Internal$Styles$iconButton, author$project$TimeTravel$Internal$Styles$debugViewTheme));
};
var elm$core$Char$toUpper = _Char_toUpper;
var elm$core$String$fromList = _String_fromList;
var elm$core$String$foldr = _String_foldr;
var elm$core$String$toList = function (string) {
	return A3(elm$core$String$foldr, elm$core$List$cons, _List_Nil, string);
};
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var author$project$InlineHover$toCamelCase = function (s) {
	return elm$core$String$fromList(
		elm$core$List$reverse(
			A3(
				elm$core$List$foldl,
				F2(
					function (c, _n0) {
						var cap = _n0.a;
						var memo = _n0.b;
						return _Utils_eq(
							c,
							_Utils_chr('-')) ? _Utils_Tuple2(true, memo) : (cap ? _Utils_Tuple2(
							false,
							A2(
								elm$core$List$cons,
								elm$core$Char$toUpper(c),
								memo)) : _Utils_Tuple2(
							false,
							A2(elm$core$List$cons, c, memo)));
					}),
				_Utils_Tuple2(false, _List_Nil),
				elm$core$String$toList(s)).b));
};
var author$project$InlineHover$enterEach = function (_n0) {
	var key = _n0.a;
	var value = _n0.b;
	var keyCamel = author$project$InlineHover$toCamelCase(key);
	var escapedValue = A2(
		elm$core$Basics$composeL,
		elm$core$String$join('\"'),
		elm$core$String$split('\''))(value);
	return 'this.setAttribute(\'data-hover-' + (key + ('\', this.style.' + (keyCamel + ('||\'\');' + ('this.style.' + (keyCamel + ('=\'' + (escapedValue + '\''))))))));
};
var elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			elm$core$List$any,
			A2(elm$core$Basics$composeL, elm$core$Basics$not, isOkay),
			list);
	});
var author$project$InlineHover$isValidKey = function (s) {
	return (s !== '') && (A2(
		elm$core$List$any,
		elm$core$Char$isLower,
		elm$core$String$toList(s)) && A2(
		elm$core$List$all,
		function (c) {
			return elm$core$Char$isLower(c) || _Utils_eq(
				c,
				_Utils_chr('-'));
		},
		elm$core$String$toList(s)));
};
var author$project$InlineHover$leaveEach = function (_n0) {
	var key = _n0.a;
	var value = _n0.b;
	var keyCamel = author$project$InlineHover$toCamelCase(key);
	return 'this.style.' + (keyCamel + ('=this.getAttribute(\'data-hover-' + (key + '\')||\'\';')));
};
var elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2(elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var elm$html$Html$Attributes$attribute = elm$virtual_dom$VirtualDom$attribute;
var author$project$InlineHover$hover = F4(
	function (styles, tag, attrs, children) {
		var validStyles = A2(
			elm$core$List$filter,
			function (_n0) {
				var k = _n0.a;
				var v = _n0.b;
				return author$project$InlineHover$isValidKey(k);
			},
			styles);
		var leave = A2(
			elm$html$Html$Attributes$attribute,
			'onmouseleave',
			A2(
				elm$core$String$join,
				';',
				A2(elm$core$List$map, author$project$InlineHover$leaveEach, validStyles)));
		var enter = A2(
			elm$html$Html$Attributes$attribute,
			'onmouseenter',
			A2(
				elm$core$String$join,
				';',
				A2(elm$core$List$map, author$project$InlineHover$enterEach, validStyles)));
		return A2(
			tag,
			A2(
				elm$core$List$cons,
				enter,
				A2(elm$core$List$cons, leave, attrs)),
			children);
	});
var author$project$TimeTravel$Internal$Styles$buttonHover = _List_fromArray(
	[
		_Utils_Tuple2('background-color', '#555')
	]);
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var author$project$TimeTravel$Internal$Styles$styles = function (styles_) {
	var style = function (_n0) {
		var name = _n0.a;
		var value = _n0.b;
		return A2(elm$html$Html$Attributes$style, name, value);
	};
	return A2(elm$core$List$map, style, styles_);
};
var author$project$TimeTravel$Internal$View$buttonView = F3(
	function (onClickMsg, buttonStyle, inner) {
		return A4(
			author$project$InlineHover$hover,
			author$project$TimeTravel$Internal$Styles$buttonHover,
			elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Events$onClick(onClickMsg)
					]),
				author$project$TimeTravel$Internal$Styles$styles(buttonStyle)),
			inner);
	});
var author$project$TimeTravel$Internal$View$minimizedDebugView = function (model) {
	return A3(
		author$project$TimeTravel$Internal$View$buttonView,
		author$project$TimeTravel$Internal$Model$ToggleMinimize,
		author$project$TimeTravel$Internal$Styles$minimizedButton(model.fixedToLeft),
		_List_fromArray(
			[
				author$project$TimeTravel$Internal$Icons$minimize(true)
			]));
};
var author$project$TimeTravel$Internal$Styles$debugView = function (fixedToLeft) {
	return _Utils_ap(
		_List_fromArray(
			[
				_Utils_Tuple2('position', 'fixed'),
				_Utils_Tuple2('width', '250px'),
				_Utils_Tuple2('top', '0'),
				_Utils_Tuple2(
				fixedToLeft ? 'left' : 'right',
				'0'),
				_Utils_Tuple2('bottom', '0'),
				_Utils_Tuple2('z-index', author$project$TimeTravel$Internal$Styles$zIndex.debugView)
			]),
		author$project$TimeTravel$Internal$Styles$debugViewTheme);
};
var author$project$TimeTravel$Internal$Styles$lineBase = _List_fromArray(
	[
		_Utils_Tuple2('padding-left', '10px'),
		_Utils_Tuple2('white-space', 'pre')
	]);
var author$project$TimeTravel$Internal$Styles$addedLine = _Utils_ap(
	_List_fromArray(
		[
			_Utils_Tuple2('background-color', 'rgba(100, 255, 100, 0.15)')
		]),
	author$project$TimeTravel$Internal$Styles$lineBase);
var author$project$TimeTravel$Internal$DiffView$addedLine = function (s) {
	return A2(
		elm$html$Html$div,
		author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$addedLine),
		_List_fromArray(
			[
				elm$html$Html$text(s)
			]));
};
var author$project$TimeTravel$Internal$Styles$deletedLine = _Utils_ap(
	_List_fromArray(
		[
			_Utils_Tuple2('background-color', 'rgba(255, 100, 100, 0.15)')
		]),
	author$project$TimeTravel$Internal$Styles$lineBase);
var author$project$TimeTravel$Internal$DiffView$deletedLine = function (s) {
	return A2(
		elm$html$Html$div,
		author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$deletedLine),
		_List_fromArray(
			[
				elm$html$Html$text(s)
			]));
};
var author$project$TimeTravel$Internal$Styles$normalLine = author$project$TimeTravel$Internal$Styles$lineBase;
var author$project$TimeTravel$Internal$DiffView$normalLine = function (s) {
	return A2(
		elm$html$Html$div,
		author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$normalLine),
		_List_fromArray(
			[
				elm$html$Html$text(s)
			]));
};
var author$project$TimeTravel$Internal$Styles$omittedLine = author$project$TimeTravel$Internal$Styles$lineBase;
var author$project$TimeTravel$Internal$DiffView$omittedLine = A2(
	elm$html$Html$div,
	author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$omittedLine),
	_List_fromArray(
		[
			elm$html$Html$text('...')
		]));
var author$project$TimeTravel$Internal$DiffView$Add = function (a) {
	return {$: 'Add', a: a};
};
var author$project$TimeTravel$Internal$DiffView$Delete = function (a) {
	return {$: 'Delete', a: a};
};
var author$project$TimeTravel$Internal$DiffView$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var author$project$TimeTravel$Internal$DiffView$Omit = {$: 'Omit'};
var elm$core$List$drop = F2(
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
var elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2(elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var elm$core$List$takeTailRec = F2(
	function (n, list) {
		return elm$core$List$reverse(
			A3(elm$core$List$takeReverse, n, list, _List_Nil));
	});
var elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _n0 = _Utils_Tuple2(n, list);
			_n0$1:
			while (true) {
				_n0$5:
				while (true) {
					if (!_n0.b.b) {
						return list;
					} else {
						if (_n0.b.b.b) {
							switch (_n0.a) {
								case 1:
									break _n0$1;
								case 2:
									var _n2 = _n0.b;
									var x = _n2.a;
									var _n3 = _n2.b;
									var y = _n3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_n0.b.b.b.b) {
										var _n4 = _n0.b;
										var x = _n4.a;
										var _n5 = _n4.b;
										var y = _n5.a;
										var _n6 = _n5.b;
										var z = _n6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _n0$5;
									}
								default:
									if (_n0.b.b.b.b && _n0.b.b.b.b.b) {
										var _n7 = _n0.b;
										var x = _n7.a;
										var _n8 = _n7.b;
										var y = _n8.a;
										var _n9 = _n8.b;
										var z = _n9.a;
										var _n10 = _n9.b;
										var w = _n10.a;
										var tl = _n10.b;
										return (ctr > 1000) ? A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A2(elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A3(elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _n0$5;
									}
							}
						} else {
							if (_n0.a === 1) {
								break _n0$1;
							} else {
								break _n0$5;
							}
						}
					}
				}
				return list;
			}
			var _n1 = _n0.b;
			var x = _n1.a;
			return _List_fromArray(
				[x]);
		}
	});
var elm$core$List$take = F2(
	function (n, list) {
		return A3(elm$core$List$takeFast, 0, n, list);
	});
var author$project$TimeTravel$Internal$DiffView$tmpToResult = F4(
	function (additionalLines, next, tmp, result) {
		return _Utils_eq(result, _List_Nil) ? _Utils_Tuple2(
			_List_Nil,
			A2(
				elm$core$List$cons,
				next,
				_Utils_ap(
					A2(elm$core$List$take, additionalLines, tmp),
					(_Utils_cmp(
						elm$core$List$length(tmp),
						additionalLines) > 0) ? _List_fromArray(
						[author$project$TimeTravel$Internal$DiffView$Omit]) : _List_Nil))) : ((_Utils_cmp(
			elm$core$List$length(tmp),
			additionalLines * 2) > 0) ? _Utils_Tuple2(
			_List_Nil,
			A2(
				elm$core$List$cons,
				next,
				_Utils_ap(
					A2(elm$core$List$take, additionalLines, tmp),
					_Utils_ap(
						_List_fromArray(
							[author$project$TimeTravel$Internal$DiffView$Omit]),
						_Utils_ap(
							A2(
								elm$core$List$drop,
								elm$core$List$length(tmp) - additionalLines,
								tmp),
							result))))) : _Utils_Tuple2(
			_List_Nil,
			A2(
				elm$core$List$cons,
				next,
				_Utils_ap(tmp, result))));
	});
var author$project$TimeTravel$Internal$DiffView$reduceLines = function (list) {
	var additionalLines = 2;
	var _n0 = A3(
		elm$core$List$foldr,
		F2(
			function (line, _n1) {
				var tmp_ = _n1.a;
				var result_ = _n1.b;
				switch (line.$) {
					case 'NoChange':
						var s = line.a;
						return _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								author$project$TimeTravel$Internal$DiffView$Normal(s),
								tmp_),
							result_);
					case 'Removed':
						var s = line.a;
						return A4(
							author$project$TimeTravel$Internal$DiffView$tmpToResult,
							additionalLines,
							author$project$TimeTravel$Internal$DiffView$Delete(s),
							tmp_,
							result_);
					default:
						var s = line.a;
						return A4(
							author$project$TimeTravel$Internal$DiffView$tmpToResult,
							additionalLines,
							author$project$TimeTravel$Internal$DiffView$Add(s),
							tmp_,
							result_);
				}
			}),
		_Utils_Tuple2(_List_Nil, _List_Nil),
		list);
	var tmp = _n0.a;
	var result = _n0.b;
	return _Utils_eq(result, _List_Nil) ? _List_Nil : ((_Utils_cmp(
		elm$core$List$length(tmp),
		additionalLines) > 0) ? A2(
		elm$core$List$cons,
		author$project$TimeTravel$Internal$DiffView$Omit,
		_Utils_ap(
			A2(
				elm$core$List$drop,
				elm$core$List$length(tmp) - additionalLines,
				tmp),
			result)) : _Utils_ap(tmp, result));
};
var author$project$TimeTravel$Internal$Styles$panel = function (visible) {
	return _List_fromArray(
		[
			_Utils_Tuple2(
			'padding',
			visible ? '20px' : '0 20px'),
			_Utils_Tuple2('overflow', 'hidden')
		]);
};
var author$project$TimeTravel$Internal$Styles$diffView = author$project$TimeTravel$Internal$Styles$panel(true);
var author$project$TimeTravel$Internal$DiffView$view = function (changes) {
	var linesView = A2(
		elm$core$List$map,
		function (line) {
			switch (line.$) {
				case 'Normal':
					var s = line.a;
					return author$project$TimeTravel$Internal$DiffView$normalLine(s);
				case 'Delete':
					var s = line.a;
					return author$project$TimeTravel$Internal$DiffView$deletedLine(s);
				case 'Add':
					var s = line.a;
					return author$project$TimeTravel$Internal$DiffView$addedLine(s);
				default:
					return author$project$TimeTravel$Internal$DiffView$omittedLine;
			}
		},
		author$project$TimeTravel$Internal$DiffView$reduceLines(changes));
	return A2(
		elm$html$Html$div,
		author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$diffView),
		linesView);
};
var author$project$TimeTravel$Internal$Model$SelectMsg = function (a) {
	return {$: 'SelectMsg', a: a};
};
var author$project$TimeTravel$Internal$Model$ToggleModelDetail = function (a) {
	return {$: 'ToggleModelDetail', a: a};
};
var author$project$TimeTravel$Internal$Util$Nel$findHelp = F2(
	function (f, list) {
		findHelp:
		while (true) {
			if (!list.b) {
				return elm$core$Maybe$Nothing;
			} else {
				var head_ = list.a;
				var tail = list.b;
				if (f(head_)) {
					return elm$core$Maybe$Just(head_);
				} else {
					var $temp$f = f,
						$temp$list = tail;
					f = $temp$f;
					list = $temp$list;
					continue findHelp;
				}
			}
		}
	});
var author$project$TimeTravel$Internal$Util$Nel$find = F2(
	function (f, nel) {
		return A2(
			author$project$TimeTravel$Internal$Util$Nel$findHelp,
			f,
			author$project$TimeTravel$Internal$Util$Nel$toList(nel));
	});
var author$project$TimeTravel$Internal$Model$selectedItem = function (model) {
	var _n0 = _Utils_Tuple2(model.sync, model.selectedMsg);
	if (_n0.a) {
		return elm$core$Maybe$Just(
			author$project$TimeTravel$Internal$Util$Nel$head(model.history));
	} else {
		if (_n0.b.$ === 'Nothing') {
			var _n1 = _n0.b;
			return elm$core$Maybe$Just(
				author$project$TimeTravel$Internal$Util$Nel$head(model.history));
		} else {
			var msgId = _n0.b.a;
			return A2(
				author$project$TimeTravel$Internal$Util$Nel$find,
				function (item) {
					return _Utils_eq(item.id, msgId);
				},
				model.history);
		}
	}
};
var author$project$TimeTravel$Internal$Util$Nel$findMapHelp = F2(
	function (f, list) {
		findMapHelp:
		while (true) {
			if (!list.b) {
				return elm$core$Maybe$Nothing;
			} else {
				var head_ = list.a;
				var tail = list.b;
				var _n1 = f(head_);
				if (_n1.$ === 'Nothing') {
					var $temp$f = f,
						$temp$list = tail;
					f = $temp$f;
					list = $temp$list;
					continue findMapHelp;
				} else {
					var x = _n1;
					return x;
				}
			}
		}
	});
var author$project$TimeTravel$Internal$Util$Nel$findMap = F2(
	function (f, nel) {
		return A2(
			author$project$TimeTravel$Internal$Util$Nel$findMapHelp,
			f,
			author$project$TimeTravel$Internal$Util$Nel$toList(nel));
	});
var author$project$TimeTravel$Internal$Model$selectedMsgAst = function (model) {
	var _n0 = model.selectedMsg;
	if (_n0.$ === 'Just') {
		var id = _n0.a;
		var _n1 = A2(
			author$project$TimeTravel$Internal$Util$Nel$findMap,
			function (item) {
				return _Utils_eq(item.id, id) ? elm$core$Maybe$Just(item.lazyMsgAst) : elm$core$Maybe$Nothing;
			},
			model.history);
		if (((_n1.$ === 'Just') && (_n1.a.$ === 'Just')) && (_n1.a.a.$ === 'Ok')) {
			var ast = _n1.a.a.a;
			return elm$core$Maybe$Just(ast);
		} else {
			return elm$core$Maybe$Nothing;
		}
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var author$project$TimeTravel$Internal$Model$msgRootOf = F2(
	function (id, history) {
		msgRootOf:
		while (true) {
			var _n0 = A2(
				author$project$TimeTravel$Internal$Util$Nel$find,
				function (item) {
					return _Utils_eq(item.id, id);
				},
				history);
			if (_n0.$ === 'Just') {
				var item = _n0.a;
				var _n1 = item.causedBy;
				if (_n1.$ === 'Just') {
					var id_ = _n1.a;
					var $temp$id = id_,
						$temp$history = history;
					id = $temp$id;
					history = $temp$history;
					continue msgRootOf;
				} else {
					return elm$core$Maybe$Just(item);
				}
			} else {
				return elm$core$Maybe$Nothing;
			}
		}
	});
var author$project$TimeTravel$Internal$Util$RTree$Node = F2(
	function (a, b) {
		return {$: 'Node', a: a, b: b};
	});
var author$project$TimeTravel$Internal$Util$RTree$singleton = function (a) {
	return A2(author$project$TimeTravel$Internal$Util$RTree$Node, a, _List_Nil);
};
var author$project$TimeTravel$Internal$Util$RTree$addChild = F2(
	function (_new, _n0) {
		var a = _n0.a;
		var list = _n0.b;
		return A2(
			author$project$TimeTravel$Internal$Util$RTree$Node,
			a,
			A2(
				elm$core$List$cons,
				author$project$TimeTravel$Internal$Util$RTree$singleton(_new),
				list));
	});
var author$project$TimeTravel$Internal$Util$RTree$addChildAt = F3(
	function (f, _new, tree) {
		var _n0 = tree;
		var a = _n0.a;
		var list = _n0.b;
		var _n1 = f(a) ? A2(author$project$TimeTravel$Internal$Util$RTree$addChild, _new, tree) : tree;
		var a_ = _n1.a;
		var list_ = _n1.b;
		return A2(
			author$project$TimeTravel$Internal$Util$RTree$Node,
			a_,
			A2(
				elm$core$List$map,
				A2(author$project$TimeTravel$Internal$Util$RTree$addChildAt, f, _new),
				list_));
	});
var author$project$TimeTravel$Internal$Util$RTree$root = function (_n0) {
	var a = _n0.a;
	var list = _n0.b;
	return a;
};
var elm$core$List$sortBy = _List_sortBy;
var author$project$TimeTravel$Internal$Util$RTree$sortEachBranchBy = F2(
	function (f, _n0) {
		var a = _n0.a;
		var list = _n0.b;
		return A2(
			author$project$TimeTravel$Internal$Util$RTree$Node,
			a,
			A2(
				elm$core$List$sortBy,
				A2(elm$core$Basics$composeL, f, author$project$TimeTravel$Internal$Util$RTree$root),
				A2(
					elm$core$List$map,
					author$project$TimeTravel$Internal$Util$RTree$sortEachBranchBy(f),
					list)));
	});
var author$project$TimeTravel$Internal$Model$selectedMsgTree = function (model) {
	var _n0 = model.selectedMsg;
	if (_n0.$ === 'Just') {
		var id = _n0.a;
		var _n1 = A2(author$project$TimeTravel$Internal$Model$msgRootOf, id, model.history);
		if (_n1.$ === 'Just') {
			var root = _n1.a;
			var f = F2(
				function (item, tree) {
					return A3(
						author$project$TimeTravel$Internal$Util$RTree$addChildAt,
						function (i) {
							return _Utils_eq(
								item.causedBy,
								elm$core$Maybe$Just(i.id));
						},
						item,
						tree);
				});
			return elm$core$Maybe$Just(
				A2(
					author$project$TimeTravel$Internal$Util$RTree$sortEachBranchBy,
					function (item) {
						return item.id;
					},
					A3(
						elm$core$List$foldr,
						f,
						author$project$TimeTravel$Internal$Util$RTree$singleton(root),
						author$project$TimeTravel$Internal$Util$Nel$toList(model.history))));
		} else {
			return elm$core$Maybe$Nothing;
		}
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var author$project$TimeTravel$Internal$MsgLike$format = function (msgLike) {
	if (msgLike.$ === 'Message') {
		var m = msgLike.a;
		return elm$core$Debug$toString(m);
	} else {
		return '[Init]';
	}
};
var author$project$TimeTravel$Internal$Styles$itemBackground = function (selected) {
	return _List_fromArray(
		[
			_Utils_Tuple2(
			'background-color',
			selected ? 'rgba(0, 0, 0, 0.5)' : '')
		]);
};
var author$project$TimeTravel$Internal$Styles$msgTreeViewItemRow = function (selected) {
	return _Utils_ap(
		_List_fromArray(
			[
				_Utils_Tuple2('white-space', 'pre'),
				_Utils_Tuple2('text-overflow', 'ellipsis'),
				_Utils_Tuple2('overflow', 'hidden')
			]),
		_Utils_ap(
			author$project$TimeTravel$Internal$Styles$itemBackground(selected),
			author$project$TimeTravel$Internal$Styles$pointer));
};
var author$project$TimeTravel$Internal$Styles$msgTreeViewItemRowHover = function (selected) {
	return selected ? _List_Nil : _List_fromArray(
		[
			_Utils_Tuple2('background-color', '#555')
		]);
};
var author$project$TimeTravel$Internal$MsgTreeView$itemRow = F4(
	function (onSelect, indent, selectedMsg, item) {
		return A4(
			author$project$InlineHover$hover,
			author$project$TimeTravel$Internal$Styles$msgTreeViewItemRowHover(
				_Utils_eq(selectedMsg, item.id)),
			elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Events$onClick(
						onSelect(item.id))
					]),
				author$project$TimeTravel$Internal$Styles$styles(
					author$project$TimeTravel$Internal$Styles$msgTreeViewItemRow(
						_Utils_eq(selectedMsg, item.id)))),
			_List_fromArray(
				[
					elm$html$Html$text(
					A2(elm$core$String$repeat, indent, '    ') + (elm$core$Debug$toString(item.id) + (': ' + author$project$TimeTravel$Internal$MsgLike$format(item.msg))))
				]));
	});
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var elm$core$List$concatMap = F2(
	function (f, list) {
		return elm$core$List$concat(
			A2(elm$core$List$map, f, list));
	});
var author$project$TimeTravel$Internal$MsgTreeView$viewTree = F4(
	function (onSelect, indent, selectedMsg, _n0) {
		var item = _n0.a;
		var list = _n0.b;
		return A2(
			elm$core$List$cons,
			A4(author$project$TimeTravel$Internal$MsgTreeView$itemRow, onSelect, indent, selectedMsg, item),
			A2(
				elm$core$List$concatMap,
				A3(author$project$TimeTravel$Internal$MsgTreeView$viewTree, onSelect, indent + 1, selectedMsg),
				list));
	});
var author$project$TimeTravel$Internal$Styles$panelBorder = _List_fromArray(
	[
		_Utils_Tuple2('border-bottom', 'solid 1px #666')
	]);
var author$project$TimeTravel$Internal$Styles$msgTreeView = _Utils_ap(
	author$project$TimeTravel$Internal$Styles$panel(true),
	author$project$TimeTravel$Internal$Styles$panelBorder);
var author$project$TimeTravel$Internal$MsgTreeView$view = F3(
	function (onSelect, selectedMsg, tree) {
		return A2(
			elm$html$Html$div,
			author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$msgTreeView),
			A4(author$project$TimeTravel$Internal$MsgTreeView$viewTree, onSelect, 0, selectedMsg, tree));
	});
var author$project$TimeTravel$Internal$Styles$detailTab = function (active) {
	return _Utils_ap(
		_List_fromArray(
			[
				_Utils_Tuple2('border-radius', '3px 3px 0 0'),
				_Utils_Tuple2('height', '30px'),
				_Utils_Tuple2('top', '-30px'),
				_Utils_Tuple2('cursor', 'pointer'),
				_Utils_Tuple2('position', 'absolute'),
				_Utils_Tuple2('text-align', 'center'),
				_Utils_Tuple2('line-height', '30px')
			]),
		_Utils_ap(
			active ? _List_Nil : _List_fromArray(
				[
					_Utils_Tuple2('box-shadow', 'rgba(0, 0, 0, 0.25) 0px -1px 5px inset')
				]),
			author$project$TimeTravel$Internal$Styles$debugViewTheme));
};
var author$project$TimeTravel$Internal$Styles$detailTabDiff = F2(
	function (fixedToLeft, active) {
		return _Utils_ap(
			_List_fromArray(
				[
					_Utils_Tuple2('width', '170px'),
					_Utils_Tuple2(
					'left',
					fixedToLeft ? '150px' : '140px')
				]),
			author$project$TimeTravel$Internal$Styles$detailTab(active));
	});
var author$project$TimeTravel$Internal$Styles$detailTabModel = F2(
	function (fixedToLeft, active) {
		return _Utils_ap(
			_List_fromArray(
				[
					_Utils_Tuple2('width', '130px'),
					_Utils_Tuple2(
					'left',
					fixedToLeft ? '10px' : '0')
				]),
			author$project$TimeTravel$Internal$Styles$detailTab(active));
	});
var author$project$TimeTravel$Internal$Styles$subPain = function (fixedToLeft) {
	return _List_fromArray(
		[
			_Utils_Tuple2(
			'box-shadow',
			fixedToLeft ? 'rgba(0, 0, 0, 0.15) 6px -3px 6px inset' : 'rgba(0, 0, 0, 0.15) -6px -3px 6px inset')
		]);
};
var author$project$TimeTravel$Internal$Styles$detailView = F2(
	function (fixedToLeft, opened) {
		return _Utils_ap(
			_List_fromArray(
				[
					_Utils_Tuple2('position', 'absolute'),
					_Utils_Tuple2('width', '320px'),
					_Utils_Tuple2(
					fixedToLeft ? 'right' : 'left',
					'-320px'),
					_Utils_Tuple2('box-sizing', 'border-box'),
					_Utils_Tuple2('height', 'calc(100% - 87px)')
				]),
			_Utils_ap(
				author$project$TimeTravel$Internal$Styles$subPain(fixedToLeft),
				author$project$TimeTravel$Internal$Styles$debugViewTheme));
	});
var author$project$TimeTravel$Internal$Styles$detailViewHead = _List_Nil;
var author$project$TimeTravel$Internal$Styles$detailedMsgView = _Utils_ap(
	_List_fromArray(
		[
			_Utils_Tuple2('white-space', 'pre')
		]),
	_Utils_ap(
		author$project$TimeTravel$Internal$Styles$panel(true),
		author$project$TimeTravel$Internal$Styles$panelBorder));
var author$project$TimeTravel$Internal$Styles$detailTabHover = _List_fromArray(
	[
		_Utils_Tuple2('background-color', '#555')
	]);
var author$project$TimeTravel$Internal$View$detailTab = F3(
	function (styles, msg, name) {
		return A4(
			author$project$InlineHover$hover,
			author$project$TimeTravel$Internal$Styles$detailTabHover,
			elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Events$onClick(msg)
					]),
				author$project$TimeTravel$Internal$Styles$styles(styles)),
			_List_fromArray(
				[
					elm$html$Html$text(name)
				]));
	});
var author$project$TimeTravel$Internal$Parser$AST$filterByExactId = F2(
	function (s, ast) {
		filterByExactId:
		while (true) {
			switch (ast.$) {
				case 'RecordX':
					var id = ast.a;
					var children = ast.b;
					return _Utils_eq(s, id) ? elm$core$Maybe$Just(ast) : ((_Utils_cmp(
						elm$core$String$length(s),
						elm$core$String$length(id)) < 0) ? elm$core$Maybe$Nothing : A2(author$project$TimeTravel$Internal$Parser$AST$filterByExactIdForList, s, children));
				case 'StringLiteralX':
					var id = ast.a;
					var v = ast.b;
					return _Utils_eq(s, id) ? elm$core$Maybe$Just(ast) : elm$core$Maybe$Nothing;
				case 'ListLiteralX':
					var id = ast.a;
					var children = ast.b;
					return _Utils_eq(s, id) ? elm$core$Maybe$Just(ast) : ((_Utils_cmp(
						elm$core$String$length(s),
						elm$core$String$length(id)) < 0) ? elm$core$Maybe$Nothing : A2(author$project$TimeTravel$Internal$Parser$AST$filterByExactIdForList, s, children));
				case 'TupleLiteralX':
					var id = ast.a;
					var children = ast.b;
					return _Utils_eq(s, id) ? elm$core$Maybe$Just(ast) : ((_Utils_cmp(
						elm$core$String$length(s),
						elm$core$String$length(id)) < 0) ? elm$core$Maybe$Nothing : A2(author$project$TimeTravel$Internal$Parser$AST$filterByExactIdForList, s, children));
				case 'ValueX':
					var id = ast.a;
					var v = ast.b;
					return _Utils_eq(s, id) ? elm$core$Maybe$Just(ast) : elm$core$Maybe$Nothing;
				case 'UnionX':
					var id = ast.a;
					var tag = ast.b;
					var children = ast.c;
					return _Utils_eq(s, id) ? elm$core$Maybe$Just(ast) : ((_Utils_cmp(
						elm$core$String$length(s),
						elm$core$String$length(id)) < 0) ? elm$core$Maybe$Nothing : A2(author$project$TimeTravel$Internal$Parser$AST$filterByExactIdForList, s, children));
				default:
					var id = ast.a;
					var key = ast.b;
					var value = ast.c;
					if (_Utils_eq(s, id)) {
						return elm$core$Maybe$Just(ast);
					} else {
						if (_Utils_cmp(
							elm$core$String$length(s),
							elm$core$String$length(id)) < 0) {
							return elm$core$Maybe$Nothing;
						} else {
							var $temp$s = s,
								$temp$ast = value;
							s = $temp$s;
							ast = $temp$ast;
							continue filterByExactId;
						}
					}
			}
		}
	});
var author$project$TimeTravel$Internal$Parser$AST$filterByExactIdForList = F2(
	function (s, list) {
		filterByExactIdForList:
		while (true) {
			if (!list.b) {
				return elm$core$Maybe$Nothing;
			} else {
				var ast = list.a;
				var tail = list.b;
				var _n1 = A2(author$project$TimeTravel$Internal$Parser$AST$filterByExactId, s, ast);
				if (_n1.$ === 'Nothing') {
					var $temp$s = s,
						$temp$list = tail;
					s = $temp$s;
					list = $temp$list;
					continue filterByExactIdForList;
				} else {
					var found = _n1;
					return found;
				}
			}
		}
	});
var elm$core$String$toLower = _String_toLower;
var author$project$TimeTravel$Internal$Parser$AST$match = F2(
	function (s, id) {
		return A2(
			elm$core$String$contains,
			elm$core$String$toLower(s),
			elm$core$String$toLower(id));
	});
var author$project$TimeTravel$Internal$Parser$AST$filterById = F2(
	function (s, ast) {
		filterById:
		while (true) {
			switch (ast.$) {
				case 'RecordX':
					var id = ast.a;
					var children = ast.b;
					return A2(author$project$TimeTravel$Internal$Parser$AST$match, s, id) ? _List_fromArray(
						[
							_Utils_Tuple2(id, ast)
						]) : A2(
						elm$core$List$concatMap,
						author$project$TimeTravel$Internal$Parser$AST$filterById(s),
						children);
				case 'StringLiteralX':
					var id = ast.a;
					var v = ast.b;
					return A2(author$project$TimeTravel$Internal$Parser$AST$match, s, id) ? _List_fromArray(
						[
							_Utils_Tuple2(id, ast)
						]) : _List_Nil;
				case 'ListLiteralX':
					var id = ast.a;
					var children = ast.b;
					return A2(author$project$TimeTravel$Internal$Parser$AST$match, s, id) ? _List_fromArray(
						[
							_Utils_Tuple2(id, ast)
						]) : A2(
						elm$core$List$concatMap,
						author$project$TimeTravel$Internal$Parser$AST$filterById(s),
						children);
				case 'TupleLiteralX':
					var id = ast.a;
					var children = ast.b;
					return A2(author$project$TimeTravel$Internal$Parser$AST$match, s, id) ? _List_fromArray(
						[
							_Utils_Tuple2(id, ast)
						]) : A2(
						elm$core$List$concatMap,
						author$project$TimeTravel$Internal$Parser$AST$filterById(s),
						children);
				case 'ValueX':
					var id = ast.a;
					var v = ast.b;
					return A2(author$project$TimeTravel$Internal$Parser$AST$match, s, id) ? _List_fromArray(
						[
							_Utils_Tuple2(id, ast)
						]) : _List_Nil;
				case 'UnionX':
					var id = ast.a;
					var tag = ast.b;
					var children = ast.c;
					return A2(author$project$TimeTravel$Internal$Parser$AST$match, s, id) ? _List_fromArray(
						[
							_Utils_Tuple2(id, ast)
						]) : A2(
						elm$core$List$concatMap,
						author$project$TimeTravel$Internal$Parser$AST$filterById(s),
						children);
				default:
					var id = ast.a;
					var key = ast.b;
					var value = ast.c;
					if (A2(author$project$TimeTravel$Internal$Parser$AST$match, s, id)) {
						return _List_fromArray(
							[
								_Utils_Tuple2(id, ast)
							]);
					} else {
						var $temp$s = s,
							$temp$ast = value;
						s = $temp$s;
						ast = $temp$ast;
						continue filterById;
					}
			}
		}
	});
var author$project$TimeTravel$Internal$Styles$modelDetailView = function (fixedToLeft) {
	return _Utils_ap(
		_List_fromArray(
			[
				_Utils_Tuple2('width', '320px'),
				_Utils_Tuple2('z-index', author$project$TimeTravel$Internal$Styles$zIndex.modelDetailView),
				_Utils_Tuple2('box-sizing', 'border-box'),
				_Utils_Tuple2('height', '100%'),
				_Utils_Tuple2('overflow-y', 'scroll')
			]),
		_List_fromArray(
			[
				_Utils_Tuple2('padding', '20px'),
				_Utils_Tuple2('overflow-x', 'hidden'),
				_Utils_Tuple2('overflow-y', 'scroll')
			]));
};
var author$project$TimeTravel$Internal$Styles$modelView = _Utils_ap(
	_List_fromArray(
		[
			_Utils_Tuple2('height', '150px'),
			_Utils_Tuple2('box-sizing', 'border-box')
		]),
	_Utils_ap(
		author$project$TimeTravel$Internal$Styles$panelBorder,
		author$project$TimeTravel$Internal$Styles$panel(true)));
var author$project$TimeTravel$Internal$Model$SelectModelFilter = function (a) {
	return {$: 'SelectModelFilter', a: a};
};
var author$project$TimeTravel$Internal$Model$ToggleModelTree = function (a) {
	return {$: 'ToggleModelTree', a: a};
};
var author$project$TimeTravel$Internal$Styles$modelDetailFlagment = _List_fromArray(
	[
		_Utils_Tuple2('white-space', 'pre'),
		_Utils_Tuple2('display', 'inline')
	]);
var author$project$TimeTravel$Internal$Styles$modelDetailFlagmentLink = _Utils_ap(
	_List_fromArray(
		[
			_Utils_Tuple2('cursor', 'pointer')
		]),
	author$project$TimeTravel$Internal$Styles$modelDetailFlagment);
var author$project$TimeTravel$Internal$Styles$textLinkHover = _List_fromArray(
	[
		_Utils_Tuple2('text-decoration', 'underline')
	]);
var author$project$TimeTravel$Internal$Styles$modelDetailFlagmentLinkHover = author$project$TimeTravel$Internal$Styles$textLinkHover;
var elm$html$Html$span = _VirtualDom_node('span');
var author$project$TimeTravel$Internal$Parser$Formatter$formatLinkAsHtml = F3(
	function (selectFilterMsg, id, s) {
		return _List_fromArray(
			[
				A4(
				author$project$InlineHover$hover,
				author$project$TimeTravel$Internal$Styles$modelDetailFlagmentLinkHover,
				elm$html$Html$span,
				_Utils_ap(
					_List_fromArray(
						[
							elm$html$Html$Events$onClick(
							selectFilterMsg(id))
						]),
					author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$modelDetailFlagmentLink)),
				_List_fromArray(
					[
						elm$html$Html$text(s)
					]))
			]);
	});
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$title = elm$html$Html$Attributes$stringProperty('title');
var author$project$TimeTravel$Internal$Parser$Formatter$formatPlainAsHtml = function (s) {
	return _List_fromArray(
		[
			A2(
			elm$html$Html$span,
			_Utils_ap(
				author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$modelDetailFlagment),
				A2(elm$core$String$startsWith, '\"', s) ? _List_fromArray(
					[
						elm$html$Html$Attributes$title(s)
					]) : _List_Nil),
			_List_fromArray(
				[
					elm$html$Html$text(s)
				]))
		]);
};
var author$project$TimeTravel$Internal$Styles$modelDetailFlagmentToggle = _List_fromArray(
	[
		_Utils_Tuple2('white-space', 'pre'),
		_Utils_Tuple2('display', 'inline'),
		_Utils_Tuple2('background-color', '#777'),
		_Utils_Tuple2('cursor', 'pointer')
	]);
var author$project$TimeTravel$Internal$Styles$modelDetailFlagmentToggleExpand = _Utils_ap(
	_List_fromArray(
		[
			_Utils_Tuple2('position', 'relative'),
			_Utils_Tuple2('left', '-16px'),
			_Utils_Tuple2('margin-right', '-14px')
		]),
	author$project$TimeTravel$Internal$Styles$modelDetailFlagmentToggle);
var author$project$TimeTravel$Internal$Parser$Formatter$formatAsHtml = F4(
	function (selectFilterMsg, toggleMsg, expandedTree, model) {
		return A5(
			author$project$TimeTravel$Internal$Parser$Formatter$formatHelp,
			author$project$TimeTravel$Internal$Parser$Formatter$formatPlainAsHtml,
			author$project$TimeTravel$Internal$Parser$Formatter$formatLinkAsHtml(selectFilterMsg),
			function (list) {
				return A2(
					elm$core$List$concatMap,
					A3(author$project$TimeTravel$Internal$Parser$Formatter$formatAsHtml, selectFilterMsg, toggleMsg, expandedTree),
					list);
			},
			F3(
				function (id, alt, children) {
					return A2(elm$core$Set$member, id, expandedTree) ? A2(
						elm$core$List$cons,
						A2(
							elm$html$Html$span,
							_Utils_ap(
								_List_fromArray(
									[
										elm$html$Html$Events$onClick(
										toggleMsg(id))
									]),
								author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$modelDetailFlagmentToggleExpand)),
							_List_fromArray(
								[
									elm$html$Html$text(' - ')
								])),
						A2(
							elm$core$List$concatMap,
							A3(author$project$TimeTravel$Internal$Parser$Formatter$formatAsHtml, selectFilterMsg, toggleMsg, expandedTree),
							children)) : _List_fromArray(
						[
							A2(
							elm$html$Html$span,
							_Utils_ap(
								_List_fromArray(
									[
										elm$html$Html$Events$onClick(
										toggleMsg(id))
									]),
								author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$modelDetailFlagmentToggle)),
							_List_fromArray(
								[
									elm$html$Html$text(alt)
								]))
						]);
				}),
			model);
	});
var author$project$TimeTravel$Internal$Styles$modelDetailTreeEach = _List_fromArray(
	[
		_Utils_Tuple2('margin-bottom', '20px')
	]);
var author$project$TimeTravel$Internal$Model$SelectModelFilterWatch = function (a) {
	return {$: 'SelectModelFilterWatch', a: a};
};
var author$project$TimeTravel$Internal$Styles$darkTextColor = '#999';
var author$project$TimeTravel$Internal$Styles$modelDetailTreeEachId = _List_fromArray(
	[
		_Utils_Tuple2('color', author$project$TimeTravel$Internal$Styles$darkTextColor),
		_Utils_Tuple2('cursor', 'pointer')
	]);
var author$project$TimeTravel$Internal$Styles$modelDetailTreeEachIdHover = author$project$TimeTravel$Internal$Styles$textLinkHover;
var author$project$TimeTravel$Internal$Styles$modelDetailTreeEachIdWatch = author$project$TimeTravel$Internal$Styles$modelDetailTreeEachId;
var author$project$TimeTravel$Internal$Styles$modelDetailTreeEachIdWatchHover = author$project$TimeTravel$Internal$Styles$modelDetailTreeEachIdHover;
var author$project$TimeTravel$Internal$View$modelDetailTreeEachId = function (id) {
	var watchLink = A4(
		author$project$InlineHover$hover,
		author$project$TimeTravel$Internal$Styles$modelDetailTreeEachIdWatchHover,
		elm$html$Html$span,
		_Utils_ap(
			_List_fromArray(
				[
					elm$html$Html$Events$onClick(
					author$project$TimeTravel$Internal$Model$SelectModelFilterWatch(id))
				]),
			author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$modelDetailTreeEachIdWatch)),
		_List_fromArray(
			[
				elm$html$Html$text('watch')
			]));
	var filterLink = A4(
		author$project$InlineHover$hover,
		author$project$TimeTravel$Internal$Styles$modelDetailTreeEachIdHover,
		elm$html$Html$span,
		_Utils_ap(
			_List_fromArray(
				[
					elm$html$Html$Events$onClick(
					author$project$TimeTravel$Internal$Model$SelectModelFilter(id))
				]),
			author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$modelDetailTreeEachId)),
		_List_fromArray(
			[
				elm$html$Html$text(id)
			]));
	return A2(
		elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				filterLink,
				A2(
				elm$html$Html$span,
				author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$modelDetailTreeEachIdWatch),
				_List_fromArray(
					[
						elm$html$Html$text(' (')
					])),
				watchLink,
				A2(
				elm$html$Html$span,
				author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$modelDetailTreeEachIdWatch),
				_List_fromArray(
					[
						elm$html$Html$text(')')
					]))
			]));
};
var author$project$TimeTravel$Internal$View$modelDetailTreeEach = F3(
	function (expandedTree, maybeId, ast) {
		var idView = function () {
			if (maybeId.$ === 'Just') {
				var id = maybeId.a;
				return author$project$TimeTravel$Internal$View$modelDetailTreeEachId(id);
			} else {
				return elm$html$Html$text('');
			}
		}();
		return A2(
			elm$html$Html$div,
			author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$modelDetailTreeEach),
			A2(
				elm$core$List$cons,
				idView,
				A4(
					author$project$TimeTravel$Internal$Parser$Formatter$formatAsHtml,
					author$project$TimeTravel$Internal$Model$SelectModelFilter,
					author$project$TimeTravel$Internal$Model$ToggleModelTree,
					expandedTree,
					author$project$TimeTravel$Internal$Parser$Formatter$makeModel(ast))));
	});
var author$project$TimeTravel$Internal$Model$InputModelFilter = function (a) {
	return {$: 'InputModelFilter', a: a};
};
var author$project$TimeTravel$Internal$Styles$modelFilterInput = _List_fromArray(
	[
		_Utils_Tuple2('display', 'block'),
		_Utils_Tuple2('width', '100%'),
		_Utils_Tuple2('padding', '5px 10px'),
		_Utils_Tuple2('background-color', 'rgba(0,0,0,0.2)'),
		_Utils_Tuple2('margin-bottom', '10px'),
		_Utils_Tuple2('border', 'none'),
		_Utils_Tuple2('box-shadow', '2px 1px 7px 0px rgba(0,0,0,0.4) inset'),
		_Utils_Tuple2('color', '#eee'),
		_Utils_Tuple2('font-size', '14px'),
		_Utils_Tuple2('width', '100%'),
		_Utils_Tuple2('box-sizing', 'border-box')
	]);
var elm$html$Html$Attributes$placeholder = elm$html$Html$Attributes$stringProperty('placeholder');
var elm$html$Html$Attributes$value = elm$html$Html$Attributes$stringProperty('value');
var author$project$TimeTravel$Internal$View$modelFilterInput = function (modelFilter) {
	return A2(
		elm$html$Html$input,
		_Utils_ap(
			_List_fromArray(
				[
					elm$html$Html$Attributes$placeholder('Filter by property'),
					elm$html$Html$Attributes$value(modelFilter),
					elm$html$Html$Events$onInput(author$project$TimeTravel$Internal$Model$InputModelFilter)
				]),
			author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$modelFilterInput)),
		_List_Nil);
};
var author$project$TimeTravel$Internal$View$modelDetailView = F5(
	function (fixedToLeft, modelFilter, expandedTree, lazyModelAst, userModel) {
		if ((lazyModelAst.$ === 'Just') && (lazyModelAst.a.$ === 'Ok')) {
			var ast = lazyModelAst.a.a;
			var filteredAst = function () {
				if (A2(elm$core$String$startsWith, '@', modelFilter)) {
					var _n2 = A2(author$project$TimeTravel$Internal$Parser$AST$filterByExactId, modelFilter, ast);
					if (_n2.$ === 'Just') {
						var x = _n2.a;
						return _List_fromArray(
							[
								_Utils_Tuple2(modelFilter, x)
							]);
					} else {
						return _List_Nil;
					}
				} else {
					return A2(author$project$TimeTravel$Internal$Parser$AST$filterById, modelFilter, ast);
				}
			}();
			var trees = A2(
				elm$core$List$map,
				function (_n1) {
					var id = _n1.a;
					var ast_ = _n1.b;
					return A3(
						author$project$TimeTravel$Internal$View$modelDetailTreeEach,
						expandedTree,
						(modelFilter !== '') ? elm$core$Maybe$Just(id) : elm$core$Maybe$Nothing,
						ast_);
				},
				filteredAst);
			var filterInput = author$project$TimeTravel$Internal$View$modelFilterInput(modelFilter);
			return A2(
				elm$html$Html$div,
				author$project$TimeTravel$Internal$Styles$styles(
					author$project$TimeTravel$Internal$Styles$modelDetailView(fixedToLeft)),
				A2(elm$core$List$cons, filterInput, trees));
		} else {
			return A2(
				elm$html$Html$div,
				author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$modelView),
				_List_fromArray(
					[
						elm$html$Html$text(
						elm$core$Debug$toString(userModel))
					]));
		}
	});
var author$project$TimeTravel$Internal$View$detailView = function (model) {
	if (!model.sync) {
		var msgTreeView = function () {
			var _n4 = _Utils_Tuple2(
				model.selectedMsg,
				author$project$TimeTravel$Internal$Model$selectedMsgTree(model));
			if ((_n4.a.$ === 'Just') && (_n4.b.$ === 'Just')) {
				var id = _n4.a.a;
				var tree = _n4.b.a;
				return A3(author$project$TimeTravel$Internal$MsgTreeView$view, author$project$TimeTravel$Internal$Model$SelectMsg, id, tree);
			} else {
				return elm$html$Html$text('');
			}
		}();
		var head = A2(
			elm$html$Html$div,
			author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$detailViewHead),
			_List_fromArray(
				[
					A3(
					author$project$TimeTravel$Internal$View$detailTab,
					A2(author$project$TimeTravel$Internal$Styles$detailTabModel, model.fixedToLeft, model.showModelDetail),
					author$project$TimeTravel$Internal$Model$ToggleModelDetail(true),
					'Model'),
					A3(
					author$project$TimeTravel$Internal$View$detailTab,
					A2(author$project$TimeTravel$Internal$Styles$detailTabDiff, model.fixedToLeft, !model.showModelDetail),
					author$project$TimeTravel$Internal$Model$ToggleModelDetail(false),
					'Messages and Diff')
				]));
		var diffView = function () {
			var _n2 = author$project$TimeTravel$Internal$Model$selectedItem(model);
			if (_n2.$ === 'Just') {
				var item = _n2.a;
				var _n3 = item.lazyDiff;
				if (_n3.$ === 'Just') {
					var changes = _n3.a;
					return author$project$TimeTravel$Internal$DiffView$view(changes);
				} else {
					return elm$html$Html$text('');
				}
			} else {
				return elm$html$Html$text('');
			}
		}();
		var detailedMsgView = function () {
			var _n1 = author$project$TimeTravel$Internal$Model$selectedMsgAst(model);
			if (_n1.$ === 'Just') {
				var ast = _n1.a;
				return A2(
					elm$html$Html$div,
					author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$detailedMsgView),
					_List_fromArray(
						[
							elm$html$Html$text(
							author$project$TimeTravel$Internal$Parser$Formatter$formatAsString(
								author$project$TimeTravel$Internal$Parser$Formatter$makeModel(ast)))
						]));
			} else {
				return elm$html$Html$text('');
			}
		}();
		var body = function () {
			if (model.showModelDetail) {
				var _n0 = author$project$TimeTravel$Internal$Model$selectedItem(model);
				if (_n0.$ === 'Just') {
					var item = _n0.a;
					return A2(
						elm$core$List$cons,
						A5(author$project$TimeTravel$Internal$View$modelDetailView, model.fixedToLeft, model.modelFilter, model.expandedTree, item.lazyModelAst, item.model),
						_List_Nil);
				} else {
					return _List_Nil;
				}
			} else {
				return _List_fromArray(
					[msgTreeView, detailedMsgView, diffView]);
			}
		}();
		return A2(
			elm$html$Html$div,
			author$project$TimeTravel$Internal$Styles$styles(
				A2(author$project$TimeTravel$Internal$Styles$detailView, model.fixedToLeft, true)),
			A2(elm$core$List$cons, head, body));
	} else {
		return elm$html$Html$text('');
	}
};
var author$project$Material$Icons$Navigation$arrow_drop_down = A2(
	author$project$Material$Icons$Internal$icon,
	'0 0 48 48',
	_List_fromArray(
		[
			A2(
			elm$svg$Svg$path,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$d('M14 20l10 10 10-10z')
				]),
			_List_Nil)
		]));
var author$project$Material$Icons$Navigation$arrow_drop_up = A2(
	author$project$Material$Icons$Internal$icon,
	'0 0 48 48',
	_List_fromArray(
		[
			A2(
			elm$svg$Svg$path,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$d('M14 28l10-10 10 10z')
				]),
			_List_Nil)
		]));
var author$project$TimeTravel$Internal$Icons$filterExpand = function (expanded) {
	return A2(
		expanded ? author$project$Material$Icons$Navigation$arrow_drop_up : author$project$Material$Icons$Navigation$arrow_drop_down,
		avh4$elm_color$Color$white,
		24);
};
var author$project$Material$Icons$Action$swap_horiz = A2(
	author$project$Material$Icons$Internal$icon,
	'0 0 48 48',
	_List_fromArray(
		[
			A2(
			elm$svg$Svg$path,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$d('M13.98 22L6 30l7.98 8v-6H28v-4H13.98v-6zM42 18l-7.98-8v6H20v4h14.02v6L42 18z')
				]),
			_List_Nil)
		]));
var author$project$TimeTravel$Internal$Icons$layout = A2(author$project$Material$Icons$Action$swap_horiz, avh4$elm_color$Color$white, 24);
var author$project$Material$Icons$Av$pause = A2(
	author$project$Material$Icons$Internal$icon,
	'0 0 48 48',
	_List_fromArray(
		[
			A2(
			elm$svg$Svg$path,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$d('M12 38h8V10h-8v28zm16-28v28h8V10h-8z')
				]),
			_List_Nil)
		]));
var author$project$Material$Icons$Av$play_arrow = A2(
	author$project$Material$Icons$Internal$icon,
	'0 0 48 48',
	_List_fromArray(
		[
			A2(
			elm$svg$Svg$path,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$d('M16 10v28l22-14z')
				]),
			_List_Nil)
		]));
var author$project$TimeTravel$Internal$Icons$sync = function (_synchronized) {
	return A2(
		_synchronized ? author$project$Material$Icons$Av$pause : author$project$Material$Icons$Av$play_arrow,
		avh4$elm_color$Color$white,
		24);
};
var author$project$TimeTravel$Internal$Model$ToggleExpand = {$: 'ToggleExpand'};
var author$project$TimeTravel$Internal$Model$ToggleLayout = {$: 'ToggleLayout'};
var author$project$TimeTravel$Internal$Model$ToggleSync = {$: 'ToggleSync'};
var author$project$TimeTravel$Internal$Styles$buttonView = function (left) {
	return _Utils_ap(
		left ? _List_fromArray(
			[
				_Utils_Tuple2('margin-right', 'auto')
			]) : _List_fromArray(
			[
				_Utils_Tuple2('margin-left', 'auto')
			]),
		author$project$TimeTravel$Internal$Styles$iconButton);
};
var author$project$TimeTravel$Internal$Styles$headerView = _Utils_ap(
	_List_fromArray(
		[
			_Utils_Tuple2('display', 'flex'),
			_Utils_Tuple2('justify-content', 'flex-end')
		]),
	author$project$TimeTravel$Internal$Styles$panel(true));
var author$project$TimeTravel$Internal$Styles$filterView = function (visible) {
	return _Utils_ap(
		_List_fromArray(
			[
				_Utils_Tuple2('background-color', '#333'),
				_Utils_Tuple2('transition', 'height ease 0.3s, padding ease 0.3s'),
				_Utils_Tuple2(
				'height',
				visible ? '' : '0')
			]),
		_Utils_ap(
			author$project$TimeTravel$Internal$Styles$panelBorder,
			author$project$TimeTravel$Internal$Styles$panel(visible)));
};
var author$project$TimeTravel$Internal$Model$ToggleFilter = function (a) {
	return {$: 'ToggleFilter', a: a};
};
var elm$html$Html$label = _VirtualDom_node('label');
var elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$bool(bool));
	});
var elm$html$Html$Attributes$checked = elm$html$Html$Attributes$boolProperty('checked');
var elm$html$Html$Attributes$type_ = elm$html$Html$Attributes$stringProperty('type');
var author$project$TimeTravel$Internal$View$filterItemView = function (_n0) {
	var name = _n0.a;
	var visible = _n0.b;
	return A2(
		elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$label,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$input,
						_List_fromArray(
							[
								elm$html$Html$Attributes$type_('checkbox'),
								elm$html$Html$Attributes$checked(visible),
								elm$html$Html$Events$onClick(
								author$project$TimeTravel$Internal$Model$ToggleFilter(name))
							]),
						_List_Nil),
						elm$html$Html$text(name)
					]))
			]));
};
var author$project$TimeTravel$Internal$View$filterView = F2(
	function (visible, filterOptions) {
		return A2(
			elm$html$Html$div,
			author$project$TimeTravel$Internal$Styles$styles(
				author$project$TimeTravel$Internal$Styles$filterView(visible)),
			A2(
				elm$core$List$map,
				author$project$TimeTravel$Internal$View$filterItemView,
				A2(elm$core$List$sortBy, elm$core$Tuple$first, filterOptions)));
	});
var author$project$TimeTravel$Internal$View$headerView = F4(
	function (fixedToLeft, sync, expand, filterOptions) {
		return A2(
			elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$headerView),
					_List_fromArray(
						[
							A3(
							author$project$TimeTravel$Internal$View$buttonView,
							author$project$TimeTravel$Internal$Model$ToggleLayout,
							author$project$TimeTravel$Internal$Styles$buttonView(true),
							_List_fromArray(
								[author$project$TimeTravel$Internal$Icons$layout])),
							A3(
							author$project$TimeTravel$Internal$View$buttonView,
							author$project$TimeTravel$Internal$Model$ToggleMinimize,
							author$project$TimeTravel$Internal$Styles$buttonView(true),
							_List_fromArray(
								[
									author$project$TimeTravel$Internal$Icons$minimize(false)
								])),
							A3(
							author$project$TimeTravel$Internal$View$buttonView,
							author$project$TimeTravel$Internal$Model$ToggleSync,
							author$project$TimeTravel$Internal$Styles$buttonView(false),
							_List_fromArray(
								[
									author$project$TimeTravel$Internal$Icons$sync(sync)
								])),
							A3(
							author$project$TimeTravel$Internal$View$buttonView,
							author$project$TimeTravel$Internal$Model$ToggleExpand,
							author$project$TimeTravel$Internal$Styles$buttonView(false),
							_List_fromArray(
								[
									author$project$TimeTravel$Internal$Icons$filterExpand(expand)
								]))
						])),
					A2(author$project$TimeTravel$Internal$View$filterView, expand, filterOptions)
				]));
	});
var author$project$TimeTravel$Internal$Styles$msgListView = author$project$TimeTravel$Internal$Styles$panel(true);
var author$project$TimeTravel$Internal$View$filterMapUntilLimitHelp = F4(
	function (result, limit, f, list) {
		filterMapUntilLimitHelp:
		while (true) {
			if (limit <= 0) {
				return result;
			} else {
				if (!list.b) {
					return result;
				} else {
					var h = list.a;
					var t = list.b;
					var _n1 = f(h);
					if (_n1.$ === 'Just') {
						var b = _n1.a;
						var $temp$result = A2(elm$core$List$cons, b, result),
							$temp$limit = limit - 1,
							$temp$f = f,
							$temp$list = t;
						result = $temp$result;
						limit = $temp$limit;
						f = $temp$f;
						list = $temp$list;
						continue filterMapUntilLimitHelp;
					} else {
						var $temp$result = result,
							$temp$limit = limit,
							$temp$f = f,
							$temp$list = t;
						result = $temp$result;
						limit = $temp$limit;
						f = $temp$f;
						list = $temp$list;
						continue filterMapUntilLimitHelp;
					}
				}
			}
		}
	});
var author$project$TimeTravel$Internal$View$filterMapUntilLimit = F3(
	function (limit, f, list) {
		return elm$core$List$reverse(
			A4(author$project$TimeTravel$Internal$View$filterMapUntilLimitHelp, _List_Nil, limit, f, list));
	});
var author$project$TimeTravel$Internal$Styles$msgView = function (selected) {
	return _Utils_ap(
		_List_fromArray(
			[
				_Utils_Tuple2('white-space', 'nowrap'),
				_Utils_Tuple2('text-overflow', 'ellipsis'),
				_Utils_Tuple2('overflow', 'hidden')
			]),
		_Utils_ap(
			author$project$TimeTravel$Internal$Styles$itemBackground(selected),
			author$project$TimeTravel$Internal$Styles$pointer));
};
var author$project$TimeTravel$Internal$Styles$msgViewHover = function (selected) {
	return selected ? _List_Nil : _List_fromArray(
		[
			_Utils_Tuple2('background-color', '#555')
		]);
};
var author$project$TimeTravel$Internal$View$msgView = F3(
	function (filterOptions, selectedMsg, _n0) {
		var id = _n0.id;
		var msg = _n0.msg;
		var causedBy = _n0.causedBy;
		var str = author$project$TimeTravel$Internal$MsgLike$format(msg);
		var visible = _Utils_eq(msg, author$project$TimeTravel$Internal$MsgLike$Init) || function () {
			var _n2 = elm$core$String$words(str);
			if (_n2.b) {
				var tag = _n2.a;
				return A2(
					elm$core$List$any,
					function (_n3) {
						var name = _n3.a;
						var visible_ = _n3.b;
						return _Utils_eq(tag, name) && visible_;
					},
					filterOptions);
			} else {
				return false;
			}
		}();
		var selected = function () {
			if (selectedMsg.$ === 'Just') {
				var msgId = selectedMsg.a;
				return _Utils_eq(msgId, id);
			} else {
				return false;
			}
		}();
		return visible ? elm$core$Maybe$Just(
			_Utils_Tuple2(
				elm$core$String$fromInt(id),
				A4(
					author$project$InlineHover$hover,
					author$project$TimeTravel$Internal$Styles$msgViewHover(selected),
					elm$html$Html$div,
					_Utils_ap(
						_List_fromArray(
							[
								elm$html$Html$Events$onClick(
								author$project$TimeTravel$Internal$Model$SelectMsg(id)),
								elm$html$Html$Attributes$title(
								elm$core$String$fromInt(id) + (': ' + str))
							]),
						author$project$TimeTravel$Internal$Styles$styles(
							author$project$TimeTravel$Internal$Styles$msgView(selected))),
					_List_fromArray(
						[
							elm$html$Html$text(
							elm$core$String$fromInt(id) + (': ' + str))
						])))) : elm$core$Maybe$Nothing;
	});
var elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var elm$html$Html$Keyed$node = elm$virtual_dom$VirtualDom$keyedNode;
var author$project$TimeTravel$Internal$View$msgListView = F5(
	function (filterOptions, selectedMsg, items, watchView_, detailView_) {
		return A2(
			elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					detailView_,
					watchView_,
					A3(
					elm$html$Html$Keyed$node,
					'div',
					author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$msgListView),
					A3(
						author$project$TimeTravel$Internal$View$filterMapUntilLimit,
						60,
						A2(author$project$TimeTravel$Internal$View$msgView, filterOptions, selectedMsg),
						items))
				]));
	});
var author$project$TimeTravel$Internal$Model$Resync = {$: 'Resync'};
var author$project$TimeTravel$Internal$Styles$resyncView = function (sync) {
	return _List_fromArray(
		[
			_Utils_Tuple2('z-index', author$project$TimeTravel$Internal$Styles$zIndex.resyncView),
			_Utils_Tuple2('position', 'fixed'),
			_Utils_Tuple2('top', '0'),
			_Utils_Tuple2('bottom', '0'),
			_Utils_Tuple2('left', '0'),
			_Utils_Tuple2('right', '0'),
			_Utils_Tuple2('background-color', 'rgba(0, 0, 0, 0.15)'),
			_Utils_Tuple2(
			'opacity',
			sync ? '0' : '1'),
			_Utils_Tuple2(
			'pointer-events',
			sync ? 'none' : ''),
			_Utils_Tuple2('transition', 'opacity ease 0.5s')
		]);
};
var elm$html$Html$Events$onMouseDown = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'mousedown',
		elm$json$Json$Decode$succeed(msg));
};
var author$project$TimeTravel$Internal$View$resyncView = function (sync) {
	return sync ? elm$html$Html$text('') : A2(
		elm$html$Html$div,
		_Utils_ap(
			_List_fromArray(
				[
					elm$html$Html$Events$onMouseDown(author$project$TimeTravel$Internal$Model$Resync)
				]),
			author$project$TimeTravel$Internal$Styles$styles(
				author$project$TimeTravel$Internal$Styles$resyncView(sync))),
		_List_Nil);
};
var author$project$Material$Icons$Navigation$close = A2(
	author$project$Material$Icons$Internal$icon,
	'0 0 48 48',
	_List_fromArray(
		[
			A2(
			elm$svg$Svg$path,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$d('M38 12.83L35.17 10 24 21.17 12.83 10 10 12.83 21.17 24 10 35.17 12.83 38 24 26.83 35.17 38 38 35.17 26.83 24z')
				]),
			_List_Nil)
		]));
var avh4$elm_color$Color$gray = A4(avh4$elm_color$Color$RgbaSpace, 211 / 255, 215 / 255, 207 / 255, 1.0);
var author$project$TimeTravel$Internal$Icons$stopWatching = A2(author$project$Material$Icons$Navigation$close, avh4$elm_color$Color$gray, 14);
var author$project$TimeTravel$Internal$Model$StopWatching = {$: 'StopWatching'};
var author$project$TimeTravel$Internal$Styles$stopWatchingButton = _List_fromArray(
	[
		_Utils_Tuple2('position', 'absolute'),
		_Utils_Tuple2('right', '20px'),
		_Utils_Tuple2('top', '20px'),
		_Utils_Tuple2('cursor', 'pointer')
	]);
var author$project$TimeTravel$Internal$Styles$stopWatchingButtonHover = _List_fromArray(
	[
		_Utils_Tuple2('opacity', '0.5')
	]);
var author$project$TimeTravel$Internal$Styles$watchView = _Utils_ap(
	_List_fromArray(
		[
			_Utils_Tuple2('position', 'relative')
		]),
	_Utils_ap(
		author$project$TimeTravel$Internal$Styles$panel(true),
		author$project$TimeTravel$Internal$Styles$panelBorder));
var author$project$TimeTravel$Internal$Styles$watchViewHeader = _List_fromArray(
	[
		_Utils_Tuple2('color', author$project$TimeTravel$Internal$Styles$darkTextColor)
	]);
var author$project$TimeTravel$Internal$View$watchView = function (model) {
	var _n0 = _Utils_Tuple2(
		model.watch,
		author$project$TimeTravel$Internal$Util$Nel$head(model.history).lazyModelAst);
	if (((_n0.a.$ === 'Just') && (_n0.b.$ === 'Just')) && (_n0.b.a.$ === 'Ok')) {
		var id = _n0.a.a;
		var ast = _n0.b.a.a;
		var treeView = function () {
			var _n1 = A2(author$project$TimeTravel$Internal$Parser$AST$filterByExactId, id, ast);
			if (_n1.$ === 'Just') {
				var ast_ = _n1.a;
				return A3(author$project$TimeTravel$Internal$View$modelDetailTreeEach, model.expandedTree, elm$core$Maybe$Nothing, ast_);
			} else {
				return elm$html$Html$text('');
			}
		}();
		var stopWatchingButton = A4(
			author$project$InlineHover$hover,
			author$project$TimeTravel$Internal$Styles$stopWatchingButtonHover,
			elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Events$onClick(author$project$TimeTravel$Internal$Model$StopWatching)
					]),
				author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$stopWatchingButton)),
			_List_fromArray(
				[author$project$TimeTravel$Internal$Icons$stopWatching]));
		return A2(
			elm$html$Html$div,
			author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$watchView),
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					author$project$TimeTravel$Internal$Styles$styles(author$project$TimeTravel$Internal$Styles$watchViewHeader),
					_List_fromArray(
						[
							elm$html$Html$text('Watching ' + id)
						])),
					treeView,
					stopWatchingButton
				]));
	} else {
		return elm$html$Html$text('');
	}
};
var author$project$TimeTravel$Internal$View$normalDebugView = function (model) {
	return A2(
		elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				author$project$TimeTravel$Internal$View$resyncView(model.sync),
				A2(
				elm$html$Html$div,
				author$project$TimeTravel$Internal$Styles$styles(
					author$project$TimeTravel$Internal$Styles$debugView(model.fixedToLeft)),
				_List_fromArray(
					[
						A4(author$project$TimeTravel$Internal$View$headerView, model.fixedToLeft, model.sync, model.expand, model.filter),
						A5(
						author$project$TimeTravel$Internal$View$msgListView,
						model.filter,
						model.selectedMsg,
						author$project$TimeTravel$Internal$Util$Nel$toList(model.history),
						author$project$TimeTravel$Internal$View$watchView(model),
						author$project$TimeTravel$Internal$View$detailView(model))
					]))
			]));
};
var author$project$TimeTravel$Internal$View$debugView = function (model) {
	return (model.minimized ? author$project$TimeTravel$Internal$View$minimizedDebugView : author$project$TimeTravel$Internal$View$normalDebugView)(model);
};
var author$project$TimeTravel$Internal$View$userView = F2(
	function (userView_, model) {
		var _n0 = author$project$TimeTravel$Internal$Model$selectedItem(model);
		if (_n0.$ === 'Just') {
			var item = _n0.a;
			return userView_(item.model);
		} else {
			return elm$html$Html$text('Error: Unable to render');
		}
	});
var elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var elm$html$Html$map = elm$virtual_dom$VirtualDom$map;
var author$project$TimeTravel$Internal$View$view = F4(
	function (transformUserMsg, transformDebuggerMsg, userViewFunc, model) {
		return A2(
			elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					elm$html$Html$map,
					transformUserMsg,
					A2(author$project$TimeTravel$Internal$View$userView, userViewFunc, model)),
					A2(
					elm$html$Html$map,
					transformDebuggerMsg,
					author$project$TimeTravel$Internal$View$debugView(model))
				]));
	});
var author$project$TimeTravel$Browser$wrap = F2(
	function (_n0, _n1) {
		var outgoingMsg = _n0.outgoingMsg;
		var incomingMsg = _n0.incomingMsg;
		var init = _n1.init;
		var view = _n1.view;
		var update = _n1.update;
		var subscriptions = _n1.subscriptions;
		var view_ = function (model) {
			return A4(
				author$project$TimeTravel$Internal$View$view,
				function (c) {
					return author$project$TimeTravel$Browser$UserMsg(
						_Utils_Tuple2(elm$core$Maybe$Nothing, c));
				},
				author$project$TimeTravel$Browser$DebuggerMsg,
				view,
				model);
		};
		var update_ = F2(
			function (msg, model) {
				return A4(author$project$TimeTravel$Browser$wrapUpdate, update, outgoingMsg, msg, model);
			});
		var subscriptions_ = function (model) {
			return A3(author$project$TimeTravel$Browser$wrapSubscriptions, subscriptions, incomingMsg, model);
		};
		var init_ = function (flags) {
			return author$project$TimeTravel$Browser$wrapInit(
				init(flags));
		};
		return {init: init_, subscriptions: subscriptions_, update: update_, view: view_};
	});
var elm$core$Platform$Sub$none = elm$core$Platform$Sub$batch(_List_Nil);
var elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$core$Task$init = elm$core$Task$succeed(_Utils_Tuple0);
var elm$core$Task$andThen = _Scheduler_andThen;
var elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return A2(
					elm$core$Task$andThen,
					function (b) {
						return elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var elm$core$Task$sequence = function (tasks) {
	return A3(
		elm$core$List$foldr,
		elm$core$Task$map2(elm$core$List$cons),
		elm$core$Task$succeed(_List_Nil),
		tasks);
};
var elm$core$Platform$sendToApp = _Platform_sendToApp;
var elm$core$Task$spawnCmd = F2(
	function (router, _n0) {
		var task = _n0.a;
		return _Scheduler_spawn(
			A2(
				elm$core$Task$andThen,
				elm$core$Platform$sendToApp(router),
				task));
	});
var elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			elm$core$Task$map,
			function (_n0) {
				return _Utils_Tuple0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(_Utils_Tuple0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0.a;
		return elm$core$Task$Perform(
			A2(elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			elm$core$Task$Perform(
				A2(elm$core$Task$map, toMessage, task)));
	});
var elm$url$Url$Http = {$: 'Http'};
var elm$url$Url$Https = {$: 'Https'};
var elm$core$String$indexes = _String_indexes;
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var elm$core$String$toInt = _String_toInt;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if (elm$core$String$isEmpty(str) || A2(elm$core$String$contains, '@', str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, ':', str);
			if (!_n0.b) {
				return elm$core$Maybe$Just(
					A6(elm$url$Url$Url, protocol, str, elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_n0.b.b) {
					var i = _n0.a;
					var _n1 = elm$core$String$toInt(
						A2(elm$core$String$dropLeft, i + 1, str));
					if (_n1.$ === 'Nothing') {
						return elm$core$Maybe$Nothing;
					} else {
						var port_ = _n1;
						return elm$core$Maybe$Just(
							A6(
								elm$url$Url$Url,
								protocol,
								A2(elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}
	});
var elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '/', str);
			if (!_n0.b) {
				return A5(elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _n0.a;
				return A5(
					elm$url$Url$chompBeforePath,
					protocol,
					A2(elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '?', str);
			if (!_n0.b) {
				return A4(elm$url$Url$chompBeforeQuery, protocol, elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _n0.a;
				return A4(
					elm$url$Url$chompBeforeQuery,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '#', str);
			if (!_n0.b) {
				return A3(elm$url$Url$chompBeforeFragment, protocol, elm$core$Maybe$Nothing, str);
			} else {
				var i = _n0.a;
				return A3(
					elm$url$Url$chompBeforeFragment,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$fromString = function (str) {
	return A2(elm$core$String$startsWith, 'http://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Http,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Https,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$sandbox = function (impl) {
	return _Browser_element(
		{
			init: function (_n0) {
				return _Utils_Tuple2(impl.init, elm$core$Platform$Cmd$none);
			},
			subscriptions: function (_n1) {
				return elm$core$Platform$Sub$none;
			},
			update: F2(
				function (msg, model) {
					return _Utils_Tuple2(
						A2(impl.update, msg, model),
						elm$core$Platform$Cmd$none);
				}),
			view: impl.view
		});
};
var author$project$TimeTravel$Browser$sandbox = function (_n0) {
	var init = _n0.init;
	var view = _n0.view;
	var update = _n0.update;
	var options = A2(
		author$project$TimeTravel$Browser$wrap,
		{
			incomingMsg: elm$core$Basics$always(elm$core$Platform$Sub$none),
			outgoingMsg: elm$core$Basics$always(elm$core$Platform$Cmd$none)
		},
		{
			init: elm$core$Basics$always(
				_Utils_Tuple2(init, elm$core$Platform$Cmd$none)),
			subscriptions: elm$core$Basics$always(elm$core$Platform$Sub$none),
			update: F2(
				function (msg, model_) {
					return _Utils_Tuple2(
						A2(update, msg, model_),
						elm$core$Platform$Cmd$none);
				}),
			view: view
		});
	return elm$browser$Browser$sandbox(
		{
			init: options.init(_Utils_Tuple0).a,
			update: F2(
				function (msg, model_) {
					return A2(options.update, msg, model_).a;
				}),
			view: options.view
		});
};
var author$project$Sandbox$main = author$project$TimeTravel$Browser$sandbox(
	{init: author$project$Sandbox$init, update: author$project$Sandbox$update, view: author$project$Sandbox$view});
_Platform_export({'Sandbox':{'init':author$project$Sandbox$main(
	elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));