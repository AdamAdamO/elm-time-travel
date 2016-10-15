#!/usr/bin/env node

var argv = require('argv');

argv.option({
	name: 'output',
	short: 'o',
	type : 'string',
	description : 'run elm-debug instead of elm-make',
	example: "'elm-debug Main.elm --output=main.js'"
});

var args = argv.run();

require('./index.js').make(args.targets, args.options);
