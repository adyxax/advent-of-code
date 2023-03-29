import * as fs from 'fs';

const setMask = /mask = ([10X]+)/;
const operation = /mem\[(\d+)] = (\d+)/;

function applyMask(addr, mask) {
	let ret = [parseInt(addr).toString(2).padStart(36, '0')];
	for (let i=0; i<36; ++i) {
		switch (mask.charAt(i)) {
		case '1':
			ret = ret.map(a => a.substring(0, i) + '1' + a.substring(i+1));
			break;
		case 'X':
			ret = ret.reduce((acc, a) => acc.concat([
				a.substring(0, i) + '1' + a.substring(i+1),
				a.substring(0, i) + '0' + a.substring(i+1),
			]), []);
		}
	}
	return ret.map(a => parseInt(a, 2));
}

function processMasks(lines) {
	let ret = new Map();
	let mask = '';
	lines.forEach(line => {
		const m = line.match(setMask);
		if (m) {
			mask = m[1];
		} else {
			const o = line.match(operation);
			const addr = o[1];
			const value = parseInt(o[2]);
			applyMask(addr, mask).forEach(r => ret.set(parseInt(r), value));
		}
	});
	return ret;
}

function load(filename) {
	return processMasks(fs.readFileSync(filename, 'utf8')
		.trim()
		.split('\n')
	);
}

let example = load('example2');
let input = load('input');

function solve(input) {
	let sum = 0;
	input.forEach(value => sum += value);
	return sum;
}

const exampleOutput = solve(example);
if (exampleOutput !== 208) {
	console.log('Example failed with ' + exampleOutput);
	process.exit(1); // eslint-disable-line
}

console.log(solve(input));
