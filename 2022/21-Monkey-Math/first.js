import * as fs from "fs";

const simple = /(\w+): (\d+)/;
const operation = /(\w+): (\w+) ([+\-*/]) (\w+)/;

function load(filename) {
	let res = {};
	fs.readFileSync(filename, "utf8")
		.trim()
		.split("\n")
		.forEach(line => {
			const s = line.match(simple);
			if (s) {
				res[s[1]] = function() { return parseInt(s[2]); };
			} else {
				const o = line.match(operation);
				res[o[1]] = function () {
					let left = res[o[2]]();
					let right = res[o[4]]();
					switch(o[3]) {
					case "+": return left + right;
					case "-": return left - right;
					case "*": return left * right;
					case "/": return left / right;
					}
				};
			}
		});
	return res;
}

let example = load("example");
let input = load("input");

function solve(input) {
	return input["root"]();
}

const exampleOutput = solve(example);
if (exampleOutput !== 152) {
	console.log("Example failed with " + exampleOutput);
	process.exit(1); // eslint-disable-line
}

console.log(solve(input));
