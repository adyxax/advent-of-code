import * as fs from "fs";

const simple = /(\w+): (\d+)/;
const operation = /(\w+): (\w+) ([+\-*/]) (\w+)/;

class Monkey {
	constructor(line) {
		// left and right are temporarily strings
		this.left = null;
		this.right = null;
		this.up = null;
		const s = line.match(simple);
		if (s) {
			this.name = s[1];
			this.value = BigInt(s[2]);
		} else {
			const o = line.match(operation);
			if (!o) throw "wtf";
			this.name = o[1];
			this.left = o[2];
			this.operation = o[3];
			this.right = o[4];
			this.value = undefined;
		}
	}
	solve(m, input) {
		if (this.name === "humn") {
			throw "unreachable humn";
		}
		if (this.value !== undefined) {
			console.log (this.name, " = ", this.value);
			return this.value;
		}
		switch(m) {
		case this.up: return this.solveUp(input);
		case this.left: return this.solveLeft(input);
		case this.right: return this.solveRight(input);
		default: throw "unreachable";
		}
	}
	solveUp(input) {
		if (this.name === "root") {
			throw "unreachable";
		}
		const left = input[this.left].solve(this.name, input);
		const right = input[this.right].solve(this.name, input);
		console.log("UP\t", this.name, " = ", this.left, "(", left, ") ", this.operation, " ", this.right, " (", right, ")");
		switch(this.operation) { // up = left op right
		case "+": return left + right;
		case "-": return left - right;
		case "*": return left * right;
		case "/": return left / right;
		default: throw "unreachable";
		}
	}
	solveLeft(input) {
		if (this.name === "root") {
			const root = input[this.right].solve(this.name, input);
			console.log("root = ", root);
			return root;
		}
		const up = input[this.up].solve(this.name, input);
		const right = input[this.right].solve(this.name, input);
		console.log("LEFT\t", this.name, "(", up, ") = ", this.left, this.operation, " ", this.right, " (", right, ")");
		switch(this.operation) { // left = up invop right
		case "+": return up - right;
		case "-": return up + right;
		case "*": return up / right;
		case "/": return up * right;
		default: throw "unreachable";
		}
	}
	solveRight(input) {
		if (this.name === "root") {
			const root = input[this.left].solve(this.name, input);
			console.log("root = ", root);
			return root;
		}
		const up = input[this.up].solve(this.name, input);
		const left = input[this.left].solve(this.name, input);
		console.log("RIGHT\t", this.name, "(", up, ") = ", this.left, "(", left, ") ", this.operation, " ", this.right);
		switch(this.operation) { // right = up invop right
		case "+": return up - left;
		case "-": return left - up;
		case "*": return up / left;
		case "/": return left / up;
		default: throw "unreachable";
		}
	}
	print() {
		console.log(this.name, this.value);
	}
}

function load(filename) {
	let res = {};
	fs.readFileSync(filename, "utf8")
		.trim()
		.split("\n")
		.forEach(line => {
			let m = new Monkey(line);
			res[m.name] = m;
		});
	Object.values(res).forEach(m => {
		if (m.value === undefined) {
			res[m.right].up = m.name;
			res[m.left].up = m.name;
		}
	});
	return res;
}

let example = load("example");
let input = load("input");

function solve(input) {
	let h = input["humn"];
	return input[h.up].solve(h.name, input);
}

const exampleOutput = solve(example);
if (exampleOutput !== 301n) {
	console.log("Example failed with " + exampleOutput);
	process.exit(1); // eslint-disable-line
}

console.log(solve(input));
