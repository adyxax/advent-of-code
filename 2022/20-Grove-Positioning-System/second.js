import * as fs from "fs";

class File {
	constructor(line) {
		this.length = line.length;
		this.data = line.map(v => ({value: v * 811589153})); // index in data
		this.data[0].left = this.data[this.length-1];
		this.data[0].right = this.data[1];
		this.data[this.length-1].left = this.data[this.length-2];
		this.data[this.length-1].right = this.data[0];
		for (let i=1; i<this.length-1; ++i) {
			this.data[i].left = this.data[i-1];
			this.data[i].right = this.data[i+1];
		}
		this.data.forEach(v => { if (v.value === 0) this.head = v; });
	}
	get(i) {
		let it = this.head;
		for (let offset = i % this.length; offset > 0; --offset) {
			it = it.right;
		}
		return it.value;
	}
	mix() {
		this.data.forEach(v => {
			if (v.value !== 0) {
				let it = v;
				let offset; // javascript and modulo... what a nightmare!
				if (v.value > 0) offset = v.value % (this.length - 1);
				if (v.value < 0) offset = ((v.value * -1) % (this.length - 1)) * -1;
				if (offset !==0) {
					v.right.left = v.left;
					v.left.right = v.right;
					if (offset < 0) {
						for (; offset < 0; ++offset) {
							it = it.left;
						}
						v.right = it;
						v.left = it.left;
						v.left.right = v;
						v.right.left = v;
					} else {
						for (; offset > 0; --offset) {
							it = it.right;
						}
						v.right = it.right;
						v.left = it;
						v.left.right = v;
						v.right.left = v;
					}
				}
			}
		});
	}
	print() {
		let it = this.head;
		for (let offset = this.length; offset > 0; --offset) {
			console.log(it.value);
			it = it.right;
		}
		console.log("---");
	}
}

function load(filename) {
	return new File(
		fs.readFileSync(filename, "utf8")
		.trim()
		.split("\n")
		.map(line => parseInt(line))
	);
}

let example = load("example");
let input = load("input");

function solve(input) {
	for (let i=0; i<10; ++i) {
		input.mix();
	}
	return input.get(1000) + input.get(2000) + input.get(3000);
}

const exampleOutput = solve(example);
if (exampleOutput !== 1623178306) {
	console.log("Example failed with " + exampleOutput);
	process.exit(1); // eslint-disable-line
}

console.log(solve(input));
