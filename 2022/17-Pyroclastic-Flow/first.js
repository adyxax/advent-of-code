import * as fs from "fs";

function load(filename) {
	let data = fs.readFileSync(filename, "utf8").trim();
	const l = data.length;
	let i = 0;
	return {
		next: function () {
			const c = data[i];
			i = (i + 1) % l;
			return c;
		}
	};
}

const shapes = [
	[0b0011110],
	[0b0001000, 0b0011100, 0b0001000],
	[0b0000100, 0b0000100, 0b0011100],
	[0b0010000, 0b0010000, 0b0010000, 0b0010000],
	[0b0011000, 0b0011000],
];
const shapes_len = shapes.length;

class Shape {
	constructor(shape) {
		this.shape = shape;
		this.length = shape.length;
	}
	fall(field) { // returns true if the shape fell without colliding
		for (let i=0; i<this.length; ++i) {
			const next = field.get(i+1);
			if (next === undefined || next & this.shape[i]) { //collision
				field.solidify(this.shape);
				return false;
			}
		}
		field.fall();
		return true;
	}
	shift(field, direction) {
		let collision = false;
		let tmp;
		switch(direction) {
		case ">":
			tmp = this.shape.map(line => {
				if (line & 0b1) { // we touch the right edge already
					collision = true;
				}
				return line >>> 1;
			});
			break;
		case "<":
			tmp = this.shape.map(line => {
				if (line & 0b1000000) { // we touch the left edge already
					collision = true;
				}
				return line << 1;
			});
			break;
		default:
			throw "invalid direction character in shape shift: " + direction;
		}
		if (collision) {
			return;
		}
		for (let i=0; i<this.length; ++i) {
			if (field.get(i) & tmp[i]) { //collision
				return;
			}
		}
		this.shape = tmp;
	}
	print() {
		this.shape.forEach(line => {
			console.log(
				line & 0b1000000 ? "#" : ".",
				line & 0b0100000 ? "#" : ".",
				line & 0b0010000 ? "#" : ".",
				line & 0b0001000 ? "#" : ".",
				line & 0b0000100 ? "#" : ".",
				line & 0b0000010 ? "#" : ".",
				line & 0b0000001 ? "#" : ".",
			);
		});
		console.log("=======");
	}
}
class Shaper {
	constructor() {
		this.index = 0;
	}
	next(field) {
		const s = new Shape(shapes[this.index]);
		field.accomodate(shapes[this.index].length + 3);
		this.index = (this.index + 1) % shapes_len;
		return s;
	}
}
class Field {
	constructor() {
		this.data = [];
		this.offset = 0;
	}
	accomodate(n) {
		this.offset = 0;
		for(let i=0; i<n; ++i) {
			this.data.unshift(0);
		}
	}
	fall() {
		if (this.data[0] !== 0) { // we need to fall bellow the top line?
			this.offset++;
		} else {
			this.data.shift();
		}
	}
	get(offset) {
		return this.data[this.offset + offset];
	}
	print() {
		this.data.forEach(line => {
			console.log(
				line & 0b1000000 ? "#" : ".",
				line & 0b0100000 ? "#" : ".",
				line & 0b0010000 ? "#" : ".",
				line & 0b0001000 ? "#" : ".",
				line & 0b0000100 ? "#" : ".",
				line & 0b0000010 ? "#" : ".",
				line & 0b0000001 ? "#" : ".",
			);
		});
		console.log("-------");
	}
	solidify(shape) {
		for(let i=0; i<shape.length; ++i) {
			this.data[i+this.offset] |= shape[i];
		}
	}
}

let example = load("example");
let input = load("input");

function solve(input) {
	let field = new Field();
	let shaper = new Shaper();
	for(let i=0; i<2022; ++i) {
		let shape = shaper.next(field);
		do {
			shape.shift(field, input.next());
		} while(shape.fall(field));
	}
	field.print();
	return field.data.length;
}

const exampleOutput = solve(example);
if (exampleOutput !== 3068) {
	console.log("Example failed with height " + exampleOutput);
	process.exit(1); // eslint-disable-line
}

console.log(solve(input));
