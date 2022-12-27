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

const masks = [
	0b1000000,
	0b0100000,
	0b0010000,
	0b0001000,
	0b0000100,
	0b0000010,
	0b0000001,
];

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
	heightMap() {
		const max = this.data.length;
		let heights = [max, max, max, max, max, max, max];
		heights.forEach((h, i) => {
			for(let j=0; j<this.data.length; ++j) {
				if (this.data[j] & masks[i]) {
					heights[i] = j;
					break;
				}
			}
		});
		return heights;
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
	let height = 0;
	let heightmaps = [];
	const sky = 1000000000000;
	for(let i=0; i<sky; ++i) {
		let shape = shaper.next(field);
		do {
			shape.shift(field, input.next());
		} while(shape.fall(field));
		const heightmap = field.heightMap();
		let cycle = -1;
		let cycleHeight = 0;
		heightmaps.forEach((h, n) => {
			if (h.heightmap.every((v, idx) => v === heightmap[idx])) {
				let nextCycle = heightmaps[(n+1)*2-1];
				if (nextCycle !== undefined && nextCycle.heightmap.every((v, idx) => v === heightmap[idx])) {
					let nextCycle2 = heightmaps[(n+1)*3-1];
					if (nextCycle2 !== undefined && nextCycle2.heightmap.every((v, idx) => v === heightmap[idx])) {
						cycle = n+1;
						cycleHeight = field.data.length - h.height;
					}
				}
			}
		});
		if (cycle > 0) {
			console.log("got cycle of", cycle, "with a height of", cycleHeight, "after", i, "iterations");
			const cycles = Math.floor((sky - i) / cycle);
			i += cycles * cycle;
			height = cycles * cycleHeight;
			console.log("computed a height of", height, "after", cycles * cycle, "additional cycles");
			heightmaps = [];
		}
		heightmaps.unshift({ heightmap: heightmap, height: field.data.length });
	}
	return height + field.data.length;
}

const exampleOutput = solve(example);
if (exampleOutput !== 1514285714288) {
	console.log("Example failed with height " + exampleOutput, "diff is", exampleOutput - 1514285714288, "positive means too high");
	process.exit(1); // eslint-disable-line
}

console.log(solve(input));
