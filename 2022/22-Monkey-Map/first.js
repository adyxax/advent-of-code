import * as fs from 'fs';

class Line {
	constructor(line) {
		this.x = 0;
		for(let i=0; i<line.length; ++i) {
			if (line[i] == ' ') ++this.x;
		}
		this.data = line.slice(this.x);
	}
}

const RIGHT=0, DOWN=1, LEFT=2, UP=3;

class Walker {
	constructor(lines, algo) {
		this.lines = lines;
		this.x = lines[0].x;
		this.y = 0;
		this.algo = algo;
		this.direction=RIGHT;
		this.height = this.lines.length;
	}
	isIn(x, y) {
		if (y >= 0 && y < this.height) {
			const line = this.lines[y];
			if (x >= line.x && x < line.x + line.data.length) {
				return true;
			}
		}
		return false;
	}
	step(d) {
		for(; d>0; --d) {
			let line;
			switch(this.direction) {
			case RIGHT:
				line = this.lines[this.y];
				if (this.isIn(this.x+1, this.y)) {
					if (line.data[this.x+1-line.x] === '.') {
						++this.x;
					}
				} else {
					if (line.data[0] === '.') {
						this.x = line.x;
					}
				}
				break;
			case LEFT:
				line = this.lines[this.y];
				if (this.isIn(this.x-1, this.y)) {
					if (line.data[this.x-1-line.x] === '.') {
						--this.x;
					}
				} else {
					if (line.data[line.data.length - 1] === '.') {
						this.x = line.x + line.data.length - 1;
					}
				}
				break;
			case DOWN:
				if (this.isIn(this.x, this.y+1)) {
					line = this.lines[this.y+1];
					if (line.data[this.x-line.x] === '.') {
						++this.y;
					}
				} else {
					let y = this.y;
					while (this.isIn(this.x, y - 1)) {
						--y;
					}
					line = this.lines[y];
					if (line.data[this.x - line.x] === '.') {
						this.y = y;
					}
				}
				break;
			case UP:
				if (this.isIn(this.x, this.y-1)) {
					line = this.lines[this.y-1];
					if (line.data[this.x-line.x] === '.') {
						--this.y;
					}
				} else {
					let y = this.y;
					while (this.isIn(this.x, y + 1)) {
						++y;
					}
					line = this.lines[y];
					if (line.data[this.x - line.x] === '.') {
						this.y = y;
					}
				}
				break;
			default: throw "unreachable";
			}
		}
	}
	walk() {
		let d = 0;
		for (let i=0; i<this.algo.length; ++i) {
			const c = this.algo[i];
			switch(c) {
			case 'L':
				this.step(d);
				d = 0;
				this.direction = (this.direction + 3) % 4;
				break;
			case 'R':
				this.step(d);
				d = 0;
				this.direction = (this.direction +1) % 4;
				break;
			default:
				d = d * 10 + parseInt(c);
			}
		}
		this.step(d);
		return 1000 * (this.y + 1) + 4 * (this.x + 1) + this.direction;
	}
}

function load(filename) {
	let lines = fs.readFileSync(filename, 'utf8').split('\n');
	return new Walker(
		lines.slice(0, -3).map(line => new Line(line)),
		lines[lines.length - 2]
	);
}

let example = load('example');
let input = load('input');

function solve(input) {
	return input.walk();
}

const exampleOutput = solve(example);
if (exampleOutput !== 6032) {
	console.log('Example failed with ' + exampleOutput);
	process.exit(1); // eslint-disable-line
}

console.log(solve(input));
