import * as fs from "fs";

function load(filename) {
	return fs.readFileSync(filename, "utf8")
		.trim()
		.split("\n");
}

let example = load("example");
let input = load("input");

function countSides(x, y, z, input) {
	let count = 0;
	if (input.includes([x+1, y, z].join(","))) count++;
	if (input.includes([x-1, y, z].join(","))) count++;
	if (input.includes([x, y+1, z].join(","))) count++;
	if (input.includes([x, y-1, z].join(","))) count++;
	if (input.includes([x, y, z+1].join(","))) count++;
	if (input.includes([x, y, z-1].join(","))) count++;
	return count;
}

function solve(input) {
	// compute the boundaries of the playing field
	let min = Infinity;
	let max = -Infinity;
	input.forEach(line => {
		let [x, y, z] = line.split(",").map(n => parseInt(n));
		min = Math.min(min, x, y, z);
		max = Math.max(max, x, y, z);
	})
	min -=1;
	max +=1;
	// start with an edge and fill up the space
	let count = 0;
	let queue = [[min, min, min]];
	let visited = new Set();
	while (queue.length > 0) {
		const elt = queue.shift();
		const eltstr = elt.join(",");
		const [x, y, z] = elt;
		if (x < min || x > max || y < min || y > max || z < min || z > max) continue;
		if (visited.has(eltstr)) continue
		if (input.includes(eltstr)) continue;
		visited.add(eltstr);
		count += countSides(x, y, z, input);
		queue.push([x+1, y, z]);
		queue.push([x-1, y, z]);
		queue.push([x, y+1, z]);
		queue.push([x, y-1, z]);
		queue.push([x, y, z+1]);
		queue.push([x, y, z-1]);
	}
	return count;
}

const exampleOutput = solve(example);
if (exampleOutput !== 58) {
	console.log("Example failed with " + exampleOutput);
	process.exit(1); // eslint-disable-line
}

console.log(solve(input));
