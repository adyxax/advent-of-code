import * as fs from "fs";

function load(filename) {
	return fs.readFileSync(filename, "utf8")
		.trim()
		.split("\n");
}

let example = load("example");
let input = load("input");

function countSides(cube, input) {
	let [x, y, z] = cube.split(",").map(n => parseInt(n));
	let count = 6;
	if (input.includes([x+1, y, z].join(","))) count--;
	if (input.includes([x-1, y, z].join(","))) count--;
	if (input.includes([x, y+1, z].join(","))) count--;
	if (input.includes([x, y-1, z].join(","))) count--;
	if (input.includes([x, y, z+1].join(","))) count--;
	if (input.includes([x, y, z-1].join(","))) count--;
	return count;
}

function solve(input) {
	let count = 0;
	input.forEach(cube => {
		count += countSides(cube, input);
	});
	return count;
}

const exampleOutput = solve(example);
if (exampleOutput !== 64) {
	console.log("Example failed with " + exampleOutput);
	process.exit(1); // eslint-disable-line
}

console.log(solve(input));
