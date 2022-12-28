import * as fs from "fs";
import { PriorityQueue } from "./priority_queue.js";

const regexp = /Blueprint (?<id>\d+): Each ore robot costs (?<ore_ore>\d+) ore. Each clay robot costs (?<clay_ore>\d+) ore. Each obsidian robot costs (?<obsidian_ore>\d+) ore and (?<obsidian_clay>\d+) clay. Each geode robot costs (?<geode_ore>\d+) ore and (?<geode_obsidian>\d+) obsidian./;

const ORE=0, CLAY=1, OBSIDIAN=2, GEODE=3;
const startingRobots = [1, 0, 0, 0];
const startingRes = [0, 0, 0, 0];
const startingTtl = 24;

class Robot {
	constructor(type, ore, clay, obsidian) {
		this.type = type;
		this.cost = [ore, clay, obsidian, 0];
	}
	canAfford(res) {
		return this.cost.map((r, i) => res[i] >= r).reduce((acc, v) => acc && v);
	}
	schedule(from) { // from is the state we are scheduling from
		const time = 1 + Math.max( // 1 turn to build + time to gather the limiting resource
			...this.cost.map((r, i) => from.res[i] >= r ? 0 : Math.ceil((r - from.res[i])/from.robots[i])) // (cost - res)/prod_rate
		);
		if (time >= from.ttl) return null; // we don't have time to build this
		return new State(
			from.robots.map((r, i) => i === this.type ? r+1 : r),
			from.res.map((r, i) => r + from.robots[i] * time - this.cost[i]),
			from.ttl - time,
		);
	}
}

class State {
	constructor(robots, res, ttl) {
		this.robots = robots;
		this.res = res;
		this.ttl = ttl;
	}
	nextStates(blueprint) {
		let ns = [];
		if (this.ttl > 2) {
			if (blueprint.maxes[ORE] > this.robots[ORE]) {
				ns.push(blueprint.bots[ORE].schedule(this));
			}
			if (blueprint.maxes[CLAY] > this.robots[CLAY]) {
				ns.push(blueprint.bots[CLAY].schedule(this));
			}
			if (blueprint.maxes[OBSIDIAN] > this.robots[OBSIDIAN] && this.robots[CLAY] > 0) {
				ns.push(blueprint.bots[OBSIDIAN].schedule(this));
			}
			if (this.robots[OBSIDIAN] > 0) {
				ns.push(blueprint.bots[GEODE].schedule(this));
			}
		}
		return ns.filter(s => s !== null);
	}
}

class Blueprint {
	constructor(b) {
		this.id = b.id;
		this.bots = [
			new Robot(ORE, b.ore_ore, 0, 0),
			new Robot(CLAY, b.clay_ore, 0, 0),
			new Robot(OBSIDIAN, b.obsidian_ore, b.obsidian_clay, 0),
			new Robot(GEODE, b.geode_ore, 0, b.geode_obsidian),
		];
		this.maxes = this.bots.map(r => r.cost).reduce((acc, b) => b.map((v, i) => Math.max(v, acc[i])));
	}
	maxGeodes() {
		let max = { max: 0 };
		let nq = new PriorityQueue(new State(startingRobots, startingRes, startingTtl));
		while(!nq.isEmpty()) {
			let elt = nq.dequeue();
			const state = elt.element;
			if (state.ttl > 1 ) state.nextStates(this).forEach(s => nq.enqueue(s, s.res[GEODE]));
			if (state.robots[GEODE] === 0) continue;
			state.max = state.res[GEODE] + state.robots[GEODE] * state.ttl; // compute this state to the end of the ttl for geodes
			if (max.max < state.max) max = state;
		}
		return max.max;
	}
}

function load(filename) {
	return fs.readFileSync(filename, "utf8")
		.trim()
		.split("\n")
		.map(line => new Blueprint(line.match(regexp).groups));
}

let example = load("example");
let input = load("input");

function solve(input) {
	let count = 0;
	input.forEach(b => count += b.id * b.maxGeodes());
	return count;
}

const exampleOutput = solve(example);
if (exampleOutput !== 33) {
	console.log("Example failed with " + exampleOutput);
	process.exit(1); // eslint-disable-line
}

console.log(solve(input));
