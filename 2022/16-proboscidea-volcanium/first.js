import * as fs from "fs";
import { PriorityQueue } from "./priority_queue.js";

const regexp = /Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? ([A-Z, ]+)/;

let valves = {};
let uv = []; // useful valves

fs.readFileSync("input", "utf8")
	.trim()
	.split("\n")
	.map(line => {
		let a = line.match(regexp);
		valves[a[1]] = {
			cost: undefined,
			label: a[1],
			links: a[3].split(", "),
			rate: a[2],
			computePathCostTo: function (target) {
				Object.values(valves).forEach(v => v.cost = 0);
				let nq = new PriorityQueue(this);
				while (true) {
					let n = nq.dequeue();
					if (n.element.label === target) {
						return n.element.cost + 1; // +1 to account for opening the valve
					}
					n.element.links.forEach(l => {
						let v = valves[l];
						if (v.cost === 0) {
							v.cost = n.element.cost + 1;
							nq.enqueue(v, v.cost);
						}
					});
				}
			},
		};
		if (a[2] > 0) {
			uv.push(a[1]);
		}
	});


let paths = {};
uv.forEach(v => {
	paths["AA"+v] = valves["AA"].computePathCostTo(v);
	uv.forEach(w => {
		if (v !== w) {
			paths[v+w] = valves[v].computePathCostTo(w);
			paths[w+v] = paths[v+w];
		}
	});
});

let max = 0;
let parcours = [{current: "AA", actives: uv, timeLeft: 30, pressure: 0}];
for(let i=0; i<parcours.length; i++) {
	let p = parcours[i];
	p.pressure += valves[p.current].rate * p.timeLeft;
	if (max < p.pressure) {
		max = p.pressure;
	}
	p.actives.forEach(v => {
		const cost = paths[p.current+v];
		if (p.timeLeft - cost > 0) {
			parcours.push({
				current:v,
				actives:p.actives.filter(f => f !== v), // this creates a shallow copy
				timeLeft: p.timeLeft - cost,
				pressure: p.pressure,
			});
		}
	});
}

console.log(max);
