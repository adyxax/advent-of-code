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
				let nq = new PriorityQueue();
				nq.enqueue(this, 0);
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
let parcours = [{current: "AA", actives: uv, steps: [], timeLeft: 26, pressure: 0}];
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
				steps: [...p.steps, v],
				timeLeft: p.timeLeft - cost,
				pressure: p.pressure,
			});
		}
	});
}
parcours.sort((a, b) => b.pressure-a.pressure); // let's process all these paths and look for two that do not intersec and improve our max
for (let i = 0; i < parcours.length; i++) {
	if (parcours[i].pressure + parcours[0].pressure < max) continue;
	for (let j = i+1; j < parcours.length; j++) {
		if (parcours[i].pressure+parcours[j].pressure < max) continue;
		if (parcours[i].steps.every(s => !parcours[j].steps.includes(s)))
			if (parcours[i].pressure+parcours[j].pressure > max) {
				max = parcours[i].pressure+parcours[j].pressure;
			}
	}
}

console.log(max);
