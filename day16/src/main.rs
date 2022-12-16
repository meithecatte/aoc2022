use regex::Regex;
use once_cell::sync::Lazy;
use std::path::Path;
use std::fs;
use std::collections::HashMap;
use ndarray::Array3;

#[derive(Debug)]
struct Valve<'a> {
    name: &'a str,
    flow_rate: u32,
    tunnels: Vec<&'a str>,
}

static REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)")
        .unwrap()
});

fn parse_line(a: &str) -> Valve<'_> {
    let cap = REGEX.captures(a).unwrap();
    Valve {
        name: cap.get(1).unwrap().as_str(),
        flow_rate: cap[2].parse().unwrap(),
        tunnels: cap.get(3).unwrap().as_str().split(", ").collect(),
    }
}

fn testcase(fname: impl AsRef<Path>) {
    let fname = fname.as_ref();
    println!("{fname:?}");
    let data = fs::read_to_string(fname).unwrap();
    let valves: Vec<Valve<'_>> = data.lines().map(parse_line).collect();
    let num_valves = valves.len();
    let names: HashMap<&str, usize> = valves.iter().enumerate()
        .map(|(i, v)| (v.name, i)).collect();

    let flow_rates: Vec<u32> = valves.iter().map(|v| v.flow_rate).collect();

    let (masks, maskdim): (Vec<u32>, u32) = {
        let mut masks: Vec<u32> = vec![];

        let mut nonzero_valves = 0;
        for valve in &valves {
            if valve.flow_rate != 0 {
                masks.push(1 << nonzero_valves);
                nonzero_valves += 1;
            } else {
                masks.push(0);
            }
        }

        println!("{nonzero_valves} non-zero valves");
        (masks, 1 << nonzero_valves)
    };

    let tunnels: Vec<Vec<usize>> = valves.into_iter().map(|v| {
        v.tunnels.into_iter().map(|s| names[s]).collect()
    }).collect();

    let total_time: usize = 30;

    let mut dp: Array3<u32> = Array3::zeros((total_time + 1, num_valves, maskdim as usize));

    for time_left in 1..=total_time {
        for cur_valve in 0..num_valves {
            for used_valves in 0..maskdim {
                let mut best = 0;

                for v in &tunnels[cur_valve] {
                    best = best.max(dp[(time_left - 1, *v, used_valves as usize)]);
                }

                if used_valves & masks[cur_valve] == 0 {
                    let potential_flow = (time_left - 1) as u32 * flow_rates[cur_valve];
                    let new_mask = used_valves | masks[cur_valve];
                    best = best.max(potential_flow + dp[(time_left - 1, cur_valve, new_mask as usize)]);
                }

                dp[(time_left, cur_valve, used_valves as usize)] = best;
            }
        }
    }

    let startv = names["AA"];
    println!("Part 1: {}", dp[(total_time, startv, 0)]);

    let part2 = (0..maskdim).map(|elephant_mask| {
        let our_mask = (maskdim - 1) ^ elephant_mask;
        dp[(26, startv, elephant_mask as usize)] + dp[(26, startv, our_mask as usize)]
    }).max().unwrap();

    println!("Part 2: {part2}");
}

fn main() {
    testcase("example.in");
    testcase("puzzle.in");
}
