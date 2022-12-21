use regex::Regex;
use std::fs;
use std::path::Path;
use std::collections::{VecDeque};
use fnv::FnvHashSet;

#[derive(Debug)]
struct Blueprint {
    blueprint_no: u32,
    ore_robot_ore: u16,
    clay_robot_ore: u16,
    obsidian_robot_ore: u16,
    obsidian_robot_clay: u16,
    geode_robot_ore: u16,
    geode_robot_obsidian: u16,
}

fn parse_input(fname: &Path) -> Vec<Blueprint> {
    let data = fs::read_to_string(fname).unwrap();
    let regex = Regex::new(r"^Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.").unwrap();
    
    data.lines()
        .map(|line| {
            let cap = regex.captures(line).unwrap();
            Blueprint {
                blueprint_no: cap[1].parse().unwrap(),
                ore_robot_ore: cap[2].parse().unwrap(),
                clay_robot_ore: cap[3].parse().unwrap(),
                obsidian_robot_ore: cap[4].parse().unwrap(),
                obsidian_robot_clay: cap[5].parse().unwrap(),
                geode_robot_ore: cap[6].parse().unwrap(),
                geode_robot_obsidian: cap[7].parse().unwrap(),
            }
        })
        .collect()
}

const TOTAL_TIME: u16 = 24;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct State {
    time: u16,
    geodes_till_end: u16,

    ore_production: u16,
    clay_production: u16,
    obsidian_production: u16,
    
    ore: u16,
    clay: u16,
    obsidian: u16,
}

fn when_sufficient(now: u16, production: u16, need: u16) -> Option<u16> {
    if production == 0 {
        None
    } else if now >= need {
        Some(0)
    } else {
        let need_more = need - now;
        Some((need_more + production - 1) / production)
    }
}

impl State {
    fn advance(&self, t: u16) -> State {
        State {
            time: self.time + t,
            ore: self.ore + t * self.ore_production,
            clay: self.clay + t * self.clay_production,
            obsidian: self.obsidian + t * self.obsidian_production,
            ..*self
        }
    }

    fn next_states(&self, bp: &Blueprint, mut cb: impl FnMut(State)) {
        if let Some(wait) = when_sufficient(self.ore, self.ore_production, bp.ore_robot_ore) {
            let mut s = self.advance(wait + 1);
            s.ore -= bp.ore_robot_ore;
            s.ore_production += 1;
            cb(s);
        }

        if let Some(wait) = when_sufficient(self.ore, self.ore_production, bp.clay_robot_ore) {
            let mut s = self.advance(wait + 1);
            s.ore -= bp.clay_robot_ore;
            s.clay_production += 1;
            cb(s);
        }

        if let Some(w1) = when_sufficient(self.ore, self.ore_production, bp.obsidian_robot_ore) {
            if let Some(w2) = when_sufficient(self.clay, self.clay_production, bp.obsidian_robot_clay) {
                let wait = w1.max(w2);
                let mut s = self.advance(wait + 1);
                s.ore -= bp.obsidian_robot_ore;
                s.clay -= bp.obsidian_robot_clay;
                s.obsidian_production += 1;
                cb(s);
            }
        }

        if let Some(w1) = when_sufficient(self.ore, self.ore_production, bp.geode_robot_ore) {
            if let Some(w2) = when_sufficient(self.obsidian, self.obsidian_production, bp.geode_robot_obsidian) {
                let wait = w1.max(w2);
                let mut s = self.advance(wait + 1);
                if s.time < TOTAL_TIME {
                    s.ore -= bp.geode_robot_ore;
                    s.obsidian -= bp.geode_robot_obsidian;
                    s.geodes_till_end += TOTAL_TIME - s.time;
                    cb(s);
                }
            }
        }
    }
}

fn testcase(fname: impl AsRef<Path>) {
    let fname = fname.as_ref();
    println!("{fname:?}");
    let blueprints = parse_input(fname);
    let mut answer: u32 = 0;
    for blueprint in blueprints {
        println!("Considering blueprint {}", blueprint.blueprint_no);

        let initial_state = State {
            time: 0,
            geodes_till_end: 0,

            ore_production: 1,
            clay_production: 0,
            obsidian_production: 0,

            ore: 0,
            clay: 0,
            obsidian: 0,
        };

        let mut visited = FnvHashSet::default();
        let mut queue = VecDeque::new();
        visited.insert(initial_state);
        queue.push_back(initial_state);

        let mut best = 0;
        while let Some(state) = queue.pop_front() {
            state.next_states(&blueprint, |s| {
                if s.time > TOTAL_TIME {
                    return;
                }
                
                if visited.insert(s) {
                    queue.push_back(s);
                    best = best.max(s.geodes_till_end);
                }
            });
        }

        println!("Visited {} states", visited.len());
        println!("Best geode count: {best}");

        answer += blueprint.blueprint_no as u32 * best as u32;

        /*
        if let Some(&(mut best_state)) = visited.keys().find(|s| s.geodes_till_end == best) {
            while best_state.time != 0 {
                dbg!(best_state);
                best_state = visited[&best_state];
            }

            dbg!(best_state);
        }
        break;
        */
    }

    println!("Part 1: {answer}");
}

fn main() {
    testcase("example.in");
    testcase("puzzle.in");
}
