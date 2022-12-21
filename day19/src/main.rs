use regex::Regex;
use std::fs;
use std::path::Path;
use std::io::Write;

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

    fn best_geodes(&self, total_time: u16, bp: &Blueprint, best: &mut u16) {
        if self.time >= total_time {
            return;
        }

        let time_remaining = total_time - self.time;
        // conservative estimate: sum from 0 to time_remaining-1
        let additional = time_remaining * (time_remaining - 1) / 2;

        if self.geodes_till_end + additional <= *best {
            return;
        }

        *best = self.geodes_till_end.max(*best);

        if let Some(wait) = when_sufficient(self.ore, self.ore_production, bp.ore_robot_ore) {
            let mut s = self.advance(wait + 1);
            s.ore -= bp.ore_robot_ore;
            s.ore_production += 1;
            s.best_geodes(total_time, bp, best);
        }

        if let Some(wait) = when_sufficient(self.ore, self.ore_production, bp.clay_robot_ore) {
            let mut s = self.advance(wait + 1);
            s.ore -= bp.clay_robot_ore;
            s.clay_production += 1;
            s.best_geodes(total_time, bp, best);
        }

        if let Some(w1) = when_sufficient(self.ore, self.ore_production, bp.obsidian_robot_ore) {
            if let Some(w2) = when_sufficient(self.clay, self.clay_production, bp.obsidian_robot_clay) {
                let wait = w1.max(w2);
                let mut s = self.advance(wait + 1);
                s.ore -= bp.obsidian_robot_ore;
                s.clay -= bp.obsidian_robot_clay;
                s.obsidian_production += 1;
                s.best_geodes(total_time, bp, best);
            }
        }

        if let Some(w1) = when_sufficient(self.ore, self.ore_production, bp.geode_robot_ore) {
            if let Some(w2) = when_sufficient(self.obsidian, self.obsidian_production, bp.geode_robot_obsidian) {
                let wait = w1.max(w2);
                let mut s = self.advance(wait + 1);
                if s.time < total_time {
                    s.ore -= bp.geode_robot_ore;
                    s.obsidian -= bp.geode_robot_obsidian;
                    s.geodes_till_end += total_time - s.time;
                    s.best_geodes(total_time, bp, best);
                }
            }
        }
    }
}

impl Blueprint {
    fn best_geodes(&self, total_time: u16) -> u16 {
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

        let mut best = 0;
        initial_state.best_geodes(total_time, self, &mut best);
        best
    }
}

fn testcase(fname: impl AsRef<Path>) {
    let fname = fname.as_ref();
    println!("{fname:?}");
    let blueprints = parse_input(fname);

    let mut answer: u32 = 0;
    for blueprint in &blueprints {
        print!("Considering blueprint {} - ", blueprint.blueprint_no);
        std::io::stdout().flush().unwrap();
        let best = blueprint.best_geodes(24);
        println!("Best geode count: {best}");
        answer += blueprint.blueprint_no as u32 * best as u32;
    }

    println!("Part 1: {answer}");

    let mut answer: u64 = 1;
    for blueprint in blueprints.iter().take(3) {
        print!("Considering blueprint {} - ", blueprint.blueprint_no);
        std::io::stdout().flush().unwrap();
        let best = blueprint.best_geodes(32);
        println!("Best geode count: {best}");
        answer *= best as u64;
    }

    println!("Part 2: {answer}");
}

fn main() {
    testcase("example.in");
    testcase("puzzle.in");
}
