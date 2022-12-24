use std::fs;
use std::path::Path;
use ndarray::Array2;

type Pos = (usize, usize);
type Delta = (isize, isize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Command {
    TurnLeft,
    TurnRight,
    Walk(u32),
}

fn turn_right((dx, dy): Delta) -> Delta {
    (-dy, dx)
}

fn turn_left((dx, dy): Delta) -> Delta {
    (dy, -dx)
}

fn parse_commands(s: &str) -> Vec<Command> {
    use Command::*;
    let mut commands = vec![];
    let mut s = s.trim();

    while let Some(c) = s.chars().next() {
        if c == 'L' {
            commands.push(TurnLeft);
            s = &s[1..];
        } else if c == 'R' {
            commands.push(TurnRight);
            s = &s[1..];
        } else {
            let end = s.find(|c| c == 'L' || c == 'R').unwrap_or(s.len());
            let (num, rest) = s.split_at(end);
            s = rest;

            commands.push(Walk(num.parse().unwrap()));
        }
    }

    commands
}

fn testcase(fname: impl AsRef<Path>) {
    let fname = fname.as_ref();
    println!("{fname:?}");
    let data = fs::read_to_string(fname).unwrap();

    let Some((map, cmds)) = data.split_once("\n\n") else { panic!() };
    let cmds = parse_commands(cmds);

    let lines: Vec<&str> = map.lines().collect();
    let width = lines.iter().map(|s| s.len()).max().unwrap();
    let height = lines.len();

    let mut grid = Array2::from_elem((width, height), b' ');
    for (y, line) in lines.iter().enumerate() {
        for (x, c) in line.bytes().enumerate() {
            grid[[x, y]] = c;
        }
    }

    let width = width as isize;
    let height = height as isize;

    let mut y = 0;
    let mut x = (0..).find(|&x| grid[[x, y]] != b' ').unwrap();
    let mut dx = 1;
    let mut dy = 0;

    let neighbour = |(mut x, mut y): Pos, (dx, dy): Delta| -> Pos {
        loop {
            x = (x as isize + dx).rem_euclid(width) as usize;
            y = (y as isize + dy).rem_euclid(height) as usize;
            if grid[[x, y]] != b' ' {
                return (x, y);
            }
        }
    };

    for cmd in cmds {
        match cmd {
            Command::TurnLeft => (dx, dy) = turn_left((dx, dy)),
            Command::TurnRight => (dx, dy) = turn_right((dx, dy)),
            Command::Walk(n) => {
                for _ in 0..n {
                    let next = neighbour((x, y), (dx, dy));
                    if grid[next] == b'#' {
                        break;
                    }
                    (x, y) = next;
                }
            }
        }
    }

    let facing = match (dx, dy) {
        (1, 0) => 0,
        (0, 1) => 1,
        (-1, 0) => 2,
        (0, -1) => 3,
        _ => unreachable!(),
    };

    let x = x + 1;
    let y = y + 1;
    let answer = 1000 * y + 4 * x + facing;

    println!("Part 1: {answer}");
}

fn main() {
    testcase("example.in");
    testcase("puzzle.in");
}
