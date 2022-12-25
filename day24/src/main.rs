use std::path::Path;
use std::fs;
use ndarray::Array2;
use std::collections::HashSet;

#[derive(Clone, Copy, Debug)]
struct Blizzard {
    x: i32,
    y: i32,
    dx: i32,
    dy: i32,
}

impl Blizzard {
    fn at(&self, t: usize, w: usize, h: usize) -> (usize, usize) {
        let x = self.x + self.dx * t as i32;
        let y = self.y + self.dy * t as i32;
        (x.rem_euclid(w as i32) as usize,
         y.rem_euclid(h as i32) as usize)
    }
}

fn simulate(blizzards: &[Blizzard], t0: usize, width: usize, height: usize,
    startpos: (i32, i32), endpos: (i32, i32)) -> usize
{
    let mut positions = HashSet::from([startpos]);
    for t in (t0 + 1).. {
        let mut grid = Array2::from_elem((width, height), false);
        for blizzard in blizzards {
            grid[blizzard.at(t, width, height)] = true;
        }

        positions = positions.into_iter().flat_map(|(x, y)| {
            [(x, y),
             (x+1, y),
             (x-1, y),
             (x, y+1),
             (x, y-1)]
        }).filter(|&(x, y)| {
            if (x, y) == startpos || (x, y) == endpos {
                return true;
            }

            if x < 0 || y < 0 {
                return false;
            }

            let x = x as usize;
            let y = y as usize;
            matches!(grid.get((x, y)), Some(false))
        }).collect();

        if positions.contains(&endpos) {
            return t;
        }
    }

    unreachable!()
}

fn testcase(fname: impl AsRef<Path>) {
    let fname = fname.as_ref();
    println!("{fname:?}");
    let data = fs::read_to_string(fname).unwrap();
    let height = data.lines().count() - 2;
    let width = data.lines().next().unwrap().len() - 2;

    let mut blizzards = vec![];

    for (y, line) in data.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            let (dx, dy) = match c {
                '#' => {
                    assert!(y == 0 || x == 0 ||
                            y == height + 1 || x == width + 1);
                    continue;
                }
                '.' => continue,
                '>' => (1, 0),
                '<' => (-1, 0),
                '^' => (0, -1),
                'v' => (0, 1),
                c => panic!("Unknown character {c:?}"),
            };

            blizzards.push(Blizzard {
                x: x as i32 - 1,
                y: y as i32 - 1,
                dx,
                dy,
            });
        }
    }

    let startpos = (0, -1);
    let endpos = (width as i32 - 1, height as i32);

    let t1 = simulate(&blizzards, 0, width, height, startpos, endpos);
    println!("Part 1: {t1}");
    let t2 = simulate(&blizzards, t1, width, height, endpos, startpos);
    println!("Backwards: {}", t2 - t1);
    let t3 = simulate(&blizzards, t2, width, height, startpos, endpos);
    println!("Forwards again: {}", t3 - t2);
    println!("Part 2: {t3}");
}

fn main() {
    testcase("example.in");
    testcase("puzzle.in");
}
