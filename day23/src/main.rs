use std::path::Path;
use std::fs;
use ndarray::Array2;
use std::collections::VecDeque;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Cell {
    Empty,
    Elf,
    ReservedUp,
    ReservedDown,
    ReservedLeft,
    ReservedRight,
    Conflict,
}

fn simulate(grid: &mut Array2<Cell>, max_rounds: usize) -> usize {
    let &[width, height] = grid.shape() else { panic!() };

    let propose = |grid: &mut Array2<Cell>, x, y, dir| {
        if grid[[x, y]] == Cell::Empty {
            grid[[x, y]] = dir;
        } else {
            grid[[x, y]] = Cell::Conflict;
        }
    };

    use Direction::*;
    let mut considerations = VecDeque::from([Up, Down, Left, Right]);

    for rounds in 1..=max_rounds {
        let mut did_move = false;

        for x in 0..width {
            for y in 0..height {
                if grid[[x, y]] != Cell::Elf {
                    continue;
                }

                if (x-1..=x+1).flat_map(|x| {
                    let grid = &grid;
                    (y-1..=y+1).filter(move |&y| grid[[x, y]] == Cell::Elf)
                }).count() <= 1 {
                    continue;
                }

                for dir in &considerations {
                    match dir {
                        Up => {
                            if grid[[x-1, y-1]] != Cell::Elf && grid[[x, y-1]] != Cell::Elf && grid[[x+1, y-1]] != Cell::Elf {
                                propose(grid, x, y-1, Cell::ReservedDown);
                                break;
                            }
                        }
                        Down => {
                            if grid[[x-1, y+1]] != Cell::Elf && grid[[x, y+1]] != Cell::Elf && grid[[x+1, y+1]] != Cell::Elf {
                                propose(grid, x, y+1, Cell::ReservedUp);
                                break;
                            }
                        }
                        Left => {
                            if grid[[x-1, y-1]] != Cell::Elf && grid[[x-1, y]] != Cell::Elf && grid[[x-1, y+1]] != Cell::Elf {
                                propose(grid, x-1, y, Cell::ReservedRight);
                                break;
                            }
                        }
                        Right => {
                            if grid[[x+1, y-1]] != Cell::Elf && grid[[x+1, y]] != Cell::Elf && grid[[x+1, y+1]] != Cell::Elf {
                                propose(grid, x+1, y, Cell::ReservedLeft);
                                break;
                            }
                        }
                    }
                }
            }
        }

        considerations.rotate_left(1);

        for x in 0..width {
            for y in 0..height {
                match grid[[x, y]] {
                    Cell::Conflict => grid[[x, y]] = Cell::Empty,
                    Cell::ReservedUp => {
                        grid[[x, y]] = Cell::Elf;
                        grid[[x, y-1]] = Cell::Empty;
                        did_move = true;
                    }
                    Cell::ReservedDown => {
                        grid[[x, y]] = Cell::Elf;
                        grid[[x, y+1]] = Cell::Empty;
                        did_move = true;
                    }
                    Cell::ReservedLeft => {
                        grid[[x, y]] = Cell::Elf;
                        grid[[x-1, y]] = Cell::Empty;
                        did_move = true;
                    }
                    Cell::ReservedRight => {
                        grid[[x, y]] = Cell::Elf;
                        grid[[x+1, y]] = Cell::Empty;
                        did_move = true;
                    }
                    Cell::Elf | Cell::Empty => {}
                }
            }
        }

        if !did_move {
            return rounds;
        }
    }

    max_rounds
}

fn testcase(fname: impl AsRef<Path>) {
    let fname = fname.as_ref();
    println!("{fname:?}");
    let data = fs::read_to_string(fname).unwrap();
    let lines: Vec<&str> = data.lines().collect();

    let margin = 100;
    let width = lines[0].len() + 2 * margin;
    let height = lines.len() + 2 * margin;
    let mut grid = Array2::from_shape_fn((width, height), |(x, y)| {
        match y.checked_sub(margin)
            .and_then(|y| lines.get(y))
            .and_then(|line| x.checked_sub(margin)
                                .and_then(|x| line.as_bytes().get(x))) {
            Some(&b'#') => Cell::Elf,
            _ => Cell::Empty,
        }
    });

    {
        let mut grid = grid.clone();
        simulate(&mut grid, 10);
        let xmin = grid.indexed_iter().filter(|&(_, &cell)| cell == Cell::Elf)
            .map(|((x, _), _)| x).min().unwrap();
        let xmax = grid.indexed_iter().filter(|&(_, &cell)| cell == Cell::Elf)
            .map(|((x, _), _)| x).max().unwrap();
        let ymin = grid.indexed_iter().filter(|&(_, &cell)| cell == Cell::Elf)
            .map(|((_, y), _)| y).min().unwrap();
        let ymax = grid.indexed_iter().filter(|&(_, &cell)| cell == Cell::Elf)
            .map(|((_, y), _)| y).max().unwrap();

        let elves = grid.iter().filter(|&&cell| cell == Cell::Elf).count();

        let bbox_width = xmax - xmin + 1;
        let bbox_height = ymax - ymin + 1;
        let bbox = bbox_width * bbox_height;
        let answer = bbox - elves;
        println!("Part 1: {answer}");
    }

    let rounds = simulate(&mut grid, usize::MAX);
    println!("Part 2: {rounds}");
}

fn main() {
    testcase("small.in");
    testcase("example.in");
    testcase("puzzle.in");
}
