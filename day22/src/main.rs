use std::fs;
use std::path::Path;
use ndarray::Array2;
use enum_map::{Enum, EnumMap, enum_map};

type Pos = (usize, usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Command {
    TurnLeft,
    TurnRight,
    Walk(u32),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Enum)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    const ALL: [Direction; 4] = [
        Direction::Up,
        Direction::Down,
        Direction::Left,
        Direction::Right,
    ];

    fn cw(self) -> Direction {
        use Direction::*;

        match self {
            Up => Right,
            Right => Down,
            Down => Left,
            Left => Up,
        }
    }

    fn ccw(self) -> Direction {
        use Direction::*;

        match self {
            Up => Left,
            Left => Down,
            Down => Right,
            Right => Up,
        }
    }

    fn opposite(self) -> Direction {
        use Direction::*;

        match self {
            Up => Down,
            Down => Up,
            Left => Right,
            Right => Left,
        }
    }

    fn as_delta(self, m: i32) -> (i32, i32) {
        use Direction::*;

        match self {
            Up => (0, -m),
            Down => (0, m),
            Left => (-m, 0),
            Right => (m, 0),
        }
    }
}

type FaceId = usize;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Edge {
    face: FaceId,
    dir: Direction,
}

#[derive(Debug)]
struct Face {
    input_coords: (i32, i32),
    edges: EnumMap<Direction, Option<Edge>>,
    connections_handled: EnumMap<Direction, bool>,
}

#[derive(Debug)]
struct Mesh {
    faces: Vec<Face>,
    face_size: i32,
}

impl Mesh {
    fn new(face_size: i32) -> Mesh {
        Mesh {
            faces: vec![],
            face_size,
        }
    }

    fn id_for(&self, input_coords: (i32, i32)) -> Option<FaceId> {
        self.faces
            .iter()
            .position(|face| face.input_coords == input_coords)
    }

    fn add_face(&mut self, input_coords: (i32, i32)) {
        let mut face = Face {
            input_coords,
            edges: enum_map! {
                _ => None,
            },
            connections_handled: enum_map! {
                _ => false,
            },
        };

        let face_id = self.faces.len();

        for (dir, edge) in face.edges.iter_mut() {
            let (x, y) = input_coords;
            let (dx, dy) = dir.as_delta(self.face_size);
            if let Some(f) = self.id_for((x + dx, y + dy)) {
                *edge = Some(Edge {
                    face: f,
                    dir: dir.opposite(),
                });

                self.faces[f].edges[dir.opposite()] = Some(Edge {
                    face: face_id,
                    dir,
                });
            }
        }

        self.faces.push(face);
    }

    fn resolve_at(&mut self, face: FaceId, dir: Direction) -> bool {
        let Some(edge1) = self.faces[face].edges[dir]
            else { return false };
        let Some(edge2) = self.faces[face].edges[dir.cw()]
            else { return false };

        let dir1 = edge1.dir.ccw();
        let dir2 = edge2.dir.cw();

        let set = |slot: &mut Option<Edge>, edge: Edge| {
            match *slot {
                None => *slot = Some(edge),
                Some(e) => assert_eq!(e, edge),
            }
        };

        set(&mut self.faces[edge1.face].edges[dir1], Edge {
            face: edge2.face,
            dir: dir2,
        });

        set(&mut self.faces[edge2.face].edges[dir2], Edge {
            face: edge1.face,
            dir: dir1,
        });

        true
    }

    fn resolve(&mut self) {
        loop {
            let mut all_resolved = true;
            let mut made_progress = false;

            for face in 0..self.faces.len() {
                for dir in Direction::ALL {
                    if !self.faces[face].connections_handled[dir] {
                        if self.resolve_at(face, dir) {
                            made_progress = true;
                            self.faces[face].connections_handled[dir] = true;
                        } else {
                            all_resolved = false;
                        }
                    }
                }
            }

            if all_resolved {
                break;
            } else if !made_progress {
                dbg!(&self);
                panic!("Resolving failed");
            }
        }
    }

    fn neighbour(&self, (x, y): Pos, dir: Direction) -> (Pos, Direction) {
        use Direction::*;

        let x = x as i32;
        let y = y as i32;

        let split = |a| {
            let smol = a % self.face_size;
            (a - smol, smol)
        };

        let (x0, x) = split(x);
        let (y0, y) = split(y);

        let (dx, dy) = dir.as_delta(1);
        let x = x + dx;
        let y = y + dy;

        if (0..self.face_size).contains(&x) && (0..self.face_size).contains(&y) {
            return (((x + x0) as usize, (y + y0) as usize), dir);
        }

        let face = self.id_for((x0, y0)).unwrap();

        let end = self.face_size - 1;

        let (edge, t) = if y < 0 {
            (self.faces[face].edges[Up], x)
        } else if y >= self.face_size {
            (self.faces[face].edges[Down], end - x)
        } else if x < 0 {
            (self.faces[face].edges[Left], end - y)
        } else if x >= self.face_size {
            (self.faces[face].edges[Right], y)
        } else {
            unreachable!()
        };

        let edge = edge.unwrap();

        let (x0, y0) = self.faces[edge.face].input_coords;
        let (x, y) = match edge.dir {
            Up => (end - t, 0),
            Down => (t, end),
            Left => (0, t),
            Right => (end, end - t),
        };

        (((x + x0) as usize, (y + y0) as usize), edge.dir.opposite())
    }
}

fn walk(
    cmds: &[Command],
    grid: &Array2<u8>,
    neighbour: impl Fn(Pos, Direction) -> (Pos, Direction),
) -> usize {
    let mut y = 0;
    let mut x = (0..).find(|&x| grid[[x, y]] != b' ').unwrap();
    let mut dir = Direction::Right;

    for &cmd in cmds {
        match cmd {
            Command::TurnLeft => dir = dir.ccw(),
            Command::TurnRight => dir = dir.cw(),
            Command::Walk(n) => {
                for _ in 0..n {
                    let (next, next_dir) = neighbour((x, y), dir);
                    if grid[next] == b'#' {
                        break;
                    }
                    (x, y) = next;
                    dir = next_dir;
                }
            }
        }
    }

    use Direction::*;
    let facing = match dir {
        Right => 0,
        Down => 1,
        Left => 2,
        Up => 3,
    };

    let x = x + 1;
    let y = y + 1;
    1000 * y + 4 * x + facing
}

fn testcase(fname: impl AsRef<Path>, face_size: usize) {
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

    let w = width as i32;
    let h = height as i32;

    let answer = walk(&cmds, &grid, |(mut x, mut y), dir| {
        loop {
            let (dx, dy) = dir.as_delta(1);
            x = (x as i32 + dx).rem_euclid(w) as usize;
            y = (y as i32 + dy).rem_euclid(h) as usize;
            if grid[[x, y]] != b' ' {
                return ((x, y), dir);
            }
        }
    });

    println!("Part 1: {answer}");

    let mut mesh = Mesh::new(face_size as i32);

    for x0 in (0..width as usize).step_by(face_size) {
        for y0 in (0..height as usize).step_by(face_size) {
            if grid[[x0, y0]] != b' ' {
                mesh.add_face((x0 as i32, y0 as i32));
            }
        }
    }

    mesh.resolve();

    let answer = walk(&cmds, &grid, |pos, dir| mesh.neighbour(pos, dir));

    println!("Part 2: {answer}");
}

fn main() {
    testcase("example.in", 4);
    testcase("puzzle.in", 50);
}
