use std::{
    io::{Write, stdin, stdout},
    process::exit,
    str::FromStr,
};

use clap::Parser;
use scacci::{
    Chess, Color, Piece,
    notation::Notation,
    vec2::{Vec2, xy},
};

#[derive(clap::Parser)]
struct FArgs {
    #[arg(short)]
    fen: Option<String>,
}

fn main() {
    let args = FArgs::parse();
    let mut lines = stdin().lines();
    let mut note_log = Vec::new();
    let mut chess = if let Some(fen) = args.fen {
        Chess::from_fen(&fen).expect("invalid input fen")
    } else {
        Chess::default()
    };
    print_chess(&chess, &[]);
    while let Some(Ok(line)) = lines.next() {
        if let Some(cmd) = line.strip_prefix("/") {
            match cmd.split(' ').collect::<Vec<_>>().as_slice() {
                ["quit"] => {
                    println!("Goodbye");
                    return;
                }
                ["fen"] => println!("{}", chess.to_fen()),
                ["pb", s @ ..] => print_chess(
                    &chess,
                    s.iter()
                        .filter_map(|s| {
                            Vec2::from_str(s)
                                .inspect_err(|x| println!("error: {x}"))
                                .ok()
                        })
                        .collect::<Vec<_>>()
                        .as_slice(),
                ),
                ["moves", p] => {
                    if let Ok(p) = Vec2::from_str(p) {
                        if let Some(class) = chess.get(p).map(|x| x.1) {
                            let moves = chess.moves(class, p);
                            print_chess(&chess, &moves);
                        }
                    }
                }
                ["hist"] => note_log.iter().for_each(|x| println!("{x}")),
                ["restart"] | ["reset"] => chess = Chess::DEFAULT_START,
                ["spawn", piece, pos] => {
                    *chess.get_mut(Vec2::from_str(pos).unwrap()) =
                        Piece::from_fen(piece.as_bytes()[0] as char);
                    print_chess(&chess, &[]);
                }
                ["kill", "all"] => chess.board.iter_mut().flatten().for_each(|x| *x = None),
                ["kill", pos] => {
                    *chess.get_mut(Vec2::from_str(pos).unwrap()) = None;
                    print_chess(&chess, &[]);
                }
                _ => println!("invalid command"),
            }
            continue;
        }
        let note = match Notation::from_str(&line) {
            Ok(note) => note,
            Err(e) => {
                println!("Error {e}");
                continue;
            }
        };
        let mut markings = Vec::new();
        match chess.move_by_note(note) {
            Ok(scacci::ChState::Check(v)) => {
                let k = chess.piece_positions(Piece(chess.turn, scacci::Class::King))[0];

                markings.extend(v.into_iter().flat_map(|x| chess.path_from(x, k)));
                println!("{k:?}");
            }
            Err(e) => {
                println!("Error: {e:?}");
                continue;
            }
            _ => {}
        }
        note_log.push(line);
        print_chess(&chess, &markings);
    }
}

fn print_chess(chess: &Chess, markings: &[Vec2]) {
    for (y, row) in chess.board.iter().enumerate() {
        print!("{} | ", y + 1);

        for (x, p) in row.iter().enumerate() {
            match (p, markings.contains(&xy(x as i32, y as i32))) {
                (Some(p), true) => {
                    print!("\x1b[4m{}\x1b[0m ", p.to_fen())
                }
                (Some(p), false) => print!("{} ", p.to_fen()),
                (None, true) => print!("x "),
                (None, false) => print!("- "),
            }
        }
        println!();
    }

    println!(
        "   -{}\n    {}",
        "-".repeat(15),
        ('a'..='h')
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join(" ")
    );
    stdout().flush().unwrap();
}
