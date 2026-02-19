use std::{
    io::{Write, stdin, stdout},
    iter::repeat,
    str::FromStr,
};

use clap::Parser;
use scacci::{Chess, Color, Notation, Piece, Vec2, xy};

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
    print_chess(&chess, vec![]);
    while let Some(Ok(line)) = lines.next() {
        if let Some(cmd) = line.strip_prefix("/") {
            match cmd.split(' ').collect::<Vec<_>>().as_slice() {
                ["fen"] => println!("{}", chess.to_fen()),
                ["hist"] => note_log.iter().for_each(|x| println!("{x}")),
                ["spawn", piece, pos] => {
                    *chess.get_mut(Vec2::from_str(pos).unwrap()) =
                        Piece::from_fen(piece.as_bytes()[0] as char);
                    print_chess(&chess, vec![]);
                }
                ["kill", pos] => {
                    *chess.get_mut(Vec2::from_str(pos).unwrap()) = None;
                    print_chess(&chess, vec![]);
                }
                _ => println!("invalid command"),
            }
            continue;
        }
        let note = Notation::from_str(&line).unwrap();
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
        print_chess(&chess, markings);
    }
}

fn print_chess(chess: &Chess, markings: Vec<Vec2>) {
    println!("{}", chess.to_fen());
    let iter = chess.board.iter().enumerate();

    let vecty: Vec<_> = if chess.turn == Color::White {
        iter.rev().collect()
    } else {
        iter.collect()
    };

    for (y, row) in chess.board.iter().enumerate() {
        print!("{} | ", y + 1);

        for (x, p) in row.iter().enumerate() {
            if let Some(p) = p {
                print!("{} ", p.to_fen());
            } else if markings.contains(&xy(x as i32, y as i32)) {
                print!("x ")
            } else {
                print!("- ");
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
