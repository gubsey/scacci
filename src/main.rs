use std::{
    io::{Write, stdin, stdout},
    iter::repeat,
    str::FromStr,
};

use scacci::{Chess, Color, notation::Notation};

fn main() {
    let mut lines = stdin().lines();
    let mut chess = Chess::default();
    print_chess(&chess);
    while let Some(Ok(line)) = lines.next() {
        let note = Notation::from_str(&line).unwrap();
        if let Err(e) = chess.move_by_note(note) {
            println!("Error: {e:?}");
        };
        print_chess(&chess);
    }
}

fn print_chess(chess: &Chess) {
    println!("{}", chess.to_fen());
    let iter = chess.board.iter().enumerate();

    let vecty: Vec<_> = if chess.turn == Color::White {
        iter.rev().collect()
    } else {
        iter.collect()
    };

    for (i, row) in vecty {
        print!("{} | ", i + 1);
        for p in row {
            if let Some(p) = p {
                print!("{} ", p.to_fen());
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