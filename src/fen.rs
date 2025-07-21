use chain_tools::Pipe;

use crate::{
    CastlePossibilities, Piece,
    aparsetheid::Parser,
    chess::{Chess, Color::*, Class::*},
    vec2::*,
};

#[inline]
pub fn chess_to_fen(chess: &Chess) -> String {
    let piece_map = chess
        .board
        .iter()
        .enumerate()
        .map(|(i, a)| {
            let mut gap = 0;
            let mut s = Vec::<u8>::new();
            for o in a {
                match o {
                    None => gap += 1,
                    Some(p) => {
                        if gap != 0 {
                            s.push(gap + b'0');
                            gap = 0;
                        }
                        s.push(p.to_fen() as u8);
                    }
                }
            }
            if gap != 0 {
                s.push(gap + b'0');
            }
            if i < 7 {
                s.push(b'/');
            }
            String::from_utf8(s).unwrap()
        })
        .collect::<String>();
    let turn = match chess.turn {
        White => 'w',
        Black => 'b',
    };
    let castling = [
        chess.castling.wk,
        chess.castling.wq,
        chess.castling.bk,
        chess.castling.bq,
    ]
    .into_iter()
    .zip(b"KQkq")
    .filter_map(|(b, c)| b.then_some(*c as char))
    .collect::<String>()
    .pipe(|s| if s.is_empty() { "-".to_string() } else { s });
    let en_pass = chess
        .en_passant
        .map(|v| v.to_fen())
        .unwrap_or("-".to_string());

    format!(
        "{piece_map} {turn} {castling} {en_pass} {} {}",
        chess.halfmoves, chess.fullmoves
    )
}

#[derive(Debug, Clone, Copy)]
pub struct Fen;
impl Parser<u8, Chess> for Fen {
    fn parse<S: crate::aparsetheid::Stream<u8>>(self, s: S) -> Option<(Chess, S)> {
        todo!()
    }
}

fn stack_collect<const N: usize, T: Copy>(a: Vec<T>) -> [T; N] {
    let mut arr: [T; N] = unsafe { std::mem::zeroed() };
    for (i, x) in a.into_iter().enumerate() {
        arr[i] = x;
    }
    arr
}
