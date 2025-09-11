use std::str::FromStr;

use chain_tools::Pipe;

use crate::{
    CastlePossibilities, Class, Piece,
    chess::{Chess, Class::*, Color::*},
    vec2::*,
};

#[inline]
pub fn chess_to_fen(chess: &Chess) -> String {
    let piece_map = chess
        .board
        .iter()
        .rev()
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

pub fn fen_to_chess(fen: &str) -> Option<Chess> {
    let mut split = fen.split(' ');

    let board_str = split.next()?;
    let mut board = [[None; 8]; 8];
    let mut x = 0;
    let mut y = 0;
    for &b in board_str.as_bytes() {
        if b == b'/' {
            x = 0;
            y += 1;
            continue;
        }
        let color = if b.is_ascii_uppercase() { White } else { Black };
        let class = Class::from_byte(b)?;
        board[y][x] = Some(Piece(color, class));
        x += 1;
    }

    let turn = match split.next()? {
        "w" => White,
        "b" => Black,
        _ => return None,
    };

    let castle_str = split.next()?;
    let castling = CastlePossibilities {
        wq: castle_str.contains('Q'),
        wk: castle_str.contains('W'),
        bq: castle_str.contains('q'),
        bk: castle_str.contains('k'),
    };

    let en_passant = match split.next()? {
        "-" => None,
        other => Some(Vec2::from_str(other).ok()?),
    };

    let halfmoves = split.next()?.parse().ok()?;
    let fullmoves = split.next()?.parse().ok()?;
    Some(Chess {
        board,
        turn,
        castling,
        en_passant,
        halfmoves,
        fullmoves,
    })
}

fn stack_collect<const N: usize, T: Copy>(a: Vec<T>) -> [T; N] {
    let mut arr: [T; N] = unsafe { std::mem::zeroed() };
    for (i, x) in a.into_iter().enumerate() {
        arr[i] = x;
    }
    arr
}
