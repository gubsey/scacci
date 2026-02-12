use std::{convert::Infallible, str::FromStr};

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

#[derive(Debug)]
pub enum ParseFenError {
    MissingBoard,
    MissingTurn,
    MissingCastle,
    MissingEnPasssant,
    MissingHalfmoves,
    MissingFullmoves,
    InvalidClass(char),
    InvalidBoard(String),
    InvalidTurn(String),
    InvalidEnPassant(String),
    InvalidHalfmoves,
    InvalidFullmoves,
}

pub fn fen_to_chess(fen: &str) -> Result<Chess, ParseFenError> {
    use ParseFenError::*;
    let mut split = fen.split(' ');

    let board_str = split.next().ok_or(MissingBoard)?;
    let mut board = [[None; 8]; 8];
    let mut x = 0;
    let mut y = 0;
    for &b in board_str.as_bytes() {
        if b == b'/' {
            x = 0;
            y += 1;
            continue;
        }
        if b.is_ascii_digit() {
            x += (b - b'0' - 1) as usize;
            if x > 7 {
                return Err(InvalidBoard(format!("board row {y} is too long")));
            }
            continue;
        }
        let color = if b.is_ascii_uppercase() { White } else { Black };
        let class = Class::from_byte(b).ok_or(InvalidClass(b as char))?;
        board[y][x] = Some(Piece(color, class));
        x += 1;
    }

    let turn = match split.next().ok_or(MissingTurn)? {
        "w" => White,
        "b" => Black,
        o => return Err(InvalidTurn(o.to_string())),
    };

    let castle_str = split.next().ok_or(MissingCastle)?;
    let castling = CastlePossibilities {
        wq: castle_str.contains('Q'),
        wk: castle_str.contains('W'),
        bq: castle_str.contains('q'),
        bk: castle_str.contains('k'),
    };

    let en_passant = match split.next().ok_or(MissingEnPasssant)? {
        "-" => None,
        other => Some(Vec2::from_str(other).map_err(|_| InvalidEnPassant(other.to_string()))?),
    };

    let halfmoves = split
        .next()
        .ok_or(MissingHalfmoves)?
        .parse()
        .map_err(|_| InvalidHalfmoves)?;
    let fullmoves = split
        .next()
        .ok_or(MissingFullmoves)?
        .parse()
        .map_err(|_| InvalidFullmoves)?;
    Ok(Chess {
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
