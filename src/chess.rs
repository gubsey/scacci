pub const STARTING_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

use std::{
    fmt::Debug,
    ops::{AddAssign, Not},
};

use Class::*;
use Color::*;

use crate::vec2::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Color {
    White,
    Black,
}
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Class {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

impl From<&str> for Class {
    fn from(value: &str) -> Self {
        match value.as_bytes()[0].to_ascii_lowercase() {
            b'p' => Pawn,
            b'b' => Bishop,
            b'n' => Knight,
            b'r' => Rook,
            b'q' => Queen,
            b'k' => King,
            _ => panic!("{value:?} is an invalid class"),
        }
    }
}

impl Class {
    pub fn from_byte(byte: u8) -> Option<Self> {
        let c = match byte.to_ascii_lowercase() {
            b'p' => Pawn,
            b'b' => Bishop,
            b'n' => Knight,
            b'r' => Rook,
            b'q' => Queen,
            b'k' => King,
            _ => return None,
        };
        Some(c)
    }
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct Piece(pub Color, pub Class);

impl Piece {
    pub fn to_fen(&self) -> char {
        let c = match self.1 {
            Pawn => 'p',
            Rook => 'r',
            Knight => 'n',
            Bishop => 'b',
            Queen => 'q',
            King => 'k',
        };
        if self.0 == White {
            c.to_ascii_uppercase()
        } else {
            c
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
pub struct Chess {
    pub board: [[Option<Piece>; 8]; 8],
    pub turn: Color,
    pub castling: CastlePossibilities,
    pub en_passant: Option<Vec2>,
    pub halfmoves: usize,
    pub fullmoves: usize,
}

impl Chess {
    pub const DEFAULT_START: Self = Self::start();
    const BACK_ORDER: [Class; 8] = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook];
    const fn start() -> Self {
        let mut board = [[None; 8]; 8];
        let mut i = 0;
        while i < 8 {
            board[7][i] = Some(Piece(White, Self::BACK_ORDER[i]));
            board[0][i] = Some(Piece(Black, Self::BACK_ORDER[i]));
            i += 1;
        }
        board[6] = [Some(Piece(White, Pawn)); 8];
        board[1] = [Some(Piece(Black, Pawn)); 8];
        Self {
            board,
            turn: White,
            castling: CastlePossibilities {
                wk: true,
                wq: true,
                bk: true,
                bq: true,
            },
            en_passant: None,
            halfmoves: 0,
            fullmoves: 1,
        }
    }

    fn rook_bishop_func(&self, p: Vec2, a: Vec2) -> Vec<Vec2> {
        a.cardinals()
            .take(4)
            .flat_map(|v| {
                v.march()
                    .map(|x| x + p)
                    .take_while(Vec2::in_bounds)
                    .scan(true, |s, v| {
                        s.then(|| match self.get(v) {
                            Some(x) if x.0 != self.turn => {
                                *s = false;
                                Some(v)
                            }
                            Some(_) => None,
                            None => Some(v),
                        })
                        .flatten()
                    })
            })
            .collect()
    }

    pub fn rook_moves(&self, p: Vec2) -> Vec<Vec2> {
        self.rook_bishop_func(p, xy(0, 1))
    }

    pub fn bishop_moves(&self, p: Vec2) -> Vec<Vec2> {
        self.rook_bishop_func(p, xy(1, 1))
    }

    pub fn queen_moves(&self, p: Vec2) -> Vec<Vec2> {
        let mut v = self.rook_moves(p);
        v.extend_from_slice(&self.bishop_moves(p));
        v
    }

    pub fn knight_moves(&self, p: Vec2) -> Vec<Vec2> {
        [true, false]
            .into_iter()
            .cycle()
            .scan(xy(1, 2), |v, b| {
                let r = *v;
                if b {
                    *v = v.transposed()
                } else {
                    *v = v.flipedy()
                }
                Some(r)
            })
            .take(8)
            .map(|x| x + p)
            .filter(Vec2::in_bounds)
            .filter(|x| self.empty_or_takeable(*x))
            .collect()
    }

    pub fn pawn_moves(&self, p: Vec2) -> Vec<Vec2> {
        let f1 = if self.turn == White {
            xy(0, 1)
        } else {
            xy(0, -1)
        };
        let mut r = vec![f1];
        if matches!((p - r[0]).y, 0 | 7) {
            r.push(r[0] * 2);
        }
        r.iter_mut().for_each(|x| *x += p);
        if self.empty_or_takeable(r[0]).not() {
            r.clear();
        } else if self.empty_or_takeable(r[1]).not() {
            r.remove(1);
        }
        self.en_passant
            .filter(|v| (f1 - *v).abs() == xy(1, 0))
            .inspect(|v| r.push(*v));
        r
    }

    pub fn king_moves(&self, p: Vec2) -> Vec<Vec2> {
        xy(0, 1)
            .cardinals()
            .chain(xy(1, 1).cardinals())
            .map(|x| x + p)
            .filter(|x| self.empty_or_takeable(*x))
            .collect()
    }

    pub fn in_check(&self) -> bool {
        let Some(k) = (0..8)
            .flat_map(|y| (0..8).map(move |x| xy(x, y)))
            .find(|v| {
                self.get(*v)
                    .is_some_and(|p| p.0 == self.turn && p.1 == King)
            })
        else {
            return false;
        };

        self.bishop_moves(k)
            .into_iter()
            .filter_map(|v| self.get(v))
            .filter(|p| p.0 != self.turn)
            .any(|p| matches!(p.1, Bishop | Queen))
            || self
                .rook_moves(k)
                .into_iter()
                .filter_map(|v| self.get(v))
                .filter(|p| p.0 != self.turn)
                .any(|p| matches!(p.1, Rook | Queen))
            || self
                .knight_moves(k)
                .into_iter()
                .filter_map(|v| self.get(v))
                .filter(|p| p.0 != self.turn)
                .any(|p| p.1 == Knight)
            || self
                .pawn_moves(k)
                .into_iter()
                .filter_map(|v| self.get(v))
                .filter(|p| p.0 != self.turn)
                .any(|p| matches!(p.1, Pawn))
            || self
                .king_moves(k)
                .into_iter()
                .filter_map(|v| self.get(v))
                .filter(|p| p.0 != self.turn)
                .any(|p| matches!(p.1, King))
    }

    pub fn get(&self, p: Vec2) -> Option<Piece> {
        self.board
            .get(p.y as usize)
            .and_then(|o| o.get(p.x as usize).copied())
            .flatten()
    }

    pub fn empty_or_takeable(&self, p: Vec2) -> bool {
        self.get(p).is_none_or(|v| v.0 != self.turn)
    }

    pub fn to_fen(&self) -> String {
        crate::fen::chess_to_fen(self)
    }
}

impl Debug for Chess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_fen())
    }
}

#[test]
fn to_fen_test() {
    assert_eq!(Chess::DEFAULT_START.to_fen(), STARTING_FEN);
}

impl Default for Chess {
    fn default() -> Self {
        Self::DEFAULT_START
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct CastlePossibilities {
    pub wq: bool,
    pub wk: bool,
    pub bq: bool,
    pub bk: bool,
}

struct March<T> {
    by: T,
    at: T,
}

impl<T: Copy + AddAssign> March<T> {
    pub fn new(t: T) -> Self {
        March { by: t, at: t }
    }
}

impl<T: Copy + AddAssign> Iterator for March<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        let r = self.at;
        self.at += self.by;
        Some(r)
    }
}

trait ToMarch {
    type Item;
    fn march(self) -> March<Self::Item>;
}

impl<T: Copy + AddAssign> ToMarch for T {
    type Item = T;
    fn march(self) -> March<Self::Item> {
        March::new(self)
    }
}
