pub const STARTING_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

use std::{
    fmt::Debug,
    ops::{AddAssign, Not},
};

use Class::*;
use Color::*;

use crate::{
    fen::{ParseFenError, fen_to_chess},
    notation::Notation,
    vec2::*,
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Color {
    White,
    Black,
}
impl Not for Color {
    type Output = Self;
    fn not(self) -> Self::Output {
        match self {
            White => Black,
            Black => White,
        }
    }
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
        Self::from_byte(value.as_bytes()[0]).unwrap()
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
            board[0][i] = Some(Piece(White, Self::BACK_ORDER[i]));
            board[7][i] = Some(Piece(Black, Self::BACK_ORDER[i]));
            i += 1;
        }
        board[1] = [Some(Piece(White, Pawn)); 8];
        board[6] = [Some(Piece(Black, Pawn)); 8];
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

    pub fn from_fen(fen: &str) -> Result<Self, ParseFenError> {
        fen_to_chess(fen)
    }

    pub fn moves(&self, class: Class, p: Vec2) -> Vec<Vec2> {
        match class {
            Pawn => self.pawn_moves(p),
            Bishop => self.bishop_moves(p),
            Knight => self.knight_moves(p),
            Rook => self.rook_moves(p),
            Queen => self.queen_moves(p),
            King => self.king_moves(p),
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

        let mut r = vec![f1 + p];
        if matches!(p.y, 1 | 6) {
            r.push(f1 * 2 + p);
        }
        //r.iter_mut().for_each(|x| *x += p);
        if self.empty_or_takeable(r[0]).not() {
            r.clear();
        } else if r.len() == 2 && self.empty_or_takeable(r[1]).not() {
            r.remove(1);
        }
        if let Some(ep) = self.en_passant
            && (ep - (p + f1)).abs() == xy(1, 0)
        {
            r.push(ep);
        }
        r
    }

    pub fn king_moves(&self, p: Vec2) -> Vec<Vec2> {
        xy(0, 1)
            .cardinals()
            .take(4)
            .chain(xy(1, 1).cardinals().take(4))
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
            .inspect(|x| println!("bishop {x:?}"))
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

    pub fn get_mut(&mut self, p: Vec2) -> &mut Option<Piece> {
        &mut self.board[p.y as usize][p.x as usize]
    }

    pub fn empty_or_takeable(&self, p: Vec2) -> bool {
        self.get(p).is_none_or(|v| v.0 != self.turn)
    }

    pub fn to_fen(&self) -> String {
        crate::fen::chess_to_fen(self)
    }

    pub fn make_move(&mut self, from: Vec2, to: Vec2) -> Result<ChState, ChError> {
        let Some(pf) = self.get(from) else {
            return Err(ChError::TriedToMoveEmptySquare);
        };

        let movables = self.moves(pf.1, from);

        if movables.contains(&to) {
            let ep = self.en_passant.take();
            if pf.1 == Pawn {
                if from.y.abs_diff(to.y) == 2 {
                    self.en_passant = Some(movables[0]);
                }
                if ep.is_some_and(|ep| ep == to) {
                    self.get_mut(xy(to.x, from.y)).take();
                }
            }
            let old_to = self.get_mut(to).take();
            *self.get_mut(to) = self.get_mut(from).take();
            if self.in_check() {
                *self.get_mut(from) = self.get_mut(to).take();
                *self.get_mut(to) = old_to;
                return Err(ChError::MoveIntoCheck);
            }
            self.turn = !self.turn;
        }

        Ok(if self.in_check() {
            ChState::Check
        } else {
            ChState::Normal
        })
    }

    pub fn move_by_note(&mut self, note: Notation) -> Result<ChState, ChError> {
        Ok(match note {
            Notation::Standard {
                rank_from,
                file_from,
                piece_class,
                cap,
                rank_to,
                file_to,
                checkmate,
                check,
                promote,
            } => {
                let pc = piece_class.unwrap_or(Pawn);
                let cap = cap.is_some();
                let check = check.is_some();
                let checkmate = checkmate.is_some();
                let vec_to = Vec2 {
                    x: rank_to.0 as i32,
                    y: file_to.0 as i32,
                };

                if cap && self.get(vec_to).is_none() {
                    return Err(ChError::FailedToCaptureEmptySquare);
                }
                if self.get(vec_to).is_some_and(|p| p.0 == self.turn) {
                    return Err(ChError::DestinationOccupiedByFriendlyPiece);
                }

                let possies = (0..8)
                    .flat_map(|x| (0..8).map(move |y| Vec2 { x, y }))
                    .collect::<Vec<_>>();

                let pv = possies
                    .into_iter()
                    .filter(|v| rank_from.is_none_or(|x| x.0 as i32 == v.x))
                    .filter(|v| file_from.is_none_or(|y| y.0 as i32 == v.y))
                    .filter(|v| self.get(*v).is_some_and(|x| x.0 == self.turn && x.1 == pc))
                    .filter(|v| self.moves(pc, *v).contains(&vec_to))
                    .collect::<Vec<_>>();
                match *pv.as_slice() {
                    [v] => self.make_move(v, vec_to)?,
                    [] => return Err(ChError::NoPiecesMatchMove),
                    _ => return Err(ChError::TooManyPiecesMatchMove),
                }
            }
            _ => todo!(),
        })
    }
}

#[derive(Debug)]
pub enum ChError {
    NoPiecesMatchMove,
    TooManyPiecesMatchMove,
    TriedToMoveEmptySquare,
    FailedToCaptureEmptySquare,
    DestinationOccupiedByFriendlyPiece,
    MoveIntoCheck,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ChState {
    Check,
    CheckMate,
    StaleMate,
    Normal,
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
        March { by: self, at: self }
    }
}

#[test]
fn tyler2e() {
    let mut map = std::collections::HashMap::new();
    map.insert(7, 7);
}
