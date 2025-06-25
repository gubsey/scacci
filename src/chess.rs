pub const STARTING_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

use std::{
    iter::{Successors, successors},
    ops::{Add, AddAssign, Deref, Mul, MulAssign, Not, Sub, SubAssign},
    str::FromStr,
};

use Color::*;
use Rank::*;
use chumsky::{container::Seq, text::Char};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Color {
    White,
    Black,
}
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Rank {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct Piece(pub Color, pub Rank);

#[derive(Debug, Copy, Clone, PartialEq)]
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
    const BACK_ORDER: [Rank; 8] = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook];
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

    pub fn parse_fen(fen: &str) -> chumsky::ParseResult<Chess, chumsky::error::Simple<'_, char>> {
        use chumsky::prelude::*;

        let p = one_of::<_, _, extra::Err<Simple<char>>>("prnbqkPRNBQK")
            .map(|c: char| {
                let rank = match c.to_ascii_lowercase() {
                    'p' => Pawn,
                    'r' => Rook,
                    'n' => Knight,
                    'b' => Bishop,
                    'q' => Queen,
                    'k' => King,
                    _ => unreachable!(),
                };
                let color = if c.is_lowercase() { Black } else { White };
                Piece(color, rank)
            })
            .boxed();

        let bp = p
            .map(Ok)
            .or(one_of('1'..='8').map(|c: char| Err(c.to_digit(9).unwrap() as usize)))
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .map(|x| {
                x.into_iter()
                    .flat_map(|r| match r {
                        Ok(p) => vec![Some(p)],
                        Err(i) => vec![None; i],
                    })
                    .collect::<Vec<_>>()
            })
            .filter(|x| x.len() == 8)
            .map(stack_collect::<8, _>)
            .separated_by(just('/'))
            .exactly(8)
            .collect_exactly::<[_; 8]>()
            .boxed();

        let turnp = just('w')
            .or(just('b'))
            .map(|c| match c {
                'w' => White,
                'b' => Black,
                _ => unreachable!(),
            })
            .boxed();

        let castlep = one_of("kqKQ")
            .repeated()
            .at_least(1)
            .at_most(4)
            .collect::<Vec<_>>()
            .map(|v| {
                let mut cp = CastlePossibilities::default();
                for x in v {
                    match x {
                        'k' => cp.bk = true,
                        'q' => cp.bq = true,
                        'K' => cp.wk = true,
                        'Q' => cp.wq = true,
                        _ => unreachable!(),
                    }
                }
                cp
            })
            .or(just('-').map(|_| Default::default()))
            .boxed();

        let en_passantp = one_of('a'..='h')
            .then(one_of('1'..='8'))
            .map(|(x, y): (char, char)| xy((b'h' - x as u8) as i32, (b'8' - y as u8) as i32))
            .map(Some)
            .or(just('-').map(|_| None))
            .boxed();

        let number = one_of('0'..='9')
            .repeated()
            .at_least(1)
            .collect::<String>()
            .map(|s| s.parse().unwrap())
            .boxed();

        bp.then_ignore(just(' '))
            .then(turnp)
            .then_ignore(just(' '))
            .then(castlep)
            .then_ignore(just(' '))
            .boxed()
            .then(en_passantp)
            .then_ignore(just(' '))
            .boxed()
            .then(number.clone())
            .then_ignore(just(' '))
            .then(number)
            .boxed()
            .map(
                |(((((board, turn), castling), en_passant), halfmoves), fullmoves)| Chess {
                    board,
                    turn,
                    castling,
                    en_passant,
                    halfmoves,
                    fullmoves,
                },
            )
            .parse(fen)
    }
}

#[test]
fn parsing() {
    let parsed = Chess::parse_fen(STARTING_FEN).unwrap();

    assert_eq!(parsed, Chess::DEFAULT_START);
}

fn stack_collect<const N: usize, T: Copy>(a: Vec<T>) -> [T; N] {
    let mut arr: [T; N] = unsafe { std::mem::zeroed() };
    for (i, x) in a.into_iter().enumerate() {
        arr[i] = x;
    }
    arr
}

impl Default for Chess {
    fn default() -> Self {
        Self::DEFAULT_START
    }
}

#[derive(Default, Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Vec2<T = i32> {
    x: T,
    y: T,
}

impl<T> Vec2<T> {
    pub fn transposed(mut self) -> Self {
        std::mem::swap(&mut self.x, &mut self.y);
        self
    }
}

impl Vec2 {
    pub fn to_usize(self) -> Vec2<usize> {
        Vec2::<usize> {
            x: self.x as usize,
            y: self.y as usize,
        }
    }

    pub fn flipedy(mut self) -> Self {
        self.y *= -1;
        self
    }
    pub fn rotate90(&mut self) {
        *self = self.rotated90()
    }
    pub fn rotated90(self) -> Self {
        self.transposed().flipedy()
    }
    pub fn cardinals(self) -> Successors<Vec2, impl FnMut(&Vec2) -> Option<Vec2>> {
        successors(Some(self), |x| Some(x.rotated90()))
    }
    pub fn in_bounds(&self) -> bool {
        matches!(self.x, 0..8) && matches!(self.y, 0..8)
    }
    pub fn abs(mut self) -> Self {
        self.x = self.x.abs();
        self.y = self.y.abs();
        self
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct CastlePossibilities {
    pub wq: bool,
    pub wk: bool,
    pub bq: bool,
    pub bk: bool,
}

impl<T: AddAssign> Add for Vec2<T> {
    type Output = Self;
    fn add(self, mut rhs: Self) -> Self::Output {
        rhs.x += self.x;
        rhs.y += self.y;
        rhs
    }
}
impl<T: AddAssign + Copy> Add<T> for Vec2<T> {
    type Output = Self;
    fn add(mut self, rhs: T) -> Self::Output {
        self.x += rhs;
        self.y += rhs;
        self
    }
}
impl<T: AddAssign> AddAssign for Vec2<T> {
    fn add_assign(&mut self, rhs: Self) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}
impl<T: SubAssign> Sub for Vec2<T> {
    type Output = Self;
    fn sub(mut self, rhs: Self) -> Self::Output {
        self.x -= rhs.x;
        self.y -= rhs.y;
        self
    }
}
impl<T: SubAssign> SubAssign for Vec2<T> {
    fn sub_assign(&mut self, rhs: Self) {
        self.x -= rhs.x;
        self.y -= rhs.y;
    }
}
impl<T: MulAssign> Mul for Vec2<T> {
    type Output = Self;
    fn mul(mut self, rhs: Self) -> Self::Output {
        self.x *= rhs.x;
        self.y *= rhs.y;
        self
    }
}
impl<T: MulAssign + Copy> Mul<T> for Vec2<T> {
    type Output = Self;
    fn mul(mut self, rhs: T) -> Self::Output {
        self.x *= rhs;
        self.y *= rhs;
        self
    }
}
pub fn xy<T>(x: T, y: T) -> Vec2<T> {
    Vec2 { x, y }
}

impl FromStr for Vec2 {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let chars: Vec<char> = s.chars().collect();
        if chars.len() != 2 {
            return Err("string not exactly 2 chars");
        }
        let x = match chars[0] {
            c @ 'a'..='h' => c as i32 - 'a' as i32,
            _ => return Err("first char out of bounds"),
        };
        let y = match chars[1].to_digit(10).ok_or("second char not a number")? {
            y @ 1..=8 => y as i32 - 1,
            _ => return Err("second char not in range"),
        };
        Ok(Self { x, y })
    }
}

pub struct March<T> {
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

pub trait ToMarch {
    type Item;
    fn march(self) -> March<Self::Item>;
}

impl<T: Copy + AddAssign> ToMarch for T {
    type Item = T;
    fn march(self) -> March<Self::Item> {
        March::new(self)
    }
}
