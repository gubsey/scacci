pub const STARTING_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

use std::{
    iter::{Successors, successors},
    ops::{Add, AddAssign, Deref, Mul, MulAssign, Not, Sub, SubAssign},
    str::FromStr,
};

use Color::*;
use Rank::*;

use crate::{
    hlist::{Either, One, one},
    parse::*,
};

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
}

impl Parser for Chess {
    type Extract = One<Self>;
    fn parse(self, a: &[u8]) -> Option<(Self::Extract, &[u8])> {
        let ws = Unit(b' ').ignore();

        let ((board, turn), xs) = PiecePositions.and(ws).and(TurnParser).and(ws).parse(a)?;
        let ((castling, en_passant), xs) = CastlePossibilitiesParser
            .and(ws)
            .and(SpaceParser.or(Unit(b'-').ignore()))
            .and(ws)
            .parse(xs)?;
        let en_passant = en_passant.a().map(|x| x.0);
        let ((halfmoves, fullmoves), xs) = Digit.many().and(ws).and(Digit.many()).parse(xs)?;
        let mut mvs = [0; 2];
        mvs.iter_mut()
            .zip(
                [halfmoves, fullmoves]
                    .into_iter()
                    .map(String::from_utf8)
                    .map(Result::unwrap)
                    .map(|s| s.parse::<usize>().unwrap()),
            )
            .for_each(|(a, b)| *a = b);
        let [halfmoves, fullmoves] = mvs;

        Some((
            one(Self {
                board,
                turn,
                castling,
                en_passant,
                halfmoves,
                fullmoves,
            }),
            xs,
        ))
    }
}

#[test]
fn chess_parse() {
    let parsed = Chess::DEFAULT_START
        .parse(STARTING_FEN.as_bytes())
        .unwrap()
        .0
        .0;
    assert_eq!(parsed, Chess::DEFAULT_START)
}

struct SpaceParser;
impl Parser for SpaceParser {
    type Extract = One<Vec2>;
    fn parse(self, a: &[u8]) -> Option<(Self::Extract, &[u8])> {
        let (x, xs) = (b'a'..=b'h')
            .map(Unit)
            .and((b'1'..=b'8').map(Unit))
            .parse(a)?;
        Some(((xy(x.0 as i32, x.1 as i32),), xs))
    }
}
struct TurnParser;
impl Parser for TurnParser {
    type Extract = One<Color>;
    fn parse(self, a: &[u8]) -> Option<(Self::Extract, &[u8])> {
        Unit(b'w')
            .or(Unit(b'b'))
            .pmap(|x| match x {
                Either::A(_) => White,
                _ => Black,
            })
            .parse(a)
    }
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

#[derive(Default, Debug, Clone, Copy)]
struct CastlePossibilitiesParser;
impl Parser for CastlePossibilitiesParser {
    type Extract = One<CastlePossibilities>;
    fn parse(self, a: &[u8]) -> Option<(Self::Extract, &[u8])> {
        let mut cp = CastlePossibilities::default();
        for b in &a[..4] {
            match b {
                b'K' => cp.wk = true,
                b'Q' => cp.wq = true,
                b'k' => cp.bk = true,
                b'q' => cp.bq = true,
                _ => return None,
            }
        }
        Some((one(cp), &a[4..]))
    }
}

struct PiecePositions;
impl Parser for PiecePositions {
    type Extract = One<[[Option<Piece>; 8]; 8]>;
    fn parse(self, a: &[u8]) -> Option<(Self::Extract, &[u8])> {
        let vect = b"prnbqkPRNBQK12345678"
            .iter()
            .copied()
            .map(Unit)
            .map(|x| {
                x.pmap(|u| match u {
                    n @ b'1'..=b'8' => Err((n - b'0') as usize),
                    b'p' => Ok(Piece(Black, Pawn)),
                    b'r' => Ok(Piece(Black, Rook)),
                    b'n' => Ok(Piece(Black, Knight)),
                    b'b' => Ok(Piece(Black, Bishop)),
                    b'q' => Ok(Piece(Black, Queen)),
                    b'k' => Ok(Piece(Black, King)),
                    b'P' => Ok(Piece(White, Pawn)),
                    b'R' => Ok(Piece(White, Rook)),
                    b'N' => Ok(Piece(White, Knight)),
                    b'B' => Ok(Piece(White, Bishop)),
                    b'Q' => Ok(Piece(White, Queen)),
                    b'K' => Ok(Piece(White, King)),
                    _ => unreachable!("{} is invalid", u as char),
                })
            })
            .collect::<Vec<_>>();
        let ((mut block, end), xs) = vect
            .iter()
            .copied()
            .many()
            .and(Unit(b'/').ignore())
            .many_n(8)
            .and(vect.iter().copied().many())
            .parse(a)?;
        block.push(end);

        let mut r = [[None; 8]; 8];

        for (y, row) in block.into_iter().enumerate() {
            let mut offset = 0;
            for (x, c) in row.into_iter().enumerate() {
                match c {
                    Ok(p) => r[y][x + offset] = Some(p),
                    Err(n) => offset += n - 1,
                }
            }
        }

        Some((one(r), xs))
    }
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
