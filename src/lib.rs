trait Pipe {
    fn pipe<O>(self, f: fn(Self) -> O) -> O;
}

impl<T> Pipe for T {
    fn pipe<O>(self, f: fn(Self) -> O) -> O {
        f(self)
    }
}

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

pub const STARTING_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

use Class::*;
use Color::*;

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

    pub fn from_fen(c: char) -> Option<Self> {
        let class = match c.to_ascii_lowercase() {
            'p' => Pawn,
            'r' => Rook,
            'n' => Knight,
            'b' => Bishop,
            'q' => Queen,
            'k' => King,
            _ => return None,
        };
        let side = if c.is_uppercase() { White } else { Black };
        Some(Piece(side, class))
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

    pub fn piece_positions(&self, piece: Piece) -> Vec<Vec2> {
        self.board
            .iter()
            .enumerate()
            .flat_map(|(y, row)| {
                row.iter()
                    .enumerate()
                    .map(move |(x, p)| (xy(x as i32, y as i32), p))
            })
            .filter_map(|(xy, p)| p.filter(|p| *p == piece).map(|p| xy))
            .collect()
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

    pub fn in_check_by(&self) -> Vec<Vec2> {
        let Some(k) = (0..8)
            .flat_map(|y| (0..8).map(move |x| xy(x, y)))
            .find(|v| {
                self.get(*v)
                    .is_some_and(|p| p.0 == self.turn && p.1 == King)
            })
        else {
            return vec![];
        };

        macro_rules! pms {
            ($moves:ident, $pat:pat) => {
                self.$moves(k).into_iter().filter(|&v| {
                    self.get(v)
                        .is_some_and(|p| p.0 != self.turn && matches!(p.1, $pat))
                })
            };
        }

        pms!(bishop_moves, Bishop | Queen)
            .chain(pms!(rook_moves, Rook | Queen))
            .chain(pms!(knight_moves, Knight))
            .chain(pms!(pawn_moves, Pawn))
            .chain(pms!(king_moves, King))
            .inspect(|x| {
                self.can_block(*x, k);
            })
            .collect()
    }

    pub fn path_from(&self, src: Vec2, dst: Vec2) -> Vec<Vec2> {
        if !matches!(self.get(src).map(|p| p.1), Some(Bishop | Queen | Rook)) {
            return Vec::new();
        }

        let norm = (dst - src).normalize();
        (1..)
            .map(|x| norm * x)
            .map(|x| src + x)
            .take_while(|x| *x != dst)
            .collect()
    }

    fn can_block(&self, src: Vec2, dst: Vec2) -> Vec<Vec2> {
        vec![]
    }

    pub fn in_checkmate(&self, checkers: Vec<Vec2>) -> bool {
        false
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
        chess_to_fen(self)
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
            if !self.in_check_by().is_empty() {
                *self.get_mut(from) = self.get_mut(to).take();
                *self.get_mut(to) = old_to;
                return Err(ChError::MoveIntoCheck);
            }
            self.turn = !self.turn;
        }

        let v = self.in_check_by();
        Ok(if v.is_empty() {
            ChState::Normal
        } else {
            ChState::Check(v)
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

#[derive(Debug, Clone, PartialEq)]
pub enum ChState {
    Check(Vec<Vec2>),
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

#[derive(Debug, Clone, Copy)]
pub struct Rank(pub usize);
impl From<&str> for Rank {
    fn from(value: &str) -> Self {
        match value.as_bytes().get(0) {
            Some(&x) if matches!(x, b'a'..=b'h') => Self((x - b'a') as usize),
            _ => panic!("{value:?} is an invalid rank"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct File(pub usize);
impl From<&str> for File {
    fn from(value: &str) -> Self {
        match value.as_bytes().get(0) {
            Some(&x) if matches!(x, b'1'..=b'8') => Self((x - b'1') as usize),
            _ => panic!("{value:?} is an invalid File"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Dispose;

impl From<&str> for Dispose {
    fn from(_value: &str) -> Self {
        Self
    }
}

#[derive(Debug, Clone, Copy)]
#[structre::structre(
    "^(?<QueenCastle>0-0-0)|(?<KingCastle>0-0)|(?:(?<rank_from>[abcdefgh])?(?<file_from>[12345678])?(?<piece_class>[BRNQK])?(?<cap>x)?(?<rank_to>[abcdefgh])(?<file_to>[12345678])(?:[=/]?(?<promote>[ BRNQK]))?)(?:(?<checkmate>(?:#)|(?:\\+\\+))?|(?<check>\\+)?)$"
)]
pub enum Notation {
    QueenCastle(Dispose),
    KingCastle(Dispose),
    Standard {
        rank_from: Option<Rank>,
        file_from: Option<File>,
        piece_class: Option<Class>,
        cap: Option<Dispose>,
        rank_to: Rank,
        file_to: File,
        checkmate: Option<Dispose>,
        check: Option<Dispose>,
        promote: Option<Class>,
    },
}

/*(
    `Rank`?
    `File`?
    [BNRQK]?
    x?
    `Rank`
    `File`
    ([=/]?[BNRQK])?
    [+#]?
) OR (0-0) OR (0-0-0)
 */

use std::{
    fmt::Debug,
    iter::{Successors, successors},
    ops::{Add, AddAssign, Mul, MulAssign, Not, Sub, SubAssign},
    str::FromStr,
};

#[derive(Default, Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Vec2<T = i32> {
    pub x: T,
    pub y: T,
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

    //normalizes to manhattan
    pub fn normalize(mut self) -> Self {
        if self.x != 0 {
            self.x /= self.x.abs();
        }
        if self.y != 0 {
            self.y /= self.y.abs();
        }
        self
    }

    pub fn to_fen(self) -> String {
        String::from_utf8(vec![self.x as u8 + b'a', self.y as u8 + b'1']).unwrap()
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

#[cfg(test)]
mod tests {
    use std::{
        io::{Write, stderr},
        str::FromStr,
    };

    use super::*;
    #[test]
    fn en_passant() {
        let mut chess = Chess::from_fen("8/8/8/pP6/8/8/8/8 w - a6 0 1").unwrap();
        let m = "a6";
        eprintln!("##> {m} <##");
        eprintln!("{}", chess.to_fen());
        stderr().flush().unwrap();
        let note = Notation::from_str(m).unwrap();
        chess.move_by_note(note).unwrap();
    }
}