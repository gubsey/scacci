use std::{cell::LazyCell, str::FromStr, sync::LazyLock};

use regex::Regex;

use crate::{Class, Piece, aparsetheid::*, chess};

#[derive(Debug, Clone, Copy)]
struct Rank(usize);

impl From<&str> for Rank {
    fn from(value: &str) -> Self {
        match value.as_bytes().get(0) {
            Some(&x) if matches!(x, b'a'..=b'h') => Self((x - b'a') as usize),
            _ => panic!("{value:?} is an invalid rank"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct File(usize);
impl From<&str> for File {
    fn from(value: &str) -> Self {
        match value.as_bytes().get(0) {
            Some(&x) if matches!(x, b'1'..=b'8') => Self((x - b'1') as usize),
            _ => panic!("{value:?} is an invalid File"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PieceLetter;
impl Parser<u8, Piece> for PieceLetter {
    fn parse<S: Stream<u8>>(self, s: S) -> Option<(Piece, S)> {
        use chess::Color::*;
        let (x, xs) = Any.parse(s)?;
        let r = Class::from_byte(x)?;
        let c = if x.is_ascii_uppercase() { White } else { Black };
        Some((Piece(c, r), xs))
    }
}

#[derive(Debug, Clone, Copy)]
struct Dispose;

impl From<&str> for Dispose {
    fn from(value: &str) -> Self {
        Self
    }
}

#[derive(Debug)]
#[structre::structre(
    "^(?<queen_castle>0-0-0)|(?<king_castle>0-0)|(?:(?<rank_from>[abcdefgh])?(?<file_from>[12345678])?(?<piece_class>[brnqkBRNQK])?(?<cap>x)?(?<rank_to>[abcdefgh])(?<file_to>[12345678])(?:[=/]?(?<promote>[brnqkBRNQK]))?)(?:(?<checkmate>(?:#)|(?:\\+\\+))?|(?<check>\\+)?)$"
)]

struct NoteRgx {
    queen_castle: Option<Dispose>,
    king_castle: Option<Dispose>,
    rank_from: Option<Rank>,
    file_from: Option<File>,
    piece_class: Option<Class>,
    cap: Option<Dispose>,
    rank_to: Option<Rank>,
    file_to: Option<File>,
    check: Option<Dispose>,
    checkmate: Option<Dispose>,
    promote: Option<Class>,
}

#[derive(Debug)]
pub enum Notation {
    Standard {
        from_rank: Option<usize>,
        from_file: Option<usize>,
        piece_rank: chess::Class,
        to: [usize; 2],
        promote: Option<chess::Class>,
    },
    KCastle,
    QCastle,
}

pub fn parse_note(note: &str) -> Option<Notation> {
    static REGEX: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new(r#"(?<queen_castle>0-0-0)|(?<king_castle>0-0)|(?:(?<rank_from>[abcdefgh])?(?<file_from>[12345678])?(?<piece_class>[brnqkBRNQK])?(?<cap>x)?(?<rank_to>[abcdefgh])(?<file_to>[12345678])(?:[=/]?(?<promote>[brnqkBRNQK]))?)(?<check>\+)?(?<checkmate>#)?"#).unwrap()
    });

    let caps = NoteRgx::from_str(note);
    dbg!(caps);
    None
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
