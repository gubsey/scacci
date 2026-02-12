use std::{fmt::Display, str::FromStr};

use crate::{Class, chess};

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
