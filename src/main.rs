fn main() {
    println!("Hello, world!");

    let ws = OneOf(b" \t\r\n").ignore();
    let castle = b"KQkq".into_iter().copied().map(One);
}

enum Rank {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}
enum Color {
    Black,
    White,
}
struct Piece(Color, Rank);

struct PieceMap;

impl Parser for PieceMap {
    type Out = [[Option<Piece>; 8]; 8];
    fn parse(self, a: &[u8]) -> Option<(Self::Out, &[u8])> {
        let piece = OneOf(b"prnbqkprnbqk12345678").many();
        let block = piece.and(One(b'/').ignore());
        let pp = block.many().and(piece);
        let ((v1, v2), xs) = pp.parse(a)?;

        None
    }
}

pub trait Parser {
    type Out;
    fn parse(self, a: &[u8]) -> Option<(Self::Out, &[u8])>;

    fn and<P: Parser>(self, p: P) -> And<Self, P>
    where
        Self: Sized,
    {
        And(self, p)
    }

    fn many(self) -> Many<Self>
    where
        Self: Sized + Copy,
    {
        Many(self)
    }

    fn ignore(self) -> Ignore<Self>
    where
        Self: Sized,
    {
        Ignore(self)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Any;
impl Parser for Any {
    type Out = u8;
    fn parse(self, a: &[u8]) -> Option<(Self::Out, &[u8])> {
        a.first().map(|x| (*x, &a[1..]))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct One(pub u8);
impl Parser for One {
    type Out = u8;
    fn parse(self, a: &[u8]) -> Option<(Self::Out, &[u8])> {
        Any.parse(a).filter(|(x, _)| *x == self.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Many<P: Parser + Copy>(pub P);
impl<P: Parser + Copy> Parser for Many<P> {
    type Out = Vec<P::Out>;
    fn parse(self, a: &[u8]) -> Option<(Self::Out, &[u8])> {
        let mut a = a;
        let mut v = Vec::new();

        while let Some((x, xs)) = self.0.parse(a) {
            v.push(x);
            a = xs;
        }

        return Some((v, a));
    }
}

#[derive(Debug, Clone, Copy)]
pub struct OneOf<'a>(pub &'a [u8]);
impl<'a> Parser for OneOf<'a> {
    type Out = u8;
    fn parse(self, a: &[u8]) -> Option<(Self::Out, &[u8])> {
        self.0
            .into_iter()
            .map(|x| One(*x))
            .filter_map(|o| o.parse(a))
            .next()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct And<P1: Parser, P2: Parser>(pub P1, pub P2);
impl<P1: Parser, P2: Parser> Parser for And<P1, P2> {
    type Out = (P1::Out, P2::Out);
    fn parse(self, a: &[u8]) -> Option<(Self::Out, &[u8])> {
        self.0
            .parse(a)
            .and_then(|(x, xs)| self.1.parse(xs).map(|(y, xs)| ((x, y), xs)))
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Ignore<P: Parser>(pub P);
impl<P: Parser> Parser for Ignore<P> {
    type Out = ();
    fn parse(self, a: &[u8]) -> Option<(Self::Out, &[u8])> {
        self.0.parse(a).map(|(_, xs)| ((), xs))
    }
}
