use std::{error::Error, fmt::Debug, sync::Arc};

use crate::hlist::*;

pub trait Parser {
    type Extract: Tuple;
    type Err: Error;
    fn parse(self, a: &[u8]) -> Result<(Self::Extract, &[u8]), Self::Err>;

    fn and<P: Parser>(self, other: P) -> And<Self, P>
    where
        Self: Sized,
    {
        And(self, other)
    }

    fn or<P: Parser>(self, other: P) -> Or<Self, P>
    where
        Self: Sized,
    {
        Or(self, other)
    }

    fn many(self) -> Many<Self>
    where
        Self: Clone,
    {
        Many(self)
    }

    fn many_n(self, n: usize) -> ManyN<Self>
    where
        Self: Clone,
    {
        ManyN(self, n)
    }

    fn many1(self) -> ManyN<Self>
    where
        Self: Clone,
    {
        ManyN(self, 1)
    }

    fn ignore(self) -> Ignore<Self>
    where
        Self: Sized,
    {
        Ignore(self)
    }

    fn pmap<T, O, F: Fn(T) -> O>(self, f: F) -> Map<Self, T, O, F>
    where
        Self: Parser<Extract = (T,)> + Sized,
    {
        Map(self, f)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ParserError {
    Eof,
    InvalidUnit(u8),
}
impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}
impl std::error::Error for ParserError {}

#[derive(Debug, Clone, Copy)]
pub struct Any;
impl Parser for Any {
    type Extract = (u8,);
    type Err = ParserError;
    fn parse(self, a: &[u8]) -> Result<(Self::Extract, &[u8]), Self::Err> {
        a.first()
            .map(|x| (one(*x), &a[1..]))
            .ok_or(ParserError::Eof)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Unit(pub u8);
impl Parser for Unit {
    type Extract = One<u8>;
    type Err = ParserError;

    fn parse(self, a: &[u8]) -> Result<(Self::Extract, &[u8]), Self::Err> {
        Any.parse(a).and_then(|x| {
            (x.0.0 == self.0)
                .then_some(x)
                .ok_or(ParserError::InvalidUnit(self.0))
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Or<A: Parser, B: Parser>(pub A, pub B);
impl<A: Parser, B: Parser> Parser for Or<A, B> {
    type Extract = One<Either<A::Extract, B::Extract>>;
    type Err = ParserError;
    fn parse(self, a: &[u8]) -> Result<(Self::Extract, &[u8]), Self::Err> {
        match self.0.parse(a) {
            Err(_) => match self.1.parse(a) {
                Err(x) => Err(x),
                Some((x, xs)) => Some((one(Either::B(x)), xs)),
            },
            Some((x, xs)) => Some((one(Either::A(x)), xs)),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FirstOf<P: Parser, I: IntoIterator<Item = P>>(pub I);
impl<P: Parser, I: IntoIterator<Item = P>> Parser for FirstOf<P, I> {
    type Extract = P::Extract;
    fn parse(self, a: &[u8]) -> Option<(Self::Extract, &[u8])> {
        self.0.into_iter().filter_map(|x| x.parse(a)).next()
    }
}

impl<P: Parser, I: IntoIterator<Item = P>> Parser for I {
    type Extract = P::Extract;
    fn parse(self, a: &[u8]) -> Option<(Self::Extract, &[u8])> {
        FirstOf(self).parse(a)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct And<A: Parser, B: Parser>(pub A, pub B);
impl<A: Parser, B: Parser> Parser for And<A, B>
where
    <A::Extract as Tuple>::HList: Combine<<B::Extract as Tuple>::HList>,
{
    type Extract = CombinedTuples<A::Extract, B::Extract>;
    fn parse(self, a: &[u8]) -> Option<(Self::Extract, &[u8])> {
        let (x, xs) = self.0.parse(a)?;
        let (y, xs) = self.1.parse(xs)?;
        Some((x.combine(y), xs))
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Map<P: Parser<Extract = (T,)>, T, O, F: Fn(T) -> O>(P, F);
impl<T, O, F: Fn(T) -> O, P: Parser<Extract = (T,)>> Parser for Map<P, T, O, F> {
    type Extract = One<O>;
    fn parse(self, a: &[u8]) -> Option<(Self::Extract, &[u8])> {
        self.0.parse(a).map(|(x, xs)| {
            let y = self.1.apply((x.0,));
            (one(y), xs)
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Many<P>(pub P);
impl<T, P: Parser<Extract = One<T>> + Clone> Parser for Many<P> {
    type Extract = One<Vec<T>>;
    fn parse(self, a: &[u8]) -> Option<(Self::Extract, &[u8])> {
        let mut a = a.clone();
        let mut r = vec![];
        while let Some((x, xs)) = self.0.clone().parse(a) {
            a = xs;
            r.push(x.0);
        }
        Some((one(r), a))
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ManyN<P>(pub P, usize);
impl<T, P: Parser<Extract = One<T>> + Clone> Parser for ManyN<P> {
    type Extract = <Many<P> as Parser>::Extract;
    fn parse(self, a: &[u8]) -> Option<(Self::Extract, &[u8])> {
        self.0.many().parse(a).filter(|x| x.0.0.len() == self.1)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Ignore<P: Parser>(P);
impl<P: Parser> Parser for Ignore<P> {
    type Extract = ();
    fn parse(self, a: &[u8]) -> Option<(Self::Extract, &[u8])> {
        self.0.parse(a).map(|x| ((), x.1))
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Digit;
impl Parser for Digit {
    type Extract = One<u8>;
    fn parse(self, a: &[u8]) -> Option<(Self::Extract, &[u8])> {
        (b'0'..=b'9').map(Unit).parse(a)
    }
}
