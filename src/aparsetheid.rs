use std::{marker::PhantomData, ops::RangeBounds};

pub trait Stream<T>: Copy {
    fn one(self) -> Option<(T, Self)>;
}

impl Stream<char> for &str {
    fn one(self) -> Option<(char, Self)> {
        self.chars().next().map(|c| (c, &self[c.len_utf8()..]))
    }
}

impl Stream<u8> for &[u8] {
    fn one(self) -> Option<(u8, Self)> {
        self.first().map(|x| (*x, &self[1..]))
    }
}

pub trait StreamT {
    fn one<T>(self) -> Option<(T, Self)>
    where
        Self: Stream<T>,
    {
        Stream::<T>::one(self)
    }
}

impl<T> StreamT for T {}

pub trait Parser<So, O> {
    fn parse<S: Stream<So>>(self, s: S) -> Option<(O, S)>;

    fn maybe(self) -> Maybe<Self>
    where
        Self: Sized,
    {
        Maybe(self)
    }

    fn many(self) -> Many<Self>
    where
        Self: Sized,
    {
        Many {
            parser: self,
            n: None,
        }
    }

    fn exactly(self, n: usize) -> Many<Self>
    where
        Self: Sized,
    {
        Many {
            parser: self,
            n: Some(n),
        }
    }

    fn or<P2: Parser<So, O>>(self, other: P2) -> Or<Self, P2>
    where
        Self: Sized,
    {
        Or(self, other)
    }

    fn map<U, F: FnOnce(O) -> U>(self, f: F) -> Map<So, O, Self, F>
    where
        Self: Sized,
    {
        Map::new(self, f)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Any;
impl<T> Parser<T, T> for Any {
    fn parse<S: Stream<T>>(self, s: S) -> Option<(T, S)> {
        s.one()
    }
}

pub struct Range<R>(pub R);
impl<T: PartialOrd, R: RangeBounds<T>> Parser<T, T> for Range<R> {
    fn parse<S: Stream<T>>(self, s: S) -> Option<(T, S)> {
        let (x, xs) = s.one()?;
        self.0.contains(&x).then_some((x, xs))
    }
}

#[derive(Copy, Clone, Debug)]
pub struct One<T>(T);
impl<T: PartialEq> Parser<T, T> for One<T> {
    fn parse<S: Stream<T>>(self, s: S) -> Option<(T, S)> {
        Any.parse(s).filter(|(x, _)| *x == self.0)
    }
}

pub struct Or<P1, P2>(P1, P2);
impl<So, O, P1: Parser<So, O>, P2: Parser<So, O>> Parser<So, O> for Or<P1, P2> {
    fn parse<S: Stream<So>>(self, s: S) -> Option<(O, S)> {
        self.0.parse(s).or_else(|| self.1.parse(s))
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Many<P> {
    parser: P,
    n: Option<usize>,
}
impl<So, O, P: Parser<So, O> + Clone> Parser<So, Vec<O>> for Many<P> {
    fn parse<S: Stream<So>>(self, mut s: S) -> Option<(Vec<O>, S)> {
        if let Some(0) = self.n {
            return Some((vec![], s));
        }
        let mut v = vec![];
        while let Some((x, xs)) = self.parser.clone().parse(s) {
            s = xs;
            v.push(x);
            if self.n.is_some_and(|n| v.len() == n) {
                return Some((v, xs));
            }
        }
        self.n.is_none().then_some((v, s))
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Maybe<P>(P);
impl<So, O, P: Parser<So, O>> Parser<So, Option<O>> for Maybe<P> {
    fn parse<S: Stream<So>>(self, s: S) -> Option<(Option<O>, S)> {
        match self.0.parse(s) {
            Some((x, xs)) => Some((Some(x), xs)),
            None => Some((None, s)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct VecP<P>(Vec<P>);
impl<So, O, P: Parser<So, O>> Parser<So, Vec<O>> for VecP<P> {
    fn parse<S: Stream<So>>(self, s: S) -> Option<(Vec<O>, S)> {
        let mut s = s;
        let mut v = Vec::new();
        for p in self.0 {
            let (x, xs) = p.parse(s)?;
            s = xs;
            v.push(x);
        }

        Some((v, s))
    }
}

#[derive(Copy, Clone, Debug)]
pub struct All<I>(I);
impl<So, O, P: Parser<So, O>, I: Iterator<Item = P>> Parser<So, O> for All<I> {
    fn parse<S: Stream<So>>(self, s: S) -> Option<(O, S)> {
        
        None
    }
}

#[derive(Clone, Debug)]
pub struct Map<St, T, P: Parser<St, T>, F>(P, Box<F>, PhantomData<(St, T)>);
impl<O, P: Parser<St, T>, F: FnOnce(T) -> O, T, St> Map<St, T, P, F> {
    pub fn new(p: P, f: F) -> Self {
        Self(p, Box::new(f), PhantomData)
    }
}
impl<T, St, O, P: Parser<St, T>, F: FnOnce(T) -> O> Parser<St, O> for Map<St, T, P, F> {
    fn parse<S: Stream<St>>(self, s: S) -> Option<(O, S)> {
        self.0.parse(s).map(|(x, xs)| ((self.1)(x), xs))
    }
}

#[macro_export]
macro_rules! doh {
    ($xs:ident = $s:expr; $($name:ident <- $f:expr);+ $(;)?) => {
        let $xs = $s;
        $(let ($name,$xs) = $f.parse($xs)?;)+
    };
}

pub use doh;

#[cfg(test)]
mod tests {
    use crate::aparsetheid::Parser;

    #[test]
    fn any() {
        let x = [1, 2, 3, 4];
        assert_eq!(Some((1, [2, 3, 4].as_ref())), super::Any.parse(x.as_ref()))
    }

    #[test]
    fn map() {
        use super::*;
        let p = Map::new(Map::new(Any, |x: u8| x as char), |x| x.is_alphanumeric());
        let b = p.parse(b"7".as_ref()).unwrap();
    }

    #[test]
    fn maybe() {
        fn inner() -> Option<()> {
            use super::*;
            let p = One('a');
            doh!(
                xs = "apple";
                _a1 <- p;
                a2 <- p.maybe();
                _p1 <- One('p');
            );

            assert!(a2.is_none());
            assert_eq!(xs, "ple");
            None
        }
        inner();
    }
}
