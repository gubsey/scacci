pub trait Monad {
    type A;
    type M<T>;
    fn fmap<B>(self, f: impl Fn(Self::A) -> B) -> Self::M<B>;
    fn mmap<B>(self, f: impl Fn(Self::A) -> Self::M<B>) -> Self::M<B>;
}

