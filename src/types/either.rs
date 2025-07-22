use crate::attrs::FpEq;
use crate::concepts::{Applicative, Functor, Monad};

#[derive(Debug)]
pub enum Either<E, V> {
    Left(E),
    Right(V),
}

impl<E: FpEq, V: FpEq> FpEq for Either<E, V> {
    fn equals(lhs: &Self, rhs: &Self) -> bool {
        match (lhs, rhs) {
            (Either::Left(e1), Either::Left(e2)) => FpEq::equals(e1, e2),
            (Either::Right(v1), Either::Right(v2)) => FpEq::equals(v1, v2),
            (Either::Left(_), Either::Right(_)) => false,
            (Either::Right(_), Either::Left(_)) => false,
        }
    }
}

impl<E, V> Functor for Either<E, V> {
    type In = V;
    type Boxed<Out> = Either<E, Out>;

    fn fmap<F, Out>(self, mut f: F) -> Self::Boxed<Out>
    where
        F: FnMut(Self::In) -> Out,
    {
        match self {
            Either::Left(e) => Either::Left(e),
            Either::Right(v) => Either::Right(f(v)),
        }
    }
}

impl<E, V> Applicative for Either<E, V> {
    fn pure(v: Self::In) -> Self::Boxed<Self::In> {
        Either::Right(v)
    }

    // type Boxed<Out> = Either<E, Out>;
    // Either<E, FnMut(V) -> Out>
    fn ap<F, Out>(self, f: Self::Boxed<F>) -> Either<E, Out>
    where
        F: FnMut(Self::In) -> Out,
    {
        match (self, f) {
            (Either::Right(v), Either::Right(func)) => Either::pure(v).fmap(func),
            (Either::Right(_), Either::Left(e)) => Either::Left(e),
            (Either::Left(e), _) => Either::Left(e),
        }
    }
}

impl<E, V> Monad for Either<E, V> {
    fn flatmap<F, Out>(self, mut f: F) -> Self::Boxed<Out>
    where
        F: FnMut(Self::In) -> Self::Boxed<Out>,
    {
        match self {
            Either::Left(e) => Either::Left(e),
            Either::Right(v) => f(v),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    type TestEither = Either<i8, u8>;

    #[test]
    fn test_functor() {
        let x = TestEither::Left(-1);
        assert!(FpEq::equals(&x.fmap(|x| x * 2), &TestEither::Left(-1)));
        let x = TestEither::Right(1);
        assert!(FpEq::equals(&x.fmap(|x| x * 2), &TestEither::Right(2)));
    }

    #[test]
    fn test_applicative() {
        let x = TestEither::pure(1);
        assert!(FpEq::equals(&x, &TestEither::Right(1)));
        let f = Either::pure(|x: u8| x * 2);
        assert!(FpEq::equals(&x.ap(f), &TestEither::Right(2)));

        let x = TestEither::Left(-1);
        let f = Either::pure(|x: u8| x * 2);
        assert!(FpEq::equals(&x.ap(f), &TestEither::Left(-1)));
        // TODO: Left transformer not works.
        // let f : Either<i8, dyn FnMut(u8) -> u8 + 'static + Sized> = Either::Left(-1);
        // assert!(FpEq::equals(&x.ap(f), &TestEither::Left(-1)));
    }

    #[test]
    fn test_monad() {
        let x = TestEither::pure(1);
        let f = |x: u8| Either::Right(x * 2);
        assert!(FpEq::equals(&x.flatmap(f), &TestEither::Right(2)));
        let x = TestEither::pure(1);
        let f = |x: u8| Either::Left((x * 2) as i8);
        assert!(FpEq::equals(&x.flatmap(f), &TestEither::Left(2)));
        // TODO: Left transformer not works.
    }
}
