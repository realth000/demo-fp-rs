use crate::attrs::FpEq;
use crate::concepts::{Applicative, Functor, Monad};

/// Custom option type, same as the [`Option`] in std.
///
/// We use it as a raw type till it becomes a monad.
#[derive(Debug)]
pub enum FpOption<T> {
    None,
    Some(T),
}

impl<T> FpOption<T> {
    pub fn or(self, v: FpOption<T>) -> FpOption<T> {
        match self {
            FpOption::Some(v) => FpOption::pure(v),
            FpOption::None => v,
        }
    }

    pub fn or_else<F>(self, mut f: F) -> FpOption<T>
    where
        F: FnMut() -> FpOption<T>,
    {
        match self {
            FpOption::Some(v) => FpOption::pure(v),
            FpOption::None => f(),
        }
    }
}

impl<T> Functor for FpOption<T> {
    type In = T;
    /// For [`FpOption`] type, box the `In` type in [`FpOption`]
    type Boxed<Out> = FpOption<Out>;

    /// Run tramsformer `f` on current [`FpOption`] value if currently
    /// have a value and return the boxed output.
    ///
    /// If have no value, aka [`FpOption::None`], return the same [`FpOption::None`].
    fn fmap<F, Out>(self, mut f: F) -> Self::Boxed<Out>
    where
        F: FnMut(Self::In) -> Out,
    {
        match self {
            FpOption::None => FpOption::<Out>::None,
            FpOption::Some(v) => FpOption::<Out>::Some(f(v)),
        }
    }
}

impl<In> Applicative for FpOption<In> {
    /// The transform applies in [`Applicative::ap`].
    ///
    /// It is a function boxed in current type [`FpOption`] which
    /// accept `In` and returns `Out`.
    ///
    /// Here it is a opaque type because we can only specify its [`FnMut`]
    /// trait in the definition of [`Applicative::ap`].

    /// Box raw value `v` into [`FpOption::Some`].
    fn pure(v: Self::In) -> Self::Boxed<Self::In> {
        FpOption::Some(v)
    }

    /// Apply the transform `f` on current [`FpOption`] if all these
    /// conditions are met:
    ///
    /// * `f` has value, not [`FpOption::None`].
    /// * `self` has value, not [`FpOption::None`].
    ///
    /// then returns the boxed value after applying `f`.
    ///
    /// Otherwise returns [`FpOption::None`].
    fn ap<F, Out>(self, f: Self::Boxed<F>) -> FpOption<Out>
    where
        F: FnMut(In) -> Out,
    {
        match (self, f) {
            (FpOption::Some(v), FpOption::Some(func)) => FpOption::pure(v).fmap(func),
            _ => FpOption::<Out>::None,
        }
    }
}

impl<T> Monad for FpOption<T> {
    fn flatmap<F, Out>(self, mut f: F) -> Self::Boxed<Out>
    where
        F: FnMut(Self::In) -> Self::Boxed<Out>,
    {
        match self {
            FpOption::Some(x) => f(x),
            _ => FpOption::None,
        }
    }
}

impl<T: FpEq> FpEq for FpOption<T> {
    fn equals(lhs: &Self, rhs: &Self) -> bool {
        match (lhs, rhs) {
            (FpOption::None, FpOption::None) => true,
            (FpOption::None, FpOption::Some(_)) => false,
            (FpOption::Some(_), FpOption::None) => false,
            (FpOption::Some(v1), FpOption::Some(v2)) => FpEq::equals(v1, v2),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_functor() {
        let x = FpOption::Some(1);
        assert!(FpEq::equals(&x.fmap(|x| x * 2), &FpOption::Some(2)));
        let x = FpOption::<i32>::None;
        assert!(FpEq::equals(&x.fmap(|x| x * 2), &FpOption::None));
    }

    #[test]
    fn test_applicative() {
        let x = FpOption::pure(1);
        assert!(FpEq::equals(&x, &FpOption::Some(1)));
        let f = FpOption::pure(|x: i32| x * 2);
        assert!(FpEq::equals(&x.ap(f), &FpOption::Some(2)));

        let x = FpOption::<i32>::None;
        let f = FpOption::pure(|x: i32| x * 2);
        assert!(FpEq::equals(&x.ap(f), &FpOption::None));
    }

    #[test]
    fn test_monad() {
        let x = FpOption::pure(1);
        assert!(FpEq::equals(
            &x.flatmap(|x| FpOption::pure(x * 2)),
            &FpOption::pure(2)
        ));
        let x = FpOption::<i32>::None;
        assert!(FpEq::equals(
            &x.flatmap(|x| FpOption::pure(x * 2)),
            &FpOption::None
        ));
    }

    #[test]
    fn test_or() {
        assert!(FpEq::equals(
            &FpOption::pure(1).or(FpOption::pure(2)),
            &FpOption::pure(1)
        ));
        assert!(FpEq::equals(
            &FpOption::pure(1).or(FpOption::None),
            &FpOption::pure(1)
        ));
        assert!(FpEq::equals(
            &FpOption::<i32>::None.or(FpOption::<i32>::pure(2)),
            &FpOption::pure(2)
        ));
        assert!(FpEq::equals(
            &FpOption::<i32>::None.or(FpOption::<i32>::None),
            &FpOption::None
        ));
    }

    #[test]
    fn test_or_else() {
        assert!(FpEq::equals(
            &FpOption::pure(1).or_else(|| FpOption::pure(2)),
            &FpOption::pure(1)
        ));
        assert!(FpEq::equals(
            &FpOption::pure(1).or_else(|| FpOption::None),
            &FpOption::pure(1)
        ));
        assert!(FpEq::equals(
            &FpOption::<i32>::None.or_else(|| FpOption::<i32>::pure(2)),
            &FpOption::pure(2)
        ));
        assert!(FpEq::equals(
            &FpOption::<i32>::None.or_else(|| FpOption::<i32>::None),
            &FpOption::None
        ));
    }
}
