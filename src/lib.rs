/// Custom option type, same as the [`Option`] in std.
///
/// We use it as a raw type till it becomes a monad.
#[derive(Debug)]
pub enum FpOption<T> {
    None,
    Some(T),
}

/// Functor is a definition requires [`Functor::fmap`].
pub trait Functor<In, Out> {
    /// The value of `In` type boxed in current type.
    type BoxedIn;

    /// The value of `Out` type boxed in current type.
    type BoxedOut;

    /// Run the transform `F` on the input and return the boxed output.
    fn fmap<F>(self, f: F) -> Self::BoxedOut
    where
        F: FnMut(In) -> Out;
}

/// Applicative is a definition requires:
///
/// * [`Functor`].
/// * [`Applicative::pure`] and [`Applicative::ap`].
pub trait Applicative<In, Out>: Functor<In, Out> {
    type BoxedFn<ApFn>;

    /// Box value `In` in current type.
    fn pure(v: In) -> Self::BoxedIn;

    /// Apply the transform `f` on raw type `In` and returns boxed output.
    fn ap<F>(self, f: Self::BoxedFn<F>) -> Self::BoxedOut
    where
        F: FnMut(In) -> Out;
}

impl<In, Out> Functor<In, Out> for FpOption<In> {
    /// For [`FpOption`] type, box the `In` type in [`FpOption`]
    type BoxedIn = FpOption<In>;

    /// For [`FpOption`] type, box the `Out` type in [`FpOption`]
    type BoxedOut = FpOption<Out>;

    /// Run tramsformer `f` on current [`FpOption`] value if currently
    /// have a value and return the boxed output.
    ///
    /// If have no value, aka [`FpOption::None`], return the same [`FpOption::None`].
    fn fmap<F>(self, mut f: F) -> Self::BoxedOut
    where
        F: FnMut(In) -> Out,
    {
        match self {
            FpOption::None => FpOption::None,
            FpOption::Some(v) => FpOption::Some(f(v)),
        }
    }
}

impl<In, Out> Applicative<In, Out> for FpOption<In> {
    /// The transform applies in [`Applicative::ap`].
    ///
    /// It is a function boxed in current type [`FpOption`] which
    /// accept `In` and returns `Out`.
    ///
    /// Here it is a opaque type because we can only specify its [`FnMut`]
    /// trait in the definition of [`Applicative::ap`].
    type BoxedFn<ApFn> = FpOption<ApFn>;

    /// Box raw value `v` into [`FpOption::Some`].
    fn pure(v: In) -> Self::BoxedIn {
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
    fn ap<F>(self, f: Self::BoxedFn<F>) -> Self::BoxedOut
    where
        F: FnMut(In) -> Out,
    {
        match (self, f) {
            (FpOption::Some(v), FpOption::Some(mut func)) => FpOption::Some(func(v)),
            _ => FpOption::None,
        }
    }
}
