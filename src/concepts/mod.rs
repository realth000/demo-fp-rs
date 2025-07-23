/// Functor is a definition requires [`Functor::fmap`].
pub trait Functor {
    type In;
    type Boxed<Out>;

    /// Run the transform `F` on the input and return the boxed output.
    fn map<F, Out>(self, f: F) -> Self::Boxed<Out>
    where
        F: FnMut(Self::In) -> Out;
}

/// Applicative is a definition requires:
///
/// * [`Functor`].
/// * [`Applicative::pure`] and [`Applicative::ap`].
pub trait Applicative: Functor {
    /// Box value `In` in current type.
    fn pure(v: Self::In) -> Self::Boxed<Self::In>;

    /// Alias [`Applicative::pure`].
    fn of(v: Self::In) -> Self::Boxed<Self::In> {
        Self::pure(v)
    }

    /// Apply the transform `f` on raw type `In` and returns boxed output.
    fn ap<F, Out>(self, f: Self::Boxed<F>) -> Self::Boxed<Out>
    where
        F: FnMut(Self::In) -> Out;
}

/// Monad is based on [`Applicative`], which has a `flatmap` method to avoid nested
/// structures when chaining multiple actions.
pub trait Monad: Applicative {
    /// Calls `f` on itself and returns the single layered boxed value, avoid nesting
    /// wrapper again and again.
    fn flatmap<F, Out>(self, f: F) -> Self::Boxed<Out>
    where
        F: FnMut(Self::In) -> Self::Boxed<Out>;
}
