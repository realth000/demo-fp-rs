pub trait FpEq {
    fn equals(lhs: &Self, rhs: &Self) -> bool;
}

impl<T: Eq + Ord> FpEq for T {
    fn equals(lhs: &Self, rhs: &Self) -> bool {
        lhs == rhs
    }
}

pub fn elem<T: FpEq>(x: T, xs: &[T]) -> bool {
    xs.iter().any(|y| FpEq::equals(&x, y))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_elem() {
        assert_eq!(elem(1, &[2, 3]), false);
        assert_eq!(elem(1, &[1, 3]), true);
        assert_eq!(elem(1, &[]), false);
    }
}
