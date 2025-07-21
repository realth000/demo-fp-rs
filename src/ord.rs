pub enum FpOrdering {
    Greater,
    Equal,
    Less,
}

pub trait FpOrd {
    fn compare(lhs: &Self, rhs: &Self) -> FpOrdering;
}

impl<T: Ord> FpOrd for T {
    fn compare(lhs: &Self, rhs: &Self) -> FpOrdering {
        match lhs.cmp(rhs) {
            std::cmp::Ordering::Less => FpOrdering::Less,
            std::cmp::Ordering::Equal => FpOrdering::Equal,
            std::cmp::Ordering::Greater => FpOrdering::Greater,
        }
    }
}

pub fn min<T: FpOrd + Clone>(lhs: T, rhs: T) -> T {
    match FpOrd::compare(&lhs, &rhs) {
        FpOrdering::Greater => rhs.clone(),
        FpOrdering::Equal => rhs.clone(),
        FpOrdering::Less => lhs.clone(),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_compare() {
        assert_eq!(min(1, 2), 1);
        assert_eq!(min(2, 2), 2);
        assert_eq!(min(4, 3), 3);
    }
}
