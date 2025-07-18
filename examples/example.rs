use fp::{Applicative, FpOption, Functor};

fn main() {
    let option_fn: FpOption<fn(i32) -> i32> = FpOption::Some(|x| x * 2);
    let option_value: FpOption<i32> = FpOption::Some(5);
    let result = option_value.ap(option_fn);
    println!("{:?}", result);

    let result = result.fmap(|x| x + 1);
    println!("{:?}", result);
}
