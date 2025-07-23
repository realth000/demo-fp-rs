use fp::concepts::{Applicative, Functor};
use fp::types::FpOption;

fn main() {
    let option_fn: FpOption<fn(i32) -> i32> = FpOption::Some(|x| x * 2);
    let option_value: FpOption<i32> = FpOption::Some(5);
    let result = option_value.ap(option_fn);
    println!("{:?}", result);

    let result = result.map(|x| x + 1);
    println!("{:?}", result);

    let x = FpOption::<i32>::None
        .map(|x| x * 2)
        .or_else(|| FpOption::pure(-100));
    println!("{:?}", x);
}
