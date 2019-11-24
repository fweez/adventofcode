pub mod adjustmentlist {
    use std::io;

    pub type Adjustment = i32;

    pub struct AdjustmentList {
        
    }

    impl AdjustmentList {
        pub fn new() -> AdjustmentList {
            AdjustmentList { }
        }
    }

    impl Iterator for AdjustmentList {
        type Item = Adjustment;

        fn next(&mut self) -> Option<Self::Item> {
            let mut input = String::new();
            match io::stdin().read_line(&mut input) {
                Err(_error) => return None,
                Ok(_sz) => {
                    let item = input.trim();
                    if item.len() == 0 {
                        return None
                    }
                    let sign: i32 = match &item[0..1] {
                        "+" => 1,
                        "-" => -1,
                        _ => return None
                    };
                    let value = item[1..].parse::<i32>().unwrap() * sign;
                    return Some(value);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
