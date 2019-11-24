pub mod inputiterator {
    use std::io;
    use std::iter::Map;

    pub struct InputIterator {

    }

    impl InputIterator {
        pub fn new() -> InputIterator { return InputIterator { } }

        pub fn transform<B, F>(f: F) -> Map<Self, F> where F: FnMut(String) -> B {
            return Self::new().map(f);
        }
    }

    impl Iterator for InputIterator {
        type Item = String;

        fn next(&mut self) -> Option<Self::Item> {
            let mut input = String::new();
            match io::stdin().read_line(&mut input) {
                Err(_error) => return None,
                Ok(_sz) => {
                    let item = input.trim();
                    if item.len() == 0 {
                        return None
                    }
                    return Some(item.to_string());
                }
            }
        }
    }
}
