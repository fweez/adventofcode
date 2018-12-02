use std::io;

fn main() {
    let mut inputs: Vec<String> = Vec::new();
    loop {
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Err(_error) => break,
            Ok(_sz) => {
                let new_id = input.trim();
                if new_id.len() == 0 {
                    break;
                }
                for old_id in &inputs {
                    let new_chars: Vec<char> = new_id.chars().collect();
                    let old_chars: Vec<char> = old_id.chars().collect();
                    let mut diffs = 0;
                    let mut last_diff_pos = 0;
                    for i in 0..new_chars.len() {
                        if new_chars[i] != old_chars[i] {
                            diffs += 1;
                            last_diff_pos = i;
                        }
                    }
                    if diffs == 1 {
                        println!("{} / {}", new_id, old_id);
                        println!("{}{}", new_id[0..last_diff_pos].to_string(), new_id[last_diff_pos+1..].to_string());
                        //return
                    }
                }
                inputs.push(new_id.to_string());
            }
        }
    }
}
