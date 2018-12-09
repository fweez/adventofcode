fn run_game(playercount: usize, lastmarble: u32) -> u32 {
    let mut ring: Vec<u32> = [0, 2, 1, 3].to_vec();
    let mut players: Vec<u32> = Vec::new();
    for _ in 0..playercount { players.push(0); }
    let mut marblevalue = 3;
    let mut currringpos: usize = 3;
    let mut currplayer: usize = 3;

    loop {
        if marblevalue > lastmarble { break; }

        marblevalue += 1;
        currplayer = (currplayer + 1) % players.len();
        
        if marblevalue % 23 == 0 {
            players[currplayer] += marblevalue;
            currringpos = match currringpos {
                p if p > 7 => p - 7,
                p => ring.len() - (7 - p)
            };
            players[currplayer] += ring.remove(currringpos);
            currringpos = currringpos % ring.len();
        } else {
            currringpos = (currringpos + 2) % ring.len();
            ring.insert(currringpos, marblevalue);
        }
    }

    players.sort();
    return *players.last().expect("should have had some players");
}

const RUNTESTS: bool = true;
fn main() {
    if RUNTESTS {
        assert_eq!(run_game(9, 25), 32);
        assert_eq!(run_game(10, 1618), 8317);
        assert_eq!(run_game(13, 7999), 146373);
        assert_eq!(run_game(17, 1104), 2764);
        assert_eq!(run_game(21, 6111), 54718);
        assert_eq!(run_game(30, 5807), 37305);
    }
    println!("High score (part a): {}", run_game(411, 71058));
}
