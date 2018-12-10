use std::rc::Rc;
use std::cell::RefCell;

struct Node<T> {
    value: T,
    prev: Link<T>,
    next: Link<T>
}

type Link<T> = Option<Rc<RefCell<Node<T>>>>;

impl<T> Node<T> {
    fn new(value: T) -> Rc<RefCell<Self>> {
        let s = Rc::new(RefCell::new(Node { value, prev: None, next: None}));
        s.borrow_mut().prev = Some(s.clone());
        s.borrow_mut().next = Some(s.clone());
        return s;
    }

    fn append(&mut self, value: T) {
        let newnext = Self::new(value);
        let oldnext = self.next.take().unwrap();
        oldnext.borrow_mut().prev = Some(newnext.clone());
        newnext.borrow_mut().next = Some(oldnext.clone());
        
        let prev = self.prev.take().unwrap();
        let this = prev.borrow_mut().next.take().unwrap();
        newnext.borrow_mut().prev = Some(this.clone());
    }

    fn remove(&mut self) {
        self.next.take().unwrap().borrow_mut().prev = Some(self.prev.take().unwrap().clone());
        self.prev.take().unwrap().borrow_mut().next = Some(self.next.take().unwrap().clone());
    }
}

fn run_game(playercount: usize, lastmarble: u32) -> u32 {
    let mut curr = Node::new(0);
    let mut players: Vec<u32> = Vec::new();
    for _ in 0..playercount { players.push(0); }
    let mut marblevalue = 0;
    let mut currplayer: usize = 0;

    loop {
        if marblevalue > lastmarble { break; }
        
        marblevalue += 1;
        currplayer = (currplayer + 1) % players.len();
        
        if marblevalue % 23 == 0 {
            players[currplayer] += marblevalue;
            for _ in 0..7 {
                let newcurr = curr.borrow_mut().prev.take().unwrap().clone();
                curr = newcurr;
            }
            players[currplayer] += curr.borrow().value;
            curr.borrow_mut().remove();
        } else {
            let newcurr = curr.borrow_mut().next.take().unwrap().clone();
            curr = newcurr;
            curr.borrow_mut().append(marblevalue);
        }
    }

    players.sort();
    return *players.last().unwrap();
}

const RUNTESTS: bool = true;
fn main() {
    let h = Node::new(0);
    println!("{}", h.borrow().value);
    h.borrow_mut().append(1);
    let newcurr = h.borrow_mut().next.take().unwrap().clone();
    println!("{}", newcurr.borrow().value);
/*
    if RUNTESTS {
        assert_eq!(run_game(9, 25), 32);
        assert_eq!(run_game(10, 1618), 8317);
        assert_eq!(run_game(13, 7999), 146373);
        assert_eq!(run_game(17, 1104), 2764);
        assert_eq!(run_game(21, 6111), 54718);
        assert_eq!(run_game(30, 5807), 37305);
    }
    println!("High score (part a): {}", run_game(411, 71058));
    println!("High score (part b): {}", run_game(411, 71058 * 100));
    */
}
