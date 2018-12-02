use std::hash::{Hash, Hasher};
use std::collections::HashMap;

struct BoxId {
    id: String,
    sorted: Vec<char>
}

impl BoxId {
    fn new(id: &str) -> BoxId {
        let id = id.to_string();
        let mut sorted: Vec<char> = id.chars().collect();
        sorted.sort();
        
        BoxId { id, sorted }
    }
}

impl Hash for BoxId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.sorted.hash(state);
    }
}

impl PartialEq for BoxId {
    fn eq(&self, other: &BoxId) -> bool {
        self.id == other.id
    }
}

impl Eq for BoxId {}

pub struct BoxIDList {
    hashes: HashMap<BoxId, (i32, bool, bool)>
}

impl BoxIDList {
    pub fn new() -> BoxIDList {
        BoxIDList { hashes: HashMap::new() }
    }

    pub fn insert(&mut self, item: &str){
        let new_id = BoxId::new(&item);
        let sorted = new_id.sorted.clone();
        let mut uniques = sorted.clone(); 
        uniques.dedup();
        let dupe_info = self.hashes.entry(new_id).or_insert((0, false, false)) ;
        dupe_info.0 += 1;
        for c in uniques {
            if dupe_info.1 && dupe_info.2 { break; }

            let mut lettercount = 0;
            for d in &sorted {
                if c == *d { lettercount += 1; }
            }
            if lettercount == 2 { dupe_info.1 = true; }
            if lettercount == 3 { dupe_info.2 = true; }
        }
    }

    pub fn checksum(&self) -> i32 {
        let mut pair_accum = 0;
        let mut triplet_accum = 0;
        for v in self.hashes.values() {
            if v.1 { pair_accum += v.0; }
            if v.2 { triplet_accum += v.0; }
        }
        return pair_accum * triplet_accum;
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_one_insert() {
        let mut boxids = super::BoxIDList::new();
        boxids.insert("aaa");
        assert_eq!(boxids.checksum(), 0);
    }

    #[test]
    fn test_two_inserts() {
        let mut boxids = super::BoxIDList::new();
        boxids.insert("abb");
        boxids.insert("bbb");
        assert_eq!(boxids.checksum(), 1);
    }

    #[test]
    fn test_three_inserts() {
        let mut boxids = super::BoxIDList::new();
        boxids.insert("aaa");
        boxids.insert("abb");
        boxids.insert("bba");
        assert_eq!(boxids.checksum(), 2);
    }

    #[test]
    fn test_three_unlike_inserts() {
        let mut boxids = super::BoxIDList::new();
        boxids.insert("aaa");
        boxids.insert("abb");
        boxids.insert("bbx");
        assert_eq!(boxids.checksum(), 2);
    }

    #[test]
    fn test_three_unlike_inserts() {
        let mut boxids = super::BoxIDList::new();
        boxids.insert("aaa");
        boxids.insert("abb");
        boxids.insert("bbx");
        assert_eq!(boxids.checksum(), 2);
    }
}
