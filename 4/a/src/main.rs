extern crate inputiterator;
use inputiterator::inputiterator::InputIterator;
extern crate chrono;
use chrono::prelude::*;
use std::collections::HashMap;

enum LogEvent {
    BeginShift,
    Asleep,
    Wake,
}

type GuardId = i16;

struct LogEntry {
    time: DateTime<Utc>,
    guard: GuardId,
    event: LogEvent,
}

const NOT_AN_ID: GuardId = -1;
const OUT_OF_BOUNDS: usize = 99;

impl LogEntry {
    fn new(input: String) -> LogEntry {
        /*
        [1518-09-02 23:57] Guard #2969 begins shift
        [1518-11-16 00:42] wakes up
        [1518-08-03 00:14] falls asleep
        */
        let mut elements = input.split(|c| c == '[' || c == ']');
        let _ignored = elements.next().expect("no pre-datetime text!");
        let date_string = elements.next().expect("no minute!");
        let time = Utc.datetime_from_str(date_string, "%Y-%m-%d %H:%M").expect("didn't parse date time!");

        let log_string = elements.next().expect("no log string!").to_string();
        let event = match log_string.trim() {
            "wakes up" => LogEvent::Wake,
            "falls asleep" => LogEvent::Asleep,
            _ => LogEvent::BeginShift,
        };
        let guard = match event {
            LogEvent::BeginShift => {
                let halves: Vec<&str> = log_string.split('#').collect();
                let words: Vec<&str> = halves[1].split(' ').collect();
                let id_string: &str = words[0];
                id_string.parse::<GuardId>().expect("guard id didn't parse!")
            }
            _ => NOT_AN_ID,
        };

        return LogEntry { time, guard, event };
    }
}

fn new_minutes_array() -> Vec<i8> {
    let mut minutes_template: Vec<i8> = Vec::new();
    for _ in 0..60 { minutes_template.push(0); }
    return minutes_template;
}

fn main() {
    let mut log_entries: Vec<LogEntry> = InputIterator::new().map(|s| LogEntry::new(s)).collect();
    log_entries.sort_by_key(|l| l.time);

    // Map guard ids to a list of counts of nights this guard was asleep at that minute
    let mut sleep_times: HashMap<GuardId, Vec<i8>> = HashMap::new();
    let mut curr_guard_id = NOT_AN_ID;
    let mut curr_start_time = OUT_OF_BOUNDS;
    for log_entry in log_entries {
        match log_entry.event {
            LogEvent::BeginShift => {
                if curr_start_time != OUT_OF_BOUNDS {
                    let minutes = sleep_times.entry(curr_guard_id).or_insert(new_minutes_array());
                    for minute in curr_start_time..60 {
                        minutes[minute] += 1;
                    }
                }
                curr_guard_id = log_entry.guard;
            }
            LogEvent::Asleep => {
                curr_start_time = log_entry.time.minute() as usize;
            }
            LogEvent::Wake => {
                if curr_start_time == OUT_OF_BOUNDS {
                    panic!("Didn't have a sleep event!");
                }
                if curr_guard_id == NOT_AN_ID {
                    panic!("Didn't have a guard id!");
                }

                let minutes = sleep_times.entry(curr_guard_id).or_insert(new_minutes_array());
                for minute in curr_start_time..(log_entry.time.minute() as usize) {
                    minutes[minute] += 1;
                }
                curr_start_time = OUT_OF_BOUNDS;
            }
        }   
    }

    let mut stats: Vec<(i32, usize, GuardId, i8)> = Vec::new();
    for (guard, minutes) in &sleep_times {
        let mut asleep_minute_count: i32 = 0;
        let mut most_often_asleep_value = 0;
        let mut most_often_asleep_minute = OUT_OF_BOUNDS;
        for (idx, minute) in minutes.iter().enumerate() {
            if *minute > 0 { asleep_minute_count += *minute as i32; }
            if *minute > most_often_asleep_value { 
                most_often_asleep_value = *minute;
                most_often_asleep_minute = idx; 
            }
        }
        stats.push((asleep_minute_count, most_often_asleep_minute, *guard, most_often_asleep_value));
    }

    stats.sort_by_key(|s| s.0);
    let winner = stats.last().expect("Should have some values in the stats!").clone();
    println!("part A: guard {} is most asleep ({} minutes), most often at {}. answer: {}",
        winner.2, winner.0, winner.1, (winner.1 as i32) * (winner.2 as i32));
    stats.sort_by_key(|s| s.3);
    let winner = stats.last().expect("Should have some values in the stats!");
    println!("part B: guard {} is most asleep during minute {}, in {} days. answer: {}",
        winner.2, winner.1, winner.3, (winner.1 as i32) * (winner.2 as i32));
    

    for (guard, minutes) in &sleep_times {
        print!("{}\t", guard);
        for (idx, minute) in minutes.iter().enumerate() {
            if *minute < 10 {
                print!("{}", *minute);
            } else {
                print!("x");
            }
        }
        print!("\n");
    }
}
