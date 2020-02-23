extern crate argparse;

use argparse::{ArgumentParser, Store};
use std::fs::File;
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::io::{self, Read};

fn invert_map(input_map: BTreeMap<String, u32>) -> BTreeMap<u32, HashSet<String>> {
    let mut inverted_map = BTreeMap::new();
    for (k, v) in input_map.iter() {
        let existing = inverted_map.get_mut(v);
        let owned_v = v.to_owned();
        let owned_k = k.to_owned();
        match existing {
            None => {
                let mut words = HashSet::new();
                words.insert(owned_k);
                inverted_map.insert(owned_v, words);
            },
            Some(existing_words) => {
                existing_words.insert(owned_k);
            }
        }
    }
    inverted_map
}

fn count_words(words: &str) -> BTreeMap<String, u32> {
    let mut word_count = BTreeMap::new();
    for word in words.split_ascii_whitespace() {
        let stripped_word = word.trim();
        let existing_count_o = word_count.get(stripped_word);
        let owned_word = stripped_word.to_owned();
        match existing_count_o {
            None => word_count.insert(owned_word, 1),
            Some(existing_count) => word_count.insert(owned_word, existing_count + 1),
        };
    }
    word_count
}

fn main() -> io::Result<()> {
    let mut filename = "".to_string();
    let mut num_words = 10;

    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Build a concordance from input. Built using Rust.");
        ap.refer(&mut filename)
            .add_option(&["-f", "--filename"], Store,
                        "Filename to read in. If none is given, will default to reading stdin.");
        ap.refer(&mut num_words)
            .add_option(&["-n", "--numWords"], Store,
                        "Number of unique words to print. Defaults to 10.");
        ap.parse_args_or_exit();
    }
    let mut buffer = String::new();
    if filename == "" {
        io::stdin().read_to_string(&mut buffer)?;
    } else {
        let mut file = File::open(filename).expect("No such file.");
        file.read_to_string(&mut buffer)?;
    }
    let word_count = count_words(&buffer);
    let inverted = invert_map(word_count);
    let top_ten: Vec<_> = inverted.iter().rev().take(num_words).collect();
    for x in top_ten.iter() {
        for y in x.1.iter() {
            println!("{:7} {}", x.0, y);
            break;
        }
    }
    Ok(())
}
