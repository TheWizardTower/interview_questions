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
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let word_count = count_words(&buffer);
    let inverted = invert_map(word_count);
    let top_ten: Vec<_> = inverted.iter().rev().take(10).collect();
    println!("{:?}", top_ten);
    Ok(())
}
