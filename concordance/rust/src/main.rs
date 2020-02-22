use std::collections::BTreeMap;
use std::collections::HashSet;
use std::io::{self, Read};

// transMap :: (Ord k, Ord a) => M.Map k a -> M.Map a (S.Set k)
// transMap = M.fromListWith S.union . map (second S.singleton . swap) . M.toList
//   where swap (one, two) = (two, one)

// countWords wordList =
//   L.foldl' f M.empty wordList
//   where f wordMap word =
//           updateWordMap wordMap word
//         updateWordMap wordMap word =
//           M.insertWith (+) word 1 wordMap

// main :: IO ()
// main = do
//   input <- B.getContents
//   let text = TE.decodeUtf8 input
//       wordList = T.words text
//       wordCounts = countWords wordList
//       topWords = take 10 $ M.toDescList $ transMap wordCounts
//   print topWords

// fn invert_map<K, V>(input_map: BTreeMap<K, V>) -> BTreeMap<V, HashSet<K>>
// fn invert_map<K, V>(input_map: BTreeMap<K, V>) -> BTreeMap<V, HashSet<K>>
// where
//     K: std::cmp::Eq,
//     K: std::hash::Hash,
//     V: std::cmp::Ord,
// {
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
                // let new_word_set: HashSet<String> = existing_words.union(&words).collect();
                // inverted_map.insert(owned_v, e);
                existing_words.insert(owned_k);
            }
        }
    }
    inverted_map
}

fn countWords(words: &str) -> BTreeMap<String, u32> {
    let mut wordCount = BTreeMap::new();
    // for word in words.split(" ") {
    for word in words.split_ascii_whitespace() {
        let stripped_word = word.trim();
        let existingCountO = wordCount.get(stripped_word);
        let owned_word = stripped_word.to_owned();
        match existingCountO {
            None => wordCount.insert(owned_word, 1),
            Some(existingCount) => wordCount.insert(owned_word, existingCount + 1),
        };
    }
    wordCount
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let wordCount = countWords(&buffer);
    let inverted = invert_map(wordCount);
    let top_ten: Vec<_> = inverted.iter().rev().take(10).collect();
    // println!("{:?}", wordCount);
    // println!("{:?}", inverted);
    println!("{:?}", top_ten);
    Ok(())
}
