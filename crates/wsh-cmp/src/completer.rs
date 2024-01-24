use itertools::Itertools;

use crate::{source::Source, trie::Trie};

pub struct Completer {
    source: Box<dyn Source + Send>,
}

impl Completer {
    pub fn new(default: Box<dyn Source + Send>) -> Self {
        Self { source: default }
    }

    pub fn suggest(&self, current_word: &str) -> Vec<String> {
        let trie = self.source.suggest().into_iter().collect::<Trie>();
        trie.prefix_list(current_word)
    }
}

impl reedline::Completer for Completer {
    fn complete(&mut self, line: &str, pos: usize) -> Vec<reedline::Suggestion> {
        let mut current_word = line
            .as_bytes()
            .iter()
            .take(pos)
            .rev()
            .take_while(|c| !c.is_ascii_whitespace())
            .copied()
            .collect_vec();
        current_word.reverse();
        let current_word = String::from_utf8(current_word).unwrap();
        self.suggest(&current_word)
            .into_iter()
            .map(|s| reedline::Suggestion {
                span: reedline::Span::new(pos - current_word.len(), pos),
                value: s,
                ..Default::default()
            })
            .collect()
    }
}
