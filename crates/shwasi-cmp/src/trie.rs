use std::{cmp::Ordering, collections::HashMap, mem};

#[derive(Debug, Default)]
pub struct Trie {
    root: Node,
}

#[derive(Debug, Default)]
struct Node {
    terminal: bool,
    children: HashMap<char, Edge>,
}

impl Node {
    fn terminal() -> Self {
        Self {
            terminal: true,
            children: HashMap::new(),
        }
    }

    fn nonterminal() -> Self {
        Self {
            terminal: false,
            children: HashMap::new(),
        }
    }

    fn add_edge(&mut self, text: String, node: Node) {
        self.children
            .insert(first_char(&text), Edge { text, to: node });
    }
}

#[derive(Debug)]
struct Edge {
    text: String,
    to: Node,
}

impl Trie {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, word: &str) {
        let mut node = &mut self.root;

        let mut i = 0;
        while i < word.len() {
            let current = &word[i..];
            let first = first_char(current);

            let edge = if node.children.contains_key(&first) {
                node.children.get_mut(&first).unwrap()
            } else {
                // Case 1
                node.add_edge(current.to_owned(), Node::terminal());
                break;
            };

            // Case 3
            if let Some(mismatch_idx) = current
                .char_indices()
                .zip(edge.text.char_indices())
                .find_map(|(c, u)| if c.1 != u.1 { Some(u.0) } else { None })
            {
                let suffix = edge.text[mismatch_idx..].to_owned();
                edge.text = edge.text[..mismatch_idx].to_owned();
                let old = mem::replace(&mut edge.to, Node::nonterminal());
                edge.to.add_edge(suffix, old);
                node = &mut edge.to;
                i += mismatch_idx;
                continue;
            }

            match current.len().cmp(&edge.text.len()) {
                // Case 3
                Ordering::Equal => {
                    edge.to.terminal = true;
                    break;
                }
                // Case 4
                Ordering::Less => {
                    let suffix = edge.text[current.len()..].to_owned();
                    edge.text = current.to_owned();
                    let old_to = mem::replace(&mut edge.to, Node::terminal());
                    edge.to.add_edge(suffix, old_to);
                    break;
                }
                // Case 2
                Ordering::Greater => {
                    i += edge.text.len();
                    node = &mut edge.to;
                }
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        self.root.children.is_empty()
    }

    pub fn lookup(&self, word: &str) -> bool {
        let mut current = &self.root;
        let mut i = 0;
        while i < word.len() {
            let word = &word[i..];
            let first = first_char(word);
            if let Some(edge) = current.children.get(&first) {
                current = &edge.to;
                i += edge.text.len();
            } else {
                return false;
            }
        }

        true
    }

    pub fn prefix_list(&self, prefix: &str) -> Vec<String> {
        let mut traversed_word = String::new();
        let mut current = &self.root;
        let mut i = 0;
        while i < prefix.len() {
            let word = &prefix[i..];
            let first = first_char(word);
            if let Some(edge) = current.children.get(&first) {
                current = &edge.to;
                traversed_word.push_str(&edge.text);
                i += edge.text.len();
            } else {
                return vec![];
            }
        }

        let mut matches = vec![];
        let mut stack = vec![(current, traversed_word)];
        while let Some((node, word)) = stack.pop() {
            if node.terminal {
                matches.push(word.clone());
            }
            for edge in node.children.values() {
                let mut word = word.clone();
                word.push_str(&edge.text);
                stack.push((&edge.to, word));
            }
        }
        matches.sort();

        matches
    }
}

impl FromIterator<String> for Trie {
    fn from_iter<T: IntoIterator<Item = String>>(iter: T) -> Self {
        let mut trie = Self::new();
        for word in iter {
            trie.insert(&word);
        }
        trie
    }
}

fn first_char(s: &str) -> char {
    s.chars().next().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn suggest_words_with_common_prefix() {
        let mut trie = Trie::new();
        trie.insert("hello");
        trie.insert("hell");
        trie.insert("help");
        trie.insert("helping");
        trie.insert("her");

        assert_eq!(
            vec![
                "hell".to_owned(),
                "hello".to_owned(),
                "help".to_owned(),
                "helping".to_owned()
            ],
            trie.prefix_list("hel"),
        );
        assert_eq!(vec!["hello".to_owned()], trie.prefix_list("hello"));
        assert_eq!(
            vec![
                "hell".to_owned(),
                "hello".to_owned(),
                "help".to_owned(),
                "helping".to_owned(),
                "her".to_owned(),
            ],
            trie.prefix_list("he")
        );
    }

    #[test]
    fn trie_no_suggestions_on_irrelevant() {
        let mut trie = Trie::new();
        trie.insert("nice");

        assert!(trie.prefix_list("hello").is_empty());
    }

    #[test]
    fn empty_trie() {
        let trie = Trie::new();
        assert!(trie.prefix_list("hello").is_empty());
    }

    #[test]
    fn directory() {
        let mut trie = Trie::new();
        trie.insert("README.md");
        trie.insert("args.wasm");
        trie.insert("env.wasm");
        trie.insert("fib.wasm");
        trie.insert("hello_wasi.wasm");
        trie.insert("new_dir.wasm");
        trie.insert("new_file.wasm");
        trie.insert("read_dir.wasm");
        trie.insert("rename.wasm");
        trie.insert("rm.wasm");
        trie.insert("rm_dir.wasm");
        trie.insert("stdin.wasm");
        dbg!(&trie);

        assert_eq!(vec!["read_dir.wasm".to_owned()], trie.prefix_list("rea"));
    }
}
