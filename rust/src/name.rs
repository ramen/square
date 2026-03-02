use std::collections::HashMap;
use std::sync::Mutex;

/// An interned name, represented as an integer for fast comparison and hashing.
/// Mirrors the OCaml `Name` module which maps strings to ints for efficient identity.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Name(u32);

impl std::fmt::Debug for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        with_interner(|i| write!(f, "Name(\"{}\")", i.resolve(self.0)))
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        with_interner(|i| write!(f, "{}", i.resolve(self.0)))
    }
}

struct Interner {
    string_to_id: HashMap<String, u32>,
    id_to_string: Vec<String>,
}

impl Interner {
    fn new() -> Self {
        Interner {
            string_to_id: HashMap::new(),
            id_to_string: Vec::new(),
        }
    }

    fn intern(&mut self, s: &str) -> u32 {
        if let Some(&id) = self.string_to_id.get(s) {
            return id;
        }
        let id = self.id_to_string.len() as u32;
        self.id_to_string.push(s.to_string());
        self.string_to_id.insert(s.to_string(), id);
        id
    }

    fn resolve(&self, id: u32) -> &str {
        &self.id_to_string[id as usize]
    }
}

static INTERNER: Mutex<Option<Interner>> = Mutex::new(None);

fn with_interner<T>(f: impl FnOnce(&mut Interner) -> T) -> T {
    let mut guard = INTERNER.lock().unwrap();
    let interner = guard.get_or_insert_with(Interner::new);
    f(interner)
}

impl Name {
    pub fn new(s: &str) -> Self {
        Name(with_interner(|i| i.intern(s)))
    }

    #[allow(clippy::inherent_to_string)]
    pub fn to_string(self) -> String {
        with_interner(|i| i.resolve(self.0).to_owned())
    }

    #[allow(dead_code)]
    pub fn as_str_with<T>(self, f: impl FnOnce(&str) -> T) -> T {
        with_interner(|i| f(i.resolve(self.0)))
    }
}
