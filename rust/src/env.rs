use std::cell::RefCell;
use std::rc::Rc;

use crate::name::Name;
use crate::names::NameMap;
use crate::value::Value;

/// An environment is a mapping from names to mutable value cells.
/// Uses Rc<RefCell<>> to mirror OCaml's ref cells for mutable bindings.
/// The environment itself is an Rc<RefCell<>> to allow sharing and mutation.
pub type Env = Rc<RefCell<EnvInner>>;

pub struct EnvInner {
    bindings: NameMap<Rc<RefCell<Value>>>,
}

impl EnvInner {
    pub fn new() -> Self {
        EnvInner {
            bindings: NameMap::new(),
        }
    }

    pub fn get(&self, name: Name) -> Option<Rc<RefCell<Value>>> {
        self.bindings.get(&name).cloned()
    }

    pub fn define(&mut self, name: Name, value: Value) {
        self.bindings.insert(name, Rc::new(RefCell::new(value)));
    }

    #[allow(dead_code)]
    pub fn set(&mut self, name: Name, cell: Rc<RefCell<Value>>) {
        self.bindings.insert(name, cell);
    }

    pub fn remove(&mut self, name: Name) {
        self.bindings.remove(&name);
    }

    pub fn clone_bindings(&self) -> NameMap<Rc<RefCell<Value>>> {
        self.bindings.clone()
    }

    pub fn to_value(&self) -> Value {
        let mut map = NameMap::new();
        for (k, v) in &self.bindings {
            map.insert(*k, v.borrow().clone());
        }
        Value::Record(crate::names::Names::record(), map)
    }
}

pub fn new_env() -> Env {
    Rc::new(RefCell::new(EnvInner::new()))
}

pub fn child_env(parent: &Env) -> Env {
    let mut inner = EnvInner::new();
    inner.bindings = parent.borrow().clone_bindings();
    Rc::new(RefCell::new(inner))
}

pub fn add_binding(env: &Env, name: &str, value: Value) {
    env.borrow_mut().define(Name::new(name), value);
}
