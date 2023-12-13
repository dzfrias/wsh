use std::collections::HashMap;

use petgraph::prelude::*;
use shwasi_parser::Instruction;

use crate::{Addr, Func, Instance, Store};

#[derive(Debug)]
pub struct CallGraph {
    graph: Graph<Addr<Func>, ()>,
}

impl CallGraph {
    pub fn construct(module: Instance, store: &Store) -> Self {
        let mut g = Graph::new();
        let mut nodes = HashMap::new();

        for func_addr in module.func_addrs() {
            let f_idx = *nodes
                .entry(*func_addr)
                .or_insert_with(|| g.add_node(*func_addr));
            let func = &store.functions[*func_addr];
            match func {
                Func::Host(_) => {}
                Func::Module(func) => {
                    for instr in &func.code.body {
                        let Instruction::Call { func_idx } = instr else {
                            continue;
                        };
                        let other_addr = module.func_addrs()[func_idx as usize];
                        if let Some(other_idx) = nodes.get(&other_addr) {
                            g.add_edge(f_idx, *other_idx, ());
                        } else {
                            let other_idx = g.add_node(other_addr);
                            nodes.insert(other_addr, other_idx);
                            g.add_edge(f_idx, other_idx, ());
                        }
                    }
                }
            }
        }

        Self { graph: g }
    }

    pub fn in_cycle(&self, func_addr: Addr<Func>) -> bool {
        let sccs = petgraph::algo::tarjan_scc(&self.graph);
        for scc in sccs {
            if scc.len() == 1 {
                continue;
            }
            if scc
                .iter()
                .any(|idx| self.graph.node_weight(*idx).unwrap() == &func_addr)
            {
                return true;
            }
        }

        false
    }
}
