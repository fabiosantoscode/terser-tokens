use std::collections::BTreeMap;

use crate::basic_blocks::{BreakableId, StructuredFlow, StructuredModule};

use super::{generate_phi_nodes, remove_phi_module};

pub fn normalize_module(module: &mut StructuredModule) {
    for (_, block_group) in module.iter_mut() {
        let blocks = std::mem::take(&mut block_group.blocks);
        let mut blocks = StructuredFlow::from_vec(blocks).simplify();
        normalize_flow(&mut blocks);
        block_group.blocks = vec![blocks];
    }

    // TODO: normalize function IDs

    remove_phi_module(module);
    generate_phi_nodes(module);
}

fn normalize_flow(flow: &mut StructuredFlow) {
    let mut brk_targets = flow.count_all_break_targets();
    normalize_flow_inner(flow, &mut brk_targets);
}

fn normalize_flow_inner(flow: &mut StructuredFlow, brk_targets: &mut BTreeMap<BreakableId, usize>) {
    match flow {
        StructuredFlow::Instruction(_varname, _ins) => {}
        StructuredFlow::Block(brk, v) => {
            normalize_flow_vec(brk, v, brk_targets, true);
        }
        StructuredFlow::Loop(brk, v) => {
            normalize_flow_vec(brk, v, brk_targets, true);
        }
        StructuredFlow::ForInOfLoop(brk, _, _, v) => {
            normalize_flow_vec(brk, v, brk_targets, true);
        }
        StructuredFlow::Cond(brk, _, cons, alt) => {
            normalize_flow_vec(brk, cons, brk_targets, true);
            normalize_flow_vec(brk, alt, brk_targets, true);

            if cons.is_empty() && alt.is_empty() {
                *flow = StructuredFlow::Block(*brk, vec![]);
            } else if cons == alt {
                *flow = StructuredFlow::Block(*brk, std::mem::take(cons));
            }
        }
        StructuredFlow::Switch(brk, _, cases) => {
            for c in cases.iter_mut() {
                normalize_flow_vec(brk, &mut c.body, brk_targets, false);
            }
        }
        StructuredFlow::Class(_, class_body) => {
            for item in class_body.iter_mut() {
                for flows in item.flows_mut() {
                    let mut no_brk = BreakableId(None);
                    normalize_flow_vec(&mut no_brk, flows, brk_targets, false);
                }
            }
        }
        StructuredFlow::TryCatch(brk, try_, catch, finally) => {
            normalize_flow_vec(brk, try_, brk_targets, true);
            normalize_flow_vec(brk, catch, brk_targets, true);
            normalize_flow_vec(brk, finally, brk_targets, true);

            if try_.is_empty() && finally.is_empty() {
                *flow = StructuredFlow::Block(*brk, vec![]);
            }
        }
        StructuredFlow::Break(_) => {}
        StructuredFlow::Continue(_) => {}
        StructuredFlow::Return(_, _) => {}
        StructuredFlow::Debugger => {}
    }
}

fn normalize_flow_vec(
    brk: &mut BreakableId,
    v: &mut Vec<StructuredFlow>,
    brk_targets: &mut BTreeMap<BreakableId, usize>,
    remove_breaks: bool,
) {
    let mut to_remove = vec![];

    for (i, flow) in v.iter_mut().enumerate() {
        normalize_flow_inner(flow, brk_targets);

        match flow {
            StructuredFlow::Break(b) => {
                if b == brk && remove_breaks {
                    to_remove.extend(i..v.len());
                    break;
                }
            }
            StructuredFlow::Continue(c) => {
                if c == brk {
                    to_remove.extend(i..v.len());
                    break;
                }
            }
            StructuredFlow::Block(_, items) if items.is_empty() => {
                to_remove.push(i);
            }
            _ => {}
        }

        if flow.aborts() {
            to_remove.extend((i + 1)..v.len());
            break;
        }
    }

    for i in to_remove.into_iter().rev() {
        let removed = v.remove(i);

        if let Some(breaks_to) = removed.breaks_to_id() {
            if let Some(count) = brk_targets.get_mut(&breaks_to) {
                *count -= 1;

                if *count == 0 {
                    brk_targets.remove(&breaks_to);
                }
            }
        }
    }

    if brk.0.is_some() && brk_targets.get(&brk).is_none() {
        brk.0 = None;
    }
}
