use crate::{cfg::Cfg, dfa::uda::GlobalLocation, AnyReg, BlockId};
use std::collections::{BTreeMap, BTreeSet, HashSet};

/// Returns the alias for `reg` for each block. If a block is not in this map, `reg` is not
/// aliased within that block. For the block specified by `location`, it might be the case that uses
/// above `location` are not aliased (i.e. still `reg`), while uses from `location` onwards have
/// been aliased. In this case, the block specified by `location` will not be present in the
/// returned map. The first element in the returned tuple will always be the original alias used
/// from `location` onwards.
pub fn alias_reg_from(
    cfg: &mut Cfg,
    reg: AnyReg,
    location: GlobalLocation,
) -> (AnyReg, BTreeMap<BlockId, AnyReg>) {
    let du_chain = &cfg.du_chains()[reg];
    let live_sets = cfg.live_sets();
    let mut var_generator = cfg.var_generator();
    let mut new_alias = || var_generator.next_of_type(reg);

    let alias = new_alias();

    // Collect all blocks in which `reg` is live that are reachable from `block_id`. This may
    // include `block_id`, but only if it is reachable from itself.
    let mut affected = BTreeSet::new();
    let mut visited = BTreeSet::new();
    let mut to_visit = BTreeSet::from([location.block_id]);
    while let Some(block_id) = to_visit.pop_first() {
        visited.insert(block_id);
        for succ_id in cfg.successor_ids(block_id) {
            if live_sets[succ_id].live_ins.contains(&reg)
                && du_chain.def_block() != succ_id
                && affected.insert(succ_id)
                && !visited.contains(&succ_id)
            {
                to_visit.insert(succ_id);
            }
        }
    }

    // Assign each block the same alias initially.
    let mut aliases =
        BTreeMap::from_iter(affected.iter().map(|&block_id| (block_id, (alias, false))));

    let mut changing = true;
    while changing {
        changing = false;
        for &block_id in &affected {
            let mut pred_aliases =
                HashSet::<_>::from_iter(cfg.predecessor_ids(block_id).map(|pred_id| {
                    if pred_id == location.block_id {
                        alias
                    } else {
                        aliases.get(&pred_id).map(|(r, _)| *r).unwrap_or(reg)
                    }
                }));
            let (block_alias, is_block_alias_original) = aliases[&block_id];
            let original_len = pred_aliases.len();
            pred_aliases.remove(&block_alias);
            // Check the number of unique pred aliases (excl. the block's own alias)
            match (original_len, pred_aliases.len()) {
                (0, _) => {
                    unreachable!("a block must have at least one pred to be in `affected`")
                }
                (_, 0) => {}
                (1, _) => {
                    let pred_alias = pred_aliases.into_iter().next().unwrap();
                    aliases.insert(block_id, (pred_alias, false));
                    changing = true;
                }
                _ => {
                    if !is_block_alias_original {
                        aliases.insert(block_id, (new_alias(), true));
                        changing = true;
                    }
                }
            }
        }
    }

    // Now insert the 'phi instructions' and replace every use of `reg` by the alias assigned to
    // the relevant block.

    for instr in &mut cfg[location.block_id].instructions[0.max(location.local.0) as usize..] {
        instr.map_uses(|r| if r == reg { alias } else { r });
    }
    cfg[location.block_id]
        .terminator_mut()
        .map_all_uses(|r| if r == reg { alias } else { r });

    for block_id in affected {
        let (block_alias, _) = aliases[&block_id];
        let mut pred_same_alias = true;
        for pred_id in cfg.predecessor_ids(block_id).collect::<Vec<_>>() {
            let pred_alias = if pred_id == location.block_id {
                alias
            } else {
                aliases.get(&pred_id).map(|(r, _)| *r).unwrap_or(reg)
            };
            if pred_alias != block_alias {
                cfg[pred_id]
                    .successor_mut(block_id)
                    .unwrap()
                    .arguments
                    .push(pred_alias);
                pred_same_alias = false;
            }
        }
        let block = &mut cfg[block_id];
        if !pred_same_alias {
            block.arguments.push(block_alias);
        }
        for instr in &mut block.instructions {
            instr.map_uses(|r| if r == reg { block_alias } else { r });
        }
        block
            .terminator_mut()
            .map_all_uses(|r| if r == reg { block_alias } else { r });
    }

    (
        alias,
        aliases.into_iter().map(|(b, (r, _))| (b, r)).collect(),
    )
}
