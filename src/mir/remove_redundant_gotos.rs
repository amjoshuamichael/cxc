use ahash::{HashMap, HashMapExt};

use super::{MIR, MLine};

pub fn remove_redundant_gotos(mir: &mut MIR) {
    //use MLine::*;
    //let mut removal_spots = Vec::new();

    //let mut pos_remap = HashMap::<u32, u32>::new();
    //let mut running_count = 0;
    //let mut create_remap = |x| { 
    //    pos_remap.insert(x, running_count); 
    //    running_count += 1;
    //};

    //for l in 0..mir.lines.len() {
    //    if let Marker(ml) = mir.lines[l] {
    //        create_remap(ml);
    //    }

    //    if let Goto(gl) = mir.lines[l] && let Marker(ml) = mir.lines[l + 1] && gl == ml {
    //        removal_spots.push(l);
    //    }
    //}

    //for spot in removal_spots.into_iter().rev() {
    //    mir.lines.remove(spot);
    //}

    //let mut l = mir.lines.len() - 1;
    //while l > 0 {
    //    l -= 1;

    //    if let Marker(from) = mir.lines[l] && let Marker(to) = mir.lines[l + 1] {
    //        pos_remap.insert(from, pos_remap[&to]);
    //        mir.lines.remove(l);
    //    }
    //}

    //for line in &mut mir.lines {
    //    if let Goto(l) | Marker(l) = line {
    //        *l = pos_remap[l];
    //    }

    //    if let Branch { yes, no, .. } = line {
    //        *yes = pos_remap[&*yes];
    //        *no = pos_remap[&*no];
    //    }
    //}
}
