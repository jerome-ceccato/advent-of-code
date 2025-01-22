use std::{
    collections::{HashMap, HashSet},
    fs,
};

use regex::Regex;

#[derive(Debug, PartialEq, Eq, Hash)]
enum DamageType {
    Slashing,
    Bludgeoning,
    Radiation,
    Fire,
    Cold,
}

impl DamageType {
    fn from(raw: &str) -> Option<DamageType> {
        match raw {
            "slashing" => Some(DamageType::Slashing),
            "bludgeoning" => Some(DamageType::Bludgeoning),
            "radiation" => Some(DamageType::Radiation),
            "fire" => Some(DamageType::Fire),
            "cold" => Some(DamageType::Cold),
            _ => return None,
        }
    }
}

#[derive(Debug)]
struct UnitStack {
    id: i32,
    amount: i32,
    hp: i32,
    damage: i32,
    damage_type: DamageType,
    weakness_table: HashMap<DamageType, i32>,
    initiative: i32,
}

impl UnitStack {
    fn effective_power(&self) -> i32 {
        self.amount * self.damage
    }

    fn received_damage(&self, attacker: &UnitStack) -> i32 {
        attacker.effective_power() * self.weakness_table[&attacker.damage_type]
    }
}

fn parse_weakness(raw: &str) -> HashMap<DamageType, i32> {
    let default_weakness_table = HashMap::from([
        (DamageType::Slashing, 1),
        (DamageType::Bludgeoning, 1),
        (DamageType::Radiation, 1),
        (DamageType::Fire, 1),
        (DamageType::Cold, 1),
    ]);

    if raw.is_empty() {
        return default_weakness_table;
    }
    let mut table = default_weakness_table;
    let re = Regex::new(r"\((weak|immune) to ([^;)]+)(?:; )?(weak|immune)?(?: to )?([^;)]+)?\)")
        .unwrap();
    let caps = re.captures(raw).unwrap();

    let groups = vec![
        (&caps[1], &caps[2]),
        (
            &caps.get(3).map_or("", |m| m.as_str()),
            &caps.get(4).map_or("", |m| m.as_str()),
        ),
    ];
    for (weakness_type, damage_types) in groups {
        if !weakness_type.is_empty() {
            let multiplier = match weakness_type {
                "weak" => 2,
                "immune" => 0,
                _ => 1,
            };
            for damage_raw in damage_types.split(", ") {
                if let Some(dmg) = DamageType::from(damage_raw) {
                    *table.entry(dmg).or_default() = multiplier;
                }
            }
        }
    }

    table
}

fn parse_unit(line: &str, id: &mut i32) -> UnitStack {
    let main_re = Regex::new(r"(\d+) units each with (\d+) hit points (\([^)]+\))? ?with an attack that does (\d+) (\w+) damage at initiative (\d+)").unwrap();
    let caps = main_re.captures(line).unwrap();

    *id += 1;
    UnitStack {
        id: *id,
        amount: caps[1].parse().unwrap(),
        hp: caps[2].parse().unwrap(),
        damage: caps[4].parse().unwrap(),
        damage_type: DamageType::from(&caps[5]).unwrap(),
        weakness_table: parse_weakness(caps.get(3).map_or("", |m| m.as_str())),
        initiative: caps[6].parse().unwrap(),
    }
}

fn get_input(path: &str) -> (Vec<UnitStack>, Vec<UnitStack>) {
    let contents = fs::read_to_string(path).unwrap();
    let mut unit_id = 0;
    let mut teams = contents
        .split("\n\n")
        .map(|team| {
            team.split("\n")
                .skip(1)
                .map(|line| parse_unit(line, &mut unit_id))
                .collect()
        })
        .collect::<Vec<Vec<UnitStack>>>();

    // We have to pop(move) them because UnitStack is not copyable
    let infection = teams.pop().unwrap();
    let immune = teams.pop().unwrap();
    (immune, infection)
}

#[derive(Debug)]
struct Battle {
    // Global lookup that holds all stacks, teams are just id sets
    units: HashMap<i32, UnitStack>,
    immune_system: HashSet<i32>,
    infection: HashSet<i32>,
}

impl Battle {
    fn from(immune_system: Vec<UnitStack>, infection: Vec<UnitStack>) -> Battle {
        let immune_units: HashSet<i32> = immune_system.iter().map(|u| u.id).collect();
        let infection_units: HashSet<i32> = infection.iter().map(|u| u.id).collect();

        let mut unit_lookup: HashMap<i32, UnitStack> = HashMap::new();
        for u in immune_system.into_iter() {
            unit_lookup.insert(u.id, u);
        }
        for u in infection.into_iter() {
            unit_lookup.insert(u.id, u);
        }
        Battle {
            units: unit_lookup,
            immune_system: immune_units,
            infection: infection_units,
        }
    }

    fn select_targets(&self) -> Vec<(i32, i32)> {
        let mut all_units_id: Vec<i32> = self
            .immune_system
            .iter()
            .chain(self.infection.iter())
            .cloned()
            .collect();
        all_units_id.sort_by(|lhsi, rhsi| {
            let lhs = &self.units[lhsi];
            let rhs = &self.units[rhsi];

            if lhs.effective_power() != rhs.effective_power() {
                lhs.effective_power().cmp(&rhs.effective_power()).reverse()
            } else {
                lhs.initiative.cmp(&rhs.initiative).reverse()
            }
        });

        let mut result: Vec<(i32, i32)> = vec![];
        let mut targetable_immune = self.immune_system.clone();
        let mut targetable_infection = self.infection.clone();
        for unit_id in all_units_id {
            let unit = &self.units[&unit_id];
            let other_group = if self.immune_system.contains(&unit_id) {
                &mut targetable_infection
            } else {
                &mut targetable_immune
            };

            let mut all_targets = other_group.iter().cloned().collect::<Vec<i32>>();
            all_targets.sort_by(|lhsi, rhsi| {
                let lhs = &self.units[lhsi];
                let rhs = &self.units[rhsi];

                if lhs.received_damage(unit) != rhs.received_damage(unit) {
                    lhs.received_damage(unit)
                        .cmp(&rhs.received_damage(unit))
                        .reverse()
                } else if lhs.effective_power() != rhs.effective_power() {
                    lhs.effective_power().cmp(&rhs.effective_power()).reverse()
                } else {
                    lhs.initiative.cmp(&rhs.initiative).reverse()
                }
            });

            if let Some(target_id) = all_targets.first() {
                if self.units[target_id].received_damage(unit) > 0 {
                    result.push((unit_id, *target_id));
                    other_group.remove(target_id);
                }
            }
        }

        result
    }

    fn run(&mut self) {
        while !self.immune_system.is_empty() && !self.infection.is_empty() {
            let mut target_selection = self.select_targets();
            target_selection.sort_by_key(|p| self.units[&p.0].initiative);
            target_selection.reverse();

            let mut tie = true;
            for (attacker_id, defender_id) in target_selection {
                let attacker = &self.units[&attacker_id];
                let defender = &self.units[&defender_id];

                if attacker.amount <= 0 {
                    continue;
                }

                let defender_losses = defender.received_damage(attacker) / defender.hp;
                if defender_losses > 0 {
                    let dies = defender.amount <= defender_losses;
                    if dies {
                        self.units.get_mut(&defender_id).unwrap().amount = 0;
                        self.immune_system.remove(&defender_id);
                        self.infection.remove(&defender_id);
                    } else {
                        self.units.get_mut(&defender_id).unwrap().amount -= defender_losses;
                    }
                    tie = false;
                }
            }
            if tie {
                return;
            }
        }
    }

    fn boost_immune_system(&mut self, value: i32) {
        for u in self.units.values_mut() {
            if self.immune_system.contains(&u.id) {
                u.damage += value;
            }
        }
    }

    fn units_left(&self) -> i32 {
        self.units.values().map(|unit| unit.amount).sum()
    }

    fn has_immune_system_won(&self) -> bool {
        self.infection.is_empty()
    }
}

fn resolved_battle(boost: i32) -> Battle {
    let (immune_system, infection) = get_input("input");
    let mut battle = Battle::from(immune_system, infection);
    battle.boost_immune_system(boost);
    battle.run();
    battle
}

fn find_smallest_boost() -> i32 {
    let mut low = 0;
    let mut high = 100_000;

    while low + 1 < high {
        let mid = low + (high - low) / 2;
        let battle = resolved_battle(mid);
        if battle.has_immune_system_won() {
            high = mid;
        } else {
            low = mid;
        }
    }

    low + 1
}

fn main() {
    let part1 = resolved_battle(0).units_left();
    println!("{}", part1);

    let part2 = resolved_battle(find_smallest_boost()).units_left();
    println!("{}", part2);
}
