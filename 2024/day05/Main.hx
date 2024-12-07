import sys.io.File;

enum PageOrdering {
	before;
	after;
	unknown;
}

class Main {
	static public function main() {
		var input = File.getContent('input');
		var sections = input.split('\n\n');
		var rawRules = sections[0].split('\n').map(s -> {
			var numbers = s.split('|').map(n -> Std.parseInt(n));
			return {before: numbers[0], after: numbers[1]};
		});
		var updates = sections[1].split('\n').map(s -> {
			s.split(',').map(n -> Std.parseInt(n));
		});

		var compiledRules = compileRules(rawRules);

		trace(part1(compiledRules, updates));
		trace(part2(compiledRules, updates));
	}

	static function part1(rules:Map<Int, {before:Array<Int>, after:Array<Int>}>, updates:Array<Array<Int>>):Int {
		var total = 0;
		for (update in updates) {
			if (isUpdateCorrectlyOrdered(rules, update)) {
				total += middlePageNumber(update);
			}
		}
		return total;
	}

	static function part2(rules:Map<Int, {before:Array<Int>, after:Array<Int>}>, updates:Array<Array<Int>>):Int {
		var total = 0;
		for (update in updates) {
			if (!isUpdateCorrectlyOrdered(rules, update)) {
				total += middlePageNumber(reorderUpdate(rules, update));
			}
		}
		return total;
	}

	static function middlePageNumber(update:Array<Int>):Int {
		return update[Std.int(update.length / 2)];
	}

	static function compileRules(rules:Array<{before:Int, after:Int}>):Map<Int, {before:Array<Int>, after:Array<Int>}> {
		var result:Map<Int, {before:Array<Int>, after:Array<Int>}> = [];

		for (rule in rules) {
			if (!result.exists(rule.before)) {
				result.set(rule.before, getAllConstraints(rules, rule.before));
			}

			if (!result.exists(rule.after)) {
				result.set(rule.after, getAllConstraints(rules, rule.after));
			}
		}
		return result;
	}

	static function getAllConstraints(rawRules:Array<{before:Int, after:Int}>, item:Int):{before:Array<Int>, after:Array<Int>} {
		var allBefore:Array<Int> = [];
		var allAfter:Array<Int> = [];

		for (rule in rawRules) {
			if (item == rule.before) {
				allAfter.push(rule.after);
			} else if (item == rule.after) {
				allBefore.push(rule.before);
			}
		}
		return {before: allBefore, after: allAfter};
	}

	static function getOrdering(rules:Map<Int, {before:Array<Int>, after:Array<Int>}>, lhs:Int, rhs:Int):PageOrdering {
		if (rules[lhs].before.contains(rhs)) {
			return PageOrdering.before;
		} else if (rules[lhs].after.contains(rhs)) {
			return PageOrdering.after;
		} else {
			return PageOrdering.unknown;
		}
	}

	static function isUpdateCorrectlyOrdered(rules:Map<Int, {before:Array<Int>, after:Array<Int>}>, update:Array<Int>):Bool {
		for (i in 0...update.length) {
			for (left in 0...i) {
				if (getOrdering(rules, update[i], update[left]) == PageOrdering.after)
					return false;
			}

			for (right in (i + 1)...update.length) {
				if (getOrdering(rules, update[i], update[right]) == PageOrdering.before)
					return false;
			}
		}
		return true;
	}

	static function swap(update:Array<Int>, i:Int, j:Int):Array<Int> {
		var tmp = update[i];
		update[i] = update[j];
		update[j] = tmp;
		return update;
	}

	static function reorderUpdate(rules:Map<Int, {before:Array<Int>, after:Array<Int>}>, update:Array<Int>):Array<Int> {
		for (i in 0...update.length) {
			for (left in 0...i) {
				if (getOrdering(rules, update[i], update[left]) == PageOrdering.after) {
					return reorderUpdate(rules, swap(update, i, left));
				}
			}

			for (right in (i + 1)...update.length) {
				if (getOrdering(rules, update[i], update[right]) == PageOrdering.before) {
					return reorderUpdate(rules, swap(update, i, right));
				}
			}
		}
		return update;
	}
}
