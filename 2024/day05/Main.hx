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
		var rules = sections[0].split('\n').map(s -> {
			var numbers = s.split('|').map(n -> Std.parseInt(n));
			return {before: numbers[0], after: numbers[1]};
		});
		var updates = sections[1].split('\n').map(s -> {
			s.split(',').map(n -> Std.parseInt(n));
		});

		trace(part1(rules, updates));
	}

	static function part1(rules:Array<{before:Int, after:Int}>, updates:Array<Array<Int>>):Int {
		var total = 0;
		for (update in updates) {
			if (isUpdateCorrectlyOrdered(rules, update)) {
				total += middlePageNumber(update);
			}
		}
		return total;
	}

	static function middlePageNumber(update:Array<Int>):Int {
		return update[Std.int(update.length / 2)];
	}

	static function getAllConstraints(rules:Array<{before:Int, after:Int}>, item:Int):{before:Array<Int>, after:Array<Int>} {
		var allBefore:Array<Int> = [];
		var allAfter:Array<Int> = [];

		for (rule in rules) {
			if (item == rule.before) {
				allAfter.push(rule.after);
			} else if (item == rule.after) {
				allBefore.push(rule.before);
			}
		}
		return {before: allBefore, after: allAfter};
	}

	static function getOrdering(rules:Array<{before:Int, after:Int}>, lhs:Int, rhs:Int):PageOrdering {
		var lhsConstraints = getAllConstraints(rules, lhs);

		if (lhsConstraints.before.contains(rhs)) {
			// trace('$rhs comes before $lhs');
			return PageOrdering.before;
		} else if (lhsConstraints.after.contains(rhs)) {
			// trace('$rhs comes after $lhs');
			return PageOrdering.after;
		} else {
			// trace('$lhs and $rhs aren\'t ordered');
			return PageOrdering.unknown;
		}
	}

	static function isUpdateCorrectlyOrdered(rules:Array<{before:Int, after:Int}>, update:Array<Int>):Bool {
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
}
