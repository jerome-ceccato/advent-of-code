import sys.io.File;

class Main {
	static public function main() {
		var input = File.getContent('input');
		var lines = input.split('\n');

		var part1 = reduceCells(lines, findAllXmas);
		var part2 = reduceCells(lines, findMas);
		trace('$part1\n$part2');
	}

	static function get(input:Array<String>, x:Int, y:Int) {
		if (y >= 0 && y < input.length) {
			return input[y].charAt(x);
		}
		return "";
	}

	static function hasXmas(input:Array<String>, x:Int, y:Int, dx:Int, dy:Int):Bool {
		var target = "XMAS";
		for (i in 0...target.length) {
			if (get(input, x + (dx * i), y + (dy * i)) != target.charAt(i)) {
				return false;
			}
		}
		return true;
	}

	static function findAllXmas(input:Array<String>, x:Int, y:Int):Int {
		var found = 0;
		for (dy in -1...2) {
			for (dx in -1...2) {
				if (hasXmas(input, x, y, dx, dy)) {
					found++;
				}
			}
		}
		return found;
	}

	static function findMas(input:Array<String>, x:Int, y:Int):Int {
		if (get(input, x, y) == 'A') {
			var offsets = [
				{left: {x: -1, y: -1}, right: {x: 1, y: 1}},
				{left: {x: 1, y: -1}, right: {x: -1, y: 1}},
			];

			var targets = ['MS', 'SM'];

			for (offset in offsets) {
				var found = false;
				for (target in targets) {
					if (get(input, x + offset.left.x, y + offset.left.y) == target.charAt(0)
						&& get(input, x + offset.right.x, y + offset.right.y) == target.charAt(1)) {
						found = true;
					}
				}
				if (!found) {
					return 0;
				}
			}
			return 1;
		}
		return 0;
	}

	static function reduceCells(input:Array<String>, fct:(Array<String>, Int, Int) -> Int):Int {
		var acc = 0;
		for (y in 0...input.length) {
			for (x in 0...input[y].length) {
				acc += fct(input, x, y);
			}
		}
		return acc;
	}
}
