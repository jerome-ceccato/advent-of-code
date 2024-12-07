import haxe.ds.HashMap;
import sys.io.File;

using StringTools;

class Point {
	public var x:Int;
	public var y:Int;

	public function new(x:Int, y:Int) {
		this.x = x;
		this.y = y;
	}

	@:op(A == B) // Overloading doesn't actually work
	static public function equals(lhs:Point, rhs:Point):Bool {
		return lhs.x == rhs.x && lhs.y == rhs.y;
	}

	// can't be computed automatically even for trivial classes
	public function hashCode():Int {
		return x + 10000 * y;
	}

	public function toString() {
		return "(" + x + "," + y + ")";
	}

	public function isInBounds(map:Array<String>):Bool {
		return this.y >= 0 && this.y < map.length && this.x >= 0 && this.x < map[this.y].length;
	}

	public function copy():Point {
		return new Point(this.x, this.y);
	}

	public function move(direction:Direction) {
		switch (direction) {
			case up:
				this.y--;
			case down:
				this.y++;
			case left:
				this.x--;
			case right:
				this.x++;
		}
	}

	public function moved(direction:Direction):Point {
		var next = this.copy();
		next.move(direction);
		return next;
	}

	public function isIn(array:Array<Point>):Bool {
		for (a in array) {
			if (Point.equals(this, a)) {
				return true;
			}
		}
		return false;
	}
}

enum Direction {
	up;
	right;
	down;
	left;
}

class Main {
	static public function main() {
		var input = File.getContent('input');
		var lines = input.split('\n');
		var guard = extractGuardPosition(lines);

		var visited = getVisitedCells(lines, guard.copy());
		// printMap(lines, visited);
		trace(visited.length);
		trace(tryObstuctions(lines, guard, visited));
	}

	static function printMap(map:Array<String>, visited:Array<Point>) {
		var output = "\n";
		for (y in 0...map.length) {
			for (x in 0...map[y].length) {
				if ((new Point(x, y)).isIn(visited)) {
					output += "X";
				} else {
					output += map[y].charAt(x);
				}
			}
			output += "\n";
		}
		trace(output);
	}

	static function extractGuardPosition(map:Array<String>):Point {
		for (y in 0...map.length) {
			for (x in 0...map[y].length) {
				if (map[y].charAt(x) == '^') {
					map[y] = map[y].replace('^', '.');
					return new Point(x, y);
				}
			}
		}
		return new Point(-1, -1);
	}

	// Enums don't support methods
	static function rotated90(direction:Direction):Direction {
		switch (direction) {
			case up:
				return Direction.right;
			case right:
				return Direction.down;
			case down:
				return Direction.left;
			case left:
				return Direction.up;
		}
	}

	static function getVisitedCells(map:Array<String>, guard:Point):Array<Point> {
		var visited = new HashMap();
		var direction = Direction.up;

		while (guard.isInBounds(map)) {
			visited.set(guard, true);

			var next = guard.moved(direction);
			if (!next.isInBounds(map))
				break;
			if (map[next.y].charAt(next.x) == "#") {
				direction = rotated90(direction);
			} else {
				guard = next;
			}
		}

		return [for (k in visited.keys()) k];
	}

	static function runUntilLoop(map:Array<String>, guard:Point):Bool {
		var visited = new HashMap();
		var direction = Direction.up;

		while (guard.isInBounds(map)) {
			if (visited.exists(guard)) {
				if (visited[guard].contains(direction))
					return true;
				visited[guard].push(direction);
			} else {
				visited.set(guard, [direction]);
			}

			var next = guard.moved(direction);
			if (!next.isInBounds(map))
				break;
			if (map[next.y].charAt(next.x) == "#") {
				direction = rotated90(direction);
			} else {
				guard = next;
			}
		}

		return false;
	}

	static function tryObstuctions(map:Array<String>, guard:Point, path:Array<Point>):Int {
		var total = 0;
		for (obstructionPos in path) {
			if (!Point.equals(guard, obstructionPos)) {
				var originalLine = map[obstructionPos.y];
				// sad
				map[obstructionPos.y] = originalLine.substr(0, obstructionPos.x) + "#" + originalLine.substr(obstructionPos.x + 1);
				if (runUntilLoop(map, guard))
					total++;
				map[obstructionPos.y] = originalLine;
			}
		}
		return total;
	}
}
