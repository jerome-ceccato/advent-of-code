import Raylib

struct Point: Equatable, Hashable {
	let x: Int
	let y: Int

	static func + (lhs: Point, rhs: Point) -> Point {
		Point(x: lhs.x + rhs.x, y: lhs.y + rhs.y)
	}

	static let up = Point(x: 0, y: -1)
	static let right = Point(x: 1, y: 0)
	static let down = Point(x: 0, y: 1)
	static let left = Point(x: -1, y: 0)
}

struct Size: Equatable, Hashable {
	let width: Int32
	let height: Int32
}

final class Day10 {
	var world: [[Int]] = []

	func setup(with inputPath: String) {
		let file = try! String(contentsOfFile: inputPath, encoding: .utf8)
			.trimmingCharacters(in: .whitespacesAndNewlines)

		world = file.components(separatedBy: "\n").map { line in
			line.compactMap(\.wholeNumberValue)
		}
	}

	func getTrailheads() -> [Point] {
		var trailheads = [Point]()
		for (y, line) in world.enumerated() {
			for (x, height) in line.enumerated() {
				if height == 0 {
					trailheads.append(Point(x: x, y: y))
				}
			}
		}
		return trailheads.reversed()
	}

	func getHeight(at point: Point) -> Int? {
		if world.indices.contains(point.y) {
			if world[point.y].indices.contains(point.x) {
				return world[point.y][point.x]
			}
		}
		return nil
	}

	func getReachableNodes(from head: Point) -> [Point: Int] {
		var queue = [head]
		var visited = [Point: Int]()
		let directions: [Point] = [.up, .right, .down, .left]

		while let current = queue.popLast() {
			visited[current, default: 0] += 1
			if let currentHeight = getHeight(at: current) {
				for dir in directions {
					let target = current + dir
					if let targetHeight = getHeight(at: target), targetHeight == currentHeight + 1 {
						queue.append(target)
					}
				}
			}
		}

		return visited
	}

	func getScore(reachableNodes: [Point: Int]) -> Int {
		return reachableNodes.keys.map { getHeight(at: $0) == 9 ? 1 : 0 }.reduce(0, +)
	}

	func getRating(reachableNodes: [Point: Int]) -> Int {
		return reachableNodes.map { getHeight(at: $0.key) == 9 ? $0.value : 0 }.reduce(0, +)
	}
}

extension Int {
	var heightColor: Color {
		let high = Color(r: 16, g: 123, b: 0, a: .max)
		let low = Color(r: 207, g: 224, b: 56, a: .max)

		let highPercent = Float(self) / 9
		let lowPercent = 1 - highPercent
		return Color(
			r: UInt8(Float(low.r) * lowPercent + Float(high.r) * highPercent),
			g: UInt8(Float(low.g) * lowPercent + Float(high.g) * highPercent),
			b: UInt8(Float(low.b) * lowPercent + Float(high.b) * highPercent),
			a: .max
		)

	}
}

enum State {
	case waiting
	case p1(trailheads: [Point], totalScore: Int, currentItem: WorkItem?)
	case p2(trailheads: [Point], totalScore: Int, currentItem: WorkItem?)
	case done(solution: Int)
}

struct WorkItem {
	let nodes: [Point: Int]
	let score: Int
}

final class Visualization {
	let solver = Day10()
	let screenWidth: Int32 = 1920
	let screenHeight: Int32 = 1080
	let fps: Int32 = 30
	let backgroundColor = Color(r: 15, g: 15, b: 35, a: UInt8.max)

	let blockSize = Size(width: 20, height: 20)
	let highlightSize = Size(width: 16, height: 16)

	var state = State.waiting

	func run() {
		Raylib.initWindow(screenWidth, screenHeight, "Day10")
		Raylib.setTargetFPS(fps)
		solver.setup(with: "input")

		while !Raylib.windowShouldClose {
			update()

			Raylib.beginDrawing()
			Raylib.clearBackground(backgroundColor)
			draw()
			Raylib.endDrawing()
		}

		Raylib.closeWindow()
	}

	func update() {
		switch state {
		case .waiting:
			if Raylib.isKeyPressed(.number1) {
				state = .p1(trailheads: solver.getTrailheads(), totalScore: 0, currentItem: nil)
			} else if Raylib.isKeyPressed(.number2) {
				state = .p2(trailheads: solver.getTrailheads(), totalScore: 0, currentItem: nil)
			}
		case .p1(var trailheads, let totalScore, _):
			if let current = trailheads.popLast() {
				let reachable = solver.getReachableNodes(from: current)
				let score = solver.getScore(reachableNodes: reachable)
				state = .p1(
					trailheads: trailheads, totalScore: score + totalScore,
					currentItem: .init(nodes: reachable, score: score))
			} else {
				state = .done(solution: totalScore)
				print(totalScore)
			}
		case .p2(var trailheads, let totalScore, _):
			if let current = trailheads.popLast() {
				let reachable = solver.getReachableNodes(from: current)
				let score = solver.getRating(reachableNodes: reachable)
				state = .p2(
					trailheads: trailheads, totalScore: score + totalScore,
					currentItem: .init(nodes: reachable, score: score))
			} else {
				state = .done(solution: totalScore)
				print(totalScore)
			}
		case .done:
			break
		}
	}

	func draw() {
		let title: String = {
			switch state {
			case .waiting:
				return "Press 1 for p1\nPress 2 for p2"
			case .p1(_, let score, let item):
				return "Total score: \(score)\nThis trail: \(item?.score ?? 0)"
			case .p2(_, let score, let item):
				return "Total rating: \(score)\nThis trail: \(item?.score ?? 0)"
			case .done(let solution):
				return "Solution: \(solution)"
			}
		}()
		let titleFontSize: Int32 = 40
		let titleX = (screenWidth - Raylib.measureText(title, titleFontSize)) / 2

		Raylib.drawText(title, titleX, 20, titleFontSize, .rayWhite)

		let globalOffset = Point(
			x: (Int(screenWidth) - (solver.world.count * Int(blockSize.width))) / 2,
			y: 150
		)

		let highlightOffset = Size(
			width: (blockSize.width - highlightSize.width) / 2,
			height: (blockSize.height - highlightSize.height) / 2
		)
		for (y, line) in solver.world.enumerated() {
			for (x, height) in line.enumerated() {
				Raylib.drawRectangle(
					Int32(globalOffset.x) + Int32(x) * blockSize.width,
					Int32(globalOffset.y) + Int32(y) * blockSize.height,
					blockSize.width,
					blockSize.height,
					height.heightColor
				)
			}
		}

		switch state {
		case .p1(_, _, let item), .p2(_, _, let item):
			if let reachable = item?.nodes {
				for point in reachable.keys {
					let color: Color = {
						guard let height = solver.getHeight(at: point) else {
							return .black
						}

						if height == 0 {
							return .rayWhite
						} else if height == 9 {
							return .red
						} else {
							return .brown
						}
					}()

					Raylib.drawRectangle(
						Int32(globalOffset.x) + Int32(point.x) * blockSize.width
							+ highlightOffset.width,
						Int32(globalOffset.y) + Int32(point.y) * blockSize.height
							+ highlightOffset.height,
						highlightSize.width,
						highlightSize.height,
						color
					)
				}
			}
		default:
			break
		}
	}
}

let vis = Visualization()
vis.run()
