import Raylib

enum DiskBlock: Equatable {
	case empty
	case used(id: Int)

	var color: Color {
		let colors: [Color] = [
			// .init(r: 115, g: 70, b: 76, a: .max),
			.init(r: 171, g: 86, b: 117, a: .max),
			.init(r: 238, g: 106, b: 124, a: .max),
			.init(r: 255, g: 167, b: 165, a: .max),
			.init(r: 255, g: 224, b: 126, a: .max),
			.init(r: 255, g: 231, b: 214, a: .max),
			.init(r: 114, g: 220, b: 187, a: .max),
			.init(r: 52, g: 172, b: 186, a: .max),
		]

		switch self {
		case .empty:
			return .blank
		case .used(let id):
			return colors[id % colors.count]
		}
	}
}

struct Position: Equatable, Hashable {
	let x: Int32
	let y: Int32
}

struct Size: Equatable, Hashable {
	let width: Int32
	let height: Int32
}

final class Day09 {
	var blocks: [DiskBlock] = []

	var startIndex = 0
	var endIndex = 0
	var currentFileId = -1

	func setup(with inputPath: String) {
		let file = try! String(contentsOfFile: inputPath, encoding: .utf8)
			.trimmingCharacters(in: .whitespacesAndNewlines)

		for (i, c) in file.enumerated() {
			let id = i / 2
			let isEmpty = (i % 2) == 1

			if let value = c.wholeNumberValue {
				for _ in 0..<value {
					blocks.append(isEmpty ? .empty : .used(id: id))
				}
			}
			if !isEmpty {
				currentFileId = id
			}
		}

		startIndex = 0
		endIndex = blocks.count - 1
	}

	func doMoves(move: () -> Bool, times: Int) -> Bool {
		for _ in 0..<times {
			if move() == false {
				return false
			}
		}
		return true
	}

	func moveBlock() -> Bool {
		while startIndex < blocks.count && blocks[startIndex] != .empty {
			startIndex += 1
		}

		while endIndex > startIndex && blocks[endIndex] == .empty {
			endIndex -= 1
		}

		guard startIndex < endIndex else {
			return false
		}

		blocks[startIndex] = blocks[endIndex]
		startIndex += 1
		endIndex -= 1
		blocks.removeLast(blocks.count - endIndex - 1)
		return true
	}

	func sizeOfEmptyBlock(from index: Int) -> Int {
		var index = index
		var size = 0
		while index <= endIndex && blocks[index] == .empty {
			size += 1
			index += 1
		}
		return size
	}

	func findEmptySpace(fitting size: Int) -> Int? {
		for index in 0...endIndex {
			if blocks[index] == .empty {
				if sizeOfEmptyBlock(from: index) >= size {
					return index
				}
			}
		}
		return nil
	}

	func moveFile() -> Bool {
		while endIndex > 0 && blocks[endIndex] != .used(id: currentFileId) {
			endIndex -= 1
		}

		guard endIndex > 0 else {
			return false
		}

		var size = 0
		while endIndex > 0 && blocks[endIndex] == .used(id: currentFileId) {
			size += 1
			endIndex -= 1
		}

		endIndex += 1
		if let emptySpaceIndex = findEmptySpace(fitting: size) {
			for i in 0..<size {
				blocks[emptySpaceIndex + i] = .used(id: currentFileId)
				blocks[endIndex + i] = .empty
			}
		}

		currentFileId -= 1

		return true
	}

	func computeSolution() -> Int {
		return blocks.enumerated().reduce(0) { (partialResult, item) in
			switch item.element {
			case .empty:
				return partialResult
			case .used(let id):
				return partialResult + (item.offset * id)
			}
		}
	}

	var defragScope: Int {
		endIndex - startIndex
	}
}

enum State {
	case waiting
	case p1
	case p2
	case done(solution: Int)
}

final class Visualization {
	let solver = Day09()
	let screenWidth: Int32 = 1920
	let screenHeight: Int32 = 1080
	let fps: Int32 = 60
	let upf = 100
	let backgroundColor = Color(r: 15, g: 15, b: 35, a: UInt8.max)

	let blockSize = Size(width: 4, height: 4)
	let blockOffset = Size(width: 4, height: 4)
	let columns: Int32 = 450

	var state = State.waiting

	func run() {
		Raylib.initWindow(screenWidth, screenHeight, "Day09")
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
				state = .p1
			} else if Raylib.isKeyPressed(.number2) {
				state = .p2
			}
		case .p1:
			if !solver.doMoves(move: solver.moveBlock, times: upf) {
				let solution = solver.computeSolution()
				print(solution)
				state = .done(solution: solution)
			}
		case .p2:
			if !solver.doMoves(move: solver.moveFile, times: upf) {
				let solution = solver.computeSolution()
				print(solution)
				state = .done(solution: solution)
			}
		case .done:
			break
		}
	}

	func draw() {
		let title: String = {
			switch state {
			case .waiting:
				return "Press 1 to defrag by block\nPress 2 to defrag by files"
			case .p1, .p2:
				return "Defragmenting \(solver.defragScope) blocks..."
			case .done(let solution):
				return "Checksum: \(solution)"
			}
		}()
		let titleFontSize: Int32 = 40
		let titleX = (screenWidth - Raylib.measureText(title, titleFontSize)) / 2

		Raylib.drawText(title, titleX, 20, titleFontSize, .rayWhite)

		let globalOffset = Position(
			x: (screenWidth - (columns * blockOffset.width)) / 2,
			y: 150
		)
		for (i, block) in solver.blocks.enumerated() {
			switch block {
			case .empty:
				break
			case .used:
				Raylib.drawRectangle(
					globalOffset.x + (Int32(i) % columns) * blockOffset.width,
					globalOffset.y + (Int32(i) / columns) * blockOffset.height,
					blockSize.width,
					blockSize.height,
					block.color
				)
			}
		}
	}
}

let vis = Visualization()
vis.run()
