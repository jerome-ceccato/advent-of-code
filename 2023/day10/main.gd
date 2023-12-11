extends Node2D

@onready var input = 'res://input.txt'
@onready var tileMap = $TileMap
@onready var lengthLabel = $UI/Len
@onready var farthestLabel = $UI/Farthest
@onready var insideCountLabel = $"UI/Inside count"
@onready var screenshot = $UI/Screenshot
@onready var finalLabel = $UI/Final

var data
var animal: Vector2i
var paused = true
var state = State.Start
var explore_dir = -1
var direction = Vector2i.UP
var path: Array[Vector2i] = []
var find_pipe_y = 0
var inside_count = 0
var ss_img: Image
var pixel_fill_queue = []

enum State {
	Start,
	Explore,
	DoneExploring,
	FindPipes,
	Done,
	Screenshot,
	End,
	DoNothing
}

enum PipeType {
	None,
	Vertical,
	Horizontal,
	NorthEast,
	NorthWest,
	SouthEast,
	SouthWest,
	Unknown
}

enum PipeTag {
	None,
	Visited,
	Loop,
	Outside,
	Inside,
}

class Pipe:
	var type: PipeType
	var tag: PipeTag
	
	func _init(c: String):
		type = [".", "|", "-", "L", "J", "F", "7", "?"].find(c) as PipeType
	
	func atlas_id() -> Vector2i:
		if type == PipeType.Unknown or (type == PipeType.None and tag != PipeTag.Inside):
			return Vector2i(type, 0)
		return Vector2i(type, tag)
	
	func _to_string() -> String:
		var type_s = [".", "|", "-", "└", "┙", "┌", "┐", "?"][type]
		var tag_s = ["None", "Visited", "Loop", "Outside", "Inside"][tag]
		return "%s [%s]" % [type_s, tag_s]

func _load_map(): 
	var contents = FileAccess.get_file_as_string(input)
	var lines = Array(contents.split("\n", false))
	
	data = Array()
	for y in lines.size():
		data.push_back(Array())
		for x in lines[y].length():
			if lines[y][x] == "S":
				animal = Vector2i(x, y)
				data[y].push_back(Pipe.new("?"))
			else:
				data[y].push_back(Pipe.new(lines[y][x]))

func render_at(pos):
	if pos == animal:
		tileMap.set_cell(0, pos, 0, Vector2i(8, 0), 0)
	else:
		tileMap.set_cell(0, pos, 0, data[pos.y][pos.x].atlas_id(), 0)

func render_all():
	tileMap.clear()
	for y in data.size():
		for x in data[y].size():
			render_at(Vector2i(x, y))
	queue_redraw()

func in_bounds(pos):
	return pos.y >= 0 and pos.y < data.size() and pos.x >= 0 and pos.x < data[pos.y].size()

func next_direction():
	if not in_bounds(animal):
		return Vector2i.ZERO
	
	match data[animal.y][animal.x].type:
		PipeType.None:
			return Vector2i.ZERO
		PipeType.Unknown:
			return direction
		PipeType.Vertical:
			if direction == Vector2i.UP or direction == Vector2i.DOWN:
				return direction
			return Vector2i.ZERO
		PipeType.Horizontal:
			if direction == Vector2i.RIGHT or direction == Vector2i.LEFT:
				return direction
			return Vector2i.ZERO
		PipeType.NorthEast:
			if direction == Vector2i.DOWN:
				return Vector2i.RIGHT
			elif direction == Vector2i.LEFT:
				return Vector2i.UP
			return Vector2i.ZERO
		PipeType.NorthWest:
			if direction == Vector2i.DOWN:
				return Vector2i.LEFT
			elif direction == Vector2i.RIGHT:
				return Vector2i.UP
			return Vector2i.ZERO
		PipeType.SouthEast:
			if direction == Vector2i.UP:
				return Vector2i.RIGHT
			elif direction == Vector2i.LEFT:
				return Vector2i.DOWN
			return Vector2i.ZERO
		PipeType.SouthWest:
			if direction == Vector2i.UP:
				return Vector2i.LEFT
			elif direction == Vector2i.RIGHT:
				return Vector2i.DOWN
			return Vector2i.ZERO

func get_unknown_piece():
	var start_dir = [Vector2i.UP, Vector2i.RIGHT, Vector2i.DOWN, Vector2i.LEFT][explore_dir]
	var sum = start_dir - direction
	if direction.x == 0 and start_dir.x == 0:
		return PipeType.Vertical
	elif direction.y == 0 and start_dir.y == 0:
		return PipeType.Horizontal
	elif sum == Vector2i.UP + Vector2i.LEFT:
		return PipeType.NorthWest
	elif sum == Vector2i.UP + Vector2i.RIGHT:
		return PipeType.NorthEast
	elif sum == Vector2i.DOWN + Vector2i.LEFT:
		return PipeType.SouthWest
	elif sum == Vector2i.DOWN + Vector2i.RIGHT:
		return PipeType.SouthEast
	return PipeType.Unknown

func try_move(dir) -> bool:
	if dir == Vector2i.ZERO:
		return false
	animal += dir
	direction = dir
	return in_bounds(animal)

func tick_once():
	if paused:
		return
	match state:
		State.Start:
			explore_dir += 1
			direction = [Vector2i.UP, Vector2i.RIGHT, Vector2i.DOWN, Vector2i.LEFT][explore_dir]
			state = State.Explore
			path.clear()
			for y in data.size():
				for x in data[y].size():
					data[y][x].tag = PipeTag.None
					if data[y][x].type == PipeType.Unknown:
						animal = Vector2i(x, y)
			render_all()
		State.Explore:
			for i in explore_per_ick:
				if state == State.Explore:
					explore()
			lengthLabel.text = "Path length: %d" % path.size()
		State.DoneExploring:
			farthestLabel.text = "Farthest point: %d" % (path.size() / 2)
			current_wait += 1
			if current_wait == 10:
				var img = tileMap.get_viewport().get_texture().get_image()
				img.save_png("screenshot-1.png")
			if current_wait > wait_before_p2:
				state = State.FindPipes
		State.FindPipes:
			if find_pipe_y >= data.size():
				for label in [lengthLabel, farthestLabel, insideCountLabel]:
					label.visible = false
				queue_redraw()
				state = State.Done
			else:
				current_find_pipe += 1
				if current_find_pipe > tick_per_find_pipe:
					current_find_pipe = 0
					find_pipe_line()
					find_pipe_y += 1
				if inside_count > 0:
					insideCountLabel.text = "Inside: %d" % inside_count
		State.Done:
			var img = tileMap.get_viewport().get_texture().get_image()
			img.save_png("screenshot-2.png")
			ss_img = img
			screenshot.texture = ImageTexture.create_from_image(img)
			tileMap.visible = false
			state = State.Screenshot
			pixel_fill_queue = [Vector2i.ZERO, Vector2i(0, img.get_size().y - 1), Vector2i(img.get_size().x - 1, 0), Vector2i(img.get_size().x - 1, img.get_size().y - 1)]
		State.Screenshot:
			const pipe_color = Color("5b6ee1")
			const bg_color = Color("3e3b5f")
			const green = Color("6abe30")
			for _i in queue_progress_per_tick:
				var next_q = []
				for pos in pixel_fill_queue:
					for dir in [Vector2i.UP, Vector2i.RIGHT, Vector2i.DOWN, Vector2i.LEFT]:
						var cpos = pos + dir
						if cpos.x >= 0 && cpos.y >= 0 && cpos.x < ss_img.get_size().x && cpos.y < ss_img.get_size().y:
							var px = ss_img.get_pixelv(cpos)
							if px != pipe_color and px != bg_color:
								ss_img.set_pixelv(cpos, bg_color)
								next_q.push_back(cpos)
				screenshot.texture = ImageTexture.create_from_image(ss_img)
				if next_q.is_empty():
					pixel_fill_queue = []
					state = State.End
					
					var inside_px = 0
					for y in ss_img.get_size().y:
						for x in ss_img.get_size().x:
							if ss_img.get_pixel(x, y) == green:
								inside_px += 1
					finalLabel.text = "Farthest point: %d     Points inside: %d" % [(path.size() / 2), inside_px / 4]
				else:
					pixel_fill_queue = next_q
		State.End:
			var img = tileMap.get_viewport().get_texture().get_image()
			img.save_png("screenshot-3.png")
			state = State.DoNothing


func test_node_inside(pos: Vector2i, visited) -> int:
	match data[pos.y][pos.x].tag:
		PipeTag.None:
			visited.push_back(pos)
			data[pos.y][pos.x].tag = PipeTag.Visited
			for dir in [Vector2i.UP, Vector2i.RIGHT, Vector2i.DOWN, Vector2i.LEFT]:
				if not in_bounds(pos + dir):
					return 0
				var test = test_node_inside(pos + dir, visited)
				if test != -1:
					return test
			return -1
		PipeTag.Loop:
			return -1
		PipeTag.Visited:
			return -1
		PipeTag.Outside:
			return 0
		PipeTag.Inside:
			return 1
	return -1

func find_pipe_line():
	for x in data[find_pipe_y].size():
		var pos = Vector2i(x, find_pipe_y)
		if data[pos.y][pos.x].tag == PipeTag.None:
			var visited = []
			var test = test_node_inside(pos, visited)
			for n in visited:
				data[n.y][n.x].tag = PipeTag.None
			if test != 0:
				data[pos.y][pos.x].tag = PipeTag.Inside
				inside_count += 1
			else:
				data[pos.y][pos.x].tag = PipeTag.Outside
			render_at(pos)


func explore():
	var prev_pos = animal
	var next_dir = next_direction()

	if try_move(next_dir):
		data[animal.y][animal.x].tag = PipeTag.Visited
		path.push_back(animal)
		render_at(prev_pos)
		if data[animal.y][animal.x].type == PipeType.Unknown:
			data[animal.y][animal.x].type = get_unknown_piece()
			for y in data.size():
				for x in data[y].size():
					if data[y][x].tag == PipeTag.Visited:
						data[y][x].tag = PipeTag.Loop
			state = State.DoneExploring
			animal = Vector2i(-1, -1)
			render_all()
		else:
			render_at(animal)
	else:
		state = State.Start


# Called when the node enters the scene tree for the first time.
func _ready():
	_load_map()
	render_all()
	

const process_per_tick = 1
const explore_per_ick = 100
const wait_before_p2 = 20
const tick_per_find_pipe = 1
const queue_progress_per_tick = 3
var current_wait = 0
var current_tick = 0
var current_find_pipe = 0
func _physics_process(_delta):
	current_tick += 1
	if current_tick > process_per_tick:
		current_tick = 0
		tick_once()

func _unhandled_input(event):
	if event.is_action_pressed("start"):
		paused = !paused
