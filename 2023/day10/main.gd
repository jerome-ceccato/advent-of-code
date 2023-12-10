extends Node2D

@onready var input = 'res://input.txt'
@onready var tileMap = $TileMap
@onready var animalTileMap = $AnimalTileMap

var data
var animal: Vector2i
var state = State.Waiting
var explore_dir = -1
var direction = Vector2i.UP

enum State {
	Waiting,
	Start,
	Explore,
	Done
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

class Pipe:
	var type: PipeType
	var visited: bool
	var loop: bool
	
	func _init(c: String):
		type = [".", "|", "-", "L", "J", "F", "7", "?"].find(c) as PipeType
	
	func atlas_id() -> Vector2i:
		if type > PipeType.None and type < PipeType.Unknown:
			if loop:
				return Vector2i(type, 2)
			elif visited:
				return Vector2i(type, 1)
		return Vector2i(type, 0)
	
	func _to_string() -> String:
		return [".", "|", "-", "└", "┙", "┌", "┐", "?"][type] 

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
	animalTileMap.set_cell(0, pos, 0, Vector2i(8, 0) if pos == animal else Vector2i(0, 0), 0)
	tileMap.set_cell(0, pos, 0, data[pos.y][pos.x].atlas_id(), 0)

func render_all():
	animalTileMap.clear()
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
	match state:
		State.Waiting:
			pass
		State.Start:
			explore_dir += 1
			direction = [Vector2i.UP, Vector2i.RIGHT, Vector2i.DOWN, Vector2i.LEFT][explore_dir]
			state = State.Explore
			for y in data.size():
				for x in data[y].size():
					data[y][x].visited = false
					if data[y][x].type == PipeType.Unknown:
						animal = Vector2i(x, y)
			render_all()
		State.Explore:
			explore()
		State.Done:
			pass

func explore():
	var prev_pos = animal
	var next_dir = next_direction()

	if try_move(next_dir):
		data[animal.y][animal.x].visited = true
		render_at(prev_pos)
		if data[animal.y][animal.x].type == PipeType.Unknown:
			data[animal.y][animal.x].type = get_unknown_piece()
			for y in data.size():
				for x in data[y].size():
					if data[y][x].visited:
						data[y][x].visited = false
						data[y][x].loop = true
			state = State.Done
			render_all()
			animalTileMap.set_cell(0, animal, 0, Vector2i(0, 0), 0)
		else:
			render_at(animal)
	else:
		state = State.Start
	

# Called when the node enters the scene tree for the first time.
func _ready():
	_load_map()
	render_all()
	

const process_per_tick = 1
const tick_at_once = 50
var current_tick = 0
func _physics_process(_delta):
	current_tick += 1
	if current_tick > process_per_tick:
		current_tick = 0
		for i in tick_at_once:
			tick_once()

func _unhandled_input(event):
	if event.is_action_pressed("start"):
		if state == State.Waiting:
			state = State.Start
