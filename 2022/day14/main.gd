extends Node2D

@onready var input = 'res://input.txt'
@onready var tileMap = $TileMap

var _nticks = 0
var _board = []
var _spawn = Vector2i.ZERO
var _has_floor: bool
var _running = false

var _last_set = Vector2i.ZERO

var ticks_per_draw: int

# Loading input

func _load_paths(): 
	var contents = FileAccess.get_file_as_string(input)
	var paths = Array(contents.split("\n", false)).map(
		func (path): return Array(path.split(" -> ", false)).map(
			func (pos): 
				var split_pos = pos.split(",", false)
				return Vector2i(int(split_pos[0]), int(split_pos[1]))))
	return paths

func _get_bounds(paths):
	var top_left = Vector2i(500, 0)
	var bottom_right = Vector2i(500, 0)
	
	for path in paths:
		for point in path:
			top_left.x = min(point.x, top_left.x)
			top_left.y = min(point.y, top_left.y)
			bottom_right.x = max(point.x, bottom_right.x)
			bottom_right.y = max(point.y, bottom_right.y)
	
	if _has_floor:
		var size = bottom_right - top_left + Vector2i(3, 3)
		return Rect2i(top_left - Vector2i.ONE - Vector2i(size.y + 1, 0), size + (2 * Vector2i(size.y + 1, 0)))
	else:
		return Rect2i(top_left - Vector2i.ONE, bottom_right - top_left + Vector2i(3, 3))

func _normalized(paths, low_bound):
	return paths.map(
		func (path): return path.map(
			func (point): return point - low_bound))

func _build_board(paths, size):
	var board = Array()
	for y in range(size.y):
		board.push_back(Array())
		board[y].resize(size.x)
		board[y].fill(0)
	
	for path in paths:
		for i in range(1, path.size()):
			var direction = (path[i] - path[i - 1]).sign()
			var target = path[i - 1]
			while target != path[i]:
				board[target.y][target.x] = 1
				target += direction
			board[target.y][target.x] = 1
	return board


# Tick

func valid(board, vec: Vector2i):
	return vec.y >= 0 && vec.y < board.size() && vec.x >= 0 && vec.x < board[vec.y].size()

func at(board, vec: Vector2i):
	return board[vec.y][vec.x] if valid(board, vec) else -1

func _tick(board, spawn_point):
	if at(board, spawn_point) > 0:
		return false
	
	var sand_pos = spawn_point
	while valid(board, sand_pos):
		if at(board, sand_pos + Vector2i.DOWN) == 0:
			sand_pos += Vector2i.DOWN
		elif !_has_floor && at(board, sand_pos + Vector2i.DOWN) == -1:
			return false
		elif at(board, sand_pos + Vector2i(-1, 1)) == 0:
			sand_pos += Vector2i(-1, 1)
		elif at(board, sand_pos + Vector2i(1, 1)) == 0:
			sand_pos += Vector2i(1, 1)
		else:
			_last_set = sand_pos
			board[sand_pos.y][sand_pos.x] = 2
			return true
	return false


# Display
func _draw_one(board, pos):
	var value = board[pos.y][pos.x]
	
	match value:
		0:
			tileMap.set_cell(0, pos, -1, Vector2i(-1, -1), -1)
		1:
			tileMap.set_cell(0, pos, 0, Vector2i(1, 0))
		2:
			tileMap.set_cell(0, pos, 0, Vector2i(0, 0))

func _draw_board(board):
	for y in range(board.size()):
		for x in range(board[y].size()):
			_draw_one(board, Vector2i(x, y))
	queue_redraw()

func _setup_camera(size):
	var center = tileMap.map_to_local(size / 2)
	$Camera2D.position = center

func _unhandled_input(event):
	if event.is_action_pressed('start'):
		_running = !_running

func _physics_process(delta):
	if !_running:
		return
	
	for i in range(ticks_per_draw):
		if _tick(_board, _spawn):
			_nticks += 1
			_draw_one(_board, _last_set)
		elif _running:
			print(_nticks)
			_running = false


# Main

func _setup(has_floor, speed):
	_has_floor = has_floor
	ticks_per_draw = speed
	
	var paths = _load_paths()
	var bounds = _get_bounds(paths)
	paths = _normalized(paths, bounds.position)
	var board = _build_board(paths, bounds.size)
	var spawn_point = Vector2i(500, 0) - bounds.position
	
	_board = board
	_spawn = spawn_point
	
	_draw_board(board)
	_setup_camera(bounds.size)
	

func _ready():
	_setup(true, 20) # set to false for part 1
