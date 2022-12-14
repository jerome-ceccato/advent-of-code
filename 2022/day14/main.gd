extends Node2D

@onready var input = 'res://input.txt'
@onready var tileMap = $TileMap
@onready var timer = $Timer

var _nticks = 0
var _board = []
var _spawn = Vector2i.ZERO

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
		elif at(board, sand_pos + Vector2i.DOWN) == -1:
			return false
		elif at(board, sand_pos + Vector2i(-1, 1)) == 0:
			sand_pos += Vector2i(-1, 1)
		elif at(board, sand_pos + Vector2i(1, 1)) == 0:
			sand_pos += Vector2i(1, 1)
		else:
			board[sand_pos.y][sand_pos.x] = 2
			return true
	return false


func _draw_board(board):
	for y in range(board.size()):
		for x in range(board[y].size()):
			var value = board[y][x]
			if value == 0:
				tileMap.set_cell(0, Vector2i(x, y), -1, Vector2i(-1, -1), -1)
			else:
				tileMap.set_cell(0, Vector2i(x, y), 0, Vector2i(board[y][x] - 1, 0))
	queue_redraw()


func _setup_camera(size):
	var center = tileMap.map_to_local(size / 2)
	$Camera2D.position = center

func _ready():
	var paths = _load_paths()
	var bounds = _get_bounds(paths)
	paths = _normalized(paths, bounds.position)
	var board = _build_board(paths, bounds.size)
	var spawn_point = Vector2i(500, 0) - bounds.position
	
	_board = board
	_spawn = spawn_point

	_draw_board(board)
	_setup_camera(bounds.size)
	#timer.start()

func _unhandled_input(event):
	if event.is_action_pressed('start'):
		_running = !_running

var _running = false
func _physics_process(delta):
	if !_running:
		return
	
	if _tick(_board, _spawn):
		_nticks += 1
	else:
		print(_nticks)
		_running = false
	_draw_board(_board)
