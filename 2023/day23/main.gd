extends Node2D

@onready var input = 'res://input.txt'
@onready var header_label = $UI/Header
@onready var tile_map = $TileMap

var data: Array[String]
var zonesId: Dictionary

var paused = true
var state = State.Start
enum State {
	Start,
	Explore,
	Done,
	Waiting,
	Explore2,
	Done2,
	DoNothing
}

func _load_map(): 
	var contents = FileAccess.get_file_as_string(input)
	data.assign(Array(contents.split("\n", false)))

func _build_tileset():
	var base_color = Color("8f563b")
	var source = TileSetAtlasSource.new()
	var tile_size = Vector2i(8, 8)
	
	var n_tiles = 0
	for key in zonesId:
		n_tiles = max(n_tiles, zonesId[key])
	
	n_tiles += 1
	var image = Image.create(tile_size.x * n_tiles, tile_size.y, false, Image.FORMAT_RGB8)
	
	var color_shift = 1.0 / (float(n_tiles) * 2.0)
	for i in n_tiles:
		var color = base_color
		var shift = float(i) * color_shift - 0.25
		if shift < 0:
			color = base_color.darkened(-shift)
		elif shift > 0:
			color = base_color.lightened(shift)
		image.fill_rect(Rect2i(Vector2i(tile_size.x * i, 0), tile_size), color)
	
	source.texture = ImageTexture.create_from_image(image)
	source.texture_region_size = tile_size
	for i in n_tiles:
		source.create_tile(Vector2i(i, 0))
	tile_map.tile_set.add_source(source)

func rerender_board():
	tile_map.clear()
	for y in data.size():
		for x in data[0].length():
			render_at(Vector2i(x, y))
	
	# start/end indicators
	tile_map.set_cell(0, Vector2i(1, -1), 0, Vector2i(2, 1), 0)
	tile_map.set_cell(0, Vector2i(data[0].length() - 2, data.size()), 0, Vector2i(2, 1), 0)
	queue_redraw()

func render_at(pos: Vector2i):
	if zonesId.has(pos):
		tile_map.set_cell(0, pos, 1, Vector2i(zonesId[pos], 0), 0)
	if data[pos.y][pos.x] != ".":
		tile_map.set_cell(1, pos, 0, Vector2i("#^>v<".find(data[pos.y][pos.x]), 0), 0)

func render_head(pos: Vector2i):
	tile_map.set_cell(2, pos, 0, Vector2i(0, 1), 0)

func render_tail(pos: Vector2i):
	tile_map.set_cell(2, pos, 0, Vector2i(1, 1), 0)

func re_render_path(apath: Array[Vector2i]):
	tile_map.clear_layer(2)
	for i in apath.size():
		if i == apath.size() - 1:
			render_head(apath[i])
		else:
			render_tail(apath[i])

func in_bounds(pos: Vector2i) -> bool:
	return pos.y >= 0 and pos.y < data.size() and pos.x >= 0 and pos.x < data[pos.y].length()

func map_zones():
	zonesId = {}
	var currentId = 0
	var zonesStart = [Vector2i(1, 0)]
	var queue = []
	var considered = {}
	while not zonesStart.is_empty():
		var zoneFirst = zonesStart.pop_front()
		if not considered.has(zoneFirst):
			queue = [zoneFirst]
			while not queue.is_empty():
				var first = queue.pop_front()
				zonesId[first] = currentId
				for dir in [Vector2i.UP, Vector2i.RIGHT, Vector2i.DOWN, Vector2i.LEFT]:
					var next = first + dir
					if in_bounds(next) and data[next.y][next.x] != '#' and not considered.has(next):
						if data[next.y][next.x] == '.':
							queue.push_back(Vector2i(next.x, next.y))
							considered[next] = 1
						else:
							var next2 = next + dir
							zonesId[next] = currentId
							considered[next] = 1
							zonesStart.push_back(next2)
			currentId += 1

func slope_dir(pos: Vector2i):
	var id = "^>v<".find(data[pos.y][pos.x])
	return [Vector2i.UP, Vector2i.RIGHT, Vector2i.DOWN, Vector2i.LEFT][id]

var map_start: Vector2i
var map_end: Vector2i
var path: Array[Vector2i] = []
var path_lookup: Dictionary = {}
var branch: Array[Vector3i] = []
var should_rerender = false
var best_path = 0

var paths_explored = 0
func tick_explore():
	if path.is_empty() and branch.is_empty():
		state = State.Done
		return true
	
	if should_rerender:
		re_render_path(path)
		should_rerender = false
	
	var head = path[path.size() - 1]
	
	if head == map_end:
		best_path = max(best_path, path.size() - 1)
		if branch.is_empty():
			state = State.Done
		else:
			var prev = branch.pop_back()
			path.resize(prev.z)
			path.push_back(Vector2i(prev.x, prev.y))
			path_lookup = {}
			for p in path:
				path_lookup[p] = 1
			should_rerender = true
			paths_explored += 1
		return true
	
	var possibilities = []
	for dir in [Vector2i.UP, Vector2i.RIGHT, Vector2i.DOWN, Vector2i.LEFT]:
		var next = head + dir
		if in_bounds(next) and data[next.y][next.x] != '#' and not path_lookup.has(next):
			if data[next.y][next.x] == '.' or dir == slope_dir(next):
				possibilities.push_back(next)
	if possibilities.is_empty():
		if branch.is_empty():
			state = State.Done
			return true
		else:
			var prev = branch.pop_back()
			path.resize(prev.z)
			path.push_back(Vector2i(prev.x, prev.y))
			path_lookup = {}
			for p in path:
				path_lookup[p] = 1
			should_rerender = true
			paths_explored += 1
			return true
	else:
		var next = possibilities.pop_front()
		for other in possibilities:
			branch.push_back(Vector3i(other.x, other.y, path.size()))	
		path.push_back(next)
		path_lookup[next] = 1
		render_tail(head)
		render_head(next)
	return false

func tick_once():
	if paused:
		return

	match state:
		State.Start:
			map_start = Vector2i(1, 0)
			map_end = Vector2i(data[0].length() - 2, data.size() - 1)
			path = [map_start]
			path_lookup = {map_start: 1}
			render_head(path[0])
			state = State.Explore
		State.Explore:
			if explore_whole_paths:
				while not tick_explore():
					pass
			else:
				tick_explore()
			if best_path > 0:
				header_label.text = "Longest path: %d" % best_path
		State.Done:
			print(best_path)
			var tween = get_tree().create_tween()
			tween.tween_interval(1.5)
			tween.tween_property(tile_map, "modulate", Color(Color.WHITE, 0.1), 1)
			tween.parallel().tween_property($UI/Transition, "visible_ratio", 1, 1)
			tween.tween_interval(1.5)
			
			tween.tween_callback(prepare_p2)
			state = State.Waiting
		State.Explore2:
			explore_fast()
			state = State.Done2
			if best_path > 0:
				header_label.text = "Longest path: %d" % best_path
		State.Done2:
			print(best_path)
			end()

func prepare_p2():
	# Remove slopes
	for y in data.size():
		for x in data[y].length():
			if data[y][x] != '#':
				data[y][x] = '.'
	rerender_board()
	tile_map.clear_layer(2)
	
	var tween = get_tree().create_tween()
	tween.tween_property(tile_map, "modulate", Color(Color.WHITE, 1), 0.5)
	tween.parallel().tween_property($UI/Transition, "modulate", Color(Color.WHITE, 0), 0.5)
	tween.tween_interval(0.5)
	tween.tween_callback(start_p2)

func start_p2():
	map_start = Vector2i(1, 0)
	map_end = Vector2i(data[0].length() - 2, data.size() - 1)
	path = [map_start]
	path_lookup = {map_start: 1}
	render_head(path[0])
	physics_process_per_tick = 1
	paths_explored = 0
	setup_graph()
	print(nodes)
	state = State.Explore2

# [Branch point: [Other connected branch point: Distance]]
var nodes = {}
func setup_graph():
	# [pos: 1]
	var tested = {}
	# [[pos, prev_branch]]
	var queue = [[map_start + Vector2i.DOWN, map_start]]
	while not queue.is_empty():
		var raw = queue.pop_back()
		var initial_pos = raw[0]
		var prev_branch = raw[1]
		if not tested.has(initial_pos):
			tested[initial_pos] = 1
			
			var straight = true
			var head = initial_pos
			var visited = {head: 1, prev_branch: 1}
			var len = 1
			while straight:
				var possibilities = []
				for dir in [Vector2i.UP, Vector2i.RIGHT, Vector2i.DOWN, Vector2i.LEFT]:
					var next = head + dir
					if in_bounds(next) and data[next.y][next.x] != '#' and not visited.has(next):
						possibilities.push_back(next)
				
				if possibilities.size() == 1:
					if possibilities[0] == map_end:
						straight = false
						if not nodes.has(prev_branch):
							nodes[prev_branch] = {}
						if nodes[prev_branch].has(map_end):
							nodes[prev_branch][map_end] = max(nodes[prev_branch][map_end], len)
						else:
							nodes[prev_branch][map_end] = len
					else:
						head = possibilities[0]
						visited[head] = 1
						len += 1
				else:
					straight = false
					for p in possibilities:
						queue.push_back([p, head])
					if not nodes.has(prev_branch):
						nodes[prev_branch] = {}
					if nodes[prev_branch].has(head):
						nodes[prev_branch][head] = max(nodes[prev_branch][head], len)
					else:
						nodes[prev_branch][head] = len
	
func explore_rec(pos: Vector2i, distance: int, previous: Dictionary) -> int:
	paths_explored += 1
	if pos == map_end:
		return distance
	previous[pos] = 1
	var all_dist = []
	for next in nodes[pos]:
		if not previous.has(next):
			all_dist.push_back(explore_rec(next, distance + nodes[pos][next], previous))
	previous.erase(pos)
	if all_dist.is_empty():
		return -1
	return all_dist.max()

func explore_fast():
	paths_explored = 0
	best_path = explore_rec(map_start, 0, {}) + 1
	print("Explored: %d" % paths_explored)

func end():
	if OS.has_feature("movie"):
		get_tree().quit()
	else:
		state = State.DoNothing

func _ready():
	_load_map()
	map_zones()
	_build_tileset()
	rerender_board()
	
	#$Camera2D.position = Vector2(92, 92)
	#$Camera2D.zoom = Vector2(5, 5)
	#prepare_p2()
	
	if OS.has_feature("movie"):
		paused = false

var physics_process_per_tick = 3
var explore_whole_paths = false
var current_tick = 0
func _physics_process(_delta):
	current_tick += 1
	if current_tick > physics_process_per_tick:
		current_tick = 0
		tick_once()

func _unhandled_input(event):
	if event.is_action_pressed("start"):
		paused = !paused
