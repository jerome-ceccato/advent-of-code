extends Node2D

@onready var input = 'res://input.txt'
@onready var header_label = $UI/Header
@onready var tile_map = $TileMap

var data: Array[String]
var floorLevels: Dictionary

var paused = true
var state = State.Start
enum State {
	Start,
	Explore,
	Done,
	DoNothing
}

func _load_map(): 
	var contents = FileAccess.get_file_as_string(input)
	data.assign(Array(contents.split("\n", false)))

func _build_tileset():
	var base_color = Color("8f563b")
	var source = TileSetAtlasSource.new()
	var tile_size = Vector2i(8, 8)
	
	var lowest = 0
	var highest = 0
	for key in floorLevels:
		lowest = min(lowest, floorLevels[key])
		highest = max(highest, floorLevels[key])
	
	var n_tiles = highest - lowest + 1
	var image = Image.create(tile_size.x * n_tiles, tile_size.y, false, Image.FORMAT_RGB8)
	
	var color_shift = 1.0 / (float(n_tiles) * 2.0)
	for i in n_tiles:
		var color = base_color
		if (i + lowest) < 0:
			color = base_color.lightened(-float(i + lowest) * color_shift)
		elif (i + lowest) > 0:
			color = base_color.darkened(float(i + highest) * color_shift)
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
	queue_redraw()

func render_at(pos: Vector2i):
	if floorLevels.has(pos):
		tile_map.set_cell(0, pos, 1, Vector2i(floorLevels[pos], 0), 0)
	if data[pos.y][pos.x] != ".":
		tile_map.set_cell(1, pos, 0, Vector2i("#^>v<".find(data[pos.y][pos.x]), 0), 0)

func in_bounds(pos: Vector2i) -> bool:
	return pos.y >= 0 and pos.y < data.size() and pos.x >= 0 and pos.x < data[pos.y].length()

func map_floor_levels():
	floorLevels = {}
	var queue = [Vector3i(1, 0, 0)]
	var considered = {}
	#var target = Vector2i(data[0].length() - 2, data.size() - 1)
	while not queue.is_empty():
		var first = queue.pop_front()
		var first_pos = Vector2i(first.x, first.y)
		floorLevels[first_pos] = first.z
		for dir in [Vector2i.UP, Vector2i.RIGHT, Vector2i.DOWN, Vector2i.LEFT]:
			var next = first_pos + dir
			if in_bounds(next) and data[next.y][next.x] != '#' and not considered.has(next):
				if data[next.y][next.x] == '.':
					queue.push_back(Vector3i(next.x, next.y, first.z))
					considered[next] = 1
				else:
					var id = "^>v<".find(data[next.y][next.x])
					var slope_dir = [Vector2i.UP, Vector2i.RIGHT, Vector2i.DOWN, Vector2i.LEFT][id]
					var next2 = next + dir
					if dir == slope_dir:
						floorLevels[next] = first.z
						queue.push_back(Vector3i(next2.x, next2.y, first.z - 1))
					else:
						floorLevels[next] = first.z + 1
						queue.push_back(Vector3i(next2.x, next2.y, first.z + 1))
					considered[next] = 1
					considered[next2] = 1


func normalize_floor_levels():
	var lowest = 0
	for key in floorLevels:
		lowest = min(lowest, floorLevels[key])
	for key in floorLevels:
		floorLevels[key] -= lowest

func tick_explore():
	pass

func tick_once():
	if paused:
		return

	match state:
		State.Start:
			state = State.Explore
		State.Explore:
			for _i in explore_per_tick:
				tick_explore()

func end():
	if OS.has_feature("movie"):
		get_tree().quit()

func _ready():
	_load_map()
	map_floor_levels()
	_build_tileset()
	normalize_floor_levels()
	rerender_board()
	
	#$Camera2D.position = Vector2(30, 40)
	#$Camera2D.zoom = Vector2(7.5, 7.5)
	#prepare_p2()
	
	if OS.has_feature("movie"):
		paused = false

const physics_process_per_tick = 30
var explore_per_tick = 1
var current_tick = 0
func _physics_process(_delta):
	current_tick += 1
	if current_tick > physics_process_per_tick:
		current_tick = 0
		tick_once()

func _unhandled_input(event):
	if event.is_action_pressed("start"):
		paused = !paused
