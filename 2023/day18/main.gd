extends Node2D

@onready var input = 'res://input.txt'
@onready var header_label = $UI/Header
@onready var tile_map = $TileMap

var raw_data: Array
var data: Array[Array]
var color_map: Dictionary
var trench_count = 0
var interior_count = 0
var starting_pos = Vector2i.ZERO
var current_pos = Vector2i.ZERO
var current_line = 0
var dig_points: Array[Vector2i] = []

var paused = true
var state = State.Start
enum State {
	Start,
	Trenches,
	Dig,
	DoneDigging,
	DoNothing
}

func _load_map(): 
	var contents = FileAccess.get_file_as_string(input)
	raw_data = Array(contents.split("\n", false)).map(
		func (line): return Array(line.split(" ", false))
	)

func get_dir(letter: String) -> Vector2i:
	var i = ["U", "R", "D", "L"].find(letter)
	return [Vector2i.UP, Vector2i.RIGHT, Vector2i.DOWN, Vector2i.LEFT][i]

func _extract_data():
	var pos = Vector2i.ZERO
	var pos_min = Vector2i.ZERO
	var pos_max = Vector2i.ZERO
	
	var color_id = 2
	color_map = {}
	for line in raw_data:
		var direction = get_dir(line[0])
		var times = int(line[1])
		var color = Color(line[2].substr(1, line[2].length() - 2))
		
		pos += (direction * times)
		pos_min.x = min(pos_min.x, pos.x)
		pos_min.y = min(pos_min.y, pos.y)
		pos_max.x = max(pos_max.x, pos.x)
		pos_max.y = max(pos_max.y, pos.y)
		
		if not color_map.has(color):
			color_map[color] = color_id
			color_id += 1
	
	var w = pos_max.x - pos_min.x + 1
	var h = pos_max.y - pos_min.y + 1
	
	starting_pos = -pos_min
	data = []
	for y in h:
		var line = []
		for x in w:
			line.push_back(0)
		data.push_back(line)
	#data[starting_pos.y][starting_pos.x] = 1

func _build_tileset():
	var source = TileSetAtlasSource.new()
	var tile_size = Vector2i(8, 8)
	var n_tiles = color_map.size() + 2
	var image = Image.create(tile_size.x * n_tiles, tile_size.y, false, Image.FORMAT_RGB8)
	
	# empty
	image.fill_rect(Rect2i(Vector2i.ZERO, tile_size), Color("0f0f23"))
	# inside
	image.fill_rect(Rect2i(Vector2i(tile_size.x, 0), tile_size), Color("e56520"))
	
	# trenches
	for color in color_map:
		image.fill_rect(Rect2i(Vector2i(tile_size.x * color_map[color], 0), tile_size), color)
	
	source.texture = ImageTexture.create_from_image(image)
	source.texture_region_size = tile_size
	for i in n_tiles:
		source.create_tile(Vector2i(i, 0))
	tile_map.tile_set.add_source(source)

func rerender_board():
	# Clear and add borders for visual interest
	tile_map.clear()
	print("[%d, %d]" % [data.size(), data[0].size()])
	for y in data.size():
		for x in data[0].size():
			render_at(Vector2i(x, y))
	queue_redraw()

func render_at(pos: Vector2i):
	tile_map.set_cell(0, pos, 0, Vector2i(data[pos.y][pos.x], 0), 0)

func in_bounds(pos: Vector2i) -> bool:
	return pos.y >= 0 and pos.y < data.size() and pos.x >= 0 and pos.x < data[pos.y].size()

func tick_trenches():
	if current_line < raw_data.size():
		var line = raw_data[current_line]
		var direction = get_dir(line[0])
		var times = int(line[1])
		var color = Color(line[2].substr(1, line[2].length() - 2))
		
		for _i in times:
			trench_count += 1
			current_pos += direction
			data[current_pos.y][current_pos.x] = color_map[color]
			render_at(current_pos)
		
		current_line += 1
	else:
		dig_points.push_back(current_pos + Vector2i.ONE)
		state = State.Dig

func tick_dig():
	var next: Array[Vector2i] = []
	for p in dig_points:
		if randi() % 3 != 0:
			for dir in [Vector2i.UP, Vector2i.RIGHT, Vector2i.DOWN, Vector2i.LEFT]:
				var p2 = p + dir
				if in_bounds(p2) and data[p2.y][p2.x] == 0:
					data[p2.y][p2.x] = 1
					render_at(p2)
					interior_count += 1
					next.push_back(p2)
		else:
			next.push_back(p)
	dig_points = next
	if dig_points.size() == 0:
		print(trench_count + interior_count)
		state = State.DoneDigging

func tick_once():
	if paused:
		return

	match state:
		State.Start:
			current_pos = starting_pos
			current_line = 0
			state = State.Trenches
		State.Trenches:
			for _i in trench_per_tick:
				tick_trenches()
			header_label.text = "Trenches: %d" % trench_count
		State.Dig:
			for _i in dig_per_tick:
				tick_dig()
			header_label.text = "Trenches: %d, Interior: %d" % [trench_count, interior_count]
		State.DoneDigging:
			if OS.has_feature("movie"):
				get_tree().quit()
			state = State.DoNothing


func _ready():
	_load_map()
	_extract_data()
	_build_tileset()
	rerender_board()
	
	if OS.has_feature("movie"):
		paused = false

const physics_process_per_tick = 1
var trench_per_tick = 10
var dig_per_tick = 4
var current_tick = 0
func _physics_process(_delta):
	current_tick += 1
	if current_tick > physics_process_per_tick:
		current_tick = 0
		tick_once()

func _unhandled_input(event):
	if event.is_action_pressed("start"):
		paused = !paused
