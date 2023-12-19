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
var end_total = 0

var data2: Array[Vector2i]
var p2x: Array[int]
var p2y: Array[int]
var p2x_lookup: Dictionary
var p2y_lookup: Dictionary
var display2: Dictionary

var paused = true
var state = State.Start
enum State {
	Start,
	Trenches,
	Dig,
	DoneDigging,
	Waiting,
	LongTrenches,
	Dig2,
	Done2,
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
	tile_map.clear()
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
			var tween = get_tree().create_tween()
			tween.tween_interval(1.5)
			tween.tween_property(tile_map, "modulate", Color(Color.WHITE, 0), 1)
			tween.parallel().tween_property($UI/Transition, "visible_ratio", 1, 1)
			tween.tween_interval(1.5)
			tween.tween_callback(prepare_p2)
			state = State.Waiting
		State.LongTrenches:
			for _i in trench_per_tick:
				tick_trenches2()
			header_label.text = "Trenches: %d" % trench_count
		State.Dig2:
			for _i in dig_per_tick:
				if state == State.Dig2:
					tick_dig2()
			header_label.text = "Trenches: %d, Interior: %d" % [trench_count, interior_count]
		State.Done2:
			header_label.text = "Total: %d" % end_total
			print(end_total)
			state = State.DoNothing
			
			var tween = get_tree().create_tween()
			tween.tween_interval(1)
			tween.tween_callback(end)

func end():
	if OS.has_feature("movie"):
		get_tree().quit()

func prepare_p2():
	tile_map.clear()
	tile_map.modulate = Color.WHITE
	header_label.text = ""
	$UI/Transition.visible = false
	$Camera2D.position = Vector2(2500, 2500)
	$Camera2D.zoom = Vector2(0.15, 0.15)
	parse_p2()

func parse_p2():
	current_pos = Vector2i.ZERO
	data2 = []
	for line in raw_data:
		var color: String = line[2]
		var direction = [Vector2i.RIGHT, Vector2i.DOWN, Vector2i.LEFT, Vector2i.UP][int(color[color.length() - 2])]
		var distance = color.substr(2, 5).hex_to_int()
		var pos = current_pos
		current_pos += (direction * distance)
		# So the fill can handle tight walls
		data2.push_back(pos + (direction * 10))
		
		data2.push_back(current_pos)
	
	var uniqx = {}
	var uniqy = {}
	for v in data2:
		uniqx[v.x] = 1
		uniqy[v.y] = 1
	p2x.assign(uniqx.keys())
	p2x.sort()
	p2y.assign(uniqy.keys())
	p2y.sort()
	p2x_lookup = {}
	p2y_lookup = {}
	for i in p2x.size():
		p2x_lookup[p2x[i]] = i
	for i in p2y.size():
		p2y_lookup[p2y[i]] = i
	
	display2 = {}
	for y in p2y.size():
		for x in p2x.size():
			display2[Vector2i(x, y)] = 0
	
	current_pos = Vector2i.ZERO
	trench_count = 0
	interior_count = 0
	current_line = 0
	starting_pos = Vector2i.ZERO
	color_map = {Color("cccccc"): 2, Color("e56520"): 3}
	_build_tileset()
	rerender_board2()

	$UI/Scale.visible = true
	state = State.LongTrenches

func rerender_board2():
	tile_map.clear()
	for y in p2y.size():
		for x in p2x.size():
			render_at2(Vector2i(x, y))
	queue_redraw()

func render_at2(pos: Vector2i):
	tile_map.set_cell(0, pos, 1, Vector2i(display2[pos], 0), 0)

func real_pos_to_index(pos: Vector2i) -> Vector2i:
	return Vector2i(p2x_lookup[pos.x], p2y_lookup[pos.y])

func index_to_real_pos(pos: Vector2i) -> Vector2i:
	return Vector2i(p2x[pos.x], p2y[pos.y])

func tick_trenches2():
	if current_line < data2.size():
		var next_pos = data2[current_line]
		var currenti = real_pos_to_index(current_pos)
		var nexti = real_pos_to_index(next_pos)
		
		var trench_len = (next_pos - current_pos).length()
		trench_count += trench_len
		
		var direction = (nexti - currenti).sign()
		while currenti != nexti:
			currenti += direction
			display2[currenti] = 2
			render_at2(currenti)
		current_pos = next_pos
		current_line += 1
	else:
		dig_points = [Vector2i(p2x_lookup[0], p2y_lookup[0]) + Vector2i.ONE]
		state = State.Dig2

func tick_dig2():
	var next: Array[Vector2i] = []
	for p in dig_points:
		if randi() % 3 != 0:
			for dir in [Vector2i.UP, Vector2i.RIGHT, Vector2i.DOWN, Vector2i.LEFT]:
				var p2 = p + dir
				if display2.has(p2) and display2[p2] == 0:
					display2[p2] = 3
					render_at2(p2)
					next.push_back(p2)
			var rp1 = index_to_real_pos(p)
			var rp2 = index_to_real_pos(p + Vector2i.ONE)
			var size = abs(rp2.x - rp1.x) * abs(rp2.y - rp1.y)
			interior_count += size
		else:
			next.push_back(p)
	dig_points = next
	if dig_points.size() == 0:
		fix_interior_count2()
		state = State.Done2

# Used to submit the answer. I had trouble with my fill algorithm and
# I eventually figured out I could just find the area of a polygon and found the shoelace method
func _fix_interior_shoelace():
	var cdata = [Vector2i.ZERO]
	cdata.append_array(data2)
	var a = 0
	var b = 0
	for x in cdata.size() - 1:
		a += cdata[x].x * cdata[x + 1].y
		b += cdata[x].y * cdata[x + 1].x
	end_total = (a - b + trench_count) / 2 + 1

# Requires messing with the input to create gaps everywhere the fill needs to go
func fix_interior_count2():
	interior_count = 0
	for y in p2y.size() - 1:
		for x in p2x.size() - 1:
			var pos = Vector2i(x, y)
			var pos2 = pos + Vector2i.ONE
			if display2[pos] == 3 or (display2[pos] == 2 and (display2[pos + Vector2i.RIGHT] == 3 or display2[pos + Vector2i.DOWN] == 3 or display2[pos2] == 3)):
				var rp1 = index_to_real_pos(pos)
				var rp2 = index_to_real_pos(pos2)
				var size = abs(rp2.x - rp1.x) * abs(rp2.y - rp1.y)
				interior_count += size
	end_total = interior_count + trench_count / 2 + 1


func _ready():
	_load_map()
	_extract_data()
	_build_tileset()
	rerender_board()
	
	#$Camera2D.position = Vector2(30, 40)
	#$Camera2D.zoom = Vector2(7.5, 7.5)
	#prepare_p2()
	
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
