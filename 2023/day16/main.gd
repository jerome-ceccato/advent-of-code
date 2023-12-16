extends Node2D

@onready var input = 'res://input.txt'
@onready var header_label = $UI/Header
@onready var progress_label = $UI/Progress
@onready var best_label = $UI/Best
@onready var tile_map = $TileMap

var data: Array[String]
var beams: Array[Beam]
var global_beam_id = 0
var found_positions: Dictionary

var best = 0

var paused = true
var state = State.Start
enum State {
	Start,
	Fill,
	BeamsStopped,
	DoNothing
}

class BeamPoint:
	var pos: Vector2i
	var direction: Vector2i
	
	func _init(ipos: Vector2i, idir: Vector2i):
		pos = ipos
		direction = idir
	
	func dir_id() -> int:
		return [Vector2i.UP, Vector2i.RIGHT, Vector2i.DOWN, Vector2i.LEFT].find(direction)
	
	func _to_string() -> String:
		var direction_s = ["^", ">", "v", "<"][dir_id()]
		return "%s %s" % [pos, direction_s]
	

func make_beam(point: BeamPoint) -> Beam:
	var beam = Beam.new(point, global_beam_id)
	global_beam_id += 1
	return beam

class Beam:
	var active: bool
	var id: int
	var body: Array[BeamPoint]
	
	func _init(point: BeamPoint, beam_id: int):
		active = true
		id = beam_id
		body = [point]
	
	func _to_string() -> String:
		var active_s = " [active]" if active else ""
		var head = body.back()
		return "%s%s" % [head, active_s]

func _load_map(): 
	var contents = FileAccess.get_file_as_string(input)
	data.assign(Array(contents.split("\n", false)))

func _setup_layers():
	var color_codes = ["73464c", "ab5675", "ee6a7c", "ffa7a5", "ffe07e", "ffe7d6", "72dcbb", "34acba"]

	for code in color_codes:
		tile_map.add_layer(-1)
		tile_map.set_layer_modulate(-1, Color(code))

func rerender_board():
	# Clear and add borders for visual interest
	tile_map.clear()
	for y in data.size() + 2:
		for x in data[0].length() + 2:
			tile_map.set_cell(0, Vector2i(x-1, y-1), 0, Vector2i(5, 0), 0)
	
	for y in data.size():
		for x in data[y].length():
			var tile_id = "./\\|-".find(data[y][x])
			tile_map.set_cell(0, Vector2i(x, y), 0, Vector2i(tile_id, 0), 0)
	queue_redraw()

func atlas_id(head: BeamPoint) -> Vector2i:
	match data[head.pos.y][head.pos.x]:
		'.':
			return Vector2i(0, [2, 1, 2, 1][head.dir_id()])
		'/':
			return Vector2i(1, [2, 1, 1, 2][head.dir_id()])
		'\\':
			return Vector2i(2, [2, 2, 1, 1][head.dir_id()])
		'|':
			if head.dir_id() in [0, 2]:
				# Straight
				return Vector2i(0, [2, 1, 2, 1][head.dir_id()])
			else:
				# Split
				return Vector2i(3, [0, 1, 0, 2][head.dir_id()])
		'-':
			if head.dir_id() in [1, 3]:
				# Straight
				return Vector2i(0, [2, 1, 2, 1][head.dir_id()])
			else:
				# Split
				return Vector2i(4, [2, 0, 1, 0][head.dir_id()])
	return Vector2i.ZERO

func render_beam_head(beam: Beam):
	var head = beam.body.back()
	var layer = 1 + (beam.id % (tile_map.get_layers_count() - 1))
	tile_map.set_cell(layer, head.pos, 0, atlas_id(head), 0)

func in_bounds(pos):
	return pos.y >= 0 and pos.y < data.size() and pos.x >= 0 and pos.x < data[pos.y].length()

func is_new_path(point: BeamPoint) -> bool:
	return not (found_positions.has(point.pos) and found_positions[point.pos].has(point.direction))

var start_points: Array[Beam] = []
var current_start = 0
func prepare_starting_points():
	var first: Array[Beam] = []
	var second: Array[Beam] = []
	
	for y in data.size():
		start_points.push_back(make_beam(BeamPoint.new(Vector2i(0, y), Vector2i.RIGHT)))
		first.push_back(make_beam(BeamPoint.new(Vector2i(data[y].length() - 1, y), Vector2i.LEFT)))
	
	for x in data[0].length():
		start_points.push_back(make_beam(BeamPoint.new(Vector2i(x, data.size() - 1), Vector2i.UP)))
		second.push_back(make_beam(BeamPoint.new(Vector2i(x, 0), Vector2i.DOWN)))
	
	first.reverse()
	second.reverse()
	start_points.append_array(first)
	start_points.append_array(second)

func tick_once():
	if paused:
		return
	for _i in beam_per_tick:
		match state:
			State.Start:
				beams = [start_points[current_start]]
				global_beam_id = 0
				found_positions.clear()
				current_start += 1
				rerender_board()
				
				if OS.has_feature("movie") and current_start > 1:
					get_tree().quit()
				tile_map.set_cell(0, beams[0].body[0].pos - beams[0].body[0].direction, 0, Vector2i(5, 1), 0)
				progress_label.text = "Progress: %d/%d" % [current_start, start_points.size()]
				state = State.Fill
			State.Fill:
				tick_beams()
				header_label.text = "Energized: %d" % found_positions.size()
			State.BeamsStopped:
				if current_start >= start_points.size():
					state = State.DoNothing
				else:
					best = max(best, found_positions.size())
					best_label.text = "Best: %d" % best
					state = State.Start

func tick_beams():
	var new_beams: Array[Beam] = []
	var active_count = 0
	
	for beam in beams:
		if beam.active:
			active_count += 1
			
			render_beam_head(beam)
			var head = beam.body.back()
			
			if found_positions.has(head.pos):
				if found_positions[head.pos].has(head.direction):
					found_positions[head.pos][head.direction] += 1
				else:
					found_positions[head.pos][head.direction] = 1
			else:
				found_positions[head.pos] = {head.direction: 1}
			
			if (data[head.pos.y][head.pos.x] == '|' and head.dir_id() in [1, 3]) or (data[head.pos.y][head.pos.x] == '-' and head.dir_id() in [0, 2]):
				# Split
				var targets: Array[BeamPoint] = []
				if data[head.pos.y][head.pos.x] == '|':
					targets.push_back(BeamPoint.new(head.pos + Vector2i.UP, Vector2i.UP))
					targets.push_back(BeamPoint.new(head.pos + Vector2i.DOWN, Vector2i.DOWN))
				else:
					targets.push_back(BeamPoint.new(head.pos + Vector2i.LEFT, Vector2i.LEFT))
					targets.push_back(BeamPoint.new(head.pos + Vector2i.RIGHT, Vector2i.RIGHT))
				
				for point in targets:
					if in_bounds(point.pos) and is_new_path(point):
						new_beams.append(make_beam(point))
				beam.active = false
			else:
				# Straight
				var next_dir
				match data[head.pos.y][head.pos.x]:
					'.':
						next_dir = head.direction
					'/':
						next_dir = [Vector2i.RIGHT, Vector2i.UP, Vector2i.LEFT, Vector2i.DOWN][head.dir_id()]
					'\\':
						next_dir = [Vector2i.LEFT, Vector2i.DOWN, Vector2i.RIGHT, Vector2i.UP][head.dir_id()]
					'|':
						next_dir = head.direction
					'-':
						next_dir = head.direction
				var next_pos = head.pos + next_dir
				var next_point = BeamPoint.new(next_pos, next_dir)
				if in_bounds(next_point.pos) and is_new_path(next_point):
					beam.body.push_back(next_point)
				else:
					beam.active = false
	
	beams.append_array(new_beams)
	if active_count == 0:
		state = State.BeamsStopped

# Called when the node enters the scene tree for the first time.
func _ready():
	_load_map()
	_setup_layers()
	prepare_starting_points()
	rerender_board()
	if OS.has_feature("movie"):
		beam_per_tick = 2
		paused = false

const process_per_tick = 1
var beam_per_tick = 20
var current_tick = 0
func _physics_process(_delta):
	current_tick += 1
	if current_tick > process_per_tick:
		current_tick = 0
		tick_once()

func _unhandled_input(event):
	if event.is_action_pressed("start"):
		paused = !paused
