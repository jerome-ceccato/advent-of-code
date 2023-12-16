extends Node2D

@onready var input = 'res://input.txt'
@onready var header_label = $UI/Header
@onready var beam_tile_map = $BeamTileMap
@onready var board_tile_map = $BoardTileMap

var data: Array[String]
var beams: Array[Beam]
var global_beam_id = 0
var beam_colors = [Color.AQUA, Color.BISQUE, Color.CORAL]

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

func build_beam_tilesets():
	var base_beam_color = Color("d95763")
	
	
	var base_image: Image = load("res://sprites.png").get_image()
	var images: Array[Image] = []
	for _c in beam_colors:
		images.push_back(base_image.duplicate(true))
	
	# Tint images
	for y in base_image.get_height():
		for x in base_image.get_width():
			if base_image.get_pixel(x, y) == base_beam_color:
				for i in beam_colors.size():
					images[i].set_pixel(x, y, beam_colors[i])
	
	for image in images:
		var source = TileSetAtlasSource.new()
		source.texture = ImageTexture.create_from_image(image)
		source.texture_region_size = Vector2i(8, 8)
		for y in [1, 2]:
			for x in [0,1,2,3,4]:
				source.create_tile(Vector2i(x, y))
		beam_tile_map.tile_set.add_source(source)

func rerender_board():
	# Clear and add borders for visual interest
	for y in data.size() + 2:
		for x in data[0].length() + 2:
			board_tile_map.set_cell(0, Vector2i(x-1, y-1), 0, Vector2i(5, 0), 0)
	
	for y in data.size():
		for x in data[y].length():
			var tile_id = "./\\|-".find(data[y][x])
			board_tile_map.set_cell(0, Vector2i(x, y), 0, Vector2i(tile_id, 0), 0)
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
	var source = beam.id % (beam_colors.size() + 1)
	beam_tile_map.set_cell(0, head.pos, source, atlas_id(head), 0)

func in_bounds(pos):
	return pos.y >= 0 and pos.y < data.size() and pos.x >= 0 and pos.x < data[pos.y].length()

func is_new_path(point: BeamPoint) -> bool:
	for beam in beams:
		for other in beam.body:
			if other.pos == point.pos and other.direction == point.direction:
				return false
	return true

func tick_once():
	if paused:
		return
	match state:
		State.Start:
			beams = [make_beam(BeamPoint.new(Vector2i.ZERO, Vector2i.RIGHT))]
			state = State.Fill
		State.Fill:
			for _i in beam_per_tick:
				tick_beams()
		State.BeamsStopped:
			state = State.DoNothing

func tick_beams():
	var new_beams: Array[Beam] = []
	var active_count = 0
	
	for beam in beams:
		if beam.active:
			active_count += 1
			render_beam_head(beam)
			var head = beam.body.back()
			
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
	
	print("ticking %d beams (%d)" % [active_count, beams.size()])
	beams.append_array(new_beams)
	if active_count == 0:
		state = State.BeamsStopped

# Called when the node enters the scene tree for the first time.
func _ready():
	_load_map()
	build_beam_tilesets()
	rerender_board()
	

const process_per_tick = 10
const beam_per_tick = 1
var current_tick = 0
func _physics_process(_delta):
	current_tick += 1
	if current_tick > process_per_tick:
		current_tick = 0
		tick_once()

func _unhandled_input(event):
	if event.is_action_pressed("start"):
		paused = !paused
