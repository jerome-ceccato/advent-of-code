extends Node3D

@onready var input = 'res://input.txt'
@onready var grid_map = $GridMap
const MESH_LIBRARY = preload("res://mesh_library.tres")

var data: Array[Brick]
var global_brick_id = 0
var bounds_low: Vector3i
var bounds_high: Vector3i
var center_point: Vector3

const down = Vector3i(0, 0, -1)
var allPositions = {}
var supported_by = {} # [brick -> bricks that supports it]
var supports = {} # [brick -> brick that needs this brick as support]

var paused = true
var state = State.Start
enum State {
	Start,
	Falling,
	CountDesintegrate,
	CountChainReaction,
	DoNothing
}

class Brick:
	var id
	var head: Vector3i
	var tail: Vector3i
	
	var points: Array[Vector3i]
	
	func _init(line: String, gid: int):
		id = gid
		var items = line.split("~", false)
		var head_raw = items[0].split(",", false)
		var tail_raw = items[1].split(",", false)
		head = Vector3i(int(head_raw[0]), int(head_raw[1]), int(head_raw[2]))
		tail = Vector3i(int(tail_raw[0]), int(tail_raw[1]), int(tail_raw[2]))
		_calculate_points()
	
	func _calculate_points():
		points = []
		var dir = (tail - head).sign()
		var current = head
		while current != tail:
			points.push_back(current)
			current += dir
		points.push_back(tail)
	
	func _to_string():
		return "%s ~ %s" % [head, tail]

func _next_id():
	var ret = global_brick_id
	global_brick_id += 1
	return ret

func _load_map(): 
	var contents = FileAccess.get_file_as_string(input)
	data.assign(Array(contents.split("\n", false)).map(
		func (line): return Brick.new(line, _next_id())
	))
	data.sort_custom(func (a, b): return min(a.head.z, a.tail.z) < min(b.head.z, b.tail.z))

func render_many(points: Array[Vector3i], id: int):
	for p in points:
		render_one(p, id)

func render_one(point: Vector3i, id: int):
	if id == grid_map.INVALID_CELL_ITEM:
		grid_map.set_cell_item(point, id)
	else:
		grid_map.set_cell_item(point, id % MESH_LIBRARY.get_item_list().size())

func render_all():
	grid_map.clear()
	
	for brick in data:
		for point in brick.points:
			render_one(point, brick.id)

func can_fall(brick: Brick):
	# Horizontal
	if brick.head.z == brick.tail.z:
		for point in brick.points:
			if point.z <= 1 or grid_map.get_cell_item(point + down) != grid_map.INVALID_CELL_ITEM:
				return false
		return true
	# Vertical
	else:
		var point = brick.head if brick.head.z < brick.tail.z else brick.tail
		return point.z > 1 and grid_map.get_cell_item(point + down) == grid_map.INVALID_CELL_ITEM

func try_fall_once():
	var has_fallen = false
	for brick in data:
		if can_fall(brick):
			var old_pos = brick.points.duplicate()
			for i in brick.points.size():
				brick.points[i].z -= 1
			brick.head.z -= 1
			brick.tail.z -= 1
			render_many(old_pos, grid_map.INVALID_CELL_ITEM)
			render_many(brick.points, brick.id)
			has_fallen = true
	if not has_fallen:
		state = State.CountDesintegrate

func can_desintegrate(brick: Brick, supported_by: Dictionary, supports: Dictionary):
	for other in supports[brick.id].keys():
		if supported_by[other].size() < 2:
			return false
	return true

func _map_support():
	for brick in data:
		supports[brick.id] = {}
		supported_by[brick.id] = {}
		for p in brick.points:
			allPositions[p] = brick.id
	
	for brick in data:
		for p in brick.points:
			var below = p + down
			if allPositions.has(below) and allPositions[below] != brick.id:
				var id = allPositions[below]
				supported_by[brick.id][id] = 1
				supports[id][brick.id] = 1

func count_desintegrate():
	var desintegration_count = 0
	for brick in data:
		if can_desintegrate(brick, supported_by, supports):
			desintegration_count += 1
	print(desintegration_count)

func should_fall(brick_id: int, falling: Dictionary):
	if supported_by[brick_id].size() == 0:
		return false # On the ground
	for supp in supported_by[brick_id]:
		if not falling.has(supp):
			return false
	return true

func get_supported_deep(initial: Brick, memo: Dictionary) -> Dictionary:
	var would_fall = {initial.id: 1}
	var last_count = 0
	while would_fall.size() != last_count:
		last_count = would_fall.size()
		for brick in data:
			if not would_fall.has(brick.id):
				if should_fall(brick.id, would_fall):
					would_fall[brick.id] = 1
					if memo.has(brick.id):
						would_fall.merge(memo[brick.id]) 
	return would_fall

func count_chain_reaction():
	var rev = data.duplicate()
	var memo = {}
	rev.reverse()
	for brick in rev:
		memo[brick.id] = get_supported_deep(brick, memo)
	var total = 0
	for key in memo:
		total += memo[key].size() - 1 # Remove itself
	print(total)

func tick_once():
	if paused:
		return

	match state:
		State.Start:
			state = State.Falling
		State.Falling:
			for _i in fall_per_tick:
				if state == State.Falling:
					try_fall_once()
		State.CountDesintegrate:
			_map_support()
			count_desintegrate()
			state = State.CountChainReaction
		State.CountChainReaction:
			count_chain_reaction()
			state = State.DoNothing
		State.DoNothing:
			if OS.has_feature("movie"):
				get_tree().quit() 

func _calculate_bounds():
	bounds_low = Vector3i.ZERO
	bounds_high = Vector3i.ZERO
	for brick in data:
		for p in [brick.head, brick.tail]:
			bounds_low.x = min(bounds_low.x, p.x)
			bounds_low.y = min(bounds_low.y, p.y)
			bounds_low.z = min(bounds_low.z, p.z)
			bounds_high.x = max(bounds_high.x, p.x)
			bounds_high.y = max(bounds_high.y, p.y)
			bounds_high.z = max(bounds_high.z, p.z)
	center_point = Vector3((bounds_high.x - bounds_low.x) / 2.0, (bounds_high.y - bounds_low.y) / 2.0, (bounds_high.z - bounds_low.z) / 2.0)

func _ready():
	_load_map()
	_calculate_bounds()
	render_all()
	
	#var pos = Vector3(5.037897, -3.927676, 1.613264)
	#var rot = Vector3(15.74983, 179.5, 180)
	var pos = Vector3(5.741437, -207.1741, 89.25851)
	var rot = Vector3(90, 0.249998, 0)
	$Camera3D.position = pos
	$Camera3D.rotation_degrees = rot
	#$Camera3D.look_at_from_position(center_point - Vector3(0, 50, 50), center_point, Vector3.BACK)
	
	if OS.has_feature("movie"):
		paused = false

const physics_process_per_tick = 1
var fall_per_tick = 1
var current_tick = 0
func _physics_process(_delta):
	current_tick += 1
	if current_tick > physics_process_per_tick:
		current_tick = 0
		tick_once()

func _input(event):
	if event.is_action_pressed("start"):
		print("start")
		paused = !paused
