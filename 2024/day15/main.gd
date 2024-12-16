extends Node2D

# Assets from https://snowhex.itch.io/dungeon-gathering

@onready var input = 'res://input.txt'
@onready var floor: TileMapLayer = $Floor
@onready var walls: TileMapLayer = $Walls
@onready var entities: TileMapLayer = $Entities

var map_size: Vector2i
var map: Dictionary # [Vector2i: Entity]
var robot: Vector2i

var actions: String
var actionp := 0

func _load_map(enlarged: bool):
	var contents = FileAccess.get_file_as_string(input)
	var blocks = contents.split("\n\n")
	
	var lines = blocks[0].split("\n")
	map_size = Vector2i(lines[0].length() * (2 if enlarged else 1), lines.size())
	map = {}
	for y in lines.size():
		for x in lines[y].length():
			if lines[y][x] == '@':
				robot = Vector2i(x * (2 if enlarged else 1), y)
			elif lines[y][x] != '.':
				if enlarged:
					match lines[y][x]:
						'#':
							map[Vector2i(x * 2, y)] = '#'
							map[Vector2i(x * 2 + 1, y)] = '#'
						'O':
							map[Vector2i(x * 2, y)] = '['
							map[Vector2i(x * 2 + 1, y)] = ']'
				else:
					map[Vector2i(x, y)] = lines[y][x]
	actions = blocks[1].replace("\n", "")

func _clear_entity(coords: Vector2i):
	entities.erase_cell(coords)

func _render_entity(coords: Vector2i):
	if coords == robot:
		entities.set_cell(coords, 1, Vector2i(3, 1))
		return
	
	match map[coords]:
		'#':
			var atlas_coords = Vector2i(3, 2)
			if coords.y == 0:
				if coords.x == 0:
					atlas_coords = Vector2i(0, 0)
				elif coords.x == (map_size.x - 1):
					atlas_coords = Vector2i(2, 0)
				else:
					atlas_coords = Vector2i(1, 0)
			elif coords.y == (map_size.y - 1):
				if coords.x == 0:
					atlas_coords = Vector2i(0, 2)
				elif coords.x == (map_size.x - 1):
					atlas_coords = Vector2i(2, 2)
				else:
					atlas_coords = Vector2i(1, 2)
			elif coords.x == 0:
				atlas_coords = Vector2i(0, 1)
			elif coords.x == (map_size.x - 1):
				atlas_coords = Vector2i(2, 1)
			walls.set_cell(coords, 1, atlas_coords)
		'O':
			entities.set_cell(coords, 1, Vector2i(3, 0))
		'[':
			entities.set_cell(coords, 1, Vector2i(0, 3))
		']':
			entities.set_cell(coords, 1, Vector2i(1, 3))

func _render_all():
	entities.clear()
	for y in map_size.y:
		for x in map_size.x:
			var coords = Vector2i(x, y)
			floor.set_cell(coords, 1, Vector2i(1, 1))
			if map.has(coords) or coords == robot:
				_render_entity(coords)

func _get_direction(instruction) -> Vector2i:
	var id = "^>v<".find(instruction)
	return [Vector2i.UP, Vector2i.RIGHT, Vector2i.DOWN, Vector2i.LEFT][id]

func move_large_box(pos: Vector2i, dir: Vector2i):
	if map[pos] == '[':
		map[pos + dir] = '['
		map[pos + Vector2i.RIGHT + dir] = ']'
		map.erase(pos)
		map.erase(pos + Vector2i.RIGHT)
	else:
		map[pos + dir] = ']'
		map[pos + Vector2i.LEFT + dir] = '['
		map.erase(pos)
		map.erase(pos + Vector2i.LEFT)

func push_large_box_h(box_to_move: Vector2i, dir: Vector2i) -> bool:
	var target = box_to_move + dir
	while map.has(target) and (map[target] == '[' or map[target] == ']'):
		target += dir
	if map.has(target):
		return false # wall
	
	while target != box_to_move:
		map[target] = map[target - dir]
		target -= dir
	map.erase(box_to_move)
	return true

func push_large_box(box_to_move: Vector2i, dir: Vector2i) -> bool:
	if dir.x != 0:
		return push_large_box_h(box_to_move, dir)
	
	var other = box_to_move + (Vector2i.RIGHT if map[box_to_move] == '[' else Vector2i.LEFT)
	var a = map[box_to_move + dir] if map.has(box_to_move + dir) else '.'
	var b = map[other + dir] if map.has(other + dir) else '.'
	
	if a == '.' and b == '.':
		move_large_box(box_to_move, dir)
		return true
	elif a == '#' or b == '#':
		return false
	else:
		# Try to move other boxes
		if a == '[' or a == ']':
			if !push_large_box(box_to_move + dir, dir):
				return false
			b = map[other + dir] if map.has(other + dir) else '.'
		if b == '[' or b == ']':
			if !push_large_box(other + dir, dir):
				return false
		move_large_box(box_to_move, dir)
	return true

func move_and_render() -> bool:
	if actionp >= actions.length():
		return false
	var dir = _get_direction(actions[actionp])
	actionp += 1

	var target = robot + dir
	if map.has(target):
		if map[target] == 'O':
			var next_non_box = target + dir
			while map.has(next_non_box) and map[next_non_box] == 'O':
				next_non_box += dir
			if !map.has(next_non_box):
				map[next_non_box] = 'O'
				map.erase(target)
				_clear_entity(robot)
				robot = target
				_render_entity(next_non_box)
				_render_entity(robot)
		elif map[target] == '[' or map[target] == ']':
			var yolo = map.duplicate() # we need to revert in case pushing failed, because we could have started moving some boxes
			if push_large_box(target, dir):
				robot = target
				_render_all()
			else:
				map = yolo
	else:
		_clear_entity(robot)
		robot = target
		_render_entity(robot)
	return true

func calc_gps() -> int:
	var total = 0
	for pos in map.keys():
		if map[pos] == 'O' or map[pos] == '[':
			total += pos.x + pos.y * 100
	return total

func _ready() -> void:
	_load_map(true)
	_render_all()

var paused = true
var ticks_per_process = 1
var done = false
func _physics_process(_delta):
	if !paused and !done:
		for i in ticks_per_process:
			paused=true
			if !move_and_render():
				print(calc_gps())
				done = true
				return

func _unhandled_input(event):
	if event.is_action_pressed("start"):
		paused = !paused
