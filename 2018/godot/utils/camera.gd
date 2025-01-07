extends Camera2D

const ZOOM_MIN = Vector2(0.1, 0.1)
const ZOOM_MAX = Vector2(10, 10)

var _drag_original_pos
var _mouse_start_pos
var _dragging = false

func _apply_zoom(new_zoom: Vector2):
	if new_zoom.x < ZOOM_MIN.x or new_zoom.y < ZOOM_MIN.y:
		zoom = ZOOM_MIN
	elif new_zoom.x > ZOOM_MAX.x or new_zoom.y > ZOOM_MAX.y:
		zoom = ZOOM_MAX
	else:
		zoom = new_zoom
	#print(zoom)

func _unhandled_input(event):
	if event.is_action_pressed("zoom_in"):
		_apply_zoom(zoom / 1.5)
	elif event.is_action_pressed("zoom_out"):
		_apply_zoom(zoom * 1.5)
	elif event.is_action("drag"):
		if event.is_pressed():
			_mouse_start_pos = event.position
			_drag_original_pos = position
			_dragging = true
		else:
			_dragging = false
	elif event is InputEventMouseMotion and _dragging:
		position = Vector2(1.0/zoom.x, 1.0/zoom.y) * (_mouse_start_pos - event.position) + _drag_original_pos
		#print(position)
