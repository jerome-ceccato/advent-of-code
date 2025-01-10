extends Control

@onready var grid_container: GridContainer = $MarginContainer/VBoxContainer/GridContainer

func _ready() -> void:
	for button in grid_container.get_children():
		if button is DayButton:
			button.pressed.connect(_on_day_pressed.bind(button.day_scene))

func _on_day_pressed(scene: PackedScene):
	get_tree().change_scene_to_packed(scene)

func _unhandled_input(event: InputEvent) -> void:
	if event.is_action_pressed("exit"):
		get_tree().quit()
