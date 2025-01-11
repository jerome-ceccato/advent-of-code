from os.path import exists
import subprocess

if __name__ == "__main__":
    for day in range(10, 25 + 1):
        real_input_path = f'day{day:02d}/input'
        godot_link = f'godot/input/day{day:02d}.txt'
        if exists(real_input_path) and not exists(godot_link):
            relative_input_path = '../../' + real_input_path
            subprocess.call(['ln', '-s', relative_input_path, godot_link])
            print(f'linked {real_input_path} -> {godot_link}')
