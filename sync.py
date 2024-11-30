from os import makedirs
from os.path import isfile, join, exists, dirname
from shutil import copy
from dataclasses import dataclass
from datetime import datetime
import subprocess

base_path = "."
# verbose also logs when no changes are made
verbose = False

# Change these functions to define a different folder structure
def public_year_path(year: int):
    return join(base_path, str(year))

def public_day_path(year: int, day: int):
    return join(public_year_path(year), f'day{day:02d}')

def public_input_filename(year: int, day: int):
    # Swift year
    if year == 2021:
        return f'day{day:02d}.txt'
    
    # Godot days
    if exists(join(public_day_path(year, day), 'main.gd')):
        return 'input.txt'

    return 'input'

def private_input_path(year: int, day: int):
    return join(base_path, 'inputs', str(year), f'day{day:02d}')


@dataclass
class AocDay:
    year: int
    day: int

    def public_input_path(self):
        return join(public_day_path(self.year, self.day), public_input_filename(self.year, self.day))

    def private_input_path(self):
        return private_input_path(self.year, self.day)

def trace(any):
    if verbose:
        print(any)

def deep_copy(from_file, to_file):
    makedirs(dirname(to_file), exist_ok=True)
    copy(from_file, to_file)

def sync_day(day: AocDay):
    if exists(day.public_input_path()):
        if exists(day.private_input_path()):
            trace(f'{day.public_input_path()} is already synced')
        else:
            deep_copy(day.public_input_path(), day.private_input_path())
            print(f'Exported {day.public_input_path()} to {day.private_input_path()}')
    else:
        if exists(day.private_input_path()):
            deep_copy(day.private_input_path(), day.public_input_path())
            print(f'Imported {day.public_input_path()} from {day.private_input_path()}')
        else:
            trace(f'No input file available for {day.public_input_path()}')

def delete_tracked_inputs(day: AocDay):
    subprocess.call(['git', 'rm', '--cached', day.public_input_path()[2:]])

def foreach_aoc_day(fct):
    for year in range(2015, datetime.now().year + 1):
        year_path = public_year_path(year)
        if exists(year_path) and not isfile(year_path):
            for day in range(25 + 1):
                day_path = public_day_path(year, day)
                if exists(day_path) and not isfile(day_path):
                    fct(AocDay(year, day))
        else:
            trace(f'No {year_path} folder')

if __name__ == "__main__":
    foreach_aoc_day(sync_day)
    #foreach_aoc_day(delete_tracked_inputs)
