from os import makedirs
from os.path import isfile, join, exists, dirname
from shutil import copy
from dataclasses import dataclass
from datetime import datetime

base_path = "."
verbose = True

@dataclass
class AocDay:
    year: int
    day: int

    # The default name is 'input', but there are some exceptions due to language restrictions
    def public_input_filename(self):
        # Swift year
        if self.year == 2021:
            return f'day{self.day:02d}.txt'
        
        # Godot days
        if exists(join(base_path, str(self.year), f'day{self.day:02d}', 'main.gd')):
            return 'input.txt'

        return 'input'

    def public_input_path(self):
        return join(base_path, str(self.year), f'day{self.day:02d}', self.public_input_filename())

    def private_input_path(self):
        return join(base_path, 'inputs', str(self.year), f'day{self.day:02d}')

def trace(any):
    if verbose:
        print(any)

def deep_copy(from_file, to_file):
    pass
    #makedirs(dirname(to_file), exist_ok=True)
    #copy(from_file, to_file)

def sync_day(day: AocDay):
    if exists(day.public_input_path()):
        if exists(day.private_input_path()):
            trace(f'{day.year}/day{day.day:02d} is already synced')
        else:
            deep_copy(day.public_input_path(), day.private_input_path())
            print(f'Exported {day.public_input_path()} to {day.private_input_path()}')
    else:
        if exists(day.private_input_path()):
            deep_copy(day.private_input_path(), day.public_input_path())
            print(f'Imported {day.public_input_path()} from {day.private_input_path()}')
        else:
            trace(f'No input file available for {day.year}/day{day.day:02d}')


def foreach_aoc_day(fct):
    for year in range(2015, datetime.now().year + 1):
        year_path = join(base_path, str(year))
        if exists(year_path) and not isfile(year_path):
            for day in range(25 + 1):
                day_path = join(year_path, f'day{day:02d}')
                if exists(day_path) and not isfile(day_path):
                    fct(AocDay(year, day))
        else:
            trace(f'No {year} folder')

if __name__ == "__main__":
    foreach_aoc_day(sync_day)