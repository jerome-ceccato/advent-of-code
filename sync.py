# A small tool to help keep input files in git, but in a private submodule
# The tool does a two-way sync between the public repo (with gitignored input files) and the private repo which holds the actual files
# That way, you can easily grab all input files after a clean clone of the repo, or copy over all new inputs to the private submodule for later retrieval

# If you've already committed your input files, follow these steps:
# - Create a private repo and add it as a submodule in your public one (this script expects the name "inputs" for the submodule)
# - Edit the functions at the top of the script to represent your repo structure
# - Run the script
# - Commit the input files in the private repo
# - Add a .gitignore rule for your input files in the public repo
# - Delete all tracked input files. You can do this by uncommenting "foreach_aoc_day(delete_tracked_inputs)" in this script
# - Commit and push your changes
# - Use a tool like BFG repo-cleaner to rewrite the whole repo history without the input files
# - Do a clean clone of your repo, and run this script to reimport the input files
# You now have all your input files as before, but they are no longer available on your public repo!
# Each time you add new input files, run the script again and commit the changes in your private repo

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
# This repo uses `{year}/day{dd}/input` with some exceptions
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
