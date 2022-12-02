import re

class Guard:
    def __init__(self, identifier):
        self.identifier = identifier
        self.total = 0
        self.minutes = [0] * 60

def readfile(filename):
    with open(filename) as file:
        return [line.rstrip('\n') for line in file]

def extract(line):
    return re.match(r'\[\d{4}-\d{2}-\d{2}\s+\d{2}:(\d{2})\]\s+((wakes up)|(falls asleep)|(Guard #(\d+) begins shift))', line).groups()

def main():
    shifts = sorted(readfile('input'))
    guards = {}

    i = 0
    while i < len(shifts):
        data = extract(shifts[i])
        identifier = int(data[5])
        if identifier not in guards:
            guards[identifier] = Guard(identifier)
        current = guards[identifier]

        i += 1
        while i < len(shifts):
            data = extract(shifts[i])
            if data[1].startswith('G'):
                break

            start = int(data[0])
            end = int(extract(shifts[i + 1])[0])

            current.total += end - start
            for j in range(start, end):
                current.minutes[j] += 1

            i += 2

    most_asleep = Guard(-1)
    for guard in guards.values():
        if guard.total > most_asleep.total:
            most_asleep = guard

    biggest_minute = 0
    for minute in range(60):
        if most_asleep.minutes[minute] > most_asleep.minutes[biggest_minute]:
            biggest_minute = minute

    print(most_asleep.identifier * biggest_minute)

main()
