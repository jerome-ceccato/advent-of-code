defmodule Point do
  defstruct x: 0, y: 0

  def compare(lhs, rhs) do
    lhs.x == rhs.x && lhs.y == rhs.y
  end

  def add(lhs, rhs) do
    %Point{x: lhs.x + rhs.x, y: lhs.y + rhs.y}
  end
end

defmodule Round do
  defstruct [:elves, :directions]
end

defmodule Aoc do
  def parse_input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, y} ->
      String.codepoints(line)
      |> Enum.with_index()
      |> Enum.flat_map(fn {c, x} ->
        case c do
          "#" -> [%Point{x: x, y: y}]
          _ -> []
        end
      end)
    end)
  end

  def offsets_at(directions) do
    Enum.reduce(directions, MapSet.new(), fn dir, set ->
      case dir do
        :n ->
          MapSet.put(set, %Point{x: -1, y: -1})
          |> MapSet.put(%Point{x: 0, y: -1})
          |> MapSet.put(%Point{x: 1, y: -1})

        :s ->
          MapSet.put(set, %Point{x: -1, y: 1})
          |> MapSet.put(%Point{x: 0, y: 1})
          |> MapSet.put(%Point{x: 1, y: 1})

        :w ->
          MapSet.put(set, %Point{x: -1, y: -1})
          |> MapSet.put(%Point{x: -1, y: 0})
          |> MapSet.put(%Point{x: -1, y: 1})

        :e ->
          MapSet.put(set, %Point{x: 1, y: -1})
          |> MapSet.put(%Point{x: 1, y: 0})
          |> MapSet.put(%Point{x: 1, y: 1})
      end
    end)
  end

  def move_elf(elf, direction) do
    offset =
      case direction do
        :n -> %Point{x: 0, y: -1}
        :s -> %Point{x: 0, y: 1}
        :w -> %Point{x: -1, y: 0}
        :e -> %Point{x: 1, y: 0}
      end

    Point.add(elf, offset)
  end

  def elves_dict(elves) do
    Enum.reduce(elves, MapSet.new(), fn a, set ->
      MapSet.put(set, a)
    end)
  end

  def elves_contain(elves, elf) do
    Enum.find_index(elves, fn e ->
      Point.compare(e, elf)
    end) != nil
  end

  def has_elves_at(elves_dict, elf, offsets) do
    Enum.find_index(offsets, fn offset ->
      MapSet.member?(elves_dict, Point.add(elf, offset))
    end) != nil
  end

  def gen_proposals(elves, directions) do
    dict = elves_dict(elves)

    Enum.map(elves, fn elf ->
      if !has_elves_at(dict, elf, offsets_at(directions)) do
        elf
      else
        Enum.find_value(directions, elf, fn dir ->
          if !has_elves_at(dict, elf, offsets_at([dir])) do
            move_elf(elf, dir)
          end
        end)
      end
    end)
  end

  def compile_proposals(proposals) do
    Enum.reduce(proposals, %{}, fn p, dict ->
      Map.put(dict, p, Map.get(dict, p, 0) + 1)
    end)
  end

  def apply_proposals(elves, proposals) do
    compiled = compile_proposals(proposals)

    Enum.with_index(elves)
    |> Enum.map(fn {elf, i} ->
      p = Enum.at(proposals, i)

      if Map.get(compiled, p, 0) == 1, do: p, else: elf
    end)
  end

  def run(elves, rounds) do
    Enum.reduce(1..rounds, %Round{elves: elves, directions: [:n, :s, :w, :e]}, fn _, round ->
      %Round{
        elves: apply_proposals(round.elves, gen_proposals(round.elves, round.directions)),
        directions: tl(round.directions) ++ [hd(round.directions)]
      }
    end).elves
  end

  def full_run(round, n) do
    next = %Round{
      elves: apply_proposals(round.elves, gen_proposals(round.elves, round.directions)),
      directions: tl(round.directions) ++ [hd(round.directions)]
    }

    if round.elves == next.elves do
      n
    else
      full_run(next, n + 1)
    end
  end

  def run_until_done(elves) do
    full_run(%Round{elves: elves, directions: [:n, :s, :w, :e]}, 1)
  end

  def get_bounds(elves) do
    lower =
      Enum.reduce(elves, hd(elves), fn elf, mn ->
        %Point{x: min(elf.x, mn.x), y: min(elf.y, mn.y)}
      end)

    upper =
      Enum.reduce(elves, hd(elves), fn elf, mx ->
        %Point{x: max(elf.x, mx.x), y: max(elf.y, mx.y)}
      end)

    {lower, upper}
  end

  def count_empty(elves) do
    {lower, upper} = get_bounds(elves)

    Enum.reduce(lower.y..upper.y, 0, fn y, acc ->
      Enum.reduce(lower.x..upper.x, acc, fn x, acc2 ->
        if elves_contain(elves, %Point{x: x, y: y}), do: acc2, else: acc2 + 1
      end)
    end)
  end
end

part1 = Aoc.run(Aoc.parse_input("input"), 10)
IO.inspect(Aoc.count_empty(part1))

part2 = Aoc.run_until_done(Aoc.parse_input("input"))
IO.inspect(part2)
