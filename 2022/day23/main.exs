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

  def elves_contain(elves, elf) do
    Enum.find_index(elves, fn e ->
      Point.compare(e, elf)
    end) != nil
  end

  def has_elves_at(elves, elf, offsets) do
    Enum.find_index(offsets, fn offset ->
      elves_contain(elves, Point.add(elf, offset))
    end) != nil
  end

  # def has_elves_around(elves, elf) do
  #  Enum.find_index(offsets_at([:n, :s, :w, :e]), fn offset ->
  #    Enum.find_index(fn e ->
  #      Point.compare(e, Point.add(elf, offset))
  #    end) != nil
  #  end) != nil
  # end

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

  def gen_proposals(elves, directions) do
    Enum.map(elves, fn elf ->
      if !has_elves_at(elves, elf, offsets_at(directions)) do
        elf
      else
        Enum.find_value(directions, elf, fn dir ->
          if !has_elves_at(elves, elf, offsets_at([dir])) do
            # IO.puts(~s(Elf #{elf.x},#{elf.y} moves #{dir}))
            move_elf(elf, dir)
          else
            # IO.puts(~s(Elf #{elf.x},#{elf.y} can't move #{dir}))
            # IO.inspect(offsets_at([dir]))
            nil
          end
        end)
      end
    end)
  end

  def is_proposal_unique(proposals, p) do
    Enum.count(proposals, fn x -> Point.compare(x, p) end) == 1
  end

  def apply_proposals(elves, proposals) do
    Enum.with_index(elves)
    |> Enum.map(fn {elf, i} ->
      p = Enum.at(proposals, i)

      if is_proposal_unique(proposals, p) do
        p
      else
        elf
      end
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

  def display(elves) do
    lower =
      Enum.reduce(elves, hd(elves), fn elf, mn ->
        %Point{x: min(elf.x, mn.x), y: min(elf.y, mn.y)}
      end)

    upper =
      Enum.reduce(elves, hd(elves), fn elf, mx ->
        %Point{x: max(elf.x, mx.x), y: max(elf.y, mx.y)}
      end)

    Enum.each(lower.y..upper.y, fn y ->
      Enum.each(lower.x..upper.x, fn x ->
        IO.write(
          if elves_contain(elves, %Point{x: x, y: y}) do
            "#"
          else
            "."
          end
        )
      end)

      IO.write("\n")
    end)
  end

  def count_empty(elves) do
    lower =
      Enum.reduce(elves, hd(elves), fn elf, mn ->
        %Point{x: min(elf.x, mn.x), y: min(elf.y, mn.y)}
      end)

    upper =
      Enum.reduce(elves, hd(elves), fn elf, mx ->
        %Point{x: max(elf.x, mx.x), y: max(elf.y, mx.y)}
      end)

    Enum.reduce(lower.y..upper.y, 0, fn y, acc ->
      Enum.reduce(lower.x..upper.x, acc, fn x, acc2 ->
        if elves_contain(elves, %Point{x: x, y: y}) do
          acc2
        else
          acc2 + 1
        end
      end)
    end)
  end
end

elves = Aoc.parse_input("input")
IO.inspect(elves)
# IO.puts("Elves:")
# Aoc.display(elves)
# IO.puts("\nProposal:")
# props = Aoc.gen_proposals(elves, [:n, :s, :w, :e])
# Aoc.display(props)
# IO.puts("\nResult:")
# Aoc.display(Aoc.apply_proposals(elves, props))
elves = Aoc.run(elves, 10)
Aoc.display(elves)
IO.inspect(Aoc.count_empty(elves))
# IO.inspect(elves)
