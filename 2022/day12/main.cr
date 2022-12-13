alias Point = {x: Int32, y: Int32}

def height_value(c : Char)
  case c
  when 'S'
    return 0
  when 'E'
    return 25
  else
    return c.ord - 'a'.ord
  end
end

class Board
  @start : Point
  @target : Point
  @grid : Array(Array(Int32))

  def initialize(filename : String)
    input = File.read_lines(filename)
    @start = {x: 0, y: 0}
    @target = {x: 0, y: 0}

    (0...input.size).each do |y|
      line = input[y]
      (0...line.size).each do |x|
        if line[x] == 'S'
          @start = {x: x, y: y}
        elsif line[x] == 'E'
          @target = {x: x, y: y}
        end
      end
    end

    @grid = input.map { |line| line.chars.map { |c| height_value(c) } }
  end

  def is_valid?(newpos : Point, this_height : Int32) : Bool
    if line = @grid[newpos[:y]]?
      if height = line[newpos[:x]]?
        return height - this_height <= 1
      end
    end
    return false
  end

  def valid_positions_around(pos : Point) : Array(Point)
    this_height = @grid[pos[:y]][pos[:x]]
    positions = [
      {x: pos[:x] - 1, y: pos[:y]},
      {x: pos[:x] + 1, y: pos[:y]},
      {x: pos[:x], y: pos[:y] - 1},
      {x: pos[:x], y: pos[:y] + 1},
    ]

    return positions.select { |newpos| is_valid?(newpos, this_height) }
  end

  def find_shortest_path
    visited = Set(Point).new
    stack = Deque(Point).new(1, @start)
    steps = 0

    while !stack.includes?(@target)
      if stack.size == 0
        return -1
      end

      stack = stack.flat_map do |item|
        next_positions = valid_positions_around(item).reject { |p| visited.includes?(p) }
        next_positions.each { |p| visited.add(p) }
        next_positions
      end
      steps += 1
    end

    return steps
  end

  def find_best_route
    starting_points = @grid.map_with_index { |line, y| line.map_with_index { |height, x| height == 0 ? {x: x, y: y} : nil }.compact }.flatten
    shortest_paths = starting_points.map do |point|
      @start = point
      find_shortest_path
    end

    return shortest_paths.reject { |s| s < 0 }.min
  end
end

board = Board.new "input"
puts board.find_shortest_path # part 1
puts board.find_best_route    # part 2
