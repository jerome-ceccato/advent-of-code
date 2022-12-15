#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>

struct Point {
    public:
    int x;
    int y;

    Point() : x(0), y(0) {}
    Point(int x, int y) : x(x), y(y) {}

    int distance(Point other) {
        return abs(x - other.x) + abs(y - other.y);
    }

    inline bool operator==(const Point& rhs) {
        return x == rhs.x && y == rhs.y;
    }
};

struct Rect {
    public:
    Point position;
    Point size;

    Rect(Point pos, Point sz) : position(pos), size(sz) {}
};

class Scanner {
    public:
    Point position;
    Point beacon;
    int range;

    Scanner(Point scanner, Point beacon) :
        position(scanner), beacon(beacon) {
        range = scanner.distance(beacon);
    }
};

class TunnelMap {
    private:
    std::vector<Scanner> _scanners;

    public:
    TunnelMap(std::string filename) : scanners(_scanners) {
        std::string buffer;
        std::ifstream file(filename);
        
        while (std::getline(file, buffer)) {
            Point scanner, beacon;
            sscanf(buffer.c_str(), "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d",
                   &scanner.x, &scanner.y, &beacon.x, &beacon.y);
            _scanners.push_back(Scanner(scanner, beacon));
        }
    }

    std::vector<Scanner> const& scanners;

    Rect get_bounds() {
        Point top_left;
        Point bottom_right;
        int biggest_range = 0;

        for (auto& scanner : _scanners) {
            top_left.x = std::min(top_left.x, scanner.position.x);
            top_left.y = std::min(top_left.y, scanner.position.y);
            bottom_right.x = std::max(bottom_right.x, scanner.position.x);
            bottom_right.y = std::max(bottom_right.y, scanner.position.y);
            biggest_range = std::max(biggest_range, scanner.range);
        }

        return Rect(
            Point(top_left.x - biggest_range, top_left.y - biggest_range),
            Point(bottom_right.x - top_left.x + 2 * biggest_range, bottom_right.y - top_left.y + 2 * biggest_range)
        );
    }

    int scan_row(int row) {
        Rect bounds(get_bounds());
        int unreachables = 0;

        for (int i = 0; i < bounds.size.x; i++) {
            if (!scan_one(Point(bounds.position.x + i, row))) {
                unreachables++;
            }
        }

        return unreachables;
    }

    bool scan_one(Point pos) {
        for (auto& scanner : _scanners) {
            if (pos == scanner.beacon || pos == scanner.position) {
                return true;
            } else if (pos.distance(scanner.position) <= scanner.range) {
                return false;
            }
        }
        return true;
    }
};


int main(void) {
    TunnelMap map("input");

    std::cout << map.scan_row(2000000);
    return 0;
}
