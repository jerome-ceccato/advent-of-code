#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>
#include <set>

struct Point {
    public:
    int x;
    int y;

    Point() : x(0), y(0) {}
    Point(int x, int y) : x(x), y(y) {}

    int distance(Point other) {
        return abs(x - other.x) + abs(y - other.y);
    }

    bool operator==(const Point& rhs) {
        return x == rhs.x && y == rhs.y;
    }
};

bool operator<(const Point& lhs, const Point& rhs) {
    return lhs.x == rhs.x ? lhs.y < rhs.y : lhs.x < rhs.x;
}

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

        for (auto& scanner : _scanners) {
            top_left.x = std::min(top_left.x, scanner.position.x - scanner.range);
            top_left.y = std::min(top_left.y, scanner.position.y - scanner.range);
            bottom_right.x = std::max(bottom_right.x, scanner.position.x + scanner.range);
            bottom_right.y = std::max(bottom_right.y, scanner.position.y + scanner.range);
        }

        return Rect(
            Point(top_left.x, top_left.y),
            Point(bottom_right.x - top_left.x, bottom_right.y - top_left.y)
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

    long long int find_beacon() {
        Point max(4000000, 4000000);

        // Find all points that are exactly outside the range of each beacon
        // This is extremely slow and inefficient, but it does not involve smart math
        std::set<Point> candidates;
        for (auto& scanner : _scanners) {
            int crange = scanner.range + 1;
            for (int x = -crange; x <= crange; x++) {
                int y = crange - abs(x);
                Point a(scanner.position.x + x, scanner.position.y + y);
                Point b(scanner.position.x + x, scanner.position.y - y);

                if (a.x >= 0 && a.x <= max.x && a.y >= 0 && a.y <= max.y)
                    candidates.insert(a);
                if (b.x >= 0 && b.x <= max.x && b.y >= 0 && b.y <= max.y)
                    candidates.insert(b);
            }
        }
        
        for (auto& p : candidates) {
            if (scan_one(p)) {
                return 4000000ll * (long long int)p.x + p.y;
            }
        }
        return 0;
    }
};


int main(void) {
    TunnelMap map("input");

    std::cout << map.scan_row(2000000) << std::endl;
    std::cout << map.find_beacon() << std::endl;;
    return 0;
}
