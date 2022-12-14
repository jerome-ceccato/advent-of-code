const fs = require('fs');

function splitList(str) {
    if (str == "[]") {
        return [];
    }

    if (str.slice(0, 1) == '[' && str.slice(-1) == ']') {
        str = str.slice(1, -1);
        let ret = [];
        let i = 0;
        let offset = 0;
        let nested = 0;
        while (i < str.length) {
            if (str[i] == '[') nested++;
            if (str[i] == ']') nested--;
            if (str[i] == ',' && nested == 0) {
                ret.push(str.slice(offset, i));
                offset = i + 1;
            }
            i++;
        }
        ret.push(str.slice(offset, i));
        return ret;
    }
    return [str];
}

function compareLists(l, r) {
    let left = splitList(l)
    let right = splitList(r)

    for (let i = 0; i < left.length && i < right.length; i++) {
        let res = 0;
        if (left[i][0] == '[' || right[i][0] == '[') {
            res = compareLists(left[i], right[i]);
        } else {
            res = parseInt(left[i]) - parseInt(right[i]);
        }

        if (res != 0) {
            return res;
        }
    }

    return left.length - right.length;
}

fs.readFile('input', 'utf8', (_, data) => {
    // part 1
    let result = data.split("\n\n")
        .map(line => compareLists(line.split('\n')[0], line.split('\n')[1]))
        .map((comp, i) => comp <= 0 ? i + 1 : 0)
        .reduce((a, b) => a + b, 0);
    console.log(result);

    // part 2
    result = (data + "\n[[2]]\n[[6]]").split("\n")
        .filter(s => s.length > 0)
        .sort(compareLists)
        .map((s, i) => (s == '[[2]]' || s == '[[6]]') ? i + 1 : 1)
        .reduce((a, b) => a * b, 1);
        console.log(result);
});
