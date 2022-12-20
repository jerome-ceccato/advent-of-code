namespace AoC
{
    struct EncryptedNumber
    {
        public EncryptedNumber(Int128 n, int index)
        {
            N = n;
            Idx = index;
        }

        public Int128 N { get; }
        public int Idx { get; }
    }
    class Day20
    {
        static EncryptedNumber[] GetInput(string filename)
        {
            int size = System.IO.File.ReadLines(filename).Count();
            EncryptedNumber[] array = new EncryptedNumber[size];

            int i = 0;
            foreach (string line in System.IO.File.ReadLines(filename))
            {
                array[i] = new EncryptedNumber(Int32.Parse(line), i);
                i++;
            }
            return array;
        }

        static void MixOnce(EncryptedNumber[] tape)
        {
            int size = tape.Count();
            for (int idx = 0; idx < size; idx++)
            {
                int i = Array.FindIndex(tape, n => n.Idx == idx);
                Int128 n = Normalize(tape[i].N, i, size);
                int direction = Int128.Sign(n);
                for (int j = 0; j != n; j += direction)
                {
                    int from = i + j;
                    int to = i + j + direction;
                    (tape[to], tape[from]) = (tape[from], tape[to]);
                }
            }
        }

        // Normalize the number of steps to avoid having to loop
        static Int128 Normalize(Int128 n, int pos, int size)
        {
            n %= (size - 1);
            if (n < 0 && (pos + n < 0))
                n += size - 1;
            else if (n > 0 && ((pos + n) >= size))
                n -= size - 1;
            return n;
        }

        static Int128 GetGroveCoords(EncryptedNumber[] tape)
        {
            int zeroPos = Array.FindIndex(tape, n => n.N == 0);
            int size = tape.Count();

            Int128 total = 0;
            foreach (int target in new List<int> { 1000, 2000, 3000 })
                total += tape[(zeroPos + target) % size].N;
            return total;
        }

        static void Main(string[] args)
        {
            EncryptedNumber[] tape = GetInput("input");
            MixOnce(tape);
            Console.WriteLine("Part 1: {0}", GetGroveCoords(tape));

            tape = GetInput("input").Select(n => new EncryptedNumber(n.N * 811589153, n.Idx)).ToArray();
            for (int i = 0; i < 10; i++)
                MixOnce(tape);

            Console.WriteLine("Part 2: {0}", GetGroveCoords(tape));
        }
    }
}
