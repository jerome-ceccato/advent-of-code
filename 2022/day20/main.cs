namespace AoC
{
    struct EncryptedNumber
    {
        public EncryptedNumber(int n, int index)
        {
            N = n;
            Idx = index;
        }

        public int N { get; }
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

        static void PrintTape(EncryptedNumber[] tape)
        {
            Console.WriteLine("{0}", String.Join(", ", tape.Select(n => n.N.ToString())));
        }

        static void MixOnce(EncryptedNumber[] tape)
        {
            int size = tape.Count();
            PrintTape(tape);
            for (int idx = 0; idx < size; idx++)
            {
                int i = Array.FindIndex(tape, n => n.Idx == idx);
                //Console.Write("i: {0} -- ", i);
                //PrintTape(tape);
                // for (int i = 0; discovered < size;)

                int n = Normalize(tape[i].N, i, size);
                int direction = Math.Sign(n);
                for (int j = 0; j != n; j += direction)
                {
                    int from = i + j;
                    int to = i + j + direction;
                    EncryptedNumber tmp = tape[to];
                    tape[to] = tape[from];
                    tape[from] = tmp;
                }
               // PrintTape(tape);


            }
            PrintTape(tape);
        }


        // normalize the number of steps to take to avoid having to loop
        static int Normalize(int n, int pos, int size)
        {
            n %= (size - 1);
            if (n < 0)
            {
                if (pos + n < 0)
                {
                    n += size - 1;
                }
            }
            else if (n > 0)
            {
                if ((pos + n) >= size)
                {
                    n -= size - 1;
                }
            }
            return n;
        }

        static int GetGroveCoords(EncryptedNumber[] tape)
        {
            int zeroPos = Array.FindIndex(tape, n => n.N == 0);
            int size = tape.Count();

            int total = 0;
            foreach (int target in new List<int> { 1000, 2000, 3000 })
            {
                total += tape[(zeroPos + target) % size].N;
            }
            return total;
        }

        static void Main(string[] args)
        {
            EncryptedNumber[] tape = GetInput("input");
            MixOnce(tape);
            Console.WriteLine("Part 1: {0}", GetGroveCoords(tape));
        }
    }
}
