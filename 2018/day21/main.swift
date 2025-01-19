import Foundation
import Device

func runDecodedProgram(returnFirstZ: Bool) -> UInt64 {
    var zHistory: [UInt64] = []
    
    var b: UInt64 = 0
    var c: UInt64 = 0
    var y: UInt64 = 0
    var z: UInt64 = 0

    y = z | 65536
    z = 8858047
    
    while true {
        c = y & 255
        z += c
        z &= 16777215
        z *= 65899
        z &= 16777215
        if 256 > y {
            if returnFirstZ {
                return z
            }
            
            if zHistory.contains(z) {
                return zHistory.last!
            }
            zHistory.append(z)

            y = z | 65536
            z = 8858047
            continue
        }
        
        c = 0
        
        repeat {
            b = c + 1
            b *= 256
            c += 1
        } while b <= y
        c -= 1
        y = c
    }
}

print(runDecodedProgram(returnFirstZ: true))
print(runDecodedProgram(returnFirstZ: false))

/*
 
 z = 0
 
 start:
 y = z | 65536
 z = 8858047
 
 loop {
     c = y & 255
     z += c
     z &= 16777215
     z *= 65899
     z &= 16777215
     if 256 > y {
         if z == a {
            win()
         } else {
            jmp start
         }
     }
     
     c = 0
     
     do {
         b = c + 1
         b *= 256
         c += 1
     } while b <= y
     c -= 1
     y = c
 }
 
 */

/*

 // reg[0] = a, reg[1] = b, reg[2] = c
 // reg[3] = ip, reg[4] = y, reg[5] = z
 
 00: z = 123            seti 123 0 5
 01: z &= 456           bani 5 456 5
 02: z = z == 72        eqri 5 72 5
 03: ip += z            addr 5 3 3
 04: ip = 0             seti 0 0 3
 
 05: z = 0              seti 0 3 5
 06: y = z | 65536      bori 5 65536 4
 07: z = 8858047        seti 8858047 4 5
 08: c = y & 255        bani 4 255 2
 09: z += c             addr 5 2 5
 10: z &= 16777215      bani 5 16777215 5
 11: z *= 65899         muli 5 65899 5
 12: z &= 16777215      bani 5 16777215 5
 13: c = 256 > y        gtir 256 4 2
 14: ip += c            addr 2 3 3
 15: ip += 1            addi 3 1 3
 16: ip = 27            seti 27 5 3
 17: c = 0              seti 0 6 2
 18: b = c + 1          addi 2 1 1
 19: b *= 256           muli 1 256 1
 20: b = b > y          gtrr 1 4 1
 21: ip += b            addr 1 3 3
 22: ip += 1            addi 3 1 3
 23: ip = 25            seti 25 1 3
 24: c += 1             addi 2 1 2
 25: ip = 17            seti 17 4 3
 26: y = c              setr 2 1 4
 27: ip = 7             seti 7 3 3
 28: c = z == a         eqrr 5 0 2
 29: ip += c            addr 2 3 3
 30: ip = 5             seti 5 2 3
 
 */
