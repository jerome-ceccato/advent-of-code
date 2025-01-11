import SwiftGodot

let allNodes: [Wrapped.Type] = [
    AocCamera.self,
    Day13Cart.self,
    
    Day10.self,
    Day11.self,
    Day12.self,
    Day13.self,
]


#initSwiftExtension(cdecl: "swift_entry_point", types: allNodes)
