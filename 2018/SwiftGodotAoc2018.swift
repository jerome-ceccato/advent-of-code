import SwiftGodot

let allNodes: [Wrapped.Type] = [
    AocCamera.self,
    
    Day10.self,
    Day11.self,
]


#initSwiftExtension(cdecl: "swift_entry_point", types: allNodes)
