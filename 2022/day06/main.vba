Sub Aoc()
' Aoc Macro
' Assumes it's run in a spreadsheet with the input string in cell A1

    Dim inputString As String

    Range("A1").Select
    inputString = ActiveCell.FormulaR1C1

    Range("A2").Select
    ActiveCell.FormulaR1C1 = "Part 1: " & SolvePuzzle(inputString, 4)

    Range("A3").Select
    ActiveCell.FormulaR1C1 = "Part 2: " & SolvePuzzle(inputString, 14)
End Sub

Function HasDuplicates(str)
    Dim i As Integer
    Dim j As Integer
    
    For i = 1 To Len(str) - 1
      For j = i + 1 To Len(str)
        If Mid(str, i, 1) = Mid(str, j, 1) Then
        HasDuplicates = True
        Exit Function
        End If
      Next j
    Next i
    HasDuplicates = False
End Function

Function RangeIsMarker(buffer, index, size)
    RangeIsMarker = Not HasDuplicates(Mid(buffer, index, size))
End Function

Function SolvePuzzle(buffer, sliceSize)
    
    For i = 1 To Len(buffer) - sliceSize
        If RangeIsMarker(buffer, i, sliceSize) Then

        SolvePuzzle = (i + sliceSize - 1)
        Exit Function
        End If
    Next i
End Function
