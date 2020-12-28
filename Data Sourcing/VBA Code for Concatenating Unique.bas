Function ConcatUniq(xRg As Range, xChar As String) As String
'updateby Extendoffice
    Dim xCell As Range
    Dim xDic As Object
    Set xDic = CreateObject("Scripting.Dictionary")
    For Each xCell In xRg
        xDic(xCell.Value) = Empty
    Next
    ConcatUniq = Join$(xDic.Keys, xChar)
    Set xDic = Nothing
End Function
