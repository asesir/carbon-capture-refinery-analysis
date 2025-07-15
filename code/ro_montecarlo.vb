Sub MC_YearSpan()
'

'

Dim m As Long
Dim n As Long
Dim p As Long
Dim q As Long


Dim NumberOfSimulation As Long

Dim wb1 As Workbook
Dim wb2 As Workbook

Dim filepath As String
Dim BinomialSheet As String
Dim NPVSheet As String
Dim SimSheet As String

Dim RO_Result As String
Dim RO_Result_OptionValue As String
Dim RO_Result_Option As String

filepath = "C:\Users\fang.li\OneDrive\PhD\PhD Programe UofC\LCA\Paper 01\Data\Raw Data"


BinomialSheet = "BinomialTree"
NPVSheet = "NPV BreakDown"
SimSheet = "RefineryInfo"
NameSheet = "FileName"
ResultSheet = "ResultCopy"

RO_Result = "Sheet1"


Set wb1 = Workbooks("RO_Calculation.xlsm") ' Change for different scenarios
' Initializing scenarios
wb1.Sheets(NPVSheet).Range("A14").Value = 1 'General Test
wb1.Sheets(NPVSheet).Range("A4").Value = 1
wb1.Sheets(NPVSheet).Range("A6").Value = 4 'Carbon Tax Scenario
wb1.Sheets(NPVSheet).Range("A8").Value = 2
wb1.Sheets(NPVSheet).Range("A10").Value = 1
'wb1.Sheets(NPVSheet).Range("A12").Value = 0 '2019 BASELINE SCENARIO

NumberOfSimulation = wb1.Sheets(NPVSheet).Range("F4").Value

Set wb2 = Workbooks.Add


' Save the workbook with the desired name
wb2.SaveAs Filename:=filepath & "RO_Result_" & wb1.Sheets(NameSheet).Range("A11").Value & "_v4_extendedCT_changeCapacity.xlsx"


wb2.Sheets(RO_Result).Cells.ClearContents


wb1.Sheets(ResultSheet).Range("A2:TV2").Copy
wb2.Sheets(RO_Result).Cells(1, 1).PasteSpecial Paste:=xlPasteValues, Transpose:=False


   
'Turn off screenupdating
Application.ScreenUpdating = False
'Application.Calculation = xlCalculationManual

For p = 1 To 3 Step 1
    
    wb1.Sheets(NPVSheet).Range("A12").Value = p
    For q = 1 To 3 Step 1
    
        wb1.Sheets(NPVSheet).Range("A10").Value = q
        
        n = 1
        
        Do While n <= NumberOfSimulation
        
        wb1.Sheets(NPVSheet).Range("B8").Value = n
        
        Application.CalculateFull
        
        ' Check any calculated volatility is error
        If Not IsError(wb1.Sheets(NPVSheet).Range("G2").Value) Then
            
            ' If it is not an error, execute the following code
            '
            wb1.Sheets(ResultSheet).Range("A3:TV3").Copy
            wb2.Sheets(RO_Result).Cells((p - 1) * 3 * NumberOfSimulation + (q - 1) * NumberOfSimulation + n + 1, 1).PasteSpecial Paste:=xlPasteValues, Transpose:=False ' Selected Options Paste
            
            ' Increment the counter
            n = n + 1
        Else
            
            ' If it is an error, just move to the next iteration without incrementing n
    
        End If
        
        Application.CutCopyMode = False
        
        Loop
    
    Next
Next

'Application.Calculation = xlCalculationAutomatic
Application.ScreenUpdating = True
wb1.Close savechanges:=True
wb2.Close savechanges:=True
Application.Quit

End Sub


