Sub Sensitivity()
'
'
'
Dim i As Long
Dim j As Long
Dim m As Long
Dim n As Long
Dim p As Long
Dim q As Long
Dim k As Long

Dim NumberOfSimulation As Long

Dim wb1 As Workbook
Dim wb2 As Workbook

Dim filepath As String
Dim BinomialSheet As String
Dim NPVSheet As String
Dim SimSheet As String
Dim ResultSheet As String

Dim RO_Sensitivity_Option As String


filepath = "C:\Users\Asesi\OneDrive\PhD\PhD Programe UofC\LCA\Paper 01\Data\Raw Data\"
'filepath = "D:\PhD\PhD Programe UofC\LCA\PhD Thesis\Results\Results Analysis_3rd Run\Real Option\RO_Calc\"

'filepath = "C:\Users\fang.li\OneDrive\PhD\PhD Programe UofC\LCA\PhD Thesis\Results\Results Analysis_3rd Run\Real Option\RO_Calc\"
BinomialSheet = "BinomialTree"
NPVSheet = "NPV BreakDown"
SimSheet = "RefineryInfo"
NameSheet = "FileName"
ResultSheet = "ResultCopy"

RO_Sensitivity_Option = "Sheet1"


Set wb1 = Workbooks("RO_Calculation_2019_CarbonTax_modified_v4_DiscountRate_changeCapcity.xlsm") ' Change for different scenarios

' Initializing scenarios

wb1.Sheets(NPVSheet).Range("A8").Value = 2 'Carbon Price, 2 for random

wb1.Sheets(NPVSheet).Range("A12").Value = 0 'Electrification Scenario, 0 for 2019 Baseline, for 2035 remove this line
wb1.Sheets(NPVSheet).Range("A14").Value = 2 'Sensitivity analysis
wb1.Sheets(NPVSheet).Range("A6").Value = 4 'Emissions threshold selection, 4 for Carbon Tax

NumberOfSimulation = wb1.Sheets(NPVSheet).Range("F4").Value


Set wb2 = Workbooks.Add


' Save the workbook with the desired name
wb2.SaveAs Filename:=filepath & "RO_Sensitivity_" & wb1.Sheets(NameSheet).Range("A9").Value & "_CT.xlsx"



wb2.Sheets(RO_Sensitivity_Option).Cells.ClearContents

wb1.Sheets(ResultSheet).Range("A17:ET17").Copy
wb2.Sheets(RO_Sensitivity_Option).Cells(1, 1).PasteSpecial Paste:=xlPasteValues, Transpose:=False

    
Application.CutCopyMode = False
    
   
'Turn off screenupdating
Application.ScreenUpdating = False


'Restore Sensitivity Parameters
wb1.Sheets(NPVSheet).Range("A4").Value = 1 'Price selection
wb1.Sheets(NPVSheet).Range("C6").Value = 2 'Riskfree interest rate
wb1.Sheets(NPVSheet).Range("D6").Value = 2 'Refinery operating cost
wb1.Sheets(NPVSheet).Range("E6").Value = 2 'Carbon capture operating cost
wb1.Sheets(NPVSheet).Range("F6").Value = 2 'Carbon price cost
wb1.Sheets(NPVSheet).Range("D10").Value = 2 'CC CapEx

For m = 1 To 3 Step 1
    
    wb1.Sheets(NPVSheet).Range("C6").Value = m 'Riskfree interest rate
    
    n = 1
    
    Do While n <= NumberOfSimulation
    
    wb1.Sheets(NPVSheet).Range("B8").Value = n
    
    Application.CalculateFull
    
    ' Check any calculated volatility is error
    If Not IsError(wb1.Sheets(NPVSheet).Range("G2").Value) Then
    
        ' If it is not an error, execute the following code
        '
        wb1.Sheets(ResultSheet).Range("A18:ET18").Copy
        wb2.Sheets(RO_Sensitivity_Option).Cells((m - 1) * NumberOfSimulation + n + 1, 1).PasteSpecial Paste:=xlPasteValues, Transpose:=False
    
        ' Increment the counter
        n = n + 1
        
    Else
    
        ' If it is an error, just move to the next iteration without incrementing n
    
    End If
    
    Application.CutCopyMode = False
    
    Loop

Next
    
    
'Restore Sensitivity Parameters
wb1.Sheets(NPVSheet).Range("A4").Value = 1 'Price selection
wb1.Sheets(NPVSheet).Range("C6").Value = 2 'Riskfree interest rate
wb1.Sheets(NPVSheet).Range("D6").Value = 2 'Refinery operating cost
wb1.Sheets(NPVSheet).Range("E6").Value = 2 'Carbon capture operating cost
wb1.Sheets(NPVSheet).Range("F6").Value = 2 'Carbon price cost
wb1.Sheets(NPVSheet).Range("D10").Value = 2 'CC CapEx

For i = 1 To 3 Step 1

    wb1.Sheets(NPVSheet).Range("A4").Value = i 'Price selection
    
    n = 1

    Do While n <= NumberOfSimulation
    
    wb1.Sheets(NPVSheet).Range("B8").Value = n
    
    Application.CalculateFull
    
    ' Check any calculated volatility is error
    If Not IsError(wb1.Sheets(NPVSheet).Range("G2").Value) Then
    
        ' If it is not an error, execute the following code
        '
        wb1.Sheets(ResultSheet).Range("A18:ET18").Copy
        wb2.Sheets(RO_Sensitivity_Option).Cells(3 * NumberOfSimulation + (i - 1) * NumberOfSimulation + n + 1, 1).PasteSpecial Paste:=xlPasteValues, Transpose:=False
    
        ' Increment the counter
        n = n + 1
        
    Else
    
        ' If it is an error, just move to the next iteration without incrementing n
    
    End If
    
    Application.CutCopyMode = False
        
    Loop
    
Next

'Restore Sensitivity Parameters
wb1.Sheets(NPVSheet).Range("A4").Value = 1 'Price selection
wb1.Sheets(NPVSheet).Range("C6").Value = 2 'Riskfree interest rate
wb1.Sheets(NPVSheet).Range("D6").Value = 2 'Refinery operating cost
wb1.Sheets(NPVSheet).Range("E6").Value = 2 'Carbon capture operating cost
wb1.Sheets(NPVSheet).Range("F6").Value = 2 'Carbon price cost
wb1.Sheets(NPVSheet).Range("D10").Value = 2 'CC CapEx

For j = 1 To 3 Step 1

    wb1.Sheets(NPVSheet).Range("F6").Value = j
    
    n = 1

    Do While n <= NumberOfSimulation
    
    wb1.Sheets(NPVSheet).Range("B8").Value = n
    
    Application.CalculateFull
    
    ' Check any calculated volatility is error
    If Not IsError(wb1.Sheets(NPVSheet).Range("G2").Value) Then
    
        ' If it is not an error, execute the following code
        '
        wb1.Sheets(ResultSheet).Range("A18:ET18").Copy
        wb2.Sheets(RO_Sensitivity_Option).Cells(2 * 3 * NumberOfSimulation + (j - 1) * NumberOfSimulation + n + 1, 1).PasteSpecial Paste:=xlPasteValues, Transpose:=False
    
        ' Increment the counter
        n = n + 1
        
    Else
    
        ' If it is an error, just move to the next iteration without incrementing n
    
    End If
    
    Application.CutCopyMode = False
    
    Loop
    
    
Next

'Restore Sensitivity Parameters
wb1.Sheets(NPVSheet).Range("A4").Value = 1 'Price selection
wb1.Sheets(NPVSheet).Range("C6").Value = 2 'Riskfree interest rate
wb1.Sheets(NPVSheet).Range("D6").Value = 2 'Refinery operating cost
wb1.Sheets(NPVSheet).Range("E6").Value = 2 'Carbon capture operating cost
wb1.Sheets(NPVSheet).Range("F6").Value = 2 'Carbon price cost
wb1.Sheets(NPVSheet).Range("D10").Value = 2 'CC CapEx

For p = 1 To 3 Step 1

    wb1.Sheets(NPVSheet).Range("D6").Value = p
    
    n = 1

    Do While n <= NumberOfSimulation
    
    wb1.Sheets(NPVSheet).Range("B8").Value = n
    
    Application.CalculateFull
    
    ' Check any calculated volatility is error
    If Not IsError(wb1.Sheets(NPVSheet).Range("G2").Value) Then
    
        ' If it is not an error, execute the following code
        '
        wb1.Sheets(ResultSheet).Range("A18:ET18").Copy
        wb2.Sheets(RO_Sensitivity_Option).Cells(3 * 3 * NumberOfSimulation + (p - 1) * NumberOfSimulation + n + 1, 1).PasteSpecial Paste:=xlPasteValues, Transpose:=False
    
        ' Increment the counter
        n = n + 1
        
    Else
    
        ' If it is an error, just move to the next iteration without incrementing n
    
    End If
    
    Application.CutCopyMode = False
    
    Loop
    
    
Next


'Restore Sensitivity Parameters
wb1.Sheets(NPVSheet).Range("A4").Value = 1 'Price selection
wb1.Sheets(NPVSheet).Range("C6").Value = 2 'Riskfree interest rate
wb1.Sheets(NPVSheet).Range("D6").Value = 2 'Refinery operating cost
wb1.Sheets(NPVSheet).Range("E6").Value = 2 'Carbon capture operating cost
wb1.Sheets(NPVSheet).Range("F6").Value = 2 'Carbon price cost
wb1.Sheets(NPVSheet).Range("D10").Value = 2 'CC CapEx

For q = 1 To 3 Step 1

    wb1.Sheets(NPVSheet).Range("E6").Value = q
    
    n = 1

    Do While n <= NumberOfSimulation
    
    wb1.Sheets(NPVSheet).Range("B8").Value = n
    
    Application.CalculateFull
    
    ' Check any calculated volatility is error
    If Not IsError(wb1.Sheets(NPVSheet).Range("G2").Value) Then
    
        ' If it is not an error, execute the following code
        '
        wb1.Sheets(ResultSheet).Range("A18:ET18").Copy
        wb2.Sheets(RO_Sensitivity_Option).Cells(4 * 3 * NumberOfSimulation + (q - 1) * NumberOfSimulation + n + 1, 1).PasteSpecial Paste:=xlPasteValues, Transpose:=False
    
        ' Increment the counter
        n = n + 1
        
    Else
    
        ' If it is an error, just move to the next iteration without incrementing n
    
    End If
    
    Application.CutCopyMode = False
    
    Loop

    
Next

'Restore Sensitivity Parameters
wb1.Sheets(NPVSheet).Range("A4").Value = 1 'Price selection
wb1.Sheets(NPVSheet).Range("C6").Value = 2 'Riskfree interest rate
wb1.Sheets(NPVSheet).Range("D6").Value = 2 'Refinery operating cost
wb1.Sheets(NPVSheet).Range("E6").Value = 2 'Carbon capture operating cost
wb1.Sheets(NPVSheet).Range("F6").Value = 2 'Carbon price cost
wb1.Sheets(NPVSheet).Range("D10").Value = 2 'CC CapEx

For k = 1 To 3 Step 1

    wb1.Sheets(NPVSheet).Range("D10").Value = k
    
    n = 1

    Do While n <= NumberOfSimulation
    
    wb1.Sheets(NPVSheet).Range("B8").Value = n
    
    Application.CalculateFull
    
    ' Check any calculated volatility is error
    If Not IsError(wb1.Sheets(NPVSheet).Range("G2").Value) Then
    
        ' If it is not an error, execute the following code
        '
        wb1.Sheets(ResultSheet).Range("A18:ET18").Copy
        wb2.Sheets(RO_Sensitivity_Option).Cells(5 * 3 * NumberOfSimulation + (k - 1) * NumberOfSimulation + n + 1, 1).PasteSpecial Paste:=xlPasteValues, Transpose:=False
    
        ' Increment the counter
        n = n + 1
        
    Else
    
        ' If it is an error, just move to the next iteration without incrementing n
    
    End If
    
    Application.CutCopyMode = False
    
    Loop

    
Next

Application.ScreenUpdating = True


End Sub



