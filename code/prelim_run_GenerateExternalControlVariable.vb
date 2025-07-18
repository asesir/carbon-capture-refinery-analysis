Sub SwingCutOptimization()

'
' SwingCutOptimization Macro
'
 'variables in main programe
 Dim m As Integer
 Dim n As Integer
 Dim i As Integer
 Dim k As Integer
 Dim results
 
 
 'Solver programe variables
 Dim x As Integer
 
 Dim rng1 As Range
 Dim rng2 As Range
 Dim rng3 As Range
   
 Dim wb1 As Workbook
 Dim wb2 As Workbook
 
 Dim Data_Matrix As String
 Data_Matrix = "Data Matrix"
 
 'Turn off screenupdating
 Application.ScreenUpdating = False
  
 'open new workbook
 
 Workbooks.Open ("C:\Users\file_location\external_parameters.xlsx") ' TO BE EDITED
 
 
 Set wb1 = Workbooks("external_parameters.xlsx")
 
 Set wb2 = Workbooks("SI_PRELIM_CC_Economic.xlsm")
 
 
 wb1.Sheets(Data_Matrix).Range("A1").Value = "Refinery Configuration"
 wb1.Sheets(Data_Matrix).Range("B1").Value = "Crudes"
 'Swing Cut
 wb2.Sheets("CokingRefineryControls").Range("B4:B34").Copy
 wb1.Sheets(Data_Matrix).Range("C1").PasteSpecial Paste:=xlPasteValues, Transpose:=True
 'MFOpt
 wb2.Sheets("MFOpt").Range("A2:A59").Copy
 wb1.Sheets(Data_Matrix).Range("AH1").PasteSpecial Paste:=xlPasteValues, Transpose:=True
  

 
 'Select PADDs PRODUCT SLATE
 
  For m = 1 To 40 Step 1 'Total of 40 slates
    'Copy product slate from target slate to exper input
    wb2.Sheets("Target Slate").Range(wb2.Sheets("Target Slate").Cells(m + 1, 5), wb2.Sheets("Target Slate").Cells(m + 1, 21)).Copy
    wb2.Sheets("Expert Input").Range("F80").PasteSpecial Paste:=xlPasteValues, Transpose:=True

    'Select Product slate
    '2 crude assays in each scenarios eachpad heavy/sour& light/sweet
    For n = 1 To 2 Step 1
    
    wb2.Sheets("Assay Inventory").Range("T11").Value = (m - 1) * 2 + n
     
        'Selecting Refinery Configurations
         For i = 2 To 8 Step 1
        
        'turn on/off RFCC for different Refinery Configurations
         If i = 2 Or 4 Or 7 Then
            wb2.Sheets("CokingRefineryPFD").Range("N69").Value = 2
         Else
            wb2.Sheets("CokingRefineryPFD").Range("N69").Value = 1
            
         End If
         
         'refresh control data for material flow
         wb2.Sheets("MFOpt").Range("P2:P59").Copy
         wb2.Sheets("MFOpt").Range("B2:B59").PasteSpecial xlPasteValues

        'save datacollectionfile
         wb1.Save
                 
        'Change Refinery Configurations
         wb2.Sheets("CokingRefineryPFD").Range("E19").Value = i
        
        'Run Solver_1
         wb2.Sheets("CokingRefineryControls").Activate
         SolverReset

         SolverOptions convergence:=0.0001, populationsize:=True, maxtimenoimp:=30, AssumeNonNeg:=True, MultiStart:=False, REQUIREBOUNDS:=True
         SolverOk SetCell:="$E$79", MaxMinVal:=3, ValueOf:=0, ByChange:="$C$4:$C$34", _
             Engine:=1, EngineDesc:="GRG Nonlinear"
         SolverAdd CellRef:="$C$4:$C$34", Relation:=1, FormulaText:="$G$4:$G$34"
         SolverAdd CellRef:="$C$4:$C$34", Relation:=3, FormulaText:="$F$4:$F$34"
         SolverAdd CellRef:="$N$4:$N$32", Relation:=1, FormulaText:="$AC$4:$AC$32"
         SolverAdd CellRef:="$N$4:$N$32", Relation:=3, FormulaText:="$AB$4:$AB$32"
         SolverAdd CellRef:="$D$84:$D$100", Relation:=3, FormulaText:="0.001"
         results = SolverSolve(True, "SolverIteration")
         Select Case results
         Case 0, 1, 2, 3, 4, 5, 6, 10, 14, 15, 16, 17
         solverfinish keepfinal:=1
         Case Else
         solverfinish keepfinal:=0
         End Select
             
                     
         'Run Solver_2
         wb2.Sheets("MFOpt").Activate
         SolverReset
         SolverOptions convergence:=0.0001, populationsize:=True, maxtimenoimp:=30, AssumeNonNeg:=True, MultiStart:=False
         SolverOk SetCell:="$C$2", MaxMinVal:=3, ValueOf:=0, ByChange:="$B$2:$B$59", _
         Engine:=1, EngineDesc:="GRG Nonlinear"

         SolverAdd CellRef:="$D$2", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$3", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$4", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$5", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$6", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$7", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$8", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$9", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$10", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$11", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$12", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$13", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$14", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$15", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$16", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$17", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$18", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$19", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$20", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$21", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$22", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$23", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$24", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$25", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$26", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$27", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$28", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$29", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$30", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$31", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$32", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$33", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$34", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$35", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$36", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$37", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$38", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$39", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$40", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$41", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$42", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$43", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$44", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$45", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$46", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$47", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$48", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$49", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$50", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$51", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$D$52", Relation:=1, FormulaText:="$E$2"
         SolverAdd CellRef:="$F$2", Relation:=3, FormulaText:="$G$2"

         results = SolverSolve(True, "SolverIteration")
         Select Case results
         Case 0, 1, 2, 3, 4, 5, 6, 10, 14, 15, 16, 17
         solverfinish keepfinal:=1
         Case Else
         solverfinish keepfinal:=0
         End Select
            
           'selecting capture scenarios
         
           'Refinery Configurations
           wb2.Sheets("CokingRefineryPFD").Range("F19").Copy
           wb1.Sheets(Data_Matrix).Cells((m - 1) * 2 * 7 + (n - 1) * 7 + i, 1).PasteSpecial Paste:=xlPasteValues
           
           Application.CutCopyMode = False
           'Crudes Name
           wb2.Sheets("CokingRefineryPFD").Range("D109").Copy
           wb1.Sheets(Data_Matrix).Cells((m - 1) * 2 * 7 + (n - 1) * 7 + i, 2).PasteSpecial Paste:=xlPasteValues
           
           Application.CutCopyMode = False
           'Swing Cut
           wb2.Sheets("CokingRefineryControls").Range("C4:C34").Copy
           wb1.Sheets(Data_Matrix).Cells((m - 1) * 2 * 7 + (n - 1) * 7 + i, 3).PasteSpecial Paste:=xlPasteValues, Transpose:=True
                    
           Application.CutCopyMode = False
           'MFOpt
           wb2.Sheets("MFOpt").Range("B2:B59").Copy
           wb1.Sheets(Data_Matrix).Cells((m - 1) * 2 * 7 + (n - 1) * 7 + i, 34).PasteSpecial Paste:=xlPasteValues, Transpose:=True
                    
           Application.CutCopyMode = False
           'refresh control value
           wb2.Sheets("CokingRefineryControls").Range("K4:K34").Copy
           wb2.Sheets("CokingRefineryControls").Range("C4").PasteSpecial xlPasteValues
          
         Next
        Next
    Next
 Application.ScreenUpdating = True
    
 wb1.Close saveChanges:=True
 wb2.Close saveChanges:=True
 

 Application.Quit
'Application.DisplayStatusBar = True
  
End Sub

Function SolverIteration(Reason As Integer)

Const solvercontinue As Boolean = False
Const solverstop As Boolean = True

Select Case Reason

Case 1
SolverIteration = solvercontinue

Case Else
SolverIteration = solverstop

End Select

End Function



