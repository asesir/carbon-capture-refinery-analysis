Sub DataCollection_ExternalControlParameters()

    '
    ' Data_collection_ExternalControlParameters Macro
    ' Updated stored elements, include sensitivity analysis.
     
    ' loop control variables
     Dim m As Integer ' Variable for scenarios
     Dim n As Integer ' Variable for crudes
     Dim i As Integer ' Variable for configuration
     Dim k As Integer ' Variable for capture scenarios
     Dim j As Integer ' Variable for loop control

    ' variable for next empty column index
     Dim NextColumn As Integer
     Dim counter As Integer

    ' variable for copy sources
    ' column names
     Dim NameRanges As Variant
     Dim NameWorkbook As Workbook
     Dim NameSheet As String
     Dim NameRange As Range
     Dim NamPrefix As String
     Dim NameAfterfix As String
     
    
     ' Create an array to hold the modified values
     Dim numOfCells As Integer
     Dim tempArray() As Variant
     ' cell values
     Dim SourceRanges As Variant
     Dim SourceWorkbook As Workbook
     Dim SourceSheet As String
     Dim SourceRange As Range
     Dim SourcePrefix As String
     Dim SourceIndicator As String
     
     Dim rng1 As Range
     Dim rng2 As Range
     Dim rng3 As Range
       
     Dim wb1 As Workbook
     Dim wb2 As Workbook
     
     Dim Data_Matrix As String
     Data_Matrix = "Data Matrix"

     
    ' solver exit mechanism function variable
     Dim results
     
    ' solver programe variables
     Dim x As Integer
     
    ' turn off screenupdating
     Application.ScreenUpdating = False
      
    ' open new workbook
     
     Workbooks.Open ("C:\Users\file_location\external_parameters.xlsx")
     
     Workbooks.Open ("C:\Users\file_location\prelimResult.xlsx")
     
    ' set workbook names
     Set wb1 = Workbooks("SI_PRELIM_CC_Economic.xlsm")
     
     Set wb2 = Workbooks("prelimResult.xlsx")
     
     Set wb3 = Workbooks("external_parameters.xlsx")
     
     wb2.Sheets(Data_Matrix).Cells.ClearContents
     
     
    ' refinery configuration
    ' refinery configuration EMPTY,capture scenarios EMPTY,year EMPTY,PADD EMPTY,Scenario EMPTY,API EMPTY,Sul EMPTY
    ' crude throughput
    ' product slate
    ' EN_Crude, EN_Process, GHG_Crude, GHG_Process
    ' GHG_PU
    ' flue gas rate by unit, CO2rate by unit
    ' CO2 avoidance cost, Avoided CO2

    
    NameRanges = Array(Array(wb1.Name, "CokingRefineryPFD", "C17"), _
                   Array("text", "Config"), Array("text", "Capture Scenario"), Array("text", "Year"), Array("text", "PADD"), Array("text", "Scenario"), Array("text", "API"), Array("text", "Sul"), _
                   Array("text", "Crude Throughput, bpd"), _
                   Array(wb1.Name, "Results Single Assay", "B13:B29"), _
                   Array(wb1.Name, "Results Single Assay", "B77", "Energy_"), Array(wb1.Name, "Results Single Assay", "B79:B104", "Energy_"), Array(wb1.Name, "Results Single Assay", "B77", "GHG_"), Array(wb1.Name, "Results Single Assay", "B79:B104", "GHG_"), _
                   Array(wb1.Name, "CokingRefinery Detailed Results", "C54:C90"), _
                   Array(wb1.Name, "CarbonCaptureCalcs", "W55:W88", "FlueGasRate_"), Array(wb1.Name, "CarbonCaptureCalcs", "W55:W88", "CO2Rate"), _
                   Array("text", "Avoidance Cost"), Array("text", "Avoided CO2, kt/y"), Array("text", "Annualized Capex, k$/y"), Array("text", "OpEx, k$/y"))
    
    
    
    For j = 0 To UBound(NameRanges)
        ' Determine the next free column in destination row
        NextColumn = wb2.Sheets(Data_Matrix).Cells(1, wb2.Sheets(Data_Matrix).Columns.Count).End(xlToLeft).Column + 1
        
        ' check if the range is a string or a cell value
        If NameRanges(j)(0) = "text" Then
            ' input string directly
            wb2.Sheets(Data_Matrix).Cells(1, NextColumn).Value = NameRanges(j)(1)
        Else
            ' Copy the value from the specified cell
            Set NameWorkbook = Workbooks(NameRanges(j)(0))
            NameSheet = NameRanges(j)(1)
            Set NameRange = NameWorkbook.Sheets(NameSheet).Range(NameRanges(j)(2))
    
            If UBound(NameRanges(j)) >= 3 Then
                NamePrefix = NameRanges(j)(3)
            Else
                NamePrefix = ""
            End If
                 
            If UBound(NameRanges(j)) >= 4 Then
                NameAfterfix = NameRanges(j)(4)
            Else
                NameAfterfix = ""
            End If
    
    
            ' Get the number of cells in the range
            numOfCells = NameRange.Cells.Count
    
            ' Redim the array to match the number of cells
            ReDim tempArray(1 To numOfCells)
    
            Dim a As Integer
            a = 1
            For Each cell In NameRange
                If CStr(NamePrefix) <> "" Then
                    tempArray(a) = CStr(NamePrefix) & CStr(cell.Value)
                ElseIf CStr(NameAfterfix) <> "" Then
                    tempArray(a) = CStr(cell.Value) & CStr(NameAfterfix)
                Else
                    tempArray(a) = CStr(cell.Value)
                End If
                a = a + 1
            Next cell
    
            ' Paste the modified values to the destination one by one
            For i = 1 To UBound(tempArray)
                wb2.Sheets(Data_Matrix).Cells(1, NextColumn + i - 1).Value = tempArray(i)
            Next i
            Erase tempArray
        End If
    Next j

    ' refinery configuration
    ' refinery configuration EMPTY,capture scenarios EMPTY,year EMPTY,PADD EMPTY,Scenario EMPTY,API EMPTY,Sul EMPTY
    ' crude throughput
    ' product slate
    ' EN_Crude, EN_Process, GHG_Crude, GHG_Process
    ' GHG_PU
    ' flue gas rate by unit, CO2rate by unit
    ' CO2 avoidance cost, Avoided CO2
     
    SourceRanges = Array(Array(wb1.Name, "CokingRefineryPFD", "F19"), _
                         Array(wb1.Name, "CokingRefineryPFD", "E18"), Array(wb1.Name, "CarbonCaptureCalcs", "G27"), Array(wb1.Name, "Assay Inventory", "C9"), Array("text", "NA"), Array("text", "NA"), Array("text", "NA"), Array("text", "NA"), _
                         Array(wb1.Name, "Assay Inventory", "E12"), _
                         Array(wb1.Name, "Results Single Assay", "D13:D29"), _
                         Array(wb1.Name, "Results Single Assay", "C77"), Array(wb1.Name, "Results Single Assay", "C79:C104"), Array(wb1.Name, "Results Single Assay", "D77"), Array(wb1.Name, "Results Single Assay", "D79:D104"), _
                         Array(wb1.Name, "CokingRefinery Detailed Results", "S54:S90"), _
                         Array(wb1.Name, "CarbonCaptureCalcs", "X55:X88"), Array(wb1.Name, "CarbonCaptureCalcs", "Y55:Y88"), _
                         Array(wb1.Name, "CarbonCaptureCalcs", "AQ92"), Array(wb1.Name, "CarbonCaptureCalcs", "AR93"), Array(wb1.Name, "CarbonCaptureCalcs", "AO89"), Array(wb1.Name, "CarbonCaptureCalcs", "AP89"))
            
      For m = 1 To 40 Step 1 'Total of 40 cases
        
        'Copy product slate from target slate to exper input
        wb1.Sheets("Target Slate").Range(wb1.Sheets("Target Slate").Cells(m + 1, 5), wb1.Sheets("Target Slate").Cells(m + 1, 21)).Copy
        wb1.Sheets("Expert Input").Range("F80").PasteSpecial Paste:=xlPasteValues, Transpose:=True
    
        'Select Product slate
        '2 crude assays in each scenarios for padd 1 heavy/sour & light/sweet
        
        For n = 1 To 2 Step 1
        
        wb1.Sheets("Assay Inventory").Range("T11").Value = (m - 1) * 2 + n
         
            'Selecting Refinery Configurations
             For i = 2 To 8 Step 1
            
            'turn on/off RFCC for different Refinery Configurations
             
             If i = 2 Or i = 4 Or i = 7 Then
                 wb1.Sheets("CokingRefineryPFD").Range("N69").Value = 2
             Else
                 wb1.Sheets("CokingRefineryPFD").Range("N69").Value = 1
             End If
             
   
            'save datacollectionfile
             wb2.Save
                     
            'Change Refinery Configurations
             wb1.Sheets("CokingRefineryPFD").Range("E19").Value = i
            
            'Copy paste control parameters to PRELIM
            
            'Copy paste to material flow
             wb3.Sheets(Data_Matrix).Range(wb3.Sheets(Data_Matrix).Cells((m - 1) * 2 * 7 + (n - 1) * 7 + i, 34), wb3.Sheets(Data_Matrix).Cells((m - 1) * 2 * 7 + (n - 1) * 7 + i, 91)).Copy
             wb1.Sheets("MFOpt").Range("B2:B59").PasteSpecial Paste:=xlPasteValues, Transpose:=True
            
            'Copy paste to swing cut
             wb3.Sheets(Data_Matrix).Range(wb3.Sheets(Data_Matrix).Cells((m - 1) * 2 * 7 + (n - 1) * 7 + i, 3), wb3.Sheets(Data_Matrix).Cells((m - 1) * 2 * 7 + (n - 1) * 7 + i, 33)).Copy
             wb1.Sheets("CokingRefineryControls").Range("C4:C34").PasteSpecial Paste:=xlPasteValues, Transpose:=True
               
               'selecting capture scenarios
               For k = 1 To 35 Step 1
               wb1.Sheets("CarbonCaptureCalcs").Range("G26").Value = k
               
                For j = 0 To UBound(SourceRanges)
                ' Determine the next free column in the destination row
                NextColumn = wb2.Sheets(Data_Matrix).Cells((m - 1) * 2 * 7 * 35 + (n - 1) * 35 * 7 + (i - 2) * 35 + k + 1, wb2.Sheets(Data_Matrix).Columns.Count).End(xlToLeft).Column + 1
                
                ' Check if the range is a string or a cell reference
                If SourceRanges(j)(0) = "text" Then
                ' Input the string directly
                wb2.Sheets(Data_Matrix).Cells((m - 1) * 2 * 7 * 35 + (n - 1) * 35 * 7 + (i - 2) * 35 + k + 1, NextColumn).Value = SourceRanges(j)(1)
                Else
                ' Copy the value from the specified cell
                Set SourceWorkbook = Workbooks(SourceRanges(j)(0))
                SourceSheet = SourceRanges(j)(1)
                Set SourceRange = SourceWorkbook.Sheets(SourceSheet).Range(SourceRanges(j)(2))

                ' Copy the range from the source sheet
                SourceRange.Copy

                ' Paste to the destination
                wb2.Sheets(Data_Matrix).Cells((m - 1) * 2 * 7 * 35 + (n - 1) * 35 * 7 + (i - 2) * 35 + k + 1, NextColumn).PasteSpecial Paste:=xlPasteValues, Transpose:=True
                
                End If
                
                Application.CutCopyMode = False
                               
                Next j
               
               Application.CutCopyMode = False
               
              Next
              
              Application.CutCopyMode = False
         Next
            
         Application.CutCopyMode = False
        
       Next
        
      Next
      Application.CutCopyMode = False
      Application.ScreenUpdating = True
       
       
      wb1.Close saveChanges:=True
      wb2.Close saveChanges:=True
      wb3.Close saveChanges:=True
      Application.Quit
     'Application.DisplayStatusBar = True
      
    End Sub

