# Project: Master thesis - ForSyDe Model
# Created by: Marcus Hanikat
# Created Sat 20 Feb 2021
# Contact: hanikat@kth.se
# Copyright: (c) 2021 Marcus Hanikat
#
# Description: Script used to extract signal values from the Signals.xlsx file and create a haskell list from them

#Parameters
param(
    [int]$rangeStart = 0,
    [int]$rangeEnd = [Int]::MaxValue
)

#Load Excel file with signals
$ExcelFile = $PSScriptRoot + "\Signals.xlsx"
$MainFile = $PSScriptRoot + "\src\Main.hs"
$excelObject = New-Object -ComObject Excel.Application 
$excelObject.Visible = $true
$signalsSheet = $excelObject.Workbooks.Open($ExcelFile).Worksheets["Signals"]


# ----- Load Signal A from Excel -----
#Load SignalA[0][0]
$signalA00 = $signalsSheet.UsedRange.Rows.Columns[2].Value2 | select -skip 1
#Load SignalA[0][1]
$signalA01 = $signalsSheet.UsedRange.Rows.Columns[3].Value2 | select -skip 1
#Load SignalA[1][0]
$signalA10 = $signalsSheet.UsedRange.Rows.Columns[4].Value2 | select -skip 1
#Load SignalA[0][0]
$signalA11 = $signalsSheet.UsedRange.Rows.Columns[5].Value2 | select -skip 1

$SignalA = "signalA = signal ["
for($signalIndex = $rangeStart; $signalIndex -lt $signalA00.Count -and $signalIndex -lt $signalA01.Count -and $signalIndex -lt $signalA10.Count -and $signalIndex -lt $signalA11.Count -and $signalIndex -lt $rangeEnd; $signalIndex++) {
    $SignalA += "($($signalA00[$signalIndex]),$($signalA01[$signalIndex]),$($signalA10[$signalIndex]),$($signalA11[$signalIndex])),"    
}
$SignalA = $SignalA.Substring(0,$SignalA.Length-1) + "]"


# ----- Load Signal B from Excel -----
#Load SignalB[0]
$signalB0 = $signalsSheet.UsedRange.Rows.Columns[6].Value2 | select -skip 1
#Load SignalB[1]
$signalB1 = $signalsSheet.UsedRange.Rows.Columns[7].Value2 | select -skip 1

$SignalB = "signalB = signal ["
for($signalIndex = $rangeStart; $signalIndex -lt $signalB0.Count -and $signalIndex -lt $signalB1.Count -and $signalIndex -lt $rangeEnd; $signalIndex++) {
    $SignalB += "($($signalB0[$signalIndex]),$($signalB1[$signalIndex])),"    
}
$SignalB = $SignalB.Substring(0,$SignalB.Length-1) + "]"


# ----- Load Signal C from Excel -----
#Load SignalC[0][0]
$signalC00 = $signalsSheet.UsedRange.Rows.Columns[8].Value2 | select -skip 1
#Load SignalC[0][1]
$signalC01 = $signalsSheet.UsedRange.Rows.Columns[9].Value2 | select -skip 1
#Load SignalC[1][0]
$signalC10 = $signalsSheet.UsedRange.Rows.Columns[10].Value2 | select -skip 1
#Load SignalC[0][0]
$signalC11 = $signalsSheet.UsedRange.Rows.Columns[11].Value2 | select -skip 1

$SignalC = "signalC = signal ["
for($signalIndex = $rangeStart; $signalIndex -lt $signalC00.Count -and $signalIndex -lt $signalC01.Count -and $signalIndex -lt $signalC10.Count -and $signalIndex -lt $signalC11.Count -and $signalIndex -lt $rangeEnd; $signalIndex++) {
    $SignalC += "($($signalC00[$signalIndex]),$($signalC01[$signalIndex]),$($signalC10[$signalIndex]),$($signalC11[$signalIndex])),"    
}
$SignalC = $SignalC.Substring(0,$SignalC.Length-1) + "]"




#Replace Signal A in Main.hs
$mainFileContent = Get-Content $MainFile
#Find lines to replace
$signalACompleteLine = $mainFileContent | Select-String "signalA = signal" | Select-Object -ExpandProperty Line
$signalBCompleteLine = $mainFileContent | Select-String "signalB = signal" | Select-Object -ExpandProperty Line
$signalCCompleteLine = $mainFileContent | Select-String "signalC = signal" | Select-Object -ExpandProperty Line
#Replace lines and write back new file content
$mainFileContent.Replace($signalACompleteLine, $SignalA).Replace($signalBCompleteLine, $SignalB).Replace($signalCCompleteLine, $SignalC) | Set-Content $MainFile

#Cleanup IO objects
$excelObject.Quit()
While([System.Runtime.Interopservices.Marshal]::ReleaseComObject($signalsSheet) -ge 0){}
while([System.Runtime.Interopservices.Marshal]::ReleaseComObject($excelObject) -ge 0){}
