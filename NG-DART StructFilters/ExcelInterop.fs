(* This file has nothing to do with Struct Filters, but I didn't want to
   create another F# project just for this file.
*)
module NGDartStructFilters.ExcelInterop

open System
open System.IO
open Microsoft.Office.Interop

 let public OpenExcelFile path classification =
    let excelApp = Excel.ApplicationClass()
    let wb = excelApp.Workbooks.Open(path)
    let ws = wb.ActiveSheet :?> Excel.Worksheet

    // Add classification to header and footer for printouts.
    ws.PageSetup.CenterHeader <- classification
    ws.PageSetup.CenterFooter <- classification
    ws.PageSetup.RightFooter <- "Page &P of &N"
    let firstDataRowNum = 7 // First six rows contain file info.
    let numRowsAfterData = 2 // one blank row and one row for the classification
    let lastDataRowNum = ws.UsedRange.Rows.Count - numRowsAfterData
    let getCell row col = ws.Cells.[row, col]
    let firstDataColumn = ws.Range(getCell firstDataRowNum 1, getCell lastDataRowNum 1)
    // Suppress the dialog "There's already data here. Do you want to replace it?"
    excelApp.DisplayAlerts <- false
    firstDataColumn.TextToColumns(DataType=Excel.XlTextParsingType.xlDelimited, 
        TextQualifier=Excel.XlTextQualifier.xlTextQualifierDoubleQuote, Semicolon=true) |> ignore

    excelApp.DisplayAlerts <- true
    let numDataColumns = ws.UsedRange.Columns.Count

    // Add column headings above the data range.
    // TODO: Derive column headings from field names (seems like a lot of work)
    let columnHeadingValues = 
        seq {1 .. numDataColumns}
        |> Seq.map (fun i -> (sprintf "C%d" i))
        |> Seq.toArray

    // insert a row above the data for the column headings
    let columnHeadingRow = ws.Rows.[1] :?> Excel.Range
    columnHeadingRow.Insert(Shift=Excel.XlInsertShiftDirection.xlShiftDown) |> ignore
    let lastDataRowNum = lastDataRowNum + 1 // shifted down 1 row
    let columnHeadingRowNum = firstDataRowNum
    let columnHeadings = ws.Range(getCell columnHeadingRowNum 1, getCell columnHeadingRowNum numDataColumns)
    columnHeadings.Value2 <- columnHeadingValues // excelApp.WorksheetFunction.Transpose(columnHeadingValues)
    columnHeadings.HorizontalAlignment <- Excel.XlHAlign.xlHAlignCenter

    // Now adjust the dataCells range to include all the columns we just parsed.
    let dataCells = ws.Range(getCell firstDataRowNum 1, getCell lastDataRowNum numDataColumns)
    dataCells.Name <- "DataRange" // mostly to verify that it's the correct range
    dataCells.Columns.AutoFit() |> ignore
    // Adjust the width of Column 1, whose auto-width is too wide.
    ws.Range(getCell 1 1, getCell 1 1).ColumnWidth <- 22
    // Add a Data Filter for the messages rows
    // See https://docs.microsoft.com/en-us/dotnet/api/microsoft.office.tools.excel.namedrange.autofilter?view=vsto-2017
    dataCells.AutoFilter(Field=1, VisibleDropDown=true) |> ignore
            //dataCells->AutoFilter(1, Type::Missing, Excel::XlAutoFilterOperator::xlAnd, Type::Missing, true);
    let xlsxFilePath = Path.ChangeExtension(path, ".xlsx")
    (*
        • Make sure the file name does not contain any of the following characters:  <  >  ?  [  ]  :  | or  *
        • Make sure the file/path name doesn't contain more than 218 characters.
    *)
    wb.SaveAs(Filename=xlsxFilePath, FileFormat=Excel.XlFileFormat.xlWorkbookDefault, 
        AccessMode=Excel.XlSaveAsAccessMode.xlNoChange)

    excelApp.Visible <- true
    excelApp.WindowState <- Excel.XlWindowState.xlMaximized
    // Now we can delete the .csv file, since we have an open copy in .xslx format.
    File.Delete path
