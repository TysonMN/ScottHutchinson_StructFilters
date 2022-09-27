// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
#load "Domain.fs"
#load "FieldData.fs"
#load "Tree.fs"
#load "Translation.fs"

open System.IO

let exeFolderPath = Path.GetDirectoryName(__SOURCE_DIRECTORY__) +  """\NG-DART\Debug"""
let headerFileName = Path.Combine(exeFolderPath, """Data\MsgTypeDefs\v101101struct.h""")
//let parentStructName = "XSCX_Operator_Alert_t"
//let parentStructName = "CEPIN_New_Cu_Track_t"
let parentStructName = "CEPIN_New_Jam_Track_t"
let parentStuct = Translation.translate headerFileName parentStructName
