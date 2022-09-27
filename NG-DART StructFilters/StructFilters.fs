module NGDartStructFilters.StructFilters

#nowarn "9" //FSharp.NativeInterop.NativePtr.ofNativeInt can't be verified as safe

open System.Diagnostics
open System.IO
open System.Windows
open Elmish
open Elmish.WPF
open NGDartStructFilters.Views

module App =
    open System.Reflection
    open Newtonsoft.Json
    open Domain

    let exeFolderPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

    let mutable window = null
    let mkNewWindow () = 
        window <- StructFiltersWindow()
        window

    let mutable implementedStructs = [] // gets loaded once the first time the user shows the StructFiltersWindow

    let loadImplementedStructs () =
        match implementedStructs with
        | [] ->
            let filePath = Path.Combine(exeFolderPath, """Data\MsgTypeDefs\implemented_structs.json""")
            let str = File.ReadAllText filePath
            implementedStructs <- JsonConvert.DeserializeObject<ImplementedStruct list>(str)
        | _ -> ()

    let mutable unmodifiedModel = { 
        MsgType = { ID = 0; Name = "NullMsg" }
        DummyRoot = FieldData.empty |> RoseTree.asLeaf
    }

    let mutable configFolderPath = ""

    let getConfigFolder path =
        let folder = Path.Combine(path, "StructFilters")
        if not (Directory.Exists(folder)) then Directory.CreateDirectory(folder) |> ignore
        folder

    let getConfigFile structName =
        Path.Combine(configFolderPath, sprintf "%s.json" structName)

    let version = "1.0.0" // semantic version of serialization data format

    let deserializeStruct file =
        let str = File.ReadAllText file
        JsonConvert.DeserializeObject<StructData>(str)

    let rec traverse (projection: RoseTree<FieldData> -> RoseTree<FieldData>) fld =
        let fields' =
            fld.Fields
            |> List.map (fun f -> 
                projection f
                |> traverse projection
            )
        { fld with Fields = fields' }

    let loadSavedChanges dummyRoot = 
        let updateFromFile (field: RoseTree<FieldData>) =
            let updateFields fileFld inMemoryFld =
                //TODO: Need to merge in the existing fields that are missing from the currently loaded struct
                //e.g. the on disk struct may have fields that exist in that version, but not in the current one
                //Not sure how to do this in F#
                inMemoryFld.Fields 
                |> List.map (fun f ->
                    fileFld.Fields
                    |> List.tryFind (fun fld -> fld.Data.Name = f.Data.Name)
                    |> Option.map (fun fld -> { f with Data = fld.Data })
                    |> Option.defaultValue f
                )
            let file = getConfigFile field.Data.Type
            if (not field.Fields.IsEmpty) && File.Exists file then
                let structData = deserializeStruct file
                if isNull structData.Version then // ignore old file missing the version number
                    field
                else
                    // Check the structData.Version number here as needed.
                    let ff = structData.Struct
                    { field with Fields = updateFields ff field }
            else field
        traverse updateFromFile dummyRoot

    /// Returns the specified resource as a string
    let LoadResourceAsString (structHdrResource: System.IntPtr) (length: int) =
        use umm = new UnmanagedMemoryStream( //Convert the HGLOBAL resource stream into a MemoryStream to avoid copying the data
                            FSharp.NativeInterop.NativePtr.ofNativeInt (WinAPI.LockResource structHdrResource), // FS0009, Uses of this construct may result in the generation of unverifiable .NET IL code.
                            System.Convert.ToInt64(length))
        use decompressor = new System.IO.Compression.GZipStream(umm, Compression.CompressionMode.Decompress)
        use rawDecompressedStream = new MemoryStream()
        decompressor.CopyTo(rawDecompressedStream)
        rawDecompressedStream.Seek(0L, SeekOrigin.Begin) |> ignore
        use reader = new System.IO.StreamReader(rawDecompressedStream, System.Text.Encoding.ASCII)
        reader.ReadToEnd()

    let init (structHdrResourceHandle: System.IntPtr) (structHdrDataLength: int) (msgTypeID: int) (msgTypeName: string) (parentStructName: string) measureElapsedTime () =
        let parentStruct = Translation.translate (LoadResourceAsString structHdrResourceHandle structHdrDataLength) parentStructName
        let dummyRoot =
            if parentStruct.Data.Type = "" then
                // Add one child to prevent a StackOverflow in Elmish.WPF.ViewModel.initializeBinding function.
                let dummyChild = FieldData.create "Parent struct not found" "" ""
                [ dummyChild |> RoseTree.asLeaf ] |> FieldData.asDummyRoot
            else
                [ parentStruct ] |> FieldData.asDummyRoot
        measureElapsedTime "App.init"
        unmodifiedModel <- 
            { MsgType = { ID = msgTypeID; Name = msgTypeName }
              DummyRoot = loadSavedChanges dummyRoot
            }
        unmodifiedModel

    type SubtreeMsg =
        | GmlSetChecked of gmlChecked: bool
        | CmlSetChecked of cmlChecked: bool
        | CmlChangeFieldSetChecked of changeChecked: bool
        | CmlEntitySetChecked of entityChecked: bool
        //| RightClick of entityChecked: bool

    [<Struct>]
    type Msg =
        | SubtreeMsg of RoseTreeMsg<FieldId, SubtreeMsg>
        | Save
        | Cancel
        | GmlSelectAll
        | GmlClearAll

    let private isGmlImplementedForParent fd =
        implementedStructs
        |> List.tryFind (fun s -> s.Name = fd.ParentType && s.Gml)
        |> Option.isSome

    let private isGmlImplementedForStruct fd =
        implementedStructs
        |> List.tryFind (fun s -> s.Name = fd.Type && s.Gml)
        |> Option.isSome

    let private isCmlImplementedForParent fd =
        implementedStructs
        |> List.tryFind (fun s -> s.Name = fd.ParentType && s.Cml)
        |> Option.isSome

    let private isCmlChangeFieldImplementedForParent fd =
        implementedStructs
        |> List.tryFind (fun s -> s.Name = fd.ParentType && s.CmlChangeField)
        |> Option.isSome

    let private isCmlEntityImplementedForParent fd =
        implementedStructs
        |> List.tryFind (fun s -> s.Name = fd.ParentType && s.CmlEntity)
        |> Option.isSome

    let setGmlForAll isGml m =
        let setGml (field: RoseTree<FieldData>) =
            if isGmlImplementedForParent field.Data then
                let fd = field.Data |> FieldData.setIsGml isGml
                { field with Data = fd }
            else
                field
        let dummyRoot' = traverse setGml m.DummyRoot
        {m with DummyRoot = dummyRoot'}

    let selectAllForGml = setGmlForAll true

    let clearAllForGml = setGmlForAll false

    let getChangedStructs m =
        let isNotEqual (fldA: RoseTree<FieldData>) (fldB: RoseTree<FieldData>) = fldA.Data <> fldB.Data
        // Depth First Search
        let rec diff acc oldFld newFld =
            match oldFld.Fields with
            | [] -> acc
            | fields -> 
                let acc' =
                    if (fields, newFld.Fields) ||> List.exists2 isNotEqual then
                        RoseTree.depthAtMost1 newFld :: acc
                    else
                        acc

                (acc', fields, newFld.Fields)
                |||> List.fold2 (fun accu oldChild newChild -> diff accu oldChild newChild)
        
        diff [] unmodifiedModel.DummyRoot.Fields.Head m.DummyRoot.Fields.Head

    let serializeStructs flds =
        let ser = JsonSerializer()
        ser.Formatting <- Newtonsoft.Json.Formatting.Indented
        flds
        |> List.iter (fun (f: RoseTree<FieldData>) ->
            let file = sprintf "%s.json" f.Data.Type
            let filePath = Path.Combine(configFolderPath, file)
            use writer = File.CreateText(filePath)
            let structData = { Version = version; Struct = f }
            ser.Serialize(writer, structData)
        )

    let showContextMenu (*isChecked*) (fld:FieldData) =
        fld

    let updateFieldData = function
        | GmlSetChecked isChecked ->
            //RoseTree.nodesCount <- 1 // just for testing how many nodes are in the tree.
            // Instead of using the mutable RoseTree.nodesCount, pass the tree into RoseTree.size
            isChecked |> FieldData.setIsGml
        | CmlSetChecked isChecked ->
            isChecked |> FieldData.setIsCml
        | CmlChangeFieldSetChecked isChecked ->
            isChecked |> FieldData.setIsCmlChangeField
        | CmlEntitySetChecked isChecked ->
            isChecked |> FieldData.setIsCmlEntity
        //| RightClick isChecked ->
        //    isChecked |> showContextMenu

    let updateSubtree msg = msg |> updateFieldData |> RoseTree.mapData

    let hasId id (fd: RoseTree<FieldData>) = fd.Data.Id = id

    let mapDummyRoot f m =
        { m with DummyRoot = m.DummyRoot |> f }

    let update msg m =
        match msg with
        | SubtreeMsg msg ->
            msg |> RoseTree.update hasId updateSubtree |> mapDummyRoot <| m
        | Save -> 
            let changedStructs = getChangedStructs m
            serializeStructs changedStructs
            window.Close()
            m
        | Cancel -> 
            window.Close()
            m
        | GmlSelectAll ->
            selectAllForGml m
        | GmlClearAll ->
            clearAllForGml m

    let rec fieldBindings level () : Binding<Model * (RoseTree<FieldData> * RoseTree<FieldData>), RoseTreeMsg<FieldId, SubtreeMsg>> list = [
        // TODO: Hide ignored fields: spares and pads.
        "Name" |> Binding.oneWay(fun (_, (_, fd)) -> fd.Data.Name)
        "Type" |> Binding.oneWay(fun (_, (_, fd)) -> fd.Data.Type)
        "IsGml" |> Binding.twoWay(
            (fun (_, (_, (fd: RoseTree<FieldData>))) -> fd.Data.IsGml),
            (fun v _ -> v |> GmlSetChecked |> LeafMsg)
        )
        "IsCml" |> Binding.twoWay(
            (fun (_, (_, (fd: RoseTree<FieldData>))) -> fd.Data.IsCml),
            (fun v _ -> v |> CmlSetChecked |> LeafMsg)
        )
        "IsCmlChangeField" |> Binding.twoWay(
            (fun (_, (_, (fd: RoseTree<FieldData>))) -> fd.Data.IsCmlChangeField),
            (fun v _ ->  v |> CmlChangeFieldSetChecked |> LeafMsg)
        )
        "IsCmlEntity" |> Binding.twoWay(
            (fun (_, (_, (fd: RoseTree<FieldData>))) -> fd.Data.IsCmlEntity),
            (fun v (_, (_, fd)) -> v |> CmlEntitySetChecked |> LeafMsg)
        )
        "RightClick" |> Binding.oneWay(fun (_, (_, fd)) -> showContextMenu fd.Data)
        "IsEnabled" |> Binding.oneWay(fun (m, _) -> not m.IsEmpty)
        "IsExpanded" |> Binding.oneWay(fun _ -> level < 2)
        (* TODO: Special DX sub-MTs 
           1. Hide the GML checkbox.
           (because that would duplicate functionality on the Special DX tab).
           2. Add the sub-MT description to the display name.
        *)
        "GmlImplementedForStruct" |> Binding.oneWay(fun (_, (_, rt)) -> isGmlImplementedForStruct rt.Data)
        "GmlImplementedForParent" |> Binding.oneWay(fun (_, (_, rt)) -> isGmlImplementedForParent rt.Data)
        "CmlImplementedForParent" |> Binding.oneWay(fun (_, (_, rt)) -> isCmlImplementedForParent rt.Data)
        "CmlChangeFieldImplementedForParent" |> Binding.oneWay(fun (_, (_, rt)) -> isCmlChangeFieldImplementedForParent rt.Data)
        "CmlEntityImplementedForParent" |> Binding.oneWay(fun (_, (_, rt)) -> isCmlEntityImplementedForParent rt.Data)
        "CmlEntityImplementedForParent" |> Binding.oneWay(fun (_, (_, rt)) -> isCmlImplementedForParent rt.Data)
        "ParentStruct"            |> Binding.oneWay(fun (_, (_, rt)) -> FieldData.isParentStruct rt.Data)
        "ChildFields" |> Binding.subModelSeq(
            (fun (_, (_, c)) -> c.Fields |> Seq.map (fun gc -> (c, gc))),
            (fun ((m, _), gc) -> (m, gc)),
            (fun (_, (_, c)) -> c.Data.Id),
            (fun (id, msg) -> msg |> RoseTree.branchMsg id),
            fieldBindings (level + 1)
        )
    ]

    let rootBindings () : Binding<Model, Msg> list = [
        "IsEnabled" |> Binding.oneWay(fun m -> not m.IsEmpty)
        "MsgType" |> Binding.oneWay(fun m -> m.MsgType)
        "Fields" |> Binding.subModelSeq(
          (fun m -> m.DummyRoot.Fields |> Seq.map (fun c -> (m.DummyRoot, c))),
          (fun (_, c) -> c.Data.Id),
          (fun (id, msg) -> msg |> RoseTree.branchMsg id |> SubtreeMsg),
          fieldBindings 0
        )
        "Save" |> Binding.cmd Save
        "Cancel" |> Binding.cmd Cancel
        "GmlSelectAll" |> Binding.cmd GmlSelectAll
        "GmlClearAll" |> Binding.cmd GmlClearAll
]

module PublicAPI = 

    let loadWindow (structHdrResourceHandle: System.IntPtr) (structHdrDataLength: int) (msgTypeID: int) (msgTypeName: string) (parentStructName: string) (configFolder: string)
        (isSuccessful: byref<bool>) =
        try
            App.loadImplementedStructs ()
            App.configFolderPath <- App.getConfigFolder configFolder
            let mutable timeMeasurementString = ""
            let watch = Stopwatch.StartNew()
            let measureElapsedTime label =
                let elapsed = watch.Elapsed // <-- explicit copy prevents level 5 warning FS0052.
                timeMeasurementString <- sprintf "%s | %s Elapsed Time: %.1f seconds" 
                    timeMeasurementString label elapsed.TotalSeconds
                watch.Restart()

            let waitWindow = WaitWindow()
            App.mkNewWindow().Activated.Add (fun _ -> 
                measureElapsedTime "TreeView loading"
                waitWindow.Close()
                )
            let init = App.init structHdrResourceHandle structHdrDataLength msgTypeID msgTypeName parentStructName measureElapsedTime
            let showDialogWithConfig config (window: Window) program =
                waitWindow.Show()
                Program.startElmishLoop config window program
                window.ShowDialog ()

            Program.mkSimpleWpf init App.update App.rootBindings
            |> showDialogWithConfig ElmConfig.Default (App.window)
            |> ignore

            isSuccessful <- true

            // Return diagnostic information in a string.
            sprintf "StructFilters Tree View (MT 0x%04X)%s" 
                msgTypeID timeMeasurementString
        with ex ->
            isSuccessful <- false
            sprintf "StructFilters loadWindow Exception: %s" ex.Message

    let tryGetFilters (structName: string) (configFolder: string) (fields: byref<string array>) (isGML: bool) =
        App.configFolderPath <- App.getConfigFolder configFolder
        let file = App.getConfigFile structName
        if File.Exists file then
            let structData = App.deserializeStruct file
            if isNull structData.Version then // ignore old file missing the version number
                false
            else
                // Check the structData.Version number here as needed.
                let fld = structData.Struct
                let outputType (field : Domain.FieldData) (isGML: bool) = 
                    if isGML then
                        field.IsGml
                    else
                        field.IsCml
                fields <-
                    (
                        fld.Fields
                        |> List.toArray
                        |> Array.choose (fun f -> if (outputType f.Data isGML) then Some f.Data.Name else None )
                    )
                true
        else false
