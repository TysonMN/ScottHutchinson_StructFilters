module Domain
open System

[<CLIMutable>]
type ImplementedStruct = {
    Name: string
    [<DefaultValue>] Gml: bool
    [<DefaultValue>] Cml: bool
    [<DefaultValue>] CmlChangeField: bool
    [<DefaultValue>] CmlEntity: bool
}

type FieldId = FieldId of Guid

type MessageType = { 
    ID: int
    Name: string
}

type FieldData = { // Leaf data
    Id: FieldId
    Name: string
    Type: string
    ParentType: string
    IsGml: bool
    IsCml: bool
    IsCmlChangeField: bool
    IsCmlEntity: bool
}

type Model = { 
    MsgType: MessageType
    DummyRoot: RoseTree<FieldData>
}
    with 
        member this.ParentStruct = this.DummyRoot.Fields.Head
        member this.IsEmpty = this.ParentStruct.Data.Type = ""

type StructData = {
    Version: string
    Struct: RoseTree<FieldData>
}
