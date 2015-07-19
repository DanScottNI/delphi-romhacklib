unit vtreefunc;

interface

uses virtualtrees;

procedure SetObject(Tree: TBaseVirtualTree; Node: PVirtualNode; Obj: TObject);
function GetObject(Tree: TBaseVirtualTree; Node: PVirtualNode): TObject;

implementation

// these 2 functions are written by Werner Lehmann and come from the
// VT support newsgroup.
procedure SetObject(Tree: TBaseVirtualTree; Node: PVirtualNode; Obj: TObject);
var
Data: pointer;
begin
//  assert(Node <> nil);
  Data := Tree.GetNodeData(Node);
//  assert(Data <> nil);
  TObject(Data^) := Obj;
end;

//------------------------------------------------------------------------------

function GetObject(Tree: TBaseVirtualTree; Node: PVirtualNode): TObject;
var
Data: pointer;
begin
//  assert(Node <> nil);
  Data := Tree.GetNodeData(Node);
  if Data = nil then
    Result := nil
  else
    Result := TObject(Data^);
end;


end.
