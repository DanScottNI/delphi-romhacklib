unit PatchProgManager;

interface

uses contnrs, inifiles, sysutils;

type
  TPatchProgram = class
  public
    PatchCommandLine : String;
  end;

  TPatchProgramList = class(TObjectList)
  protected
    function GetItem(Index: Integer) : TPatchProgram;
    procedure SetItem(Index: Integer; const Value: TPatchProgram);
  public
    function Add(AObject: TPatchProgram) : Integer;
    property Items[Index: Integer] : TPatchProgram read GetItem write SetItem;default;
    function Last : TPatchProgram;
    procedure Load(pFilename : String);
    function Save(pFilename : String) : Boolean;
  end;

implementation

{ TPatchProgramList }

function TPatchProgramList.Add(AObject: TPatchProgram): Integer;
begin
  Result := inherited Add(AObject);
end;

function TPatchProgramList.GetItem(Index: Integer): TPatchProgram;
begin
  Result := TPatchProgram(inherited Items[Index]);
end;

function TPatchProgramList.Last: TPatchProgram;
begin
  result := TPatchProgram(inherited Last);
end;

procedure TPatchProgramList.Load(pFilename: String);
begin

end;

function TPatchProgramList.Save(pFilename: String): Boolean;
var
  ini : TMemINIFile;
  Num : Integer;
begin
  ini := TMemINIFile.Create(pFilename);
  try
    // Load in the number of patch manager settings.

    ini.UpdateFile;
  finally
    FreeAndNil(ini);
  end;
end;

procedure TPatchProgramList.SetItem(Index: Integer; const Value: TPatchProgram);
begin
  inherited Items[Index] := Value;
end;

end.