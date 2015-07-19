unit SaveState;

interface

uses SysUtils,Classes;

type
  TSaveState = class
  private
    _Filename : String;
    _SSE : Array Of Byte;
    procedure LoadSaveState();
    procedure SaveSavestate();

    function ReadSSE(Index: Integer): Byte;
    procedure WriteSSE(Index: Integer; const Value: Byte);
  public
    property Filename : String read _Filename write _Filename;
    property SSE [Index : Integer] : Byte read ReadSSE write WriteSSE;default;
    constructor Create(pFilename : String);overload;
    constructor Create(pFilename : String;pGZIP : Boolean);overload;
    destructor Destroy;override;
    function  ReadInt(pOffset : Integer) : Integer;
    Procedure WriteInt (Offset:Integer; Value:Integer);
    function  ReadRInt(pOffset : Integer) : Integer;
    Procedure WriteRInt (Offset:Integer; Value:Integer);
    Function  Read3Byte(Offset:Integer): Integer;
    Procedure Write3Byte(Offset:Integer;Value:Integer);
    Function  ReadLong(Offset:Integer): Integer;
    Procedure WriteLong(Offset:Integer;Value:Integer);
    procedure Save;    
  end;

implementation

{ TSaveState }

constructor TSaveState.Create(pFilename: String);
begin
  _Filename := pFilename;
  LoadSaveState();
end;

constructor TSaveState.Create(pFilename: String; pGZIP: Boolean);
begin
end;

procedure TSaveState.LoadSaveState;
var
  FileSizeInBytes : Longword;

  Mem : TMemoryStream;

  ByteCounter : Longword;
begin
  // Modified from Ultima's sm_verify tool's source, and modified
  // by me to use memory streams, and include try .. finally .. end
  // blocks.
  Mem := TMemoryStream.Create(); // open the file
  try
    Mem.LoadFromFile(_Filename);
    FileSizeInBytes := Mem.Size;

    SetLength(_SSE, FileSizeInBytes); // re-dimension the array to hold the file

    ByteCounter := 0; // start at the beginning of the array

    // hooray for streams...no loop
    Mem.Read(_SSE[ByteCounter], FileSizeInBytes);

  finally
    FreeAndNil(Mem); // memory cleanup
  end;

end;

function TSaveState.Read3Byte(Offset: Integer): Integer;
begin

end;

function TSaveState.ReadInt(pOffset: Integer): Integer;
begin
  result := StrToInt('$' + IntToHex(_SSE[pOffset+1],2) + IntToHex(_SSE[pOffset],2));
end;

function TSaveState.ReadLong(Offset: Integer): Integer;
begin

end;

function TSaveState.ReadRInt(pOffset: Integer): Integer;
begin
  result := StrToInt('$' + IntToHex(_SSE[pOffset],2) + IntToHex(_SSE[pOffset+1],2));
end;

function TSaveState.ReadSSE(Index: Integer): Byte;
begin
  result := _SSE[Index];
end;

procedure TSaveState.Save;
begin
  SaveSaveState();
end;

procedure TSaveState.SaveSavestate;
var
  Mem : TMemoryStream;
  i : Integer;
begin

  Mem := TMemoryStream.Create;
  try
    Mem.SetSize(high(_SSE));

    Mem.Position :=0;

    for i := 0 to mem.Size do
      Mem.Write(_SSE[i],1);

//    try
    Mem.SaveToFile(Filename);
//    except
//      on EFCreateError do
//        showmessage('The file is already opened by another program.');
//    end;
  finally
    FreeAndNil(Mem);
  end;
end;

procedure TSaveState.Write3Byte(Offset, Value: Integer);
begin

end;

procedure TSaveState.WriteInt(Offset, Value: Integer);
var
  HexVal : String;
begin
  if Value > 65535 then exit;
  HexVal := IntToHex(Value,4);
  _SSE[Offset+1] := StrToInt('$' + HexVal[1] + HexVal[2]);
  _SSE[Offset] := StrToInt('$' + HexVal[3] + HexVal[4]);
end;

procedure TSaveState.WriteLong(Offset, Value: Integer);
begin

end;

procedure TSaveState.WriteRInt(Offset, Value: Integer);
var
  HexVal : String;
begin
  if Value > 65535 then exit;
  HexVal := IntToHex(Value,4);
  _SSE[Offset] := StrToInt('$' + HexVal[1] + HexVal[2]);
  _SSE[Offset+1] := StrToInt('$' + HexVal[3] + HexVal[4]);
end;

procedure TSaveState.WriteSSE(Index: Integer; const Value: Byte);
begin
  _SSE[Index] := Value;
end;

destructor TSaveState.Destroy;
begin
  SetLength(_SSE,0);
  inherited;
end;

end.
