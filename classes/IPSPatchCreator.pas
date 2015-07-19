unit IPSPatchCreator;

interface

// This is my own class to be used in IPS patch creating.
// It's really only for NES ROMs, as the code loads in two ROMs
// into memory. Which could be a big memory hog for large ROMs.

uses classes, contnrs, sysutils;

type
  TIPSDifference = class
  public
    Start : Integer;
    Size : Integer;
  end;

  TIPSDifferenceList = class(TObjectList)
  protected
    function GetItem(Index: Integer) : TIPSDifference;
    procedure SetItem(Index: Integer; const Value: TIPSDifference);
  public
    function Add(AObject: TIPSDifference) : Integer;
    property Items[Index: Integer] : TIPSDifference read GetItem write SetItem;default;
    function Last : TIPSDifference;
  end;

  TIPSPatcher = class
  private
    _OriginalFilename : String;
    _ModifiedFilename : String;
    _OutputPatch : String;
    _Differences : TIPSDifferenceList;
    procedure CompareDifferences;
  public
    constructor Create(pOrigFilename, pModifiedFilename,pOutputPatch : String);
    destructor Destroy;override;
    procedure SaveIPSPatch;
  end;

implementation

destructor TIPSPatcher.Destroy;
begin
  FreeAndNil(_Differences);
  inherited;
end;

constructor TIPSPatcher.Create(pOrigFilename, pModifiedFilename, pOutputPatch : String);
begin
  _OriginalFilename := pOrigFilename;
  _ModifiedFilename := pModifiedFilename;
  _OutputPatch := pOutputPatch;

  CompareDifferences;

end;

procedure TIPSPatcher.SaveIPSPatch;
begin

end;

procedure TIPSPatcher.CompareDifferences;
var
  OrigROM : Array Of Byte;
  DestROM : Array Of Byte;
  FileSizeInBytes : Longword;
  Mem : TMemoryStream;
  ByteCounter : Longword;
  i,SizeDiff : Integer;
  PrevDiff : Boolean;
begin

  if Assigned(_Differences) then
    FreeAndNil(_Differences);
  _Differences := TIPSDifferenceList.Create(true);

  // Load the original ROM into memory.
  Mem := TMemoryStream.Create(); // open the file
  try
    Mem.LoadFromFile(_OriginalFilename);
    FileSizeInBytes := Mem.Size;

    SetLength(OrigROM, FileSizeInBytes); // re-dimension the array to hold the file

    ByteCounter := 0; // start at the beginning of the array

    // hooray for streams...no loop
    Mem.Read(OrigROM[ByteCounter], FileSizeInBytes);

  finally
    FreeAndNil(Mem); // memory cleanup
  end;

  // Load the modified ROM into memory.
  Mem := TMemoryStream.Create(); // open the file
  try
    Mem.LoadFromFile(_ModifiedFilename);
    FileSizeInBytes := Mem.Size;

    SetLength(DestROM, FileSizeInBytes); // re-dimension the array to hold the file

    ByteCounter := 0; // start at the beginning of the array

    // hooray for streams...no loop
    Mem.Read(DestROM[ByteCounter], FileSizeInBytes);

  finally
    FreeAndNil(Mem); // memory cleanup
  end;

  // Now both ROMs are loaded into memory, we have to compare the two
  // files and see if there are any differences.
  PrevDiff := False;
  SizeDiff := 0;
  for i := 0 to high(OrigROM) do
  begin
    if DestROM[i] <> OrigROM[i] then
    begin

      if PrevDiff = false then
      begin
        _Differences.Add(TIPSDifference.Create);
        PrevDiff := True;
        _Differences.Last.Start := i;
        _Differences.Last.Size := 1;
      end
      else
      begin
        _Differences.Last.Size := _Differences.Last.Size + 1;
      end;

    end
    else
    begin
      PrevDiff := False;
    end;

  end;

  // Also, we need to take into account that there may be some ROM
  // expansion, and if the modified ROM is bigger than the original ROM
  // mark everything after the end of the original ROM as a difference.
  if high(DestROM) > high(OrigROM) then
  begin
    _Differences.Add(TIPSDifference.Create);
    _Differences.Last.Start := high(OrigROM);
    _Differences.Last.Size := high(DestROM) - high(OrigROM);
  end;
end;

{ TIPSDifferenceList }

function TIPSDifferenceList.Add(AObject: TIPSDifference): Integer;
begin
  Result := inherited Add(AObject);
end;

function TIPSDifferenceList.GetItem(Index: Integer): TIPSDifference;
begin
  Result := TIPSDifference(inherited Items[Index]);
end;

procedure TIPSDifferenceList.SetItem(Index: Integer; const Value: TIPSDifference);
begin
  inherited Items[Index] := Value;
end;

function TIPSDifferenceList.Last : TIPSDifference;
begin
  result := TIPSDifference(inherited Last);
end;

end.
