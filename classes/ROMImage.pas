{ ROMImage v1.0 }

unit ROMImage;

interface

uses classes, SysUtils;

type
  TROMImage = class
  protected
    _Filename : String;
    _Changed, _IgnoreChanges : Boolean;
    function ReadROM(Index: Integer): Byte;virtual;
    procedure WriteROM(Index: Integer; const Value: Byte);
    function OrdToBinary(const Value: Byte): string;
    procedure LoadROM(pFilename : String);
    function SaveROM(pFilename : String) : Boolean;
    procedure SetReadOnly(pReadOnly : Boolean);
    function GetReadOnly() : Boolean;
    function GetROMSize: Integer;
  public
    RawROM : Array Of Byte;
    constructor Create(pFilename: String);
    destructor Destroy;override;
    function ByteToBin(OneByte: Byte): String;
    function BoolToByte(pBool: Boolean): Byte;
    function BinToByte(BinaryString: String): Byte;
    function ApplyIPSPatch(pIPSFile: String): boolean;
    property Filename : String read _Filename write _Filename;
    property Changed : Boolean read _Changed write _Changed;
    property IgnoreChanges : Boolean read _IgnoreChanges write _IgnoreChanges;
    property WriteProtected : Boolean read GetReadOnly write SetReadOnly;
    function Save : Boolean;
    function SaveAs (pFilename : String) : Boolean;
    procedure SetROMLength(pSize : Integer);
    property ROMSize : Integer read GetROMSize;
  end;

implementation

function TROMImage.OrdToBinary(const Value: Byte): string;
var
  I: Integer;
  B: Byte;
  P: PChar;
begin
  SetLength(Result, 8);
  P := PChar(Result) + ((8 - 1) * SizeOf(Char));
  B := Value;
  for I := 0 to 8 - 1 do
  begin
    P^ := Chr(48 + (B and $00000001));
    Dec(P);
    B := B shr 1;
  end;

end;

procedure TROMImage.LoadROM(pFilename : String);
var
  Mem : TMemoryStream;
begin
  // Modified from Ultima's sm_verify tool's source, and modified
  // by me to use memory streams, and include try .. finally .. end
  // blocks.
  Mem := TMemoryStream.Create(); // open the file
  try
    Mem.LoadFromFile(pFilename);
    SetLength(RawROM, Mem.Size); // re-dimension the array to hold the file
    Mem.Read(RawROM[0], Mem.Size);
  finally
    FreeAndNil(Mem); // memory cleanup
  end;
end;

function TROMImage.SaveROM(pFilename : String) : Boolean;
var
  Mem : TMemoryStream;
  i : Integer;
begin
  result := true;
  Mem := TMemoryStream.Create;
  try
    Mem.SetSize(high(RawROM));
    Mem.Position :=0;

    for i := 0 to mem.Size do
      Mem.Write(RawROM[i],1);

    try
      Mem.SaveToFile(pFilename);
    except
      on EFCreateError do
        result := false;
    end;
  finally
    FreeAndNil(Mem);
  end;
  _changed := false;

end;

constructor TROMImage.Create(pFilename: String);
begin

  // Load the filename variable with the argument
  // specified in piNESImagePath.
  _Filename := pFilename;
  // Load the entire ROM into memory.
  LoadROM(_Filename);
  // Load its header into memory.
  _changed := false;
  _ignorechanges := False;

end;

function TROMImage.Save : Boolean;
begin
  result := SaveROM(_Filename);
end;

function TROMImage.SaveAs (pFilename : String) : Boolean;
begin
  result := SaveROM(pFilename);
end;

function TROMImage.ReadROM(Index: Integer): Byte;
begin
  result := self.RawROM[Index];
end;

procedure TROMImage.WriteROM(Index: Integer; const Value: Byte);
begin
  if (RawROM[Index] <> Value) and (_ignorechanges = false) then
    Changed := True;

  self.RawROM[Index] := Value;

end;

procedure TROMImage.SetReadOnly(pReadOnly : Boolean);
begin
  FileSetReadOnly(self._FileName,pReadOnly);
end;

function TROMImage.GetReadOnly() : Boolean;
var
  Attributes : word;
begin
  Attributes := FileGetAttr(self._Filename);
  if Attributes and faReadOnly = faReadOnly then
    result := True
  else
    result := False;
end;

function TROMImage.BoolToByte(pBool : Boolean) : Byte;
begin
  if pBool = true then
    result := 1
  else
    result := 0;

end;

Function TROMImage.BinToByte(BinaryString : String): Byte;
var
Value : Byte;
begin

  Value := 0;
  if BinaryString[1] = '1' Then Value := Value + 1;
  if BinaryString[2] = '1' Then Value := Value + 2;
  if BinaryString[3] = '1' Then Value := Value + 4;
  if BinaryString[4] = '1' Then Value := Value + 8;
  if BinaryString[5] = '1' Then Value := Value + 16;
  if BinaryString[6] = '1' Then Value := Value + 32;
  if BinaryString[7] = '1' Then Value := Value + 64;
  if BinaryString[8] = '1' Then Value := Value + 128;

  BinToByte := Value;
end;

Function TROMImage.ByteToBin(OneByte : Byte) : String;
var
  BinaryString : String;
begin

  BinaryString := OrdToBinary(OneByte);
  ByteToBin := copy(BinaryString,1, 8);
end;

destructor TROMImage.Destroy;
begin
  SetLength(RawROM,0);
  inherited;
end;

procedure TROMImage.SetROMLength(pSize: Integer);
begin
  SetLength(RawROM,pSize);
end;

function TROMImage.GetROMSize: Integer;
begin
  result := Length(RawROM);
end;

function TROMImage.ApplyIPSPatch(pIPSFile : String):boolean;
var
  Offset : Integer;
  Size : Word;
  i : Integer;
  RLESize : Word;
  RLEVal : Byte;
  IPSPosition : Integer;
  PatchMem : TMemoryStream;
  IPSByte : Array of Byte;
begin
  // First, it is best to load the IPS into memory. For
  // speed. The actual file that is being patched isn't
  // loaded into memory, as it needs to be dynamic-resized.
  // (Probably).
  PatchMem := TMemoryStream.Create(); // open the file
  try
    PatchMem.LoadFromFile(pIPSFile);
    SetLength(IPSByte, PatchMem.Size); // re-dimension the array to hold the file
    PatchMem.Read(IPSByte[0], PatchMem.Size);
  finally
    FreeAndNil(PatchMem);
  end;

  try
    // Set the position of the Memory Stream to 0
    // (Just in case).
    //Position :=0;
    // Next, we check if the first five bytes of the
    // IPS file are 'PATCH'. If they are not display a messagebox
    // and exit the subroutine.
    If (IPSByte[0] <> 80) or (IPSByte[1] <> 65) or (IPSByte[2] <> 84) or (IPSByte[3] <> 67) or (IPSByte[4] <> 72) then
    begin
      result := False;
      Exit;
    end;
    // If we are here, the patch has a valid PATCH header.
    // Next, we need to load in the offset for the first patch
    // To do this, we set the IPSCounter value to
    IPSPosition := 5;

    while (IPSPosition < high(IPSByte)) do
    begin
      if (IPSByte[IPSPosition] = 69) and (IPSByte[IPSPosition + 1] = 79)
        and (IPSByte[IPSPosition + 2] =70) then
          break;

      // First load in the offset (3-bytes)
      Offset := StrToInt('$' + IntToHex(IPSByte[IPSPosition],2) +
        IntToHex(IPSByte[IPSPosition+1],2) +
        IntToHex(IPSByte[IPSPosition+2],2));
      IPSPosition := IPSPosition + 3;

      // Next load in the size of the bytes (2-bytes)
      Size := StrToInt('$' + IntToHex(IPSByte[IPSPosition],2) +
        IntToHex(IPSByte[IPSPosition+1],2));
      IPSPosition := IPSPosition + 2;

      // Now we need to check if the size value is 0.
      // if it is, it's using RLE.
      if Size = 0 then
      begin
        RLESize := StrToInt('$' + IntToHex(IPSByte[IPSPosition],2) +
          IntToHex(IPSByte[IPSPosition+1],2));
        IPSPosition := IPSPosition + 2;

        RLEVal := IPSByte[IPSPosition];
        inc(IPSPosition);

        for i := 0 to RLESize -1 do
          RawROM[Offset + i] := RLEVal;
      end
      else if Size >0 then
      begin
        for i := IPSPosition to (IPSPosition + Size -1) do
          RawROM[Offset + (i - IPSPosition)] := IPSByte[i];

        IPSPosition := IPSPosition + Size;

      end;
    end;
  finally
    Finalize(IPSByte);
  end;
  Result := True;
end;

end.
