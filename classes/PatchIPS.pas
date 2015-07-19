unit PatchIPS;

interface

uses Classes, SysUtils;

  type
    TIPSPatch = class
    private
      function ConvertIntTo3Byte(pInteger : Integer):Integer;
    public
      procedure CreateIPSPatch(pOriginalFile,pModifiedFile, pDestFile : String);
			function IPSPatch(pFileToPatch, pIPSFile : String):boolean;
    end;

implementation

{ TIPSPatch }

procedure TIPSPatch.CreateIPSPatch(pOriginalFile,pModifiedFile, pDestFile : String);
begin

end;

function TIPSPatch.ConvertIntTo3Byte(pInteger : Integer):Integer;
var
  TempByte : Byte;
begin
  // Reverse the first three bytes in the int.
  TempByte := pInteger and $FF;
  TempByte := (pInteger shr 8) and $FF;
  TempByte := (pInteger shr 16) and $FF;
end;

function TIPSPatch.IPSPatch(pFileToPatch, pIPSFile : String):boolean;
begin
{var
  Offset : Cardinal;
  Size : Word;
  Data : Array of Byte;
  i : Integer;
  RLESize : Word;
  RLEVal : Byte;
  Position : Cardinal;
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
    Position := 5;

    while (Position < high(IPSByte)) do
    begin
      if (IPSByte[Position] = 69) and (IPSByte[Position + 1] = 79)
        and (IPSByte[Position + 2] =70) then
          break;

      // First load in the offset (3-bytes)
      Offset := StrToInt('$' + IntToHex(IPSByte[Position],2) +
        IntToHex(IPSByte[Position+1],2) +
        IntToHex(IPSByte[Position+2],2));
      Position := Position + 3;

      // Next load in the size of the bytes (2-bytes)
      Size := StrToInt('$' + IntToHex(IPSByte[Position],2) +
        IntToHex(IPSByte[Position+1],2));
      Position := Position + 2;

      // Now we need to check if the size value is 0.
      // if it is, it's using RLE.
      if Size = 0 then
      begin
        RLESize := StrToInt('$' + IntToHex(IPSByte[Position],2) +
          IntToHex(IPSByte[Position+1],2));
        Position := Position + 2;

        RLEVal := IPSByte[Position];
        inc(Position);
        ROMToBePatched.Seek(Offset,soFromBeginning);
        for i := 0 to RLESize -1 do
          ROMToBePatched.Write(RLEVal,1);

      end
      else if Size >0 then
      begin
        // Next resize the data array to the size of the
        // data and store the data inside the array
        SetLength(Data,Size);
        ROMToBePatched.Seek(Offset,soFromBeginning);
        for i := Position to (Position + Size -1) do
          ROMToBePatched.Write(IPSByte[i],1);

        position := Position + Size;

      end;
    end;

  finally
    Finalize(IPSByte);
  end;
  Result := True;}
end;


end.
