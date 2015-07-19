{

  iNESImage v1.0

  A unit designed to encapsulate a NES ROM (iNES format)

  Updates:

  29-04-2005 ~
  - Removed ReturnColor32NESPal (or whatever it's called :P)
    It breaks compatibility in some of my older programs, but
    it's an entirely pointless routine.
  - Recoded the NES tile drawing routine. Now much speedier
    compared to the old code. (I wrote a benchmarking program,
    my old code took 3 seconds to draw 32737 tiles, but my new
    code takes 78 milliseconds to do the same amount!) Further
    optimisation is still possible.

  15-02-2005 ~
  - Added a data logging capability.

  12-02-2005 ~
  - Restructured the entire class hierarchy of TiNESImage.
    TiNESImage now descends from a TROMImage class, which
    encapsulates all the basic functionality of a ROM
    regardless of the system that it is for.

  17-01-2005 ~
  - Added a function to draw the NES colours on a TBitmap32.

  23-11-2004 ~
  - Remove the deprecated functions completely.

  20-11-2004 ~
  - Added a way to tell if the ROM has the read-only flag set.

  13-09-2004 ~
  - Deprecated the ReturnPointerOffset, etc, and implemented new methods.

  19-08-2004 ~
  - Renamed the _ROM byte array to RawROM and made it public.
    This breaks the encapsulation, but unfortunately it is needed
    to allow a pointer to be passed to this array.
  - Added a new overloaded method of DrawNESTile. This version
  uses pointers to point to the various byte arrays, and is actually
  reusuable unlike the previous routine.
  - Removed some needless variables and replaced them with functions
  that return values from the underlaying RawROM array.

  10-07-2004 ~
  - Added some checking code that determines whether the ROM has
  a DiskDude! tag.

  04-06-2004 ~
  - Added a procedure that takes an offset to convert
  to a pointer, and another offset to store aforementioned pointer at.
  (StorePointerOffset)

}

unit iNESImage;

interface

uses GR32, Classes,  SysUtils, Graphics, Dialogs,pasCRC32, ROMImage, cExceptions;

type

  TiNESImage = class(TROMImage)
  protected
    _NESPal : Array [0 .. 63] Of TColor32;
    _ValidImage : Boolean;
    _BackupBank : Array of Byte;
    function ReadNESPal(Index: Integer): TColor32;
    procedure WriteNESPal(Index: Integer; const Value: TColor32);
    procedure ReadMapperData();
    function ReadROM(Index: Integer): Byte;override;
    procedure ApplyTrainer;
    procedure RemoveTrainer;
    function GetTrainer : Boolean;
    procedure SetTrainer (pTrainer : Boolean);
  public
    function CRC32: LongWord;
    procedure BackupBank(pBankNumber : Integer);overload;
    procedure BackupBank(pBankNumber : Integer;pBankSize : Integer);overload;
    procedure RestoreBank(pBankNumber : Integer);
    function CalculateBank(pOffset : integer) : Integer;
    function PointerToOffset(pOffset : Integer; pBank: Integer = $0; pMemAddress : Integer = $8000): Integer;
    function OffsetToPointer(pOffset, pMemAddress: Integer): Integer;overload;
    function OffsetToPointerS(pOffset : Integer; pMemAddress: Integer = $8000): String;overload;
    function FindFreeSpace(pSize,pStartOffset,pEndOffset : Integer) : Integer;
    procedure StorePointerOffset(pOffsetPointer,pStoreOffset : Integer;pMemAddress : Integer = $8000);
    procedure LoadPaletteFile(pFilename : String);
    Procedure LoadDefaultPalette();
    procedure DrawNESColours(var pBitmap : TBitmap32);
    procedure DrawNESTile(pData : pointer;var pBitmap: TBitmap32; pX,pY : Integer;pPalette : pointer);overload;
    // A function that returns the mapper number
    function MapperNumber : Integer;
    // A function that returns the mapper name.
    function MapperName : String;
    // A function that returns the number of PRG banks
    function PRGCount : Integer;
    // A function that returns the number of CHR banks
    function CHRCount : Integer;
    // Whether the iNES header contains the DiskDude! text.
    function DiskDudePresent : Boolean;
    // A property that returns whether the file is a valid
    // iNES ROM
    property ValidImage : Boolean read _ValidImage;
    property ROM [Index : Integer] : Byte read ReadROM write WriteROM;default;
    property NESPal [Index : Integer] : TColor32 read ReadNESPal write WriteNESPal;
    property Trainer : Boolean read GetTrainer write SetTrainer;
    constructor Create(pFilename: String);overload;
    constructor Create(pFilename : String;pPaletteFile : String);overload;
  end;

// Accessor class with a static member variable that points to an instance of
// iNESImage. This class should be inherited from.
type
  TiNESImageAccessor = class
  public
    class var ROM : TiNESImage;
  end;


implementation
{$WARN SYMBOL_DEPRECATED OFF}
var
  MapperNames : Array [0..255] of String =
  ('NROM','Nintendo MMC1','UNROM','CNROM','Nintendo MMC3','Nintendo MMC5',
  'FFE F3xxx series','AOROM','FFE F3xxx series','Nintendo MMC2',
  'Nintendo MMC4','Colour Dreams','12/Unknown','CPROM','14/Unknown',
  '100-in-1 chip','Bandai chip','FFE F8xxx','Jaleco SS8806','Namcot 106',
  'Nintendo Disk System','Konami VRC4 2A','Konami VRC4 1B','Konami VRC2b','Konami VRC6','Konami VRC4',
  'Konami VRC6V','27/Unknown','28/Unknown','29/Unknown',
  '30/Unknown','31/Unknown','Irem G101','Taito TC0190/TC0350','Nina-1','35/Unknown',
  '36/Unknown','37/Unknown','38/Unknown','39/Unknown',
  'SMB2j','Caltron 6-in-1','Mario Baby','43/Unknown','7-in-1','1000000-in-1',
  'Rumble Station','NES-QJ','Taito TC190V','4-in-1',
  'SMB2j','51/Unknown','52/Unknown','53/Unknown','54/Unknown','55/Unknown',
  '56/Unknown','57/Unknown','58/Unknown','59/Unknown',
  '60/Unknown','61/Unknown','62/Unknown','63/Unknown','Tengen Rambo-1','Irem H3001',
  'Bandai 74161/32','Sunsoft Mapper 3','Sunsoft Mapper 4','Sunsoft Mapper 5',
  '74161/32','Camerica Mapper','Jaleco Early Mapper #0','Konami VRC3','74/Unknown','Jaleco/Konami VRC1',
  'Namco 109','Irem Early Mapper 0','Jaleco 74161/32','Nina-3 (AVE)',
  'Taito X-005','81/Unknown','Taito C075','Cony','84/Unknown','Knami VRC7',
  'Jaleco Early Mapper #2','Konami 74161/32','Namco 118','Sunsoft Early Mapper',
  'PC-JY-??','91/Unknown','Jaleco Early Mapper #1','Sunsoft 74161/32','Capcom 74161/32','Namco 106M',
  'Bandai 74161/32','Irem 74161/32','98/Unknown','VS Unisystem',
  'Nesticle MMC3','Jaleco 74161/32','102/Unknown','103/Unknown','104/Unknown','NES-EVENT',
  '106/Unknown','107/Unknown','108/Unknown','109/Unknown',
  '110/Unknown','111/Unknown','PC-Asder','PC-Sachen/Hacker','114/Unknown','115/Unknown',
  '116/Unknown','PC-Future','IQS MMC3','119/Unknown',
  '120/Unknown','121/Unknown','Sunsoft 74161/32','123/Unknown','124/Unknown','125/Unknown',
  '126/Unknown','127/Unknown','128/Unknown','129/Unknown',
  '130/Unknown','131/Unknown','132/Unknown','133/Unknown','134/Unknown','135/Unknown',
  '136/Unknown','137/Unknown','138/Unknown','139/Unknown',
  'Jaleco ??','141/Unknown','142/Unknown','143/Unknown','Death Race','145/Unknown',
  '146/Unknown','147/Unknown','148/Unknown','149/Unknown',
  '150/Unknown','VS Unisystem (Konami)','?? (Arkanoid 2)','Bandai ?? (Famicom Jump 2)','Namco ?? (Devil Man)','MMC1 w/o normal WRAM disable',
  '156/Unknown','157/Unknown','158/Unknown','159/Unknown',
  'PC-Aladdin','161/Unknown','162/Unknown','163/Unknown','164/Unknown','165/Unknown',
  '166/Unknown','167/Unknown','168/Unknown','169/Unknown',
  '170/Unknown','171/Unknown','172/Unknown','173/Unknown','174/Unknown','175/Unknown',
  '176/Unknown','177/Unknown','178/Unknown','179/Unknown',
  'Nichibutsu','181/Unknown','PC-SuperDonkeyKong','183/Unknown','Sunsoft 74161/32','CHR-ROM Disable Protect',
  '186/Unknown','Street Fighter Zero 2 97','Bandai Karaoke Studio','Street Fighter 2 Yoko',
  '190/Unknown','191/Unknown','192/Unknown','193/Unknown','194/Unknown','195/Unknown',
  '196/Unknown','197/Unknown','198/Unknown','199/Unknown',
  '200/Unknown','201/Unknown','202/Unknown','203/Unknown','204/Unknown','205/Unknown',
  '206/Unknown','207/Unknown','208/Unknown','209/Unknown',
  '210/Unknown','211/Unknown','212/Unknown','213/Unknown','214/Unknown','215/Unknown',
  '216/Unknown','217/Unknown','218/Unknown','219/Unknown',
  '220/Unknown','221/Unknown','222/Unknown','223/Unknown','224/Unknown','72-in-1',
  '76-in-1','1200-in-1','Action 52','31-in-1',
  '22-in-1','20-in-1','Quattro Games','42-in-1','234/Unknown','150-in-1',
  '236/Unknown','237/Unknown','238/Unknown','239/Unknown',
  'Gen Ke Le Zhuan','241/Unknown','Wai Xing Zhan Shi','PC-Sachen/Hacker','244/Unknown','245/Unknown',
  'Phone Serm Berm','247/Unknown','248/Unknown','Waixing ??',
  'Time Diver Avenger','251/Unknown','252/Unknown','253/Unknown','254/Unknown','110-in-1');

constructor TiNESImage.Create(pFilename : String);
begin
  inherited;
  ReadMapperData();

end;

constructor TiNESImage.Create(pFilename : String;pPaletteFile : String);
begin
  inherited Create(pFilename);
  ReadMapperData;
  if FileExists(pPaletteFile) then
    LoadPaletteFile(pPaletteFile)
  else
    LoadDefaultPalette;
end;

procedure TiNESImage.ReadMapperData;
begin

  // Now check if the first three bytes of the header is
  // 'NES'
  if chr(ROM[0]) + chr(ROM[1]) + chr(ROM[2]) = 'NES' then
    // The first three bytes equal NES.
    _ValidImage := True
  else
  begin
    // The first three bytes do not equal NES. This is
    // not a valid iNES image.
    _ValidImage := False;
    // Exit the subroutine. We shouldn't load in anything.
    Exit;
  end;

end;

procedure TiNESImage.DrawNESTile(pData : pointer;var pBitmap: TBitmap32; pX,
  pY : Integer;pPalette : pointer);
var
  x,y : Integer;
  Tile1 : Array [0..15] of Byte;
  Pal : Array [0..3] of Byte;
  temp : ^byte;
  Temp2, Temp3,Temp4 : Byte;
  size : Integer;
begin
  temp := pData;
  // Load the tile data into memory.
  For y := 0 To 15 do
  begin
    Tile1[y] := temp^;
    inc(temp);
  end;

  temp := pPalette;
  // Load the palette data into memory.
  for y := 0 to 3 do
  begin
    Pal[y] := temp^;
    inc(temp);
  end;
  size := 7;

  for y := 0 to size do
  begin
    for x := 0 to size do
    begin
      Temp2 := ((Tile1[y] and ($80 shr x)) shr (size -x));
      Temp3 := ((Tile1[y + 8] and ($80 shr x)) shr (size -x));

      Temp4 := (Temp3 shl 1) + Temp2;

      pBitmap.Pixel[pX + x,py+y] := _NESPal[Pal[Temp4]];

    end;
  end;

end;

procedure TiNESImage.LoadDefaultPalette;
begin
  _NESPal[0] := WinColor(StringToColor('$FF848078'));
  _NESPal[1] := WinColor(StringToColor('$FFFC0000'));
  _NESPal[2] := WinColor(StringToColor('$FFC40000'));
  _NESPal[3] := WinColor(StringToColor('$FFC42840'));
  _NESPal[4] := WinColor(StringToColor('$FF8C0094'));
  _NESPal[5] := WinColor(StringToColor('$FF2800AC'));
  _NESPal[6] := WinColor(StringToColor('$FF0010AC'));
  _NESPal[7] := WinColor(StringToColor('$FF00188C'));
  _NESPal[8] := WinColor(StringToColor('$FF003050'));
  _NESPal[9] := WinColor(StringToColor('$FF007800'));
  _NESPal[10] := WinColor(StringToColor('$FF006800'));
  _NESPal[11] := WinColor(StringToColor('$FF005800'));
  _NESPal[12] := WinColor(StringToColor('$FF584000'));
  _NESPal[13] := WinColor(StringToColor('$FF000000'));
  _NESPal[14] := WinColor(StringToColor('$FF000000'));
  _NESPal[15] := WinColor(StringToColor('$FF080000'));
  _NESPal[16] := WinColor(StringToColor('$FFC4C0BC'));
  _NESPal[17] := WinColor(StringToColor('$FFFC7800'));
  _NESPal[18] := WinColor(StringToColor('$FFFC8800'));
  _NESPal[19] := WinColor(StringToColor('$FFFC4868'));
  _NESPal[20] := WinColor(StringToColor('$FFD400DC'));
  _NESPal[21] := WinColor(StringToColor('$FF6000E4'));
  _NESPal[22] := WinColor(StringToColor('$FF0038FC'));
  _NESPal[23] := WinColor(StringToColor('$FF1860E4'));
  _NESPal[24] := WinColor(StringToColor('$FF0080AC'));
  _NESPal[25] := WinColor(StringToColor('$FF00B800'));
  _NESPal[26] := WinColor(StringToColor('$FF00A800'));
  _NESPal[27] := WinColor(StringToColor('$FF48A800'));
  _NESPal[28] := WinColor(StringToColor('$FF948800'));
  _NESPal[29] := WinColor(StringToColor('$FF2C2C2C'));
  _NESPal[30] := WinColor(StringToColor('$FF000000'));
  _NESPal[31] := WinColor(StringToColor('$FF000000'));
  _NESPal[32] := WinColor(StringToColor('$FFFCF8FC'));
  _NESPal[33] := WinColor(StringToColor('$FFFCC038'));
  _NESPal[34] := WinColor(StringToColor('$FFFC8868'));
  _NESPal[35] := WinColor(StringToColor('$FFFC789C'));
  _NESPal[36] := WinColor(StringToColor('$FFFC78FC'));
  _NESPal[37] := WinColor(StringToColor('$FF9C58FC'));
  _NESPal[38] := WinColor(StringToColor('$FF5878FC'));
  _NESPal[39] := WinColor(StringToColor('$FF48A0FC'));
  _NESPal[40] := WinColor(StringToColor('$FF00B8FC'));
  _NESPal[41] := WinColor(StringToColor('$FF18F8BC'));
  _NESPal[42] := WinColor(StringToColor('$FF58D858'));
  _NESPal[43] := WinColor(StringToColor('$FF9CF858'));
  _NESPal[44] := WinColor(StringToColor('$FFE4E800'));
  _NESPal[45] := WinColor(StringToColor('$FF606060'));
  _NESPal[46] := WinColor(StringToColor('$FF000000'));
  _NESPal[47] := WinColor(StringToColor('$FF000000'));
  _NESPal[48] := WinColor(StringToColor('$FFFCF8FC'));
  _NESPal[49] := WinColor(StringToColor('$FFFCE8A4'));
  _NESPal[50] := WinColor(StringToColor('$FFFCB8BC'));
  _NESPal[51] := WinColor(StringToColor('$FFFCB8DC'));
  _NESPal[52] := WinColor(StringToColor('$FFFCB8FC'));
  _NESPal[53] := WinColor(StringToColor('$FFE0C0F4'));
  _NESPal[54] := WinColor(StringToColor('$FFB4D0F4'));
  _NESPal[55] := WinColor(StringToColor('$FFB4E0FC'));
  _NESPal[56] := WinColor(StringToColor('$FF84D8FC'));
  _NESPal[57] := WinColor(StringToColor('$FF78F8DC'));
  _NESPal[58] := WinColor(StringToColor('$FF78F8B8'));
  _NESPal[59] := WinColor(StringToColor('$FFD8F0B0'));
  _NESPal[60] := WinColor(StringToColor('$FFFCF800'));
  _NESPal[61] := WinColor(StringToColor('$FFC0C0C8'));
  _NESPal[62] := WinColor(StringToColor('$FF000000'));
  _NESPal[63] := WinColor(StringToColor('$FF000000'));
end;

procedure TiNESImage.LoadPaletteFile(pFilename: String);
var
  i : Integer;
  FileStr : TFileStream;
  Red,Green, Blue : Byte;
begin
  FileStr := TFileStream.Create(pFilename,fmOpenRead);
  try
    for i := 0 to 63 do
    begin
      Red :=0;
      Green :=0;
      Blue :=0;

      FileStr.Read(Red,1);
      FileStr.Read(Green,1);
      FileStr.Read(Blue,1);
      _NESPal[i] := WinColor(StringToColor('$' + IntToHex(Blue,2) + IntToHex(Green,2) + IntToHex(Red,2)));
    end;
  finally
    FreeAndNil(FileStr);
  end;

end;

function TiNESImage.ReadNESPal(Index: Integer): TColor32;
begin
  result := self._NESPal[Index];
end;

procedure TiNESImage.WriteNESPal(Index: Integer; const Value: TColor32);
begin
  self._NESPal[Index] := Value;
end;


function TiNESImage.CRC32 : LongWord;
var
  lwCRC32 : Longword;
begin
  lwCRC32 := $FFFFFFFF; // match "normal" / PKZIP / etc. , step one: initialize with $FFFFFFFF (-1 signed)
  CalcCRC32(@RawROM[0], Length(RawROM), lwCRC32);
  lwCRC32 := not lwCRC32; // match "normal" / PKZIP / etc. , step two: invert the bits of the result

  result := lwCRC32;
end;

function TiNESImage.PointerToOffset(pOffset : Integer; pBank: Integer = $0; pMemAddress : Integer = $8000): Integer;
var
  TempPointer : Integer;
  TempBank : Integer;
begin
  // Now we work out where the pointer leads to.
  TempPointer := StrToInt('$' + IntToHex(ROM[pOffset + 1],2) + IntToHex(ROM[pOffset],2));

  if pBank = $0 then
    TempBank := pOffset
  else
    TempBank := pBank;

  // Now we subtract $8000 from the pointer, subtract $10
  // from the offset, divide the offset of the bank by $4000
  // then multiply it by $4000 and add $10. This should work out the
  // precise address of the pointer. Hopefully. :)
  result := (TempPointer - pMemAddress) + (((TempBank - $10) div $4000) * $4000 + $10);

end;

function TiNESImage.OffsetToPointer(pOffset, pMemAddress: Integer): Integer;
begin
  result := (pOffset - (((pOffset div $4000) * $4000) + $10)) + pMemAddress;
end;

function TiNESImage.OffsetToPointerS(pOffset, pMemAddress: Integer): String;
begin
  result := IntToHex(OffsetToPointer(pOffset,pMemAddress),4);
end;

procedure TiNESImage.StorePointerOffset(pOffsetPointer,pStoreOffset : Integer;pMemAddress : Integer = $8000);
var
  TempPointer : String;
begin

  TempPointer := self.OffsetToPointerS(pOffsetPointer,pMemAddress);

  self[pStoreOffset] := StrToInt('$' + TempPointer[3] + TempPointer[4]);
  self[pStoreOffset+1] := StrToInt('$' + TempPointer[1] + TempPointer[2]);

end;

function TiNESImage.MapperName: String;
begin
  result := MapperNames[self.MapperNumber];
end;

function TiNESImage.MapperNumber : Integer;
begin
  // Check for the DiskDude! tag in the iNES header.
  // If it is present, the iNES header is probably using the old specification
  // so we only need the first nibble to tell what mapper it is.
  if (RawROM[$7] = Ord('D')) and (RawROM[$8] = Ord('i')) and (RawROM[$9] = Ord('s')) and (RawROM[$A] = Ord('k'))
    and (RawROM[$B] = Ord('D')) and (RawROM[$C] = Ord('u')) and (RawROM[$D] = Ord('d')) and (RawROM[$E] = Ord('e'))
      and (RawROM[$F] = Ord('!')) then
  begin
    result := StrToInt('$' + inttohex(Ord(ROM[6]) shr 4,1));
  end
  else
  begin
    // Now load in the mapper number.
    // The mapper number is contained in two bytes of the iNES header
    // It's an 8-bit value but these two bytes need to have their bits
    // shifted right by 4 and then combined to yield the mapper number.
    result := StrToInt('$' + inttohex(Ord(ROM[7]) shr 4,1) + inttohex(Ord(ROM[6]) shr 4,1));
  end;
end;

function TiNESImage.PRGCount : Integer;
begin
  // Now load in the number of PRG banks
  // This is a simple one byte value.
  result := ROM[4];
end;

function TiNESImage.CHRCount : Integer;
begin
  // Now load in the number of CHR banks
  // This is a simple one byte value also.
  result := ROM[5];
end;

function TiNESImage.DiskDudePresent : Boolean;
begin
  // Check for the DiskDude! tag in the iNES header.
  // If it is present, the iNES header is probably using the old specification
  // so we only need the first nibble to tell what mapper it is.
  if (RawROM[$7] = Ord('D')) and (RawROM[$8] = Ord('i')) and (RawROM[$9] = Ord('s')) and (RawROM[$A] = Ord('k'))
    and (RawROM[$B] = Ord('D')) and (RawROM[$C] = Ord('u')) and (RawROM[$D] = Ord('d')) and (RawROM[$E] = Ord('e'))
      and (RawROM[$F] = Ord('!')) then
  begin
    result := True;
  end
  else
  begin
    result := False;
  end;
end;

function TiNESImage.ReadROM(Index: Integer): Byte;
begin
  result := self.RawROM[Index];
end;

procedure TiNESImage.DrawNESColours(var pBitmap : TBitmap32);
var
  i,x : Integer;

begin

  pBitmap.Width := 287;
  pBitmap.Height := 74;

  for i := 0 to 3 do
    for x :=0 to 15 do
      pBitmap.FillRect(x*18,i*18 + 1,(x*18)+17,i*18+18,_NESPal[(i*16) + x]);

  pBitmap.Line(0,0,0,74, clBlack32);


end;

procedure TiNESImage.BackupBank(pBankNumber : Integer);
begin
{var
  offset,i : Integer;
begin
  setlength(_BackupBank,$4000);
  offset := ((pBankNumber * $4000) + $10);
  for i := offset to offset + $3FFF do
    _BackupBank[i - offset] := RawROM[i];}
  BackupBank(pBankNumber,$4000);
end;

procedure TiNESImage.BackupBank(pBankNumber : Integer;pBankSize : Integer);
var
  offset,i : Integer;
begin
  setlength(_BackupBank,pBankSize);
  offset := ((pBankNumber * $4000) + $10);
  for i := offset to offset + high(_BackupBank) do
    _BackupBank[i - offset] := RawROM[i];

end;

procedure TiNESImage.RestoreBank(pBankNumber : Integer);
var
  offset,i : Integer;
begin
  offset := ((pBankNumber * $4000) + $10);
  for i := offset to offset + high(_BackupBank) do
    RawROM[i] := _BackupBank[i - offset];

end;


// Calculates the bank that a certain offset falls into.
function TiNESImage.CalculateBank(pOffset : integer) : Integer;
begin
  result := ((pOffset - $10) div $4000);
end;

procedure TiNESImage.ApplyTrainer;
var
  Temp : Array of Byte;
  i : Integer;
begin
  if GetTrainer = true then
    raise ENESTRAINEREXIST.Create('The ROM already has a trainer.');
  if ROMSize = $10 + (self.PRGCount * $4000) + (self.CHRCount * $1000) then
  begin
    // Set the trainer bit to be true.
    ROM[6] := ROM[6] +4;
    setlength(temp, (self.PRGCount * $4000) + (CHRCount * $1000));
    // Copy the PRG and CHR banks into the temporary array.
    for i := 0 to high(temp) do
      temp[i] := RawROM[i + $10];

    // increase the size of the ROM by $200 bytes.
    setlength(RawROM,length(RawROM) + $200);

    for i := $10 to $20F do
      RawROM[i] := $00;

    for i := $210 to high(RawROM) do
      RawROM[i] := Temp[i - $210];
  end
  else
  begin
    raise ENESTRAINEREXIST.Create('The ROM already has a trainer,' + chr(13) + chr(10) + 'that is not specified in the iNES header');
  end;

end;

procedure TiNESImage.RemoveTrainer;
begin

end;

function TiNESImage.GetTrainer : Boolean;
begin
  if ROM[6] and 4 = 4 then
  begin
    result := true;
  end
  else
  begin
    result := false;
  end;
end;

procedure TiNESImage.SetTrainer(pTrainer : Boolean);
begin
  if pTrainer = True then
    ApplyTrainer
  else
    RemoveTrainer;
end;

function TiNESImage.FindFreeSpace(pSize,pStartOffset,pEndOffset : Integer) : Integer;
var
  i : integer;
  freespaceaddr,freespacecount : Integer;
  freespacefound,sizefound : Boolean;
begin

  if (pStartOffset > GetROMSize()) or (pEndOffset > GetROMSize()) then
  begin
    result := -1;
    exit;
  end;

  if (pStartOffset = pEndOffset) or (pStartOffset > pEndOffset) then
  begin
    result := -1;
    exit;
  end;

  freespacecount := 0;
  freespaceaddr := 0;
  freespacefound := false;
  sizefound := false;
  for i := pStartOffset to pEndOffset do
  begin
    if (self.RawROM[i] = $FF) and (FreespaceFound = False) then
    begin

      freespaceaddr := i;
      freespacefound := True;
      freespacecount := 1;
    end
    else if (self.RawROM[i] = $FF) and (FreespaceFound = True) then
    begin
      inc(freespacecount);
      if freespacecount = pSize then
      begin
        sizefound := true;
        break;
      end;
    end
    else
    begin
      freespacefound := false;
      freespacecount := 0;
      freespaceaddr := -1;
    end;

  end;
  if sizefound = false then
    result := -1
  else
    result := freespaceaddr;

end;

end.

