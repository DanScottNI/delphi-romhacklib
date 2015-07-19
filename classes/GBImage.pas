{ GBImage v0.1 }

unit GBImage;

interface

uses ROMImage, classes, SysUtils;

type
  TGBImage = class(TROMImage)
  private
    function GetGameName : String;
    function GetCountry : String;
    function GetSuperGB : Boolean;
    function GetColourGB : Boolean;
    function GetCartridgeType : String;
    function GetManufacturerName : String;
    function GetROMSize : Integer;
    function GetRAMSize : Integer;
    function GetHeaderChecksum : Byte;
    function GetCalcHeaderChecksum : Byte;
  public
    property GameName : String read GetGameName;
    property Country : String read GetCountry;
    property SuperGameboy : Boolean read GetSuperGB;
    property GameboyColor : Boolean read GetColourGB;
    property CartridgeType : String read GetCartridgeType;
    property ManufacturerName : String read GetManufacturerName;
    property ROMSize : Integer read GetROMSize;
    property RAMSize : Integer read GetRAMSize;
    constructor Create(pFilename : String);
    property HeaderChecksum : Byte read GetHeaderChecksum;
    property CalculatedHeaderChecksum : Byte read GetCalcHeaderChecksum;
  end;

implementation

var
OldManufacturer : Array [0..255] of String = ('None','Nintendo','02\Unknown',
'03\Unknown','04\Unknown','05\Unknown','06\Unknown','07\Unknown','Capcom',
'Hot-B','Jaleco','Coconuts','Elite Systems','0D\Unknown','0E\Unknown',
'0F\Unknown','10\Unknown','11\Unknown','12\Unknown','Electronic Arts',
'14\Unknown','15\Unknown','16\Unknown','17\Unknown','Hudsonsoft',
'ITC Entertainment','Yanoman','1B\Unknown','1C\Unknown','Clary','1E\Unknown',
'Virgin','20\Unknown','21\Unknown','22\Unknown','23\Unknown','PCM Complete',
'San-X','26\Unknown','27\Unknown','Kotobuki Systems','Seta','2A\Unknown',
'2B\Unknown','2C\Unknown','2D\Unknown','2E\Unknown','2F\Unknown','Infogrames',
'Nintendo','Bandai','33\Unknown','Konami','Hector','36\Unknown','37\Unknown',
'Capcom','Banpresto','3A\Unknown','3B\Unknown','Entertainment i','3D\Unknown',
'Gremlin','3F\Unknown','40\Unknown','Ubi-Soft','Atlus','43\Unknown','Malibu',
'45\Unknown','Angel','Spectrum Holobyte','48\Unknown','Irem','Virgin','4B\Unknown',
'4C\Unknown','Malibu','4E\Unknown','US Gold','Absolute','Acclaim','Activision',
'American Sammy','Gametek','Park Place','LJN','Matchbox','58\Unknown','Milton Bradley',
'Mindscape','ROMStar','Naxat Soft','Tradewest','5E\Unknown','5F\Unknown',
'Titus','Virgin','62\Unknown','63\Unknown','64\Unknown','65\Unknown','66\Unknown',
'Ocean','68\Unknown','Electronic Arts','6A\Unknown','6B\Unknown','6C\Unknown',
'6D\Unknown','Elite Systems','Electro Brain','Infogrames','Interplay',
'Broderbund','Sculptered Soft','74\Unknown','The Sales Curve','76\Unknown',
'77\Unknown','T*HQ','Accolade','Triffix Entertainment','7B\Unknown','Microprose',
'7D\Unknown','7E\Unknown','Kemco','Misawa Entertainment','81\Unknown',
'82\Unknown','Lozc','84\Unknown','85\Unknown','Tokuma Shoten i','87\Unknown',
'88\Unknown','89\Unknown','8A\Unknown','Bullet-Proof Software','Vic Tokai',
'8D\Unknown','Ape','i''max','90\Unknown','Chun Soft','Video System','Tsuburava',
'94\Unknown','Varie','Yonezawa/s''pal','Kaneko','98\Unknown','Arc','Nihon Bussan',
'Tecmo','Imagineer','Banpresto','9E\Unknown','Nova','A0\Unknown','Hori Electric',
'Bandai','A3\Unknown','Konami','A5\Unknown','Kawada','Takara','A8\Unknown',
'Technos Japan','Broderbund','AB\Unknown','TOEI Animation','Toho','AE\Unknown',
'Namco','Acclaim','ASCII/Nexoft','Bandai','B3\Unknown','Enix','B5\Unknown',
'HAL','SNK','B8\Unknown','Pony Canyon','Culture Brain','Sunsoft','BC\Unknown',
'Sony Imagesoft','BE\Unknown','Sammy','Taito','C1\Unknown','Kemco','SquareSoft',
'Tokuma Shoten i','Data East','Tonkin House','C7\Unknown','Koei','UFL','Ultra',
'Vap','Use','Meldac','Pony Canyon','Angel','Taito','Sofel','Quest','Sigma Enterprises',
'Ask Kodansha','D5\Unknown','Naxat Soft','Copya Systems','D8\Unknown','Banpresto',
'Tomy','LJN','DC\Unknown','NCS','Human','Altron','Jaleco','Towachiki','Uutaka',
'Varie','E4\Unknown','Epoch','E6\Unknown','Athena','Asmik','Natsume','King Records',
'Atlus','Epic/Sony Records','ED\Unknown','IGS','EF\Unknown','A Wave','F1\Unknown',
'F2\Unknown','Extreme Entertainment','F4\Unknown','F5\Unknown','F6\Unknown',
'F7\Unknown','F8\Unknown','F9\Unknown','FA\Unknown','FB\Unknown','FC\Unknown',
'FD\Unknown','FE\Unknown','LJN');

CartridgeTypes : Array [0..255] of String = ('ROM','ROM + MBC1','ROM + MBC1 + RAM',
'ROM + MBC1 + RAM + BATTERY','04\Unknown','ROM + MBC2','ROM + MBC2 + BATTERY',
'07\Unknown','ROM + RAM','ROM + RAM + BATTERY','0A\Unknown','ROM + MMMO1',
'ROM + MMMO1 + SRAM','ROM + MMMO1 + SRAM + BATTERY','0E\Unknown',
'ROM + MBC3 + TIMER + BATTERY','ROM + MBC3 + TIMER + RAM + BATTERY','ROM + MBC3',
'ROM + MBC3 + RAM','ROM + MBC3 + RAM + BATTERY','14\Unknown','15\Unknown',
'16\Unknown','17\Unknown','18\Unknown','ROM + MBC5','ROM + MBC5 + RAM',
'ROM + MBC5 + RAM + BATTERY','ROM + MBC5 + RUMBLE','ROM + MBC5 + RUMBLE + SRAM',
'ROM + MBC5 + RUMBLE + SRAM + BATTERY','POCKET CAMERA','20\Unknown','21\Unknown',
'22\Unknown','23\Unknown','24\Unknown','25\Unknown','26\Unknown','27\Unknown',
'28\Unknown','29\Unknown','2A\Unknown','2B\Unknown','2C\Unknown','2D\Unknown',
'2E\Unknown','2F\Unknown','30\Unknown','31\Unknown','32\Unknown','33\Unknown',
'34\Unknown','35\Unknown','36\Unknown','37\Unknown','38\Unknown','39\Unknown',
'3A\Unknown','3B\Unknown','3C\Unknown','3D\Unknown','3E\Unknown','3F\Unknown',
'40\Unknown','41\Unknown','42\Unknown','43\Unknown','44\Unknown','45\Unknown',
'46\Unknown','47\Unknown','48\Unknown','49\Unknown','4A\Unknown','4B\Unknown',
'4C\Unknown','4D\Unknown','4E\Unknown','4F\Unknown','50\Unknown','51\Unknown',
'52\Unknown','53\Unknown','54\Unknown','55\Unknown','56\Unknown','57\Unknown',
'58\Unknown','59\Unknown','5A\Unknown','5B\Unknown','5C\Unknown','5D\Unknown',
'5E\Unknown','5F\Unknown','60\Unknown','61\Unknown','62\Unknown','63\Unknown',
'64\Unknown','65\Unknown','66\Unknown','67\Unknown','68\Unknown','69\Unknown',
'6A\Unknown','6B\Unknown','6C\Unknown','6D\Unknown','6E\Unknown','6F\Unknown',
'70\Unknown','71\Unknown','72\Unknown','73\Unknown','74\Unknown','75\Unknown',
'76\Unknown','77\Unknown','78\Unknown','79\Unknown','7A\Unknown','7B\Unknown',
'7C\Unknown','7D\Unknown','7E\Unknown','7F\Unknown','80\Unknown','81\Unknown',
'82\Unknown','83\Unknown','84\Unknown','85\Unknown','86\Unknown','87\Unknown',
'88\Unknown','89\Unknown','8A\Unknown','8B\Unknown','8C\Unknown','8D\Unknown',
'8E\Unknown','8F\Unknown','90\Unknown','91\Unknown','92\Unknown','93\Unknown',
'94\Unknown','95\Unknown','96\Unknown','97\Unknown','98\Unknown','99\Unknown',
'9A\Unknown','9B\Unknown','9C\Unknown','9D\Unknown','9E\Unknown','9F\Unknown',
'A0\Unknown','A1\Unknown','A2\Unknown','A3\Unknown','A4\Unknown','A5\Unknown',
'A6\Unknown','A7\Unknown','A8\Unknown','A9\Unknown','AA\Unknown','AB\Unknown',
'AC\Unknown','AD\Unknown','AE\Unknown','AF\Unknown','B0\Unknown','B1\Unknown',
'B2\Unknown','B3\Unknown','B4\Unknown','B5\Unknown','B6\Unknown','B7\Unknown',
'B8\Unknown','B9\Unknown','BA\Unknown','BB\Unknown','BC\Unknown','BD\Unknown',
'BE\Unknown','BF\Unknown','C0\Unknown','C1\Unknown','C2\Unknown','C3\Unknown',
'C4\Unknown','C5\Unknown','C6\Unknown','C7\Unknown','C8\Unknown','C9\Unknown',
'CA\Unknown','CB\Unknown','CC\Unknown','CD\Unknown','CE\Unknown','CF\Unknown',
'D0\Unknown','D1\Unknown','D2\Unknown','D3\Unknown','D4\Unknown','D5\Unknown',
'D6\Unknown','D7\Unknown','D8\Unknown','D9\Unknown','DA\Unknown','DB\Unknown',
'DC\Unknown','DD\Unknown','DE\Unknown','DF\Unknown','E0\Unknown','E1\Unknown',
'E2\Unknown','E3\Unknown','E4\Unknown','E5\Unknown','E6\Unknown','E7\Unknown',
'E8\Unknown','E9\Unknown','EA\Unknown','EB\Unknown','EC\Unknown','ED\Unknown',
'EE\Unknown','EF\Unknown','F0\Unknown','F1\Unknown','F2\Unknown','F3\Unknown',
'F4\Unknown','F5\Unknown','F6\Unknown','F7\Unknown','F8\Unknown','F9\Unknown',
'FA\Unknown','FB\Unknown','FC\Unknown','BANDAI TAMA5','HUDSON HuC 3','HUDSON HuC 1');

NewManufacturer : Array [0..255] of String = ('None','Nintendo','02\Unknown',
'03\Unknown','04\Unknown','05\Unknown','06\Unknown','07\Unknown','Capcom',
'09\Unknown','0A\Unknown','0B\Unknown','0C\Unknown','0D\Unknown','0E\Unknown',
'0F\Unknown','10\Unknown','11\Unknown','12\Unknown','Electronic Arts',
'14\Unknown','15\Unknown','16\Unknown','17\Unknown','Hudsonsoft','b-ai',
'1A\Unknown','1B\Unknown','1C\Unknown','1D\Unknown','1E\Unknown','1F\Unknown',
'KSS','21\Unknown','POW','23\Unknown','PCM Complete','San-X','26\Unknown',
'27\Unknown','Kemco Japan','Seta','2A\Unknown','2B\Unknown','2C\Unknown',
'2D\Unknown','2E\Unknown','2F\Unknown','Viacom','Nintendo','Bandai','Ocean/Acclaim',
'Konami','Hector','36\Unknown','Taito','Hudson','Banpresto','3A\Unknown',
'3B\Unknown','3C\Unknown','3D\Unknown','3E\Unknown','3F\Unknown','40\Unknown',
'UBI Soft','Atlus','43\Unknown','Malibu','45\Unknown','Angel','Bullet Proof',
'48\Unknown','Irem','4A\Unknown','4B\Unknown','4C\Unknown','4D\Unknown',
'4E\Unknown','4F\Unknown','Absolute','Acclaim','Activision','American Sammy',
'Konami','Hi Tech Entertainment','LJN','Matchbox','Mattel','Milton Bradley',
'5A\Unknown','5B\Unknown','5C\Unknown','5D\Unknown','5E\Unknown','5F\Unknown',
'Titus','Virgin','62\Unknown','63\Unknown','Lucasarts','65\Unknown','66\Unknown',
'Ocean','68\Unknown','Electronic Arts','6A\Unknown','6B\Unknown','6C\Unknown',
'6D\Unknown','6E\Unknown','6F\Unknown','Infogrames','Interplay','Broderbund',
'Sculptured','74\Unknown','SCI','76\Unknown','77\Unknown','T*HQ','Accolade',
'7A\Unknown','7B\Unknown','7C\Unknown','7D\Unknown','7E\Unknown','7F\Unknown',
'Misawa','81\Unknown','82\Unknown','Lozc','84\Unknown','85\Unknown','Tokuma Shoten i',
'Tsukuda Ori','88\Unknown','89\Unknown','8A\Unknown','8B\Unknown','8C\Unknown',
'8D\Unknown','8E\Unknown','8F\Unknown','90\Unknown','Chun Soft','Video System',
'Ocean/Acclaim','94\Unknown','Varie','Yonezawa/s''Pal','Kaneko','98\Unknown',
'Pack In Soft','9A\Unknown','9B\Unknown','9C\Unknown','9D\Unknown','9E\Unknown',
'9F\Unknown','A0\Unknown','A1\Unknown','A2\Unknown','A3\Unknown','A4\Unknown',
'A5\Unknown','A6\Unknown','A7\Unknown','A8\Unknown','A9\Unknown','AA\Unknown',
'AB\Unknown','AC\Unknown','AD\Unknown','AE\Unknown','AF\Unknown','B0\Unknown',
'B1\Unknown','B2\Unknown','B3\Unknown','B4\Unknown','B5\Unknown','B6\Unknown',
'B7\Unknown','B8\Unknown','B9\Unknown','BA\Unknown','BB\Unknown','BC\Unknown',
'BD\Unknown','BE\Unknown','BF\Unknown','C0\Unknown','C1\Unknown','C2\Unknown',
'C3\Unknown','C4\Unknown','C5\Unknown','C6\Unknown','C7\Unknown','C8\Unknown',
'C9\Unknown','CA\Unknown','CB\Unknown','CC\Unknown','CD\Unknown','CE\Unknown',
'CF\Unknown','D0\Unknown','D1\Unknown','D2\Unknown','D3\Unknown','D4\Unknown',
'D5\Unknown','D6\Unknown','D7\Unknown','D8\Unknown','D9\Unknown','DA\Unknown',
'DB\Unknown','DC\Unknown','DD\Unknown','DE\Unknown','DF\Unknown','E0\Unknown',
'E1\Unknown','E2\Unknown','E3\Unknown','E4\Unknown','E5\Unknown','E6\Unknown',
'E7\Unknown','E8\Unknown','E9\Unknown','EA\Unknown','EB\Unknown','EC\Unknown',
'ED\Unknown','EE\Unknown','EF\Unknown','F0\Unknown','F1\Unknown','F2\Unknown',
'F3\Unknown','F4\Unknown','F5\Unknown','F6\Unknown','F7\Unknown','F8\Unknown',
'F9\Unknown','FA\Unknown','FB\Unknown','FC\Unknown','FD\Unknown','FE\Unknown',
'FF\Unknown');

ROMSizes : Array [0..255] Of Integer = (
$8000,$10000,$20000,$40000,$80000,$100000,$200000,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$120000,$140000,$180000,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);

RAMSizes : Array [0..4] Of Integer = (
$00,$800,$2000,$8000,$20000);

function TGBImage.GetGameName : String;
var
  i : Integer;
  res : String;

begin


  // This function returns the Game Name from the
  // in-built header.
  i := $134;

  while ((i < $144) and (RawROM[i] <> $00)) do
  begin
    res := res + Chr(RawROM[i]);
    inc(i);
  end;
  result := res;
end;

function TGBImage.GetCountry : String;
begin
  if RawROM[$014A] = 0 then
    result := 'Japan'
  else
    result := 'USA + Europe';
end;

function TGBImage.GetSuperGB : Boolean;
begin

  if RawROM[$014B] <> $33 then
  begin
    result := false;
    exit;
  end;

  if RawROM[$146] = 3 then
    result := true
  else
    result := false;
end;

function TGBImage.GetColourGB : Boolean;
begin
  if RawROM[$143] and $80 = $80 then
    result := true
  else
    result := false;
end;


constructor TGBImage.Create(pFilename : String);
begin
  inherited;
end;

function TGBImage.GetCartridgeType : String;
begin
  result := CartridgeTypes[RawROM[$147]];
end;

function TGBImage.GetManufacturerName : String;
begin
  // If $014B is $33 then we use the new manufacturer's
  // list
  if RawROM[$014B] = $33 then
  begin
    result := NewManufacturer[StrToInt('$' + Chr(RawROM[$0144]) + Chr(RawROM[$0145]))];
  end
  else
  begin
    result := OldManufacturer[RawROM[$014b]];
  end;
end;

function TGBImage.GetROMSize : Integer;
begin
  result := ROMSizes[RawROM[$0148]];
end;

function TGBImage.GetRAMSize : Integer;
begin
  result := RAMSizes[RawROM[$0149]];
end;

function TGBImage.GetHeaderChecksum : Byte;
begin
  result := RawROM[$014D];
end;

function TGBImage.GetCalcHeaderChecksum : Byte;
var
  Temp : Byte;
  i : Integer;
begin
  Temp := 0;
  for i := $134 to $14D - 1 do
    Temp:= Temp + RawROM[i];

  result := $E7 - Temp;
end;

end.
