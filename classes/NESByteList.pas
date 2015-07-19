// Class that implements a list of offsets, for NES ROMs.
unit NESByteList;

interface

uses SysUtils,classes, iNESImage;

type
  TNESByteList = class
  private
    _Strings : TStringList;
    _iNESImage : TiNESImage;
  public
    constructor Create(var piNESROMImage: TiNESImage);
    destructor Destroy; override;
    procedure Add(pName : String; pOffset : Integer);
    function GetOffset(pName : String) : Integer;
    procedure SetOffset(pName : String; pValue : Integer);
    function GetByte(pName : String) : Byte;
    procedure SetByte(pName : String;pValue : Byte);
    function GetShort(pName : String) : Word;
    procedure SetShort(pName : String;pValue : Word);
  end;

implementation

procedure TNESByteList.Add(pName: String; pOffset: Integer);
begin
  _Strings.AddObject( pName, TObject(pOffset));
end;

constructor TNESByteList.Create(var piNESROMImage : TiNESImage);
begin
  _Strings := TStringList.Create();
  _iNESImage := piNESROMImage;
end;

destructor TNESByteList.Destroy();
begin
  FreeAndNil(_Strings);
  _iNESImage := nil;
end;

function TNESByteList.GetByte(pName: String): Byte;
var
  Offset : Integer;
begin
  Offset := Integer(_Strings.Objects[pName]);
  GetByte := _iNESImage[Offset];
end;

function TNESByteList.GetOffset(pName: String): Integer;
begin

end;

function TNESByteList.GetShort(pName: String): Word;
begin

end;

procedure TNESByteList.SetByte(pName: String; pValue: Byte);
var
  Offset : Integer;
begin
  Offset := Integer(_Strings.Objects[pName]);
  _iNESImage[Offset] := pValue;
end;

procedure TNESByteList.SetOffset(pName: String; pValue: Integer);
begin
  _Strings.Values[pName] := IntToStr(pValue);
end;

procedure TNESByteList.SetShort(pName: String; pValue: Word);
begin

end;

end.
