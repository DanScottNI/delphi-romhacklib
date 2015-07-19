unit OffsetList;

interface

uses SysUtils,contnrs, inesimage, classes,cExceptions;

  type
    T1ByteProperty = Class(TiNESImageAccessor)
    protected
      _Name : String;
      _Offset : Integer;
      function GetValue : Byte;virtual;
      procedure SetValue(pByte : Byte);virtual;
      function GetSignedValue : ShortInt;virtual;
      procedure SetSignedValue(pByte : ShortInt);virtual;
    public
      constructor Create(pName : String;pOffset : Integer);
      destructor Destroy;override;
      property Name : String read _Name write _Name;
      property Offset : Integer read _Offset write _Offset;
      property Value : Byte read GetValue write SetValue;
      property SignedValue : ShortInt read GetSignedValue write SetSignedValue;
    end;

    T1BytePropertyList = class(TObjectList)
    private
      _HashTable : TStringList;
      function Get1BytePropertyItem(Index: Integer) : T1ByteProperty;
      procedure Set1BytePropertyItem(Index: Integer; const Value: T1ByteProperty);
      function Find(pName : String) : T1ByteProperty;
    public
      constructor Create(AOwnsObjects : Boolean);overload;
      destructor Destroy;override;
      function Add(AT1ByteProperty : T1ByteProperty) : Integer;
      property Items[Index: Integer] : T1ByteProperty read Get1BytePropertyItem write Set1BytePropertyItem;
      function Last : T1ByteProperty;


      function ValueExists(pName : String) : Boolean;
      property ItemsByName[index : String] : T1ByteProperty read Find;default;

    end;


implementation

{ T1ByteProperty }

constructor T1ByteProperty.Create(pName : String;pOffset : Integer);
begin
  _Name := pName;
  _Offset := pOffset;
end;

function T1ByteProperty.GetSignedValue : ShortInt;
var
  i : ShortInt;
begin
  if ROM[_Offset] and $80 = $80 then
    i := -128
  else
    i := 0;

  result := i + (ROM[_Offset] and $7F);
end;

procedure T1ByteProperty.SetSignedValue(pByte : ShortInt);
var
  temp : Byte;
begin
  if pByte < 0 then
    temp := $80
  else
    temp := $00;

  temp := temp + (pByte and $7F);
  ROM[_Offset] := temp;
end;

destructor T1ByteProperty.Destroy;
begin

end;

function T1ByteProperty.GetValue : Byte;
begin
  result := ROM[_Offset];
end;

procedure T1ByteProperty.SetValue(pByte : Byte);
begin
  ROM[_Offset] := pByte;
end;

{ T1BytePropertyList }

constructor T1BytePropertyList.Create(AOwnsObjects : Boolean);
begin
  inherited Create(AOwnsObjects);
  _HashTable := TStringList.Create;
end;

function T1BytePropertyList.Get1BytePropertyItem(Index: Integer) : T1ByteProperty;
begin
  Result := T1ByteProperty(inherited Items[Index]);
end;

procedure T1BytePropertyList.Set1BytePropertyItem(Index: Integer; const Value: T1ByteProperty);
begin
  inherited Items[Index] := Value;
end;

function T1BytePropertyList.Add(AT1ByteProperty : T1ByteProperty) : Integer;
//var
//  TempByteProp : T1ByteProperty;
begin
{  TempByteProp := T1ByteProperty.Create(_iNESImage,pName,pOffset);

  result := inherited add(TempByteProp);
  TempByteProp := nil;}
  _HashTable.Add(AT1ByteProperty._Name);
  result := inherited add(AT1ByteProperty);

end;

function T1BytePropertyList.ValueExists(pName : String) : Boolean;
begin
  if _HashTable.IndexOf(pName) = -1 then
    result := false
  else
    result := true;
end;

function T1BytePropertyList.Last : T1ByteProperty;
begin
  result := T1ByteProperty(inherited Last);
end;

function T1BytePropertyList.Find(pName : String) : T1ByteProperty;
var
  i : Integer;
begin
  i := _HashTable.IndexOf(pName);
  if (i > -1) and (i < self.Count) then
    result := Items[_HashTable.IndexOf(pName)]
  else
    result := nil;
end;

destructor T1BytePropertyList.Destroy;
begin
  FreeAndNil(_HashTable);
  inherited;
end;

end.
