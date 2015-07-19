// This unit was written because I was tired of manipulating
// byte arrays. It was far too much work, so I decided to
// make the hard (well, not hard, merely annoying) parts
// into a class.
unit ByteList;

interface

type
  TByteList = class(TObject)
  private
    _bytearray : Array Of Byte;
    function GetArraySize : Integer;
    procedure SetArraySize(pSize : Integer);
    function GetItem(pItem : Integer) : Byte;
    procedure SetItem(pItem : Integer;pNewItem : Byte);
  public
    constructor Create(pSize : Integer);
    destructor Destroy;override;
    property Count : Integer read GetArraySize write SetArraySize;
    procedure Add(pByte : Byte;pPosition : Integer = -1);
    procedure Delete(pPosition : Integer);
    property Items [index : Integer] : byte read GetItem write SetItem;default;
  end;

implementation

constructor TByteList.Create(pSize : Integer);
begin
  setLength(_bytearray,pSize);
end;

function TByteList.GetArraySize : Integer;
begin
  result := Length(_bytearray);
end;

procedure TByteList.SetArraySize(pSize : Integer);
begin
  setLength(_bytearray,pSize);
end;

destructor TByteList.Destroy;
begin
  setLength(_bytearray,0);
  inherited;
end;

procedure TByteList.Add(pByte : Byte;pPosition : Integer = -1);
var
  i : Integer;
begin
  // Increase the size of the array by 1.
  setLength(_bytearray,length(_bytearray) + 1);

  // Now, if the position variable is not -1
  // Shift all the bytes in the array up, and
  // put the new byte in the new position
  if pPosition > -1 then
  begin
    for i := length(_bytearray) - 2 downto pPosition do
    begin
      _bytearray[i + 1] := _bytearray[i];
    end;
    _bytearray[pPosition] := pByte;
  end
  else
    _bytearray[length(_bytearray) - 1] := pByte;

end;

procedure TByteList.Delete(pPosition : Integer);
var
  i : Integer;
begin
  for i := pPosition to length(_bytearray) - 2 do
  begin
    _bytearray[i] := _bytearray[i + 1];
  end;
  setlength(_bytearray,length(_bytearray) - 1);
end;

function TByteList.GetItem(pItem : Integer) : Byte;
begin
  if pItem > Length(_bytearray) - 1 then
  begin
    result := 0;
    exit;
  end;
  result := _bytearray[pItem];
end;

procedure TByteList.SetItem(pItem : Integer;pNewItem : Byte);
begin
  if pItem > Length(_bytearray) - 1 then
  begin
    exit;
  end;
  _bytearray[pItem] := pNewItem;
end;

end.
