unit DConfig;

interface

uses GR32,contnrs, SysUtils, INIFiles;

  type TConfigData = class
  private
    _Name : String;
    _Section : String;
  public
    constructor Create(pName, pSection : String);
    property Name : String read _Name;
    property Section : String read _Section;
  end;

  type TConfigByte = class(TConfigData)
  private
    _Value : Byte;
  public
    constructor Create(pName, pSection : String;pValue : Byte);
    property Value : Byte read _Value write _Value;
  end;

  type TConfigBool = class(TConfigData)
  private
    _Value : Boolean;
  public
    constructor Create(pName, pSection : String;pValue : Boolean);
    property Value : Boolean read _Value write _Value;
  end;

  type TConfigColor32 = class(TConfigData)
  private
    _Value : TColor32;
  public
    constructor Create(pName, pSection : String;pValue : TColor32);
    property Value : TColor32 read _Value write _Value;
  end;

  type TConfigString = class(TConfigData)
  private
    _Value : String;
  public
    constructor Create(pName, pSection : String;pValue : String);
    property Value : String read _Value write _Value;
  end;

  TConfigByteList = class(TObjectList)
  protected
    function GetItem(Index: Integer) : TConfigByte;
    procedure SetItem(Index: Integer; const Value: TConfigByte);
    function FindIndex(Name : String) : Integer;
  public
    function Add(AObject: TConfigByte) : Integer;
    property Items[Index: Integer] : TConfigByte read GetItem write SetItem;
    function Last : TConfigByte;
    property Find[Name : String] : Integer read FindIndex;default;
  end;

  TConfigBoolList = class(TObjectList)
  protected
    function GetItem(Index: Integer) : TConfigBool;
    procedure SetItem(Index: Integer; const Value: TConfigBool);
    function FindIndex(Name : String) : Integer;
  public
    function Add(AObject: TConfigBool) : Integer;
    property Items[Index: Integer] : TConfigBool read GetItem write SetItem;
    function Last : TConfigBool;
    property Find[Name : String] : Integer read FindIndex;default;
  end;

  TConfigColor32List = class(TObjectList)
  protected
    function GetItem(Index: Integer) : TConfigColor32;
    procedure SetItem(Index: Integer; const Value: TConfigColor32);
    function FindIndex(Name : String) : Integer;
  public
    function Add(AObject: TConfigColor32) : Integer;
    property Items[Index: Integer] : TConfigColor32 read GetItem write SetItem;
    function Last : TConfigColor32;
    property Find[Name : String] : Integer read FindIndex;default;
  end;

  TConfigStringList = class(TObjectList)
  protected
    function GetItem(Index: Integer) : TConfigString;
    procedure SetItem(Index: Integer; const Value: TConfigString);
    function FindIndex(Name : String) : Integer;
  public
    function Add(AObject: TConfigString) : Integer;
    property Items[Index: Integer] : TConfigString read GetItem write SetItem;
    function Last : TConfigString;
    property Find[Name : String] : Integer read FindIndex;default;
  end;


  type TConfiguration = class
  private
    _Filename : String;
  public
    BoolValues : TConfigBoolList;
    ByteValues : TConfigByteList;
    Color32Values : TConfigColor32List;
    StringValues : TConfigStringList;
    procedure Save;
    procedure Load;
    constructor Create(pFilename : String);
    destructor Destroy;override;
  end;


implementation

{ TConfiguration }

procedure TConfiguration.Save;
var
  INI : TMemINIFile;
  i : Integer;
begin
  INI := TMemINIFile.Create(_Filename);
  try
    if (Assigned(BoolValues)) and (BoolValues.Count > 0) then
    begin
      for i := 0 to BoolValues.Count -1 do
      begin
        INI.WriteBool(BoolValues.Items[i].Section,BoolValues.Items[i].Name,BoolValues.Items[i].Value);
      end;
    end;

    if (Assigned(ByteValues)) and (ByteValues.Count > 0) then
    begin
      for i := 0 to ByteValues.Count -1 do
      begin
        INI.WriteInteger(ByteValues.Items[i].Section,ByteValues.Items[i].Name,ByteValues.Items[i].Value);
      end;
    end;

    if (Assigned(Color32Values)) and (Color32Values.Count > 0) then
    begin
      for i := 0 to Color32Values.Count -1 do
      begin
        INI.WriteInteger(Color32Values.Items[i].Section,Color32Values.Items[i].Name + 'R',(Color32Values.Items[i].Value and $00FF0000) shr 16);
        INI.WriteInteger(Color32Values.Items[i].Section,Color32Values.Items[i].Name + 'G',(Color32Values.Items[i].Value and $0000FF00) shr 8);
        INI.WriteInteger(Color32Values.Items[i].Section,Color32Values.Items[i].Name + 'B',Color32Values.Items[i].Value and $0000FF);
        INI.WriteInteger(Color32Values.Items[i].Section,Color32Values.Items[i].Name + 'A',Color32Values.Items[i].Value shr 24);
      end;
    end;

    if (Assigned(StringValues)) and (StringValues.Count > 0) then
    begin
      for i := 0 to StringValues.Count -1 do
      begin
        INI.WriteString(StringValues.Items[i].Section,StringValues.Items[i].Name,StringValues.Items[i].Value);
      end;
    end;

    INI.UpdateFile;

  finally
    FreeAndNil(INI);
  end;
end;

procedure TConfiguration.Load;
var
  INI : TMemINIFile;
  i : Integer;
begin
  INI := TMemINIFile.Create(_Filename);
  try
    if (Assigned(BoolValues)) and (BoolValues.Count > 0) then
    begin
      for i := 0 to BoolValues.Count -1 do
      begin
        BoolValues.Items[i].Value := INI.ReadBool(BoolValues.Items[i].Section,BoolValues.Items[i].Name,BoolValues.Items[i].Value);
      end;
    end;

    if (Assigned(ByteValues)) and (ByteValues.Count > 0) then
    begin
      for i := 0 to ByteValues.Count -1 do
      begin
        ByteValues.Items[i].Value := INI.ReadInteger(ByteValues.Items[i].Section,ByteValues.Items[i].Name,ByteValues.Items[i].Value);
      end;
    end;

    if (Assigned(Color32Values)) and (Color32Values.Count > 0) then
    begin
      for i := 0 to Color32Values.Count -1 do
      begin
        Color32Values.Items[i].Value := (INI.ReadInteger(Color32Values.Items[i].Section,Color32Values.Items[i].Name + 'A',$FF) shl 24) +
          (INI.ReadInteger(Color32Values.Items[i].Section,Color32Values.Items[i].Name + 'R',$00) shl 16) +
            (INI.ReadInteger(Color32Values.Items[i].Section,Color32Values.Items[i].Name + 'G',$00) shl 8) +
              (INI.ReadInteger(Color32Values.Items[i].Section,Color32Values.Items[i].Name + 'B',$00));
      end;
    end;

    if (Assigned(StringValues)) and (StringValues.Count > 0) then
    begin
      for i := 0 to StringValues.Count -1 do
      begin
        StringValues.Items[i].Value := INI.ReadString(StringValues.Items[i].Section,StringValues.Items[i].Name,StringValues.Items[i].Value);
      end;
    end;



  finally
    FreeAndNil(INI);
  end;
end;

constructor TConfiguration.Create(pFilename : String);
begin
  inherited Create();
  _Filename := pFilename;

  BoolValues := TConfigBoolList.Create(true);
  ByteValues := TConfigByteList.Create(true);
  Color32Values := TConfigColor32List.Create(true);
  StringValues := TConfigStringList.Create(true);

end;

destructor TConfiguration.Destroy;
begin
  FreeAndNil(BoolValues);
  FreeAndNil(ByteValues);
  FreeAndNil(Color32Values);
  FreeAndNil(StringValues);
end;

{ TConfigData }

constructor TConfigData.Create(pName, pSection : String);
begin
  _Name := pName;
  _Section := pSection;
end;

{ TConfigByte }

constructor TConfigByte.Create(pName, pSection : String;pValue : Byte);
begin
  inherited Create(pName,pSection);
  _Value := pValue;
end;

{ TConfigBool }

constructor TConfigBool.Create(pName, pSection : String;pValue : Boolean);
begin
  inherited Create(pName,pSection);
  _Value := pValue;
end;

{ TConfigColor32 }

constructor TConfigColor32.Create(pName, pSection : String;pValue : TColor32);
begin
  inherited Create(pName,pSection);
  _Value := pValue;
end;

{ TConfigString }

constructor TConfigString.Create(pName, pSection : String;pValue : String);
begin
  inherited Create(pName,pSection);
  _Value := pValue;
end;


{ TConfigByteList }

function TConfigByteList.FindIndex(Name : String) : Integer;
var
  i : Integer;
begin
  result := -1;
  if Count >0 then
  begin
    for i := 0 to Count -1 do
    begin
      if LowerCase(Items[i].Name) = LowerCase(Name) then
        result := i; 
    end;
  end;
end;

function TConfigByteList.Add(AObject: TConfigByte): Integer;
begin
  Result := inherited Add(AObject);
end;

function TConfigByteList.GetItem(Index: Integer): TConfigByte;
begin
  Result := TConfigByte(inherited Items[Index]);
end;

procedure TConfigByteList.SetItem(Index: Integer; const Value: TConfigByte);
begin
  inherited Items[Index] := Value;
end;

function TConfigByteList.Last : TConfigByte;
begin
  result := TConfigByte(inherited Last);
end;

{ TConfigBoolList }

function TConfigBoolList.FindIndex(Name : String) : Integer;
var
  i : Integer;
begin
  result := -1;
  if Count >0 then
  begin
    for i := 0 to Count -1 do
    begin
      if LowerCase(Items[i].Name) = LowerCase(Name) then
        result := i; 
    end;
  end;

end;

function TConfigBoolList.Add(AObject: TConfigBool): Integer;
begin
  Result := inherited Add(AObject);
end;

function TConfigBoolList.GetItem(Index: Integer): TConfigBool;
begin
  Result := TConfigBool(inherited Items[Index]);
end;

procedure TConfigBoolList.SetItem(Index: Integer; const Value: TConfigBool);
begin
  inherited Items[Index] := Value;
end;

function TConfigBoolList.Last : TConfigBool;
begin
  result := TConfigBool(inherited Last);
end;

{ TConfigColor32List }

function TConfigColor32List.FindIndex(Name : String) : Integer;
var
  i : Integer;
begin
  result := -1;
  if Count >0 then
  begin
    for i := 0 to Count -1 do
    begin
      if LowerCase(Items[i].Name) = LowerCase(Name) then
        result := i; 
    end;
  end;

end;

function TConfigColor32List.Add(AObject: TConfigColor32): Integer;
begin
  Result := inherited Add(AObject);
end;

function TConfigColor32List.GetItem(Index: Integer): TConfigColor32;
begin
  Result := TConfigColor32(inherited Items[Index]);
end;

procedure TConfigColor32List.SetItem(Index: Integer; const Value: TConfigColor32);
begin
  inherited Items[Index] := Value;
end;

function TConfigColor32List.Last : TConfigColor32;
begin
  result := TConfigColor32(inherited Last);
end;

{ TConfigStringList }

function TConfigStringList.FindIndex(Name : String) : Integer;
var
  i : Integer;
begin
  result := -1;
  if Count >0 then
  begin
    for i := 0 to Count -1 do
    begin
      if LowerCase(Items[i].Name) = LowerCase(Name) then
        result := i; 
    end;
  end;

end;

function TConfigStringList.Add(AObject: TConfigString): Integer;
begin
  Result := inherited Add(AObject);
end;

function TConfigStringList.GetItem(Index: Integer): TConfigString;
begin
  Result := TConfigString(inherited Items[Index]);
end;

procedure TConfigStringList.SetItem(Index: Integer; const Value: TConfigString);
begin
  inherited Items[Index] := Value;
end;

function TConfigStringList.Last : TConfigString;
begin
  result := TConfigString(inherited Last);
end;


end.
