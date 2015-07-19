unit ROMObj;

interface

uses iNESImage;

{type
  TNESROMObject = class
  protected
    ROM : TiNESImage;
  public
    constructor Create;
    //property ROM : TiNESImage read _ROM;
  end;

  procedure CreateROM(pFilename :String);}

implementation

{var  _ROM : TiNESIMage;

constructor TNESROMObject.Create();
begin
  inherited;
  ROM := _ROM;
end;

procedure CreateROM(pFilename :String);
begin
  _ROM := TiNESImage.Create(pFilename);
end;}

end.
