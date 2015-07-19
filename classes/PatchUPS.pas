unit PatchUPS;

interface
type
  TUPSPatch = class
  private

  public
    constructor Create();
    procedure Patch(PatchFilename : String; ROMFilename : String);
    procedure CreatePatch(OriginalROMFilename, ModifiedROMFilename,
      OutputPatchFilename: String);
  end;
implementation

{ TUPSPatch }

constructor TUPSPatch.Create();
begin

end;

procedure TUPSPatch.Patch(PatchFilename, ROMFilename: String);
begin
end;

procedure TUPSPatch.CreatePatch(OriginalROMFilename, ModifiedROMFilename, OutputPatchFilename : String);
begin

end;

end.