unit PatchNINJA;

interface

uses Classes;

type

  TNINJAInfo = record
    enc   : byte; // (   1 byte ) // Info text encoding - 1 = UTF-8
    auth  : widestring; // AUTH  (  84 bytes) // Author
    ver   : widestring; // VER   (  11 bytes) // Version
    title : widestring; // TITLE ( 256 bytes) // Title
    genre : widestring; // GENRE (  48 bytes) // Genre
    lang  : widestring; // LANG  (  48 bytes) // Language
    date  : widestring; // DATE  (   8 bytes) // Date as YYYYMMDD
    web   : widestring; // WEB   ( 512 bytes) // Website
    desc  : widestring; // DESC  (1074 bytes) // Info (New line marked by "\n")
  end;

  TNINJA2Patch = class
  private

  public
    function ViewPatchInfo(pFilename :String): TNINJAInfo;

  end;

implementation

function TNINJA2Patch.ViewPatchInfo(pFilename :String): TNINJAInfo;
var
  FS : TFileStream;
  HeaderTxt : String;
begin
  FS := TFileStream.Create(pFilename, fmOpenRead);
  try
    // Load in the header.
    SetLength(HeaderTxt,6);

    widestring

  finally
    FreeAndNil(FS);
  end;
end;

end.
